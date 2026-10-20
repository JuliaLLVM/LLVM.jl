# build a standalone LLVM library: a version of LLVM from LLVM_full_jll, linked statically
# together with LLVMExtra into a library that only exports the LLVM C API and LLVMExtra (and
# the functions that LLVM's JIT looks up by name), so that it can be used next to the LLVM
# that Julia uses, e.g., to test LLVM.jl with versions of LLVM that no version of Julia uses.
#
# usage: julia --project=deps deps/build_standalone.jl VERSION [INSTALL_DIR]
#
# VERSION selects LLVM_full_jll, e.g., `23` or `23.1`. The path of the library is printed;
# by default, it is installed in a scratch space, specific to the version of LLVM, the
# sources of LLVMExtra and the build type.

using Pkg
Pkg.activate(@__DIR__)
Pkg.instantiate()

using Scratch, Libdl

isempty(ARGS) && error("Usage: julia --project=deps deps/build_standalone.jl VERSION [INSTALL_DIR]")
llvm_version = ARGS[1]

cmake_path = Sys.which("cmake")
cmake_path === nothing && error("cmake not found on PATH")

LLVM_UUID = Base.UUID("929cbde3-209d-540e-8aea-75f648917ca0")
source_dir = joinpath(@__DIR__, "LLVMExtra")

# download LLVM, in a separate process, as the JLL may be another version than the one this
# environment would load
script = """
    using Pkg
    Pkg.activate(; temp=true)
    Pkg.add(; name="LLVM_full_jll", version=ARGS[1], io=devnull)
    using LLVM_full_jll
    zstd = isdefined(LLVM_full_jll, :Zstd_jll) ? LLVM_full_jll.Zstd_jll.artifact_dir : ""
    println(LLVM_full_jll.artifact_dir)
    println(LLVM_full_jll.LIBPATH[])
    println(zstd)
"""
llvm_dir, llvm_libpath, zstd_dir = let lines = readlines(`$(Base.julia_cmd()) -e $script $llvm_version`)
    lines[end-2], lines[end-1], lines[end]
end
llvm_config = joinpath(llvm_dir, "tools", "llvm-config")

# LLVM_full_jll's tools (llvm-config, which CMake also runs) need the libraries of the JLL's
# dependencies
function with_libpath(cmd)
    var = Sys.iswindows() ? "PATH" : Sys.isapple() ? "DYLD_FALLBACK_LIBRARY_PATH" : "LD_LIBRARY_PATH"
    paths = filter(!isempty, [llvm_libpath, get(ENV, var, "")])
    addenv(cmd, var => join(paths, Sys.iswindows() ? ';' : ':'))
end

# its static libraries may need zstd, which it provides as a shared library
zstd_lib = isempty(zstd_dir) ? nothing : joinpath(zstd_dir, Sys.iswindows() ? "bin" : "lib")

full_version = VersionNumber(chopsuffix(readchomp(with_libpath(`$llvm_config --version`)), "jl"))

# like `deps/build_local.jl`, build with the optimization and debug info of LLVMExtra_jll
build_type = "RelWithDebInfo"

# the default installation directory is specific to the build of LLVM (the artifact, which
# differs between builds of the same version of LLVM, and between platforms), the sources
# and the build type
custom_install_dir = length(ARGS) >= 2
install_dir = if custom_install_dir
    abspath(ARGS[2])
else
    llvm_key = basename(llvm_dir)[1:12]
    source_key = bytes2hex(Pkg.GitTools.tree_hash(source_dir))[1:12]
    joinpath(get_scratch!(LLVM_UUID, "standalone"),
             "llvm$(full_version)-$(llvm_key)-$(source_key)-$(lowercase(build_type))")
end

lib_name = "libLLVM-standalone-$(full_version.major).$(Libdl.dlext)"
lib_path = joinpath(install_dir, "lib", lib_name)
if isfile(lib_path)
    @info "Using the existing build in $install_dir"
else
    linker_flags = zstd_lib === nothing ? String[] : ["-L$zstd_lib", "-Wl,-rpath,$zstd_lib"]

    # the default directory is installed into through a temporary one next to it, which is
    # then moved in place, so that concurrent builds don't interfere
    prefix = if custom_install_dir
        install_dir
    else
        mktempdir(mkpath(dirname(install_dir)); prefix="tmp-", cleanup=false)
    end
    mktempdir() do build_dir
        @info "Building" llvm_version=full_version source_dir install_dir
        run(with_libpath(`$cmake_path -B$build_dir -S$source_dir
             -DLLVMEXTRA_STANDALONE=ON
             -DLLVM_ROOT=$(joinpath(llvm_dir, "lib", "cmake", "llvm"))
             -DLLVM_CONFIG=$llvm_config
             -DCMAKE_BUILD_TYPE=$build_type
             -DCMAKE_SHARED_LINKER_FLAGS=$(join(linker_flags, " "))
             -DCMAKE_INSTALL_PREFIX=$prefix`))
        run(`$cmake_path --build $build_dir --target install --parallel $(Sys.CPU_THREADS)`)
    end
    if !custom_install_dir
        chmod(prefix, 0o755)    # `mktempdir` only gives access to the owner
        try
            mv(prefix, install_dir)
        catch
            # a concurrent build of the same sources, which we can use instead
            isfile(lib_path) || rethrow()
            rm(prefix; recursive=true)
        end
    end
    isfile(lib_path) || error("Could not find the library at $lib_path")
end

println(lib_path)
