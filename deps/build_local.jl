# build a local version of LLVMExtra

using Pkg
Pkg.activate(@__DIR__)
Pkg.instantiate()

if haskey(ENV, "GITHUB_ACTIONS")
    println("::warning ::Using a locally-built LLVMExtra; A bump of LLVMExtra_jll will be required before releasing LLVM.jl.")
end

using Pkg, Scratch, Preferences, Libdl

cmake_path = Sys.which("cmake")
cmake_path === nothing && error("cmake not found on PATH; please install it (e.g. `pacman -S mingw-w64-x86_64-cmake` inside msys2 on Windows)")

LLVM = Base.UUID("929cbde3-209d-540e-8aea-75f648917ca0")

source_dir = joinpath(@__DIR__, "LLVMExtra")

# get build directory (the first argument, or a temporary one)
build_dir = if isempty(ARGS)
    mktempdir()
else
    ARGS[1]
end
mkpath(build_dir)

# download LLVM
Pkg.activate(; temp=true)
llvm_assertions = try
    dlsym(dlopen(Base.libllvm_path()), :_ZN4llvm24DisableABIBreakingChecksE)
    false
catch
    true
end
llvm_pkg_version = "$(Base.libllvm_version.major).$(Base.libllvm_version.minor)"

# get the directory to install into: the second argument, or by default a directory in a
# scratch space, which is shared by every Julia version that uses this depot. to keep
# builds for other versions of LLVM, or of the sources, from deleting a library that an
# environment's preferences point to, that directory is specific to both.
custom_install_dir = length(ARGS) >= 2
install_dir = if custom_install_dir
    abspath(ARGS[2])
else
    llvm_key = "llvm$(Base.libllvm_version)" * (llvm_assertions ? "-assert" : "")
    source_key = bytes2hex(Pkg.GitTools.tree_hash(source_dir))[1:12]
    joinpath(get_scratch!(LLVM, "builds"), "$(llvm_key)-$(source_key)")
end

LLVM = if llvm_assertions
    Pkg.add(name="LLVM_full_assert_jll", version=llvm_pkg_version)
    using LLVM_full_assert_jll
    LLVM_full_assert_jll
else
    Pkg.add(name="LLVM_full_jll", version=llvm_pkg_version)
    using LLVM_full_jll
    LLVM_full_jll
end
LLVM_DIR = joinpath(LLVM.artifact_dir, "lib", "cmake", "llvm")

# build and install. the default directory is installed into through a temporary one next
# to it, which is then moved in place, so that concurrent builds don't interfere.
prefix = if custom_install_dir
    install_dir
else
    mkpath(dirname(install_dir))
    mktempdir(dirname(install_dir); prefix="tmp-", cleanup=false)
end
@info "Building" source_dir install_dir build_dir LLVM_DIR cmake_path
config_opts = `-DLLVM_ROOT=$(LLVM_DIR) -DCMAKE_INSTALL_PREFIX=$(prefix)`
if Sys.iswindows()
    # prevent picking up MSVC
    config_opts = `$config_opts -G "MSYS Makefiles"`
end
run(`$cmake_path $config_opts -B$(build_dir) -S$(source_dir)`)
run(`$cmake_path --build $(build_dir) --target install`)

# discover the built library, which is named after the version of LLVM it was built for
built_libs = filter(readdir(joinpath(prefix, "lib"))) do file
    endswith(file, ".$(Libdl.dlext)")
end
lib_name = only(built_libs)
occursin("-$(Base.libllvm_version.major).", lib_name) ||
    error("Built $lib_name, which is not for LLVM $(Base.libllvm_version.major); was it built against another LLVM than $LLVM_DIR?")

if !custom_install_dir
    chmod(prefix, 0o755)    # `mktempdir` only gives access to the owner
    try
        # this fails if the directory already exists, e.g., because of a concurrent build.
        # as it is specific to the version of LLVM and the sources, use that library
        # instead of replacing it, which could break an environment that is using it.
        mv(prefix, install_dir)
    catch
        isdir(install_dir) || rethrow()
        @info "Using the existing build in $install_dir"
        rm(prefix; recursive=true)
    end
end
lib_path = joinpath(install_dir, "lib", lib_name)
isfile(lib_path) || error("Could not find library $lib_path in build directory")

# tell LLVM.jl to load our library instead of the default artifact one
set_preferences!(
    joinpath(dirname(@__DIR__), "LocalPreferences.toml"),
    "LLVM",
    "libLLVMExtra" => lib_path;
    force=true,
)
