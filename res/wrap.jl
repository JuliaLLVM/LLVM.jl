# generate wrappers for the LLVM and LLVM Extra libraries
#
# these wrappers are LLVM version-specific, so we generate multiple copies.

using Clang.Generators
import Clang

@add_def off_t

# based on the supported Julia versions
const llvm_versions = ["15", "16", "17", "18", "19", "20"]
const llvm_configs = Dict()

function main()
    for version in llvm_versions
        config = get!(llvm_configs, version) do
            # we cannot import multiple versions of the same module in a single session,
            # so read all the information we need using a temporary process
            script = raw"""
                version = ARGS[1]

                using Pkg
                Pkg.activate(; temp=true)
                Pkg.add(; name="LLVM_full_jll", version)

                using LLVM_full_jll
                data = LLVM_full_jll.llvm_config() do exe
                    (; includedir=readchomp(`$exe --includedir`),
                       cppflags=readchomp(`$exe --cppflags`))
                end
                print(repr(data))
            """
            cmd = `$(Base.julia_cmd()) -e $script $version`
            data = read(cmd, String)
            eval(Meta.parse(data))
        end

        cd(@__DIR__) do
            wrap(version; config.includedir, config.cppflags)
        end
    end
end

function wrap(version; includedir, cppflags)

    args = get_default_args("x86_64-linux-gnu")
    push!(args, "-isystem$includedir")
    append!(args, split(cppflags))

    mkpath("../lib/$version")

    # LLVM C APIs
    let
        options = load_options(joinpath(@__DIR__, "llvm.toml"))
        options["general"]["output_file_path"] = "../lib/$version/libLLVM.jl"

        header_files = detect_headers(joinpath(includedir, "llvm-c"), args)

        ctx = create_context(header_files, args, options)

        build!(ctx, BUILDSTAGE_NO_PRINTING)

        build!(ctx, BUILDSTAGE_PRINTING_ONLY)
    end

    # LLVMExtra
    let
        options = load_options(joinpath(@__DIR__, "llvmextra.toml"))
        options["general"]["output_file_path"] = "../lib/$version/libLLVM_extra.jl"

        header_files = map(["LLVMExtra.h"]) do file
            realpath(joinpath(@__DIR__, "..", "deps", "LLVMExtra", "include", file))
        end

        ctx = create_context(header_files, args, options)

        build!(ctx)
    end

    return
end

isinteractive() || main()
