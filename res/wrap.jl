# generate wrappers for the LLVM and LLVM Extra libraries
#
# the C APIs differ between versions of LLVM, so we generate wrappers for every version, and
# merge them into a single set of wrappers that works with all of them (see merge.jl).
#
# usage: julia --project wrap.jl [DIR], where DIR is where to keep the per-version wrappers

using Clang.Generators
import Clang

include("merge.jl")

@add_def off_t

# based on the supported Julia versions
const llvm_versions = ["15", "16", "17", "18", "19", "20", "21", "22", "23"]
const llvm_configs = Dict()

function main(outdir=mktempdir())
    provenance = Dict{Int,String}()
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
                       cppflags=readchomp(`$exe --cppflags`),
                       version=readchomp(`$exe --version`))
                end
                print(repr(data))
            """
            cmd = `$(Base.julia_cmd()) -e $script $version`
            data = read(cmd, String)
            eval(Meta.parse(data))
        end

        cd(@__DIR__) do
            wrap(version, joinpath(outdir, version); config.includedir, config.cppflags)
        end
        provenance[parse(Int, version)] = "LLVM_full_jll $(config.version)"
    end

    libdir = joinpath(@__DIR__, "..", "lib")
    merged = merge_versions(outdir, parse.(Int, llvm_versions), libdir)
    write_wrappers(merged, libdir)
    write_inventory(merged, joinpath(@__DIR__, "ABI.md"); provenance)
end

# custom rewriter
function rewrite!(e::Expr)
    Meta.isexpr(e, :function) || return e
    fname = e.args[1].args[1]
    if fname == :LLVMConstDataArray
        # The data argument for LLVMConstDataArray accepts a memory buffer not a c-string
        ccall_ex = e.args[2].args[1]
        @assert Meta.isexpr(ccall_ex, :call) && ccall_ex.args[1] == :ccall
        types_arg = ccall_ex.args[4]
        @assert Meta.isexpr(types_arg, :tuple) && types_arg.args[2] == :Cstring
        types_arg.args[2] = :(Ptr{Cvoid})
    end
    return e
end

function rewrite!(dag::ExprDAG)
    for node in get_nodes(dag)
        for expr in get_exprs(node)
            rewrite!(expr)
        end
    end
end

function wrap(version, dir; includedir, cppflags)

    args = get_default_args("x86_64-linux-gnu")
    push!(args, "-isystem$includedir")
    append!(args, split(cppflags))

    mkpath(dir)

    # LLVM C APIs
    let
        options = load_options(joinpath(@__DIR__, "llvm.toml"))
        options["general"]["output_file_path"] = joinpath(dir, "libLLVM.jl")

        header_files = detect_headers(joinpath(includedir, "llvm-c"), args)

        ctx = create_context(header_files, args, options)

        build!(ctx, BUILDSTAGE_NO_PRINTING)

        rewrite!(ctx.dag)

        build!(ctx, BUILDSTAGE_PRINTING_ONLY)
    end

    # LLVMExtra
    let
        options = load_options(joinpath(@__DIR__, "llvmextra.toml"))
        options["general"]["output_file_path"] = joinpath(dir, "libLLVM_extra.jl")

        header_files = map(["LLVMExtra.h"]) do file
            realpath(joinpath(@__DIR__, "..", "deps", "LLVMExtra", "include", file))
        end

        ctx = create_context(header_files, args, options)

        build!(ctx)
    end

    return
end

isinteractive() || main(ARGS...)
