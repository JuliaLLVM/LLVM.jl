include("setup.jl")

using InteractiveUtils
@info "System information:\n" * sprint(io->versioninfo(io))

ctx_typed_pointers = Context() do ctx
    supports_typed_pointers(ctx)
end
julia_typed_pointers = let
    ir = sprint(io->code_llvm(io, unsafe_load, Tuple{Ptr{Int}}))
    if occursin(r"load i64, i64\* .+, align 1", ir)
        true
    elseif occursin(r"load i64, ptr .+, align 1", ir)
        false
    else
        error("could not determine whether Julia uses typed pointers")
    end
end
@info "Pointer settings: Julia uses $(julia_typed_pointers ? "typed" : "opaque") pointers, default contexts use $(ctx_typed_pointers ? "typed" : "opaque") pointers"

@info "Debug settings: typecheck = $(LLVM.typecheck_enabled), memcheck = $(LLVM.memcheck_enabled)"


@testset "LLVM" verbose=true begin

include("essential.jl")
include("support.jl")
include("core.jl")
include("linker.jl")
include("instructions.jl")
include("buffer.jl")
include("analysis.jl")
include("passmanager.jl")
include("newpm.jl")
include("pass.jl")
include("execution.jl")
include("target.jl")
include("targetmachine.jl")
include("datalayout.jl")
include("debuginfo.jl")
include("util.jl")
include("interop.jl")
include("orc.jl")
if !Sys.iswindows()
    # XXX: hangs on Windows
    include("jljit.jl")
end
if LLVM.has_oldpm()
    include("transform.jl")
end
include("Kaleidoscope.jl")
include("examples.jl")
include("aqua.jl")

end
