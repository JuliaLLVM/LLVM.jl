include("setup.jl")

# with `LLVM_TEST_LIBRARY` set to the path of a standalone library (e.g., built by
# `deps/build_standalone.jl`), the tests use that library next to the default one, e.g.,
# LLVM 23 next to the LLVM 15 of Julia 1.10. this checks that LLVM.jl uses the version of the
# library in use, and not the one it was compiled for.
test_library = let path = get(ENV, "LLVM_TEST_LIBRARY", "")
    isempty(path) ? LLVM.library() : LLVM.register_library!(:test, path)
end

using InteractiveUtils
@info "System information:\n" * sprint(io->versioninfo(io))

ctx_typed_pointers = Context(; library=test_library) do ctx
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

test_library === LLVM.julia_library ||
    @info "Using LLVM $(test_library.version) from $(test_library.path) instead of Julia's"
@info "Debug settings: typecheck = $(LLVM.typecheck_enabled), memcheck = $(LLVM.memcheck_enabled)"


LLVM.library!(test_library) do
@testset "LLVM" verbose=true begin

include("essential.jl")
include("libraries.jl")
include("support.jl")
include("enums.jl")
include("core.jl")
include("properties.jl")
include("strings.jl")
include("linker.jl")
include("instructions.jl")
include("insertion.jl")
include("atomics.jl")
include("buffer.jl")
include("analysis.jl")
include("range.jl")
include("passes.jl")
include("execution.jl")
include("target.jl")
include("targetmachine.jl")
include("datalayout.jl")
include("debuginfo.jl")
include("util.jl")
include("documentation.jl")
if julia_llvm_in_use()
    # integration with Julia's code generator, which requires Julia's LLVM
    include("interop.jl")
else
    @testset "integration with Julia's code generator" begin
        @test_throws "requires the LLVM library that Julia uses" LLVM.JuliaOJIT()
        @test_throws "requires the LLVM library that Julia uses" LLVM.Interop.JuliaPipeline()
        @dispose ctx=Context() begin
            @test_throws "requires the LLVM library that Julia uses" convert(LLVMType, Int)
        end
        # code for Julia's code generator is generated with Julia's LLVM
        @test LLVM.Interop.generate_llvmcall(Int, Tuple{Int}, :x) do builder, x
            x
        end isa Expr
        @test !("RemoveNI" in LLVM.module_passes())
    end
end
include("orc.jl")
include("disasm.jl")
if julia_llvm_in_use()
    include("jljit.jl")
end
include("Kaleidoscope.jl")
include("examples.jl")
include("aqua.jl")

end
end
