# Example on how access experimental intrinsic in LLVM
#
# This example uses the constrained floating point intrinsics
# https://releases.llvm.org/6.0.0/docs/LangRef.html#constrained-floating-point-intrinsics
#
# Note that this will only change compile time behavior/optimizations and not the actual
# runtime behavior. For changing runtime behavior please use the SetRounding.jl package or a
# similar mechanism.
#
# This example is not complete and could use some better error handling

using LLVM, LLVM.IR, LLVM.Build
using LLVM.Interop

# map Julia functions to llvm intrinsic
func(::Type{typeof(+)}) = "fadd"

# stub of a Trait system for the different modes
abstract type Round end
abstract type FPExcept end

struct RoundUpward <: Round end
struct FPExceptStrict <: FPExcept end

meta(::Type{RoundUpward}) = "round.upward"
meta(::Type{FPExceptStrict}) = "fpexcept.strict"

@generated function constrained(::F, ::Type{round}, ::Type{fpexcept}, xs::Vararg{T,N}) where
                               {F, round, fpexcept, T<:AbstractFloat, N}
    @assert N >= 0

    @dispose ctx=Context() begin
        typ = convert(LLVMType, T)

        # create a function
        paramtyps = [typ for i in 1:N]
        llvm_f, _ = create_function(typ, paramtyps)

        # create the intrinsic
        mtyp = LLVM.MetadataType()
        mround = MDString(meta(round))
        mfpexcept = MDString(meta(fpexcept))
        mod = LLVM.parent(llvm_f)
        intrinsic = Intrinsic("llvm.experimental.constrained.$(func(F))")
        intrinsic_fun = LLVM.Function(mod, intrinsic, [typ])
        ftype = LLVM.FunctionType(intrinsic,[typ])

        # generate IR
        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)
            val = call!(builder, ftype, intrinsic_fun,
                        [parameters(llvm_f)..., Value(mround), Value(mfpexcept)])
            ret!(builder, val)
        end

        call_function(llvm_f, T, Tuple{(T for i in 1:N)...},
                      (Expr(:ref, :xs, i) for i in 1:N)...)
    end
end

cadd(x, y) = constrained(+, RoundUpward, FPExceptStrict, x, y)

using Test
@test cadd(1.0, 2.0) == 3.0

using InteractiveUtils
ir = sprint(io->code_llvm(io, cadd, (Float32, Float32)))
@test occursin("@llvm.experimental.constrained.fadd.f32", ir)
