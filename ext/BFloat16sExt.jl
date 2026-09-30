module BFloat16sExt

using LLVM
using LLVM: API, BFloatType, ConstantFP, ConstantDataArray

using BFloat16s

## constant values

LLVM.ConstantFP(val::BFloat16) = ConstantFP(BFloatType(), val)

Base.convert(::Type{BFloat16}, val::ConstantFP) =
    convert(BFloat16, API.LLVMConstRealGetDouble(val, Ref{API.LLVMBool}()))

LLVM.ConstantDataArray(data::AbstractVector{BFloat16}) =
    ConstantDataArray(BFloatType(), data)

end
