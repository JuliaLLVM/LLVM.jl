# an example using generated functions which build their own IR

using LLVM, LLVM.IR, LLVM.Build
using LLVM.Interop

# pointer wrapper type for which we'll build our own low-level intrinsics
struct CustomPtr{T}
    ptr::Ptr{T}
end

# generate the IR that loads an element; the arguments are bound to LLVM values
@llvmgenerated builder function load_element(ptr::Ptr{T}, i::Int)::T where {T}
    eltyp = convert(LLVMType, T)

    # older versions of Julia pass pointers as integers
    if ptr.value_type isa LLVM.IntegerType
        ptr = inttoptr!(builder, ptr, LLVM.PointerType(eltyp))
    end

    ptr = gep!(builder, eltyp, ptr, [i])
    load!(builder, eltyp, ptr)
end

Base.unsafe_load(p::CustomPtr, i::Integer=1) = load_element(p.ptr, Int(i-1))

a = [42]
ptr = CustomPtr{Int}(pointer(a))

using Test

@test unsafe_load(ptr) == a[1]
