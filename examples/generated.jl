# an example using generated functions which build their own IR

using LLVM
using LLVM.Interop

# pointer wrapper type for which we'll build our own low-level intrinsics
struct CustomPtr{T}
    ptr::Ptr{T}
end

@generated function Base.unsafe_load(p::CustomPtr{T}, i::Integer=1) where T
    @dispose ctx=Context() begin
        # get the element type
        eltyp = convert(LLVMType, T)

        T_int = LLVM.IntType(sizeof(Int)*8)
        T_ptr = LLVM.PointerType(eltyp)

        # create a function
        paramtyps = if VERSION >= v"1.12-"
            [T_ptr, T_int]
        else
            [T_int, T_int]
        end
        llvmf, _ = create_function(eltyp, paramtyps)

        # generate IR
        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvmf, "entry")
            position!(builder, entry)

            ptr = parameters(llvmf)[1]
            if value_type(ptr) isa LLVM.IntegerType
                ptr = inttoptr!(builder, ptr, T_ptr)
            end

            ptr = gep!(builder, eltyp, ptr, [parameters(llvmf)[2]])
            val = load!(builder, eltyp, ptr)
            ret!(builder, val)
        end

        call_function(llvmf, T, Tuple{Ptr{T}, Int}, :(p.ptr), :(Int(i-1)))
    end
end

a = [42]
ptr = CustomPtr{Int}(pointer(a))

using Test

@test unsafe_load(ptr) == a[1]
