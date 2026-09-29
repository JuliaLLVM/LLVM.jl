## user values (<: llvm::User)

"""
    LLVM.User <: LLVM.Value

A value that uses other values.

See also: [`operands`](@ref).
"""
abstract type User <: Value end
@vocabulary IR User

# operand iteration

@vocabulary IR operands

struct UserOperandSet <: AbstractVector{Value}
    user::User
end

"""
    operands(user::LLVM.User)

Get an iterator over the operands of the given user.
"""
operands(user::User) = UserOperandSet(user)

Base.size(iter::UserOperandSet) = (API.LLVMGetNumOperands(iter.user),)

Base.IndexStyle(::UserOperandSet) = IndexLinear()

function Base.getindex(iter::UserOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetOperand(iter.user, i-1))
end

Base.setindex!(iter::UserOperandSet, val::Value, i) =
    API.LLVMSetOperand(iter.user, i-1, val)

@inline function Base.iterate(iter::UserOperandSet, i=1)
    i >= length(iter) + 1 ? nothing : (iter[i], i+1)
end
