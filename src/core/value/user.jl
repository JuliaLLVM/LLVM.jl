## user values (<: llvm::User)

"""
    LLVM.User <: LLVM.Value

A value that uses other values.

See also the [`operands`](@ref LLVM.User) property.

# Properties

    user.operands

The operands of a user, e.g., an instruction or a constant expression, as a mutable view:
assigning to an element, `inst.operands[i] = val`, replaces that operand.

The properties of [`Value`](@ref LLVM.Value) are available too.
"""
abstract type User <: Value end
@vocabulary IR User

# operand iteration

struct UserOperandSet <: AbstractVector{Value}
    user::User
end

operands(user::User) = UserOperandSet(user)

@property User operands

Base.size(iter::UserOperandSet) = (API.LLVMGetNumOperands(iter.user),)

Base.IndexStyle(::UserOperandSet) = IndexLinear()

function Base.getindex(iter::UserOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetOperand(iter.user, i-1))
end

function Base.setindex!(iter::UserOperandSet, val::Value, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    API.LLVMSetOperand(iter.user, i-1, val)
    return iter
end

@inline function Base.iterate(iter::UserOperandSet, i=1)
    i >= length(iter) + 1 ? nothing : (iter[i], i+1)
end
