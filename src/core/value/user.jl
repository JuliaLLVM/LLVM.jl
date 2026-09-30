## user values (<: llvm::User)

"""
    LLVM.User <: LLVM.Value

A value that uses other values.

See also the [`operands`](@ref LLVM.User) property.

# Properties

    user.operands

The operands of a user, e.g., an instruction or a constant expression, as a view. For
instructions and global values, the view is mutable: assigning to an element,
`inst.operands[i] = val`, replaces that operand, and `replace!(inst.operands, old => new)`
replaces every operand that is `old`. The operands of other constants cannot be changed,
as LLVM uniques constants by their operands; use [`replace_uses!`](@ref) on the operand to
update the constants that use it instead.

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

Base.size(iter::UserOperandSet) = (Int(API.LLVMGetNumOperands(iter.user)),)

Base.IndexStyle(::Type{UserOperandSet}) = IndexLinear()

function Base.getindex(iter::UserOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetOperand(iter.user, i-1))
end

function Base.setindex!(iter::UserOperandSet, val::Value, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    # LLVM uniques constants by their operands (it asserts this, if enabled)
    iter.user isa Constant && !(iter.user isa GlobalValue) &&
        throw(ArgumentError("Cannot change the operands of a $(typeof(iter.user))"))
    API.LLVMSetOperand(iter.user, i-1, val)
    return iter
end

@inline function Base.iterate(iter::UserOperandSet, i=1)
    i >= length(iter) + 1 ? nothing : (iter[i], i+1)
end
