@vocabulary IR BasicBlock, remove!, erase!,
               move_before, move_after

"""
    BasicBlock

A basic block in the IR. A basic block is a sequence of instructions that
always ends in a terminator instruction.

# Properties

    bb.parent

The function that contains the basic block, or `nothing` if the block is not part of a
function.

    bb.terminator

The terminator instruction of the basic block, or `nothing` if the block does not end with
a terminator.

    bb.instructions

The instructions of the basic block, in order, as a read-only view that always reflects the
current contents of the block. Use an `IRBuilder` to add instructions, and operations like
`remove!` or `erase!` to remove them. While iterating over the view, it is safe to remove or
erase the instruction that was just returned, but not other instructions.

    bb.predecessors

The predecessors of the basic block, i.e., the blocks whose terminator branches to it, as a
read-only view. A block that branches to it several times (e.g., a `switch` with multiple
cases) is included once per branch.

The predecessors are derived from the uses of the block, and are only computed while
iterating the view, so use `collect` to get a vector.

    bb.successors

The successors of the basic block, i.e., the `successors` of its terminator. Throws an
`ArgumentError` if the block does not have a terminator.

    bb.next
    bb.prev

The next or previous basic block in the function, or `nothing` if there is none (or if the
block is not part of a function).

The properties of [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct BasicBlock <: Value
    ref::API.LLVMValueRef
end
register(BasicBlock, API.LLVMBasicBlockValueKind)

BasicBlock(ref::API.LLVMBasicBlockRef) = BasicBlock(API.LLVMBasicBlockAsValue(ref))
Base.unsafe_convert(::Type{API.LLVMBasicBlockRef}, bb::BasicBlock) = API.LLVMValueAsBasicBlock(bb)

"""
    BasicBlock(name::String)

Create a new, empty basic block with the given name.
"""
BasicBlock(name::String) =
    BasicBlock(API.LLVMCreateBasicBlockInContext(context(), name))

"""
    BasicBlock(f::LLVM.Function, name::String)

Create a new, empty basic block with the given name, and insert it at the end of the given
function.
"""
BasicBlock(f::Function, name::String;) =
    BasicBlock(API.LLVMAppendBasicBlockInContext(context(f), f, name))

"""
    BasicBlock(bb::BasicBlock, name::String)

Create a new, empty basic block with the given name, and insert it before the given basic
block.
"""
BasicBlock(bb::BasicBlock, name::String) =
    BasicBlock(API.LLVMInsertBasicBlockInContext(context(bb), bb, name))

"""
    remove!(bb::BasicBlock)

Remove the given basic block from its parent function, but do not free the object.
"""
remove!(bb::BasicBlock) = API.LLVMRemoveBasicBlockFromParent(bb)

"""
    erase!(fun::Function, bb::BasicBlock)

Remove the given basic block from its parent function and free the object.

!!! warning

    This function is unsafe because it does not check if the basic block is used elsewhere.
"""
erase!(bb::BasicBlock) = API.LLVMDeleteBasicBlock(bb)

function parent(bb::BasicBlock)
    ref = API.LLVMGetBasicBlockParent(bb)
    ref == C_NULL && return nothing
    Function(ref)
end

@property BasicBlock parent

function terminator(bb::BasicBlock)
    ref = API.LLVMGetBasicBlockTerminator(bb)
    ref == C_NULL && return nothing
    Instruction(ref)
end

@property BasicBlock terminator

name(bb::BasicBlock) = unsafe_string(API.LLVMGetBasicBlockName(bb))

"""
    move_before(bb::BasicBlock, pos::BasicBlock)

Move the given basic block before the given position.
"""
move_before(bb::BasicBlock, pos::BasicBlock) =
    API.LLVMMoveBasicBlockBefore(bb, pos)

"""
    move_after(bb::BasicBlock, pos::BasicBlock)

Move the given basic block after the given position.
"""
move_after(bb::BasicBlock, pos::BasicBlock) =
    API.LLVMMoveBasicBlockAfter(bb, pos)


## instruction iteration

struct BasicBlockInstructionSet
    bb::BasicBlock
end

instructions(bb::BasicBlock) = BasicBlockInstructionSet(bb)

@property BasicBlock instructions

Base.eltype(::Type{BasicBlockInstructionSet}) = Instruction

@inline function Base.iterate(iter::BasicBlockInstructionSet,
                              state=API.LLVMGetFirstInstruction(iter.bb))
    state == C_NULL ? nothing : (Instruction(state), API.LLVMGetNextInstruction(state))
end

function Base.first(iter::BasicBlockInstructionSet)
    ref = API.LLVMGetFirstInstruction(iter.bb)
    ref == C_NULL && throw(BoundsError(iter))
    Instruction(ref)
end

function Base.last(iter::BasicBlockInstructionSet)
    ref = API.LLVMGetLastInstruction(iter.bb)
    ref == C_NULL && throw(BoundsError(iter))
    Instruction(ref)
end

Base.isempty(iter::BasicBlockInstructionSet) =
    API.LLVMGetLastInstruction(iter.bb) == C_NULL

Base.IteratorSize(::Type{BasicBlockInstructionSet}) = Base.SizeUnknown()

function next(inst::Instruction)
    API.LLVMGetInstructionParent(inst) == C_NULL && return nothing
    ref = API.LLVMGetNextInstruction(inst)
    ref == C_NULL ? nothing : Instruction(ref)
end

function prev(inst::Instruction)
    API.LLVMGetInstructionParent(inst) == C_NULL && return nothing
    ref = API.LLVMGetPreviousInstruction(inst)
    ref == C_NULL ? nothing : Instruction(ref)
end

@property Instruction next
@property Instruction prev


## cfg-like operations

struct BasicBlockPredecessorSet
    bb::BasicBlock
end

predecessors(bb::BasicBlock) = BasicBlockPredecessorSet(bb)

@property BasicBlock predecessors

Base.eltype(::Type{BasicBlockPredecessorSet}) = BasicBlock

Base.IteratorSize(::Type{BasicBlockPredecessorSet}) = Base.SizeUnknown()

function Base.iterate(iter::BasicBlockPredecessorSet, use=API.LLVMGetFirstUse(iter.bb))
    while use != C_NULL
        user = API.LLVMGetUser(use)
        use = API.LLVMGetNextUse(use)
        # blocks are also used by, e.g., `blockaddress` constants
        API.LLVMIsATerminatorInst(user) == C_NULL && continue
        return BasicBlock(API.LLVMGetInstructionParent(user)), use
    end
    return nothing
end

Base.length(iter::BasicBlockPredecessorSet) = count(Returns(true), iter)

Base.isempty(iter::BasicBlockPredecessorSet) = iterate(iter) === nothing

function successors(bb::BasicBlock)
    term = terminator(bb)
    term === nothing &&
        throw(ArgumentError("Cannot query successors of unterminated basic block"))
    successors(term)
end

@property BasicBlock successors
