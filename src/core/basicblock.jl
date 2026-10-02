@vocabulary IR BasicBlock, remove!, erase!

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
A saved view refers to that terminator; query `bb.successors` again after replacing it.

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
    BasicBlock(name::AbstractString)

Create a new, empty basic block with the given name.
"""
BasicBlock(name::AbstractString) =
    BasicBlock(API.LLVMCreateBasicBlockInContext(context(), name))

"""
    BasicBlock(f::LLVM.Function, name::AbstractString)

Create a new, empty basic block with the given name, and insert it at the end of the given
function.
"""
BasicBlock(f::Function, name::AbstractString;) =
    BasicBlock(API.LLVMAppendBasicBlockInContext(context(f), f, name))

"""
    remove!(bb::BasicBlock)

Remove the given basic block from its parent function, but do not free the object.
"""
remove!(bb::BasicBlock) = API.LLVMRemoveBasicBlockFromParent(bb)

"""
    erase!(bb::BasicBlock)

Remove the given basic block from its parent function, if any, and free the object.

!!! warning

    This function is unsafe because it does not check if the basic block is used elsewhere.
"""
erase!(bb::BasicBlock) = API.LLVMExtraDeleteBasicBlock(bb)

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


## block addresses

@vocabulary IR BlockAddress

"""
    BlockAddress <: LLVM.Constant

The address of a basic block, `blockaddress(@f, %bb)` in LLVM IR, e.g., as the destination
of an `indirectbr` instruction.

# Properties

    ba.function

The function that contains the basic block.

    ba.block

The basic block whose address this is.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct BlockAddress <: Constant
    ref::API.LLVMValueRef
end
register(BlockAddress, API.LLVMBlockAddressValueKind)

"""
    BlockAddress(bb::BasicBlock)

Get the address of the basic block `bb`, which must be part of a function. Taking the
address of the entry block of a function is not valid IR.
"""
function BlockAddress(bb::BasicBlock)
    f = parent(bb)
    f === nothing &&
        throw(ArgumentError("Cannot take the address of a basic block that is not part of a function"))
    BlockAddress(API.LLVMBlockAddress(f, bb))
end

# before LLVM 19, the C API has no getters, but the function and the block are operands
function blockaddress_function(ba::BlockAddress)
    ref = @static if version() >= v"19"
        API.LLVMGetBlockAddressFunction(ba)
    else
        API.LLVMGetOperand(ba, 0)
    end
    Function(ref)
end

function blockaddress_block(ba::BlockAddress)
    @static if version() >= v"19"
        BasicBlock(API.LLVMGetBlockAddressBasicBlock(ba))
    else
        BasicBlock(API.LLVMGetOperand(ba, 1))
    end
end

@property BlockAddress var"function" => blockaddress_function
@property BlockAddress block => blockaddress_block
