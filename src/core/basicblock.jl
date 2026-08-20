export BasicBlock, remove!, erase!,
       terminator, name,
       move_before, move_after

"""
    BasicBlock

A basic block in the IR. A basic block is a sequence of instructions that
always ends in a terminator instruction.
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

"""
    parent(bb::BasicBlock) -> LLVM.Function

Get the function that contains the given basic block, or `nothing` if the block is not part
of a function.
"""
function parent(bb::BasicBlock)
    ref = API.LLVMGetBasicBlockParent(bb)
    ref == C_NULL && return nothing
    Function(ref)
end

"""
    terminator(bb::BasicBlock) -> LLVM.Instruction

Get the terminator instruction of the given basic block.
"""
function terminator(bb::BasicBlock)
    ref = API.LLVMGetBasicBlockTerminator(bb)
    ref == C_NULL && return nothing
    Instruction(ref)
end

"""
    name(bb::BasicBlock) -> String

Get the name of the given basic block.
"""
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

export instructions, previnst, nextinst

struct BasicBlockInstructionSet
    bb::BasicBlock
end

"""
    instructions(bb::BasicBlock)

Get an iterator over the instructions in the given basic block.
"""
instructions(bb::BasicBlock) = BasicBlockInstructionSet(bb)

Base.eltype(::BasicBlockInstructionSet) = Instruction

function Base.iterate(iter::BasicBlockInstructionSet,
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

"""
    previnst(inst::Instruction)

Get the instruction before the given instruction in the basic block, or `nothing` if there
is none.
"""
function previnst(inst::Instruction)
    ref = API.LLVMGetPreviousInstruction(inst)
    ref == C_NULL && return nothing
    Instruction(ref)
end

"""
    nextinst(inst::Instruction)

Get the instruction after the given instruction in the basic block, or `nothing` if there
is none.
"""
function nextinst(inst::Instruction)
    ref = API.LLVMGetNextInstruction(inst)
    ref == C_NULL && return nothing
    Instruction(ref)
end


## cfg-like operations

export predecessors, successors

"""
    predecessors(bb::BasicBlock)

Get the predecessors of the given basic block.
"""
function predecessors(bb::BasicBlock)
    preds = BasicBlock[]
    for use in uses(bb)
        inst = user(use)
        isterminator(inst) || continue
        push!(preds, parent(inst))
    end
    return preds
end

"""
    successors(bb::BasicBlock)

Get the successors of the given basic block.
"""
function successors(bb::BasicBlock)
    term = terminator(bb)
    term === nothing &&
        throw(ArgumentError("Cannot query successors of unterminated basic block"))
    successors(term)
end
