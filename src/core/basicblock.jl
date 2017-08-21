export BasicBlock, unsafe_delete!,
       parent, terminator, name,
       move_before, move_after

import Base: delete!

@checked immutable BasicBlock <: Value
    ref::reftype(Value)
end
identify(::Type{Value}, ::Val{API.LLVMBasicBlockValueKind}) = BasicBlock

# forward declarations
@checked immutable Function <: GlobalObject
    ref::reftype(GlobalObject)
end

BasicBlock(ref::API.LLVMBasicBlockRef) = BasicBlock(API.LLVMBasicBlockAsValue(ref))
blockref(bb::BasicBlock) = API.LLVMValueAsBasicBlock(ref(bb))

BasicBlock(f::Function, name::String) = 
    BasicBlock(API.LLVMAppendBasicBlock(ref(f), name))
BasicBlock(f::Function, name::String, ctx::Context) = 
    BasicBlock(API.LLVMAppendBasicBlockInContext(ref(ctx), ref(f), name))
BasicBlock(bb::BasicBlock, name::String) = 
    BasicBlock(API.LLVMInsertBasicBlock(blockref(bb), name))
BasicBlock(bb::BasicBlock, name::String, ctx::Context) = 
    BasicBlock(API.LLVMInsertBasicBlockInContext(ref(ctx), blockref(bb), name))

unsafe_delete!(::Function, bb::BasicBlock) = API.LLVMDeleteBasicBlock(blockref(bb))
delete!(::Function, bb::BasicBlock) =
    API.LLVMRemoveBasicBlockFromParent(blockref(bb))

parent(bb::BasicBlock) = Function(API.LLVMGetBasicBlockParent(blockref(bb)))

terminator(bb::BasicBlock) = Instruction(API.LLVMGetBasicBlockTerminator(blockref(bb)))

name(bb::BasicBlock) =
    unsafe_string(API.LLVMGetBasicBlockName(blockref(bb)))

move_before(bb::BasicBlock, pos::BasicBlock) =
    API.LLVMMoveBasicBlockBefore(blockref(bb), blockref(pos))
move_after(bb::BasicBlock, pos::BasicBlock) =
    API.LLVMMoveBasicBlockAfter(blockref(bb), blockref(pos))


## instruction iteration

export instructions

import Base: eltype, start, next, done, last, iteratorsize

immutable BasicBlockInstructionSet
    bb::BasicBlock
end

instructions(bb::BasicBlock) = BasicBlockInstructionSet(bb)

eltype(::BasicBlockInstructionSet) = Instruction

start(iter::BasicBlockInstructionSet) = API.LLVMGetFirstInstruction(blockref(iter.bb))

next(::BasicBlockInstructionSet, state) =
    (Instruction(state), API.LLVMGetNextInstruction(state))

done(::BasicBlockInstructionSet, state) = state == C_NULL

last(iter::BasicBlockInstructionSet) =
    Instruction(API.LLVMGetLastInstruction(blockref(iter.bb)))

iteratorsize(::BasicBlockInstructionSet) = Base.SizeUnknown()
