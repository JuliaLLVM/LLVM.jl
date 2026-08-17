# Basic blocks

```@docs
BasicBlock
BasicBlock(name::String)
BasicBlock(f::LLVM.Function, name::String)
BasicBlock(bb::BasicBlock, name::String)
```

## Properties and operations

```@docs
remove!(::BasicBlock)
erase!(::BasicBlock)
LLVM.parent(::BasicBlock)
terminator(::BasicBlock)
name(::BasicBlock)
move_before(::BasicBlock, ::BasicBlock)
move_after(::BasicBlock, ::BasicBlock)
```

## Control flow

```@docs
predecessors(::BasicBlock)
successors(::BasicBlock)
```

## Instructions

```@docs
instructions
previnst
nextinst
```
