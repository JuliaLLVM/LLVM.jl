# Basic blocks

```@docs
BasicBlock
BasicBlock(name::String)
BasicBlock(f::LLVM.Function, name::String)
BasicBlock(bb::BasicBlock, name::String)
```

## Operations

```@docs
remove!(::BasicBlock)
erase!(::BasicBlock)
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
