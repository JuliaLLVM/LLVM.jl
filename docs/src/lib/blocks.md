# Basic blocks

```@docs
BasicBlock
BasicBlock(name::String)
BasicBlock(f::LLVM.Function, name::String)
BasicBlock(pos::InsertionPoint{BasicBlock}, name::String)
```

## Operations

```@docs
remove!(::BasicBlock)
erase!(::BasicBlock)
```

Basic blocks are moved using [`move!`](@ref).
