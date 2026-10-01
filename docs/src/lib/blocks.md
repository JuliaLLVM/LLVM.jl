# Basic blocks

```@docs
BasicBlock
BasicBlock(name::AbstractString)
BasicBlock(f::LLVM.Function, name::AbstractString)
BasicBlock(pos::InsertionPoint{BasicBlock}, name::AbstractString)
```

## Operations

```@docs
remove!(::BasicBlock)
erase!(::BasicBlock)
```

Basic blocks are moved using [`move!`](@ref).
