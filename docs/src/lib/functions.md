# Functions

```@docs
LLVM.Function
LLVM.Function(::LLVM.Module, ::String, ::LLVM.FunctionType)
Argument
copy_attributes!
```

## Operations

```@docs
empty!
erase!(::LLVM.Function)
```

Functions are reordered using [`move!`](@ref).

## Attributes

```@docs
Attribute
EnumAttribute(::Union{Symbol,String}, ::Integer)
TypeAttribute(::Union{Symbol,String}, ::LLVMType)
StringAttribute(::AbstractString, ::AbstractString)
```

## Memory effects

```@docs
MemoryEffects
FunctionMemoryEffects
EnumAttribute(::MemoryEffects)
```

## Intrinsics

```@docs
Intrinsic
isintrinsic
isoverloaded
LLVM.overloaded_name
LLVM.Function(::LLVM.Module, ::Intrinsic, ::Vector{<:LLVMType})
LLVM.FunctionType(::Intrinsic, ::Vector{<:LLVMType})
```
