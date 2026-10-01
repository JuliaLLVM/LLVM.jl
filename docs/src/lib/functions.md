# Functions

```@docs
LLVM.Function
LLVM.Function(::LLVM.Module, ::AbstractString, ::LLVM.FunctionType)
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
EnumAttribute(::Union{Symbol,AbstractString}, ::Integer)
TypeAttribute(::Union{Symbol,AbstractString}, ::LLVMType)
StringAttribute(::AbstractString, ::AbstractString)
ConstantRangeAttribute
ConstantRangeListAttribute
```

## Memory effects

```@docs
MemoryEffects
FunctionMemoryEffects
EnumAttribute(::MemoryEffects)
LLVM.memory_attributes
```

## Intrinsics

```@docs
Intrinsic
tryparse(::Type{Intrinsic}, ::AbstractString)
parse(::Type{Intrinsic}, ::AbstractString)
isintrinsic
isoverloaded
LLVM.overloaded_name
LLVM.Function(::LLVM.Module, ::Intrinsic, ::Vector{<:LLVMType})
LLVM.FunctionType(::Intrinsic, ::Vector{<:LLVMType})
```
