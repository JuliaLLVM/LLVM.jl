# Functions

```@docs
LLVM.Function
LLVM.Function(::LLVM.Module, ::String, ::LLVM.FunctionType)
Argument
```

## Operations

```@docs
empty!
erase!(::LLVM.Function)
move_before(::LLVM.Function, ::LLVM.Function)
move_after(::LLVM.Function, ::LLVM.Function)
```

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
