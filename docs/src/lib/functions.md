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
function_attributes(::LLVM.Function)
parameter_attributes(::LLVM.Function, ::Integer)
return_attributes(::LLVM.Function)
```

## Memory effects

```@docs
MemoryEffects
FunctionMemoryEffects
EnumAttribute(::MemoryEffects)
```

## Parameters

```@docs
parameters
```

## Basic Blocks

```@docs
blocks
prevblock
nextblock
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
