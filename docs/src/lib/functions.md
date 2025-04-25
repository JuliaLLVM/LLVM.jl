# Functions

```@docs
LLVM.Function
LLVM.Function(::LLVM.Module, ::String, ::LLVM.FunctionType)
```

## Properties and operations

```@docs
function_type
empty!
erase!(::LLVM.Function)
personality
personality!
callconv
callconv!
gc
gc!
entry
```

## Attributes

```@docs
function_attributes(::LLVM.Function)
parameter_attributes(::LLVM.Function, ::Integer)
return_attributes(::LLVM.Function)
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
isintrinsic
isoverloaded
name(::Intrinsic)
name(::Intrinsic, ::Vector{<:LLVMType})
LLVM.Function(::LLVM.Module, ::Intrinsic, ::Vector{<:LLVMType})
LLVM.FunctionType(::Intrinsic, ::Vector{<:LLVMType})
```
