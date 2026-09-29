# Types

```@docs
LLVMType
issized
context(::LLVMType)
eltype(::LLVMType)
```

## Integer types

```@docs
LLVM.IntegerType
LLVM.IntType
width
```

## Floating-point types

```@docs
LLVM.HalfType
LLVM.BFloatType
LLVM.FloatType
LLVM.DoubleType
LLVM.FP128Type
LLVM.X86FP80Type
LLVM.PPCFP128Type
```

## Function types

```@docs
LLVM.FunctionType
isvararg
return_type
parameters(::LLVM.FunctionType)
```

## Pointer types

```@docs
LLVM.PointerType
addrspace
isopaque(::LLVM.PointerType)
```

## Array types

```@docs
LLVM.ArrayType
length(::LLVM.ArrayType)
isempty(::LLVM.ArrayType)
```

## Vector types

```@docs
LLVM.VectorType
length(::LLVM.VectorType)
```

## Structure types

```@docs
LLVM.StructType
name(::LLVM.StructType)
ispacked
isopaque(::LLVM.StructType)
elements!
elements
```

## Other types

```@docs
LLVM.VoidType
LLVM.LabelType
LLVM.MetadataType
LLVM.TokenType
```

## Type iteration

```@docs
types
```
