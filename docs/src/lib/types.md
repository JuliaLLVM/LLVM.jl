# Types

```@docs
LLVMType
issized
eltype(::LLVMType)
```

## Integer types

```@docs
LLVM.IntegerType
LLVM.IntType
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
```

## Pointer types

```@docs
LLVM.PointerType
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
ispacked
isopaque(::LLVM.StructType)
elements!
```

## Other types

```@docs
LLVM.VoidType
LLVM.LabelType
LLVM.MetadataType
LLVM.TokenType
```
