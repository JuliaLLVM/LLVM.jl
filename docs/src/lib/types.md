# Types

```@docs
LLVMType
issized
```

## Integer types

```@docs
LLVM.IntegerType
LLVM.IntType
```

## Floating-point types

```@docs
LLVM.FloatingPointType
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
isemptytype
```

## Vector types

```@docs
LLVM.VectorType
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
