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
LLVM.ScalableVectorType
```

## Structure types

```@docs
LLVM.StructType
ispacked
isopaque(::LLVM.StructType)
elements!
```

## Target extension types

Available with LLVM 16 and later.

```@autodocs
Modules = [LLVM]
Filter = t -> isdefined(LLVM, :TargetExtType) && t === LLVM.TargetExtType
```

## Other types

```@docs
LLVM.VoidType
LLVM.LabelType
LLVM.MetadataType
LLVM.TokenType
LLVM.X86AMXType
```

`LLVM.X86MMXType` (LLVM 19 and earlier):

```@autodocs
Modules = [LLVM]
Filter = t -> isdefined(LLVM, :X86MMXType) && t === LLVM.X86MMXType
```
