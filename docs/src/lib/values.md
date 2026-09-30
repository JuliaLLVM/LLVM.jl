# Values

## General APIs

```@docs
LLVM.Value
isconstant(::Value)
isundef
ispoison
isnull
take_name!
strip_pointer_casts
strip_pointer_casts_and_aliases
```

## User values

```@docs
LLVM.User
```

## Constant values

```@docs
LLVM.Constant
null
all_ones
PointerNull
UndefValue
PoisonValue
ConstantInt
convert(::Type, val::ConstantInt)
ConstantFP
convert(::Type{T}, val::ConstantFP) where {T<:AbstractFloat}
ConstantStruct
LLVM.ConstantDataSequential
ConstantDataArray
ConstantDataArray(::LLVMType, ::AbstractVector{T}) where {T <: Union{Integer, AbstractFloat}}
ConstantDataArray(::AbstractVector)
isstring
String(::ConstantDataArray)
ConstantDataVector
ConstantArray
ConstantArray(::LLVMType, ::AbstractArray{<:LLVM.Constant,N}) where {N}
ConstantArray(::AbstractArray)
ConstantVector
ConstantAggregateZero
LLVM.ConstantAggregate
InlineAsm
convert_users_to_instructions!
remove_dead_constant_users!
```

### Constant expressions

```@docs
LLVM.ConstantExpr
```

```@autodocs
Modules = [LLVM]
Filter = f -> f isa Function && startswith(string(nameof(f)), "const_")
```

## Global values

```@docs
LLVM.GlobalValue
LLVM.GlobalObject
isdeclaration
```

### Global variables

Global variables are a specific kind of global values, and have additional APIs:

```@docs
GlobalVariable
erase!(::GlobalVariable)
```

Global variables are reordered using [`move!`](@ref).

### Global aliases

```@docs
GlobalAlias
```

### Global ifuncs

```@docs
GlobalIFunc
erase!(::GlobalIFunc)
```

## Uses

```@docs
replace_uses!
replace_metadata_uses!
Use
```
