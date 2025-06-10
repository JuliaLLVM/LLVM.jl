# Values

## General APIs

```@docs
LLVM.Value
value_type
context(::Value)
name(::Value)
name!(::Value, ::String)
isconstant(::Value)
isundef
ispoison
isnull
```

## User values

```@docs
LLVM.User
operands(::LLVM.User)
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
ConstantDataArray
ConstantDataArray(::LLVMType, ::AbstractVector{T}) where {T <: Union{Integer, AbstractFloat}}
ConstantDataArray(::AbstractVector)
ConstantDataVector
ConstantArray
ConstantArray(::LLVMType, ::AbstractArray{<:LLVM.Constant,N}) where {N}
ConstantArray(::AbstractArray)
collect(::ConstantArray)
InlineAsm
LLVM.ConstantExpr
```

## Global values

```@docs
LLVM.GlobalValue
global_value_type
LLVM.parent(::LLVM.GlobalValue)
isdeclaration
linkage
linkage!
section
section!
visibility
visibility!
dllstorage
dllstorage!
unnamed_addr
unnamed_addr!
local_unnamed_addr
local_unnamed_addr!
```

### Global variables

Global variables are a specific kind of global values, and have additional APIs:

```@docs
GlobalVariable
erase!(::GlobalVariable)
initializer
initializer!
isthreadlocal
threadlocal!
threadlocalmode
threadlocalmode!
isconstant(::GlobalVariable)
constant!
isextinit
extinit!
alignment(::GlobalVariable)
alignment!(::GlobalVariable, ::Integer)
```

## Uses

```@docs
replace_uses!
replace_metadata_uses!
uses
Use
user
value
```
