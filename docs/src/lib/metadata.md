# Metadata

```@docs
LLVM.Metadata
MDString
MDString(::String)
convert(::Type{String}, ::MDString)
MDNode
operands(::MDNode)
MDTuple
MDNode(::Vector)
```

## Metadata <-> Value

```@docs
LLVM.ValueAsMetadata
Metadata(::Value)
LLVM.MetadataAsValue
Value(::Metadata)
```

## Inspecting and attaching

```@docs
metadata(::Union{Instruction, LLVM.GlobalObject})
NamedMDNode
metadata(::LLVM.Module)
name(::NamedMDNode)
operands(::NamedMDNode)
push!(::NamedMDNode, ::MDNode)
```

## Debug information

```@docs
DINode
```

### Builder

```@docs
DIBuilder
DIBuilder(::LLVM.Module)
dispose(::DIBuilder)
LLVM.finalize!(::DIBuilder)
LLVM.finalize_subprogram!
```

### Location information

```@docs
DILocation
line(::DILocation)
column
scope(::DILocation)
inlined_at
```

### Variables

```@docs
DIVariable
LLVM.DILocalVariable
LLVM.DIGlobalVariable
file(::DIVariable)
scope(::DIVariable)
line(::DIVariable)
```

### Scopes

```@docs
DIScope
file(::DIScope)
name(::DIScope)
```

### File

```@docs
DIFile
directory
filename
source
LLVM.file!
```

### Lexical Block

```@docs
LLVM.DILexicalBlock
LLVM.DILexicalBlockFile
LLVM.lexicalblock!
LLVM.lexicalblockfile!
```

### Module

```@docs
LLVM.DIModule
LLVM.dimodule!
```

### Namespace

```@docs
LLVM.DINamespace
LLVM.namespace!
```

### Type

```@docs
DIType
name(::DIType)
Base.sizeof(::DIType)
offset(::DIType)
line(::DIType)
flags(::DIType)
LLVM.align
LLVM.DIEnumerator
LLVM.DISubrange
```

Built-in factories for primitive types:

```@docs
LLVM.basictype!
LLVM.unspecifiedtype!
LLVM.nullptrtype!
```

Derived types (pointers, qualifiers, members, inheritance, ...):

```@docs
LLVM.pointertype!
LLVM.referencetype!
LLVM.typedeftype!
LLVM.qualifiedtype!
LLVM.artificialtype!
LLVM.objectpointertype!
LLVM.membertype!
LLVM.bitfieldmembertype!
LLVM.staticmembertype!
LLVM.memberpointertype!
LLVM.inheritance!
```

Composite types:

```@docs
LLVM.structtype!
LLVM.uniontype!
LLVM.classtype!
LLVM.arraytype!
LLVM.vectortype!
LLVM.enumerator!
LLVM.enumerationtype!
LLVM.forwarddecl!
LLVM.replaceablecompositetype!
LLVM.getorcreatesubrange!
```

Subroutine types:

```@docs
LLVM.subroutinetype!
```

### Subprogram

```@docs
DISubProgram
line(::DISubProgram)
LLVM.subprogram!(::DIBuilder, ::DIScope, ::AbstractString, ::AbstractString, ::DIFile, ::Integer, ::LLVM.DISubroutineType, ::Integer)
```

### Variables

Factories for local variables and parameters:

```@docs
LLVM.autovariable!
LLVM.parametervariable!
```

### Expressions

```@docs
LLVM.DIExpression
LLVM.DIGlobalVariableExpression
LLVM.expression!
LLVM.constantvalueexpression!
LLVM.variable
LLVM.expression
LLVM.globalvariableexpression!
LLVM.tempglobalvariablefwddecl!
```

### Instruction-level insertion

The `declare_*!` / `value_*!` methods return an `Instruction` on LLVM ≤ 18
(legacy `llvm.dbg.*` intrinsics) and a `LLVM.DbgRecord` on LLVM ≥ 19
(the new `#dbg_*` record format).

```@docs
LLVM.declare_before!
LLVM.declare_at_end!
LLVM.value_before!
LLVM.value_at_end!
LLVM.debuglocation(::Instruction)
LLVM.debuglocation!(::Instruction, ::DILocation)
```

### Compile Unit

```@docs
DICompileUnit
LLVM.compileunit!
```

### Other

```@docs
DEBUG_METADATA_VERSION
strip_debuginfo!
subprogram(::LLVM.Function)
subprogram!
```
