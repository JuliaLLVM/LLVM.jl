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
```

### Subprogram

```@docs
DISubProgram
line(::DISubProgram)
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
