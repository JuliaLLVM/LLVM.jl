# Metadata

```@docs
LLVM.Metadata
MDString
MDString(::String)
convert(::Type{String}, ::MDString)
MDNode
MDTuple
MDNode(::Vector)
```

## Temporary nodes

```@docs
TemporaryMDNode
TemporaryMDNode(::AbstractVector)
replace_temporary!
dispose(::TemporaryMDNode)
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
NamedMDNode
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
```

### Variables

```@docs
DIVariable
LLVM.DILocalVariable
LLVM.DIGlobalVariable
```

### Scopes

```@docs
DIScope
DILocalScope
```

### File

```@docs
DIFile
LLVM.file!
```

### Lexical Block

```@docs
LLVM.DILexicalBlock
LLVM.DILexicalBlockFile
LLVM.lexical_block!
LLVM.lexical_block_file!
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
LLVM.DIBasicType
LLVM.DIDerivedType
LLVM.DICompositeType
LLVM.DISubroutineType
LLVM.DIEnumerator
LLVM.DISubrange
```

#### Properties

Built-in factories for primitive types:

```@docs
LLVM.basic_type!
LLVM.unspecified_type!
LLVM.nullptr_type!
```

Derived types (pointers, qualifiers, members, inheritance, ...):

```@docs
LLVM.pointer_type!
LLVM.reference_type!
LLVM.lvalue_reference_type!
LLVM.rvalue_reference_type!
LLVM.typedef_type!
LLVM.qualified_type!
LLVM.const_type!
LLVM.volatile_type!
LLVM.artificial_type!
LLVM.object_pointer_type!
LLVM.member_type!
LLVM.bitfield_member_type!
LLVM.static_member_type!
LLVM.member_pointer_type!
LLVM.inheritance!
```

Composite types:

```@docs
LLVM.struct_type!
LLVM.union_type!
LLVM.class_type!
LLVM.array_type!
LLVM.vector_type!
LLVM.enumerator!
LLVM.enumeration_type!
LLVM.forward_decl!
LLVM.replaceable_composite_type!
LLVM.get_or_create_subrange!
```

Subroutine types:

```@docs
LLVM.subroutine_type!
```

Array-node helpers:

```@docs
LLVM.get_or_create_array!
LLVM.get_or_create_type_array!
```

Objective-C:

```@docs
DIObjCProperty
LLVM.objc_ivar!
LLVM.objc_property!
```

### Subprogram

```@docs
DISubprogram
LLVM.subprogram!
```

### Variables

Factories for local variables and parameters:

```@docs
LLVM.auto_variable!
LLVM.parameter_variable!
```

### Expressions

```@docs
LLVM.DIExpression
LLVM.DIGlobalVariableExpression
LLVM.expression!
LLVM.constant_value_expression!
LLVM.global_variable_expression!
LLVM.temp_global_variable_fwd_decl!
```

### Imported entities

```@docs
LLVM.DIImportedEntity
LLVM.imported_module_from_namespace!
LLVM.imported_module_from_alias!
LLVM.imported_module_from_module!
LLVM.imported_declaration!
```

### Macros

```@docs
LLVM.DIMacro
LLVM.DIMacroFile
LLVM.macro!
LLVM.temp_macro_file!
```

### Instruction-level insertion

Debug records are inserted at an [`InsertionPoint`](@ref). The functions that insert them
return an `Instruction` on LLVM ≤ 18 (legacy `llvm.dbg.*` intrinsics) and a
`LLVM.DbgRecord` on LLVM ≥ 19 (the new `#dbg_*` record format). On LLVM 20 and later,
`dbg_label!(dib, label, location, pos)` inserts a label record.

```@docs
dbg_declare!
dbg_value!
```

The debug location of an instruction is available as its `debug_location` property (see
[`inst.debug_location`](@ref LLVM.Instruction)).

### Compile Unit

```@docs
DICompileUnit
LLVM.compile_unit!
```

### Other

```@docs
DEBUG_METADATA_VERSION
strip_debuginfo!
```

The debug info of modules and functions is available as their `debug_metadata_version`
and `subprogram` properties (see [`mod.debug_metadata_version`](@ref LLVM.Module) and
[`f.subprogram`](@ref LLVM.Function)).
