## debug info builder

@vocabulary Build DIBuilder, finalize!

"""
    DIBuilder

A builder for constructing debug information metadata.

This object needs to be disposed of using [`dispose`](@ref), which also
finalizes the debug info. Call [`finalize!`](@ref) explicitly only if you
need to use the finalized debug info (e.g. emit code) *before* disposing of
the builder.
"""
@checked mutable struct DIBuilder
    ref::API.LLVMDIBuilderRef
    needs_finalization::Bool
end

Base.unsafe_convert(::Type{API.LLVMDIBuilderRef}, builder::DIBuilder) =
    mark_use(builder).ref

"""
    DIBuilder(mod::Module; allow_unresolved::Bool=true)

Create a new debug info builder that emits metadata into `mod`.

When `allow_unresolved` is `true` (the default), the builder collects unresolved
metadata nodes attached to the module so that cycles can be resolved during
[`dispose`](@ref). When `false`, the builder errors on unresolved nodes instead.
"""
function DIBuilder(mod::Module; allow_unresolved::Bool=true)
    ref = allow_unresolved ? API.LLVMCreateDIBuilder(mod) :
                             API.LLVMCreateDIBuilderDisallowUnresolved(mod)
    mark_alloc(DIBuilder(ref, false))
end

"""
    dispose(builder::DIBuilder)

Finalize the debug info and dispose of the builder. Finalization populates
the compile unit's enum/retained-type/global/imported-entity/macro arrays,
seals each subprogram's retained-nodes list, and resolves remaining cycles.
If no compile unit was registered with the builder, or the debug info was
already finalized through [`finalize!`](@ref), finalization is skipped:
`DIBuilder::finalize` is not idempotent (e.g., re-finalizing accesses
already-deleted temporary macro files).
"""
function dispose(builder::DIBuilder)
    finalize!(builder)
    mark_dispose(API.LLVMDisposeDIBuilder, builder)
end

function DIBuilder(f::Core.Function, args...; kwargs...)
    builder = DIBuilder(args...; kwargs...)
    try
        f(builder)
    finally
        dispose(builder)
    end
end

Base.show(io::IO, builder::DIBuilder) = @printf(io, "DIBuilder(%p)", builder.ref)

"""
    finalize!(builder::DIBuilder)

Resolve any unresolved metadata nodes and mark all compile units finalized.
Called automatically by [`dispose`](@ref); call explicitly only if the
DI-enriched module must be consumed (e.g. for code emission) before the
builder is disposed of. Skipped if no compile unit has been registered, or
if the debug info has already been finalized.
"""
function finalize!(builder::DIBuilder)
    if builder.needs_finalization
        API.LLVMDIBuilderFinalize(builder)
        builder.needs_finalization = false
    end
    return
end


## nodes

@vocabulary IR DINode

"""
    DINode

a tagged DWARF-like metadata node.

# Properties

    node.tag

The DWARF tag of the node, or `0` if it has none. Requires LLVM 17+.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
abstract type DINode <: MDNode end


# LLVM line numbers are unsigned, with 0 meaning "no line". Julia's codegen additionally
# emits -1 (all ones) for an unknown line, which its DWARF reader reads back as a signed
# `int`. Return that as -1, like `Base.StackTraces` does, instead of 4294967295 or, on
# 32-bit platforms, an InexactError.
line_number(x::Cuint) = x == typemax(Cuint) ? -1 : Int(x)


## variables

@vocabulary IR DIVariable

"""
    DIVariable

Abstract supertype for all variable-like metadata nodes.

# Properties

    var.file

The file in which the variable is declared, or `nothing` if unknown.

    var.scope

The scope of the variable, or `nothing` if unknown. The scope of a local variable is a
[`DILocalScope`](@ref).

    var.line

The line number at which the variable is declared, or -1 if unknown.

The properties of [`DINode`](@ref LLVM.DINode) and [`MDNode`](@ref LLVM.MDNode) are
available too.
"""
abstract type DIVariable <: DINode end

for var in (:Local, :Global)
    var_name = Symbol("DI$(var)Variable")
    var_kind = Symbol("LLVM$(var_name)MetadataKind")
    @eval begin
        @checked struct $var_name <: DIVariable
            ref::API.LLVMMetadataRef
        end
        register($var_name, API.$var_kind)
    end
end

"""
    DILocalVariable <: DIVariable

A local variable in the source code.
"""
DILocalVariable

"""
    DIGlobalVariable <: DIVariable

A global variable in the source code.
"""
DIGlobalVariable

@vocabulary IR DILocalVariable, DIGlobalVariable

function file(var::DIVariable)
    ref = API.LLVMDIVariableGetFile(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIFile
end

function scope(var::DIVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

function scope(var::DILocalVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DILocalScope
end

line(var::DIVariable) = line_number(API.LLVMDIVariableGetLine(var))

@property DIVariable file
@property DIVariable scope
@property DIVariable line


## scopes

@vocabulary IR DIScope

"""
    DIScope

Abstract supertype for lexical scopes and types (which are also declaration contexts).

# Properties

    scope.file

The file associated with the scope.

    scope.name

The name of the scope, or `nothing` if it has none.

The properties of [`DINode`](@ref LLVM.DINode) and [`MDNode`](@ref LLVM.MDNode) are
available too.
"""
abstract type DIScope <: DINode end

file(scope::DIScope) = DIFile(API.LLVMDIScopeGetFile(scope))

function name(scope::DIScope)
    len = Ref{Cuint}()
    data = API.LLVMDIScopeGetName(scope, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property DIScope file
@property DIScope name

@vocabulary IR DILocalScope

"""
    DILocalScope

Abstract supertype for scopes that can contain local variables, labels and source
locations: subprograms ([`DISubprogram`](@ref)) and the lexical blocks nested in them
([`LLVM.DILexicalBlock`](@ref) and [`LLVM.DILexicalBlockFile`](@ref)).

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available.
"""
abstract type DILocalScope <: DIScope end


## location information

@vocabulary IR DILocation

"""
    DILocation

A location in the source code.

# Properties

    loc.line

The line number of the debug location, or -1 if unknown.

    loc.column

The column number of the debug location.

    loc.scope

The local scope of the debug location, a [`DILocalScope`](@ref).

    loc.inlined_at

The location that the code at this debug location has been inlined at, or `nothing` if it
hasn't been inlined.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DILocation <: MDNode
    ref::API.LLVMMetadataRef
end
register(DILocation, API.LLVMDILocationMetadataKind)

"""
    DILocation(line::Integer, col::Integer, scope::DILocalScope,
               [inlined_at::DILocation]) -> DILocation

Creates a new debug location that describes a source location in the local scope `scope`,
e.g., a [`DISubprogram`](@ref).
"""
function DILocation(line::Integer, col::Integer, scope::DILocalScope,
                    inlined_at::Union{DILocation,Nothing}=nothing)
    DILocation(API.LLVMDIBuilderCreateDebugLocation(context(), line, col, scope,
                                                    something(inlined_at, C_NULL)))
end

line(location::DILocation) = line_number(API.LLVMDILocationGetLine(location))

column(location::DILocation) = Int(API.LLVMDILocationGetColumn(location))

function scope(location::DILocation)
    ref = API.LLVMDILocationGetScope(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocalScope
end

function inlined_at(location::DILocation)
    ref = API.LLVMDILocationGetInlinedAt(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end

@property DILocation line
@property DILocation column
@property DILocation scope
@property DILocation inlined_at


## file

@vocabulary IR DIFile
@vocabulary Build file!

"""
    DIFile

A file in the source code.

# Properties

    file.directory

The directory of the file.

    file.filename

The name of the file.

    file.source

The source code of the file, or `nothing` if it is not available.

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DIFile <: DIScope
    ref::API.LLVMMetadataRef
end
register(DIFile, API.LLVMDIFileMetadataKind)

"""
    file!(builder::DIBuilder, filename::AbstractString, directory::AbstractString) -> DIFile

Create a new [`DIFile`](@ref) describing the given source file.
"""
function file!(builder::DIBuilder, filename::AbstractString, directory::AbstractString)
    DIFile(API.LLVMDIBuilderCreateFile(builder,
                                       filename, Csize_t(ncodeunits(filename)),
                                       directory, Csize_t(ncodeunits(directory))))
end

function directory(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetDirectory(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

function filename(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetFilename(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

function source(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetSource(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property DIFile directory
@property DIFile filename
@property DIFile source


## type

@vocabulary IR DIType, DIEnumerator, DISubrange
@vocabulary Build basic_type!, unspecified_type!, pointer_type!, reference_type!, nullptr_type!,
        typedef_type!, qualified_type!, artificial_type!, object_pointer_type!,
        inheritance!, member_type!, bitfield_member_type!, static_member_type!,
        member_pointer_type!, struct_type!, union_type!, class_type!, array_type!,
        vector_type!, enumeration_type!, enumerator!, forward_decl!,
        replaceable_composite_type!, subroutine_type!, get_or_create_subrange!

"""
    DIType

Abstract supertype for all type-like metadata nodes.

# Properties

    typ.name

The name of the type, or `nothing` if it has none.

    typ.size_in_bits

The size in bits of the type.

    typ.offset_in_bits

The offset in bits of the type, e.g., of a member within its structure.

    typ.line

The line number at which the type is declared, or -1 if unknown.

    typ.flags

The flags of the type, as an `LLVM.API.LLVMDIFlags` bitmask.

    typ.align_in_bits

The alignment in bits of the type, or `0` if it has none.

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available too.
"""
abstract type DIType <: DIScope end

for typ in (:Basic, :Derived, :Composite, :Subroutine)
    typ_name = Symbol("DI$(typ)Type")
    typ_kind = Symbol("LLVM$(typ_name)MetadataKind")
    @eval begin
        @checked struct $typ_name <: DIType
            ref::API.LLVMMetadataRef
        end
        register($typ_name, API.$typ_kind)
    end
end

"""
    DIBasicType <: DIType

A primitive type (integer, floating-point, boolean, ...), built with
[`basic_type!`](@ref).
"""
DIBasicType

"""
    DIDerivedType <: DIType

A type derived from another type by adding qualifiers, reference/pointer
indirection, a typedef name, or by describing a member/field. Built with
[`pointer_type!`](@ref), [`typedef_type!`](@ref), [`member_type!`](@ref), and
the other qualifier/member factories.
"""
DIDerivedType

"""
    DICompositeType <: DIType

An aggregate type (struct, union, class, array, vector, enumeration, ...).
Built with [`struct_type!`](@ref), [`union_type!`](@ref),
[`array_type!`](@ref), etc.
"""
DICompositeType

"""
    DISubroutineType <: DIType

A function/subroutine type, listing the return and parameter types. Built
with [`subroutine_type!`](@ref).
"""
DISubroutineType

@vocabulary IR DIBasicType, DIDerivedType, DICompositeType, DISubroutineType

"""
    DIEnumerator

A single enumerator value in an enumeration type.
"""
@checked struct DIEnumerator <: DINode
    ref::API.LLVMMetadataRef
end
register(DIEnumerator, API.LLVMDIEnumeratorMetadataKind)

"""
    DISubrange

A subrange describing one dimension of an array or vector type.
"""
@checked struct DISubrange <: DINode
    ref::API.LLVMMetadataRef
end
register(DISubrange, API.LLVMDISubrangeMetadataKind)

function name(typ::DIType)
    len = Ref{Csize_t}()
    data = API.LLVMDITypeGetName(typ, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

size_in_bits(typ::DIType) = Int(API.LLVMDITypeGetSizeInBits(typ))

offset_in_bits(typ::DIType) = Int(API.LLVMDITypeGetOffsetInBits(typ))

line(typ::DIType) = line_number(API.LLVMDITypeGetLine(typ))

flags(typ::DIType) = API.LLVMDITypeGetFlags(typ)

align_in_bits(typ::DIType) = Int(API.LLVMDITypeGetAlignInBits(typ))

@property DIType name
@property DIType size_in_bits
@property DIType offset_in_bits
@property DIType align_in_bits
@property DIType line
@property DIType flags

@static if version() >= v"17"
tag(node::DINode) = Int(API.LLVMGetDINodeTag(node))
@property DINode tag
end


# basic types

"""
    basic_type!(builder::DIBuilder, name::AbstractString, size_in_bits::Integer,
               encoding::Integer; flags=API.LLVMDIFlagZero) -> DIBasicType

Create a new [`DIBasicType`](@ref), such as an integer or floating-point type.
`encoding` is a `DW_ATE_*` value (see the DWARF standard).
"""
function basic_type!(builder::DIBuilder, name::AbstractString, size_in_bits::Integer,
                    encoding::Integer; flags=API.LLVMDIFlagZero)
    DIBasicType(API.LLVMDIBuilderCreateBasicType(
        builder, name, Csize_t(ncodeunits(name)),
        UInt64(size_in_bits), Cuint(encoding), flags))
end

"""
    unspecified_type!(builder::DIBuilder, name::AbstractString) -> DIBasicType

Create a new unspecified type (`DW_TAG_unspecified_type`), e.g. a C++ `decltype(nullptr)`.
"""
function unspecified_type!(builder::DIBuilder, name::AbstractString)
    DIBasicType(API.LLVMDIBuilderCreateUnspecifiedType(
        builder, name, Csize_t(ncodeunits(name))))
end


# derived types

"""
    pointer_type!(builder::DIBuilder, pointee_type::DIType, size_in_bits::Integer;
                 align_in_bits::Integer=0, address_space::Integer=0,
                 name::AbstractString="") -> DIDerivedType

Create a new pointer type.
"""
function pointer_type!(builder::DIBuilder, pointee_type::DIType, size_in_bits::Integer;
                      align_in_bits::Integer=0, address_space::Integer=0,
                      name::AbstractString="")
    DIDerivedType(API.LLVMDIBuilderCreatePointerType(
        builder, pointee_type,
        UInt64(size_in_bits), UInt32(align_in_bits), Cuint(address_space),
        name, Csize_t(ncodeunits(name))))
end

"""
    reference_type!(builder::DIBuilder, tag::Integer, type::DIType) -> DIDerivedType

Create a new reference type (C++ `T&` / `T&&`), with the given DWARF `tag`
(e.g. `DW_TAG_reference_type` or `DW_TAG_rvalue_reference_type`).
"""
function reference_type!(builder::DIBuilder, tag::Integer, type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateReferenceType(builder, Cuint(tag), type))
end

"""
    nullptr_type!(builder::DIBuilder) -> DIBasicType

Create a new type representing a null pointer.
"""
nullptr_type!(builder::DIBuilder) =
    DIBasicType(API.LLVMDIBuilderCreateNullPtrType(builder))

"""
    typedef_type!(builder::DIBuilder, type::DIType, name::AbstractString,
                 file::DIFile, line::Integer, scope::DIScope;
                 align_in_bits::Integer=0) -> DIDerivedType

Create a new typedef type.
"""
function typedef_type!(builder::DIBuilder, type::DIType, name::AbstractString,
                      file::DIFile, line::Integer, scope::DIScope;
                      align_in_bits::Integer=0)
    DIDerivedType(API.LLVMDIBuilderCreateTypedef(
        builder, type, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), scope, UInt32(align_in_bits)))
end

"""
    qualified_type!(builder::DIBuilder, tag::Integer, type::DIType) -> DIDerivedType

Create a new qualified type, such as `const T` (`DW_TAG_const_type`) or
`volatile T` (`DW_TAG_volatile_type`). See also the named convenience
wrappers [`const_type!`](@ref) and [`volatile_type!`](@ref).
"""
function qualified_type!(builder::DIBuilder, tag::Integer, type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateQualifiedType(builder, Cuint(tag), type))
end

@vocabulary Build const_type!, volatile_type!, lvalue_reference_type!, rvalue_reference_type!

# DWARF tag values used by the convenience wrappers below. Not exported; part
# of a wider DWARF-constants cleanup.
const _DW_TAG_reference_type        = 0x10
const _DW_TAG_const_type            = 0x26
const _DW_TAG_volatile_type         = 0x35
const _DW_TAG_rvalue_reference_type = 0x42

"""
    const_type!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a `const`-qualified type. Shorthand for
`qualified_type!(builder, DW_TAG_const_type, type)`.
"""
const_type!(builder::DIBuilder, type::DIType) =
    qualified_type!(builder, _DW_TAG_const_type, type)

"""
    volatile_type!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a `volatile`-qualified type. Shorthand for
`qualified_type!(builder, DW_TAG_volatile_type, type)`.
"""
volatile_type!(builder::DIBuilder, type::DIType) =
    qualified_type!(builder, _DW_TAG_volatile_type, type)

"""
    lvalue_reference_type!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a C++ `T&` reference type. Shorthand for
`reference_type!(builder, DW_TAG_reference_type, type)`.
"""
lvalue_reference_type!(builder::DIBuilder, type::DIType) =
    reference_type!(builder, _DW_TAG_reference_type, type)

"""
    rvalue_reference_type!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a C++ `T&&` rvalue-reference type. Shorthand for
`reference_type!(builder, DW_TAG_rvalue_reference_type, type)`.
"""
rvalue_reference_type!(builder::DIBuilder, type::DIType) =
    reference_type!(builder, _DW_TAG_rvalue_reference_type, type)

"""
    artificial_type!(builder::DIBuilder, type::DIType) -> DIType

Create a new artificial type (`DI_FLAG_ARTIFICIAL`), e.g. an implicit `this`.
The concrete subtype matches the input (e.g. a `DIBasicType` stays a
`DIBasicType`).
"""
artificial_type!(builder::DIBuilder, type::DIType) =
    Metadata(API.LLVMDIBuilderCreateArtificialType(builder, type))::DIType

"""
    object_pointer_type!(builder::DIBuilder, type::DIType;
                        implicit::Bool=true) -> DIType

Create a new type identifying an object pointer (`DI_FLAG_OBJECT_POINTER`).
The concrete subtype matches the input. On LLVM 20+ an `implicit::Bool` keyword
is accepted: when `true` (the default, matching LLVM ≤ 19 behavior) the clone
also sets `DI_FLAG_ARTIFICIAL`.
"""
object_pointer_type!

@static if version() >= v"20"
object_pointer_type!(builder::DIBuilder, type::DIType; implicit::Bool=true) =
    Metadata(API.LLVMDIBuilderCreateObjectPointerType(builder, type, implicit))::DIType
else
object_pointer_type!(builder::DIBuilder, type::DIType) =
    Metadata(API.LLVMDIBuilderCreateObjectPointerType(builder, type))::DIType
end # @static

"""
    inheritance!(builder::DIBuilder, derived::DIType, base::DIType,
                 base_offset::Integer, vbptr_offset::Integer=0;
                 flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new inheritance relationship from `derived` to `base`.
"""
function inheritance!(builder::DIBuilder, derived::DIType, base::DIType,
                      base_offset::Integer, vbptr_offset::Integer=0;
                      flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateInheritance(
        builder, derived, base, UInt64(base_offset), UInt32(vbptr_offset), flags))
end

"""
    member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, offset_in_bits::Integer,
                type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new member (field) of a composite type.
"""
function member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, offset_in_bits::Integer,
                     type::DIType; flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateMemberType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags, type))
end

"""
    bitfield_member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                        file::DIFile, line::Integer, size_in_bits::Integer,
                        offset_in_bits::Integer, storage_offset_in_bits::Integer,
                        type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new bit-field member of a composite type.
"""
function bitfield_member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                             file::DIFile, line::Integer, size_in_bits::Integer,
                             offset_in_bits::Integer, storage_offset_in_bits::Integer,
                             type::DIType; flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateBitFieldMemberType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt64(offset_in_bits), UInt64(storage_offset_in_bits),
        flags, type))
end

"""
    static_member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                      file::DIFile, line::Integer, type::DIType,
                      constant_val::Constant;
                      flags=API.LLVMDIFlagZero,
                      align_in_bits::Integer=0) -> DIDerivedType

Create a new static member of a composite type. `constant_val` is required:
the underlying C entry point unconditionally `cast<Constant>`s it and
crashes on null.
"""
function static_member_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                           file::DIFile, line::Integer, type::DIType,
                           constant_val::Constant;
                           flags=API.LLVMDIFlagZero,
                           align_in_bits::Integer=0)
    DIDerivedType(API.LLVMDIBuilderCreateStaticMemberType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), type, flags,
        constant_val, UInt32(align_in_bits)))
end

"""
    member_pointer_type!(builder::DIBuilder, pointee_type::DIType, class_type::DIType,
                       size_in_bits::Integer;
                       align_in_bits::Integer=0,
                       flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new pointer-to-member type for C++.
"""
function member_pointer_type!(builder::DIBuilder, pointee_type::DIType, class_type::DIType,
                            size_in_bits::Integer;
                            align_in_bits::Integer=0,
                            flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateMemberPointerType(
        builder, pointee_type, class_type,
        UInt64(size_in_bits), UInt32(align_in_bits), flags))
end


# composite types

"""
    struct_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                flags=API.LLVMDIFlagZero, derived_from=nothing,
                runtime_lang::Integer=0, vtable_holder=nothing,
                unique_id::AbstractString="") -> DICompositeType

Create a new struct type.
"""
function struct_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                     flags=API.LLVMDIFlagZero, derived_from=nothing,
                     runtime_lang::Integer=0, vtable_holder=nothing,
                     unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateStructType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        something(derived_from, C_NULL),
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        something(vtable_holder, C_NULL),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    union_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, elements::AbstractVector{<:Metadata};
               flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
               unique_id::AbstractString="") -> DICompositeType

Create a new union type.
"""
function union_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                    flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
                    unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateUnionType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    class_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, offset_in_bits::Integer,
               elements::AbstractVector{<:Metadata};
               flags=API.LLVMDIFlagZero, derived_from=nothing,
               vtable_holder=nothing, template_params=nothing,
               unique_id::AbstractString="") -> DICompositeType

Create a new C++ class type.
"""
function class_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, offset_in_bits::Integer,
                    elements::AbstractVector{<:Metadata};
                    flags=API.LLVMDIFlagZero, derived_from=nothing,
                    vtable_holder=nothing, template_params=nothing,
                    unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateClassType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags,
        something(derived_from, C_NULL),
        elts, Cuint(length(elts)),
        something(vtable_holder, C_NULL),
        something(template_params, C_NULL),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    array_type!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
               element_type::DIType, subscripts::AbstractVector{<:Metadata}) -> DICompositeType

Create a new array type. Subscripts are typically built with
[`get_or_create_subrange!`](@ref).
"""
function array_type!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                    element_type::DIType, subscripts::AbstractVector{<:Metadata})
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateArrayType(
        builder, UInt64(size), UInt32(align_in_bits),
        element_type, subs, Cuint(length(subs))))
end

"""
    vector_type!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                element_type::DIType, subscripts::AbstractVector{<:Metadata}) -> DICompositeType

Create a new vector type. Subscripts are typically built with
[`get_or_create_subrange!`](@ref).
"""
function vector_type!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                     element_type::DIType, subscripts::AbstractVector{<:Metadata})
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateVectorType(
        builder, UInt64(size), UInt32(align_in_bits),
        element_type, subs, Cuint(length(subs))))
end

"""
    enumerator!(builder::DIBuilder, name::AbstractString, value::Integer;
                unsigned::Bool=false) -> DIEnumerator

Create a new enumerator for use inside an enumeration type.
"""
function enumerator!(builder::DIBuilder, name::AbstractString, value::Integer;
                     unsigned::Bool=false)
    DIEnumerator(API.LLVMDIBuilderCreateEnumerator(
        builder, name, Csize_t(ncodeunits(name)), Int64(value), unsigned))
end

"""
    enumeration_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                     class_ty=nothing) -> DICompositeType

Create a new enumeration type. `elements` should be a vector of
[`DIEnumerator`](@ref) metadata nodes.
"""
function enumeration_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                          file::DIFile, line::Integer, size_in_bits::Integer,
                          align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                          class_ty=nothing)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateEnumerationType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits),
        elts, Cuint(length(elts)),
        something(class_ty, C_NULL)))
end

"""
    forward_decl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                 scope::DIScope, file::DIFile, line::Integer;
                 runtime_lang::Integer=0, size_in_bits::Integer=0,
                 align_in_bits::Integer=0,
                 unique_id::AbstractString="") -> DICompositeType

Create a new forward declaration to a composite type.
"""
function forward_decl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                      scope::DIScope, file::DIFile, line::Integer;
                      runtime_lang::Integer=0, size_in_bits::Integer=0,
                      align_in_bits::Integer=0,
                      unique_id::AbstractString="")
    DICompositeType(API.LLVMDIBuilderCreateForwardDecl(
        builder, Cuint(tag), name, Csize_t(ncodeunits(name)),
        scope, file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    replaceable_composite_type!(builder::DIBuilder, tag::Integer,
                              name::AbstractString, scope::DIScope,
                              file::DIFile, line::Integer;
                              runtime_lang::Integer=0, size_in_bits::Integer=0,
                              align_in_bits::Integer=0,
                              flags=API.LLVMDIFlagZero,
                              unique_id::AbstractString="") -> DICompositeType

Create a new replaceable composite type forward declaration.
"""
function replaceable_composite_type!(builder::DIBuilder, tag::Integer,
                                   name::AbstractString, scope::DIScope,
                                   file::DIFile, line::Integer;
                                   runtime_lang::Integer=0, size_in_bits::Integer=0,
                                   align_in_bits::Integer=0,
                                   flags=API.LLVMDIFlagZero,
                                   unique_id::AbstractString="")
    DICompositeType(API.LLVMDIBuilderCreateReplaceableCompositeType(
        builder, Cuint(tag), name, Csize_t(ncodeunits(name)),
        scope, file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        unique_id, Csize_t(ncodeunits(unique_id))))
end


# subroutine types

"""
    subroutine_type!(builder::DIBuilder, file::DIFile,
                    return_type::Union{DIType,Nothing},
                    parameter_types::AbstractVector{<:Metadata}=Metadata[];
                    flags=API.LLVMDIFlagZero) -> DISubroutineType

Create a new subroutine type with the given return and parameter types. Pass
`nothing` for `return_type` to describe a `void`-returning subroutine.
"""
function subroutine_type!(builder::DIBuilder, file::DIFile,
                         return_type::Union{DIType,Nothing},
                         parameter_types::AbstractVector{<:Metadata}=Metadata[];
                         flags=API.LLVMDIFlagZero)
    # LLVM packs the return type as the 0th element of the parameter-types array,
    # with a null entry standing for `void`.
    params = API.LLVMMetadataRef[
        return_type === nothing ? C_NULL :
            Base.unsafe_convert(API.LLVMMetadataRef, return_type)]
    for p in parameter_types
        push!(params, Base.unsafe_convert(API.LLVMMetadataRef, p))
    end
    DISubroutineType(API.LLVMDIBuilderCreateSubroutineType(
        builder, file, params, Cuint(length(params)), flags))
end


# subrange / array helpers

@vocabulary Build get_or_create_array!, get_or_create_type_array!

"""
    get_or_create_subrange!(builder::DIBuilder, lower_bound::Integer, count::Integer)

Get or create a subrange metadata node, describing one dimension of an array
or vector type.
"""
get_or_create_subrange!(builder::DIBuilder, lower_bound::Integer, count::Integer) =
    DISubrange(API.LLVMDIBuilderGetOrCreateSubrange(
        builder, Int64(lower_bound), Int64(count)))

"""
    get_or_create_array!(builder::DIBuilder, elements::AbstractVector{<:Metadata})

Get or create a generic metadata array node, used for lists such as
`elements` fields of composite types.
"""
function get_or_create_array!(builder::DIBuilder, elements::AbstractVector{<:Metadata})
    elts = convert(Vector{Metadata}, elements)
    Metadata(API.LLVMDIBuilderGetOrCreateArray(builder, elts, Csize_t(length(elts))))
end

"""
    get_or_create_type_array!(builder::DIBuilder, types::AbstractVector{<:Metadata})

Get or create a metadata node for a type array, used for e.g. template
parameter lists.
"""
function get_or_create_type_array!(builder::DIBuilder, types::AbstractVector{<:Metadata})
    tys = convert(Vector{Metadata}, types)
    Metadata(API.LLVMDIBuilderGetOrCreateTypeArray(builder, tys, Csize_t(length(tys))))
end


# ObjC

@vocabulary IR DIObjCProperty
@vocabulary Build objc_ivar!, objc_property!

"""
    DIObjCProperty

An Objective-C `@property` descriptor.
"""
@checked struct DIObjCProperty <: DINode
    ref::API.LLVMMetadataRef
end
register(DIObjCProperty, API.LLVMDIObjCPropertyMetadataKind)

"""
    objc_ivar!(builder::DIBuilder, name::AbstractString, file::DIFile,
              line::Integer, size_in_bits::Integer, align_in_bits::Integer,
              offset_in_bits::Integer, type::DIType, property_node::Metadata;
              flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new Objective-C instance variable.
"""
function objc_ivar!(builder::DIBuilder, name::AbstractString, file::DIFile,
                   line::Integer, size_in_bits::Integer, align_in_bits::Integer,
                   offset_in_bits::Integer, type::DIType, property_node::Metadata;
                   flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateObjCIVar(
        builder, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags, type, property_node))
end

"""
    objc_property!(builder::DIBuilder, name::AbstractString, file::DIFile,
                  line::Integer, getter::AbstractString, setter::AbstractString,
                  attributes::Integer, type::DIType) -> DIObjCProperty

Create a new Objective-C `@property` descriptor.
"""
function objc_property!(builder::DIBuilder, name::AbstractString, file::DIFile,
                       line::Integer, getter::AbstractString, setter::AbstractString,
                       attributes::Integer, type::DIType)
    DIObjCProperty(API.LLVMDIBuilderCreateObjCProperty(
        builder, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        getter, Csize_t(ncodeunits(getter)),
        setter, Csize_t(ncodeunits(setter)),
        Cuint(attributes), type))
end


# LLVM 21+ additions

@static if version() >= v"21"

@vocabulary IR DISubrangeType
@vocabulary Build set_type!, subrange_type!, dynamic_array_type!, enumerator_arbitrary!

"""
    DISubrangeType <: DIType

A subrange type (an integer range type, as found in Fortran or Ada), built
with [`subrange_type!`](@ref). Requires LLVM 21+.
"""
@checked struct DISubrangeType <: DIType
    ref::API.LLVMMetadataRef
end
register(DISubrangeType, API.LLVMDISubrangeTypeMetadataKind)

"""
    set_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
             file::DIFile, line::Integer, size_in_bits::Integer,
             align_in_bits::Integer, base_type::DIType) -> DIDerivedType

Create a new set type (`DW_TAG_set_type`). Requires LLVM 21+.
"""
function set_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                  file::DIFile, line::Integer, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateSetType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), base_type))
end

"""
    subrange_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                  line::Integer, file::DIFile, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType;
                  flags=API.LLVMDIFlagZero,
                  lower_bound=nothing, upper_bound=nothing,
                  stride=nothing, bias=nothing) -> DISubrangeType

Create a new subrange type. Requires LLVM 21+.
"""
function subrange_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                       line::Integer, file::DIFile, size_in_bits::Integer,
                       align_in_bits::Integer, base_type::DIType;
                       flags=API.LLVMDIFlagZero,
                       lower_bound=nothing, upper_bound=nothing,
                       stride=nothing, bias=nothing)
    DISubrangeType(API.LLVMDIBuilderCreateSubrangeType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        Cuint(line), file,
        UInt64(size_in_bits), UInt32(align_in_bits), flags, base_type,
        something(lower_bound, C_NULL),
        something(upper_bound, C_NULL),
        something(stride, C_NULL),
        something(bias, C_NULL)))
end

"""
    dynamic_array_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                      line::Integer, file::DIFile, size::Integer,
                      align_in_bits::Integer, element_type::DIType,
                      subscripts::AbstractVector{<:Metadata};
                      data_location=nothing, associated=nothing,
                      allocated=nothing, rank=nothing,
                      bit_stride=nothing) -> DICompositeType

Create a new dynamic array type (Fortran assumed-shape/deferred-shape arrays).
Requires LLVM 21+.
"""
function dynamic_array_type!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                           line::Integer, file::DIFile, size::Integer,
                           align_in_bits::Integer, element_type::DIType,
                           subscripts::AbstractVector{<:Metadata};
                           data_location=nothing, associated=nothing,
                           allocated=nothing, rank=nothing,
                           bit_stride=nothing)
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateDynamicArrayType(
        builder, scope, name, Csize_t(ncodeunits(name)),
        Cuint(line), file,
        UInt64(size), UInt32(align_in_bits), element_type,
        subs, Cuint(length(subs)),
        something(data_location, C_NULL),
        something(associated, C_NULL),
        something(allocated, C_NULL),
        something(rank, C_NULL),
        something(bit_stride, C_NULL)))
end

"""
    enumerator_arbitrary!(builder::DIBuilder, name::AbstractString,
                   size_in_bits::Integer, words::AbstractVector{UInt64};
                   unsigned::Bool=false) -> DIEnumerator

Create a new arbitrary-precision enumerator. Requires LLVM 21+.
"""
function enumerator_arbitrary!(builder::DIBuilder, name::AbstractString,
                        size_in_bits::Integer, words::AbstractVector{UInt64};
                        unsigned::Bool=false)
    # LLVM reads cld(size_in_bits, 64) words from the array
    if length(words) < cld(size_in_bits, 64)
        throw(ArgumentError("words must contain at least cld(size_in_bits, 64) = $(cld(size_in_bits, 64)) elements"))
    end
    DIEnumerator(API.LLVMDIBuilderCreateEnumeratorOfArbitraryPrecision(
        builder, name, Csize_t(ncodeunits(name)),
        UInt64(size_in_bits), as_vector(words), unsigned))
end

end # @static if version() >= v"21"


## subprogram

@vocabulary IR DISubprogram
@vocabulary Build subprogram!, finalize_subprogram!

"""
    DISubprogram <: DILocalScope

A subprogram (a function) in the source code.

# Properties

    sp.line

The line number of the subprogram, or -1 if unknown.

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DISubprogram <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DISubprogram, API.LLVMDISubprogramMetadataKind)

line(subprogram::DISubprogram) = line_number(API.LLVMDISubprogramGetLine(subprogram))

@property DISubprogram line

"""
    subprogram!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer, type::DISubroutineType;
                linkage_name::AbstractString="", scope_line::Integer=line,
                is_local_to_unit::Bool=false, is_definition::Bool=true,
                flags=API.LLVMDIFlagZero,
                is_optimized::Bool=false) -> DISubprogram

Create a new [`DISubprogram`](@ref) describing a function. When
`linkage_name` is empty, LLVM falls back to `name`. `scope_line`
defaults to the function's `line`, which is the usual case.
"""
function subprogram!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, type::DISubroutineType;
                     linkage_name::AbstractString="", scope_line::Integer=line,
                     is_local_to_unit::Bool=false, is_definition::Bool=true,
                     flags=API.LLVMDIFlagZero,
                     is_optimized::Bool=false)
    DISubprogram(API.LLVMDIBuilderCreateFunction(
        builder, scope, name, Csize_t(ncodeunits(name)),
        linkage_name, Csize_t(ncodeunits(linkage_name)),
        file, Cuint(line), type,
        is_local_to_unit, is_definition, Cuint(scope_line),
        flags, is_optimized))
end

"""
    finalize_subprogram!(builder::DIBuilder, sp::DISubprogram)

Finalize a single subprogram early, sealing its retained-nodes list. After
this, no more local variables can be added to `sp`. A no-op if `sp` was not
tracked by `builder` (e.g. created elsewhere or already finalized).

Calling this is never required for correctness — [`dispose`](@ref) /
[`finalize!`](@ref) finalize every tracked subprogram automatically. Use it
only when streaming many subprograms through the builder and wanting to
release their bookkeeping early.
"""
finalize_subprogram!(builder::DIBuilder, sp::DISubprogram) =
    API.LLVMDIBuilderFinalizeSubprogram(builder, sp)


## compile unit

@vocabulary IR DICompileUnit
@vocabulary Build compile_unit!

"""
    DICompileUnit

A compilation unit in the source code.
"""
@checked struct DICompileUnit <: DIScope
    ref::API.LLVMMetadataRef
end
register(DICompileUnit, API.LLVMDICompileUnitMetadataKind)

"""
    compile_unit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                 optimized::Bool=true, cmdline::AbstractString="",
                 runtime_version::Integer=0,
                 split_name::Union{AbstractString,Nothing}=nothing,
                 emission_kind=LLVM.DWARFEmissionKind.Full,
                 dwo_id::Integer=0,
                 split_debug_inlining::Bool=true,
                 debug_info_for_profiling::Bool=false,
                 sysroot::AbstractString="", sdk::AbstractString="") -> DICompileUnit

Create a new [`DICompileUnit`](@ref). `lang` is a `LLVM.DWARFSourceLanguage.T`
value (e.g. `LLVM.DWARFSourceLanguage.Julia`). `cmdline` is a
command-line string embedded verbatim in the emitted debug info.
"""
function compile_unit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                      optimized::Bool=true,
                      cmdline::AbstractString="",
                      runtime_version::Integer=0,
                      split_name::Union{AbstractString,Nothing}=nothing,
                      emission_kind=API.LLVMDWARFEmissionFull,
                      dwo_id::Integer=0,
                      split_debug_inlining::Bool=true,
                      debug_info_for_profiling::Bool=false,
                      sysroot::AbstractString="",
                      sdk::AbstractString="")
    split_name_ptr = split_name === nothing ? C_NULL : split_name
    split_name_len = split_name === nothing ? Csize_t(0) : Csize_t(ncodeunits(split_name))
    cu = DICompileUnit(API.LLVMDIBuilderCreateCompileUnit(
        builder, lang, file,
        producer, Csize_t(ncodeunits(producer)),
        optimized,
        cmdline, Csize_t(ncodeunits(cmdline)),
        Cuint(runtime_version),
        split_name_ptr, split_name_len,
        emission_kind,
        Cuint(dwo_id),
        split_debug_inlining,
        debug_info_for_profiling,
        sysroot, Csize_t(ncodeunits(sysroot)),
        sdk, Csize_t(ncodeunits(sdk))))
    builder.needs_finalization = true
    return cu
end


## module

@vocabulary IR DIModule
@vocabulary Build dimodule!

"""
    DIModule

A module in the source code (Clang modules / Fortran modules / Swift modules).
"""
@checked struct DIModule <: DIScope
    ref::API.LLVMMetadataRef
end
register(DIModule, API.LLVMDIModuleMetadataKind)

"""
    dimodule!(builder::DIBuilder, parent_scope::DIScope, name::AbstractString;
              config_macros::AbstractString="", include_path::AbstractString="",
              api_notes_file::AbstractString="") -> DIModule

Create a new [`DIModule`](@ref) describing a module in the source code.
"""
function dimodule!(builder::DIBuilder, parent_scope::DIScope, name::AbstractString;
                   config_macros::AbstractString="",
                   include_path::AbstractString="",
                   api_notes_file::AbstractString="")
    DIModule(API.LLVMDIBuilderCreateModule(
        builder, parent_scope,
        name, Csize_t(ncodeunits(name)),
        config_macros, Csize_t(ncodeunits(config_macros)),
        include_path, Csize_t(ncodeunits(include_path)),
        api_notes_file, Csize_t(ncodeunits(api_notes_file))))
end


## variable factories

@vocabulary Build auto_variable!, parameter_variable!

"""
    auto_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                  file::DIFile, line::Integer, type::DIType;
                  always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                  align_in_bits::Integer=0) -> DILocalVariable

Create a new local variable descriptor (for a compiler-introduced automatic
variable).
"""
function auto_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                       file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                       align_in_bits::Integer=0)
    DILocalVariable(API.LLVMDIBuilderCreateAutoVariable(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), type,
        always_preserve, flags, UInt32(align_in_bits)))
end

"""
    parameter_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                       arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false,
                       flags=API.LLVMDIFlagZero) -> DILocalVariable

Create a new descriptor for a function parameter variable. `arg_no` is
the 1-based parameter index.
"""
function parameter_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                            arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                            always_preserve::Bool=false,
                            flags=API.LLVMDIFlagZero)
    DILocalVariable(API.LLVMDIBuilderCreateParameterVariable(
        builder, scope, name, Csize_t(ncodeunits(name)), Cuint(arg_no),
        file, Cuint(line), type,
        always_preserve, flags))
end


## expression

@vocabulary IR DIExpression, DIGlobalVariableExpression
@vocabulary Build expression!, constant_value_expression!

"""
    DIExpression

A DWARF expression that modifies how a variable's value is expressed at runtime.
"""
@checked struct DIExpression <: MDNode
    ref::API.LLVMMetadataRef
end
register(DIExpression, API.LLVMDIExpressionMetadataKind)

"""
    DIGlobalVariableExpression

A pairing of a [`DIGlobalVariable`](@ref) and its associated [`DIExpression`](@ref).

# Properties

    gve.variable

The global variable described by the global variable expression.

    gve.expression

The expression of the global variable expression, which describes the location of the
variable.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DIGlobalVariableExpression <: MDNode
    ref::API.LLVMMetadataRef
end
register(DIGlobalVariableExpression, API.LLVMDIGlobalVariableExpressionMetadataKind)

"""
    expression!(builder::DIBuilder,
                addr::AbstractVector{<:Integer}=UInt64[]) -> DIExpression

Create a new [`DIExpression`](@ref) from the given array of opcodes (encoding
a DWARF expression such as `DW_OP_plus_uconst`).
"""
function expression!(builder::DIBuilder, addr::AbstractVector{<:Integer}=UInt64[])
    DIExpression(API.LLVMDIBuilderCreateExpression(
        builder, Vector{UInt64}(addr), Csize_t(length(addr))))
end

"""
    constant_value_expression!(builder::DIBuilder, value::Integer) -> DIExpression

Create a new [`DIExpression`](@ref) representing a single constant value.
"""
function constant_value_expression!(builder::DIBuilder, value::Integer)
    DIExpression(API.LLVMDIBuilderCreateConstantValueExpression(
        builder, UInt64(value)))
end

function variable(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetVariable(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIGlobalVariable
end

function expression(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetExpression(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIExpression
end

@property DIGlobalVariableExpression variable
@property DIGlobalVariableExpression expression


## global variable

@vocabulary Build global_variable_expression!, temp_global_variable_fwd_decl!

"""
    global_variable_expression!(builder::DIBuilder, scope::DIScope,
                              name::AbstractString, linkage::AbstractString,
                              file::DIFile, line::Integer, type::DIType,
                              local_to_unit::Bool, expression::DIExpression;
                              declaration=nothing,
                              align_in_bits::Integer=0) -> DIGlobalVariableExpression

Create a new global variable descriptor paired with a DWARF expression.
"""
function global_variable_expression!(builder::DIBuilder, scope::DIScope,
                                   name::AbstractString, linkage::AbstractString,
                                   file::DIFile, line::Integer, type::DIType,
                                   local_to_unit::Bool, expression::DIExpression;
                                   declaration=nothing,
                                   align_in_bits::Integer=0)
    DIGlobalVariableExpression(API.LLVMDIBuilderCreateGlobalVariableExpression(
        builder, scope, name, Csize_t(ncodeunits(name)),
        linkage, Csize_t(ncodeunits(linkage)),
        file, Cuint(line), type, local_to_unit, expression,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end

"""
    temp_global_variable_fwd_decl!(builder::DIBuilder, scope::DIScope,
                               name::AbstractString, linkage::AbstractString,
                               file::DIFile, line::Integer, type::DIType,
                               local_to_unit::Bool;
                               declaration=nothing,
                               align_in_bits::Integer=0) -> DIGlobalVariable

Create a new temporary forward declaration for a global variable.
"""
function temp_global_variable_fwd_decl!(builder::DIBuilder, scope::DIScope,
                                    name::AbstractString, linkage::AbstractString,
                                    file::DIFile, line::Integer, type::DIType,
                                    local_to_unit::Bool;
                                    declaration=nothing,
                                    align_in_bits::Integer=0)
    DIGlobalVariable(API.LLVMDIBuilderCreateTempGlobalVariableFwdDecl(
        builder, scope, name, Csize_t(ncodeunits(name)),
        linkage, Csize_t(ncodeunits(linkage)),
        file, Cuint(line), type, local_to_unit,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end


## lexical block

@vocabulary IR DILexicalBlock, DILexicalBlockFile
@vocabulary Build lexical_block!, lexical_block_file!

"""
    DILexicalBlock

A lexical block (a nested scope, typically a compound statement) in the source code.
"""
@checked struct DILexicalBlock <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DILexicalBlock, API.LLVMDILexicalBlockMetadataKind)

"""
    DILexicalBlockFile

A lexical block that changes the current source file, e.g. due to an `#include`.
"""
@checked struct DILexicalBlockFile <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DILexicalBlockFile, API.LLVMDILexicalBlockFileMetadataKind)

"""
    lexical_block!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                  line::Integer, column::Integer) -> DILexicalBlock

Create a new [`DILexicalBlock`](@ref) describing a nested source scope.
"""
function lexical_block!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                       line::Integer, column::Integer)
    DILexicalBlock(API.LLVMDIBuilderCreateLexicalBlock(
        builder, scope, file, Cuint(line), Cuint(column)))
end

"""
    lexical_block_file!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                      discriminator::Integer=0) -> DILexicalBlockFile

Create a new [`DILexicalBlockFile`](@ref) for tracking source-file changes
within a lexical scope.
"""
function lexical_block_file!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                           discriminator::Integer=0)
    DILexicalBlockFile(API.LLVMDIBuilderCreateLexicalBlockFile(
        builder, scope, file, Cuint(discriminator)))
end


## namespace

@vocabulary IR DINamespace
@vocabulary Build namespace!

"""
    DINamespace

A namespace in the source code.
"""
@checked struct DINamespace <: DIScope
    ref::API.LLVMMetadataRef
end
register(DINamespace, API.LLVMDINamespaceMetadataKind)

"""
    namespace!(builder::DIBuilder, parent_scope::DIScope, name::AbstractString;
               export_symbols::Bool=false) -> DINamespace

Create a new [`DINamespace`](@ref) describing a namespace in the source code.
"""
function namespace!(builder::DIBuilder, parent_scope::DIScope, name::AbstractString;
                    export_symbols::Bool=false)
    DINamespace(API.LLVMDIBuilderCreateNameSpace(
        builder, parent_scope,
        name, Csize_t(ncodeunits(name)),
        export_symbols))
end


## instruction insertion

@vocabulary Build dbg_declare!, dbg_value!

"""
    dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                 expr::DIExpression, debugloc::DILocation,
                 pos::InsertionPoint{Instruction})

Insert a debug record that declares `storage` as the address of the variable `var` at the
given position, e.g., `LLVM.after(alloca)`. Returns a `DbgRecord` on LLVM ≥ 19, or
the `llvm.dbg.declare` call [`Instruction`](@ref) on LLVM < 19.

Debug records can not be inserted at the end of a block that has a terminator; use
`LLVM.before(bb.terminator)` instead. Several records inserted at a position that is
before the debug records of an instruction (e.g., `LLVM.after(inst)` or
`LLVM.at_begin(bb)`) end up in reverse order, as each one is inserted in front of the
others. Use `LLVM.before(inst)` to append records to the ones of `inst`.
"""
dbg_declare!

"""
    dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
               expr::DIExpression, debugloc::DILocation,
               pos::InsertionPoint{Instruction})

Insert a debug record that describes `val` as the value of the variable `var` at the given
position. Returns a `DbgRecord` on LLVM ≥ 19, or the `llvm.dbg.value` call
[`Instruction`](@ref) on LLVM < 19. See [`dbg_declare!`](@ref) for which positions can be
used.
"""
dbg_value!

# debug records can't be inserted after a terminator, or refer to values of another context
function check_record_position(pos::InsertionPoint{Instruction}, val=nothing)
    bb = check_valid(pos)
    pos.anchor == C_NULL && API.LLVMGetBasicBlockTerminator(bb) != C_NULL &&
        throw(ArgumentError("Cannot insert debug records after the terminator of a basic block"))
    val === nothing || API.LLVMGetValueContext(val) == API.LLVMGetValueContext(bb) ||
        throw(ArgumentError("Cannot insert a debug record for a value of another context"))
    return bb
end

@static if version() >= v"19"

@vocabulary IR DbgRecord

"""
    DbgRecord

A non-instruction debug record attached to a basic block, replacing the
legacy `llvm.dbg.*` intrinsics in LLVM ≥ 19.

# Properties

    record.kind

The kind of the debug record, an `LLVM.API.LLVMDbgRecordKind`: `LLVMDbgRecordDeclare`,
`LLVMDbgRecordValue` or `LLVMDbgRecordAssign` for variable records, which describe the
location of a source variable, or `LLVMDbgRecordLabel` for label records.

Variable records can be further inspected using the following properties:
- `record.variable`: the source variable that is described;
- `record.expression`: the expression that computes the variable's location;
- `record.value`: the IR value used by that expression, or `record.location_operands` for
  expressions that use several values.

The source location of every record is available as `record.debug_location`.

    record.debug_location

The source location of the debug record.

    record.variable

The source variable described by a variable record.

    record.expression

The expression that computes the location of the variable described by a variable record,
in terms of its `location_operands`.

    record.location_operands

The IR values that are used to compute the location of the variable described by a
variable record, as a read-only view. There is usually only one, but records that use a
`!DIArgList` can refer to several. Entries are `nothing` if the value has been deleted.

See also the `LLVM.DbgRecord` property.

    record.value

The IR value used to compute the location of the variable described by a variable record,
or `nothing` if that value has been deleted. Records that refer to several values need to
be inspected using their `location_operands` instead.

    record.next
    record.prev

The next or previous debug record attached to the same instruction, or `nothing` if there
is none. `prev` requires LLVM 20+.
"""
@checked struct DbgRecord
    ref::API.LLVMDbgRecordRef
end
@properties DbgRecord

Base.unsafe_convert(::Type{API.LLVMDbgRecordRef}, record::DbgRecord) = record.ref

function Base.show(io::IO, record::DbgRecord)
    str_ptr = API.LLVMPrintDbgRecordToString(record)
    str = unsafe_string(str_ptr)
    print(io, rstrip(str))
    # LLVMPrintDbgRecordToString-returned memory is freed by LLVMDisposeMessage
    API.LLVMDisposeMessage(str_ptr)
end

# record iteration

struct DbgRecordIterator
    inst::Instruction
end

debug_records(inst::Instruction) = DbgRecordIterator(inst)

@property Instruction debug_records

Base.IteratorSize(::Type{DbgRecordIterator}) = Base.SizeUnknown()
Base.eltype(::Type{DbgRecordIterator}) = DbgRecord

function Base.iterate(iter::DbgRecordIterator)
    ref = @static if version() >= v"22"
        API.LLVMGetFirstDbgRecord(iter.inst)
    else
        # the upstream function crashes on instructions without debug records
        API.LLVMGetFirstDbgRecord2(iter.inst)
    end
    iterate(iter, ref)
end
function Base.iterate(::DbgRecordIterator, ref::API.LLVMDbgRecordRef)
    ref == C_NULL && return nothing
    return DbgRecord(ref), API.LLVMGetNextDbgRecord(ref)
end

# record inspection

kind(record::DbgRecord) = API.LLVMDbgRecordGetKind(record)

function check_variable_record(record::DbgRecord)
    kind(record) == API.LLVMDbgRecordLabel &&
        throw(ArgumentError("Label records do not describe a variable"))
    return
end

debug_location(record::DbgRecord) =
    Metadata(API.LLVMDbgRecordGetDebugLoc(record))::DILocation

function variable(record::DbgRecord)
    check_variable_record(record)
    Metadata(API.LLVMDbgVariableRecordGetVariable(record))::DILocalVariable
end

function expression(record::DbgRecord)
    check_variable_record(record)
    Metadata(API.LLVMDbgVariableRecordGetExpression(record))::DIExpression
end

struct DbgRecordLocationOperandSet <: AbstractVector{Union{Value,Nothing}}
    record::DbgRecord
end

function location_operands(record::DbgRecord)
    check_variable_record(record)
    DbgRecordLocationOperandSet(record)
end

Base.size(iter::DbgRecordLocationOperandSet) =
    (Int(API.LLVMExtraDbgVariableRecordGetNumValues(iter.record)),)

Base.IndexStyle(::Type{DbgRecordLocationOperandSet}) = IndexLinear()

function Base.getindex(iter::DbgRecordLocationOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    ref = API.LLVMDbgVariableRecordGetValue(iter.record, i-1)
    return ref == C_NULL ? nothing : Value(ref)
end

function value(record::DbgRecord)
    check_variable_record(record)
    n = API.LLVMExtraDbgVariableRecordGetNumValues(record)
    n == 1 ||
        throw(ArgumentError("Debug record refers to $n values, use its location_operands"))
    ref = API.LLVMDbgVariableRecordGetValue(record, 0)
    return ref == C_NULL ? nothing : Value(ref)
end

function next(record::DbgRecord)
    ref = API.LLVMGetNextDbgRecord(record)
    ref == C_NULL ? nothing : DbgRecord(ref)
end

function prev(record::DbgRecord)
    ref = API.LLVMGetPreviousDbgRecord(record)
    ref == C_NULL ? nothing : DbgRecord(ref)
end

@property DbgRecord kind
@property DbgRecord debug_location
@property DbgRecord variable
@property DbgRecord expression
@property DbgRecord value
@property DbgRecord location_operands
@property DbgRecord next
@static if version() >= v"20"
    @property DbgRecord prev
end

function dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                      expr::DIExpression, debugloc::DILocation,
                      pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, storage)
    DbgRecord(API.LLVMExtraDIBuilderInsertDeclareRecordAt(
        builder, storage, var, expr, debugloc, bb, pos.anchor, pos.head))
end

function dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, val)
    DbgRecord(API.LLVMExtraDIBuilderInsertDbgValueRecordAt(
        builder, val, var, expr, debugloc, bb, pos.anchor, pos.head))
end

else # LLVM < 19: debug intrinsics, which are ordinary instructions

function dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                      expr::DIExpression, debugloc::DILocation,
                      pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, storage)
    # at the end of a block without a terminator, AtEnd inserts at the end
    Instruction(pos.anchor == C_NULL ?
        API.LLVMDIBuilderInsertDeclareAtEnd(builder, storage, var, expr, debugloc, bb) :
        API.LLVMDIBuilderInsertDeclareBefore(builder, storage, var, expr, debugloc,
                                             pos.anchor))
end

function dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, val)
    Instruction(pos.anchor == C_NULL ?
        API.LLVMDIBuilderInsertDbgValueAtEnd(builder, val, var, expr, debugloc, bb) :
        API.LLVMDIBuilderInsertDbgValueBefore(builder, val, var, expr, debugloc,
                                              pos.anchor))
end

end # @static version check


## label (LLVM 20+)

@static if version() >= v"20"

@vocabulary IR DILabel
@vocabulary Build label!, dbg_label!

"""
    DILabel

A debug-info label, describing a source-level code location by name.
Requires LLVM 20+.
"""
@checked struct DILabel <: DINode
    ref::API.LLVMMetadataRef
end
register(DILabel, API.LLVMDILabelMetadataKind)

"""
    label!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
           file::DIFile, line::Integer;
           always_preserve::Bool=false) -> DILabel

Create a new [`DILabel`](@ref). Requires LLVM 20+.
"""
function label!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                file::DIFile, line::Integer;
                always_preserve::Bool=false)
    DILabel(API.LLVMDIBuilderCreateLabel(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), always_preserve))
end

"""
    dbg_label!(builder::DIBuilder, label::DILabel, location::DILocation,
               pos::InsertionPoint{Instruction}) -> DbgRecord

Insert a debug record for the label `label` at the given position. See
[`dbg_declare!`](@ref) for which positions can be used. Requires LLVM 20+.
"""
function dbg_label!(builder::DIBuilder, label::DILabel, location::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos)
    DbgRecord(API.LLVMExtraDIBuilderInsertLabelAt(builder, label, location, bb, pos.anchor,
                                                  pos.head))
end

end # @static version check


## imported entity

@vocabulary IR DIImportedEntity
@vocabulary Build imported_module_from_namespace!, imported_module_from_alias!,
        imported_module_from_module!, imported_declaration!

"""
    DIImportedEntity

An imported entity, such as a C++ `using` declaration or module import.
"""
@checked struct DIImportedEntity <: DINode
    ref::API.LLVMMetadataRef
end
register(DIImportedEntity, API.LLVMDIImportedEntityMetadataKind)

"""
    imported_module_from_namespace!(builder::DIBuilder, scope::DIScope,
                                 ns::DINamespace, file::DIFile,
                                 line::Integer) -> DIImportedEntity

Create a new `DIImportedEntity` from a namespace.
"""
imported_module_from_namespace!(builder::DIBuilder, scope::DIScope, ns::DINamespace,
                             file::DIFile, line::Integer) =
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromNamespace(
        builder, scope, ns, file, Cuint(line)))

"""
    imported_module_from_alias!(builder::DIBuilder, scope::DIScope,
                             imported::DIImportedEntity, file::DIFile,
                             line::Integer,
                             elements::AbstractVector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from an alias.
"""
function imported_module_from_alias!(builder::DIBuilder, scope::DIScope,
                                  imported::DIImportedEntity, file::DIFile,
                                  line::Integer,
                                  elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromAlias(
        builder, scope, imported, file, Cuint(line),
        elts, Cuint(length(elts))))
end

"""
    imported_module_from_module!(builder::DIBuilder, scope::DIScope,
                              mod::DIModule, file::DIFile, line::Integer,
                              elements::AbstractVector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from a module.
"""
function imported_module_from_module!(builder::DIBuilder, scope::DIScope,
                                   mod::DIModule, file::DIFile, line::Integer,
                                   elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromModule(
        builder, scope, mod, file, Cuint(line),
        elts, Cuint(length(elts))))
end

"""
    imported_declaration!(builder::DIBuilder, scope::DIScope, decl::Metadata,
                        file::DIFile, line::Integer, name::AbstractString,
                        elements::AbstractVector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from a declaration.
"""
function imported_declaration!(builder::DIBuilder, scope::DIScope, decl::Metadata,
                              file::DIFile, line::Integer, name::AbstractString,
                              elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedDeclaration(
        builder, scope, decl, file, Cuint(line),
        name, Csize_t(ncodeunits(name)),
        elts, Cuint(length(elts))))
end


## macro

@vocabulary IR DIMacro, DIMacroFile
@vocabulary Build macro!, temp_macro_file!

"""
    DIMacro

A single preprocessor macro definition or undefinition.
"""
@checked struct DIMacro <: DINode
    ref::API.LLVMMetadataRef
end
register(DIMacro, API.LLVMDIMacroMetadataKind)

"""
    DIMacroFile

A collection of macro records corresponding to a single source file.
"""
@checked struct DIMacroFile <: DINode
    ref::API.LLVMMetadataRef
end
register(DIMacroFile, API.LLVMDIMacroFileMetadataKind)

"""
    macro!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
           line::Integer, record_type, name::AbstractString,
           value::AbstractString) -> DIMacro

Create a new [`DIMacro`](@ref). `record_type` is a
`LLVMDWARFMacinfoRecordType` value (e.g. `LLVM.API.LLVMDWARFMacinfoRecordTypeDefine`).
"""
function macro!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
                line::Integer, record_type, name::AbstractString,
                value::AbstractString)
    DIMacro(API.LLVMDIBuilderCreateMacro(
        builder, something(parent_macrofile, C_NULL), Cuint(line), record_type,
        name, Csize_t(ncodeunits(name)),
        value, Csize_t(ncodeunits(value))))
end

"""
    temp_macro_file!(builder::DIBuilder,
                   parent_macrofile::Union{DIMacroFile,Nothing},
                   line::Integer, file::DIFile) -> DIMacroFile

Create a new (temporary) [`DIMacroFile`](@ref).
"""
temp_macro_file!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
               line::Integer, file::DIFile) =
    DIMacroFile(API.LLVMDIBuilderCreateTempMacroFile(
        builder, something(parent_macrofile, C_NULL), Cuint(line), file))


## instruction debug location

# extends the `debug_location` / `debug_location!` functions for IRBuilder.

function debug_location(inst::Instruction)
    ref = API.LLVMInstructionGetDebugLoc(inst)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end

debug_location!(inst::Instruction, loc::DILocation) =
    API.LLVMInstructionSetDebugLoc(inst, loc)
debug_location!(inst::Instruction) =
    API.LLVMInstructionSetDebugLoc(inst, C_NULL)

@property Instruction debug_location (inst, loc::Union{DILocation,Nothing}) ->
    loc === nothing ? debug_location!(inst) : debug_location!(inst, loc)


## mutation / advanced helpers

@vocabulary IR temporary_mdnode, dispose_temporary

"""
    temporary_mdnode(operands::AbstractVector{<:Metadata}=Metadata[]) -> MDNode

Create a temporary metadata node in the task-local [`context`](@ref) with the
given operands. Temporary nodes are useful for constructing cycles and must be
either replaced via [`replace_uses!`](@ref) or disposed of via
[`dispose_temporary`](@ref).
"""
function temporary_mdnode(operands::AbstractVector{<:Metadata}=Metadata[])
    ops = convert(Vector{Metadata}, operands)
    ref = API.LLVMTemporaryMDNode(context(), ops, Csize_t(length(ops)))
    Metadata(ref)
end

"""
    dispose_temporary(md::Metadata)

Dispose of a temporary metadata node returned by [`temporary_mdnode`](@ref).
"""
dispose_temporary(md::Metadata) = API.LLVMDisposeTemporaryMDNode(md)

"""
    replace_uses!(temp::Metadata, replacement::Metadata)

Replace all uses of temporary metadata `temp` with `replacement`, and dispose
of `temp`. Method on the existing [`replace_uses!`](@ref) for [`Value`](@ref).
"""
replace_uses!(temp::Metadata, replacement::Metadata) =
    API.LLVMMetadataReplaceAllUsesWith(temp, replacement)


@static if version() >= v"21"

@vocabulary Build replace_arrays!, replace_type!

"""
    replace_arrays!(builder::DIBuilder, T::DICompositeType,
                   elements::AbstractVector{<:Metadata})

Replace the elements array of the given composite type `T`. Requires LLVM 21+.
"""
function replace_arrays!(builder::DIBuilder, T::DICompositeType,
                        elements::AbstractVector{<:Metadata})
    elts = convert(Vector{Metadata}, elements)
    tref = Ref(T.ref)
    API.LLVMReplaceArrays(builder, tref, elts, Cuint(length(elts)))
    return Metadata(tref[])::DICompositeType
end

"""
    replace_type!(sp::DISubprogram, ty::DISubroutineType)

Replace the type of the given subprogram. Requires LLVM 21+.
"""
replace_type!(sp::DISubprogram, ty::DISubroutineType) =
    API.LLVMDISubprogramReplaceType(sp, ty)

end # @static version check


## other

@vocabulary IR DEBUG_METADATA_VERSION, strip_debuginfo!

"""
    DEBUG_METADATA_VERSION()

The current debug info version number, as supported by LLVM.
"""
DEBUG_METADATA_VERSION() = API.LLVMDebugMetadataVersion()

debug_metadata_version(mod::Module) = Int(API.LLVMGetModuleDebugMetadataVersion(mod))

@property Module debug_metadata_version

"""
    strip_debuginfo!(mod::Module)

Strip the debug information from the given module.
"""
strip_debuginfo!(mod::Module) = API.LLVMStripModuleDebugInfo(mod)

function subprogram(func::Function)
    ref = API.LLVMGetSubprogram(func)
    ref==C_NULL ? nothing : Metadata(ref)::DISubprogram
end

# `subprogram!` is the `DIBuilder` function that creates a subprogram
@property Function subprogram (func, sp::DISubprogram) -> API.LLVMSetSubprogram(func, sp)
