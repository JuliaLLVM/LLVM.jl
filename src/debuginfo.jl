## debug info builder

export DIBuilder

"""
    DIBuilder

A builder for constructing debug information metadata.

This object needs to be disposed of using [`dispose`](@ref), after first having
called [`finalize!`](@ref).
"""
@checked struct DIBuilder
    ref::API.LLVMDIBuilderRef
end

Base.unsafe_convert(::Type{API.LLVMDIBuilderRef}, builder::DIBuilder) =
    mark_use(builder).ref

"""
    DIBuilder(mod::Module; allow_unresolved::Bool=true)

Create a new debug info builder that emits metadata into `mod`.

When `allow_unresolved` is `true` (the default), the builder collects unresolved
metadata nodes attached to the module so that cycles can be resolved during a
call to [`finalize!`](@ref). When `false`, the builder errors on unresolved
nodes instead.
"""
function DIBuilder(mod::Module; allow_unresolved::Bool=true)
    ref = allow_unresolved ? API.LLVMCreateDIBuilder(mod) :
                             API.LLVMCreateDIBuilderDisallowUnresolved(mod)
    mark_alloc(DIBuilder(ref))
end

"""
    dispose(builder::DIBuilder)

Dispose of a debug info builder. [`finalize!`](@ref) must be called first.
"""
dispose(builder::DIBuilder) = mark_dispose(API.LLVMDisposeDIBuilder, builder)

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
Must be called before [`dispose`](@ref) and before using the emitted debug
information.
"""
finalize!(builder::DIBuilder) = API.LLVMDIBuilderFinalize(builder)


## location information

export DILocation, line, column, scope, inlined_at

"""
    DILocation

A location in the source code.
"""
@checked struct DILocation <: MDNode
    ref::API.LLVMMetadataRef
end
register(DILocation, API.LLVMDILocationMetadataKind)

"""
    DILocation([line::Int], [col::Int], [scope::Metadata], [inlined_at::Metadata])

Creates a new DebugLocation that describes a source location.
"""
function DILocation(line=0, col=0, scope=nothing, inlined_at=nothing)
    # XXX: are null scopes valid? they crash LLVM:
    #      DILocation(Context(), 1, 2).scope
    DILocation(API.LLVMDIBuilderCreateDebugLocation(context(), line, col,
                                                    something(scope, C_NULL),
                                                    something(inlined_at, C_NULL)))
end

"""
    line(location::DILocation)

Get the line number of this debug location.
"""
line(location::DILocation) = Int(API.LLVMDILocationGetLine(location))

"""
    column(location::DILocation)

Get the column number of this debug location.
"""
column(location::DILocation) = Int(API.LLVMDILocationGetColumn(location))

"""
    scope(location::DILocation)

Get the local scope associated with this debug location.
"""
function scope(location::DILocation)
    ref = API.LLVMDILocationGetScope(location)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

"""
    inlined_at(location::DILocation)

Get the "inline at" location associated with this debug location.
"""
function inlined_at(location::DILocation)
    ref = API.LLVMDILocationGetInlinedAt(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end


## nodes

export DINode

"""
    DINode

a tagged DWARF-like metadata node.
"""
abstract type DINode <: MDNode end


## variables

export DIVariable, file, scope, line

"""
    DIVariable

Abstract supertype for all variable-like metadata nodes.
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

"""
    file(var::DIVariable)

Get the file of the given variable.
"""
function file(var::DIVariable)
    ref = API.LLVMDIVariableGetFile(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIFile
end

"""
    name(var::DIVariable)

Get the name of the given variable.
"""
function scope(var::DIVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

"""
    line(var::DIVariable)

Get the line number of the given variable.
"""
line(var::DIVariable) = Int(API.LLVMDIVariableGetLine(var))


## scopes

export DIScope, file, name

"""
    DIScope

Abstract supertype for lexical scopes and types (which are also declaration contexts).
"""
abstract type DIScope <: DINode end

"""
    file(scope::DIScope)

Get the metadata of the file associated with a given scope.
"""
file(scope::DIScope) = DIFile(API.LLVMDIScopeGetFile(scope))

"""
    name(scope::DIScope)

Get the name of the given scope.
"""
function name(scope::DIScope)
    len = Ref{Cuint}()
    data = API.LLVMDIScopeGetName(scope, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

abstract type DILocalScope <: DIScope end


## file

export DIFile, directory, filename, source
@public file!

"""
    DIFile

A file in the source code.
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
                                       filename, Csize_t(length(filename)),
                                       directory, Csize_t(length(directory))))
end

"""
    directory(file::DIFile)

Get the directory of a given file.
"""
function directory(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetDirectory(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    filename(file::DIFile)

Get the filename of the given file.
"""
function filename(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetFilename(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    source(file::DIFile)

Get the source of the given file, or `nothing` if the source is not available.
"""
function source(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetSource(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end


## type

export DIType, DIEnumerator, DISubrange, name, offset, line, flags
@public align,
        basictype!, unspecifiedtype!, pointertype!, referencetype!, nullptrtype!,
        typedeftype!, qualifiedtype!, artificialtype!, objectpointertype!,
        inheritance!, membertype!, bitfieldmembertype!, staticmembertype!,
        memberpointertype!, structtype!, uniontype!, classtype!, arraytype!,
        vectortype!, enumerationtype!, enumerator!, forwarddecl!,
        replaceablecompositetype!, subroutinetype!, getorcreatesubrange!

"""
    DIType

Abstract supertype for all type-like metadata nodes.
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

"""
    name(typ::DIType)

Get the name of the given type.
"""
function name(typ::DIType)
    len = Ref{Csize_t}()
    data = API.LLVMDITypeGetName(typ, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    sizeof(typ::DIType)

Get the size in bits of the given type.
"""
Base.sizeof(typ::DIType) = 8*Int(API.LLVMDITypeGetSizeInBits(typ))

"""
    offset(typ::DIType)

Get the offset in bits of the given type.
"""
offset(typ::DIType) = Int(API.LLVMDITypeGetOffsetInBits(typ))

"""
    line(typ::DIType)

Get the line number of the given type.
"""
line(typ::DIType) = Int(API.LLVMDITypeGetLine(typ))

"""
    flags(typ::DIType)

Get the flags of the given type.
"""
flags(typ::DIType) = API.LLVMDITypeGetFlags(typ)

"""
    align(typ::DIType)

Get the alignment in bits of the given type.
"""
align(typ::DIType) = Int(API.LLVMDITypeGetAlignInBits(typ))

"""
    tag(node::DINode)

Get the DWARF tag of the given node, or `0` if none.
"""
tag(node::DINode) = Int(API.LLVMGetDINodeTag(node))


# basic types

"""
    basictype!(builder::DIBuilder, name::AbstractString, size_in_bits::Integer,
               encoding::Integer; flags=API.LLVMDIFlagZero) -> DIBasicType

Create a new [`DIBasicType`](@ref), such as an integer or floating-point type.
`encoding` is a `DW_ATE_*` value (see the DWARF standard).
"""
function basictype!(builder::DIBuilder, name::AbstractString, size_in_bits::Integer,
                    encoding::Integer; flags=API.LLVMDIFlagZero)
    DIBasicType(API.LLVMDIBuilderCreateBasicType(
        builder, name, Csize_t(length(name)),
        UInt64(size_in_bits), Cuint(encoding), flags))
end

"""
    unspecifiedtype!(builder::DIBuilder, name::AbstractString) -> DIBasicType

Create a new unspecified type (`DW_TAG_unspecified_type`), e.g. a C++ `decltype(nullptr)`.
"""
function unspecifiedtype!(builder::DIBuilder, name::AbstractString)
    DIBasicType(API.LLVMDIBuilderCreateUnspecifiedType(
        builder, name, Csize_t(length(name))))
end


# derived types

"""
    pointertype!(builder::DIBuilder, pointee_type::DIType, size_in_bits::Integer;
                 align_in_bits::Integer=0, address_space::Integer=0,
                 name::AbstractString="") -> DIDerivedType

Create a new pointer type.
"""
function pointertype!(builder::DIBuilder, pointee_type::DIType, size_in_bits::Integer;
                      align_in_bits::Integer=0, address_space::Integer=0,
                      name::AbstractString="")
    DIDerivedType(API.LLVMDIBuilderCreatePointerType(
        builder, pointee_type,
        UInt64(size_in_bits), UInt32(align_in_bits), Cuint(address_space),
        name, Csize_t(length(name))))
end

"""
    referencetype!(builder::DIBuilder, tag::Integer, type::DIType) -> DIDerivedType

Create a new reference type (C++ `T&` / `T&&`), with the given DWARF `tag`
(e.g. `DW_TAG_reference_type` or `DW_TAG_rvalue_reference_type`).
"""
function referencetype!(builder::DIBuilder, tag::Integer, type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateReferenceType(builder, Cuint(tag), type))
end

"""
    nullptrtype!(builder::DIBuilder) -> DIBasicType

Create a new type representing a null pointer.
"""
nullptrtype!(builder::DIBuilder) =
    DIBasicType(API.LLVMDIBuilderCreateNullPtrType(builder))

"""
    typedeftype!(builder::DIBuilder, type::DIType, name::AbstractString,
                 file::DIFile, line::Integer, scope::DIScope;
                 align_in_bits::Integer=0) -> DIDerivedType

Create a new typedef type.
"""
function typedeftype!(builder::DIBuilder, type::DIType, name::AbstractString,
                      file::DIFile, line::Integer, scope::DIScope;
                      align_in_bits::Integer=0)
    DIDerivedType(API.LLVMDIBuilderCreateTypedef(
        builder, type, name, Csize_t(length(name)),
        file, Cuint(line), scope, UInt32(align_in_bits)))
end

"""
    qualifiedtype!(builder::DIBuilder, tag::Integer, type::DIType) -> DIDerivedType

Create a new qualified type, such as `const T` (`DW_TAG_const_type`) or
`volatile T` (`DW_TAG_volatile_type`).
"""
function qualifiedtype!(builder::DIBuilder, tag::Integer, type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateQualifiedType(builder, Cuint(tag), type))
end

"""
    artificialtype!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a new artificial type (`DI_FLAG_ARTIFICIAL`), e.g. an implicit `this`.
"""
artificialtype!(builder::DIBuilder, type::DIType) =
    DIDerivedType(API.LLVMDIBuilderCreateArtificialType(builder, type))

"""
    objectpointertype!(builder::DIBuilder, type::DIType) -> DIDerivedType

Create a new type identifying an object pointer (`DI_FLAG_OBJECT_POINTER`).
"""
objectpointertype!(builder::DIBuilder, type::DIType) =
    DIDerivedType(API.LLVMDIBuilderCreateObjectPointerType(builder, type))

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
    membertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, offset_in_bits::Integer,
                type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new member (field) of a composite type.
"""
function membertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, offset_in_bits::Integer,
                     type::DIType; flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateMemberType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags, type))
end

"""
    bitfieldmembertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                        file::DIFile, line::Integer, size_in_bits::Integer,
                        offset_in_bits::Integer, storage_offset_in_bits::Integer,
                        type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new bit-field member of a composite type.
"""
function bitfieldmembertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                             file::DIFile, line::Integer, size_in_bits::Integer,
                             offset_in_bits::Integer, storage_offset_in_bits::Integer,
                             type::DIType; flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateBitFieldMemberType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt64(offset_in_bits), UInt64(storage_offset_in_bits),
        flags, type))
end

"""
    staticmembertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                      file::DIFile, line::Integer, type::DIType;
                      flags=API.LLVMDIFlagZero,
                      constant_val=nothing,
                      align_in_bits::Integer=0) -> DIDerivedType

Create a new static member of a composite type.
"""
function staticmembertype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                           file::DIFile, line::Integer, type::DIType;
                           flags=API.LLVMDIFlagZero,
                           constant_val=nothing,
                           align_in_bits::Integer=0)
    DIDerivedType(API.LLVMDIBuilderCreateStaticMemberType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line), type, flags,
        something(constant_val, C_NULL), UInt32(align_in_bits)))
end

"""
    memberpointertype!(builder::DIBuilder, pointee_type::DIType, class_type::DIType,
                       size_in_bits::Integer;
                       align_in_bits::Integer=0,
                       flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new pointer-to-member type for C++.
"""
function memberpointertype!(builder::DIBuilder, pointee_type::DIType, class_type::DIType,
                            size_in_bits::Integer;
                            align_in_bits::Integer=0,
                            flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateMemberPointerType(
        builder, pointee_type, class_type,
        UInt64(size_in_bits), UInt32(align_in_bits), flags))
end


# composite types

"""
    structtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, elements::Vector{<:Metadata};
                flags=API.LLVMDIFlagZero, derived_from=nothing,
                runtime_lang::Integer=0, vtable_holder=nothing,
                unique_id::AbstractString="") -> DICompositeType

Create a new struct type.
"""
function structtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::Vector{<:Metadata};
                     flags=API.LLVMDIFlagZero, derived_from=nothing,
                     runtime_lang::Integer=0, vtable_holder=nothing,
                     unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateStructType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        something(derived_from, C_NULL),
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        something(vtable_holder, C_NULL),
        unique_id, Csize_t(length(unique_id))))
end

"""
    uniontype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, elements::Vector{<:Metadata};
               flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
               unique_id::AbstractString="") -> DICompositeType

Create a new union type.
"""
function uniontype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, elements::Vector{<:Metadata};
                    flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
                    unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateUnionType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        unique_id, Csize_t(length(unique_id))))
end

"""
    classtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, offset_in_bits::Integer,
               elements::Vector{<:Metadata};
               flags=API.LLVMDIFlagZero, derived_from=nothing,
               vtable_holder=nothing, template_params=nothing,
               unique_id::AbstractString="") -> DICompositeType

Create a new C++ class type.
"""
function classtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, offset_in_bits::Integer,
                    elements::Vector{<:Metadata};
                    flags=API.LLVMDIFlagZero, derived_from=nothing,
                    vtable_holder=nothing, template_params=nothing,
                    unique_id::AbstractString="")
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateClassType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags,
        something(derived_from, C_NULL),
        elts, Cuint(length(elts)),
        something(vtable_holder, C_NULL),
        something(template_params, C_NULL),
        unique_id, Csize_t(length(unique_id))))
end

"""
    arraytype!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
               element_type::DIType, subscripts::Vector{<:Metadata}) -> DICompositeType

Create a new array type. Subscripts are typically built with
[`getorcreatesubrange!`](@ref).
"""
function arraytype!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                    element_type::DIType, subscripts::Vector{<:Metadata})
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateArrayType(
        builder, UInt64(size), UInt32(align_in_bits),
        element_type, subs, Cuint(length(subs))))
end

"""
    vectortype!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                element_type::DIType, subscripts::Vector{<:Metadata}) -> DICompositeType

Create a new vector type. Subscripts are typically built with
[`getorcreatesubrange!`](@ref).
"""
function vectortype!(builder::DIBuilder, size::Integer, align_in_bits::Integer,
                     element_type::DIType, subscripts::Vector{<:Metadata})
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
        builder, name, Csize_t(length(name)), Int64(value), unsigned))
end

"""
    enumerationtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::Vector{<:Metadata};
                     class_ty=nothing) -> DICompositeType

Create a new enumeration type. `elements` should be a vector of
[`DIEnumerator`](@ref) metadata nodes.
"""
function enumerationtype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                          file::DIFile, line::Integer, size_in_bits::Integer,
                          align_in_bits::Integer, elements::Vector{<:Metadata};
                          class_ty=nothing)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateEnumerationType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits),
        elts, Cuint(length(elts)),
        something(class_ty, C_NULL)))
end

"""
    forwarddecl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                 scope::DIScope, file::DIFile, line::Integer;
                 runtime_lang::Integer=0, size_in_bits::Integer=0,
                 align_in_bits::Integer=0,
                 unique_id::AbstractString="") -> DICompositeType

Create a new forward declaration to a composite type.
"""
function forwarddecl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                      scope::DIScope, file::DIFile, line::Integer;
                      runtime_lang::Integer=0, size_in_bits::Integer=0,
                      align_in_bits::Integer=0,
                      unique_id::AbstractString="")
    DICompositeType(API.LLVMDIBuilderCreateForwardDecl(
        builder, Cuint(tag), name, Csize_t(length(name)),
        scope, file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits),
        unique_id, Csize_t(length(unique_id))))
end

"""
    replaceablecompositetype!(builder::DIBuilder, tag::Integer,
                              name::AbstractString, scope::DIScope,
                              file::DIFile, line::Integer;
                              runtime_lang::Integer=0, size_in_bits::Integer=0,
                              align_in_bits::Integer=0,
                              flags=API.LLVMDIFlagZero,
                              unique_id::AbstractString="") -> DICompositeType

Create a new replaceable composite type forward declaration.
"""
function replaceablecompositetype!(builder::DIBuilder, tag::Integer,
                                   name::AbstractString, scope::DIScope,
                                   file::DIFile, line::Integer;
                                   runtime_lang::Integer=0, size_in_bits::Integer=0,
                                   align_in_bits::Integer=0,
                                   flags=API.LLVMDIFlagZero,
                                   unique_id::AbstractString="")
    DICompositeType(API.LLVMDIBuilderCreateReplaceableCompositeType(
        builder, Cuint(tag), name, Csize_t(length(name)),
        scope, file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        unique_id, Csize_t(length(unique_id))))
end


# subroutine types

"""
    subroutinetype!(builder::DIBuilder, file::DIFile,
                    parameter_types::Vector{<:Metadata};
                    flags=API.LLVMDIFlagZero) -> DISubroutineType

Create a new subroutine type. The first entry of `parameter_types` is the
return type; the rest are the parameter types.
"""
function subroutinetype!(builder::DIBuilder, file::DIFile,
                         parameter_types::Vector{<:Metadata};
                         flags=API.LLVMDIFlagZero)
    params = convert(Vector{Metadata}, parameter_types)
    DISubroutineType(API.LLVMDIBuilderCreateSubroutineType(
        builder, file, params, Cuint(length(params)), flags))
end


# subrange / array helpers

@public getorcreatearray!, getorcreatetypearray!

"""
    getorcreatesubrange!(builder::DIBuilder, lower_bound::Integer, count::Integer)

Get or create a subrange metadata node, describing one dimension of an array
or vector type.
"""
getorcreatesubrange!(builder::DIBuilder, lower_bound::Integer, count::Integer) =
    DISubrange(API.LLVMDIBuilderGetOrCreateSubrange(
        builder, Int64(lower_bound), Int64(count)))

"""
    getorcreatearray!(builder::DIBuilder, elements::Vector{<:Metadata})

Get or create a generic metadata array node, used for lists such as
`elements` fields of composite types.
"""
function getorcreatearray!(builder::DIBuilder, elements::Vector{<:Metadata})
    elts = convert(Vector{Metadata}, elements)
    Metadata(API.LLVMDIBuilderGetOrCreateArray(builder, elts, Csize_t(length(elts))))
end

"""
    getorcreatetypearray!(builder::DIBuilder, types::Vector{<:Metadata})

Get or create a metadata node for a type array, used for e.g. template
parameter lists.
"""
function getorcreatetypearray!(builder::DIBuilder, types::Vector{<:Metadata})
    tys = convert(Vector{Metadata}, types)
    Metadata(API.LLVMDIBuilderGetOrCreateTypeArray(builder, tys, Csize_t(length(tys))))
end


# ObjC

@public objcivar!, objcproperty!

"""
    objcivar!(builder::DIBuilder, name::AbstractString, file::DIFile,
              line::Integer, size_in_bits::Integer, align_in_bits::Integer,
              offset_in_bits::Integer, type::DIType, property_node::Metadata;
              flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new Objective-C instance variable.
"""
function objcivar!(builder::DIBuilder, name::AbstractString, file::DIFile,
                   line::Integer, size_in_bits::Integer, align_in_bits::Integer,
                   offset_in_bits::Integer, type::DIType, property_node::Metadata;
                   flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateObjCIVar(
        builder, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags, type, property_node))
end

"""
    objcproperty!(builder::DIBuilder, name::AbstractString, file::DIFile,
                  line::Integer, getter::AbstractString, setter::AbstractString,
                  attributes::Integer, type::DIType) -> DIDerivedType

Create a new Objective-C `@property` descriptor.
"""
function objcproperty!(builder::DIBuilder, name::AbstractString, file::DIFile,
                       line::Integer, getter::AbstractString, setter::AbstractString,
                       attributes::Integer, type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateObjCProperty(
        builder, name, Csize_t(length(name)),
        file, Cuint(line),
        getter, Csize_t(length(getter)),
        setter, Csize_t(length(setter)),
        Cuint(attributes), type))
end


# LLVM 21+ additions

@static if version() >= v"21"

@public settype!, subrangetype!, dynamicarraytype!, enumeratorarb!

"""
    settype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
             file::DIFile, line::Integer, size_in_bits::Integer,
             align_in_bits::Integer, base_type::DIType) -> DIDerivedType

Create a new set type (`DW_TAG_set_type`). Requires LLVM 21+.
"""
function settype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                  file::DIFile, line::Integer, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType)
    DIDerivedType(API.LLVMDIBuilderCreateSetType(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), base_type))
end

"""
    subrangetype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                  line::Integer, file::DIFile, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType;
                  flags=API.LLVMDIFlagZero,
                  lower_bound=nothing, upper_bound=nothing,
                  stride=nothing, bias=nothing)

Create a new subrange type. Requires LLVM 21+.
"""
function subrangetype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                       line::Integer, file::DIFile, size_in_bits::Integer,
                       align_in_bits::Integer, base_type::DIType;
                       flags=API.LLVMDIFlagZero,
                       lower_bound=nothing, upper_bound=nothing,
                       stride=nothing, bias=nothing)
    Metadata(API.LLVMDIBuilderCreateSubrangeType(
        builder, scope, name, Csize_t(length(name)),
        Cuint(line), file,
        UInt64(size_in_bits), UInt32(align_in_bits), flags, base_type,
        something(lower_bound, C_NULL),
        something(upper_bound, C_NULL),
        something(stride, C_NULL),
        something(bias, C_NULL)))
end

"""
    dynamicarraytype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                      line::Integer, file::DIFile, size::Integer,
                      align_in_bits::Integer, element_type::DIType,
                      subscripts::Vector{<:Metadata};
                      data_location=nothing, associated=nothing,
                      allocated=nothing, rank=nothing,
                      bit_stride=nothing) -> DICompositeType

Create a new dynamic array type (Fortran assumed-shape/deferred-shape arrays).
Requires LLVM 21+.
"""
function dynamicarraytype!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                           line::Integer, file::DIFile, size::Integer,
                           align_in_bits::Integer, element_type::DIType,
                           subscripts::Vector{<:Metadata};
                           data_location=nothing, associated=nothing,
                           allocated=nothing, rank=nothing,
                           bit_stride=nothing)
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateDynamicArrayType(
        builder, scope, name, Csize_t(length(name)),
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
    enumeratorarb!(builder::DIBuilder, name::AbstractString,
                   size_in_bits::Integer, words::Vector{UInt64};
                   unsigned::Bool=false) -> DIEnumerator

Create a new arbitrary-precision enumerator. Requires LLVM 21+.
"""
function enumeratorarb!(builder::DIBuilder, name::AbstractString,
                        size_in_bits::Integer, words::Vector{UInt64};
                        unsigned::Bool=false)
    DIEnumerator(API.LLVMDIBuilderCreateEnumeratorOfArbitraryPrecision(
        builder, name, Csize_t(length(name)),
        UInt64(size_in_bits), words, unsigned))
end

end # @static if version() >= v"21"


## subprogram

export DISubProgram, line
@public finalize_subprogram!

"""
    DISubProgram

A subprogram in the source code.
"""
@checked struct DISubProgram <: DIScope
    ref::API.LLVMMetadataRef
end
register(DISubProgram, API.LLVMDISubprogramMetadataKind)

"""
    line(subprogram::DISubProgram)

Get the line number of the given subprogram.
"""
line(subprogram::DISubProgram) = Int(API.LLVMDISubprogramGetLine(subprogram))

"""
    subprogram!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                linkage_name::AbstractString, file::DIFile, line::Integer,
                type::DISubroutineType, scope_line::Integer;
                is_local_to_unit::Bool=false, is_definition::Bool=true,
                flags=API.LLVMDIFlagZero,
                is_optimized::Bool=false) -> DISubProgram

Create a new [`DISubProgram`](@ref) describing a function.
"""
function subprogram!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                     linkage_name::AbstractString, file::DIFile, line::Integer,
                     type::DISubroutineType, scope_line::Integer;
                     is_local_to_unit::Bool=false, is_definition::Bool=true,
                     flags=API.LLVMDIFlagZero,
                     is_optimized::Bool=false)
    DISubProgram(API.LLVMDIBuilderCreateFunction(
        builder, scope, name, Csize_t(length(name)),
        linkage_name, Csize_t(length(linkage_name)),
        file, Cuint(line), type,
        is_local_to_unit, is_definition, Cuint(scope_line),
        flags, is_optimized))
end

"""
    finalize_subprogram!(builder::DIBuilder, sp::DISubProgram)

Finalize the given subprogram, allowing later changes to be disallowed. Must be
called before [`finalize!`](@ref).
"""
finalize_subprogram!(builder::DIBuilder, sp::DISubProgram) =
    API.LLVMDIBuilderFinalizeSubprogram(builder, sp)


## compile unit

export DICompileUnit
@public compileunit!

"""
    DICompileUnit

A compilation unit in the source code.
"""
@checked struct DICompileUnit <: DIScope
    ref::API.LLVMMetadataRef
end
register(DICompileUnit, API.LLVMDICompileUnitMetadataKind)

"""
    compileunit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                 optimized::Bool=true, flags::AbstractString="",
                 runtime_version::Integer=0,
                 split_name::Union{AbstractString,Nothing}=nothing,
                 emission_kind=API.LLVMDWARFEmissionFull,
                 dwo_id::Integer=0,
                 split_debug_inlining::Bool=true,
                 debug_info_for_profiling::Bool=false,
                 sysroot::AbstractString="", sdk::AbstractString="") -> DICompileUnit

Create a new [`DICompileUnit`](@ref). `lang` is a `LLVMDWARFSourceLanguage`
value (e.g. `LLVM.API.LLVMDWARFSourceLanguageJulia`).
"""
function compileunit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                      optimized::Bool=true,
                      flags::AbstractString="",
                      runtime_version::Integer=0,
                      split_name::Union{AbstractString,Nothing}=nothing,
                      emission_kind=API.LLVMDWARFEmissionFull,
                      dwo_id::Integer=0,
                      split_debug_inlining::Bool=true,
                      debug_info_for_profiling::Bool=false,
                      sysroot::AbstractString="",
                      sdk::AbstractString="")
    split_name_ptr = split_name === nothing ? C_NULL : split_name
    split_name_len = split_name === nothing ? Csize_t(0) : Csize_t(length(split_name))
    DICompileUnit(API.LLVMDIBuilderCreateCompileUnit(
        builder, lang, file,
        producer, Csize_t(length(producer)),
        optimized,
        flags, Csize_t(length(flags)),
        Cuint(runtime_version),
        split_name_ptr, split_name_len,
        emission_kind,
        Cuint(dwo_id),
        split_debug_inlining,
        debug_info_for_profiling,
        sysroot, Csize_t(length(sysroot)),
        sdk, Csize_t(length(sdk))))
end


## module

export DIModule
@public dimodule!

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
        name, Csize_t(length(name)),
        config_macros, Csize_t(length(config_macros)),
        include_path, Csize_t(length(include_path)),
        api_notes_file, Csize_t(length(api_notes_file))))
end


## variable factories

@public autovariable!, parametervariable!

"""
    autovariable!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                  file::DIFile, line::Integer, type::DIType;
                  always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                  align_in_bits::Integer=0) -> DILocalVariable

Create a new local variable descriptor (for a compiler-introduced automatic
variable).
"""
function autovariable!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                       file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                       align_in_bits::Integer=0)
    DILocalVariable(API.LLVMDIBuilderCreateAutoVariable(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line), type,
        always_preserve, flags, UInt32(align_in_bits)))
end

"""
    parametervariable!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                       arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false,
                       flags=API.LLVMDIFlagZero) -> DILocalVariable

Create a new descriptor for a function parameter variable. `arg_no` is
the 1-based parameter index.
"""
function parametervariable!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                            arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                            always_preserve::Bool=false,
                            flags=API.LLVMDIFlagZero)
    DILocalVariable(API.LLVMDIBuilderCreateParameterVariable(
        builder, scope, name, Csize_t(length(name)), Cuint(arg_no),
        file, Cuint(line), type,
        always_preserve, flags))
end


## expression

export DIExpression, DIGlobalVariableExpression, variable, expression
@public expression!, constantvalueexpression!

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
"""
@checked struct DIGlobalVariableExpression <: MDNode
    ref::API.LLVMMetadataRef
end
register(DIGlobalVariableExpression, API.LLVMDIGlobalVariableExpressionMetadataKind)

"""
    expression!(builder::DIBuilder,
                addr::Vector{UInt64}=UInt64[]) -> DIExpression

Create a new [`DIExpression`](@ref) from the given array of opcodes (encoding
a DWARF expression such as `DW_OP_plus_uconst`).
"""
function expression!(builder::DIBuilder, addr::Vector{UInt64}=UInt64[])
    DIExpression(API.LLVMDIBuilderCreateExpression(
        builder, addr, Csize_t(length(addr))))
end

"""
    constantvalueexpression!(builder::DIBuilder, value::Integer) -> DIExpression

Create a new [`DIExpression`](@ref) representing a single constant value.
"""
function constantvalueexpression!(builder::DIBuilder, value::Integer)
    DIExpression(API.LLVMDIBuilderCreateConstantValueExpression(
        builder, UInt64(value)))
end

"""
    variable(gve::DIGlobalVariableExpression)

Get the debug info global variable associated with the given expression.
"""
function variable(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetVariable(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIGlobalVariable
end

"""
    expression(gve::DIGlobalVariableExpression)

Get the debug info expression associated with the given global-variable pair.
"""
function expression(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetExpression(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIExpression
end


## global variable

@public globalvariableexpression!, tempglobalvariablefwddecl!

"""
    globalvariableexpression!(builder::DIBuilder, scope::DIScope,
                              name::AbstractString, linkage::AbstractString,
                              file::DIFile, line::Integer, type::DIType,
                              local_to_unit::Bool, expression::DIExpression;
                              declaration=nothing,
                              align_in_bits::Integer=0) -> DIGlobalVariableExpression

Create a new global variable descriptor paired with a DWARF expression.
"""
function globalvariableexpression!(builder::DIBuilder, scope::DIScope,
                                   name::AbstractString, linkage::AbstractString,
                                   file::DIFile, line::Integer, type::DIType,
                                   local_to_unit::Bool, expression::DIExpression;
                                   declaration=nothing,
                                   align_in_bits::Integer=0)
    DIGlobalVariableExpression(API.LLVMDIBuilderCreateGlobalVariableExpression(
        builder, scope, name, Csize_t(length(name)),
        linkage, Csize_t(length(linkage)),
        file, Cuint(line), type, local_to_unit, expression,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end

"""
    tempglobalvariablefwddecl!(builder::DIBuilder, scope::DIScope,
                               name::AbstractString, linkage::AbstractString,
                               file::DIFile, line::Integer, type::DIType,
                               local_to_unit::Bool;
                               declaration=nothing,
                               align_in_bits::Integer=0) -> DIGlobalVariable

Create a new temporary forward declaration for a global variable.
"""
function tempglobalvariablefwddecl!(builder::DIBuilder, scope::DIScope,
                                    name::AbstractString, linkage::AbstractString,
                                    file::DIFile, line::Integer, type::DIType,
                                    local_to_unit::Bool;
                                    declaration=nothing,
                                    align_in_bits::Integer=0)
    DIGlobalVariable(API.LLVMDIBuilderCreateTempGlobalVariableFwdDecl(
        builder, scope, name, Csize_t(length(name)),
        linkage, Csize_t(length(linkage)),
        file, Cuint(line), type, local_to_unit,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end


## lexical block

export DILexicalBlock, DILexicalBlockFile
@public lexicalblock!, lexicalblockfile!

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
    lexicalblock!(builder::DIBuilder, scope::DIScope, file::DIFile,
                  line::Integer, column::Integer) -> DILexicalBlock

Create a new [`DILexicalBlock`](@ref) describing a nested source scope.
"""
function lexicalblock!(builder::DIBuilder, scope::DIScope, file::DIFile,
                       line::Integer, column::Integer)
    DILexicalBlock(API.LLVMDIBuilderCreateLexicalBlock(
        builder, scope, file, Cuint(line), Cuint(column)))
end

"""
    lexicalblockfile!(builder::DIBuilder, scope::DIScope, file::DIFile,
                      discriminator::Integer=0) -> DILexicalBlockFile

Create a new [`DILexicalBlockFile`](@ref) for tracking source-file changes
within a lexical scope.
"""
function lexicalblockfile!(builder::DIBuilder, scope::DIScope, file::DIFile,
                           discriminator::Integer=0)
    DILexicalBlockFile(API.LLVMDIBuilderCreateLexicalBlockFile(
        builder, scope, file, Cuint(discriminator)))
end


## namespace

export DINamespace
@public namespace!

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
        name, Csize_t(length(name)),
        export_symbols))
end


## instruction insertion

@public declare_before!, declare_at_end!, value_before!, value_at_end!

@static if version() >= v"19"

export DbgRecord

"""
    DbgRecord

A non-instruction debug record attached to a basic block, replacing the
legacy `llvm.dbg.*` intrinsics in LLVM ≥ 19.
"""
@checked struct DbgRecord
    ref::API.LLVMDbgRecordRef
end

Base.unsafe_convert(::Type{API.LLVMDbgRecordRef}, record::DbgRecord) = record.ref

function Base.show(io::IO, record::DbgRecord)
    str_ptr = API.LLVMPrintDbgRecordToString(record)
    str = unsafe_string(str_ptr)
    print(io, rstrip(str))
    # LLVMPrintDbgRecordToString-returned memory is freed by LLVMDisposeMessage
    API.LLVMDisposeMessage(str_ptr)
end

"""
    declare_before!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    instr::Instruction) -> DbgRecord

Insert a new `#dbg_declare` record describing `storage` as the runtime
location of `var`, immediately before `instr`.
"""
declare_before!(builder::DIBuilder, storage::Value, var::DIVariable,
                expr::DIExpression, debugloc::DILocation, instr::Instruction) =
    DbgRecord(API.LLVMDIBuilderInsertDeclareRecordBefore(
        builder, storage, var, expr, debugloc, instr))

"""
    declare_at_end!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    block::BasicBlock) -> DbgRecord

Insert a new `#dbg_declare` record at the end of `block`.
"""
declare_at_end!(builder::DIBuilder, storage::Value, var::DIVariable,
                expr::DIExpression, debugloc::DILocation, block::BasicBlock) =
    DbgRecord(API.LLVMDIBuilderInsertDeclareRecordAtEnd(
        builder, storage, var, expr, debugloc, block))

"""
    value_before!(builder::DIBuilder, val::Value, var::DILocalVariable,
                  expr::DIExpression, debugloc::DILocation,
                  instr::Instruction) -> DbgRecord

Insert a new `#dbg_value` record describing `val` as the value of `var`,
immediately before `instr`.
"""
value_before!(builder::DIBuilder, val::Value, var::DIVariable,
              expr::DIExpression, debugloc::DILocation, instr::Instruction) =
    DbgRecord(API.LLVMDIBuilderInsertDbgValueRecordBefore(
        builder, val, var, expr, debugloc, instr))

"""
    value_at_end!(builder::DIBuilder, val::Value, var::DILocalVariable,
                  expr::DIExpression, debugloc::DILocation,
                  block::BasicBlock) -> DbgRecord

Insert a new `#dbg_value` record at the end of `block`.
"""
value_at_end!(builder::DIBuilder, val::Value, var::DIVariable,
              expr::DIExpression, debugloc::DILocation, block::BasicBlock) =
    DbgRecord(API.LLVMDIBuilderInsertDbgValueRecordAtEnd(
        builder, val, var, expr, debugloc, block))

else # LLVM < 19: legacy intrinsic-based insertion

"""
    declare_before!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    instr::Instruction) -> Instruction

Insert a new `llvm.dbg.declare` intrinsic call immediately before `instr`.
"""
declare_before!(builder::DIBuilder, storage::Value, var::DIVariable,
                expr::DIExpression, debugloc::DILocation, instr::Instruction) =
    Instruction(API.LLVMDIBuilderInsertDeclareBefore(
        builder, storage, var, expr, debugloc, instr))

"""
    declare_at_end!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    block::BasicBlock) -> Instruction

Insert a new `llvm.dbg.declare` intrinsic call at the end of `block`.
"""
declare_at_end!(builder::DIBuilder, storage::Value, var::DIVariable,
                expr::DIExpression, debugloc::DILocation, block::BasicBlock) =
    Instruction(API.LLVMDIBuilderInsertDeclareAtEnd(
        builder, storage, var, expr, debugloc, block))

"""
    value_before!(builder::DIBuilder, val::Value, var::DILocalVariable,
                  expr::DIExpression, debugloc::DILocation,
                  instr::Instruction) -> Instruction

Insert a new `llvm.dbg.value` intrinsic call immediately before `instr`.
"""
value_before!(builder::DIBuilder, val::Value, var::DIVariable,
              expr::DIExpression, debugloc::DILocation, instr::Instruction) =
    Instruction(API.LLVMDIBuilderInsertDbgValueBefore(
        builder, val, var, expr, debugloc, instr))

"""
    value_at_end!(builder::DIBuilder, val::Value, var::DILocalVariable,
                  expr::DIExpression, debugloc::DILocation,
                  block::BasicBlock) -> Instruction

Insert a new `llvm.dbg.value` intrinsic call at the end of `block`.
"""
value_at_end!(builder::DIBuilder, val::Value, var::DIVariable,
              expr::DIExpression, debugloc::DILocation, block::BasicBlock) =
    Instruction(API.LLVMDIBuilderInsertDbgValueAtEnd(
        builder, val, var, expr, debugloc, block))

end # @static version check


## label (LLVM 20+)

@static if version() >= v"20"

export DILabel
@public label!, label_before!, label_at_end!

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
    label!(builder::DIBuilder, scope::DIScope, name::AbstractString,
           file::DIFile, line::Integer;
           always_preserve::Bool=false) -> DILabel

Create a new [`DILabel`](@ref). Requires LLVM 20+.
"""
function label!(builder::DIBuilder, scope::DIScope, name::AbstractString,
                file::DIFile, line::Integer;
                always_preserve::Bool=false)
    DILabel(API.LLVMDIBuilderCreateLabel(
        builder, scope, name, Csize_t(length(name)),
        file, Cuint(line), always_preserve))
end

"""
    label_before!(builder::DIBuilder, label::DILabel,
                  location::DILocation, instr::Instruction) -> DbgRecord

Insert a new label record immediately before `instr`. Requires LLVM 20+.
"""
label_before!(builder::DIBuilder, label::DILabel,
              location::DILocation, instr::Instruction) =
    DbgRecord(API.LLVMDIBuilderInsertLabelBefore(builder, label, location, instr))

"""
    label_at_end!(builder::DIBuilder, label::DILabel,
                  location::DILocation, block::BasicBlock) -> DbgRecord

Insert a new label record at the end of `block`. Requires LLVM 20+.
"""
label_at_end!(builder::DIBuilder, label::DILabel,
              location::DILocation, block::BasicBlock) =
    DbgRecord(API.LLVMDIBuilderInsertLabelAtEnd(builder, label, location, block))

end # @static version check


## imported entity

export DIImportedEntity
@public importedmodulefromnamespace!, importedmodulefromalias!,
        importedmodulefrommodule!, importeddeclaration!

"""
    DIImportedEntity

An imported entity, such as a C++ `using` declaration or module import.
"""
@checked struct DIImportedEntity <: DINode
    ref::API.LLVMMetadataRef
end
register(DIImportedEntity, API.LLVMDIImportedEntityMetadataKind)

"""
    importedmodulefromnamespace!(builder::DIBuilder, scope::DIScope,
                                 ns::DINamespace, file::DIFile,
                                 line::Integer) -> DIImportedEntity

Create a new `DIImportedEntity` from a namespace.
"""
importedmodulefromnamespace!(builder::DIBuilder, scope::DIScope, ns::DINamespace,
                             file::DIFile, line::Integer) =
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromNamespace(
        builder, scope, ns, file, Cuint(line)))

"""
    importedmodulefromalias!(builder::DIBuilder, scope::DIScope,
                             imported::DIImportedEntity, file::DIFile,
                             line::Integer,
                             elements::Vector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from an alias.
"""
function importedmodulefromalias!(builder::DIBuilder, scope::DIScope,
                                  imported::DIImportedEntity, file::DIFile,
                                  line::Integer,
                                  elements::Vector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromAlias(
        builder, scope, imported, file, Cuint(line),
        elts, Cuint(length(elts))))
end

"""
    importedmodulefrommodule!(builder::DIBuilder, scope::DIScope,
                              mod::DIModule, file::DIFile, line::Integer,
                              elements::Vector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from a module.
"""
function importedmodulefrommodule!(builder::DIBuilder, scope::DIScope,
                                   mod::DIModule, file::DIFile, line::Integer,
                                   elements::Vector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromModule(
        builder, scope, mod, file, Cuint(line),
        elts, Cuint(length(elts))))
end

"""
    importeddeclaration!(builder::DIBuilder, scope::DIScope, decl::Metadata,
                        file::DIFile, line::Integer, name::AbstractString,
                        elements::Vector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new `DIImportedEntity` from a declaration.
"""
function importeddeclaration!(builder::DIBuilder, scope::DIScope, decl::Metadata,
                              file::DIFile, line::Integer, name::AbstractString,
                              elements::Vector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedDeclaration(
        builder, scope, decl, file, Cuint(line),
        name, Csize_t(length(name)),
        elts, Cuint(length(elts))))
end


## macro

export DIMacro, DIMacroFile
@public macro!, tempmacrofile!

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
        name, Csize_t(length(name)),
        value, Csize_t(length(value))))
end

"""
    tempmacrofile!(builder::DIBuilder,
                   parent_macrofile::Union{DIMacroFile,Nothing},
                   line::Integer, file::DIFile) -> DIMacroFile

Create a new (temporary) [`DIMacroFile`](@ref).
"""
tempmacrofile!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
               line::Integer, file::DIFile) =
    DIMacroFile(API.LLVMDIBuilderCreateTempMacroFile(
        builder, something(parent_macrofile, C_NULL), Cuint(line), file))


## instruction debug location

# re-uses the existing `debuglocation` / `debuglocation!` exports on IRBuilder.

"""
    debuglocation(inst::Instruction) -> Union{DILocation,Nothing}

Get the debug location attached to the given instruction, or `nothing`.
"""
function debuglocation(inst::Instruction)
    ref = API.LLVMInstructionGetDebugLoc(inst)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end

"""
    debuglocation!(inst::Instruction, loc::DILocation)

Set the debug location of the given instruction.
"""
debuglocation!(inst::Instruction, loc::DILocation) =
    API.LLVMInstructionSetDebugLoc(inst, loc)


## mutation / advanced helpers

@public temporary_mdnode, dispose_temporary, replace_all_uses_with!

"""
    temporary_mdnode(operands::Vector{<:Metadata}=Metadata[]) -> MDNode

Create a temporary metadata node in the task-local [`context`](@ref) with the
given operands. Temporary nodes are useful for constructing cycles and must be
either replaced via [`replace_all_uses_with!`](@ref) or disposed of via
[`dispose_temporary`](@ref).
"""
function temporary_mdnode(operands::Vector{<:Metadata}=Metadata[])
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
    replace_all_uses_with!(temp::Metadata, replacement::Metadata)

Replace all uses of `temp` with `replacement`, and dispose of `temp`.
"""
replace_all_uses_with!(temp::Metadata, replacement::Metadata) =
    API.LLVMMetadataReplaceAllUsesWith(temp, replacement)


@static if version() >= v"21"

@public replacearrays!, replacetype!

"""
    replacearrays!(builder::DIBuilder, T::DICompositeType,
                   elements::Vector{<:Metadata})

Replace the elements array of the given composite type `T`. Requires LLVM 21+.
"""
function replacearrays!(builder::DIBuilder, T::DICompositeType,
                        elements::Vector{<:Metadata})
    elts = convert(Vector{Metadata}, elements)
    tref = Ref(T.ref)
    API.LLVMReplaceArrays(builder, tref, elts, Cuint(length(elts)))
    return Metadata(tref[])::DICompositeType
end

"""
    replacetype!(sp::DISubProgram, ty::DISubroutineType)

Replace the type of the given subprogram. Requires LLVM 21+.
"""
replacetype!(sp::DISubProgram, ty::DISubroutineType) =
    API.LLVMDISubprogramReplaceType(sp, ty)

end # @static version check


## other

export DEBUG_METADATA_VERSION, strip_debuginfo!, subprogram, subprogram!

"""
    DEBUG_METADATA_VERSION()

The current debug info version number, as supported by LLVM.
"""
DEBUG_METADATA_VERSION() = API.LLVMDebugMetadataVersion()

"""
    debug_metadata_version(mod::Module)

Get the debug info version number emitted in the given module, or `0` if none
is attached.
"""
debug_metadata_version(mod::Module) = Int(API.LLVMGetModuleDebugMetadataVersion(mod))
@public debug_metadata_version

"""
    strip_debuginfo!(mod::Module)

Strip the debug information from the given module.
"""
strip_debuginfo!(mod::Module) = API.LLVMStripModuleDebugInfo(mod)

"""
    subprogram(func::Function) -> DISubProgram

Get the subprogram of the given function, or `nothing` if the function has no subprogram.
"""
function subprogram(func::Function)
    ref = API.LLVMGetSubprogram(func)
    ref==C_NULL ? nothing : Metadata(ref)::DISubProgram
end

"""
    subprogram!(func::Function, sp::DISubProgram)

Set the subprogram of the given function.
"""
subprogram!(func::Function, sp::DISubProgram) = API.LLVMSetSubprogram(func, sp)
