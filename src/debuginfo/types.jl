## type

@vocabulary IR DIType, DIEnumerator, DISubrange
@vocabulary Build basic_type!, unspecified_type!, pointer_type!, reference_type!, nullptr_type!,
        typedef_type!, qualified_type!, artificial_type!, object_pointer_type!,
        inheritance!, member_type!, bitfield_member_type!, static_member_type!,
        member_pointer_type!, struct_type!, union_type!, class_type!, array_type!,
        vector_type!, enumeration_type!, enumerator!, forward_decl!,
        replaceable_composite_type!, subroutine_type!, subrange!

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
    name = String(name)
    DIBasicType(API.LLVMDIBuilderCreateBasicType(
        builder, name, Csize_t(ncodeunits(name)),
        UInt64(size_in_bits), Cuint(encoding), flags))
end

"""
    unspecified_type!(builder::DIBuilder, name::AbstractString) -> DIBasicType

Create a new unspecified type (`DW_TAG_unspecified_type`), e.g. a C++ `decltype(nullptr)`.
"""
function unspecified_type!(builder::DIBuilder, name::AbstractString)
    name = String(name)
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
    name = String(name)
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
                 file::DIFile, line::Integer, scope::Union{DIScope,Nothing};
                 align_in_bits::Integer=0) -> DIDerivedType

Create a new typedef type.
"""
function typedef_type!(builder::DIBuilder, type::DIType, name::AbstractString,
                      file::DIFile, line::Integer, scope::Union{DIScope,Nothing};
                      align_in_bits::Integer=0)
    name = String(name)
    DIDerivedType(API.LLVMDIBuilderCreateTypedef(
        builder, type, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), something(scope, C_NULL), UInt32(align_in_bits)))
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
    inheritance!(builder::DIBuilder, derived::DIType, base::DIType, base_offset::Integer;
                 vbptr_offset::Integer=0, flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new inheritance relationship from `derived` to `base`.
"""
function inheritance!(builder::DIBuilder, derived::DIType, base::DIType,
                      base_offset::Integer; vbptr_offset::Integer=0,
                      flags=API.LLVMDIFlagZero)
    DIDerivedType(API.LLVMDIBuilderCreateInheritance(
        builder, derived, base, UInt64(base_offset), UInt32(vbptr_offset), flags))
end

"""
    member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, offset_in_bits::Integer,
                type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new member (field) of a composite type.
"""
function member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, offset_in_bits::Integer,
                     type::DIType; flags=API.LLVMDIFlagZero)
    name = String(name)
    DIDerivedType(API.LLVMDIBuilderCreateMemberType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), UInt64(offset_in_bits),
        flags, type))
end

"""
    bitfield_member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                        file::DIFile, line::Integer, size_in_bits::Integer,
                        offset_in_bits::Integer, storage_offset_in_bits::Integer,
                        type::DIType; flags=API.LLVMDIFlagZero) -> DIDerivedType

Create a new bit-field member of a composite type.
"""
function bitfield_member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                             file::DIFile, line::Integer, size_in_bits::Integer,
                             offset_in_bits::Integer, storage_offset_in_bits::Integer,
                             type::DIType; flags=API.LLVMDIFlagZero)
    name = String(name)
    DIDerivedType(API.LLVMDIBuilderCreateBitFieldMemberType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt64(offset_in_bits), UInt64(storage_offset_in_bits),
        flags, type))
end

"""
    static_member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                      file::DIFile, line::Integer, type::DIType,
                      constant_val::Constant;
                      flags=API.LLVMDIFlagZero,
                      align_in_bits::Integer=0) -> DIDerivedType

Create a new static member of a composite type. `constant_val` is required:
the underlying C entry point unconditionally `cast<Constant>`s it and
crashes on null.
"""
function static_member_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                           file::DIFile, line::Integer, type::DIType,
                           constant_val::Constant;
                           flags=API.LLVMDIFlagZero,
                           align_in_bits::Integer=0)
    name = String(name)
    DIDerivedType(API.LLVMDIBuilderCreateStaticMemberType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
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
    struct_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                file::DIFile, line::Integer, size_in_bits::Integer,
                align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                flags=API.LLVMDIFlagZero, derived_from=nothing,
                runtime_lang::Integer=0, vtable_holder=nothing,
                unique_id::AbstractString="") -> DICompositeType

Create a new struct type.
"""
function struct_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                     flags=API.LLVMDIFlagZero, derived_from=nothing,
                     runtime_lang::Integer=0, vtable_holder=nothing,
                     unique_id::AbstractString="")
    name = String(name)
    unique_id = String(unique_id)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateStructType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        something(derived_from, C_NULL),
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        something(vtable_holder, C_NULL),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    union_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, elements::AbstractVector{<:Metadata};
               flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
               unique_id::AbstractString="") -> DICompositeType

Create a new union type.
"""
function union_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                    flags=API.LLVMDIFlagZero, runtime_lang::Integer=0,
                    unique_id::AbstractString="")
    name = String(name)
    unique_id = String(unique_id)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateUnionType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        elts, Cuint(length(elts)),
        Cuint(runtime_lang),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    class_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
               file::DIFile, line::Integer, size_in_bits::Integer,
               align_in_bits::Integer, offset_in_bits::Integer,
               elements::AbstractVector{<:Metadata};
               flags=API.LLVMDIFlagZero, derived_from=nothing,
               vtable_holder=nothing, template_params=nothing,
               unique_id::AbstractString="") -> DICompositeType

Create a new C++ class type.
"""
function class_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                    file::DIFile, line::Integer, size_in_bits::Integer,
                    align_in_bits::Integer, offset_in_bits::Integer,
                    elements::AbstractVector{<:Metadata};
                    flags=API.LLVMDIFlagZero, derived_from=nothing,
                    vtable_holder=nothing, template_params=nothing,
                    unique_id::AbstractString="")
    name = String(name)
    unique_id = String(unique_id)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateClassType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
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
    array_type!(builder::DIBuilder, size_in_bits::Integer, align_in_bits::Integer,
               element_type::DIType, subscripts::AbstractVector{<:Metadata}) -> DICompositeType

Create a new array type. Subscripts are typically built with
[`subrange!`](@ref).
"""
function array_type!(builder::DIBuilder, size_in_bits::Integer, align_in_bits::Integer,
                    element_type::DIType, subscripts::AbstractVector{<:Metadata})
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateArrayType(
        builder, UInt64(size_in_bits), UInt32(align_in_bits),
        element_type, subs, Cuint(length(subs))))
end

"""
    vector_type!(builder::DIBuilder, size_in_bits::Integer, align_in_bits::Integer,
                element_type::DIType, subscripts::AbstractVector{<:Metadata}) -> DICompositeType

Create a new vector type. Subscripts are typically built with
[`subrange!`](@ref).
"""
function vector_type!(builder::DIBuilder, size_in_bits::Integer, align_in_bits::Integer,
                     element_type::DIType, subscripts::AbstractVector{<:Metadata})
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateVectorType(
        builder, UInt64(size_in_bits), UInt32(align_in_bits),
        element_type, subs, Cuint(length(subs))))
end

"""
    enumerator!(builder::DIBuilder, name::AbstractString, value::Integer;
                unsigned::Bool=false, size_in_bits::Integer=64) -> DIEnumerator

Create a new enumerator for use inside an enumeration type. The value is interpreted as a
signed or `unsigned` integer of `size_in_bits` bits, and must fit in it. Sizes other than
64 bits require LLVM 21+.
"""
function enumerator!(builder::DIBuilder, name::AbstractString, value::Integer;
                     unsigned::Bool=false, size_in_bits::Integer=64)
    name = String(name)
    size_in_bits > 0 || throw(ArgumentError("The size of an enumerator must be positive"))
    lo, hi = unsigned ? (big(0), big(2)^size_in_bits - 1) :
                        (-big(2)^(size_in_bits-1), big(2)^(size_in_bits-1) - 1)
    lo <= value <= hi ||
        throw(ArgumentError("The value $value does not fit in " *
                            (unsigned ? "an unsigned" : "a signed") *
                            " enumerator of $size_in_bits bits"))
    if size_in_bits == 64
        bits = unsigned ? reinterpret(Int64, UInt64(value)) : Int64(value)
        return DIEnumerator(API.LLVMDIBuilderCreateEnumerator(
            builder, name, Csize_t(ncodeunits(name)), bits, unsigned))
    end
    @static if version() >= v"21"
        # the two's complement words of the value, as LLVM's APInt stores them
        val = big(value)
        words = UInt64[(val >> (64*(i-1))) % UInt64 for i in 1:cld(size_in_bits, 64)]
        DIEnumerator(API.LLVMDIBuilderCreateEnumeratorOfArbitraryPrecision(
            builder, name, Csize_t(ncodeunits(name)), UInt64(size_in_bits), words,
            unsigned))
    else
        throw(ArgumentError("Enumerators with a size other than 64 bits require LLVM 21+"))
    end
end

"""
    enumeration_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                     file::DIFile, line::Integer, size_in_bits::Integer,
                     align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                     underlying_type=nothing) -> DICompositeType

Create a new enumeration type. `elements` should be a vector of
[`DIEnumerator`](@ref) metadata nodes, and `underlying_type` is the integer type of the
enumeration, if it has one.
"""
function enumeration_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                          file::DIFile, line::Integer, size_in_bits::Integer,
                          align_in_bits::Integer, elements::AbstractVector{<:Metadata};
                          underlying_type::Union{DIType,Nothing}=nothing)
    name = String(name)
    elts = convert(Vector{Metadata}, elements)
    DICompositeType(API.LLVMDIBuilderCreateEnumerationType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits),
        elts, Cuint(length(elts)),
        something(underlying_type, C_NULL)))
end

"""
    forward_decl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                 scope::Union{DIScope,Nothing}, file::DIFile, line::Integer;
                 runtime_lang::Integer=0, size_in_bits::Integer=0,
                 align_in_bits::Integer=0,
                 unique_id::AbstractString="") -> DICompositeType

Create a new forward declaration to a composite type.
"""
function forward_decl!(builder::DIBuilder, tag::Integer, name::AbstractString,
                      scope::Union{DIScope,Nothing}, file::DIFile, line::Integer;
                      runtime_lang::Integer=0, size_in_bits::Integer=0,
                      align_in_bits::Integer=0,
                      unique_id::AbstractString="")
    name = String(name)
    unique_id = String(unique_id)
    DICompositeType(API.LLVMDIBuilderCreateForwardDecl(
        builder, Cuint(tag), name, Csize_t(ncodeunits(name)),
        something(scope, C_NULL), file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits),
        unique_id, Csize_t(ncodeunits(unique_id))))
end

"""
    replaceable_composite_type!(builder::DIBuilder, tag::Integer,
                              name::AbstractString, scope::Union{DIScope,Nothing},
                              file::DIFile, line::Integer;
                              runtime_lang::Integer=0, size_in_bits::Integer=0,
                              align_in_bits::Integer=0,
                              flags=API.LLVMDIFlagZero,
                              unique_id::AbstractString="")
        -> TemporaryMDNode{DICompositeType}

Create a temporary composite type, a placeholder for a type that refers to itself, e.g.,
through a pointer to it. Build the complete type with the temporary type as a
placeholder, and then replace it with [`replace_temporary!`](@ref), before the builder is
finalized:

```julia
fwd = replaceable_composite_type!(dib, DW_TAG_structure_type, "Node", nothing, file, 1)
next = member_type!(dib, nothing, "next", file, 2, 64, 64, 0, pointer_type!(dib, fwd.node, 64))
node = struct_type!(dib, nothing, "Node", file, 1, 64, 64, [next])
replace_temporary!(fwd, node)
```
"""
function replaceable_composite_type!(builder::DIBuilder, tag::Integer,
                                   name::AbstractString, scope::Union{DIScope,Nothing},
                                   file::DIFile, line::Integer;
                                   runtime_lang::Integer=0, size_in_bits::Integer=0,
                                   align_in_bits::Integer=0,
                                   flags=API.LLVMDIFlagZero,
                                   unique_id::AbstractString="")
    name = String(name)
    unique_id = String(unique_id)
    TemporaryMDNode{DICompositeType}(API.LLVMDIBuilderCreateReplaceableCompositeType(
        builder, Cuint(tag), name, Csize_t(ncodeunits(name)),
        something(scope, C_NULL), file, Cuint(line), Cuint(runtime_lang),
        UInt64(size_in_bits), UInt32(align_in_bits), flags,
        unique_id, Csize_t(ncodeunits(unique_id))))
end


# subroutine types

"""
    subroutine_type!(builder::DIBuilder, file::DIFile,
                    return_type::Union{DIType,Nothing},
                    parameter_types::AbstractVector=Metadata[];
                    flags=API.LLVMDIFlagZero) -> DISubroutineType

Create a new subroutine type with the given return and parameter types. Pass
`nothing` for `return_type` to describe a `void`-returning subroutine, and as the last
parameter type of a variadic subroutine. The parameter types must be [`DIType`](@ref)s or
`nothing`.
"""
function subroutine_type!(builder::DIBuilder, file::DIFile,
                         return_type::Union{DIType,Nothing},
                         parameter_types::AbstractVector=Metadata[];
                         flags=API.LLVMDIFlagZero)
    # LLVM packs the return type as the 0th element of the parameter-types array,
    # with a null entry standing for `void`.
    params = API.LLVMMetadataRef[
        return_type === nothing ? C_NULL :
            Base.unsafe_convert(API.LLVMMetadataRef, return_type)]
    for p in parameter_types
        p === nothing || p isa DIType ||
            throw(ArgumentError("Parameter types must be DITypes or nothing, got $(typeof(p))"))
        push!(params, p === nothing ? C_NULL : Base.unsafe_convert(API.LLVMMetadataRef, p))
    end
    DISubroutineType(API.LLVMDIBuilderCreateSubroutineType(
        builder, file, params, Cuint(length(params)), flags))
end


# subranges

"""
    subrange!(builder::DIBuilder, lower_bound::Integer, count::Integer) -> DISubrange

Get or create a subrange, describing one dimension of an array or vector type.
"""
subrange!(builder::DIBuilder, lower_bound::Integer, count::Integer) =
    DISubrange(API.LLVMDIBuilderGetOrCreateSubrange(
        builder, Int64(lower_bound), Int64(count)))


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
    name = String(name)
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
    name = String(name)
    getter = String(getter)
    setter = String(setter)
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
@vocabulary Build set_type!, subrange_type!, dynamic_array_type!

@doc """
    DISubrangeType <: DIType

A subrange type (an integer range type, as found in Fortran or Ada), built
with [`subrange_type!`](@ref). Requires LLVM 21+.
"""
@checked struct DISubrangeType <: DIType
    ref::API.LLVMMetadataRef
end
register(DISubrangeType, API.LLVMDISubrangeTypeMetadataKind)

@doc """
    set_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
             file::DIFile, line::Integer, size_in_bits::Integer,
             align_in_bits::Integer, base_type::DIType) -> DIDerivedType

Create a new set type (`DW_TAG_set_type`). Requires LLVM 21+.
"""
function set_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                  file::DIFile, line::Integer, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType)
    name = String(name)
    DIDerivedType(API.LLVMDIBuilderCreateSetType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        file, Cuint(line),
        UInt64(size_in_bits), UInt32(align_in_bits), base_type))
end

@doc """
    subrange_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                  file::DIFile, line::Integer, size_in_bits::Integer,
                  align_in_bits::Integer, base_type::DIType;
                  flags=API.LLVMDIFlagZero,
                  lower_bound=nothing, upper_bound=nothing,
                  stride=nothing, bias=nothing) -> DISubrangeType

Create a new subrange type. Requires LLVM 21+.
"""
function subrange_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                       file::DIFile, line::Integer, size_in_bits::Integer,
                       align_in_bits::Integer, base_type::DIType;
                       flags=API.LLVMDIFlagZero,
                       lower_bound=nothing, upper_bound=nothing,
                       stride=nothing, bias=nothing)
    name = String(name)
    DISubrangeType(API.LLVMDIBuilderCreateSubrangeType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        Cuint(line), file,
        UInt64(size_in_bits), UInt32(align_in_bits), flags, base_type,
        something(lower_bound, C_NULL),
        something(upper_bound, C_NULL),
        something(stride, C_NULL),
        something(bias, C_NULL)))
end

@doc """
    dynamic_array_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                      file::DIFile, line::Integer, size_in_bits::Integer,
                      align_in_bits::Integer, element_type::DIType,
                      subscripts::AbstractVector{<:Metadata};
                      data_location=nothing, associated=nothing,
                      allocated=nothing, rank=nothing,
                      bit_stride=nothing) -> DICompositeType

Create a new dynamic array type (Fortran assumed-shape/deferred-shape arrays).
Requires LLVM 21+.
"""
function dynamic_array_type!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                           file::DIFile, line::Integer, size_in_bits::Integer,
                           align_in_bits::Integer, element_type::DIType,
                           subscripts::AbstractVector{<:Metadata};
                           data_location=nothing, associated=nothing,
                           allocated=nothing, rank=nothing,
                           bit_stride=nothing)
    name = String(name)
    subs = convert(Vector{Metadata}, subscripts)
    DICompositeType(API.LLVMDIBuilderCreateDynamicArrayType(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        Cuint(line), file,
        UInt64(size_in_bits), UInt32(align_in_bits), element_type,
        subs, Cuint(length(subs)),
        something(data_location, C_NULL),
        something(associated, C_NULL),
        something(allocated, C_NULL),
        something(rank, C_NULL),
        something(bit_stride, C_NULL)))
end


end # @static if version() >= v"21"

@static if version() >= v"21"

@vocabulary Build replace_arrays!

@doc """
    replace_arrays!(builder::DIBuilder, T::DICompositeType,
                   elements::AbstractVector{<:Metadata}) -> DICompositeType

Replace the elements array of the given composite type `T`, and return the resulting type.
Use the returned type instead of `T`, as LLVM can replace `T` with an existing, identical
type. Requires LLVM 21+.
"""
function replace_arrays!(builder::DIBuilder, T::DICompositeType,
                        elements::AbstractVector{<:Metadata})
    elts = convert(Vector{Metadata}, elements)
    tref = Ref(T.ref)
    API.LLVMReplaceArrays(builder, tref, elts, Cuint(length(elts)))
    return Metadata(tref[])::DICompositeType
end

end # @static version check
