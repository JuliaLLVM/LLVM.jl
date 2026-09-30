# Attributes that can be associated with parameters, function results, or the function
# itself.

@vocabulary IR Attribute,
               EnumAttribute, StringAttribute, TypeAttribute,
               ConstantRangeAttribute, ConstantRangeListAttribute

"""
    Attribute

An attribute of a function, of its return value or one of its parameters, or of a call
site. Attributes are immutable, and are created using one of the constructors of its
subtypes: [`EnumAttribute`](@ref), [`TypeAttribute`](@ref), [`StringAttribute`](@ref), or
`ConstantRangeAttribute` (LLVM 19+).

# Properties

    attr.kind

The kind of the attribute: a `Symbol` naming one of LLVM's attribute kinds (like
`:nounwind` or `:align`) for enum, type and constant range attributes, or the `String` name
of a string attribute. Attribute sets can be indexed by this kind, e.g.,
`f.function_attributes[attr.kind]`.

    attr.value

The value of the attribute: an integer for enum attributes (0 if the attribute has no
value), a string for string attributes, and a type for type attributes. Constant range
attributes do not have this property, as the C API cannot read their value.
"""
abstract type Attribute end
@properties Attribute

Base.unsafe_convert(::Type{API.LLVMAttributeRef}, attr::Attribute) = attr.ref

@checked struct EnumAttribute <: Attribute
    ref::API.LLVMAttributeRef
end

@checked struct StringAttribute <: Attribute
    ref::API.LLVMAttributeRef
end

@checked struct TypeAttribute <: Attribute
    ref::API.LLVMAttributeRef
end

# ConstantRange attribute kind (e.g. `range`, introduced in LLVM 17).
@checked struct ConstantRangeAttribute <: Attribute
    ref::API.LLVMAttributeRef
end

# ConstantRangeList attribute kind (e.g. `initializes`, introduced in LLVM 20).
@checked struct ConstantRangeListAttribute <: Attribute
    ref::API.LLVMAttributeRef
end

# TODO: make the identify mechanism flexible enough to cover cases like this one,
#       and not only Value and Type

function Attribute(ref::API.LLVMAttributeRef)
    ref == C_NULL && throw(UndefRefError())
    if Bool(API.LLVMIsEnumAttribute(ref))
        return EnumAttribute(ref)
    elseif Bool(API.LLVMIsStringAttribute(ref))
        return StringAttribute(ref)
    elseif Bool(API.LLVMIsTypeAttribute(ref))
        return TypeAttribute(ref)
    else
        @static if version() >= v"20"
            Bool(API.LLVMIsConstantRangeAttribute(ref)) && return ConstantRangeAttribute(ref)
            Bool(API.LLVMIsConstantRangeListAttribute(ref)) && return ConstantRangeListAttribute(ref)
        elseif version() >= v"19"
            Bool(API.LLVMIsConstantRangeAttribute(ref)) && return ConstantRangeAttribute(ref)
        end
        error("unknown attribute kind")
    end
end

# display attributes as the call that creates them (eliding the value of range attributes,
# which the C API cannot read back)
function Base.show(io::IO, attr::Attribute)
    print(io, nameof(typeof(attr)), "(")
    show(io, kind(attr))
    if attr isa Union{EnumAttribute,StringAttribute,TypeAttribute}
        val = value(attr)
        if !(attr isa EnumAttribute && val == 0) && !(attr isa StringAttribute && isempty(val))
            print(io, ", ")
            val isa Integer ? print(io, val) : show(io, val)
        end
    else
        print(io, ", ...")
    end
    print(io, ")")
end

# LLVM's attribute kinds are an enum that is not part of the C API, so we identify them by
# their name, as used in textual IR. these are the integer IDs that the C API works with.
attribute_kind_id(attr::Attribute) = API.LLVMGetEnumAttributeKind(attr)
attribute_kind_id(name::Symbol) =
    API.LLVMGetEnumAttributeKindForName(name, ccall(:strlen, Csize_t, (Cstring,), name))
attribute_kind_id(name::String) =
    API.LLVMGetEnumAttributeKindForName(name, ncodeunits(name))

# the category of an attribute kind, which determines the kind of attribute it is used for
function attribute_kind_category(id::Integer)
    Bool(API.LLVMExtraIsEnumAttributeKind(id)) && return :enum
    Bool(API.LLVMExtraIsIntAttributeKind(id)) && return :int
    Bool(API.LLVMExtraIsTypeAttributeKind(id)) && return :type
    @static if version() >= v"19"
        Bool(API.LLVMExtraIsConstantRangeAttributeKind(id)) && return :range
    end
    return :other
end

# look up the ID of an attribute kind, checking that it can be used for a certain kind of
# attribute, as LLVM does not check this (except for an assertion)
function checked_attribute_kind_id(name::Union{Symbol,String}, categories::Symbol...)
    id = attribute_kind_id(name)
    id == 0 && throw(ArgumentError("Unknown attribute kind: $name"))
    category = attribute_kind_category(id)
    if !(category in categories)
        constructor = category in (:enum, :int) ? "EnumAttribute" :
                      category == :type ? "TypeAttribute" :
                      category == :range ? "ConstantRangeAttribute" : nothing
        msg = "Attribute kind $name cannot be used for this kind of attribute"
        constructor === nothing || (msg *= "; use $constructor instead")
        throw(ArgumentError(msg))
    end
    return id
end

function attribute_kind_name(id::Integer)
    len = Ref{Csize_t}()
    data = API.LLVMExtraGetAttributeKindName(id, len)
    data == C_NULL && error("Unknown attribute kind ID $id")
    return Symbol(unsafe_string(convert(Ptr{UInt8}, data), len[]))
end


## enum attribute

"""
    EnumAttribute(kind::Symbol, value::Integer=0)

Create an attribute of one of LLVM's attribute kinds, e.g., `EnumAttribute(:nounwind)` or
`EnumAttribute(:align, 16)`. Only kinds that take an integer, like `align`, can have a
value. Attributes that carry a type, like `sret`, are created using [`TypeAttribute`](@ref)
instead. The kind can also be passed as a `String`.
"""
function EnumAttribute(kind::Union{Symbol,String}, value::Integer)
    enum_kind = checked_attribute_kind_id(kind, :enum, :int)
    if attribute_kind_category(enum_kind) == :int
        # before LLVM 16, a zero value selects the representation of a valueless attribute
        version() < v"16" && value == 0 &&
            throw(ArgumentError("Attribute kind $kind requires a value"))
    else
        value == 0 || throw(ArgumentError("Attribute kind $kind does not take a value"))
    end
    return EnumAttribute(API.LLVMCreateEnumAttribute(context(), enum_kind, UInt64(value)))
end

EnumAttribute(kind::Union{Symbol,String}) = EnumAttribute(kind, 0)

kind(attr::EnumAttribute) = attribute_kind_name(attribute_kind_id(attr))

value(attr::EnumAttribute) = API.LLVMGetEnumAttributeValue(attr)


## string attribute

"""
    StringAttribute(kind::String, value::String="")

Create a string attribute, identified by an arbitrary name, and optionally carrying a
string value. These are used for target-specific attributes like `"target-cpu"`, or for
information that a compiler wants to attach to IR.
"""
StringAttribute(kind::AbstractString, value::AbstractString="") =
    StringAttribute(API.LLVMCreateStringAttribute(context(), kind, ncodeunits(kind),
                                                  value, ncodeunits(value)))

function kind(attr::StringAttribute)
    len = Ref{Cuint}()
    data = API.LLVMGetStringAttributeKind(attr, len)
    return unsafe_string(convert(Ptr{Int8}, data), len[])
end

function value(attr::StringAttribute)
    len = Ref{Cuint}()
    data = API.LLVMGetStringAttributeValue(attr, len)
    return unsafe_string(convert(Ptr{Int8}, data), len[])
end

## type attribute

"""
    TypeAttribute(kind::Symbol, value::LLVMType)

Create an attribute of one of LLVM's attribute kinds that carries a type, e.g.,
`TypeAttribute(:sret, T)` or `TypeAttribute(:byval, T)`. The kind can also be passed as a
`String`.
"""
function TypeAttribute(kind::Union{Symbol,String}, value::LLVMType)
    enum_kind = checked_attribute_kind_id(kind, :type)
    return TypeAttribute(API.LLVMCreateTypeAttribute(context(), enum_kind, value))
end

kind(attr::TypeAttribute) = attribute_kind_name(attribute_kind_id(attr))

function value(attr::TypeAttribute)
    return LLVMType(API.LLVMGetTypeAttributeValue(attr))
end

## constant range attribute

if version() >= v"19"
    function ConstantRangeAttribute(kind::Union{Symbol,String}, nbits::Integer,
                                    lower::Vector{UInt64}, upper::Vector{UInt64})
        enum_kind = checked_attribute_kind_id(kind, :range)
        # LLVM reads the words of both bounds, cld(nbits, 64) each (ignoring bits beyond
        # nbits), and asserts that the range is valid and not the full range
        0 < nbits <= typemax(Cuint) ||
            throw(ArgumentError("Invalid number of bits for a range: $nbits"))
        nwords = cld(nbits, 64)
        length(lower) == length(upper) == nwords ||
            throw(ArgumentError("The bounds of a $nbits-bit range need $nwords words each, got $(length(lower)) and $(length(upper))"))
        bound(words) = foldl((x, (i, w)) -> x | big(w) << (64*(i-1)), enumerate(words);
                             init=big(0)) & (big(1) << nbits - 1)
        lo, hi = bound(lower), bound(upper)
        if lo == hi
            lo == 0 || lo == big(1) << nbits - 1 ||
                throw(ArgumentError("The bounds of a range can only be equal to denote the empty range (0, 0)"))
            lo == 0 || throw(ArgumentError("A range attribute cannot be the full range"))
        end
        return ConstantRangeAttribute(
            API.LLVMCreateConstantRangeAttribute(context(), enum_kind, Cuint(nbits),
                                                 lower, upper))
    end
end

kind(attr::ConstantRangeAttribute) = attribute_kind_name(attribute_kind_id(attr))

## constant range list attribute

kind(attr::ConstantRangeListAttribute) = attribute_kind_name(attribute_kind_id(attr))



## attribute sets

# the attributes of a function or call site, at a specific index (the function itself, its
# return value, or one of its parameters). subtypes implement `attribute_ref(set, kind)`,
# returning the attribute of the given kind (an attribute kind ID, or the name of a string
# attribute) or `C_NULL`, and `remove_attribute!(set, kind)`.
abstract type AttributeSet end

Base.eltype(::Type{<:AttributeSet}) = Attribute

# LLVM only supports fetching all attributes at once
function Base.iterate(iter::AttributeSet, (attrs, i)=(collect(iter), 1))
    i > length(attrs) ? nothing : (attrs[i], (attrs, i+1))
end

function Base.append!(iter::AttributeSet, attrs)
    for attr in attrs
        push!(iter, attr)
    end
    return iter
end

function Base.show(io::IO, iter::AttributeSet)
    print(io, nameof(typeof(iter)), "(")
    join(io, collect(iter), ", ")
    print(io, ")")
end

# look up attributes by their kind: a `Symbol` for LLVM's attribute kinds, and a string for
# string attributes. unknown kinds cannot be present.
function attribute_ref(iter::AttributeSet, kind::Symbol)
    id = attribute_kind_id(kind)
    id == 0 ? API.LLVMAttributeRef(C_NULL) : attribute_ref(iter, id)
end

Base.haskey(iter::AttributeSet, kind::Union{Symbol,AbstractString}) =
    attribute_ref(iter, kind) != C_NULL

function Base.get(iter::AttributeSet, kind::Union{Symbol,AbstractString}, default)
    ref = attribute_ref(iter, kind)
    ref == C_NULL ? default : Attribute(ref)
end

function Base.getindex(iter::AttributeSet, kind::Union{Symbol,AbstractString})
    ref = attribute_ref(iter, kind)
    ref == C_NULL && throw(KeyError(kind))
    return Attribute(ref)
end

function Base.delete!(iter::AttributeSet, kind::Symbol)
    id = attribute_kind_id(kind)
    id == 0 || remove_attribute!(iter, id)
    return iter
end

function Base.delete!(iter::AttributeSet, kind::AbstractString)
    remove_attribute!(iter, kind)
    return iter
end

function Base.delete!(iter::AttributeSet,
                      attr::Union{EnumAttribute,TypeAttribute,ConstantRangeAttribute,
                                  ConstantRangeListAttribute})
    remove_attribute!(iter, attribute_kind_id(attr))
    return iter
end

Base.delete!(iter::AttributeSet, attr::StringAttribute) = delete!(iter, kind(attr))


## memory effects

@vocabulary IR MemoryEffects

"""
    MemoryEffects(default::Symbol=:none; argmem, inaccessiblemem, errnomem, other, ...)

The memory effects of a function or call, i.e., the kind of access that may happen to each
location of memory. These effects are encoded in the `memory` attribute, which since LLVM 16
replaces the `readnone`, `readonly`, `writeonly`, `argmemonly`, `inaccessiblememonly` and
`inaccessiblemem_or_argmemonly` function attributes. Use `EnumAttribute(effects)` to create
that attribute and `MemoryEffects(attrs)` to decode it from a set of function or call
attributes, or the [`memory_effects`](@ref LLVM.Function) property of a function to
get and set it directly. That property returns a [`FunctionMemoryEffects`](@ref) view,
which `MemoryEffects(effects)` converts to a value.

The access kind of every location is one of `:none`, `:read`, `:write` or `:readwrite`, and
defaults to `default`. The locations are:

- `argmem`: memory accessed through pointer arguments;
- `inaccessiblemem`: memory that is not accessible by the current module;
- `errnomem`: the `errno` variable (LLVM 21 and later, before that part of `other`);
- `target_mem0`, `target_mem1`: target-specific state (LLVM 22 and later, experimental);
- `other`: any other memory.

These effects are an upper bound: an access kind like `:read` does not guarantee that a
read happens. The access kind of a single location can be queried by indexing, e.g.,
`effects[:argmem]`, while the [`access`](@ref LLVM.MemoryEffects) property returns the access kind
for all locations combined. Effects can be combined with `|` (union) and `&`
(intersection).

# Examples

```julia
MemoryEffects(:none)                    # memory(none), formerly `readnone`
MemoryEffects(:read)                    # memory(read), formerly `readonly`
MemoryEffects(argmem=:readwrite)        # memory(argmem: readwrite), formerly `argmemonly`
MemoryEffects(:read; argmem=:readwrite) # memory(read, argmem: readwrite)
```

!!! note
    The `memory` attribute requires LLVM 16 or later.

# Properties

    effects.access

The kind of memory access that is possible for any location: `:none` if no memory may be
accessed, `:read` if it may only be read, `:write` if it may only be written, and
`:readwrite` otherwise.
"""
struct MemoryEffects
    # LLVM's encoding (`MemoryEffects::toIntValue`): two bits per location, holding the
    # `ModRefInfo` in the position of the location in `memory_locations()`
    data::UInt32
    MemoryEffects(data::UInt32) = new(data)
end
@properties MemoryEffects

MemoryEffects(effects::MemoryEffects) = effects

# The memory locations of the LLVM version in use, in the order of LLVM's `IRMemLocation`.
# This needs to be kept in sync with `llvm/Support/ModRef.h` when adding a new LLVM version.
function memory_locations()
    if version() >= v"22"
        (:argmem, :inaccessiblemem, :errnomem, :other, :target_mem0, :target_mem1)
    elseif version() >= v"21"
        (:argmem, :inaccessiblemem, :errnomem, :other)
    elseif version() >= v"16"
        (:argmem, :inaccessiblemem, :other)
    else
        throw(ArgumentError("The memory attribute requires LLVM 16 or later"))
    end
end
const all_memory_locations =
    (:argmem, :inaccessiblemem, :errnomem, :other, :target_mem0, :target_mem1)

# The access kinds, in the order of LLVM's `ModRefInfo`.
const memory_access_kinds = (:none, :read, :write, :readwrite)

function memory_location_pos(loc::Symbol)
    pos = findfirst(==(loc), memory_locations())
    if pos === nothing
        if loc in all_memory_locations
            throw(ArgumentError("Memory location $(repr(loc)) is not supported by LLVM $(version())"))
        else
            throw(ArgumentError("Unknown memory location $(repr(loc)); expected one of $(join(map(repr, memory_locations()), ", "))"))
        end
    end
    return 2 * (pos - 1)
end

function memory_access_value(kind::Symbol)
    val = findfirst(==(kind), memory_access_kinds)
    val === nothing &&
        throw(ArgumentError("Unknown memory access kind $(repr(kind)); expected one of $(join(map(repr, memory_access_kinds), ", "))"))
    return UInt32(val - 1)
end

function MemoryEffects(default::Symbol=:none; kwargs...)
    # reject invalid arguments, even if they aren't used
    memory_access_value(default)
    for loc in keys(kwargs)
        memory_location_pos(loc)
    end
    data = UInt32(0)
    for loc in memory_locations()
        data |= memory_access_value(get(kwargs, loc, default)) << memory_location_pos(loc)
    end
    return MemoryEffects(data)
end

function Base.getindex(effects::MemoryEffects, loc::Symbol)
    memory_access_kinds[((effects.data >> memory_location_pos(loc)) & 0x3) + 1]
end

function access(effects::MemoryEffects)
    val = UInt32(0)
    for loc in memory_locations()
        val |= (effects.data >> memory_location_pos(loc)) & 0x3
    end
    return memory_access_kinds[val + 1]
end

@property MemoryEffects access

Base.:(|)(a::MemoryEffects, b::MemoryEffects) = MemoryEffects(a.data | b.data)
Base.:(&)(a::MemoryEffects, b::MemoryEffects) = MemoryEffects(a.data & b.data)

# print like LLVM does, with the access kind for `other` as the default
function Base.show(io::IO, effects::MemoryEffects)
    default = effects[:other]
    print(io, "MemoryEffects(")
    show_default = default != :none || access(effects) == default
    show_default && show(io, default)
    first = true
    for loc in memory_locations()
        kind = effects[loc]
        (loc == :other || kind == default) && continue
        print(io, first ? (show_default ? "; " : "") : ", ", loc, "=")
        show(io, kind)
        first = false
    end
    print(io, ")")
end

memory_kind() = API.LLVMGetEnumAttributeKindForName("memory", 6)

"""
    EnumAttribute(effects::MemoryEffects)

Create a `memory` attribute describing the given memory effects.
"""
function EnumAttribute(effects::MemoryEffects)
    memory_locations()  # check that the attribute is supported
    return EnumAttribute(API.LLVMCreateEnumAttribute(context(), memory_kind(), effects.data))
end

"""
    MemoryEffects(attr::EnumAttribute)

Get the memory effects described by a `memory` attribute.
"""
function MemoryEffects(attr::EnumAttribute)
    memory_locations()  # check that the attribute is supported
    attribute_kind_id(attr) == memory_kind() ||
        throw(ArgumentError("Expected a memory attribute, got $attr"))
    return MemoryEffects(UInt32(value(attr)))
end


## properties

@property Attribute kind
@property Union{EnumAttribute,StringAttribute,TypeAttribute} value
