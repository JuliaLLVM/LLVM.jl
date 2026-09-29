# Attributes that can be associated with parameters, function results, or the function
# itself.

@vocabulary IR Attribute,
               EnumAttribute, StringAttribute, TypeAttribute,
               ConstantRangeAttribute, ConstantRangeListAttribute

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

function Base.show(io::IO, attr::T) where T<:Attribute
    print(io, "$T $(kind(attr))=$(value(attr))")
end


## enum attribute

# NOTE: the AttrKind enum is not exported in the C API,
#       so we don't expose a way to construct EnumAttribute from its raw enum value
#       (which also would conflict with the inner ref constructor)
function EnumAttribute(kind::String, value::Integer=0)
    enum_kind = API.LLVMGetEnumAttributeKindForName(kind, Csize_t(length(kind)))
    return EnumAttribute(API.LLVMCreateEnumAttribute(context(), enum_kind, UInt64(value)))
end

kind(attr::EnumAttribute) = API.LLVMGetEnumAttributeKind(attr)

value(attr::EnumAttribute) = API.LLVMGetEnumAttributeValue(attr)


## string attribute

StringAttribute(kind::String, value::String="") =
    StringAttribute(API.LLVMCreateStringAttribute(context(), kind, length(kind),
                                                  value, length(value)))

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

function TypeAttribute(kind::String, value::LLVMType)
    enum_kind = API.LLVMGetEnumAttributeKindForName(kind, Csize_t(length(kind)))
    return TypeAttribute(API.LLVMCreateTypeAttribute(context(), enum_kind, value))
end

kind(attr::TypeAttribute) = API.LLVMGetEnumAttributeKind(attr)

function value(attr::TypeAttribute)
    return LLVMType(API.LLVMGetTypeAttributeValue(attr))
end

## constant range attribute

if version() >= v"19"
    function ConstantRangeAttribute(kind::String, nbits::Integer,
                                    lower::Vector{UInt64}, upper::Vector{UInt64})
        enum_kind = API.LLVMGetEnumAttributeKindForName(kind, Csize_t(length(kind)))
        return ConstantRangeAttribute(
            API.LLVMCreateConstantRangeAttribute(context(), enum_kind, Cuint(nbits),
                                                 lower, upper))
    end
end

kind(attr::ConstantRangeAttribute) = API.LLVMGetEnumAttributeKind(attr)

## constant range list attribute

kind(attr::ConstantRangeListAttribute) = API.LLVMGetEnumAttributeKind(attr)


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
get and set it directly.

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
    kind(attr) == memory_kind() ||
        throw(ArgumentError("Expected a memory attribute, got $attr"))
    return MemoryEffects(UInt32(value(attr)))
end


## properties

@property Attribute kind
@property Union{EnumAttribute,StringAttribute,TypeAttribute} value
