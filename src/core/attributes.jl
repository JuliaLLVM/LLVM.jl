# Attributes that can be associated with parameters, function results, or the function
# itself.

export Attribute,
       EnumAttribute, StringAttribute, TypeAttribute,
       ConstantRangeAttribute, ConstantRangeListAttribute,
       kind, value

abstract type Attribute end

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
        elseif version() >= v"17"
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
