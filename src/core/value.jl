# The bulk of LLVM's object model consists of values, which comprise a very rich type
# hierarchy.

export Value

"""
    LLVM.Value

Abstract type representing an LLVM value.
"""
abstract type Value end

# subtypes are expected to have a 'ref::API.LLVMValueRef' field
Base.unsafe_convert(::Type{API.LLVMValueRef}, val::Value) = val.ref

const value_kinds = Vector{Type}(fill(Nothing, typemax(API.LLVMValueKind)+1))
function identify(::Type{Value}, ref::API.LLVMValueRef)
    kind = API.LLVMGetValueKind(ref)
    typ = @inbounds value_kinds[kind+1]
    typ === Nothing && error("Unknown value kind $kind")
    return typ
end
function register(T::Type{<:Value}, kind::API.LLVMValueKind)
    value_kinds[kind+1] = T
end

function refcheck(::Type{T}, ref::API.LLVMValueRef) where T<:Value
    ref==C_NULL && throw(UndefRefError())
    if typecheck_enabled
        T′ = identify(Value, ref)
        if T != T′
            error("invalid conversion of $T′ value reference to $T")
        end
    end
end

# Construct a concretely typed value object from an abstract value ref
function Value(ref::API.LLVMValueRef)
    ref == C_NULL && throw(UndefRefError())
    T = identify(Value, ref)
    return T(ref)
end


## general APIs

export value_type, name, name!, isconstant, isundef, ispoison, context

"""
    value_type(val::Value)

Get the type of the given value.
"""
value_type(val::Value) = LLVMType(API.LLVMTypeOf(val))

# defer size queries to the LLVM type (where we'll error)
Base.sizeof(val::Value) = sizeof(value_type(val))

"""
    name(val::Value)

Get the name of the given value.
"""
name(val::Value) = unsafe_string(API.LLVMGetValueName(val))

"""
    name!(val::Value, name::String)

Set the name of the given value.
"""
name!(val::Value, name::String) = API.LLVMSetValueName(val, name)

Base.string(val::Value) = unsafe_message(API.LLVMPrintValueToString(val))

# by default, only print the value type and its name or address
function Base.show(io::IO, val::Value)
    if !isempty(name(val))
        @printf(io, "%s(\"%s\")", typeof(val), name(val))
    else
        @printf(io, "%s(%p)", typeof(val), val.ref)
    end
end

# when more output is requested, render the value (which may print multiple lines)
function Base.show(io::IO, ::MIME"text/plain", val::Value)
    print(io, strip(string(val)))
end

"""
    isconstant(val::LLVM.Value)

Check if the given value is a constant value.
"""
isconstant(val::Value) = API.LLVMIsConstant(val) |> Bool

"""
    isundef(val::LLVM.Value)

Check if the given value is an undef value.
"""
isundef(val::Value) = API.LLVMIsUndef(val) |> Bool

"""
    ispoison(val::LLVM.Value)

Check if the given value is a poison value.
"""
ispoison(val::Value) = API.LLVMIsPoison(val) |> Bool

"""
    context(val::LLVM.Value)

Return the context in which the given value was created.
"""
context(val::Value) = Context(API.LLVMGetValueContext(val))


## user values

include("value/user.jl")


## constants

include("value/constant.jl")


## usage

export replace_uses!, replace_metadata_uses!, Use, user, value

"""
    replace_uses!(old::LLVM.Value, new::LLVM.Value)

Replace all uses of an `old` value in the IR with `new`.

This does not replace uses in metadata, which must be done separately with
[`replace_metadata_uses!`](@ref).
"""
replace_uses!(old::Value, new::Value) = API.LLVMReplaceAllUsesWith(old, new)

"""
    replace_metadata_uses!(old::LLVM.Value, new::LLVM.Value)

Replace all uses of an `old` value in metadata with `new`.
"""
function replace_metadata_uses!(old::Value, new::Value)
    if value_type(old) == value_type(new)
        API.LLVMReplaceAllMetadataUsesWith(old, new)
    else
        # NOTE: LLVM does not support replacing values of different types, either using
        #       regular RAUW or only on metadata. The latter should probably be supported.
        #       Instead, we replace by a bitcast to the old type.
        compat_new = const_bitcast(new, value_type(old))
        replace_metadata_uses!(old, compat_new)

        # the above is often invalid, e.g. for module-level metadata identifying functions.
        # so we peek into such metadata and try to get rid of the bitcast. see also
        # https://discourse.llvm.org/t/replacing-module-metadata-uses-of-function/62431/4
        mod = LLVM.parent(new)
        while !isa(mod, LLVM.Module)
            mod = LLVM.parent(new)
        end
        function recurse(md)
            for (i, op) in enumerate(operands(md))
                if op isa ValueAsMetadata && Value(op) == compat_new
                    LLVM.replace_operand(md, i, Metadata(new))
                elseif isa(op, MDTuple)
                    recurse(op)
                end
            end
        end
        for (key, md) in metadata(mod)
            recurse(md)
        end
    end
end

"""
    LLVM.Use

A use of a value in the IR. Knows both the user and the used value.

See also: [`user`](@ref), [`value`](@ref).
"""
@checked struct Use
    ref::API.LLVMUseRef
end

Base.unsafe_convert(::Type{API.LLVMUseRef}, use::Use) = use.ref

"""
    user(use::LLVM.Use)

Get the user of the given use.
"""
user(use::Use) =  Value(API.LLVMGetUser(     use))

"""
    value(use::LLVM.Use)

Get the used value of the given use.
"""
value(use::Use) = Value(API.LLVMGetUsedValue(use))

# use iteration

export uses

struct ValueUseSet
    val::Value
end

"""
    uses(val::LLVM.Value)

Get an iterator over the uses of the given value.

See also: [`LLVM.Use`](@ref).
"""
uses(val::Value) = ValueUseSet(val)

Base.eltype(::ValueUseSet) = Use

function Base.iterate(iter::ValueUseSet, state=first_use(iter.val))
    state == C_NULL ? nothing : (Use(state), API.LLVMGetNextUse(state))
end

first_use(val::Value) = API.LLVMGetFirstUse(val)
@static if version() >= v"21"
    # LLVM 21 removed the uselist from ConstantData values
    first_use(::ConstantData) = C_NULL
end

Base.IteratorSize(::ValueUseSet) = Base.SizeUnknown()
