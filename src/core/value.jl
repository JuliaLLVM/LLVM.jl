# The bulk of LLVM's object model consists of values, which comprise a very rich type
# hierarchy.

@vocabulary IR Value

"""
    LLVM.Value

Abstract type representing an LLVM value.

# Properties

    bb.name
    bb.name = name::String

The name of the basic block, like that of other values.

    val.value_type

The type of the value.

    val.name
    val.name = name::String

The name of the value, or an empty string if it is unnamed. When assigning a name that is
already in use in the same function or module, LLVM makes it unique by adding a suffix.

    val.context

The context in which the value was created.
"""
abstract type Value end
@properties Value

# subtypes must be immutable structs with a single `ref::API.LLVMValueRef` field
# (see `check_layout`)
@inline function Base.unsafe_convert(::Type{API.LLVMValueRef},
                                      @nospecialize(val::Value))
    typecheck_enabled && check_layout(typeof(val), API.LLVMValueRef)
    unsafe_load_ref(API.LLVMValueRef, val)
end

@inline propref(@nospecialize(x::Value)) = Base.unsafe_convert(API.LLVMValueRef, x)

# avoid specializing the conversions performed by `ccall` on the concrete wrapper type.
# wrappers consist of nothing but their reference, so there's nothing else to keep alive.
Base.cconvert(::Type{API.LLVMValueRef}, @nospecialize(obj::Value)) = obj
function Base.cconvert(::Type{Ptr{API.LLVMValueRef}},
                       @nospecialize(objs::Vector{<:Value}))
    R = API.LLVMValueRef
    R[Base.unsafe_convert(R, obj) for obj in objs]
end

const value_kinds = Vector{Type}(fill(Nothing, typemax(API.LLVMValueKind)+1))
function identify(::Type{Value}, ref::API.LLVMValueRef)
    kind = API.LLVMGetValueKind(ref)
    typ = @inbounds value_kinds[kind+1]
    typ === Nothing && error("Unknown value kind $kind")
    return typ
end
function register(T::Type{<:Value}, kind::API.LLVMValueKind)
    # instructions are identified further by their opcode
    T === Instruction || check_layout(T, API.LLVMValueRef)
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
    T === Instruction && return Instruction(ref)
    return unsafe_wrap_ref(T, ref)::Value
end


## general APIs

@vocabulary IR isconstant, isundef, ispoison, context

value_type(val::Value) = LLVMType(API.LLVMTypeOf(val))

# defer size queries to the LLVM type (where we'll error)
Base.sizeof(val::Value) = sizeof(value_type(val))

name(val::Value) = unsafe_string(API.LLVMGetValueName(val))

name!(val::Value, name::String) = API.LLVMSetValueName(val, name)

@property Value value_type
@property Value name name!

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

context(val::Value) = Context(API.LLVMGetValueContext(val))

@property Value context


## user values

include("value/user.jl")


## constants

include("value/constant.jl")


## usage

@vocabulary IR replace_uses!, replace_metadata_uses!, Use

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

A use of a value in the IR, with properties for both the `user` and the used `value`.

# Properties

    use.user

The user of the use, i.e., the value that has the used value as an operand.

    use.value

The used value of the use.
"""
@checked struct Use
    ref::API.LLVMUseRef
end
@properties Use

Base.unsafe_convert(::Type{API.LLVMUseRef}, use::Use) = use.ref

user(use::Use) =  Value(API.LLVMGetUser(     use))

value(use::Use) = Value(API.LLVMGetUsedValue(use))

@property Use user
@property Use value

# use iteration

@vocabulary IR uses

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

@inline function Base.iterate(iter::ValueUseSet, state=first_use(iter.val))
    state == C_NULL ? nothing : (Use(state), API.LLVMGetNextUse(state))
end

first_use(val::Value) = API.LLVMGetFirstUse(val)
@static if version() >= v"21"
    # LLVM 21 removed the uselist from ConstantData values
    first_use(::ConstantData) = C_NULL
end

Base.IteratorSize(::Type{ValueUseSet}) = Base.SizeUnknown()
