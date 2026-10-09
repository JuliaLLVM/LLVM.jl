@vocabulary IR erase!

"""
    LLVM.Function

A function in the IR.

# Properties

    f.function_type

The function type of the function, as opposed to the `value_type` property, which is the
pointer type of the function constant.

    f.personality
    f.personality = persfn::Union{LLVM.Constant,Nothing}

The personality function of the function, or `nothing` if it has none. Assigning `nothing`
removes the personality function.

The personality is usually a `Function`, but can be any constant referring to one, such as
a `GlobalAlias` or a constant expression (e.g., a bitcast when using typed pointers).

    f.callconv
    f.callconv = cc

The calling convention of the function, e.g., `LLVM.CallConv.Fast`.

    f.gc
    f.gc = name::AbstractString

The name of the garbage collector of the function, or an empty string if it has none.

    f.alignment
    f.alignment = bytes::Integer

The alignment of the code of the function in bytes, or 0 if it has no explicit alignment.
The assigned alignment must be a power of 2, or 0 to remove the explicit alignment.

    f.entry

The entry basic block of the function, or `nothing` if the function has no body.

    f.function_attributes

The attributes of the function itself, as a mutable view that can be iterated, and
supports `push!`, `append!` and `delete!`. Adding an attribute replaces any existing
attribute of the same kind. The view can also be indexed by the kind of an attribute: a
`Symbol` for LLVM's attribute kinds, and a string for string attributes, e.g.,
`haskey(f.function_attributes, :nounwind)`, `f.function_attributes["target-cpu"]` or
`delete!(f.function_attributes, :noinline)`.

See also the `return_attributes` and `parameter_attributes` properties.

    f.parameter_attributes

The attributes of the parameters of the function, as a vector with a view of the attributes
of each parameter. These views work like the `function_attributes` of the function, e.g.,
`push!(f.parameter_attributes[1], EnumAttribute("nocapture"))`.

    f.return_attributes

The attributes of the return value of the function, as a mutable view that works like the
`function_attributes` of the function.

    f.memory_effects
    f.memory_effects = effects::Union{MemoryEffects,FunctionMemoryEffects}

The memory effects of the function, as described by its `memory` attribute, or
`MemoryEffects(:readwrite)` if it doesn't have one. The effects are returned as a
[`FunctionMemoryEffects`](@ref) view, which can be used to change the access kind of a
single location, e.g., `f.memory_effects[:argmem] = :read`. Assigning adds a `memory`
attribute, replacing any existing one. On LLVM 15, this uses the attributes that the
`memory` attribute replaced, which can't represent all effects (see
[`MemoryEffects`](@ref)).

See also: [`MemoryEffects`](@ref)

    f.parameters

The parameters of the function, as a read-only view. These are `Argument` values that can
be used as inputs to other instructions.

    f.blocks

The basic blocks of the function, in order, as a read-only view that always reflects the
current body of the function. Create a `BasicBlock` to add one, and use operations like
`remove!` or `move!` to change the list of blocks. Indexing the view walks the list
of blocks, so iterate instead of indexing each block. While iterating over the view, it is
safe to remove or erase the block that was just returned, but not other blocks.

    f.subprogram
    f.subprogram = sp::Union{DISubprogram,Nothing}

The subprogram that describes the function, or `nothing` if it has none. Assigning
`nothing` removes it.

    f.intrinsic

The intrinsic that the function declares, or `nothing` if it isn't an intrinsic.

    f.next
    f.prev

The next or previous function in the module, or `nothing` if there is none.

The properties of [`GlobalObject`](@ref LLVM.GlobalObject), [`GlobalValue`](@ref
LLVM.GlobalValue), [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available
too.
"""
Function
# forward declaration of Function in src/core/basicblock.jl

register(Function, API.LLVMFunctionValueKind)

# not part of a vocabulary, as it would clash with `Base.Function`
@public Function

"""
    LLVM.Function(mod::Module, name::AbstractString, ft::FunctionType)

Create a new function in the given module with the given name and function type.
"""
Function(mod::Module, name::AbstractString, ft::FunctionType) =
    Function(API.LLVMAddFunction(mod, name, ft))

@vocabulary IR copy_attributes!

"""
    copy_attributes!(dest::LLVM.Function, src::LLVM.Function)
    copy_attributes!(dest::GlobalVariable, src::GlobalVariable)

Copy the attributes of `src` that are not needed to create it to `dest`, like C++'s
`copyAttributesFrom`, e.g., when replacing a function by one with a different signature.
This copies the visibility, DLL storage class, `unnamed_addr`, thread-local mode,
alignment and section, and for functions also the calling convention, garbage collector,
personality, and function, return and parameter attributes, and for global variables
whether they are externally initialized, their attributes and code model. Returns `dest`.

The name, linkage, body or initializer, and metadata are not copied. Parameter attributes
are copied by position, so if the parameters of `dest` differ from those of `src`, fix up
`dest.parameter_attributes` afterwards.
COMDAT is not copied. If the source has no personality, prefix or prologue data, those
fields on the destination are left as they were.
"""
function copy_attributes!(dest::Function, src::Function)
    API.LLVMExtraCopyAttributesFrom(dest, src)
    return dest
end
function copy_attributes!(dest::GlobalVariable, src::GlobalVariable)
    API.LLVMExtraCopyAttributesFrom(dest, src)
    return dest
end

function_type(Fn::Function) = FunctionType(API.LLVMGetFunctionType(Fn))

@property Function function_type

"""
    empty!(f::Function)

Delete the body of the given function, and convert the linkage to external.
"""
function Base.empty!(f::Function)
    API.LLVMFunctionDeleteBody(f)
    return f
end

"""
    erase!(f::Function)

Remove the given function from its parent module and free the object.

!!! warning

    This function is unsafe because it does not check if the function is used elsewhere.
"""
erase!(f::Function) = API.LLVMDeleteFunction(f)

function personality(f::Function)
    has_personality = API.LLVMHasPersonalityFn(f) |> Bool
    return has_personality ? Value(API.LLVMGetPersonalityFn(f)) : nothing
end

function personality!(f::Function, persfn::Union{Nothing,Constant})
    api = version() >= v"20" ? API.LLVMSetPersonalityFn : API.LLVMSetPersonalityFn2
    api(f, something(persfn, C_NULL))
end

@property Function personality personality!

callconv(f::Function) = API.LLVMGetFunctionCallConv(f)

callconv!(f::Function, cc) = API.LLVMSetFunctionCallConv(f, cc)

@property Function callconv callconv!

function gc(f::Function)
  ptr = API.LLVMGetGC(f)
  return ptr==C_NULL ? "" : unsafe_string(ptr)
end

gc!(f::Function, name::AbstractString) = API.LLVMSetGC(f, name)

@property Function gc gc!

alignment(f::Function) = API.LLVMGetAlignment(f)

function alignment!(f::Function, bytes::Integer)
    check_alignment(bytes; allow_zero=true)
    API.LLVMSetAlignment(f, bytes)
end

@property Function alignment alignment!

function entry(f::Function)
    ref = API.LLVMGetFirstBasicBlock(f)
    ref == C_NULL ? nothing : BasicBlock(ref)
end

@property Function entry


# attributes

struct FunctionAttrSet <: AttributeSet
    f::Function
    idx::API.LLVMAttributeIndex
end

function_attributes(f::Function) =
    FunctionAttrSet(f, reinterpret(API.LLVMAttributeIndex, API.LLVMAttributeFunctionIndex))

@property Function function_attributes

struct FunctionParameterAttrSets <: AbstractVector{FunctionAttrSet}
    f::Function
end

parameter_attributes(f::Function) = FunctionParameterAttrSets(f)

@property Function parameter_attributes

Base.size(iter::FunctionParameterAttrSets) = (Int(API.LLVMCountParams(iter.f)),)

Base.IndexStyle(::Type{FunctionParameterAttrSets}) = IndexLinear()

function Base.getindex(iter::FunctionParameterAttrSets, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return FunctionAttrSet(iter.f, API.LLVMAttributeIndex(i))
end

return_attributes(f::Function) = FunctionAttrSet(f, API.LLVMAttributeReturnIndex)

@property Function return_attributes

function Base.collect(iter::FunctionAttrSet)
    elems = Vector{API.LLVMAttributeRef}(undef, length(iter))
    if length(iter) > 0
      # FIXME: this prevents a nullptr ref in LLVM similar to D26392
      API.LLVMGetAttributesAtIndex(iter.f, iter.idx, elems)
    end
    return Attribute[Attribute(elem) for elem in elems]
end

function Base.push!(iter::FunctionAttrSet, attr::Attribute)
    API.LLVMAddAttributeAtIndex(iter.f, iter.idx, attr)
    return iter
end

function Base.length(iter::FunctionAttrSet)
    API.LLVMGetAttributeCountAtIndex(iter.f, iter.idx)
end

attribute_ref(iter::FunctionAttrSet, id::Integer) =
    API.LLVMGetEnumAttributeAtIndex(iter.f, iter.idx, id)
function attribute_ref(iter::FunctionAttrSet, kind::AbstractString)
    kind = String(kind)
    API.LLVMGetStringAttributeAtIndex(iter.f, iter.idx, kind, ncodeunits(kind))
end

remove_attribute!(iter::FunctionAttrSet, id::Integer) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, id)
function remove_attribute!(iter::FunctionAttrSet, kind::AbstractString)
    kind = String(kind)
    API.LLVMRemoveStringAttributeAtIndex(iter.f, iter.idx, kind, ncodeunits(kind))
end

"""
    MemoryEffects(attrs)

Get the memory effects described by the `memory` attribute in the given function
attributes, or `MemoryEffects(:readwrite)` if there is no such attribute. `attrs` can be
the attributes of a function, `f.function_attributes`, or of a call,
`call.function_attributes`. In the latter case, only the attributes of the call site are
considered, and not, e.g., those of the called function. To set the memory effects, push
the corresponding attribute: `push!(attrs, EnumAttribute(effects))`, which replaces any
existing one, or assign the `memory_effects` property of the function or call.

On LLVM 15, which doesn't have the `memory` attribute, this combines the effects of the
attributes that it replaced (`readnone`, `readonly`, `argmemonly`, ...).

See also the `memory_effects` property of functions and calls.
"""
function MemoryEffects(iter::FunctionAttrSet)
    check_memory_effects_index(iter.idx)
    version() >= v"16" || return legacy_memory_effects_of(iter)
    ref = API.LLVMGetEnumAttributeAtIndex(iter.f, iter.idx, memory_kind())
    ref == C_NULL && return MemoryEffects(:readwrite)
    return MemoryEffects(EnumAttribute(ref))
end

@vocabulary IR FunctionMemoryEffects

"""
    FunctionMemoryEffects

The memory effects of a function or a call, as returned by their `memory_effects` property.
This is a view of the `memory` attribute in their function attributes, which supports the
same operations as a [`MemoryEffects`](@ref) value: indexing (`effects[:argmem]`), the
`access` property, `|` and `&`, and comparing to other effects. In addition, the access
kind of a single location can be changed in place, which replaces the `memory` attribute:

```julia
f.memory_effects[:argmem] = :read
```

Use `MemoryEffects(effects)` to get the current effects as a value, which doesn't change
along with the function or call.
"""
struct FunctionMemoryEffects{T<:AttributeSet}
    attrs::T
end
@properties FunctionMemoryEffects

MemoryEffects(effects::FunctionMemoryEffects) = MemoryEffects(getfield(effects, :attrs))

Base.getindex(effects::FunctionMemoryEffects, loc::Symbol) = MemoryEffects(effects)[loc]

function Base.setindex!(effects::FunctionMemoryEffects, kind::Symbol, loc::Symbol)
    pos = memory_location_pos(loc)
    data = MemoryEffects(effects).data & ~(UInt32(0x3) << pos)
    data |= memory_access_value(kind) << pos
    memory_effects!(getfield(effects, :attrs), MemoryEffects(data))
    return effects
end

access(effects::FunctionMemoryEffects) = access(MemoryEffects(effects))
@property FunctionMemoryEffects access

EnumAttribute(effects::FunctionMemoryEffects) = EnumAttribute(MemoryEffects(effects))

# the view behaves like the effects it currently describes
const AnyMemoryEffects = Union{MemoryEffects, FunctionMemoryEffects}
Base.:(|)(a::AnyMemoryEffects, b::AnyMemoryEffects) = MemoryEffects(a) | MemoryEffects(b)
Base.:(&)(a::AnyMemoryEffects, b::AnyMemoryEffects) = MemoryEffects(a) & MemoryEffects(b)
Base.:(==)(a::AnyMemoryEffects, b::AnyMemoryEffects) = MemoryEffects(a) === MemoryEffects(b)
Base.hash(effects::FunctionMemoryEffects, h::UInt) = hash(MemoryEffects(effects), h)
Base.show(io::IO, effects::FunctionMemoryEffects) = show(io, MemoryEffects(effects))

@public memory_attributes

"""
    LLVM.memory_attributes(effects::Union{MemoryEffects,FunctionMemoryEffects})

Create the attributes that describe the memory `effects` of a function or call, on any
version of LLVM: the `memory` attribute on LLVM 16 and later, or the attributes that it
replaced on LLVM 15 (none for unrestricted effects). This is for building lists of
attributes, e.g., for a function declaration; on LLVM 15, it throws an `ArgumentError` for
effects that can't be represented (see [`MemoryEffects`](@ref)).

Adding these attributes to a function or call doesn't remove the ones that describe other
effects on LLVM 15. To replace the memory effects of a function or call, assign its
`memory_effects` property instead.
"""
function memory_attributes(effects::AnyMemoryEffects)
    effects = MemoryEffects(effects)
    if version() >= v"16"
        return Attribute[EnumAttribute(effects)]
    else
        return Attribute[legacy_memory_attributes(effects)...]
    end
end

memory_effects(f::Function) = FunctionMemoryEffects(function_attributes(f))

memory_effects!(f::Function, effects::AnyMemoryEffects) =
    memory_effects!(function_attributes(f), MemoryEffects(effects))

@property Function memory_effects memory_effects!

check_memory_effects_index(idx::API.LLVMAttributeIndex) =
    idx == reinterpret(API.LLVMAttributeIndex, API.LLVMAttributeFunctionIndex) ||
        throw(ArgumentError("Memory effects can only be associated with functions and calls, not with parameters or return values"))


# parameter iteration

@vocabulary IR Argument

"""
    LLVM.Argument

A parameter of a function, as a value that can be used in its body.

# Properties

    arg.parent

The function that the parameter belongs to.

    arg.index

The position of the parameter in the parameter list of its function, starting at 1, so
that `arg.parent.parameters[arg.index] == arg`.

    arg.next
    arg.prev

The next or previous parameter of the function, or `nothing` if there is none.

The properties of [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct Argument <: Value
    ref::API.LLVMValueRef
end
register(Argument, API.LLVMArgumentValueKind)

struct FunctionParameterSet <: AbstractVector{Argument}
    f::Function
end

parameters(f::Function) = FunctionParameterSet(f)

@property Function parameters

Base.size(iter::FunctionParameterSet) = (Int(API.LLVMCountParams(iter.f)),)

Base.IndexStyle(::Type{FunctionParameterSet}) = IndexLinear()

function Base.getindex(iter::FunctionParameterSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Argument(API.LLVMGetParam(iter.f, i-1))
end

@inline function Base.iterate(iter::FunctionParameterSet, state=API.LLVMGetFirstParam(iter.f))
    state == C_NULL ? nothing : (Argument(state), API.LLVMGetNextParam(state))
end

function Base.first(iter::FunctionParameterSet)
    ref = API.LLVMGetFirstParam(iter.f)
    ref == C_NULL && throw(BoundsError(iter))
    Argument(ref)
end

function Base.last(iter::FunctionParameterSet)
    ref = API.LLVMGetLastParam(iter.f)
    ref == C_NULL && throw(BoundsError(iter))
    Argument(ref)
end

# NOTE: optimized `collect`
function Base.collect(iter::FunctionParameterSet)
    elems = Vector{API.LLVMValueRef}(undef, length(iter))
    API.LLVMGetParams(iter.f, elems)
    return map(el->Argument(el), elems)
end

function next(arg::Argument)
    ref = API.LLVMGetNextParam(arg)
    ref == C_NULL ? nothing : Argument(ref)
end

function prev(arg::Argument)
    ref = API.LLVMGetPreviousParam(arg)
    ref == C_NULL ? nothing : Argument(ref)
end

@property Argument next
@property Argument prev

parent(arg::Argument) = Function(API.LLVMGetParamParent(arg))

@property Argument parent

index(arg::Argument) = Int(API.LLVMExtraGetArgNo(arg)) + 1

@property Argument index


# basic block iteration

struct FunctionBlockSet <: AbstractVector{BasicBlock}
    f::Function
end

blocks(f::Function) = FunctionBlockSet(f)

@property Function blocks

Base.size(iter::FunctionBlockSet) = (Int(API.LLVMCountBasicBlocks(iter.f)),)
Base.isempty(iter::FunctionBlockSet) = API.LLVMGetFirstBasicBlock(iter.f) == C_NULL

Base.IndexStyle(::Type{FunctionBlockSet}) = IndexLinear()

function Base.first(iter::FunctionBlockSet)
    ref = API.LLVMGetFirstBasicBlock(iter.f)
    ref == C_NULL && throw(BoundsError(iter))
    BasicBlock(ref)
end

function Base.last(iter::FunctionBlockSet)
    ref = API.LLVMGetLastBasicBlock(iter.f)
    ref == C_NULL && throw(BoundsError(iter))
    BasicBlock(ref)
end

@inline function Base.iterate(iter::FunctionBlockSet, state=API.LLVMGetFirstBasicBlock(iter.f))
    state == C_NULL ? nothing : (BasicBlock(state), API.LLVMGetNextBasicBlock(state))
end

function next(bb::BasicBlock)
    API.LLVMGetBasicBlockParent(bb) == C_NULL && return nothing
    ref = API.LLVMGetNextBasicBlock(bb)
    ref == C_NULL ? nothing : BasicBlock(ref)
end

function prev(bb::BasicBlock)
    API.LLVMGetBasicBlockParent(bb) == C_NULL && return nothing
    ref = API.LLVMGetPreviousBasicBlock(bb)
    ref == C_NULL ? nothing : BasicBlock(ref)
end

@property BasicBlock next
@property BasicBlock prev

# LLVM keeps blocks in a linked list. Caching them would make the view go stale.
function Base.getindex(iter::FunctionBlockSet, i::Int)
    @boundscheck i >= 1 || throw(BoundsError(iter, i))
    ref = API.LLVMGetFirstBasicBlock(iter.f)
    for _ in 2:i
        ref == C_NULL && break
        ref = API.LLVMGetNextBasicBlock(ref)
    end
    @boundscheck ref != C_NULL || throw(BoundsError(iter, i))
    return BasicBlock(ref)
end

# NOTE: optimized `collect`
function Base.collect(iter::FunctionBlockSet)
    elems = Vector{API.LLVMBasicBlockRef}(undef, length(iter))
    API.LLVMGetBasicBlocks(iter.f, elems)
    return BasicBlock[BasicBlock(elem) for elem in elems]
end


# intrinsics

@vocabulary IR isintrinsic, Intrinsic, isoverloaded
@public overloaded_name, overload_types

"""
    LLVM.Intrinsic
    Intrinsic(name::AbstractString)
    Intrinsic(f::LLVM.Function)

An LLVM intrinsic function, identified by its (base) name, e.g., `Intrinsic("llvm.memcpy")`,
or the intrinsic that a function declares. Throws an `ArgumentError` if there is no such
intrinsic; use [`tryparse`](@ref tryparse(::Type{LLVM.Intrinsic}, ::AbstractString)) to
look up a name that the version of LLVM in use may not know, and the `intrinsic` property
of functions to check whether a function is an intrinsic.

# Properties

    intr.name

The name of the intrinsic. For overloaded intrinsics, this is the base name, e.g.,
`llvm.sin`; see [`LLVM.overloaded_name`](@ref) to get the name of a specific overload.
"""
struct Intrinsic
    id::UInt32

    function Intrinsic(f::Function)
        id = API.LLVMGetIntrinsicID(f)
        id == 0 && throw(ArgumentError("Function is not an intrinsic"))
        new(id)
    end

    function Intrinsic(name::AbstractString)
        id = lookup_intrinsic_id(name)
        id == 0 && throw(ArgumentError("Unknown intrinsic: $name"))
        new(id)
    end

    # for IDs that are known to be valid
    Intrinsic(id::UInt32, ::Val{:unchecked}) = new(id)
end
@properties Intrinsic

function lookup_intrinsic_id(name::AbstractString)
    name = String(name)
    API.LLVMLookupIntrinsicID(name, ncodeunits(name))
end

"""
    tryparse(LLVM.Intrinsic, name::AbstractString)

Look up the intrinsic with the given name, like [`Intrinsic(name)`](@ref LLVM.Intrinsic),
but return `nothing` if the version of LLVM in use doesn't know it. The name can be the
base name of an overloaded intrinsic (e.g., `"llvm.sin"`) or the name of an overload
(e.g., `"llvm.sin.f64"`), as LLVM recognizes them.
"""
function Base.tryparse(::Type{Intrinsic}, name::AbstractString)
    id = lookup_intrinsic_id(name)
    return id == 0 ? nothing : Intrinsic(id, Val(:unchecked))
end

"""
    parse(LLVM.Intrinsic, name::AbstractString)

Look up the intrinsic with the given name, throwing an `ArgumentError` if the version of
LLVM in use doesn't know it. This is the same as [`Intrinsic(name)`](@ref LLVM.Intrinsic).
"""
Base.parse(::Type{Intrinsic}, name::AbstractString) = Intrinsic(name)

intrinsic(f::Function) = isintrinsic(f) ? Intrinsic(f) : nothing

@property Function intrinsic

"""
    isintrinsic(val::Value)
    isintrinsic(val::Value, intr::Intrinsic)

Check if the given value is a function that is an intrinsic, or a specific intrinsic. This
works with any value, e.g., to check the `called_operand` of a call instruction:

```julia
memcpy = Intrinsic("llvm.memcpy")
isintrinsic(call.called_operand, memcpy)
```

Intrinsics are identified by their ID, so unlike C++'s `Function::isIntrinsic`, which
checks for the `llvm.` prefix that is reserved for intrinsics, this is false for functions
that are named like intrinsics that the current version of LLVM does not know.
"""
isintrinsic(val::Value) = API.LLVMGetIntrinsicID(val) != 0
isintrinsic(val::Value, intr::Intrinsic) = API.LLVMGetIntrinsicID(val) == intr.id

Base.convert(::Type{UInt32}, intr::Intrinsic) = intr.id

function name(intr::Intrinsic)
    # LLVMIntrinsicGetName asserts that the intrinsic is not overloaded, but the name of an
    # overload without any types is the base name
    isoverloaded(intr) && return overloaded_name(intr, LLVMType[])
    len = Ref{Csize_t}()
    str = API.LLVMIntrinsicGetName(intr, len)
    unsafe_string(convert(Ptr{Cchar}, str), len[])
end

@property Intrinsic name

"""
    LLVM.overloaded_name(intr::LLVM.Intrinsic, params::AbstractVector{<:LLVMType})

Get the name of the given overloaded intrinsic with the given parameter types, e.g.,
`llvm.sin.f64`. Without types, this is the base name of the intrinsic, like `intr.name`.
Throws an `ArgumentError` if the intrinsic isn't overloaded.
"""
function overloaded_name(intr::Intrinsic, params::AbstractVector{<:LLVMType})
    isoverloaded(intr) ||
        throw(ArgumentError("Intrinsic $(name(intr)) is not overloaded"))
    len = Ref{Csize_t}()
    str = API.LLVMIntrinsicCopyOverloadedName(intr, as_vector(params), length(params), len)
    unsafe_message(convert(Ptr{Cchar}, str), len[])
end

"""
    isoverloaded(intr::Intrinsic)

Check if the given intrinsic is overloaded.
"""
function isoverloaded(intr::Intrinsic)
    API.LLVMIntrinsicIsOverloaded(intr) |> Bool
end

# LLVM uses the types of the overloaded parameters of an intrinsic without checking them,
# naming the declaration after any extra types, and crashing if types are missing. without
# the intrinsic's type table, only check whether there should be types at all.
function check_overloaded_types(intr::Intrinsic, params::AbstractVector{<:LLVMType})
    if isoverloaded(intr)
        isempty(params) &&
            throw(ArgumentError("Intrinsic $(name(intr)) is overloaded, so it requires the types of its overloaded parameters"))
    else
        isempty(params) ||
            throw(ArgumentError("Intrinsic $(name(intr)) is not overloaded, so it does not take parameter types"))
    end
end

"""
    Function(mod::Module, intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])

Get the declaration of the given intrinsic in the given module. For an overloaded
intrinsic, `params` are the types of its overloaded parameters, in the order of the name of
the overload: e.g., `[ptr, ptr, i64]` for `llvm.memcpy.p0.p0.i64`. Other intrinsics take no
types. Whether the intrinsic is overloaded can differ between versions of LLVM (e.g.,
`llvm.va_start` is overloaded since LLVM 19), which [`isoverloaded`](@ref) checks.

Throws an `ArgumentError` if types are given for an intrinsic that isn't overloaded, or none
for one that is. Other mismatches aren't detected: extra types end up in the name of the
declaration, and missing ones crash LLVM.
"""
function Function(mod::Module, intr::Intrinsic,
                  params::AbstractVector{<:LLVMType}=LLVMType[])
    check_overloaded_types(intr, params)
    Value(API.LLVMGetIntrinsicDeclaration(mod, intr, as_vector(params), length(params)))
end

"""
    LLVM.overload_types(intr::Intrinsic, ft::LLVM.FunctionType)

Get the types of the overloaded parameters of the given intrinsic for which it has the
given function type, e.g., `[double, i32]` for `llvm.powi` with type `double (double, i32)`,
in the order that [`LLVM.Function(mod, intr, params)`](@ref LLVM.Function(::LLVM.Module,
::Intrinsic, ::Vector{<:LLVMType})) and [`LLVM.overloaded_name`](@ref) expect them. For an
intrinsic that isn't overloaded, this is an empty vector. Returns `nothing` if `ft` is not
a valid signature of the intrinsic.

This is useful to declare an intrinsic from its base name and the type of a call to it,
which [`LLVM.Function(mod, intr, ft)`](@ref LLVM.Function(::LLVM.Module, ::Intrinsic,
::LLVM.FunctionType)) does.
"""
function overload_types(intr::Intrinsic, ft::FunctionType)
    count = Ref{Csize_t}(0)
    Bool(API.LLVMExtraIntrinsicGetOverloadTypes(intr, ft, C_NULL, count)) || return nothing
    refs = Vector{API.LLVMTypeRef}(undef, count[])
    if !isempty(refs)
        API.LLVMExtraIntrinsicGetOverloadTypes(intr, ft, refs, count)
    end
    return LLVMType[LLVMType(ref) for ref in refs]
end

"""
    Function(mod::Module, intr::Intrinsic, ft::FunctionType)

Get the declaration of the given intrinsic in the given module, with the given function
type. For an overloaded intrinsic, this determines the overload from the function type,
e.g., `llvm.powi.f64.i32` for `llvm.powi` with type `double (double, i32)`; see
[`LLVM.overload_types`](@ref). Throws an `ArgumentError` if `ft` is not a valid signature
of the intrinsic.
"""
function Function(mod::Module, intr::Intrinsic, ft::FunctionType)
    params = overload_types(intr, ft)
    params === nothing &&
        throw(ArgumentError("Invalid signature for intrinsic $(name(intr)): $(strip(string(ft)))"))
    Value(API.LLVMGetIntrinsicDeclaration(mod, intr, as_vector(params), length(params)))
end

"""
    FunctionType(intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])

Get the function type of the given intrinsic with the given types of its overloaded
parameters, like for [`LLVM.Function(mod, intr, params)`](@ref LLVM.Function(::LLVM.Module, ::Intrinsic,
::Vector{<:LLVMType})), which describes how they're checked.
"""
function FunctionType(intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])
    check_overloaded_types(intr, params)
    LLVMType(API.LLVMIntrinsicGetType(context(), intr, as_vector(params), length(params)))
end

# display intrinsics as the call that creates them, as their IDs differ between versions
Base.show(io::IO, intr::Intrinsic) = print(io, "Intrinsic(", repr(name(intr)), ")")
