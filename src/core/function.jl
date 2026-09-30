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
    f.gc = name::String

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
attribute, replacing any existing one.

See also: [`MemoryEffects`](@ref)

    f.parameters

The parameters of the function, as a read-only view. These are `Argument` values that can
be used as inputs to other instructions.

    f.blocks

The basic blocks of the function, in order, as a read-only view that always reflects the
current body of the function. Create a `BasicBlock` to add one, and use operations like
`remove!` or `move_before!` to change the list of blocks. Indexing the view walks the list
of blocks, so iterate instead of indexing each block. While iterating over the view, it is
safe to remove or erase the block that was just returned, but not other blocks.

    f.subprogram
    f.subprogram = sp::DISubProgram

The subprogram that describes the function, or `nothing` if it has none.

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
    LLVM.Function(mod::Module, name::String, ft::FunctionType)

Create a new function in the given module with the given name and function type.
"""
Function(mod::Module, name::String, ft::FunctionType) =
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

"""
    move_before!(f::Function, pos::Function)

Move the function `f` before the function `pos` in the function list of the containing
module. Both functions must reside in the same module.
"""
move_before!(f::Function, pos::Function) = API.LLVMMoveFunctionBefore(f, pos)

"""
    move_after!(f::Function, pos::Function)

Move the function `f` after the function `pos` in the function list of the containing
module. Both functions must reside in the same module.
"""
move_after!(f::Function, pos::Function) = API.LLVMMoveFunctionAfter(f, pos)

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

gc!(f::Function, name::String) = API.LLVMSetGC(f, name)

@property Function gc gc!

alignment(f::Function) = API.LLVMGetAlignment(f)

function alignment!(f::Function, bytes::Integer)
    check_alignment(bytes; allow_zero=true)
    API.LLVMSetAlignment(f, bytes)
end

@property Function alignment alignment!

function entry(f::Function)
    # LLVM does not check whether the function has a body
    API.LLVMCountBasicBlocks(f) == 0 && return nothing
    BasicBlock(API.LLVMGetEntryBasicBlock(f))
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
attribute_ref(iter::FunctionAttrSet, kind::AbstractString) =
    API.LLVMGetStringAttributeAtIndex(iter.f, iter.idx, kind, ncodeunits(kind))

remove_attribute!(iter::FunctionAttrSet, id::Integer) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, id)
remove_attribute!(iter::FunctionAttrSet, kind::AbstractString) =
    API.LLVMRemoveStringAttributeAtIndex(iter.f, iter.idx, kind, ncodeunits(kind))

"""
    MemoryEffects(attrs)

Get the memory effects described by the `memory` attribute in the given function
attributes, or `MemoryEffects(:readwrite)` if there is no such attribute. `attrs` can be
the attributes of a function, `f.function_attributes`, or of a call,
`call.function_attributes`. In the latter case, only the attributes of the call site are
considered, and not, e.g., those of the called function. To set the memory effects, push
the corresponding attribute: `push!(attrs, EnumAttribute(effects))`, which replaces any
existing one.

See also the `memory_effects` property of functions.
"""
function MemoryEffects(iter::FunctionAttrSet)
    check_memory_effects_index(iter.idx)
    memory_locations()  # check that the attribute is supported
    ref = API.LLVMGetEnumAttributeAtIndex(iter.f, iter.idx, memory_kind())
    ref == C_NULL && return MemoryEffects(:readwrite)
    return MemoryEffects(EnumAttribute(ref))
end

@vocabulary IR FunctionMemoryEffects

"""
    FunctionMemoryEffects

The memory effects of a function, as returned by its `memory_effects` property. This is a
view of the function's `memory` attribute, which supports the same operations as a
[`MemoryEffects`](@ref) value: indexing (`effects[:argmem]`), the `access` property, `|`
and `&`, and comparing to other effects. In addition, the access kind of a single location
can be changed in place, which replaces the `memory` attribute of the function:

```julia
f.memory_effects[:argmem] = :read
```

Use `MemoryEffects(effects)` to get the current effects as a value, which doesn't change
along with the function.
"""
struct FunctionMemoryEffects
    f::Function
end
@properties FunctionMemoryEffects

MemoryEffects(effects::FunctionMemoryEffects) =
    MemoryEffects(function_attributes(getfield(effects, :f)))

Base.getindex(effects::FunctionMemoryEffects, loc::Symbol) = MemoryEffects(effects)[loc]

function Base.setindex!(effects::FunctionMemoryEffects, kind::Symbol, loc::Symbol)
    pos = memory_location_pos(loc)
    data = MemoryEffects(effects).data & ~(UInt32(0x3) << pos)
    data |= memory_access_value(kind) << pos
    memory_effects!(getfield(effects, :f), MemoryEffects(data))
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

memory_effects(f::Function) = FunctionMemoryEffects(f)

function memory_effects!(f::Function, effects::AnyMemoryEffects)
    push!(function_attributes(f), EnumAttribute(MemoryEffects(effects)))
    return
end

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

# LLVM keeps blocks in a linked list, so random access walks the list (from whichever end
# is closest). caching the blocks would make the view go stale when blocks are added or
# removed.
function Base.getindex(iter::FunctionBlockSet, i::Int)
    n = length(iter)
    @boundscheck 1 <= i <= n || throw(BoundsError(iter, i))
    if i <= n ÷ 2
        ref = API.LLVMGetFirstBasicBlock(iter.f)
        for _ in 2:i
            ref = API.LLVMGetNextBasicBlock(ref)
        end
    else
        ref = API.LLVMGetLastBasicBlock(iter.f)
        for _ in i+1:n
            ref = API.LLVMGetPreviousBasicBlock(ref)
        end
    end
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
@public overloaded_name

"""
    LLVM.Intrinsic
    Intrinsic(name::String)
    Intrinsic(f::LLVM.Function)

An LLVM intrinsic function, identified by its (base) name, e.g., `Intrinsic("llvm.memcpy")`,
or the intrinsic that a function declares. Throws an `ArgumentError` if there is no such
intrinsic; see the `intrinsic` property of functions for a non-throwing alternative.

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

    function Intrinsic(name::String)
        id = API.LLVMLookupIntrinsicID(name, ncodeunits(name))
        id == 0 && throw(ArgumentError("Unknown intrinsic: $name"))
        new(id)
    end
end
@properties Intrinsic

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
`llvm.sin.f64`.
"""
function overloaded_name(intr::Intrinsic, params::AbstractVector{<:LLVMType})
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

"""
    Function(mod::Module, intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])

Get the declaration of the given intrinsic in the given module.
"""
function Function(mod::Module, intr::Intrinsic,
                  params::AbstractVector{<:LLVMType}=LLVMType[])
    Value(API.LLVMGetIntrinsicDeclaration(mod, intr, as_vector(params), length(params)))
end

"""
    FunctionType(intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])

Get the function type of the given intrinsic with the given parameter types.
"""
function FunctionType(intr::Intrinsic, params::AbstractVector{<:LLVMType}=LLVMType[])
    LLVMType(API.LLVMIntrinsicGetType(context(), intr, as_vector(params), length(params)))
end

# display intrinsics as the call that creates them, as their IDs differ between versions
Base.show(io::IO, intr::Intrinsic) = print(io, "Intrinsic(", repr(name(intr)), ")")
