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

The calling convention of the function, e.g., `LLVM.API.LLVMFastCallConv`.

    f.gc
    f.gc = name::String

The name of the garbage collector of the function, or an empty string if it has none.

    f.alignment
    f.alignment = bytes::Integer

The alignment of the code of the function in bytes, or 0 if it has no explicit alignment.
The assigned alignment must be a power of 2, or 0 to remove the explicit alignment.

    f.entry

The entry basic block of the function, or `nothing` if the function has no body.

    f.memory_effects
    f.memory_effects = effects::Union{MemoryEffects,FunctionMemoryEffects}

The memory effects of the function, as described by its `memory` attribute, or
`MemoryEffects(:readwrite)` if it doesn't have one. The effects are returned as a
[`FunctionMemoryEffects`](@ref) view, which can be used to change the access kind of a
single location, e.g., `f.memory_effects[:argmem] = :read`. Assigning adds a `memory`
attribute, replacing any existing one.

See also: [`MemoryEffects`](@ref)

    f.subprogram
    f.subprogram = sp::DISubProgram

The subprogram that describes the function, or `nothing` if it has none.

The properties of [`GlobalValue`](@ref LLVM.GlobalValue) and [`Value`](@ref LLVM.Value) are
available too.
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

function_type(Fn::Function) = FunctionType(API.LLVMGetFunctionType(Fn))

@property Function function_type

"""
    empty!(f::Function)

Delete the body of the given function, and convert the linkage to external.
"""
Base.empty!(f::Function) = API.LLVMFunctionDeleteBody(f)

"""
    erase!(f::Function)

Remove the given function from its parent module and free the object.

!!! warning

    This function is unsafe because it does not check if the function is used elsewhere.
"""
erase!(f::Function) = API.LLVMDeleteFunction(f)

"""
    move_before(f::Function, pos::Function)

Move the function `f` before the function `pos` in the function list of the containing
module. Both functions must reside in the same module.
"""
move_before(f::Function, pos::Function) = API.LLVMMoveFunctionBefore(f, pos)

"""
    move_after(f::Function, pos::Function)

Move the function `f` after the function `pos` in the function list of the containing
module. Both functions must reside in the same module.
"""
move_after(f::Function, pos::Function) = API.LLVMMoveFunctionAfter(f, pos)

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

@vocabulary IR function_attributes, parameter_attributes, return_attributes

struct FunctionAttrSet
    f::Function
    idx::API.LLVMAttributeIndex
end

"""
    function_attributes(f::Function)

Get the attributes of the given function.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
function_attributes(f::Function) =
    FunctionAttrSet(f, reinterpret(API.LLVMAttributeIndex, API.LLVMAttributeFunctionIndex))

"""
    parameter_attributes(f::Function, idx::Integer)

Get the attributes of the given parameter of the given function.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
parameter_attributes(f::Function, idx::Integer) =
    FunctionAttrSet(f, API.LLVMAttributeIndex(idx))

"""
    return_attributes(f::Function)

Get the attributes of the return value of the given function.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
return_attributes(f::Function) = FunctionAttrSet(f, API.LLVMAttributeReturnIndex)

Base.eltype(::FunctionAttrSet) = Attribute

function Base.collect(iter::FunctionAttrSet)
    elems = Vector{API.LLVMAttributeRef}(undef, length(iter))
    if length(iter) > 0
      # FIXME: this prevents a nullptr ref in LLVM similar to D26392
      API.LLVMGetAttributesAtIndex(iter.f, iter.idx, elems)
    end
    return Attribute[Attribute(elem) for elem in elems]
end

Base.push!(iter::FunctionAttrSet, attr::Attribute) =
    API.LLVMAddAttributeAtIndex(iter.f, iter.idx, attr)

Base.delete!(iter::FunctionAttrSet, attr::EnumAttribute) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, kind(attr))

Base.delete!(iter::FunctionAttrSet, attr::TypeAttribute) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, kind(attr))

Base.delete!(iter::FunctionAttrSet, attr::ConstantRangeAttribute) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, kind(attr))

Base.delete!(iter::FunctionAttrSet, attr::ConstantRangeListAttribute) =
    API.LLVMRemoveEnumAttributeAtIndex(iter.f, iter.idx, kind(attr))

function Base.delete!(iter::FunctionAttrSet, attr::StringAttribute)
    k = kind(attr)
    API.LLVMRemoveStringAttributeAtIndex(iter.f, iter.idx, k, length(k))
end

function Base.length(iter::FunctionAttrSet)
    API.LLVMGetAttributeCountAtIndex(iter.f, iter.idx)
end

"""
    MemoryEffects(attrs)

Get the memory effects described by the `memory` attribute in the given function
attributes, or `MemoryEffects(:readwrite)` if there is no such attribute. `attrs` can be
the attributes of a function, `function_attributes(f)`, or of a call,
`function_attributes(call)`. In the latter case, only the attributes of the call site are
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

@vocabulary IR Argument, parameters

"""
    LLVM.Argument

A parameter of a function, as a value that can be used in its body.

# Properties

    arg.parent

The function that the parameter belongs to.

The properties of [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct Argument <: Value
    ref::API.LLVMValueRef
end
register(Argument, API.LLVMArgumentValueKind)

struct FunctionParameterSet <: AbstractVector{Argument}
    f::Function
end

"""
    parameters(f::Function)

Get an iterator over the parameters of the given function. These are values that can be
used as inputs to other instructions.
"""
parameters(f::Function) = FunctionParameterSet(f)

Base.size(iter::FunctionParameterSet) = (API.LLVMCountParams(iter.f),)

Base.IndexStyle(::FunctionParameterSet) = IndexLinear()

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

parent(arg::Argument) = Function(API.LLVMGetParamParent(arg))

@property Argument parent


# basic block iteration

@vocabulary IR blocks, prevblock, nextblock

struct FunctionBlockSet <: AbstractVector{BasicBlock}
    f::Function
    cache::Vector{API.LLVMValueRef}

    FunctionBlockSet(f::Function) = new(f, API.LLVMValueRef[])
end

"""
    blocks(f::Function)

Get an iterator over the basic blocks of the given function.
"""
blocks(f::Function) = FunctionBlockSet(f)

Base.size(iter::FunctionBlockSet) = (Int(API.LLVMCountBasicBlocks(iter.f)),)

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

"""
    prevblock(bb::BasicBlock)

Get the previous basic block of the given basic block, or `nothing` if there is none.
"""
function prevblock(bb::BasicBlock)
    ref = API.LLVMGetPreviousBasicBlock(bb)
    ref == C_NULL && return nothing
    BasicBlock(ref)
end

"""
    nextblock(bb::BasicBlock)

Get the next basic block of the given basic block, or `nothing` if there is none.
"""
function nextblock(bb::BasicBlock)
    ref = API.LLVMGetNextBasicBlock(bb)
    ref == C_NULL && return nothing
    BasicBlock(ref)
end

# provide a random access interface by maintaining a cache of blocks
function Base.getindex(iter::FunctionBlockSet, i::Int)
    i <= 0 && throw(BoundsError(iter, i))
    i == 1 && return first(iter)
    while i > length(iter.cache)
        next = if isempty(iter.cache)
            iterate(iter)
        else
            iterate(iter, iter.cache[end])
        end
        next === nothing && throw(BoundsError(iter, i))
        push!(iter.cache, next[2])
    end
    return BasicBlock(iter.cache[i-1])
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
    isintrinsic(f::Function)

Check if the given function is an intrinsic.
"""
isintrinsic(f::Function) = API.LLVMGetIntrinsicID(f) != 0

"""
    LLVM.Intrinsic

An LLVM intrinsic function, identified by its ID.

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
        new(API.LLVMLookupIntrinsicID(name, length(name)))
    end
end
@properties Intrinsic

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
    LLVM.overloaded_name(intr::LLVM.Intrinsic, params::Vector{<:LLVMType})

Get the name of the given overloaded intrinsic with the given parameter types, e.g.,
`llvm.sin.f64`.
"""
function overloaded_name(intr::Intrinsic, params::Vector{<:LLVMType})
    len = Ref{Csize_t}()
    str = API.LLVMIntrinsicCopyOverloadedName(intr, params, length(params), len)
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
    Function(mod::Module, intr::Intrinsic, params::Vector{<:LLVMType}=LLVMType[])

Get the declaration of the given intrinsic in the given module.
"""
function Function(mod::Module, intr::Intrinsic, params::Vector{<:LLVMType}=LLVMType[])
    Value(API.LLVMGetIntrinsicDeclaration(mod, intr, params, length(params)))
end

"""
    FunctionType(intr::Intrinsic, params::Vector{<:LLVMType}=LLVMType[])

Get the function type of the given intrinsic with the given parameter types.
"""
function FunctionType(intr::Intrinsic, params::Vector{<:LLVMType}=LLVMType[])
    LLVMType(API.LLVMIntrinsicGetType(context(), intr, params, length(params)))
end

function Base.show(io::IO, intr::Intrinsic)
    print(io, "Intrinsic($(intr.id))")
    if isoverloaded(intr)
        print(io, ": overloaded intrinsic")
    else
        print(io, ": \"$(name(intr))\"")
    end
end
