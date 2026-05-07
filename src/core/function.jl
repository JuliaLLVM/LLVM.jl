export erase!,
       personality, personality!,
       callconv, callconv!,
       gc, gc!,
       entry, function_type

"""
    LLVM.Function

A function in the IR.
"""
Function
# forward declaration of Function in src/core/basicblock.jl

register(Function, API.LLVMFunctionValueKind)

"""
    LLVM.Function(mod::Module, name::String, ft::FunctionType)

Create a new function in the given module with the given name and function type.
"""
Function(mod::Module, name::String, ft::FunctionType) =
    Function(API.LLVMAddFunction(mod, name, ft))

"""
    function_type(f::Function) -> LLVM.FunctionType

Get the function type of the given function. This returns a function type, as opposed to
[`value_type`](@ref) which returns the pointer type of the function constant.
"""
function_type(Fn::Function) = FunctionType(API.LLVMGetFunctionType(Fn))

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
    personality(f::Function)

Get the personality function of the given function, or `nothing` if it has none.
"""
function personality(f::Function)
    has_personality = API.LLVMHasPersonalityFn(f) |> Bool
    return has_personality ? Function(API.LLVMGetPersonalityFn(f)) : nothing
end

"""
    personality!(f::Function, persfn::Function)

Set the personality function of the given function. Pass `nothing` to remove the personality
function.
"""
function personality!(f::Function, persfn::Union{Nothing,Function})
    api = version() >= v"20" ? API.LLVMSetPersonalityFn : API.LLVMSetPersonalityFn2
    api(f, something(persfn, C_NULL))
end

"""
    callconv(f::Function)

Get the calling convention of the given function.
"""
callconv(f::Function) = API.LLVMGetFunctionCallConv(f)

"""
    callconv!(f::Function, cc)

Set the calling convention of the given function.
"""
callconv!(f::Function, cc) = API.LLVMSetFunctionCallConv(f, cc)

"""
    gc(f::Function)

Get the garbage collector name of the given function, or an empty string if it has none.
"""
function gc(f::Function)
  ptr = API.LLVMGetGC(f)
  return ptr==C_NULL ? "" : unsafe_string(ptr)
end

"""
    gc!(f::Function, name::String)

Set the garbage collector name of the given function.
"""
gc!(f::Function, name::String) = API.LLVMSetGC(f, name)

"""
    entry(f::Function) -> BasicBlock

Get the entry basic block of the given function.
"""
entry(f::Function) = BasicBlock(API.LLVMGetEntryBasicBlock(f))


# attributes

export function_attributes, parameter_attributes, return_attributes

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


# parameter iteration

export Argument, parameters

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

function Base.iterate(iter::FunctionParameterSet, state=API.LLVMGetFirstParam(iter.f))
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


# basic block iteration

export blocks, prevblock, nextblock

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

function Base.iterate(iter::FunctionBlockSet, state=API.LLVMGetFirstBasicBlock(iter.f))
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

export isintrinsic, Intrinsic, isoverloaded

"""
    isintrinsic(f::Function)

Check if the given function is an intrinsic.
"""
isintrinsic(f::Function) = API.LLVMGetIntrinsicID(f) != 0

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

Base.convert(::Type{UInt32}, intr::Intrinsic) = intr.id

"""
    name(intr::LLVM.Intrinsic)

Get the name of the given intrinsic.
"""
function name(intr::Intrinsic)
    len = Ref{Csize_t}()
    str = API.LLVMIntrinsicGetName(intr, len)
    unsafe_string(convert(Ptr{Cchar}, str), len[])
end

"""
    name(intr::LLVM.Intrinsic, params::Vector{<:LLVMType})

Get the name of the given overloaded intrinsic with the given parameter types.
"""
function name(intr::Intrinsic, params::Vector{<:LLVMType})
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
