## module and function verification

@vocabulary IR verify, verification_error

"""
    verify(mod::Module)
    verify(f::Function)

Verify the module or function `mod` or `f`. If verification fails, an `LLVMException` is
thrown with the verifier's message. See [`verification_error`](@ref) for a variant that
does not throw.
"""
function verify(x::Union{Module, Function})
    msg = verification_error(x)
    msg === nothing || throw(LLVMException(msg))
    return
end

"""
    verification_error(mod::Module)
    verification_error(f::Function)

Verify the module or function `mod` or `f`, returning the verifier's message if it is
broken, or `nothing` if it is valid. This is useful to report errors with more context:

```julia
msg = verification_error(f)
msg === nothing || error("Generated invalid code for \$name:\n\$msg\n\$(string(f))")
```
"""
function verification_error(mod::Module)
    out_error = Ref{Cstring}()
    status = API.LLVMVerifyModule(mod, API.LLVMReturnStatusAction, out_error) |> Bool
    msg = unsafe_message(out_error[])
    return status ? msg : nothing
end

function verification_error(f::Function)
    out_error = Ref{Cstring}()
    status = API.LLVMExtraVerifyFunction(f, out_error) |> Bool
    return status ? unsafe_message(out_error[]) : nothing
end


## dominator analysis

@vocabulary IR dominates

"""
    dominates(tree::DomTree, A::Instruction, B::Instruction)
    dominates(tree::PostDomTree, A::Instruction, B::Instruction)

Check if instruction `A` dominates instruction `B` in the dominator tree `tree`.
"""
dominates(tree, A::Instruction, B::Instruction)

# dominance

@vocabulary IR DomTree

"""
    DomTree

Dominator tree for a function.
"""
@checked struct DomTree
    ref::API.LLVMDominatorTreeRef
end

Base.unsafe_convert(::Type{API.LLVMDominatorTreeRef}, domtree::DomTree) =
    mark_use(domtree).ref

"""
    DomTree(f::Function)
    DomTree(callback, f::Function)

Create a dominator tree for the function `f`.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form.
"""
DomTree(f::Function) = mark_alloc(DomTree(API.LLVMCreateDominatorTree(f)))

DomTree(callback::Core.Function, f::Function) = with_disposal(callback, DomTree(f))

"""
    dispose(::DomTree)

Dispose of a dominator tree.
"""
dispose(domtree::DomTree) = mark_dispose(API.LLVMDisposeDominatorTree, domtree)

function dominates(domtree::DomTree, A::Instruction, B::Instruction)
    API.LLVMDominatorTreeInstructionDominates(domtree, A, B) |> Bool
end


## post-dominance

@vocabulary IR PostDomTree

"""
    PostDomTree

Post-dominator tree for a function.
"""
@checked struct PostDomTree
    ref::API.LLVMPostDominatorTreeRef
end

Base.unsafe_convert(::Type{API.LLVMPostDominatorTreeRef}, postdomtree::PostDomTree) =
    mark_use(postdomtree).ref

"""
    PostDomTree(f::Function)
    PostDomTree(callback, f::Function)

Create a post-dominator tree for the function `f`.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form.
"""
PostDomTree(f::Function) = mark_alloc(PostDomTree(API.LLVMCreatePostDominatorTree(f)))

PostDomTree(callback::Core.Function, f::Function) = with_disposal(callback, PostDomTree(f))

"""
    dispose(tree::PostDomTree)

Dispose of a post-dominator tree.
"""
dispose(postdomtree::PostDomTree) =
    mark_dispose(API.LLVMDisposePostDominatorTree, postdomtree)

function dominates(postdomtree::PostDomTree, A::Instruction, B::Instruction)
    API.LLVMPostDominatorTreeInstructionDominates(postdomtree, A, B) |> Bool
end


## assumption cache

@vocabulary Analysis AssumptionCache, AssumptionEntry

"""
    AssumptionCache

The `llvm.assume` calls of a function, as cached by LLVM's assumption cache. It is obtained
from the analysis manager of a custom pass (`am[AssumptionCache]`), and used by other
analyses to find the assumptions that are relevant to a value.

- iterating the cache yields the assumptions (as `CallInst`s) of the function;
- `ac[v]` returns the assumptions that affect the value `v`, as a vector of
  [`AssumptionEntry`](@ref);
- `push!(ac, assume)` registers an `llvm.assume` call that the pass created, which is
  required for other analyses to see it;
- `empty!(ac)` clears the cache, so that it is rebuilt from the function when it is used
  next.

Unlike other analyses, the assumption cache is not invalidated when a pass changes the
function: passes that create assumptions must register them, while deleted assumptions are
handled automatically. The assumptions that are returned are a snapshot.
"""
@checked struct AssumptionCache
    ref::API.LLVMAssumptionCacheRef
end

Base.unsafe_convert(::Type{API.LLVMAssumptionCacheRef}, ac::AssumptionCache) = ac.ref

function assumptions(ac::AssumptionCache)
    n = API.LLVMExtraAssumptionCacheGetAssumptions(ac, C_NULL)
    refs = Vector{API.LLVMValueRef}(undef, n)
    API.LLVMExtraAssumptionCacheGetAssumptions(ac, refs)
    return CallInst[CallInst(ref) for ref in refs]
end

Base.IteratorSize(::Type{AssumptionCache}) = Base.SizeUnknown()
Base.eltype(::Type{AssumptionCache}) = CallInst
Base.iterate(ac::AssumptionCache, state=(assumptions(ac), 1)) =
    state[2] > length(state[1]) ? nothing : (state[1][state[2]], (state[1], state[2] + 1))

"""
    AssumptionEntry

An assumption that affects a value, as returned by indexing an [`AssumptionCache`](@ref):

- `entry.assume`: the `llvm.assume` call;
- `entry.bundle_index`: the (1-based) index of the operand bundle of the assumption that
  affects the value, or `nothing` if the value is affected by the condition of the
  assumption.
"""
struct AssumptionEntry
    assume::CallInst
    bundle_index::Union{Nothing,Int}
end

function Base.getindex(ac::AssumptionCache, v::Value)
    n = API.LLVMExtraAssumptionCacheGetAssumptionsFor(ac, v, C_NULL, C_NULL)
    refs = Vector{API.LLVMValueRef}(undef, n)
    indices = Vector{Cint}(undef, n)
    API.LLVMExtraAssumptionCacheGetAssumptionsFor(ac, v, refs, indices)
    return AssumptionEntry[AssumptionEntry(CallInst(ref), idx < 0 ? nothing : idx + 1)
                           for (ref, idx) in zip(refs, indices)]
end

function Base.push!(ac::AssumptionCache, assume::Instruction)
    API.LLVMExtraAssumptionCacheRegisterAssumption(ac, assume) |> Bool ||
        throw(ArgumentError("Only calls to llvm.assume can be registered as assumptions"))
    return ac
end

function Base.empty!(ac::AssumptionCache)
    API.LLVMExtraAssumptionCacheClear(ac)
    return ac
end


## value tracking

@vocabulary Analysis is_valid_assume_for_context, is_guaranteed_not_to_be_poison,
                     program_undefined_if_poison

# the bit width of the integers in a value, or `nothing` if it isn't one
function integer_width(v::Value)
    ty = value_type(v)
    ty isa VectorType && (ty = eltype(ty))
    return ty isa IntegerType ? width(ty) : nothing
end

function checked_integer_width(v::Value)
    nbits = integer_width(v)
    nbits === nothing &&
        throw(ArgumentError("Value is not an integer or a vector of integers"))
    return nbits
end

"""
    ConstantRange(v::Value; signed=false, at=nothing, assumptions=nothing, domtree=nothing,
                  datalayout=nothing, use_instruction_info=true)

Compute a range of the values of the integer (or vector of integers) `v`, using LLVM's
value tracking (`computeConstantRange`). It analyzes the instructions computing `v`, and
optionally the assumptions that hold at instruction `at` (which needs the
[`AssumptionCache`](@ref) and, for assumptions that are not in the same block, the
[`DomTree`](@ref)). With `signed=true`, a range that does not wrap in the signed domain is
preferred. The data layout defaults to that of the module containing `at` or `v`; with
`use_instruction_info=false`, metadata and flags of instructions (like `!range` or `nuw`)
are ignored.

The result is a fact about `v` if it is not poison: it does not prove that `v` is not
poison (see [`is_guaranteed_not_to_be_poison`](@ref)).
"""
function ConstantRange(v::Value; signed::Bool=false, at::Union{Nothing,Instruction}=nothing,
                       assumptions::Union{Nothing,AssumptionCache}=nothing,
                       domtree::Union{Nothing,DomTree}=nothing,
                       datalayout::Union{Nothing,DataLayout}=nothing,
                       use_instruction_info::Bool=true)
    nbits = checked_integer_width(v)
    result = compute_range(Val(nwords(nbits)), nbits) do lo, hi
        API.LLVMExtraComputeConstantRange(v, signed, use_instruction_info,
                                          something(assumptions, C_NULL),
                                          something(at, C_NULL), something(domtree, C_NULL),
                                          something(datalayout, C_NULL), lo, hi) |> Bool
    end
    result === nothing && throw(ArgumentError("No data layout available for $v"))
    return result
end

"""
    KnownBits(v::Value; at=nothing, assumptions=nothing, domtree=nothing,
              datalayout=nothing, use_instruction_info=true)

Compute the bits of the integer (or vector of integers) `v` that are known to be zero or
one, using LLVM's value tracking (`computeKnownBits`). The keyword arguments are the same
as for [`ConstantRange(::Value)`](@ref). A data layout is required, so if neither `at` nor
`v` are part of a module, it needs to be passed explicitly.
"""
function KnownBits(v::Value; at::Union{Nothing,Instruction}=nothing,
                   assumptions::Union{Nothing,AssumptionCache}=nothing,
                   domtree::Union{Nothing,DomTree}=nothing,
                   datalayout::Union{Nothing,DataLayout}=nothing,
                   use_instruction_info::Bool=true)
    nbits = checked_integer_width(v)
    # known bits have the same layout as a range: reuse its machinery
    result = compute_range(Val(nwords(nbits)), nbits) do zero, one
        API.LLVMExtraComputeKnownBits(v, use_instruction_info,
                                      something(assumptions, C_NULL), something(at, C_NULL),
                                      something(domtree, C_NULL),
                                      something(datalayout, C_NULL), zero, one) |> Bool
    end
    result === nothing && throw(ArgumentError("No data layout available for $v"))
    return unsafe_known_bits(nbits, getfield(result, :lower), getfield(result, :upper))
end

"""
    is_valid_assume_for_context(assume::Instruction, at::Instruction; domtree=nothing)

Check whether the assumption `assume` (an `llvm.assume` call) holds at instruction `at`:
when it dominates `at`, or comes after it in the same block with only instructions in
between that are guaranteed to transfer execution to their successor. Without a dominator
tree, only assumptions in the same block or a predecessor of it are recognized.
"""
is_valid_assume_for_context(assume::Instruction, at::Instruction;
                            domtree::Union{Nothing,DomTree}=nothing) =
    API.LLVMExtraIsValidAssumeForContext(assume, at, something(domtree, C_NULL)) |> Bool

"""
    is_guaranteed_not_to_be_poison(v::Value; at=nothing, assumptions=nothing,
                                   domtree=nothing)

Check whether `v` is known not to be poison, optionally at instruction `at`.
"""
is_guaranteed_not_to_be_poison(v::Value; at::Union{Nothing,Instruction}=nothing,
                               assumptions::Union{Nothing,AssumptionCache}=nothing,
                               domtree::Union{Nothing,DomTree}=nothing) =
    API.LLVMExtraIsGuaranteedNotToBePoison(v, something(assumptions, C_NULL),
                                           something(at, C_NULL),
                                           something(domtree, C_NULL)) |> Bool

"""
    program_undefined_if_poison(inst::Instruction)

Check whether the program has undefined behavior if `inst` evaluates to poison, e.g.,
because the result is used as a pointer that is dereferenced, or as a branch condition, in
a way that is guaranteed to execute.
"""
program_undefined_if_poison(inst::Instruction) =
    API.LLVMExtraProgramUndefinedIfPoison(inst) |> Bool


## lazy value info

@vocabulary Analysis LazyValueInfo

"""
    LazyValueInfo

LLVM's lazy value information analysis, which computes the ranges of integer values at
specific points of a function, taking into account the conditions of the branches that lead
there (and assumptions). It is obtained from the analysis manager of a custom pass
(`am[LazyValueInfo]`), and queried by constructing a range:

    ConstantRange(lvi, v; at::Instruction, undef_allowed=false)
    ConstantRange(lvi, v; from::BasicBlock, to::BasicBlock, at=nothing)
    ConstantRange(lvi, use::Use; undef_allowed=false)

These compute the range of the integer (or vector of integers) `v` at instruction `at`, on
the edge between blocks `from` and `to` (optionally at an instruction `at` in `to`), or at
a use of `v` (LLVM 16+), which also takes into account the instruction using it. With
`undef_allowed=true`, the range may not include all values of a `v` that is undef.

The analysis caches its results, which become stale when the function changes (see
[`invalidate!`](@ref)).
"""
@checked struct LazyValueInfo
    ref::API.LLVMLazyValueInfoRef
end

Base.unsafe_convert(::Type{API.LLVMLazyValueInfoRef}, lvi::LazyValueInfo) = lvi.ref

function ConstantRange(lvi::LazyValueInfo, v::Value; at::Union{Nothing,Instruction}=nothing,
                       from::Union{Nothing,BasicBlock}=nothing,
                       to::Union{Nothing,BasicBlock}=nothing, undef_allowed::Bool=false)
    nbits = checked_integer_width(v)
    if from === nothing && to === nothing
        at === nothing &&
            throw(ArgumentError("Either a context instruction or an edge is required"))
        compute_range(Val(nwords(nbits)), nbits) do lo, hi
            API.LLVMExtraLazyValueInfoGetConstantRange(lvi, v, at, undef_allowed, lo,
                                                       hi) |> Bool
        end
    elseif from !== nothing && to !== nothing
        undef_allowed &&
            throw(ArgumentError("`undef_allowed` is not supported for ranges on an edge"))
        compute_range(Val(nwords(nbits)), nbits) do lo, hi
            API.LLVMExtraLazyValueInfoGetConstantRangeOnEdge(lvi, v, from, to,
                                                             something(at, C_NULL), lo,
                                                             hi) |> Bool
        end
    else
        throw(ArgumentError("An edge requires both `from` and `to`"))
    end
end

if version() >= v"16"
    function ConstantRange(lvi::LazyValueInfo, use::Use; undef_allowed::Bool=false)
        nbits = checked_integer_width(use.value)
        compute_range(Val(nwords(nbits)), nbits) do lo, hi
            API.LLVMExtraLazyValueInfoGetConstantRangeAtUse(lvi, use, undef_allowed, lo,
                                                            hi) |> Bool
        end
    end
end
