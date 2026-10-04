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

    dominates(tree::DomTree, A::Instruction, use::Use)

Check if instruction `A` dominates a use of a value, i.e., whether the value of `A` is
available where it is used (for a use by a PHI node, at the end of the incoming block).

    dominates(tree::DomTree, A::BasicBlock, B::BasicBlock)

Check if basic block `A` dominates basic block `B`.
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

dominates(domtree::DomTree, A::Instruction, use::Use) =
    API.LLVMExtraDominatorTreeInstructionDominatesUse(domtree, A, use) |> Bool

dominates(domtree::DomTree, A::BasicBlock, B::BasicBlock) =
    API.LLVMExtraDominatorTreeBlockDominates(domtree, A, B) |> Bool


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


## loop info

@vocabulary Analysis LoopInfo, Loop

"""
    LoopInfo

The natural loops of a function, as computed by LLVM's loop analysis. It is obtained from
the analysis manager of a custom pass (`am[LoopInfo]`). Indexing it with a basic block,
`li[bb]`, returns the innermost [`Loop`](@ref) containing the block, or `nothing` if the
block is not part of a loop.
"""
@checked struct LoopInfo
    ref::API.LLVMLoopInfoRef
end

Base.unsafe_convert(::Type{API.LLVMLoopInfoRef}, li::LoopInfo) = li.ref

"""
    Loop

A natural loop, as identified by [`LoopInfo`](@ref). Loops are owned by the loop analysis.
Use `bb in loop` to check whether a basic block is part of the loop (or of one of its
nested loops).

Like the loop analysis itself, loops must not be used after the pass that obtained them
returns, or after the analysis was invalidated.

# Properties

- `loop.header`: the header block of the loop, which dominates all of its blocks.
- `loop.parent`: the loop containing this loop, or `nothing` for an outermost loop.
- `loop.depth`: the nesting depth of the loop (1 for an outermost loop).
"""
@checked struct Loop
    ref::API.LLVMLoopRef
end
@properties Loop

Base.unsafe_convert(::Type{API.LLVMLoopRef}, loop::Loop) = loop.ref

loop_or_nothing(ref::API.LLVMLoopRef) = ref == C_NULL ? nothing : Loop(ref)

Base.getindex(li::LoopInfo, bb::BasicBlock) =
    loop_or_nothing(API.LLVMExtraLoopInfoGetLoopFor(li, bb))

header(loop::Loop) = BasicBlock(API.LLVMExtraLoopGetHeader(loop))
parent(loop::Loop) = loop_or_nothing(API.LLVMExtraLoopGetParent(loop))
depth(loop::Loop) = Int(API.LLVMExtraLoopGetDepth(loop))

@property Loop header
@property Loop parent
@property Loop depth

Base.in(bb::BasicBlock, loop::Loop) = API.LLVMExtraLoopContains(loop, bb) |> Bool

Base.show(io::IO, loop::Loop) =
    print(io, "Loop(header=", repr(header(loop).name), ", depth=", depth(loop), ")")


## scalar evolution

@vocabulary Analysis ScalarEvolution, SCEV, SCEVConstant, SCEVVScale, SCEVTruncateExpr,
                     SCEVZeroExtendExpr, SCEVSignExtendExpr, SCEVPtrToIntExpr, SCEVAddExpr,
                     SCEVMulExpr, SCEVUDivExpr, SCEVAddRecExpr, SCEVUMaxExpr, SCEVSMaxExpr,
                     SCEVUMinExpr, SCEVSMinExpr, SCEVSequentialUMinExpr, SCEVUnknown,
                     SCEVCouldNotCompute, scev_add, scev_minus, contains_scev

"""
    ScalarEvolution

LLVM's scalar evolution analysis, which represents integer and pointer values as symbolic
expressions ([`SCEV`](@ref)s), e.g., to recognize induction variables of loops. It is
obtained from the analysis manager of a custom pass (`am[ScalarEvolution]`).

- `se[v]`: the expression for the integer or pointer value `v`;
- [`scev_add`](@ref), [`scev_minus`](@ref): build new expressions;
- `ConstantRange(se, s; signed=false)`: the range of the values of an expression.

Expressions are owned by the analysis and uniqued, so that expressions that are equal are
also identical (`==`). The analysis caches its results, which become stale when the
function changes.
"""
@checked struct ScalarEvolution
    ref::API.LLVMScalarEvolutionRef
end

Base.unsafe_convert(::Type{API.LLVMScalarEvolutionRef}, se::ScalarEvolution) = se.ref

"""
    SCEV

A symbolic expression for a value, as computed by [`ScalarEvolution`](@ref). Expressions
are represented by a concrete subtype for each kind of expression: `SCEVConstant`,
`SCEVUnknown` (a value that the analysis does not look into), `SCEVAddExpr`,
`SCEVMulExpr`, `SCEVUDivExpr`, `SCEVAddRecExpr` (an add recurrence, like an induction
variable of a loop), casts (`SCEVTruncateExpr`, `SCEVZeroExtendExpr`,
`SCEVSignExtendExpr`, `SCEVPtrToIntExpr`), minima and maxima (`SCEVUMaxExpr`,
`SCEVSMaxExpr`, `SCEVUMinExpr`, `SCEVSMinExpr`, `SCEVSequentialUMinExpr`), `SCEVVScale`
(LLVM 17+), and `SCEVCouldNotCompute` (e.g., for the difference of pointers with
different bases). Like the analysis itself, expressions must not be used after the pass
that obtained them returns, or after the analysis was invalidated.

# Properties

- `s.type`: the type of the expression (not available for `SCEVCouldNotCompute`).
- `s.operands`: the operands of the expression (a vector of `SCEV`s).
- `s.value`: the `ConstantInt` of a `SCEVConstant`, or the value of a `SCEVUnknown`.
- `s.loop`: the [`Loop`](@ref) of a `SCEVAddRecExpr`.
"""
abstract type SCEV end
@properties SCEV

Base.unsafe_convert(::Type{API.LLVMSCEVRef}, s::SCEV) = s.ref

const scev_kinds = Dict{API.LLVMExtraSCEVKind,Type{<:SCEV}}()

for (T, kind) in [(:SCEVConstant, :LLVMExtraSCEVConstantKind),
                  (:SCEVTruncateExpr, :LLVMExtraSCEVTruncateKind),
                  (:SCEVZeroExtendExpr, :LLVMExtraSCEVZeroExtendKind),
                  (:SCEVSignExtendExpr, :LLVMExtraSCEVSignExtendKind),
                  (:SCEVAddExpr, :LLVMExtraSCEVAddKind),
                  (:SCEVMulExpr, :LLVMExtraSCEVMulKind),
                  (:SCEVUDivExpr, :LLVMExtraSCEVUDivKind),
                  (:SCEVAddRecExpr, :LLVMExtraSCEVAddRecKind),
                  (:SCEVUMaxExpr, :LLVMExtraSCEVUMaxKind),
                  (:SCEVSMaxExpr, :LLVMExtraSCEVSMaxKind),
                  (:SCEVUMinExpr, :LLVMExtraSCEVUMinKind),
                  (:SCEVSMinExpr, :LLVMExtraSCEVSMinKind),
                  (:SCEVSequentialUMinExpr, :LLVMExtraSCEVSequentialUMinKind),
                  (:SCEVUnknown, :LLVMExtraSCEVUnknownKind),
                  (:SCEVCouldNotCompute, :LLVMExtraSCEVCouldNotComputeKind),
                  (:SCEVVScale, :LLVMExtraSCEVVScaleKind),
                  (:SCEVPtrToIntExpr, :LLVMExtraSCEVPtrToIntKind),
                  (:SCEVOtherExpr, :LLVMExtraSCEVOtherKind)]
    @eval begin
        @checked struct $T <: SCEV
            ref::API.LLVMSCEVRef
        end
        scev_kinds[API.$kind] = $T
        scev_kind(::Type{$T}) = API.$kind
        @doc (@doc SCEV) $T
    end
end

# wrap an expression in the type corresponding to its kind
function SCEV(ref::API.LLVMSCEVRef)
    ref == C_NULL && throw(UndefRefError())
    return scev_kinds[API.LLVMExtraSCEVGetKind(ref)](ref)
end

function type(s::SCEV)
    s isa SCEVCouldNotCompute &&
        throw(ArgumentError("A could-not-compute expression has no type"))
    return LLVMType(API.LLVMExtraSCEVGetType(s))
end

function operands(s::SCEV)
    n = API.LLVMExtraSCEVGetOperands(s, C_NULL)
    refs = Vector{API.LLVMSCEVRef}(undef, n)
    API.LLVMExtraSCEVGetOperands(s, refs)
    return SCEV[SCEV(ref) for ref in refs]
end

value(s::SCEVConstant) = ConstantInt(API.LLVMExtraSCEVGetValue(s))
value(s::SCEVUnknown) = Value(API.LLVMExtraSCEVGetValue(s))

loop(s::SCEVAddRecExpr) = Loop(API.LLVMExtraSCEVAddRecGetLoop(s))

@property SCEV type
@property SCEV operands
@property Union{SCEVConstant,SCEVUnknown} value
@property SCEVAddRecExpr loop

function Base.show(io::IO, s::SCEV)
    str = unsafe_message(API.LLVMExtraPrintSCEVToString(s))
    print(io, nameof(typeof(s)), "(", str, ")")
end

function Base.getindex(se::ScalarEvolution, v::Value)
    API.LLVMExtraScalarEvolutionIsSCEVable(se, value_type(v)) |> Bool ||
        throw(ArgumentError("Scalar evolution only supports integer and pointer values"))
    return SCEV(API.LLVMExtraScalarEvolutionGetSCEV(se, v))
end

"""
    scev_add(se::ScalarEvolution, operands::SCEV...)
    scev_minus(se::ScalarEvolution, a::SCEV, b::SCEV)

Build the (simplified) expression for the sum of the `operands`, or for `a - b`. The
operands must have the same type, where pointers count as integers of their index width,
and only one of the operands of a sum can be a pointer. The difference of two pointers with
different bases is a `SCEVCouldNotCompute`.
"""
function scev_add(se::ScalarEvolution, operands::SCEV...)
    isempty(operands) && throw(ArgumentError("Cannot add zero expressions"))
    refs = API.LLVMSCEVRef[s.ref for s in operands]
    ref = API.LLVMExtraScalarEvolutionGetAddExpr(se, refs, length(refs))
    ref == C_NULL && throw(ArgumentError("Cannot add expressions of incompatible types"))
    return SCEV(ref)
end

@doc (@doc scev_add)
function scev_minus(se::ScalarEvolution, a::SCEV, b::SCEV)
    ref = API.LLVMExtraScalarEvolutionGetMinusSCEV(se, a, b)
    ref == C_NULL &&
        throw(ArgumentError("Cannot subtract expressions of incompatible types"))
    return SCEV(ref)
end

"""
    contains_scev(s::SCEV, T::Type{<:SCEV})

Check whether the expression `s`, or one of its subexpressions, is of type `T`. For
example, `contains_scev(s, SCEVAddRecExpr)` checks whether `s` varies with a loop.
"""
contains_scev(s::SCEV, T::Type{<:SCEV}) = API.LLVMExtraSCEVContains(s, scev_kind(T)) |> Bool

"""
    ConstantRange(se::ScalarEvolution, s::SCEV; signed=false)

The range of the values of the expression `s`, as computed by scalar evolution, preferring
a range that does not wrap in the unsigned domain or, with `signed=true`, in the signed
domain.
"""
function ConstantRange(se::ScalarEvolution, s::SCEV; signed::Bool=false)
    s isa SCEVCouldNotCompute &&
        throw(ArgumentError("A could-not-compute expression has no range"))
    nbits = API.LLVMExtraScalarEvolutionGetRange(se, s, signed, C_NULL, C_NULL)
    compute_range(Val(nwords(nbits)), nbits) do lo, hi
        API.LLVMExtraScalarEvolutionGetRange(se, s, signed, lo, hi)
    end
end


## dead code

@vocabulary IR is_trivially_dead, erase_trivially_dead!

"""
    is_trivially_dead(inst::Instruction)

Check whether the instruction is unused and has no side effects, so that it can be
deleted.
"""
is_trivially_dead(inst::Instruction) = API.LLVMExtraIsInstructionTriviallyDead(inst) |> Bool

"""
    erase_trivially_dead!(inst::Instruction)

If the instruction is trivially dead (see [`is_trivially_dead`](@ref)), erase it, together
with its operands that become trivially dead as a result, recursively. Returns whether the
instruction was erased.

Other wrappers of the erased instructions (e.g., of the operands of `inst`) must not be used
anymore.
"""
erase_trivially_dead!(inst::Instruction) =
    API.LLVMExtraRecursivelyDeleteTriviallyDeadInstructions(inst) |> Bool
