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

@vocabulary Analysis recalculate!

"""
    recalculate!(tree::DomTree, f::Function)

Recompute the dominator tree for the current control-flow graph of the function `f`, e.g.,
after a pass changed it, and return it. For a tree borrowed from an analysis manager, `f`
must be the function it was computed for; this makes it valid again, so that a pass can
preserve it.
"""
function recalculate!(domtree::DomTree, f::Function)
    isdeclaration(f) &&
        throw(ArgumentError("Cannot compute the dominator tree of a declaration"))
    API.LLVMExtraDominatorTreeRecalculate(domtree, f)
    return domtree
end


## control flow

@vocabulary IR split_block!, split_block_and_insert_if_then!

"""
    split_block!(pos::InsertionPoint{Instruction}, name::AbstractString="")
    split_block!(inst::Instruction, name::AbstractString="")

Split a basic block in two at an insertion point, or before an instruction: the
instructions from there on move to a new block with the given name, which is inserted after
the original one, and which the original block branches to unconditionally:

```
head:                            ; the original block, up to the split point
  br label %name
name:                            ; the returned block, with the rest of the original one
  ...
```

Returns the new block. The PHI nodes in the successors of the original block are updated
to refer to the new block. The block must have a terminator, and the split point must not be
at the end of the block, before a PHI node or before an exception-handling pad. Debug
records at the split point move along when the insertion point is at their head (e.g.,
`LLVM.after(prev)`), and stay in the original block otherwise (e.g., `LLVM.before(inst)`).

Analyses, like the dominator tree, are not updated.

This uses LLVM's `BasicBlock::splitBasicBlock`.
"""
function split_block!(pos::InsertionPoint{Instruction}, name::AbstractString="")
    bb = check_valid(pos)
    terminator(bb) === nothing &&
        throw(ArgumentError("Cannot split a basic block without a terminator"))
    pos.anchor == C_NULL &&
        throw(ArgumentError("Cannot split a basic block at its end"))
    Instruction(pos.anchor) isa Union{PHIInst,LandingPadInst,CatchPadInst,CleanupPadInst,
                                      CatchSwitchInst} &&
        throw(ArgumentError("Cannot split a basic block before a PHI node or an exception-handling pad"))
    BasicBlock(API.LLVMExtraSplitBasicBlock(bb, pos.anchor, pos.head, name))
end
split_block!(inst::Instruction, name::AbstractString="") = split_block!(before(inst), name)

"""
    split_block_and_insert_if_then!(cond::Value, at::Instruction; unreachable=false,
                                    domtree=nothing)

Split the block containing the instruction `at` before it, and make the first part branch
on the `i1` value `cond` to a new block, which branches to the second part, the tail (or,
with `unreachable=true`, ends with an `unreachable` instruction):

```
head:                            ; the original block, up to `at`
  br i1 %cond, label %then, label %tail
then:
  br label %tail                 ; the returned terminator
tail:                            ; `at` and the rest of the original block
  ...
```

Returns the terminator of the new block, before which instructions can be inserted. The
instruction `at` must not be a PHI node or an exception-handling pad.

If a dominator tree of the function containing `at` is passed as `domtree`, it is updated
for the new control flow, so that a pass can preserve it (e.g., with
`PreservedAnalyses(DomTree)`).
Other analyses, like [`LoopInfo`](@ref), are not updated, and the control-flow graph has
changed, so `CFGAnalyses` are not preserved.
"""
function split_block_and_insert_if_then!(cond::Value, at::Instruction;
                                         unreachable::Bool=false,
                                         domtree::Union{Nothing,DomTree}=nothing)
    ty = value_type(cond)
    ty isa IntegerType && width(ty) == 1 ||
        throw(ArgumentError("The condition must be an i1 value"))
    bb = parent(at)
    bb === nothing && throw(ArgumentError("Instruction is not part of a basic block"))
    terminator(bb) === nothing &&
        throw(ArgumentError("Cannot split a basic block without a terminator"))
    at isa Union{PHIInst,LandingPadInst,CatchPadInst,CleanupPadInst,CatchSwitchInst} &&
        throw(ArgumentError("Cannot split a basic block before a PHI node or an exception-handling pad"))
    term = API.LLVMExtraSplitBlockAndInsertIfThen(cond, at, unreachable,
                                                  something(domtree, C_NULL))
    term == C_NULL &&
        throw(ArgumentError("The dominator tree is not one of the function containing the instruction, or it is out of date"))
    return Instruction(term)
end


## inlining

@vocabulary IR inline!, inline_viability

"""
    inline!(call::CallBase; merge_attributes=false, insert_lifetime=true)

Inline the function that is called by a `call` or `invoke` instruction at this call site:
the body of the function is copied into the calling function, replacing the call. Returns
`nothing` if the call was inlined, after which the call instruction is erased, or a message
that describes why it could not be inlined, in which case the IR is unchanged:

```julia
reason = inline!(call)
reason === nothing || @debug "Could not inline \$call" reason
```

This is LLVM's `InlineFunction`, which only refuses call sites that it cannot transform:
indirect calls and calls of declarations, `callbr` instructions, calls with operand bundles
it doesn't support, and callers whose garbage collector or exception-handling personality
is incompatible with that of the called function. It does not check whether the body of the
called function can be inlined at all: e.g., inlining a function that calls `llvm.va_start`
into a function without variadic arguments results in invalid IR. Check that first with
[`inline_viability`](@ref), unless the functions are known to be fine. Whether inlining
is worth it (its cost, `noinline` attributes, or a definition that may be replaced at link
or load time) is up to the caller as well.

With `merge_attributes=true`, the attributes of the calling function are updated for the
inlined code (e.g., to combine their target features or stack protection). With
`insert_lifetime=true`, the static allocas of the inlined function are marked with lifetime
intrinsics. No call graph is updated, and no alias analysis is used.
"""
function inline!(call::CallBase; merge_attributes::Bool=false, insert_lifetime::Bool=true)
    bb = API.LLVMGetInstructionParent(check_attached(call))
    API.LLVMGetBasicBlockParent(bb) == C_NULL &&
        throw(ArgumentError("Call is not part of a function"))
    reason = Ref{Cstring}(C_NULL)
    API.LLVMExtraInlineFunction(call, merge_attributes, insert_lifetime, reason) |> Bool &&
        return nothing
    return unsafe_message(reason[])
end

"""
    inline_viability(f::LLVM.Function)

Check whether the body of a function can be inlined into other functions: returns `nothing`
if it can, or a message that describes why not, e.g., because it calls `llvm.va_start`,
uses `indirectbr` or `blockaddress`, calls itself, or calls a `returns_twice` function like
`setjmp`. This is what [`inline!`](@ref) expects the caller to check. The check is
conservative: a rejected function can sometimes still be inlined correctly (e.g., one level
of a recursive function).

This uses LLVM's `isInlineViable`.
"""
function inline_viability(f::Function)
    isdeclaration(f) && throw(ArgumentError("Function is a declaration"))
    reason = Ref{Cstring}(C_NULL)
    API.LLVMExtraIsInlineViable(f, reason) |> Bool && return nothing
    return unsafe_message(reason[])
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
                     is_guaranteed_not_to_be_undef_or_poison, program_undefined_if_poison,
                     is_known_non_negative, is_known_positive, is_known_non_zero,
                     num_sign_bits

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
    is_guaranteed_not_to_be_undef_or_poison(v::Value; at=nothing, assumptions=nothing,
                                            domtree=nothing)

Check whether `v` is known to be neither undef nor poison (i.e., each use of it observes
the same, well-defined value), optionally at instruction `at`. A value that is `noundef`
(e.g., a `noundef` argument, or a `freeze` instruction) is.
"""
is_guaranteed_not_to_be_undef_or_poison(v::Value; at::Union{Nothing,Instruction}=nothing,
                                        assumptions::Union{Nothing,AssumptionCache}=nothing,
                                        domtree::Union{Nothing,DomTree}=nothing) =
    API.LLVMExtraIsGuaranteedNotToBeUndefOrPoison(v, something(assumptions, C_NULL),
                                                  something(at, C_NULL),
                                                  something(domtree, C_NULL)) |> Bool

function is_known(fact::API.LLVMExtraKnownFact, v::Value, at, assumptions, domtree,
                  datalayout, use_instruction_info)
    known = Ref{API.LLVMBool}()
    API.LLVMExtraIsKnown(fact, v, use_instruction_info, something(assumptions, C_NULL),
                         something(at, C_NULL), something(domtree, C_NULL),
                         something(datalayout, C_NULL), known) |> Bool ||
        throw(ArgumentError("No data layout available for $v"))
    return Bool(known[])
end

"""
    is_known_non_negative(v::Value; at=nothing, assumptions=nothing, domtree=nothing,
                          datalayout=nothing, use_instruction_info=true)
    is_known_positive(v::Value; ...)
    is_known_non_zero(v::Value; ...)

Check whether the integer (or vector of integers) `v` is known to be non-negative, or
strictly positive, when interpreted as a signed integer, or whether the integer or pointer
(or vector of them) `v` is known to be non-zero, using LLVM's value tracking. For vectors,
the fact needs to hold for every element. The keyword arguments are the same as for
[`ConstantRange(::Value)`](@ref).

Like other value tracking queries, these are facts about `v` if it is not poison: they do
not prove that `v` is not poison (see [`is_guaranteed_not_to_be_poison`](@ref)). A result
of `false` means that the fact could not be proven.
"""
function is_known_non_negative(v::Value; at::Union{Nothing,Instruction}=nothing,
                               assumptions::Union{Nothing,AssumptionCache}=nothing,
                               domtree::Union{Nothing,DomTree}=nothing,
                               datalayout::Union{Nothing,DataLayout}=nothing,
                               use_instruction_info::Bool=true)
    checked_integer_width(v)
    return is_known(API.LLVMExtraKnownNonNegative, v, at, assumptions, domtree, datalayout,
                    use_instruction_info)
end

@doc (@doc is_known_non_negative)
function is_known_positive(v::Value; at::Union{Nothing,Instruction}=nothing,
                           assumptions::Union{Nothing,AssumptionCache}=nothing,
                           domtree::Union{Nothing,DomTree}=nothing,
                           datalayout::Union{Nothing,DataLayout}=nothing,
                           use_instruction_info::Bool=true)
    checked_integer_width(v)
    return is_known(API.LLVMExtraKnownPositive, v, at, assumptions, domtree, datalayout,
                    use_instruction_info)
end

@doc (@doc is_known_non_negative)
function is_known_non_zero(v::Value; at::Union{Nothing,Instruction}=nothing,
                           assumptions::Union{Nothing,AssumptionCache}=nothing,
                           domtree::Union{Nothing,DomTree}=nothing,
                           datalayout::Union{Nothing,DataLayout}=nothing,
                           use_instruction_info::Bool=true)
    ty = value_type(v)
    ty isa VectorType && (ty = eltype(ty))
    ty isa Union{IntegerType,PointerType} ||
        throw(ArgumentError("Value is not an integer or a pointer, or a vector of them"))
    return is_known(API.LLVMExtraKnownNonZero, v, at, assumptions, domtree, datalayout,
                    use_instruction_info)
end

"""
    num_sign_bits(v::Value; at=nothing, assumptions=nothing, domtree=nothing,
                  datalayout=nothing, use_instruction_info=true)

Compute the number of high bits of the integer (or vector of integers) `v` that are known
to be equal to its sign bit, including the sign bit itself, using LLVM's value tracking.
This is a lower bound, at least 1, which holds for every element of a vector. For example,
an `i32` value with 17 sign bits fits in a signed 16-bit integer. The keyword arguments
are the same as for [`ConstantRange(::Value)`](@ref).
"""
function num_sign_bits(v::Value; at::Union{Nothing,Instruction}=nothing,
                       assumptions::Union{Nothing,AssumptionCache}=nothing,
                       domtree::Union{Nothing,DomTree}=nothing,
                       datalayout::Union{Nothing,DataLayout}=nothing,
                       use_instruction_info::Bool=true)
    checked_integer_width(v)
    n = API.LLVMExtraComputeNumSignBits(v, use_instruction_info,
                                        something(assumptions, C_NULL),
                                        something(at, C_NULL), something(domtree, C_NULL),
                                        something(datalayout, C_NULL))
    n == 0 && throw(ArgumentError("No data layout available for $v"))
    return Int(n)
end

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
block is not part of a loop. Iterating it yields the outermost loops of the function.
"""
@checked struct LoopInfo
    ref::API.LLVMLoopInfoRef
end

Base.unsafe_convert(::Type{API.LLVMLoopInfoRef}, li::LoopInfo) = li.ref

"""
    Loop

A natural loop, as identified by [`LoopInfo`](@ref). Loops are owned by the loop analysis.
Use `bb in loop` or `inst in loop` to check whether a basic block or instruction is part
of the loop (or of one of its nested loops), and [`is_loop_invariant`](@ref) to check
whether a value is defined outside of it.

Like the loop analysis itself, loops must not be used after the pass that obtained them
returns, or after the analysis was invalidated.

# Properties

- `loop.header`: the header block of the loop, which dominates all of its blocks.
- `loop.parent`: the loop containing this loop, or `nothing` for an outermost loop.
- `loop.depth`: the nesting depth of the loop (1 for an outermost loop).
- `loop.blocks`: the blocks of the loop, including those of nested loops, starting with
  the header (a read-only vector).
- `loop.subloops`: the loops that are directly nested in this loop (a read-only vector).
- `loop.exit_blocks`: the blocks outside of the loop that it branches to, without
  duplicates (an iterable that finds them when it is iterated).
- `loop.latch`: the block that branches back to the header, or `nothing` if there are
  several.
- `loop.preheader`: the only predecessor of the header that is outside of the loop, if it
  only branches to the header, or `nothing` otherwise (e.g., because the loop is not in
  the simplified form that the `loop-simplify` pass creates).
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

# read a list of references, using a C function that returns their number, and writes them
# to an array if it is not NULL
function read_refs(f, ::Type{T}, args...) where {T}
    n = f(args..., C_NULL)
    refs = Vector{T}(undef, n)
    f(args..., refs)
    return refs
end

struct LoopBlockSet <: AbstractVector{BasicBlock}
    loop::Loop
end

Base.size(iter::LoopBlockSet) = (Int(API.LLVMExtraLoopGetNumBlocks(iter.loop)),)
function Base.getindex(iter::LoopBlockSet, i::Int)
    @boundscheck checkbounds(iter, i)
    return BasicBlock(API.LLVMExtraLoopGetBlock(iter.loop, i - 1))
end

struct LoopSubLoopSet <: AbstractVector{Loop}
    loop::Loop
end

Base.size(iter::LoopSubLoopSet) = (Int(API.LLVMExtraLoopGetNumSubLoops(iter.loop)),)
function Base.getindex(iter::LoopSubLoopSet, i::Int)
    @boundscheck checkbounds(iter, i)
    return Loop(API.LLVMExtraLoopGetSubLoop(iter.loop, i - 1))
end

# the exit blocks are found by looking at the successors of the blocks of the loop
struct LoopExitBlockSet
    loop::Loop
end

Base.IteratorSize(::Type{LoopExitBlockSet}) = Base.SizeUnknown()
Base.eltype(::Type{LoopExitBlockSet}) = BasicBlock
function Base.iterate(iter::LoopExitBlockSet,
                      state=(read_refs(API.LLVMExtraLoopGetUniqueExitBlocks,
                                       API.LLVMBasicBlockRef, iter.loop), 1))
    refs, i = state
    return i > length(refs) ? nothing : (BasicBlock(refs[i]), (refs, i + 1))
end

blocks(loop::Loop) = LoopBlockSet(loop)
subloops(loop::Loop) = LoopSubLoopSet(loop)
exit_blocks(loop::Loop) = LoopExitBlockSet(loop)
block_or_nothing(ref::API.LLVMBasicBlockRef) = ref == C_NULL ? nothing : BasicBlock(ref)
latch(loop::Loop) = block_or_nothing(API.LLVMExtraLoopGetLatch(loop))
preheader(loop::Loop) = block_or_nothing(API.LLVMExtraLoopGetPreheader(loop))

@property Loop header
@property Loop parent
@property Loop depth
@property Loop blocks
@property Loop subloops
@property Loop exit_blocks
@property Loop latch
@property Loop preheader

Base.in(bb::BasicBlock, loop::Loop) = API.LLVMExtraLoopContains(loop, bb) |> Bool
function Base.in(inst::Instruction, loop::Loop)
    bb = parent(inst)
    return bb !== nothing && bb in loop
end

Base.IteratorSize(::Type{LoopInfo}) = Base.SizeUnknown()
Base.eltype(::Type{LoopInfo}) = Loop
function Base.iterate(li::LoopInfo,
                      state=(read_refs(API.LLVMExtraLoopInfoGetTopLevelLoops,
                                       API.LLVMLoopRef, li), 1))
    refs, i = state
    return i > length(refs) ? nothing : (Loop(refs[i]), (refs, i + 1))
end

Base.show(io::IO, loop::Loop) =
    print(io, "Loop(header=", repr(header(loop).name), ", depth=", depth(loop), ")")


## scalar evolution

@vocabulary Analysis ScalarEvolution, SCEV, SCEVConstant, SCEVVScale, SCEVTruncateExpr,
                     SCEVZeroExtendExpr, SCEVSignExtendExpr, SCEVPtrToIntExpr, SCEVAddExpr,
                     SCEVMulExpr, SCEVUDivExpr, SCEVAddRecExpr, SCEVUMaxExpr, SCEVSMaxExpr,
                     SCEVUMinExpr, SCEVSMinExpr, SCEVSequentialUMinExpr, SCEVUnknown,
                     SCEVCouldNotCompute, scev_add, scev_minus, scev_mul, scev_cast,
                     contains_scev, is_scevable, is_loop_invariant, backedge_taken_count,
                     step_recurrence

"""
    ScalarEvolution

LLVM's scalar evolution analysis, which represents integer and pointer values as symbolic
expressions ([`SCEV`](@ref)s), e.g., to recognize induction variables of loops. It is
obtained from the analysis manager of a custom pass (`am[ScalarEvolution]`).

- `se[v]`: the expression for the integer or pointer value `v` (see
  [`is_scevable`](@ref));
- [`scev_add`](@ref), [`scev_minus`](@ref), [`scev_mul`](@ref), [`scev_cast`](@ref):
  build new expressions;
- `ConstantRange(se, s; signed=false)`: the range of the values of an expression;
- [`is_loop_invariant`](@ref), [`backedge_taken_count`](@ref),
  [`step_recurrence`](@ref): loop-related queries.

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
- `s.start`: the start value of a `SCEVAddRecExpr` (its first operand).
- `s.affine`: whether a `SCEVAddRecExpr` is affine, i.e., has a step that is invariant in
  its loop (when it has two operands: `{start,+,step}`).
- `s.nuw`, `s.nsw`, `s.nw`: the no-wrap flags of an n-ary expression (`SCEVAddExpr`,
  `SCEVMulExpr`, `SCEVAddRecExpr`, and the minima and maxima): whether it is known not to
  wrap in the unsigned or signed sense, and (for an add recurrence) whether it is known to
  never wrap back to its start value (no self-wrap). These are facts that scalar evolution
  proved or derived from the IR, which hold in the context of the expression.
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

start(s::SCEVAddRecExpr) = first(operands(s))
affine(s::SCEVAddRecExpr) = API.LLVMExtraSCEVGetOperands(s, C_NULL) == 2

const SCEVNAryExpr = Union{SCEVAddExpr,SCEVMulExpr,SCEVAddRecExpr,SCEVUMaxExpr,SCEVSMaxExpr,
                           SCEVUMinExpr,SCEVSMinExpr,SCEVSequentialUMinExpr}

# the values of SCEV::NoWrapFlags
nw(s::SCEVNAryExpr) = API.LLVMExtraSCEVGetNoWrapFlags(s) & 1 != 0
nuw(s::SCEVNAryExpr) = API.LLVMExtraSCEVGetNoWrapFlags(s) & 2 != 0
nsw(s::SCEVNAryExpr) = API.LLVMExtraSCEVGetNoWrapFlags(s) & 4 != 0

@property SCEV type
@property SCEV operands
@property Union{SCEVConstant,SCEVUnknown} value
@property SCEVAddRecExpr loop
@property SCEVAddRecExpr start
@property SCEVAddRecExpr affine
@property SCEVNAryExpr nw
@property SCEVNAryExpr nuw
@property SCEVNAryExpr nsw

function Base.show(io::IO, s::SCEV)
    str = unsafe_message(API.LLVMExtraPrintSCEVToString(s))
    print(io, nameof(typeof(s)), "(", str, ")")
end

"""
    is_scevable(se::ScalarEvolution, T::LLVMType)
    is_scevable(se::ScalarEvolution, v::Value)

Check whether scalar evolution can analyze values of type `T`, or the value `v`, i.e.,
whether `se[v]` is supported: integers and pointers are.
"""
is_scevable(se::ScalarEvolution, T::LLVMType) =
    API.LLVMExtraScalarEvolutionIsSCEVable(se, T) |> Bool
is_scevable(se::ScalarEvolution, v::Value) = is_scevable(se, value_type(v))

function Base.getindex(se::ScalarEvolution, v::Value)
    is_scevable(se, v) ||
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
    scev_mul(se::ScalarEvolution, operands::SCEV...)

Build the (simplified) expression for the product of the `operands`, which must be integer
expressions of the same type.
"""
function scev_mul(se::ScalarEvolution, operands::SCEV...)
    isempty(operands) && throw(ArgumentError("Cannot multiply zero expressions"))
    refs = API.LLVMSCEVRef[s.ref for s in operands]
    ref = API.LLVMExtraScalarEvolutionGetMulExpr(se, refs, length(refs))
    ref == C_NULL && throw(ArgumentError("Cannot multiply expressions of incompatible types"))
    return SCEV(ref)
end

"""
    scev_cast(se::ScalarEvolution, opcode, s::SCEV, T::IntegerType)

Build the (simplified) expression for the cast `opcode` of the integer expression `s` to
the integer type `T`: `LLVM.Opcode.Trunc` to a narrower type, or `LLVM.Opcode.ZExt` or
`LLVM.Opcode.SExt` to a wider one.
"""
function scev_cast(se::ScalarEvolution, opcode::API.LLVMOpcode, s::SCEV, T::IntegerType)
    ref = API.LLVMExtraScalarEvolutionGetCastExpr(se, opcode, s, T)
    ref == C_NULL &&
        throw(ArgumentError("Invalid $opcode of $s to $T"))
    return SCEV(ref)
end

"""
    is_loop_invariant(loop::Loop, v::Value)
    is_loop_invariant(se::ScalarEvolution, loop::Loop, s::SCEV)

Check whether the value `v` is defined outside of `loop` (a constant, an argument, or an
instruction in a block that is not part of the loop), or whether the value of the
expression `s` does not change during the execution of `loop`. The former does not imply
that an instruction using `v` can be moved out of the loop, which may not be safe.
"""
is_loop_invariant(loop::Loop, v::Value) = API.LLVMExtraLoopIsLoopInvariant(loop, v) |> Bool

@doc (@doc is_loop_invariant)
is_loop_invariant(se::ScalarEvolution, loop::Loop, s::SCEV) =
    API.LLVMExtraScalarEvolutionIsLoopInvariant(se, s, loop) |> Bool

"""
    backedge_taken_count(se::ScalarEvolution, loop::Loop; kind=:exact)

The number of times that the backedge of `loop` is taken before the loop exits, which is
the number of iterations minus one, as an expression that is evaluated before the loop is
entered. It is a `SCEVCouldNotCompute` if it is not known. Use `kind=:constant_max` for
an upper bound that is a constant (as a `SCEVConstant`), or `kind=:symbolic_max` for an
upper bound that is an expression.

The count only describes the iterations of the loop when it is entered, and not exited
abnormally (e.g., by throwing an exception).
"""
function backedge_taken_count(se::ScalarEvolution, loop::Loop; kind::Symbol=:exact)
    api_kind = if kind === :exact
        API.LLVMExtraExactBackedgeTakenCount
    elseif kind === :constant_max
        API.LLVMExtraConstantMaxBackedgeTakenCount
    elseif kind === :symbolic_max
        API.LLVMExtraSymbolicMaxBackedgeTakenCount
    else
        throw(ArgumentError("Invalid kind :$kind; expected :exact, :constant_max or :symbolic_max"))
    end
    return SCEV(API.LLVMExtraScalarEvolutionGetBackedgeTakenCount(se, loop, api_kind))
end

"""
    step_recurrence(se::ScalarEvolution, s::SCEVAddRecExpr)

The step of the add recurrence `s`, i.e., the value that is added to it in each iteration
of its loop. For an affine recurrence `{start,+,step}` this is `step`, while for other
recurrences it is itself an add recurrence (e.g., `{a,+,b,+,c}` has the step `{b,+,c}`).
"""
step_recurrence(se::ScalarEvolution, s::SCEVAddRecExpr) =
    SCEV(API.LLVMExtraSCEVAddRecGetStepRecurrence(se, s))

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


## constant folding and simplification

@vocabulary IR constant_fold_cast, simplify_instruction

"""
    constant_fold_cast(opcode, c::Constant, T::LLVMType, dl::DataLayout)

Fold the cast `opcode` (e.g., `LLVM.Opcode.Trunc`) of the constant `c` to type `T`, using
LLVM's constant folder and the data layout `dl`. Returns the resulting constant, which can
be a constant expression, or `nothing` if the cast cannot be folded (e.g., since LLVM 18,
for some casts that would need to be constant expressions, which LLVM no longer supports).
Throws an `ArgumentError` if the cast is invalid.
"""
function constant_fold_cast(opcode::API.LLVMOpcode, c::Constant, T::LLVMType,
                            dl::DataLayout)
    result = Ref{API.LLVMValueRef}()
    API.LLVMExtraConstantFoldCastOperand(opcode, c, T, dl, result) |> Bool ||
        throw(ArgumentError("Invalid cast $opcode of $c to $T"))
    return result[] == C_NULL ? nothing : Value(result[])
end

"""
    simplify_instruction(inst::Instruction)

Simplify an instruction, like the `instsimplify` pass: returns a simpler value that the
instruction is equivalent to (e.g., an operand, or a constant), or `nothing` if there is
none, e.g., `add i32 %x, 0` simplifies to `%x`, and `and i1 %c, false` to `false`. This
does not change the IR: to fold the instruction, replace its uses by the returned value
(`replace_uses!`), and erase it. The instruction must be part of a function in a module,
whose data layout is used.

This uses LLVM's `simplifyInstruction`, which also implements the `InstSimplifyFolder`
that C++ code can use to simplify instructions as an `IRBuilder` creates them.
"""
function simplify_instruction(inst::Instruction)
    bb = API.LLVMGetInstructionParent(check_attached(inst))
    f = API.LLVMGetBasicBlockParent(bb)
    (f == C_NULL || API.LLVMGetGlobalParent(f) == C_NULL) &&
        throw(ArgumentError("Instruction is not part of a function in a module"))
    ref = API.LLVMExtraSimplifyInstruction(inst)
    return ref == C_NULL ? nothing : Value(ref)
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
    erase_trivially_dead!(insts::AbstractVector{<:Instruction})

If the instruction is trivially dead (see [`is_trivially_dead`](@ref)), erase it, together
with its operands that become trivially dead as a result, recursively. Returns whether the
instruction was erased.

Given a vector of instructions, erase those that are trivially dead in the same way,
skipping the others, and returns whether any instruction was erased. Unlike erasing them
one by one, this handles instructions that are erased because they are an operand of
another instruction in the vector.

Other wrappers of the erased instructions (e.g., of the operands of `inst`) must not be used
anymore.
"""
erase_trivially_dead!(inst::Instruction) =
    API.LLVMExtraRecursivelyDeleteTriviallyDeadInstructions(inst) |> Bool
function erase_trivially_dead!(insts::AbstractVector{<:Instruction})
    refs = API.LLVMValueRef[propref(inst) for inst in insts]
    return API.LLVMExtraRecursivelyDeleteTriviallyDeadInstructionsPermissive(
        refs, length(refs)) |> Bool
end


## promotion to registers

@vocabulary IR is_promotable, promote_mem_to_reg!

"""
    is_promotable(alloca::AllocaInst)

Check whether [`promote_mem_to_reg!`](@ref) can replace the alloca by SSA values: it is only
used by non-volatile loads and stores of the allocated value, and by lifetime markers and
debug information.
"""
is_promotable(alloca::AllocaInst) = API.LLVMExtraIsAllocaPromotable(alloca) |> Bool

"""
    promote_mem_to_reg!(alloca::AllocaInst, domtree::DomTree; [assumptions::AssumptionCache])
    promote_mem_to_reg!(allocas::AbstractVector{<:AllocaInst}, domtree::DomTree;
                        [assumptions::AssumptionCache])

Promote allocas to SSA values, inserting phi nodes where needed, like the `mem2reg` pass
does for every promotable alloca of a function. The allocas must be promotable (see
[`is_promotable`](@ref)) and part of the same function, and `domtree` must be an up-to-date
dominator tree of that function, e.g., `am[DomTree]` in a custom pass. If given,
`assumptions` must be the assumption cache of that function. Throws an `ArgumentError`,
without changing anything, if these requirements are not met, except for whether the tree is
up to date and whether `assumptions` is one of that function, which are not checked.

The CFG is not changed, so `domtree` remains valid. The allocas are erased, together with
the instructions that use them (loads, stores, lifetime markers and the casts and GEPs they
use, debug intrinsics): their wrappers must not be used anymore.
"""
promote_mem_to_reg!(alloca::AllocaInst, domtree::DomTree; kwargs...) =
    promote_mem_to_reg!([alloca], domtree; kwargs...)
function promote_mem_to_reg!(allocas::AbstractVector{<:AllocaInst}, domtree::DomTree;
                             assumptions::Union{Nothing,AssumptionCache}=nothing)
    refs = API.LLVMValueRef[propref(alloca) for alloca in allocas]
    allunique(refs) || throw(ArgumentError("Cannot promote an alloca more than once"))
    fn = C_NULL
    for (ref, alloca) in zip(refs, allocas)
        bb = API.LLVMGetInstructionParent(ref)
        f = bb == C_NULL ? C_NULL : API.LLVMGetBasicBlockParent(bb)
        f == C_NULL && throw(ArgumentError("Alloca is not part of a function"))
        fn == C_NULL && (fn = f)
        f == fn ||
            throw(ArgumentError("Cannot promote allocas of different functions together"))
        is_promotable(alloca) ||
            throw(ArgumentError("Alloca is not promotable (see `is_promotable`)"))
    end
    API.LLVMExtraPromoteMemToReg(refs, length(refs), domtree,
                                 something(assumptions, C_NULL)) |> Bool ||
        throw(ArgumentError("The dominator tree is not one of the function containing the allocas"))
    return
end
