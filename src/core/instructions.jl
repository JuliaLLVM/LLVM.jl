@vocabulary IR Instruction, remove!, erase!

"""
    Instruction

An instruction in the LLVM IR.

# Properties

    inst.parent

The basic block that contains the instruction, or `nothing` if the instruction is not part
of a basic block.

    inst.opcode

The opcode of the instruction, e.g., `LLVM.Opcode.Add`.

    inst.metadata
    gv.metadata

The metadata attached to an instruction or a global object (a function or global variable),
as a dictionary-like view that maps the kind of metadata to a metadata node. The kind can be
an `MDKind`, like `LLVM.MD_dbg`, or the name of the kind, like `"tbaa"`. The view can be
iterated (in the case of an instruction, this includes its debug location), and is mutable:
assign to a kind to attach metadata, e.g., `inst.metadata["tbaa"] = node`, and use `delete!`
to remove it.

    inst.debug_records

The debug records attached to the instruction, i.e., the `#dbg_declare`, `#dbg_value`,
`#dbg_assign` and `#dbg_label` records that are printed right before it, as a read-only view
that can be iterated. Requires LLVM 19+.

The records can be inspected using their properties, like `record.kind`; see
`LLVM.DbgRecord` for the full list.

    inst.debug_location
    inst.debug_location = loc::Union{DILocation,Nothing}

The debug location attached to the instruction, or `nothing` if it has none. Assigning
`nothing` removes the debug location.

    inst.next
    inst.prev

The next or previous instruction in the basic block, or `nothing` if there is none (or if
the instruction is not part of a basic block).

    cmp.predicate

The comparison predicate of an integer or floating-point comparison instruction, e.g.,
`LLVM.IntPredicate.EQ` or `LLVM.RealPredicate.OLT`.

    phi.incoming

The incoming values of the phi node, as a view of `(value, block)` tuples of the incoming
value and the block it originates from. The view is mutable: incoming values can be added
using `push!` or `append!`.

    alloca.allocated_type

The type that an `alloca` instruction allocates memory for.

    gep.pointer_operand

The pointer that a `getelementptr` instruction indexes into.

    gep.source_element_type

The type that a `getelementptr` instruction indexes into, as passed to the builder.

    gep.indices

The indices of a `getelementptr` instruction, i.e., its operands after the pointer, as a
view of those operands: assigning an element replaces the operand.

    gep.inbounds
    gep.inbounds = flag::Bool

Whether a `getelementptr` instruction is `inbounds`, i.e., whether the resulting pointer is
known to be within the bounds of the object that the pointer operand is based on.

    inst.indices

The indices of an `extractvalue` or `insertvalue` instruction, as a read-only view of
integers. These
are the zero-based indices that select the element of the aggregate, like in textual IR,
e.g., `[1, 0]` for `extractvalue {i32, {i8, i8}} %agg, 1, 0`.

    or.disjoint
    or.disjoint = flag::Bool

Whether an `or` instruction has the `disjoint` flag, which makes the result poison if both
operands have a bit set in the same position. Requires LLVM 18+.

    icmp.samesign
    icmp.samesign = flag::Bool

Whether an `icmp` instruction has the `samesign` flag, which makes the result poison if the
operands have different signs. Requires LLVM 20+.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
Instruction
# forward definition of Instruction in src/core/value/constant.jl

register(Instruction, API.LLVMInstructionValueKind)

const instruction_opcodes = Vector{Type}(fill(Nothing, typemax(API.LLVMOpcode)+1))
function identify(::Type{Instruction}, ref::API.LLVMValueRef)
    opcode = API.LLVMGetInstructionOpcode(ref)
    typ = @inbounds instruction_opcodes[opcode+1]
    typ === Nothing && error("Unknown type opcode $opcode")
    return typ
end
Base.@nospecializeinfer function register(@nospecialize(T::Type{<:Instruction}),
                                          opcode::API.LLVMOpcode)
    check_layout(T, API.LLVMValueRef)
    instruction_opcodes[opcode+1] = T
end

function refcheck(::Type{T}, ref::API.LLVMValueRef) where T<:Instruction
    ref==C_NULL && throw(UndefRefError())
    if typecheck_enabled
        T′ = identify(Instruction, ref)
        if T != T′
            error("invalid conversion of $T′ instruction reference to $T")
        end
    end
end

# Construct a concretely typed instruction object from an abstract value ref
function Instruction(ref::API.LLVMValueRef)
    ref == C_NULL && throw(UndefRefError())
    T = identify(Instruction, ref)
    return unsafe_wrap_ref(T, ref)::Instruction
end

"""
    copy(inst::Instruction)

Create a copy of the given instruction.
"""
Base.copy(inst::Instruction) = Instruction(API.LLVMInstructionClone(inst))

"""
    remove!(inst::Instruction)

Remove the given instruction from the containing basic block, but do not delete the object.
"""
remove!(inst::Instruction) = API.LLVMInstructionRemoveFromParent(inst)

"""
    erase!(inst::Instruction)

Remove the given instruction from the containing basic block, if any, and delete the
object.

!!! warning

    This function is unsafe because it does not check if the instruction is used elsewhere.
"""
function erase!(inst::Instruction)
    if API.LLVMGetInstructionParent(inst) == C_NULL
        # e.g., a copy, or an instruction that was removed from its block
        API.LLVMDeleteInstruction(inst)
    else
        API.LLVMInstructionEraseFromParent(inst)
    end
end

@vocabulary IR comes_before, may_read_from_memory, may_write_to_memory,
               may_have_side_effects

function check_attached(inst::Instruction)
    API.LLVMGetInstructionParent(inst) == C_NULL &&
        throw(ArgumentError("Instruction is not part of a basic block"))
    return inst
end

"""
    comes_before(a::Instruction, b::Instruction)

Check whether instruction `a` comes before `b`, which should be part of the same basic
block. An instruction does not come before itself.
"""
function comes_before(a::Instruction, b::Instruction)
    bb = API.LLVMGetInstructionParent(check_attached(a))
    bb == API.LLVMGetInstructionParent(check_attached(b)) ||
        throw(ArgumentError("Instructions are not part of the same basic block"))
    API.LLVMExtraInstructionComesBefore(a, b) |> Bool
end

"""
    may_read_from_memory(inst::Instruction)

Check whether the given instruction may read from memory. This is a conservative check,
e.g., calls may access memory unless their memory effects say otherwise, and ordered
stores are considered to also read memory.
"""
may_read_from_memory(inst::Instruction) = API.LLVMExtraMayReadFromMemory(inst) |> Bool

"""
    may_write_to_memory(inst::Instruction)

Check whether the given instruction may write to memory. This is a conservative check,
e.g., calls may access memory unless their memory effects say otherwise, and ordered loads
are considered to also write memory.
"""
may_write_to_memory(inst::Instruction) = API.LLVMExtraMayWriteToMemory(inst) |> Bool

"""
    may_have_side_effects(inst::Instruction)

Check whether the given instruction may have side effects: whether it may write to memory,
unwind, or not return.
"""
may_have_side_effects(inst::Instruction) = API.LLVMExtraMayHaveSideEffects(inst) |> Bool

function parent(inst::Instruction)
    ref = API.LLVMGetInstructionParent(inst)
    ref == C_NULL && return nothing
    BasicBlock(ref)
end

@property Instruction parent

opcode(inst::Instruction) = API.LLVMGetInstructionOpcode(inst)

@property Instruction opcode

# strip unnecessary whitespace
Base.show(io::IO, ::MIME"text/plain", inst::Instruction) = print(io, lstrip(string(inst)))

# instructions are typically only a single line, so always display them in full
Base.show(io::IO, inst::Instruction) = print(io, typeof(inst), "(", lstrip(string(inst)), ")")


## instruction types

const opcodes = [:Ret, :Br, :Switch, :IndirectBr, :Invoke, :Unreachable, :CallBr, :FNeg,
                 :Add, :FAdd, :Sub, :FSub, :Mul, :FMul, :UDiv, :SDiv, :FDiv, :URem, :SRem,
                 :FRem, :Shl, :LShr, :AShr, :And, :Or, :Xor, :Alloca, :Load, :Store,
                 :GetElementPtr, :Trunc, :ZExt, :SExt, :FPToUI, :FPToSI, :UIToFP, :SIToFP,
                 :FPTrunc, :FPExt, :PtrToInt, :IntToPtr, :BitCast, :AddrSpaceCast, :ICmp,
                 :FCmp, :PHI, :Call, :Select, :UserOp1, :UserOp2, :VAArg, :ExtractElement,
                 :InsertElement, :ShuffleVector, :ExtractValue, :InsertValue, :Freeze,
                 :Fence, :AtomicCmpXchg, :AtomicRMW, :Resume, :LandingPad, :CleanupRet,
                 :CatchRet, :CatchPad, :CleanupPad, :CatchSwitch]

if version() >= v"22"
    push!(opcodes, :PtrToAddr)
end

for op in opcodes
    typename = Symbol(op, :Inst)
    enum = Symbol(:LLVM, op)
    # the instruction's name in textual IR
    irname = op === :VAArg ? "va_arg" : op === :AtomicCmpXchg ? "cmpxchg" :
             op === :AtomicRMW ? "atomicrmw" : lowercase(string(op))
    doc = if op in (:UserOp1, :UserOp2)
        """
            LLVM.$typename <: Instruction

        An instruction that only exists internally, while some of LLVM's passes run.
        """
    else
        """
            LLVM.$typename <: Instruction

        The type of `$irname` instructions.
        """
    end
    @eval begin
        @checked struct $typename <: Instruction
            ref::API.LLVMValueRef
        end
        register($typename, API.$enum)
        @doc $doc $typename
    end
end
@eval @vocabulary IR $(Expr(:tuple, (Symbol(op, :Inst) for op in opcodes)...))


## comparisons

predicate(inst::ICmpInst) = API.LLVMGetICmpPredicate(inst)
predicate(inst::FCmpInst) = API.LLVMGetFCmpPredicate(inst)

@property Union{ICmpInst,FCmpInst} predicate


## atomics

@vocabulary IR isatomic, SyncScope,
               is_stronger, is_acquire_or_stronger, is_release_or_stronger, merged_ordering,
               strongest_failure_ordering, mmra!, copy_atomic_metadata!

@vocabulary IR AtomicInst

"""
    LLVM.AtomicInst

The group of instructions that can be atomic: `load`, `store`, `fence`, `atomicrmw` and
`cmpxchg`.

# Properties

    inst.ordering
    inst.ordering = ordering::LLVM.AtomicOrdering.T

The atomic ordering of a load, store, fence or `atomicrmw` instruction. Reading it
requires the instruction to be atomic, while assigning an ordering to a load or store makes
it atomic. `cmpxchg` instructions have separate `success_ordering` and `failure_ordering`
properties instead, which can be combined using [`merged_ordering`](@ref).

    inst.syncscope
    inst.syncscope = scope::Union{SyncScope,AbstractString,Symbol}

The synchronization scope of an atomic load, store, fence, `atomicrmw` or `cmpxchg`
instruction, which belongs to the context of the instruction. A scope can be assigned by
name (e.g., `inst.syncscope = "agent"`), which is looked up in the instruction's context.

    rmw.binop

The binary operation of an atomic read-modify-write instruction, e.g.,
`LLVM.AtomicRMWBinOp.Add`.

    cmpxchg.weak
    cmpxchg.weak = flag::Bool

Whether an atomic compare-and-exchange instruction is weak, i.e., whether it is allowed to
fail spuriously, even if the comparison succeeds.

    cmpxchg.success_ordering
    cmpxchg.success_ordering = ordering::LLVM.AtomicOrdering.T

The ordering of an atomic compare-and-exchange instruction when the comparison succeeds.

    cmpxchg.failure_ordering
    cmpxchg.failure_ordering = ordering::LLVM.AtomicOrdering.T

The ordering of an atomic compare-and-exchange instruction when the comparison fails.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const AtomicInst = Union{LoadInst, StoreInst, FenceInst, AtomicRMWInst, AtomicCmpXchgInst}

"""
    isatomic(inst::Instruction)

Check if the given instruction is atomic. This includes atomic operations such as
`atomicrmw` or `fence`, but also loads and stores that have been made atomic by setting an
atomic ordering.
"""
isatomic(inst::Instruction) = API.LLVMIsAtomic(inst) |> Bool

function ordering(inst::AtomicInst)
    isatomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    @static if version() < v"18"
        API.LLVMExtraGetOrdering(inst)
    else
        API.LLVMGetOrdering(inst)
    end
end
ordering(::AtomicCmpXchgInst) =
    throw(ArgumentError("cmpxchg instructions have a success and a failure ordering"))

function ordering!(inst::AtomicInst, ord::API.LLVMAtomicOrdering)
    # loads and stores can be made atomic by setting an ordering, but LLVM asserts when
    # setting an invalid ordering on other instructions
    if inst isa AtomicRMWInst
        is_stronger(ord, API.LLVMAtomicOrderingUnordered) ||
            throw(ArgumentError("atomicrmw requires an ordering of at least monotonic, got $(msgname(ord))"))
    elseif inst isa FenceInst
        check_fence_ordering(ord)
    end
    @static if version() < v"18"
        API.LLVMExtraSetOrdering(inst, ord)
    else
        API.LLVMSetOrdering(inst, ord)
    end
end
ordering!(::AtomicCmpXchgInst, ::API.LLVMAtomicOrdering) =
    throw(ArgumentError("cmpxchg instructions have a success and a failure ordering"))

# cmpxchg instructions have separate success and failure orderings
@property Union{LoadInst,StoreInst,FenceInst,AtomicRMWInst} ordering ordering!

check_fence_ordering(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingAcquire || is_release_or_stronger(o) ||
        throw(ArgumentError("Fences must have acquire, release, acq_rel or seq_cst ordering, got $(msgname(o))"))

# the names LLVM uses in IR, and Julia's names for the orderings that differ.
#
# these tables are built when the package is defined, so their keys and values are untyped
# (with type assertions where they are looked up): a dictionary that's typed on the enums
# would get its methods compiled for these types, which ends up in the package image and
# makes loading the package slower, while untyped ones use the methods in Julia's system
# image.
const ORDERING_NAMES = Dict{String,Any}(
    "not_atomic" => API.LLVMAtomicOrderingNotAtomic,
    "unordered" => API.LLVMAtomicOrderingUnordered,
    "monotonic" => API.LLVMAtomicOrderingMonotonic,
    "acquire" => API.LLVMAtomicOrderingAcquire,
    "release" => API.LLVMAtomicOrderingRelease,
    "acq_rel" => API.LLVMAtomicOrderingAcquireRelease,
    "acquire_release" => API.LLVMAtomicOrderingAcquireRelease,
    "seq_cst" => API.LLVMAtomicOrderingSequentiallyConsistent,
    "sequentially_consistent" => API.LLVMAtomicOrderingSequentiallyConsistent)

"""
    parse(LLVM.AtomicOrdering.T, name::AbstractString)

Get the atomic ordering with the given name, as used in LLVM IR (e.g. `"acq_rel"`), or as
used by Julia's atomics (e.g. `"acquire_release"`). Throws an `ArgumentError` for unknown
names; see
[`tryparse`](@ref tryparse(::Type{LLVM.API.LLVMAtomicOrdering}, ::AbstractString)) for a
version that returns `nothing` instead.
"""
function Base.parse(::Type{API.LLVMAtomicOrdering}, name::AbstractString)
    ord = tryparse(API.LLVMAtomicOrdering, name)
    ord === nothing && throw(ArgumentError("Unknown atomic ordering \"$name\""))
    return ord
end

"""
    tryparse(LLVM.AtomicOrdering.T, name::AbstractString)

Get the atomic ordering with the given name, like
[`parse`](@ref parse(::Type{LLVM.API.LLVMAtomicOrdering}, ::AbstractString)), but return
`nothing` if the name is unknown.
"""
Base.tryparse(::Type{API.LLVMAtomicOrdering}, name::AbstractString) =
    get(ORDERING_NAMES, name, nothing)::Union{Nothing,API.LLVMAtomicOrdering}

const RMW_BINOP_NAMES = Dict{String,Any}(
    "xchg" => API.LLVMAtomicRMWBinOpXchg, "add" => API.LLVMAtomicRMWBinOpAdd,
    "sub" => API.LLVMAtomicRMWBinOpSub, "and" => API.LLVMAtomicRMWBinOpAnd,
    "nand" => API.LLVMAtomicRMWBinOpNand, "or" => API.LLVMAtomicRMWBinOpOr,
    "xor" => API.LLVMAtomicRMWBinOpXor, "max" => API.LLVMAtomicRMWBinOpMax,
    "min" => API.LLVMAtomicRMWBinOpMin, "umax" => API.LLVMAtomicRMWBinOpUMax,
    "umin" => API.LLVMAtomicRMWBinOpUMin, "fadd" => API.LLVMAtomicRMWBinOpFAdd,
    "fsub" => API.LLVMAtomicRMWBinOpFSub, "fmax" => API.LLVMAtomicRMWBinOpFMax,
    "fmin" => API.LLVMAtomicRMWBinOpFMin, "uinc_wrap" => API.LLVMAtomicRMWBinOpUIncWrap,
    "udec_wrap" => API.LLVMAtomicRMWBinOpUDecWrap,
    "usub_cond" => API.LLVMAtomicRMWBinOpUSubCond,
    "usub_sat" => API.LLVMAtomicRMWBinOpUSubSat,
    "fmaximum" => API.LLVMAtomicRMWBinOpFMaximum,
    "fminimum" => API.LLVMAtomicRMWBinOpFMinimum,
    "fmaximumnum" => API.LLVMAtomicRMWBinOpFMaximumNum,
    "fminimumnum" => API.LLVMAtomicRMWBinOpFMinimumNum)

"""
    parse(LLVM.AtomicRMWBinOp.T, name::AbstractString)

Get the `atomicrmw` operation with the given name, as used in LLVM IR (e.g. `"uinc_wrap"`).
This works for every operation, whether or not the version of LLVM in use supports it (see
[`LLVM.isavailable`](@ref)). Throws an `ArgumentError` for unknown names; see
[`tryparse`](@ref tryparse(::Type{LLVM.API.LLVMAtomicRMWBinOp}, ::AbstractString)) for a
version that returns `nothing` instead.
"""
function Base.parse(::Type{API.LLVMAtomicRMWBinOp}, name::AbstractString)
    op = tryparse(API.LLVMAtomicRMWBinOp, name)
    op === nothing && throw(ArgumentError("Unknown atomicrmw operation \"$name\""))
    return op
end

"""
    tryparse(LLVM.AtomicRMWBinOp.T, name::AbstractString)

Get the `atomicrmw` operation with the given name, like
[`parse`](@ref parse(::Type{LLVM.API.LLVMAtomicRMWBinOp}, ::AbstractString)), but return
`nothing` if the name is unknown. As with `parse`, this recognizes every operation, whether
or not the version of LLVM in use supports it; use [`LLVM.isavailable`](@ref) to check
that.
"""
Base.tryparse(::Type{API.LLVMAtomicRMWBinOp}, name::AbstractString) =
    get(RMW_BINOP_NAMES, name, nothing)::Union{Nothing,API.LLVMAtomicRMWBinOp}

@public irname

const RMW_BINOP_IRNAMES = let irnames = Dict{Any,String}()
    for (name, op) in RMW_BINOP_NAMES
        irnames[op] = name
    end
    irnames
end
const ORDERING_IRNAMES = Dict{Any,String}(
    API.LLVMAtomicOrderingNotAtomic => "not_atomic",
    API.LLVMAtomicOrderingUnordered => "unordered",
    API.LLVMAtomicOrderingMonotonic => "monotonic",
    API.LLVMAtomicOrderingAcquire => "acquire",
    API.LLVMAtomicOrderingRelease => "release",
    API.LLVMAtomicOrderingAcquireRelease => "acq_rel",
    API.LLVMAtomicOrderingSequentiallyConsistent => "seq_cst")

"""
    LLVM.irname(op::LLVM.AtomicRMWBinOp.T)
    LLVM.irname(ordering::LLVM.AtomicOrdering.T)

Get the name of an `atomicrmw` operation or an atomic ordering as used in LLVM IR, e.g.,
`"uinc_wrap"` or `"acq_rel"`. This is the inverse of `parse`, and works for every
operation, whether or not the version of LLVM in use supports it (see
[`LLVM.isavailable`](@ref)).
"""
irname(op::API.LLVMAtomicRMWBinOp) = RMW_BINOP_IRNAMES[op]
irname(ordering::API.LLVMAtomicOrdering) = ORDERING_IRNAMES[ordering]

# for error messages: the IR name, or the integer of an invalid value, which has no name
msgname(op::API.LLVMAtomicRMWBinOp) = get(RMW_BINOP_IRNAMES, op, Integer(op))
msgname(ordering::API.LLVMAtomicOrdering) = get(ORDERING_IRNAMES, ordering, Integer(ordering))

@public isfloatingpoint

"""
    LLVM.isfloatingpoint(op::LLVM.AtomicRMWBinOp.T)

Check whether `op` is a floating-point `atomicrmw` operation, such as `fadd` or `fmax`.
These operations require floating-point values. The others require integers, except
`xchg`, which also applies to floating-point and pointer values. Vectors of these types
follow the same rules. This works for every operation, whether or not the version of LLVM
in use supports it (see [`LLVM.isavailable`](@ref)).

This corresponds to `AtomicRMWInst::isFPOperation` in LLVM's C++ API.
"""
isfloatingpoint(op::API.LLVMAtomicRMWBinOp) =
    op in (API.LLVMAtomicRMWBinOpFAdd, API.LLVMAtomicRMWBinOpFSub,
           API.LLVMAtomicRMWBinOpFMax, API.LLVMAtomicRMWBinOpFMin,
           API.LLVMAtomicRMWBinOpFMaximum, API.LLVMAtomicRMWBinOpFMinimum,
           API.LLVMAtomicRMWBinOpFMaximumNum, API.LLVMAtomicRMWBinOpFMinimumNum)

# the lattice of orderings, from llvm/Support/AtomicOrdering.h
const ORDERING_LATTICE = let
    NA, UN, MO = API.LLVMAtomicOrderingNotAtomic, API.LLVMAtomicOrderingUnordered,
                 API.LLVMAtomicOrderingMonotonic
    AC, RE, AR = API.LLVMAtomicOrderingAcquire, API.LLVMAtomicOrderingRelease,
                 API.LLVMAtomicOrderingAcquireRelease
    SC = API.LLVMAtomicOrderingSequentiallyConsistent
    # each ordering, and the ones it is strictly stronger than
    Dict{Any,Any}(NA => (), UN => (NA,), MO => (NA, UN), AC => (NA, UN, MO),
                  RE => (NA, UN, MO), AR => (NA, UN, MO, AC, RE),
                  SC => (NA, UN, MO, AC, RE, AR))
end

"""
    is_stronger(a::LLVM.AtomicOrdering.T, b::LLVM.AtomicOrdering.T)

Check whether ordering `a` is strictly stronger than `b`. Orderings are only partially
ordered: `acquire` and `release` are incomparable, and both are weaker than `acq_rel`.
"""
is_stronger(a::API.LLVMAtomicOrdering, b::API.LLVMAtomicOrdering) =
    b in ORDERING_LATTICE[a]::Tuple

"""
    is_acquire_or_stronger(ordering::LLVM.AtomicOrdering.T)

Check whether an ordering has acquire semantics: `acquire`, `acq_rel` or `seq_cst`.
"""
is_acquire_or_stronger(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingAcquire || is_stronger(o, API.LLVMAtomicOrderingAcquire)

"""
    is_release_or_stronger(ordering::LLVM.AtomicOrdering.T)

Check whether an ordering has release semantics: `release`, `acq_rel` or `seq_cst`.
"""
is_release_or_stronger(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingRelease || is_stronger(o, API.LLVMAtomicOrderingRelease)

"""
    merged_ordering(a::LLVM.AtomicOrdering.T, b::LLVM.AtomicOrdering.T)
    merged_ordering(inst::AtomicCmpXchgInst)

Get the weakest ordering that is at least as strong as both `a` and `b`, e.g., to perform
an operation that needs the guarantees of both. For a `cmpxchg` instruction, this merges
its success and failure orderings.
"""
function merged_ordering(a::API.LLVMAtomicOrdering, b::API.LLVMAtomicOrdering)
    if (a == API.LLVMAtomicOrderingAcquire && b == API.LLVMAtomicOrderingRelease) ||
       (a == API.LLVMAtomicOrderingRelease && b == API.LLVMAtomicOrderingAcquire)
        return API.LLVMAtomicOrderingAcquireRelease
    end
    return is_stronger(a, b) ? a : b
end
merged_ordering(inst::AtomicCmpXchgInst) =
    merged_ordering(success_ordering(inst), failure_ordering(inst))

"""
    strongest_failure_ordering(success::LLVM.AtomicOrdering.T)

Get the strongest failure ordering that is valid for a `cmpxchg` with the given success
ordering, i.e., the success ordering without its release semantics. This is the
conventional choice of failure ordering (and the default of [`atomic_cmpxchg!`](@ref)),
but other combinations are valid too, e.g. `release` on success and `acquire` on failure.
"""
function strongest_failure_ordering(success::API.LLVMAtomicOrdering)
    if success == API.LLVMAtomicOrderingRelease || success == API.LLVMAtomicOrderingMonotonic
        API.LLVMAtomicOrderingMonotonic
    elseif success == API.LLVMAtomicOrderingAcquireRelease ||
           success == API.LLVMAtomicOrderingAcquire
        API.LLVMAtomicOrderingAcquire
    elseif success == API.LLVMAtomicOrderingSequentiallyConsistent
        API.LLVMAtomicOrderingSequentiallyConsistent
    else
        throw(ArgumentError("cmpxchg requires an ordering of at least monotonic, got $(msgname(success))"))
    end
end

"""
    SyncScope

A synchronization scope for atomic operations. Synchronization scopes belong to a context,
and can only be used with instructions of that context.

# Properties

    scope.name

The name of the synchronization scope.

    scope.context

The context that the synchronization scope belongs to. The scope doesn't keep it alive,
so it can only be used for as long as the context is.
"""
struct SyncScope
    id::Cuint
    context_ref::API.LLVMContextRef     # borrowed

    # scope IDs are specific to a context (except for the first ones, which are fixed)
    SyncScope(id::Integer, ctx::Context) = new(id, ctx.ref)
end
@properties SyncScope

"""
    SyncScope(name::AbstractString; context=context())

Get the synchronization scope with the given name in `context`, by default the active
context. This can be a well-known scope such as `"singlethread"` or `"system"`, or a
target-specific scope, e.g., `"agent"`. `"system"` is the default scope, which LLVM IR
doesn't spell out: an instruction in it has no `syncscope`.
"""
function SyncScope(name::AbstractString; context::Context=LLVM.context())
    # the default, system syncscope gets encoded as an empty string
    str = name == "system" ? "" : String(name)
    SyncScope(API.LLVMGetSyncScopeID(context, str, ncodeunits(str)), context)
end

context(scope::SyncScope) = Context(scope.context_ref)

@property SyncScope context

# the first scope IDs are fixed
function _name(scope::SyncScope)
    scope.id == 0 && return "singlethread"
    scope.id == 1 && return "system"
    len = Ref{Csize_t}()
    ptr = convert(Ptr{UInt8}, API.LLVMExtraGetSyncScopeName(context(scope), scope.id, len))
    ptr == C_NULL && return nothing
    return unsafe_string(ptr, len[])
end

function name(scope::SyncScope)
    str = _name(scope)
    str === nothing && throw(ArgumentError("Unknown synchronization scope $(scope.id)"))
    return str
end

@property SyncScope name

function Base.show(io::IO, scope::SyncScope)
    str = if scope.id <= 1 || isdefined(API, :libLLVMExtra)
        _name(scope)
    end
    if str === nothing
        print(io, "SyncScope(target-specific scope $(scope.id))")
    else
        print(io, "SyncScope(", repr(str), ")")
    end
end

function check_context(scope::SyncScope, ctx::Context)
    scope.context_ref == ctx.ref ||
        throw(ArgumentError("$scope belongs to another context; use `SyncScope(scope.name; context)` to get the scope with the same name in another context"))
    return scope
end

function syncscope(inst::AtomicInst)
    isatomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    SyncScope(API.LLVMGetAtomicSyncScopeID(inst), context(inst))
end

function syncscope!(inst::AtomicInst, scope::SyncScope)
    isatomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    check_context(scope, context(inst))
    API.LLVMSetAtomicSyncScopeID(inst, scope.id)
end
syncscope!(inst::AtomicInst, name::Union{AbstractString,Symbol}) =
    syncscope!(inst, SyncScope(String(name); context=context(inst)))

@property AtomicInst syncscope syncscope!

function binop(inst::AtomicRMWInst)
    @static if v"16" <= version() < v"19"
        API.LLVMAtomicRMWBinOp(API.LLVMExtraGetAtomicRMWBinOp(inst))
    else
        API.LLVMGetAtomicRMWBinOp(inst)
    end
end

@property AtomicRMWInst binop

# the LLVM version that introduced each atomicrmw operation, indexed by its C API value
const ATOMIC_RMW_BINOP_SINCE = (
    ntuple(_ -> v"0", 15)...,   # Xchg through FMin
    v"16", v"16",               # UIncWrap, UDecWrap
    v"20", v"20",               # USubCond, USubSat
    v"21", v"21",               # FMaximum, FMinimum
    v"23", v"23",               # FMaximumNum, FMinimumNum
)

"""
    isavailable(op::LLVM.AtomicRMWBinOp.T)

Check whether the atomic read-modify-write operation `op` is supported by the version of
LLVM in use. All operations can be named on every LLVM version, and are listed by
`instances(LLVM.AtomicRMWBinOp.T)`, but instructions can only be created with the ones
that are available, e.g., `filter(LLVM.isavailable, instances(LLVM.AtomicRMWBinOp.T))`.
"""
function isavailable(op::API.LLVMAtomicRMWBinOp)
    since = get(ATOMIC_RMW_BINOP_SINCE, Integer(op) + 1, nothing)
    since !== nothing && version() >= since
end
@public isavailable

weak(inst::AtomicCmpXchgInst) = API.LLVMGetWeak(inst) |> Bool

weak!(inst::AtomicCmpXchgInst, flag::Bool) = API.LLVMSetWeak(inst, flag)

@property AtomicCmpXchgInst weak weak!

@vocabulary IR MemAccessInst

"""
    LLVM.MemAccessInst

The group of instructions that access memory: `load`, `store`, `atomicrmw` and `cmpxchg`.

# Properties

    inst.volatile
    inst.volatile = flag::Bool

Whether a memory access (a `load`, `store`, `atomicrmw` or `cmpxchg` instruction) is
volatile.

    inst.pointer_operand

The pointer operand of a memory access, i.e., the address of the memory that it accesses.

    inst.value_operand

The value operand of a `store` or `atomicrmw` instruction, i.e., the value that is stored
or combined with the value in memory.

    cmpxchg.compare_operand
    cmpxchg.new_value_operand

The operands of a `cmpxchg` instruction: the value that the memory is compared with, and
the value that is stored if they are equal.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const MemAccessInst = Union{LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst}

volatile(inst::MemAccessInst) = API.LLVMGetVolatile(inst) |> Bool

volatile!(inst::MemAccessInst, flag::Bool) = API.LLVMSetVolatile(inst, flag)

@property MemAccessInst volatile volatile!

"""
    mmra!(inst::Instruction, tags::Pair{<:AbstractString,<:AbstractString}...)

Attach memory model relaxation annotations (`!mmra` metadata) to an instruction, replacing
any existing ones. Each tag is a `prefix => suffix` pair, e.g. `"amdgpu-as" => "local"`.
Without tags, the annotations are removed. MMRAs are supported by LLVM 19 and later;
older versions preserve the metadata but don't interpret it.
"""
function mmra!(inst::Instruction, tags::Pair{<:AbstractString,<:AbstractString}...)
    md = metadata(inst)
    if isempty(tags)
        delete!(md, "mmra")
    else
        nodes = [MDNode([MDString(String(k)), MDString(String(v))]) for (k, v) in tags]
        md["mmra"] = length(nodes) == 1 ? only(nodes) : MDNode(nodes)
    end
    return inst
end

# the metadata that AtomicExpand preserves when rewriting an atomic memory operation
const ATOMIC_METADATA = ["tbaa", "tbaa.struct", "alias.scope", "noalias",
                         "noalias.addrspace", "llvm.access.group", "mmra",
                         "amdgpu.no.remote.memory", "amdgpu.no.fine.grained.memory",
                         "amdgpu.ignore.denormal.mode"]

"""
    copy_atomic_metadata!(dest::Instruction, src::Instruction)

Copy the metadata of an atomic memory operation `src` that remains valid for `dest`, an
instruction that implements (part of) the same memory access, e.g., when expanding an
`atomicrmw` into a `cmpxchg` loop. This includes the debug location, aliasing information,
memory model relaxation annotations and target-specific atomic metadata, but not
metadata that describes the value, like `!range`. Metadata of `src` that `dest` already
has is overwritten.
"""
function copy_atomic_metadata!(dest::Instruction, src::Instruction)
    loc = debug_location(src)
    loc === nothing || debug_location!(dest, loc)
    src_md, dest_md = metadata(src), metadata(dest)
    for kind in ATOMIC_METADATA
        haskey(src_md, kind) && (dest_md[kind] = src_md[kind])
    end
    return dest
end

function success_ordering(inst::AtomicCmpXchgInst)
    API.LLVMGetCmpXchgSuccessOrdering(inst)
end

function success_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)
    API.LLVMSetCmpXchgSuccessOrdering(inst, ord)
end

@property AtomicCmpXchgInst success_ordering success_ordering!

function failure_ordering(inst::AtomicCmpXchgInst)
    API.LLVMGetCmpXchgFailureOrdering(inst)
end

function failure_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)
    API.LLVMSetCmpXchgFailureOrdering(inst, ord)
end

@property AtomicCmpXchgInst failure_ordering failure_ordering!


## call sites and invocations

# TODO: add this to the actual type hierarchy
@vocabulary IR CallBase

"""
    LLVM.CallBase

The group of call sites: `call`, `invoke` and `callbr` instructions, like LLVM's `CallBase`.

# Properties

    call.callconv
    call.callconv = cc

The calling convention of a `call`, `invoke` or `callbr` instruction, e.g.,
`LLVM.CallConv.Fast`.

    call.called_operand
    call.called_operand = callee::Value

The operand of a `call`, `invoke` or `callbr` instruction that represents the called
function. This can be any value, e.g., a function, a cast of one, or a function pointer.

Assigning to this property only replaces the callee: the function type of the call (its
`called_type`), arguments and attributes remain unchanged, so the new callee should be
callable using that function type.

    call.called_function

The function that a `call`, `invoke` or `callbr` instruction calls directly, or `nothing`
for other calls, e.g., of a function pointer. Like C++'s `CallBase::getCalledFunction`,
this does not look through casts or aliases (use `strip_pointer_casts` on the
`called_operand` for that), and is `nothing` if the function's type differs from the
function type of the call.

    call.called_type

The type of the function that is called by a `call`, `invoke` or `callbr` instruction.

    call.arguments

The arguments of a `call`, `invoke` or `callbr` instruction, as a view of the instruction's
operands (which also include, e.g., the called function). The view is mutable, so an
argument can be replaced by assigning to it: `call.arguments[i] = val`.

    call.function_attributes

The function attributes of a `call`, `invoke` or `callbr` instruction, as a mutable view
that can be iterated, indexed by attribute kind, and supports `push!`, `append!` and
`delete!`, like the `function_attributes` of a function. These are the attributes of the
call site, which do not include those of the called function.

See also the `return_attributes` and `argument_attributes` properties.

    call.memory_effects
    call.memory_effects = effects::Union{MemoryEffects,FunctionMemoryEffects}

The memory effects of a `call`, `invoke` or `callbr` instruction, as described by the
`memory` attribute of the call site, or `MemoryEffects(:readwrite)` if it doesn't have one.
Like the `function_attributes` of the call, this does not include the effects of the called
function. The effects are returned as a [`FunctionMemoryEffects`](@ref) view, like the
`memory_effects` of a function.

    call.argument_attributes

The attributes of the arguments of a `call`, `invoke` or `callbr` instruction, as a vector
with a view of the attributes of each argument. These views work like the
`function_attributes` of the call, e.g.,
`push!(call.argument_attributes[1], EnumAttribute("noundef"))`.

    call.return_attributes

The attributes of the return value of a `call`, `invoke` or `callbr` instruction, as a
mutable view that works like the `function_attributes` of the call.

    call.operand_bundles

The operand bundles attached to a `call`, `invoke` or `callbr` instruction, as a read-only
view. The bundles themselves are copies, which do not change along with the instruction.

    call.tailcall
    call.tailcall = flag::Bool

Whether a `call` instruction is marked as a tail call, i.e., whether it has the `tail` or
`musttail` marker. This is a view of the `tailcall_kind` property: assigning `true` to a
call that is not a tail call marks it `tail`, while assigning `false` to a tail call
removes the marker. Assigning the current value does not change the kind of tail call.

    call.tailcall_kind
    call.tailcall_kind = kind::LLVM.TailCallKind.T

The tail call marker of a `call` instruction: `LLVM.TailCallKind.None`,
`LLVM.TailCallKind.Tail` (`tail`), `LLVM.TailCallKind.MustTail` (`musttail`) or
`LLVM.TailCallKind.NoTail` (`notail`). See also the `tailcall` property.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const CallBase = Union{CallBrInst, CallInst, InvokeInst}

callconv(inst::CallBase) = API.LLVMGetInstructionCallConv(inst)

callconv!(inst::CallBase, cc) =
    API.LLVMSetInstructionCallConv(inst, cc)

@property CallBase callconv callconv!

tailcall(inst::CallInst) = API.LLVMIsTailCall(inst) |> Bool

# only change the kind when needed, so that marking a tail call as such does not demote a
# `musttail` call, and clearing the flag of a `notail` call keeps that marker (unlike
# LLVM's `setTailCall`)
function tailcall!(inst::CallInst, flag::Bool)
    flag == tailcall(inst) || API.LLVMSetTailCall(inst, flag)
    return
end

@property CallInst tailcall tailcall!

tailcall_kind(inst::CallInst) = API.LLVMGetTailCallKind(inst)

tailcall_kind!(inst::CallInst, kind::API.LLVMTailCallKind) =
    API.LLVMSetTailCallKind(inst, kind)

@property CallInst tailcall_kind tailcall_kind!

called_operand(inst::CallBase) = Value(API.LLVMGetCalledValue(inst))

function called_operand!(inst::CallBase, callee::Value)
    # the callee is the last operand of every kind of call site
    idx = API.LLVMGetNumOperands(inst) - 1
    old_type = API.LLVMTypeOf(API.LLVMGetOperand(inst, idx))
    API.LLVMTypeOf(callee) == old_type ||
        throw(ArgumentError("Callee of type $(value_type(callee)) does not match the called operand of type $(LLVMType(old_type))"))
    API.LLVMSetOperand(inst, idx, callee)
end

function called_function(inst::CallBase)
    ref = API.LLVMGetCalledValue(inst)
    (API.LLVMIsAFunction(ref) != C_NULL &&
     API.LLVMGetFunctionType(ref) == API.LLVMGetCalledFunctionType(inst)) || return nothing
    return Function(ref)
end

function called_type(inst::CallBase)
    LLVMType(API.LLVMGetCalledFunctionType(inst))
end

@property CallBase called_operand called_operand!
@property CallBase called_function
@property CallBase called_type

struct CallArgumentSet <: AbstractVector{Value}
    inst::CallBase
end

arguments(inst::CallBase) = CallArgumentSet(inst)

@property CallBase arguments

Base.size(iter::CallArgumentSet) = (Int(API.LLVMGetNumArgOperands(iter.inst)),)

Base.IndexStyle(::Type{CallArgumentSet}) = IndexLinear()

# the arguments are the first operands of a call site
function Base.getindex(iter::CallArgumentSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetOperand(iter.inst, i-1))
end

function Base.setindex!(iter::CallArgumentSet, val::Value, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    API.LLVMSetOperand(iter.inst, i-1, val)
    return iter
end

# attributes

struct CallSiteAttrSet <: AttributeSet
    instr::LLVM.CallBase
    idx::LLVM.API.LLVMAttributeIndex
end

function_attributes(instr::LLVM.CallBase) =
    CallSiteAttrSet(instr, reinterpret(LLVM.API.LLVMAttributeIndex, LLVM.API.LLVMAttributeFunctionIndex))

struct CallSiteArgumentAttrSets <: AbstractVector{CallSiteAttrSet}
    instr::CallBase
end

argument_attributes(instr::CallBase) = CallSiteArgumentAttrSets(instr)

Base.size(iter::CallSiteArgumentAttrSets) =
    (Int(API.LLVMGetNumArgOperands(iter.instr)),)

Base.IndexStyle(::Type{CallSiteArgumentAttrSets}) = IndexLinear()

function Base.getindex(iter::CallSiteArgumentAttrSets, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return CallSiteAttrSet(iter.instr, API.LLVMAttributeIndex(i))
end

return_attributes(instr::LLVM.CallBase) = CallSiteAttrSet(instr, LLVM.API.LLVMAttributeReturnIndex)

@property CallBase function_attributes
@property CallBase argument_attributes
@property CallBase return_attributes

function Base.collect(iter::CallSiteAttrSet)
    elems = Vector{LLVM.API.LLVMAttributeRef}(undef, length(iter))
    if length(iter) > 0
      # FIXME: this prevents a nullptr ref in LLVM similar to D26392
      LLVM.API.LLVMGetCallSiteAttributes(iter.instr, iter.idx, elems)
    end
    return LLVM.Attribute[LLVM.Attribute(elem) for elem in elems]
end

function Base.push!(iter::CallSiteAttrSet, attr::LLVM.Attribute)
    LLVM.API.LLVMAddCallSiteAttribute(iter.instr, iter.idx, attr)
    return iter
end

function Base.length(iter::CallSiteAttrSet)
    return LLVM.API.LLVMGetCallSiteAttributeCount(iter.instr, iter.idx)
end

attribute_ref(iter::CallSiteAttrSet, id::Integer) =
    API.LLVMGetCallSiteEnumAttribute(iter.instr, iter.idx, id)
function attribute_ref(iter::CallSiteAttrSet, kind::AbstractString)
    kind = String(kind)
    API.LLVMGetCallSiteStringAttribute(iter.instr, iter.idx, kind, ncodeunits(kind))
end

remove_attribute!(iter::CallSiteAttrSet, id::Integer) =
    API.LLVMRemoveCallSiteEnumAttribute(iter.instr, iter.idx, id)
function remove_attribute!(iter::CallSiteAttrSet, kind::AbstractString)
    kind = String(kind)
    API.LLVMRemoveCallSiteStringAttribute(iter.instr, iter.idx, kind, ncodeunits(kind))
end

function MemoryEffects(iter::CallSiteAttrSet)
    check_memory_effects_index(iter.idx)
    version() >= v"16" || return legacy_memory_effects_of(iter)
    ref = API.LLVMGetCallSiteEnumAttribute(iter.instr, iter.idx, memory_kind())
    ref == C_NULL && return MemoryEffects(:readwrite)
    return MemoryEffects(EnumAttribute(ref))
end

memory_effects(call::CallBase) = FunctionMemoryEffects(function_attributes(call))

memory_effects!(call::CallBase, effects::AnyMemoryEffects) =
    memory_effects!(function_attributes(call), MemoryEffects(effects))

@property CallBase memory_effects memory_effects!

# operand bundles

@vocabulary IR OperandBundle

# NOTE: OperandBundle objects aren't LLVM IR objects, but created by the C API wrapper,
#       so we need to free them explicitly when we get or create them.

"""
    OperandBundle

An operand bundle attached to a call site.

# Properties

    bundle.tag

The tag of the operand bundle, e.g., `"deopt"`.

    bundle.inputs

The inputs of the operand bundle, as a read-only view.
"""
@checked mutable struct OperandBundle
    ref::API.LLVMOperandBundleRef
end
@properties OperandBundle

Base.unsafe_convert(::Type{API.LLVMOperandBundleRef}, bundle::OperandBundle) =
    bundle.ref

"""
    OperandBundle(tag::AbstractString, args::Vector{Value}=Value[])

Create a new operand bundle with the given tag and arguments.
"""
function OperandBundle(tag::AbstractString, args::AbstractVector{<:Value}=Value[])
    tag = String(tag)
    bundle = OperandBundle(API.LLVMCreateOperandBundle(tag, ncodeunits(tag),
                                                       as_vector(args), length(args)))
    finalizer(bundle) do obj
        API.LLVMDisposeOperandBundle(obj)
    end
end

struct OperandBundleIterator <: AbstractVector{OperandBundle}
    inst::Instruction
end

operand_bundles(inst::CallBase) = OperandBundleIterator(inst)

@property CallBase operand_bundles

Base.size(iter::OperandBundleIterator) = (Int(API.LLVMGetNumOperandBundles(iter.inst)),)

Base.IndexStyle(::Type{OperandBundleIterator}) = IndexLinear()

function Base.getindex(iter::OperandBundleIterator, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    bundle = OperandBundle(API.LLVMGetOperandBundleAtIndex(iter.inst, i-1))
    finalizer(bundle) do obj
        API.LLVMDisposeOperandBundle(obj)
    end
end

function tag(bundle::OperandBundle)
    len = Ref{Csize_t}()
    data = API.LLVMGetOperandBundleTag(bundle, len)
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property OperandBundle tag

struct OperandBundleInputIterator <: AbstractVector{Value}
    bundle::OperandBundle
end

inputs(bundle::OperandBundle) = OperandBundleInputIterator(bundle)

@property OperandBundle inputs

Base.size(iter::OperandBundleInputIterator) =
    (Int(API.LLVMGetNumOperandBundleArgs(iter.bundle)),)

Base.IndexStyle(::Type{OperandBundleInputIterator}) = IndexLinear()

function Base.getindex(iter::OperandBundleInputIterator, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    Value(API.LLVMGetOperandBundleArgAtIndex(iter.bundle, i-1))
end

function Base.string(bundle::OperandBundle)
    # mimic how bundles are rendered in LLVM IR
    "\"$(tag(bundle))\"(" * join(string.(inputs(bundle)), ", ") * ")"
end

function Base.show(io::IO, ::MIME"text/plain", bundle::OperandBundle)
    print(io, string(bundle))
end

Base.show(io::IO, bundle::OperandBundle) =
    print(io, typeof(bundle), "(", string(bundle), ")")


## terminators

@vocabulary IR isterminator, isconditional

"""
    isterminator(inst::Instruction)

Check if the given instruction is a terminator instruction.
"""
isterminator(inst::Instruction) = API.LLVMIsATerminatorInst(inst) != C_NULL

"""
    isconditional(br::BrInst)

Check if the given branch instruction is conditional.
"""
isconditional(br::BrInst) = API.LLVMIsConditional(br) |> Bool

condition(br::BrInst) = Value(API.LLVMGetCondition(br))

condition!(br::BrInst, cond::Value) = API.LLVMSetCondition(br, cond)

@property BrInst condition condition!

default_dest(switch::SwitchInst) = BasicBlock(API.LLVMGetSwitchDefaultDest(switch))

@property SwitchInst default_dest

struct SwitchCaseValueSet <: AbstractVector{ConstantInt}
    switch::SwitchInst
end

case_values(switch::SwitchInst) = SwitchCaseValueSet(switch)

@property SwitchInst case_values

Base.size(iter::SwitchCaseValueSet) = (length(successors(iter.switch)) - 1,)

Base.IndexStyle(::Type{SwitchCaseValueSet}) = IndexLinear()

# the C API indexes cases by the index of their successor
function Base.getindex(iter::SwitchCaseValueSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetSwitchCaseValue(iter.switch, i))::ConstantInt
end

function Base.setindex!(iter::SwitchCaseValueSet, value::ConstantInt, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    check_case_value(iter.switch, value, i)
    API.LLVMSetSwitchCaseValue(iter.switch, i, value)
    return iter
end

function check_case_value(switch::SwitchInst, value::ConstantInt, except::Int=0)
    cond = Value(API.LLVMGetOperand(switch, 0))
    value_type(value) == value_type(cond) ||
        throw(ArgumentError("Switch case value of type $(value_type(value)) does not match the condition of type $(value_type(cond))"))
    for i in 1:API.LLVMGetNumSuccessors(switch)-1
        i == except && continue
        API.LLVMGetSwitchCaseValue(switch, i) == value.ref &&
            throw(ArgumentError("Switch already has a case for $(value)"))
    end
end

function check_case_dest(switch::SwitchInst, dest::BasicBlock)
    bb = API.LLVMGetInstructionParent(switch)
    bb == C_NULL && return
    API.LLVMGetBasicBlockParent(dest) == API.LLVMGetBasicBlockParent(bb) ||
        throw(ArgumentError("Switch case destination is not part of the same function"))
end

struct SwitchCaseSet <: AbstractVector{Tuple{ConstantInt,BasicBlock}}
    switch::SwitchInst
end

cases(switch::SwitchInst) = SwitchCaseSet(switch)

@property SwitchInst cases

Base.size(iter::SwitchCaseSet) = (Int(API.LLVMGetNumSuccessors(iter.switch)) - 1,)

Base.IndexStyle(::Type{SwitchCaseSet}) = IndexLinear()

# the C API indexes cases by the index of their successor
function Base.getindex(iter::SwitchCaseSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return (Value(API.LLVMGetSwitchCaseValue(iter.switch, i))::ConstantInt,
            BasicBlock(API.LLVMGetSuccessor(iter.switch, i)))
end

function Base.setindex!(iter::SwitchCaseSet, (value, dest)::Tuple{ConstantInt,BasicBlock},
                        i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    check_case_value(iter.switch, value, i)
    check_case_dest(iter.switch, dest)
    API.LLVMSetSwitchCaseValue(iter.switch, i, value)
    API.LLVMSetSuccessor(iter.switch, i, dest)
    return iter
end

function Base.push!(iter::SwitchCaseSet, (value, dest)::Tuple{ConstantInt,BasicBlock})
    check_case_value(iter.switch, value)
    check_case_dest(iter.switch, dest)
    API.LLVMAddCase(iter.switch, value, dest)
    return iter
end

function Base.append!(iter::SwitchCaseSet, cases)
    for case in cases
        push!(iter, case)
    end
    return iter
end

# successor iteration

@vocabulary IR TerminatorInst

"""
    LLVM.TerminatorInst

The group of terminators: the instructions that end a basic block, like `ret`, `br` or
`switch`.

# Properties

    term.successors

The successors of a terminator instruction, as a mutable view: assigning to an element,
`term.successors[i] = bb`, changes the destination of the terminator.

    br.condition
    br.condition = cond::Value

The condition of a conditional branch instruction.

    switch.default_dest

The default destination of a switch instruction.

    switch.case_values

The values of the cases of a switch instruction, as a mutable view. The destination of the
`i`th case is `switch.successors[i+1]`, the first successor being the default destination.
Assigning to an element changes the value of that case, which needs to have the same type
as the switch condition.

    switch.cases

The cases of a switch instruction, as a view of `(value, block)` tuples of the value of the
condition and the block to branch to, not including the default destination. The view is
mutable: cases can be added using `push!` or `append!`, and replaced by assigning to an
element, e.g., `switch.cases[1] = (ConstantInt(Int32(42)), bb)`. The value needs to have
the same type as the condition, there can only be one case for each value, and the block
needs to be part of the same function.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const TerminatorInst = Union{RetInst, BrInst, SwitchInst, IndirectBrInst, InvokeInst,
                             UnreachableInst, CallBrInst, ResumeInst, CleanupRetInst,
                             CatchRetInst, CatchSwitchInst}

struct TerminatorSuccessorSet <: AbstractVector{BasicBlock}
    term::Instruction
end

successors(term::Instruction) = TerminatorSuccessorSet(term)

@property TerminatorInst successors

Base.size(iter::TerminatorSuccessorSet) = (Int(API.LLVMGetNumSuccessors(iter.term)),)

Base.IndexStyle(::Type{TerminatorSuccessorSet}) = IndexLinear()

function Base.getindex(iter::TerminatorSuccessorSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return BasicBlock(API.LLVMGetSuccessor(iter.term, i-1))
end

function Base.setindex!(iter::TerminatorSuccessorSet, bb::BasicBlock, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    API.LLVMSetSuccessor(iter.term, i-1, bb)
    return iter
end


## phi nodes

# incoming iteration

struct PhiIncomingSet <: AbstractVector{Tuple{Value,BasicBlock}}
    phi::Instruction
end

incoming(phi::PHIInst) = PhiIncomingSet(phi)

@property PHIInst incoming

Base.size(iter::PhiIncomingSet) = (Int(API.LLVMCountIncoming(iter.phi)),)

Base.IndexStyle(::Type{PhiIncomingSet}) = IndexLinear()

function Base.getindex(iter::PhiIncomingSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return tuple(Value(API.LLVMGetIncomingValue(iter.phi, i-1)),
                       BasicBlock(API.LLVMGetIncomingBlock(iter.phi, i-1)))
end

function Base.push!(iter::PhiIncomingSet, (val, bb)::Tuple{<:Value, BasicBlock})
    API.LLVMAddIncoming(iter.phi, [val], [bb], 1)
    return iter
end

function Base.append!(iter::PhiIncomingSet, args)
    for arg in args
        push!(iter, arg)
    end
    return iter
end


## poison-generating flags

# the instructions that support each flag, depending on the version of LLVM. querying a
# flag on other instructions asserts (or worse), so the properties are only declared for
# these instructions.
@vocabulary IR NoWrapInst

"""
    LLVM.NoWrapInst

The group of instructions that can have the `nuw` and `nsw` flags: `add`, `sub`, `mul`,
`shl` and, on LLVM 19+, `trunc`.

# Properties

    inst.nuw
    inst.nuw = flag::Bool

Whether an `add`, `sub`, `mul`, `shl` or (on LLVM 19+) `trunc` instruction has the `nuw`
(no unsigned wrap) flag, which makes the result poison if unsigned overflow occurs.

    inst.nsw
    inst.nsw = flag::Bool

Whether an `add`, `sub`, `mul`, `shl` or (on LLVM 19+) `trunc` instruction has the `nsw`
(no signed wrap) flag, which makes the result poison if signed overflow occurs.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const NoWrapInst = version() >= v"19" ?
    Union{AddInst, SubInst, MulInst, ShlInst, TruncInst} :
    Union{AddInst, SubInst, MulInst, ShlInst}
@vocabulary IR ExactInst

"""
    LLVM.ExactInst

The group of instructions that can have the `exact` flag: `udiv`, `sdiv`, `lshr` and `ashr`.

# Properties

    inst.exact
    inst.exact = flag::Bool

Whether a `udiv`, `sdiv`, `lshr` or `ashr` instruction has the `exact` flag, which makes
the result poison if the division has a remainder, or if the shift shifts out any non-zero
bits.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const ExactInst = Union{UDivInst, SDivInst, LShrInst, AShrInst}
@vocabulary IR NonNegInst

"""
    LLVM.NonNegInst

The group of instructions that can have the `nneg` flag: `zext` and, on LLVM 19+, `uitofp`.

# Properties

    inst.nneg
    inst.nneg = flag::Bool

Whether a `zext` or (on LLVM 19+) `uitofp` instruction has the `nneg` (non-negative) flag,
which makes the result poison if the operand is negative. Requires LLVM 18+.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const NonNegInst = version() >= v"19" ? Union{ZExtInst, UIToFPInst} : ZExtInst

nuw(inst::NoWrapInst) = API.LLVMGetNUW(inst) |> Bool

nuw!(inst::NoWrapInst, flag::Bool) = API.LLVMSetNUW(inst, flag)

@property NoWrapInst nuw nuw!

nsw(inst::NoWrapInst) = API.LLVMGetNSW(inst) |> Bool

nsw!(inst::NoWrapInst, flag::Bool) = API.LLVMSetNSW(inst, flag)

@property NoWrapInst nsw nsw!

exact(inst::ExactInst) = API.LLVMGetExact(inst) |> Bool

exact!(inst::ExactInst, flag::Bool) = API.LLVMSetExact(inst, flag)

@property ExactInst exact exact!

disjoint(inst::OrInst) = API.LLVMGetIsDisjoint(inst) |> Bool

disjoint!(inst::OrInst, flag::Bool) = API.LLVMSetIsDisjoint(inst, flag)

if version() >= v"18"
    @property OrInst disjoint disjoint!
end

nneg(inst::NonNegInst) = API.LLVMGetNNeg(inst) |> Bool

nneg!(inst::NonNegInst, flag::Bool) = API.LLVMSetNNeg(inst, flag)

if version() >= v"18"
    @property NonNegInst nneg nneg!
end

samesign(inst::ICmpInst) = API.LLVMGetICmpSameSign(inst) |> Bool

samesign!(inst::ICmpInst, flag::Bool) = API.LLVMSetICmpSameSign(inst, flag)

if version() >= v"20"
    @property ICmpInst samesign samesign!
end

## floating point operations

@vocabulary IR FastMathFlags

# the fast-math flags and their bit in LLVM's `LLVMFastMathFlags`
const fast_math_flag_bits = (
    nnan = UInt32(API.LLVMFastMathNoNaNs),
    ninf = UInt32(API.LLVMFastMathNoInfs),
    nsz = UInt32(API.LLVMFastMathNoSignedZeros),
    arcp = UInt32(API.LLVMFastMathAllowReciprocal),
    contract = UInt32(API.LLVMFastMathAllowContract),
    afn = UInt32(API.LLVMFastMathApproxFunc),
    reassoc = UInt32(API.LLVMFastMathAllowReassoc),
)
const fast_math_all_bits = UInt32(API.LLVMFastMathAll)

"""
    FastMathFlags

The fast-math flags of a floating-point instruction, as returned by its `fast_math`
property. This is a view of the flags of that instruction: each flag is a `Bool` property,
which reads the flag from the instruction, and sets or clears it when assigned to:

 - `nnan`: assume arguments and results are not NaN
 - `ninf`: assume arguments and results are not Inf
 - `nsz`: treat the sign of zero arguments and results as insignificant
 - `arcp`: allow use of reciprocal rather than perform division
 - `contract`: allow contraction of operations
 - `afn`: allow substitution of approximate calculations for functions
 - `reassoc`: allow reassociation of operations

In addition, `fast` is `true` when all flags are set (which LLVM prints as `fast`), and
assigning to it sets or clears all flags.

Use `NamedTuple(flags)` to get the value of every flag, and assign to the `fast_math`
property of the instruction to replace all flags at once. Two sets of flags are equal when
the same flags are set.

# Examples

```julia
inst.fast_math.nnan = true      # set a single flag
inst.fast_math.nnan             # true
inst.fast_math = (; ninf=true)  # replace all flags, clearing the others
inst.fast_math.fast = true      # set all flags
```
"""
struct FastMathFlags
    inst::Instruction

    function FastMathFlags(inst::Instruction)
        Bool(API.LLVMCanValueUseFastMathFlags(inst)) ||
            throw(ArgumentError("Instruction cannot use fast math flags"))
        new(inst)
    end
end
@properties FastMathFlags

fast_math_bits(flags::FastMathFlags) =
    UInt32(API.LLVMGetFastMathFlags(getfield(flags, :inst)))

# unlike `LLVMSetFastMathFlags`, which only adds flags, this replaces them
set_fast_math_bits!(flags::FastMathFlags, bits::UInt32) =
    API.LLVMExtraSetFastMathFlags(getfield(flags, :inst), bits)

for (flag, bit) in pairs(fast_math_flag_bits)
    @eval begin
        getprop(flags::FastMathFlags, ::Val{$(QuoteNode(flag))}) =
            fast_math_bits(flags) & $bit != 0
        function setprop!(flags::FastMathFlags, ::Val{$(QuoteNode(flag))}, v::Bool)
            bits = fast_math_bits(flags)
            set_fast_math_bits!(flags, v ? bits | $bit : bits & ~$bit)
            return v
        end
        push!(property_registry, (FastMathFlags, $(QuoteNode(flag))))
    end
end

getprop(flags::FastMathFlags, ::Val{:fast}) =
    fast_math_bits(flags) & fast_math_all_bits == fast_math_all_bits
function setprop!(flags::FastMathFlags, ::Val{:fast}, v::Bool)
    set_fast_math_bits!(flags, v ? fast_math_all_bits : UInt32(0))
    return v
end
push!(property_registry, (FastMathFlags, :fast))

function Base.NamedTuple(flags::FastMathFlags)
    bits = fast_math_bits(flags)
    return map(bit -> bits & bit != 0, fast_math_flag_bits)
end

Base.:(==)(a::FastMathFlags, b::FastMathFlags) = fast_math_bits(a) == fast_math_bits(b)
Base.hash(flags::FastMathFlags, h::UInt) =
    hash(fast_math_bits(flags), hash(FastMathFlags, h))

function Base.show(io::IO, flags::FastMathFlags)
    print(io, "FastMathFlags(")
    if flags.fast
        print(io, "fast=true")
    else
        bits = fast_math_bits(flags)
        join(io, ("$flag=true" for (flag, bit) in pairs(fast_math_flag_bits)
                  if bits & bit != 0), ", ")
    end
    print(io, ")")
end

fast_math(inst::Instruction) = FastMathFlags(inst)

function fast_math!(inst::Instruction, flags::FastMathFlags)
    set_fast_math_bits!(FastMathFlags(inst), fast_math_bits(flags))
    return
end

function fast_math!(inst::Instruction, flags::NamedTuple)
    for (flag, v) in pairs(flags)
        if flag !== :fast && !haskey(fast_math_flag_bits, flag)
            valid = join(map(repr, (keys(fast_math_flag_bits)..., :fast)), ", ")
            throw(ArgumentError("Unknown fast-math flag $(repr(flag)); expected $valid"))
        end
        v isa Bool || throw(ArgumentError("Fast-math flags must be Bools, got $(repr(v))"))
    end
    bits = get(flags, :fast, false) ? fast_math_all_bits : UInt32(0)
    for (flag, v) in pairs(flags)
        flag === :fast && continue
        bit = fast_math_flag_bits[flag]
        bits = v ? bits | bit : bits & ~bit
    end
    set_fast_math_bits!(FastMathFlags(inst), bits)
    return
end

# the instructions that can be an `FPMathOperator`, which depends on the LLVM version
@vocabulary IR FPMathInst

"""
    LLVM.FPMathInst

The group of floating-point operations that can have fast-math flags, like LLVM's
`FPMathOperator`: `fneg`, `fadd`, `fsub`, `fmul`, `fdiv`, `frem` and `fcmp`, `fptrunc` and
`fpext` on LLVM 20+, `uitofp` and `sitofp` on LLVM 23+, and `phi`, `select` and `call`
instructions of floating-point type.

# Properties

    inst.fast_math
    inst.fast_math = flags::Union{NamedTuple,FastMathFlags}

The fast-math flags of a floating-point instruction, as a [`FastMathFlags`](@ref) view
that can be used to inspect and change individual flags, e.g.,
`inst.fast_math.nnan = true`. Only available on the instructions that LLVM considers
floating-point operations; `phi`, `select` and `call` instructions only have fast-math
flags if they produce a floating-point value, and throw an `ArgumentError` otherwise. Use
[`supports_fast_math`](@ref) to check whether an instruction has fast-math flags.

Assigning replaces all flags: with a named tuple of `Bool`s (e.g., `(; nnan=true,
ninf=true)`), the flags that are not specified are cleared, while `fast=true` sets all
flags that are not specified. The flags can also be copied from another instruction by
assigning its `FastMathFlags`.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const FPMathInst = Union{FNegInst, FAddInst, FSubInst, FMulInst, FDivInst, FRemInst, FCmpInst,
                         PHIInst, SelectInst, CallInst,
                         (version() >= v"20" ? (FPTruncInst, FPExtInst) : ())...,
                         (version() >= v"23" ? (UIToFPInst, SIToFPInst) : ())...}

@vocabulary IR supports_fast_math

"""
    supports_fast_math(inst::Instruction)

Check whether the given instruction supports fast-math flags, i.e., whether it is a
floating-point operation like C++'s `FPMathOperator`. For `phi`, `select` and `call`
instructions, this depends on whether they produce a floating-point value.
"""
supports_fast_math(inst::Instruction) = Bool(API.LLVMCanValueUseFastMathFlags(inst))

@property FPMathInst fast_math fast_math!


## memory operations

pointer_operand(inst::LoadInst) = Value(API.LLVMGetOperand(inst, 0))
pointer_operand(inst::StoreInst) = Value(API.LLVMGetOperand(inst, 1))
pointer_operand(inst::GetElementPtrInst) = Value(API.LLVMGetOperand(inst, 0))
pointer_operand(inst::AtomicRMWInst) = Value(API.LLVMGetOperand(inst, 0))
pointer_operand(inst::AtomicCmpXchgInst) = Value(API.LLVMGetOperand(inst, 0))

@property Union{LoadInst,StoreInst,GetElementPtrInst,AtomicRMWInst,AtomicCmpXchgInst} pointer_operand

value_operand(inst::StoreInst) = Value(API.LLVMGetOperand(inst, 0))
value_operand(inst::AtomicRMWInst) = Value(API.LLVMGetOperand(inst, 1))

@property Union{StoreInst,AtomicRMWInst} value_operand

compare_operand(inst::AtomicCmpXchgInst) = Value(API.LLVMGetOperand(inst, 1))
new_value_operand(inst::AtomicCmpXchgInst) = Value(API.LLVMGetOperand(inst, 2))

@property AtomicCmpXchgInst compare_operand
@property AtomicCmpXchgInst new_value_operand

allocated_type(inst::AllocaInst) = LLVMType(API.LLVMGetAllocatedType(inst))

@property AllocaInst allocated_type

source_element_type(inst::GetElementPtrInst) =
    LLVMType(API.LLVMGetGEPSourceElementType(inst))

@property GetElementPtrInst source_element_type

function check_gep(ce::ConstantExpr)
    opcode(ce) == API.LLVMGetElementPtr ||
        throw(ArgumentError("Expected a getelementptr constant expression, got a $(opcode(ce)) expression"))
    return ce
end

source_element_type(ce::ConstantExpr) =
    LLVMType(API.LLVMGetGEPSourceElementType(check_gep(ce)))

@property ConstantExpr source_element_type

@public constant_offset

"""
    LLVM.constant_offset(gep, dl::DataLayout)

Compute the offset in bytes that a `getelementptr` instruction or constant expression adds
to its pointer operand, according to the data layout `dl`, or `nothing` if the offset isn't
constant. The offset is a signed integer, computed with the index width of the pointer's
address space, and returned as a `BigInt`.

Only GEPs that compute a single pointer are supported, not those that compute a vector of
pointers.

    LLVM.constant_offset(T::Type{<:Integer}, gep, dl::DataLayout)

Compute the offset like `LLVM.constant_offset(gep, dl)`, but return it as an integer of type
`T` (e.g., `Int`), throwing an `InexactError` if it doesn't fit.
"""
function constant_offset(::Type{T}, gep::Union{GetElementPtrInst,ConstantExpr},
                         dl::DataLayout) where {T<:Integer}
    offset = constant_offset(gep, dl)
    return offset === nothing ? nothing : convert(T, offset)
end
function constant_offset(gep::Union{GetElementPtrInst,ConstantExpr}, dl::DataLayout)
    gep isa ConstantExpr && check_gep(gep)
    T = value_type(gep)
    T isa PointerType ||
        throw(ArgumentError("Cannot compute the constant offset of a GEP of vectors of pointers"))
    bits = Int(API.LLVMExtraGetIndexSizeInBits(dl, addrspace(T)))
    words = zeros(UInt64, cld(bits, 64))
    Bool(API.LLVMExtraGEPAccumulateConstantOffset(gep, dl, words)) || return nothing
    offset = BigInt(0)
    for (i, word) in enumerate(words)
        offset |= BigInt(word) << (64 * (i - 1))
    end
    # the offset is a signed integer of `bits` bits
    if bits > 0 && isodd(offset >> (bits - 1))
        offset -= BigInt(1) << bits
    end
    return offset
end

indices(inst::GetElementPtrInst) = @view operands(inst)[2:end]
indices(ce::ConstantExpr) = @view operands(check_gep(ce))[2:end]

@property GetElementPtrInst indices
@property ConstantExpr indices

inbounds(inst::GetElementPtrInst) = API.LLVMIsInBounds(inst) |> Bool

inbounds!(inst::GetElementPtrInst, flag::Bool) = API.LLVMSetIsInBounds(inst, flag)

@property GetElementPtrInst inbounds inbounds!


## aggregate operations

struct AggregateIndexSet <: AbstractVector{Int}
    inst::Instruction
end

indices(inst::Union{ExtractValueInst,InsertValueInst}) = AggregateIndexSet(inst)

@property Union{ExtractValueInst,InsertValueInst} indices

Base.size(iter::AggregateIndexSet) = (Int(API.LLVMGetNumIndices(iter.inst)),)

Base.IndexStyle(::Type{AggregateIndexSet}) = IndexLinear()

function Base.getindex(iter::AggregateIndexSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Int(unsafe_load(API.LLVMGetIndices(iter.inst), i))
end


## alignment

@vocabulary IR AlignedInst

"""
    LLVM.AlignedInst

The group of instructions that have an alignment: `alloca` and the instructions that access
memory.

# Properties

    inst.alignment
    inst.alignment = bytes::Integer

The alignment in bytes of an `alloca`, `load`, `store`, `atomicrmw` or `cmpxchg`
instruction. The assigned alignment must be a positive power of 2.

The properties of [`Instruction`](@ref LLVM.Instruction), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
const AlignedInst = Union{AllocaInst, MemAccessInst}

alignment(inst::AlignedInst) = API.LLVMGetAlignment(inst)

function alignment!(inst::AlignedInst, bytes::Integer)
    check_alignment(bytes)
    API.LLVMSetAlignment(inst, bytes)
end

# LLVM only supports querying the alignment of memory instructions
@property AlignedInst alignment alignment!
