export Instruction, remove!, erase!, opcode

"""
    Instruction

An instruction in the LLVM IR.
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
function register(T::Type{<:Instruction}, opcode::API.LLVMOpcode)
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
    return T(ref)
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

Remove the given instruction from the containing basic block and delete the object.

!!! warning

    This function is unsafe because it does not check if the instruction is used elsewhere.
"""
erase!(inst::Instruction) = API.LLVMInstructionEraseFromParent(inst)

"""
    parent(inst::Instruction)

Get the basic block that contains the given instruction.
"""
parent(inst::Instruction) = BasicBlock(API.LLVMGetInstructionParent(inst))

opcode(inst::Instruction) = API.LLVMGetInstructionOpcode(inst)

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
    @eval begin
        @checked struct $typename <: Instruction
            ref::API.LLVMValueRef
        end
        register($typename, API.$enum)
    end
end


## comparisons

export predicate

"""
    predicate(inst::ICmpInst)
    predicate(inst::FCmpInst)

Get the comparison predicate of the given integer or floating-point comparison instruction.
"""
predicate

predicate(inst::ICmpInst) = API.LLVMGetICmpPredicate(inst)
predicate(inst::FCmpInst) = API.LLVMGetFCmpPredicate(inst)


## atomics

export is_atomic, ordering, ordering!, SyncScope, syncscope, syncscope!, binop,
       isweak, weak!, isvolatile, volatile!,
       success_ordering, success_ordering!, failure_ordering, failure_ordering!,
       is_stronger, is_acquire_or_stronger, is_release_or_stronger, merged_ordering,
       strongest_failure_ordering, mmra!, copy_atomic_metadata!

const AtomicInst = Union{LoadInst, StoreInst, FenceInst, AtomicRMWInst, AtomicCmpXchgInst}

"""
    is_atomic(inst::Instruction)

Check if the given instruction is atomic. This includes atomic operations such as
`atomicrmw` or `fence`, but also loads and stores that have been made atomic by setting an
atomic ordering.
"""
is_atomic(inst::Instruction) = API.LLVMIsAtomic(inst) |> Bool

"""
    ordering(atomic_inst::Instruction)

Get the atomic ordering of the given atomic instruction. For `cmpxchg` instructions, use
[`success_ordering`](@ref) and [`failure_ordering`](@ref), or [`merged_ordering`](@ref).
"""
function ordering(inst::AtomicInst)
    is_atomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    @static if version() < v"18"
        API.LLVMExtraGetOrdering(inst)
    else
        API.LLVMGetOrdering(inst)
    end
end
ordering(::AtomicCmpXchgInst) =
    throw(ArgumentError("cmpxchg instructions have a success and a failure ordering"))

"""
    ordering!(inst::Instruction, ordering::LLVM.AtomicOrdering)

Set the atomic ordering of the given instruction. For `cmpxchg` instructions, use
[`success_ordering!`](@ref) and [`failure_ordering!`](@ref).
"""
function ordering!(inst::AtomicInst, ord::API.LLVMAtomicOrdering)
    # loads and stores can be made atomic by setting an ordering, but LLVM asserts when
    # setting an invalid ordering on other instructions
    if inst isa AtomicRMWInst
        is_stronger(ord, API.LLVMAtomicOrderingUnordered) ||
            throw(ArgumentError("atomicrmw requires an ordering of at least monotonic, got $ord"))
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

check_fence_ordering(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingAcquire || is_release_or_stronger(o) ||
        throw(ArgumentError("Fences must have acquire, release, acq_rel or seq_cst ordering, got $o"))

# the names LLVM uses in IR, and Julia's names for the orderings that differ
const ORDERING_NAMES = Dict(
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
    parse(API.LLVMAtomicOrdering, name::AbstractString)

Get the atomic ordering with the given name, as used in LLVM IR (e.g. `"acq_rel"`), or as
used by Julia's atomics (e.g. `"acquire_release"`).
"""
function Base.parse(::Type{API.LLVMAtomicOrdering}, name::AbstractString)
    ord = get(ORDERING_NAMES, name, nothing)
    ord === nothing && throw(ArgumentError("Unknown atomic ordering \"$name\""))
    return ord
end

const RMW_BINOP_NAMES = Dict(
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
    "fminimum" => API.LLVMAtomicRMWBinOpFMinimum)

"""
    parse(API.LLVMAtomicRMWBinOp, name::AbstractString)

Get the `atomicrmw` operation with the given name, as used in LLVM IR (e.g. `"uinc_wrap"`).
This works for every operation, whether or not it is [`available`](@ref) with the version
of LLVM in use.
"""
function Base.parse(::Type{API.LLVMAtomicRMWBinOp}, name::AbstractString)
    op = get(RMW_BINOP_NAMES, name, nothing)
    op === nothing && throw(ArgumentError("Unknown atomicrmw operation \"$name\""))
    return op
end

is_fp_rmw(op::API.LLVMAtomicRMWBinOp) =
    op in (API.LLVMAtomicRMWBinOpFAdd, API.LLVMAtomicRMWBinOpFSub,
           API.LLVMAtomicRMWBinOpFMax, API.LLVMAtomicRMWBinOpFMin,
           API.LLVMAtomicRMWBinOpFMaximum, API.LLVMAtomicRMWBinOpFMinimum)

# the lattice of orderings, from llvm/Support/AtomicOrdering.h
const ORDERING_LATTICE = let
    NA, UN, MO = API.LLVMAtomicOrderingNotAtomic, API.LLVMAtomicOrderingUnordered,
                 API.LLVMAtomicOrderingMonotonic
    AC, RE, AR = API.LLVMAtomicOrderingAcquire, API.LLVMAtomicOrderingRelease,
                 API.LLVMAtomicOrderingAcquireRelease
    SC = API.LLVMAtomicOrderingSequentiallyConsistent
    # each ordering, and the ones it is strictly stronger than
    Dict(NA => (), UN => (NA,), MO => (NA, UN), AC => (NA, UN, MO), RE => (NA, UN, MO),
         AR => (NA, UN, MO, AC, RE), SC => (NA, UN, MO, AC, RE, AR))
end

"""
    is_stronger(a::API.LLVMAtomicOrdering, b::API.LLVMAtomicOrdering)

Check whether ordering `a` is strictly stronger than `b`. Orderings are only partially
ordered: `acquire` and `release` are incomparable, and both are weaker than `acq_rel`.
"""
is_stronger(a::API.LLVMAtomicOrdering, b::API.LLVMAtomicOrdering) = b in ORDERING_LATTICE[a]

"""
    is_acquire_or_stronger(ordering::API.LLVMAtomicOrdering)

Check whether an ordering has acquire semantics: `acquire`, `acq_rel` or `seq_cst`.
"""
is_acquire_or_stronger(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingAcquire || is_stronger(o, API.LLVMAtomicOrderingAcquire)

"""
    is_release_or_stronger(ordering::API.LLVMAtomicOrdering)

Check whether an ordering has release semantics: `release`, `acq_rel` or `seq_cst`.
"""
is_release_or_stronger(o::API.LLVMAtomicOrdering) =
    o == API.LLVMAtomicOrderingRelease || is_stronger(o, API.LLVMAtomicOrderingRelease)

"""
    merged_ordering(a::API.LLVMAtomicOrdering, b::API.LLVMAtomicOrdering)
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
    strongest_failure_ordering(success::API.LLVMAtomicOrdering)

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
        throw(ArgumentError("cmpxchg requires an ordering of at least monotonic, got $success"))
    end
end

"""
    SyncScope

A synchronization scope for atomic operations.
"""
struct SyncScope
    id::Cuint
end

"""
    SyncScope(name::String)

Create a synchronization scope with the given name. This can be a well-known scope such as
`"singlethread"` or `"system"`, or a target-specific scope.
"""
function SyncScope(name::String)
    # the default, system syncscope gets encoded as an empty string
    if name == "system"
        name = ""
    end
    SyncScope(API.LLVMGetSyncScopeID(context(), name, length(name)))
end

Base.convert(::Type{Cuint}, scope::SyncScope) = scope.id

# scope IDs are specific to a context, but the first ones are fixed
function _name(scope::SyncScope)
    scope.id == 0 && return "singlethread"
    scope.id == 1 && return "system"
    len = Ref{Csize_t}()
    ptr = convert(Ptr{UInt8}, API.LLVMExtraGetSyncScopeName(context(), scope, len))
    ptr == C_NULL && return nothing
    return unsafe_string(ptr, len[])
end

"""
    name(scope::SyncScope)

Get the name of the given synchronization scope, as known by the current context.
"""
function name(scope::SyncScope)
    str = _name(scope)
    str === nothing && throw(ArgumentError("Unknown synchronization scope $(scope.id)"))
    return str
end

function Base.show(io::IO, scope::SyncScope)
    str = if scope.id <= 1 ||
             (context(; throw_error=false) !== nothing && isdefined(API, :libLLVMExtra))
        _name(scope)
    end
    if str === nothing
        print(io, "SyncScope(target-specific scope $(scope.id))")
    else
        print(io, "SyncScope(", repr(str), ")")
    end
end

"""
    syncscope(inst::AtomicInst)

Get the synchronization scope of the given atomic instruction.
"""
function syncscope(inst::AtomicInst)
    is_atomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    SyncScope(API.LLVMGetAtomicSyncScopeID(inst))
end

"""
    syncscope!(inst::AtomicInst, scope::SyncScope)

Set the synchronization scope of the given atomic instruction.
"""
function syncscope!(inst::AtomicInst, scope::SyncScope)
    is_atomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    API.LLVMSetAtomicSyncScopeID(inst, scope)
end

"""
    binop(inst::AtomicRMWInst)

Get the binary operation of the given atomic read-modify-write instruction.
"""
function binop(inst::AtomicRMWInst)
    @static if v"16" <= version() < v"19"
        API.LLVMAtomicRMWBinOp(API.LLVMExtraGetAtomicRMWBinOp(inst))
    else
        API.LLVMGetAtomicRMWBinOp(inst)
    end
end

# the LLVM version that introduced each atomicrmw operation, indexed by its C API value
const ATOMIC_RMW_BINOP_SINCE = (
    ntuple(_ -> v"0", 15)...,   # Xchg through FMin
    v"16", v"16",               # UIncWrap, UDecWrap
    v"20", v"20",               # USubCond, USubSat
    v"21", v"21",               # FMaximum, FMinimum
    v"23", v"23",               # FMaximumNum, FMinimumNum
)

"""
    available(op::API.LLVMAtomicRMWBinOp)

Check whether the atomic read-modify-write operation `op` is supported by the version of
LLVM in use. All operations can be named on every LLVM version, but instructions can only
be created with the ones that are available.
"""
function available(op::API.LLVMAtomicRMWBinOp)
    since = get(ATOMIC_RMW_BINOP_SINCE, Integer(op) + 1, nothing)
    since !== nothing && version() >= since
end

"""
    isweak(inst::AtomicCmpXchgInst)

Check if the given atomic compare-and-exchange instruction is weak.
"""
function isweak(inst::AtomicCmpXchgInst)
    API.LLVMGetWeak(inst) |> Bool
end

"""
    weak!(inst::AtomicCmpXchgInst, is_weak::Bool)

Set whether the given atomic compare-and-exchange instruction is weak.
"""
function weak!(inst::AtomicCmpXchgInst, is_weak::Bool)
    API.LLVMSetWeak(inst, is_weak)
end

const MemAccessInst = Union{LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst}

"""
    isvolatile(inst::Union{LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst})

Check whether the given memory access is volatile.
"""
isvolatile(inst::MemAccessInst) = API.LLVMGetVolatile(inst) |> Bool

"""
    volatile!(inst::Union{LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst}, is_volatile::Bool)

Set whether the given memory access is volatile.
"""
volatile!(inst::MemAccessInst, is_volatile::Bool) = API.LLVMSetVolatile(inst, is_volatile)

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
    loc = debuglocation(src)
    loc === nothing || debuglocation!(dest, loc)
    src_md, dest_md = metadata(src), metadata(dest)
    for kind in ATOMIC_METADATA
        haskey(src_md, kind) && (dest_md[kind] = src_md[kind])
    end
    return dest
end

"""
    success_ordering(inst::AtomicCmpXchgInst)

Get the success ordering of the given atomic compare-and-exchange instruction.
"""
function success_ordering(inst::AtomicCmpXchgInst)
    API.LLVMGetCmpXchgSuccessOrdering(inst)
end

"""
    success_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)

Set the success ordering of the given atomic compare-and-exchange instruction.
"""
function success_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)
    API.LLVMSetCmpXchgSuccessOrdering(inst, ord)
end

"""
    failure_ordering(inst::AtomicCmpXchgInst)

Get the failure ordering of the given atomic compare-and-exchange instruction.
"""
function failure_ordering(inst::AtomicCmpXchgInst)
    API.LLVMGetCmpXchgFailureOrdering(inst)
end

"""
    failure_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)

Set the failure ordering of the given atomic compare-and-exchange instruction.
"""
function failure_ordering!(inst::AtomicCmpXchgInst, ord::API.LLVMAtomicOrdering)
    API.LLVMSetCmpXchgFailureOrdering(inst, ord)
end


## call sites and invocations

# TODO: add this to the actual type hierarchy
const CallBase = Union{CallBrInst, CallInst, InvokeInst}

export callconv, callconv!,
       istailcall, tailcall!,
       called_operand, arguments, called_type

"""
    callconv(call_inst::Instruction)

Get the calling convention of the given callable instruction.
"""
callconv(inst::CallBase) = API.LLVMGetInstructionCallConv(inst)

"""
    callconv!(call_inst::Instruction, cc)

Set the calling convention of the given callable instruction.
"""
callconv!(inst::CallBase, cc) =
    API.LLVMSetInstructionCallConv(inst, cc)

"""
    istailcall(call_inst::Instruction)

Tests if this call site must be tail call optimized.
"""
istailcall(inst::CallBase) = API.LLVMIsTailCall(inst) |> Bool

"""
    tailcall!(call_inst::Instruction, is_tail::Bool)

Sets whether this call site must be tail call optimized.
"""
tailcall!(inst::CallBase, bool) = API.LLVMSetTailCall(inst, bool)

"""
    called_operand(call_inst::Instruction)

Get the operand of a callable instruction that represents the called function.
"""
called_operand(inst::CallBase) = Value(API.LLVMGetCalledValue(inst))

"""
    called_type(call_inst::Instruction)

Get the type of the function being called by the given callable instruction.
"""
function called_type(inst::CallBase)
    @static if version() >= v"11"
        LLVMType(API.LLVMGetCalledFunctionType(inst))
    else
        value_type(called_operand(inst))
    end
end

"""
    arguments(call_inst::Instruction)

Get the arguments of a callable instruction.
"""
function arguments(inst::CallBase)
    nargs = API.LLVMGetNumArgOperands(inst)
    operands(inst)[1:nargs]
end

# attributes

export function_attributes, argument_attributes, return_attributes

struct CallSiteAttrSet
    instr::LLVM.CallBase
    idx::LLVM.API.LLVMAttributeIndex
end

"""
    function_attributes(instr::CallBase)

Get the attributes of the given instruction.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
function_attributes(instr::LLVM.CallBase) =
    CallSiteAttrSet(instr, reinterpret(LLVM.API.LLVMAttributeIndex, LLVM.API.LLVMAttributeFunctionIndex))

"""
    argument_attributes(instr::CallBase, idx::Integer)

Get the attributes of the given argument of the given instruction.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
argument_attributes(instr::LLVM.CallBase, idx::Integer) =
    CallSiteAttrSet(instr, LLVM.API.LLVMAttributeIndex(idx))

"""
    return_attributes(instr::CallBase)

Get the attributes of the return value of the given instruction.

This is a mutable iterator, supporting `push!`, `append!` and `delete!`.
"""
return_attributes(instr::LLVM.CallBase) = CallSiteAttrSet(instr, LLVM.API.LLVMAttributeReturnIndex)

Base.eltype(::CallSiteAttrSet) = Attribute

function Base.collect(iter::CallSiteAttrSet)
    elems = Vector{LLVM.API.LLVMAttributeRef}(undef, length(iter))
    if length(iter) > 0
      # FIXME: this prevents a nullptr ref in LLVM similar to D26392
      LLVM.API.LLVMGetCallSiteAttributes(iter.instr, iter.idx, elems)
    end
    return LLVM.Attribute[LLVM.Attribute(elem) for elem in elems]
end

Base.push!(iter::CallSiteAttrSet, attr::LLVM.Attribute) =
    LLVM.API.LLVMAddCallSiteAttribute(iter.instr, iter.idx, attr)

Base.delete!(iter::CallSiteAttrSet, attr::LLVM.EnumAttribute) =
    LLVM.API.LLVMRemoveCallSiteEnumAttribute(iter.instr, iter.idx, kind(attr))

Base.delete!(iter::CallSiteAttrSet, attr::LLVM.TypeAttribute) =
    LLVM.API.LLVMRemoveCallSiteEnumAttribute(iter.instr, iter.idx, kind(attr))

Base.delete!(iter::CallSiteAttrSet, attr::LLVM.ConstantRangeAttribute) =
    LLVM.API.LLVMRemoveCallSiteEnumAttribute(iter.instr, iter.idx, kind(attr))

Base.delete!(iter::CallSiteAttrSet, attr::LLVM.ConstantRangeListAttribute) =
    LLVM.API.LLVMRemoveCallSiteEnumAttribute(iter.instr, iter.idx, kind(attr))

function Base.delete!(iter::CallSiteAttrSet, attr::LLVM.StringAttribute)
    k = kind(attr)
    return LLVM.API.LLVMRemoveCallSiteStringAttribute(iter.instr, iter.idx, k, length(k))
end

function Base.length(iter::CallSiteAttrSet)
    return LLVM.API.LLVMGetCallSiteAttributeCount(iter.instr, iter.idx)
end

# operand bundles

export OperandBundle, operand_bundles, tag, inputs

# NOTE: OperandBundle objects aren't LLVM IR objects, but created by the C API wrapper,
#       so we need to free them explicitly when we get or create them.

"""
    OperandBundle

An operand bundle attached to a call site.
"""
@checked mutable struct OperandBundle
    ref::API.LLVMOperandBundleRef
end
Base.unsafe_convert(::Type{API.LLVMOperandBundleRef}, bundle::OperandBundle) =
    bundle.ref

"""
    OperandBundle(tag::String, args::Vector{Value}=Value[])

Create a new operand bundle with the given tag and arguments.
"""
function OperandBundle(tag::String, args::Vector{<:Value}=Value[])
    bundle = OperandBundle(API.LLVMCreateOperandBundle(tag, length(tag), args, length(args)))
    finalizer(bundle) do obj
        API.LLVMDisposeOperandBundle(obj)
    end
end

struct OperandBundleIterator <: AbstractVector{OperandBundle}
    inst::Instruction
end

"""
    operand_bundles(call_inst::Instruction)

Get the operand bundles attached to the given call instruction.
"""
operand_bundles(inst::CallBase) = OperandBundleIterator(inst)

Base.size(iter::OperandBundleIterator) = (API.LLVMGetNumOperandBundles(iter.inst),)

Base.IndexStyle(::OperandBundleIterator) = IndexLinear()

function Base.getindex(iter::OperandBundleIterator, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    bundle = OperandBundle(API.LLVMGetOperandBundleAtIndex(iter.inst, i-1))
    finalizer(bundle) do obj
        API.LLVMDisposeOperandBundle(obj)
    end
end

"""
    tag(bundle::OperandBundle)

Get the tag of the given operand bundle.
"""
function tag(bundle::OperandBundle)
    len = Ref{Csize_t}()
    data = API.LLVMGetOperandBundleTag(bundle, len)
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

struct OperandBundleInputIterator <: AbstractVector{Value}
    bundle::OperandBundle
end

"""
    inputs(bundle::OperandBundle)

Get an iterator over the inputs of the given operand bundle.
"""
inputs(bundle::OperandBundle) = OperandBundleInputIterator(bundle)

Base.size(iter::OperandBundleInputIterator) = (API.LLVMGetNumOperandBundleArgs(iter.bundle),)

Base.IndexStyle(::OperandBundleInputIterator) = IndexLinear()

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

export isterminator, isconditional, condition, condition!, default_dest

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

"""
    condition(br::BrInst)

Get the condition of the given branch instruction.
"""
condition(br::BrInst) = Value(API.LLVMGetCondition(br))

"""
    condition!(br::BrInst, cond::Value)

Set the condition of the given branch instruction.
"""
condition!(br::BrInst, cond::Value) = API.LLVMSetCondition(br, cond)

"""
    default_dest(switch::SwitchInst)

Get the default destination of the given switch instruction.
"""
default_dest(switch::SwitchInst) = BasicBlock(API.LLVMGetSwitchDefaultDest(switch))

@static if version() >= v"22"
    export case_value, case_value!

    """
        case_value(switch::SwitchInst, i::Integer)

    Get the value of the `i`th case of a switch instruction. Requires LLVM 22+.
    """
    function case_value(switch::SwitchInst, i::Integer)
        @boundscheck 1 <= i < length(successors(switch)) || throw(BoundsError(switch, i))
        Value(API.LLVMGetSwitchCaseValue(switch, i))
    end

    """
        case_value!(switch::SwitchInst, i::Integer, value::ConstantInt)

    Set the value of the `i`th case of a switch instruction. Requires LLVM 22+.
    """
    function case_value!(switch::SwitchInst, i::Integer, value::ConstantInt)
        @boundscheck 1 <= i < length(successors(switch)) || throw(BoundsError(switch, i))
        API.LLVMSetSwitchCaseValue(switch, i, value)
    end
end

# successor iteration

export successors

struct TerminatorSuccessorSet <: AbstractVector{BasicBlock}
    term::Instruction
end

"""
    successors(term::Instruction)

Get an iterator over the successors of the given terminator instruction.

This is a mutable iterator, so you can modify the successors of the terminator by
calling `setindex!`.
"""
successors(term::Instruction) = TerminatorSuccessorSet(term)

Base.size(iter::TerminatorSuccessorSet) = (API.LLVMGetNumSuccessors(iter.term),)

Base.IndexStyle(::TerminatorSuccessorSet) = IndexLinear()

function Base.getindex(iter::TerminatorSuccessorSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return BasicBlock(API.LLVMGetSuccessor(iter.term, i-1))
end

Base.setindex!(iter::TerminatorSuccessorSet, bb::BasicBlock, i::Int) =
    API.LLVMSetSuccessor(iter.term, i-1, bb)


## phi nodes

# incoming iteration

export incoming

struct PhiIncomingSet <: AbstractVector{Tuple{Value,BasicBlock}}
    phi::Instruction
end

"""
    incoming(phi::PhiInst)

Get an iterator over the incoming values of the given phi node.

This is a mutable iterator, so you can modify the incoming values of the phi node by
calling `push!` or `append!`, passing a tuple of the incoming value and the originating
basic block.
"""
incoming(phi::PHIInst) = PhiIncomingSet(phi)

Base.size(iter::PhiIncomingSet) = (API.LLVMCountIncoming(iter.phi),)

Base.IndexStyle(::PhiIncomingSet) = IndexLinear()

function Base.getindex(iter::PhiIncomingSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return tuple(Value(API.LLVMGetIncomingValue(iter.phi, i-1)),
                       BasicBlock(API.LLVMGetIncomingBlock(iter.phi, i-1)))
end

function Base.append!(iter::PhiIncomingSet, args::Vector{Tuple{V, BasicBlock}} where V <: Value)
    vals, blocks = zip(args...)
    API.LLVMAddIncoming(iter.phi, collect(vals), collect(blocks), length(args))
end

Base.push!(iter::PhiIncomingSet, args::Tuple{<:Value, BasicBlock}) = append!(iter, [args])


## floating point operations

export fast_math, fast_math!

"""
    fast_math(inst::Instruction)

Get the fast math flags on an instruction.
"""
function fast_math(inst::Instruction)
    if !Bool(API.LLVMCanValueUseFastMathFlags(inst))
        throw(ArgumentError("Instruction cannot use fast math flags"))
    end
    flags = API.LLVMGetFastMathFlags(inst)
    return (;
        nnan = flags & LLVM.API.LLVMFastMathNoNaNs != 0,
        ninf = flags & LLVM.API.LLVMFastMathNoInfs != 0,
        nsz = flags & LLVM.API.LLVMFastMathNoSignedZeros != 0,
        arcp = flags & LLVM.API.LLVMFastMathAllowReciprocal != 0,
        contract = flags & LLVM.API.LLVMFastMathAllowContract != 0,
        afn = flags & LLVM.API.LLVMFastMathApproxFunc != 0,
        reassoc = flags & LLVM.API.LLVMFastMathAllowReassoc != 0,
    )
end

"""
    fast_math!(inst::Instruction; [flag=...], [all=...])

Set the fast math flags on an instruction. If `all` is `true`, then all flags are set.

The following flags are supported:
 - `nnan`: assume arguments and results are not NaN
 - `ninf`: assume arguments and results are not Inf
 - `nsz`: treat the sign of zero arguments and results as insignificant
 - `arcp`: allow use of reciprocal rather than perform division
 - `contract`: allow contraction of operations
 - `afn`: allow substitution of approximate calculations for functions
 - `reassoc`: allow reassociation of operations
"""
function fast_math!(inst::Instruction; nnan=false, ninf=false, nsz=false, arcp=false,
                          contract=false, afn=false, reassoc=false, all=false)
    if !Bool(API.LLVMCanValueUseFastMathFlags(inst))
        throw(ArgumentError("Instruction cannot use fast math flags"))
    end
    if all
        API.LLVMSetFastMathFlags(inst, LLVM.API.LLVMFastMathAll)
    else
        flags = 0
        nnan && (flags |= LLVM.API.LLVMFastMathNoNaNs)
        ninf && (flags |= LLVM.API.LLVMFastMathNoInfs)
        nsz && (flags |= LLVM.API.LLVMFastMathNoSignedZeros)
        arcp && (flags |= LLVM.API.LLVMFastMathAllowReciprocal)
        contract && (flags |= LLVM.API.LLVMFastMathAllowContract)
        afn && (flags |= LLVM.API.LLVMFastMathApproxFunc)
        reassoc && (flags |= LLVM.API.LLVMFastMathAllowReassoc)
        API.LLVMSetFastMathFlags(inst, flags)
    end
end


## alignment

const AlignedInst = Union{AllocaInst, MemAccessInst}

"""
    alignment(inst::Union{AllocaInst, LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst})

Get the alignment of the given stack allocation or memory access, in bytes.
"""
alignment(inst::AlignedInst) = API.LLVMGetAlignment(inst)

"""
    alignment!(inst::Union{AllocaInst, LoadInst, StoreInst, AtomicRMWInst, AtomicCmpXchgInst},
               bytes::Integer)

Set the alignment of the given stack allocation or memory access to `bytes`, which must be
a positive power of 2.
"""
function alignment!(inst::AlignedInst, bytes::Integer)
    check_alignment(bytes)
    API.LLVMSetAlignment(inst, bytes)
end
