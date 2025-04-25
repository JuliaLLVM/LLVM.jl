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
       isweak, weak!, success_ordering, success_ordering!, failure_ordering, failure_ordering!

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

Get the atomic ordering of the given atomic instruction.
"""
function ordering(inst::AtomicInst)
    is_atomic(inst) || throw(ArgumentError("Instruction is not atomic"))
    API.LLVMGetOrdering(inst)
end

"""
    ordering!(inst::Instruction, ordering::LLVM.AtomicOrdering)

Set the atomic ordering of the given instruction.
"""
function ordering!(inst::AtomicInst, ord::API.LLVMAtomicOrdering)
    # loads and stores can be made atomic by setting an ordering
    API.LLVMSetOrdering(inst, ord)
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

function Base.show(io::IO, scope::SyncScope)
    if scope.id == 0
        print(io, "SyncScope(\"singlethread\")")
    elseif scope.id == 1
        print(io, "SyncScope(\"system\")")
    else
        print(io, "SyncScope(target-specific scope $(scope.id))")
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
    API.LLVMGetAtomicRMWBinOp(inst)
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

# XXX: only for load, store, alloca

"""
    alignment(val::Instruction)

Get the alignment of the instruction.
"""
alignment(inst::Instruction) = API.LLVMGetAlignment(inst)

"""
    alignment!(val::Instruction, bytes::Integer)

Set the alignment of the instruction.
"""
alignment!(inst::Instruction, bytes::Integer) = API.LLVMSetAlignment(inst, bytes)
