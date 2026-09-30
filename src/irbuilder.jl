# An instruction builder represents a point within a basic block and is the exclusive means
# of building instructions using the C interface.

# instruction builders are thin wrappers around the C API, so don't specialize them on the
# concrete type of their operands (which would compile them for every combination).
@nospecialize

@vocabulary Build IRBuilder,
                  position!

"""
    IRBuilder

An instruction builder, which is used to build instructions within a basic block.

# Properties

    builder.context

The context of the instruction builder.

    builder.debug_location
    builder.debug_location = loc::Union{Metadata,MetadataAsValue,Nothing}

The debug location that the instruction builder attaches to the instructions it creates,
or `nothing` if no location is set. Assigning `nothing` clears the location.

To give an existing instruction the builder's location, assign it to the instruction
instead: `inst.debug_location = builder.debug_location`.
"""
@checked struct IRBuilder
    ref::API.LLVMBuilderRef
end
@properties IRBuilder

Base.unsafe_convert(::Type{API.LLVMBuilderRef}, builder::IRBuilder) =
    mark_use(builder).ref

"""
    IRBuilder()

Create a new, unpositioned instruction builder.

This object needs to be disposed of using [`dispose`](@ref).
"""
IRBuilder() = mark_alloc(IRBuilder(API.LLVMCreateBuilderInContext(context())))

"""
    dispose(builder::IRBuilder)

Dispose of an instruction builder.
"""
dispose(builder::IRBuilder) = mark_dispose(API.LLVMDisposeBuilder, builder)

context(builder::IRBuilder) = Context(API.LLVMGetBuilderContext(builder))

@property IRBuilder context

function IRBuilder(@specialize(f::Core.Function), args...; kwargs...)
    builder = IRBuilder(args...; kwargs...)
    try
        f(builder)
    finally
        dispose(builder)
    end
end

Base.show(io::IO, builder::IRBuilder) = @printf(io, "IRBuilder(%p)", builder.ref)

"""
    position(builder::IRBuilder)

Return the current position of the instruction builder.
"""
Base.position(builder::IRBuilder) = BasicBlock(API.LLVMGetInsertBlock(builder))

"""
    position!(builder::IRBuilder, inst::Instruction; after::Bool=false)

Position the instruction builder before the given instruction, or, when `after` is set,
directly after it (at the end of the basic block if it is the last instruction). Like C++'s
`Instruction::insertAfter`, new instructions are then inserted before any debug records
that precede the next instruction.

Positioning is literal: after a `phi` instruction, the builder is positioned before the next
instruction, even if that is another `phi` instruction where other instructions cannot be
inserted.
"""
function position!(builder::IRBuilder, inst::Instruction; after::Bool=false)
    if after
        bb = API.LLVMGetInstructionParent(inst)
        bb == C_NULL &&
            throw(ArgumentError("Cannot position after an instruction that is not part of a basic block"))
        next = API.LLVMGetNextInstruction(inst)
        if next == C_NULL
            API.LLVMPositionBuilderAtEnd(builder, bb)
        else
            @static if version() >= v"19"
                API.LLVMPositionBuilderBeforeInstrAndDbgRecords(builder, next)
            else
                API.LLVMPositionBuilderBefore(builder, next)
            end
        end
    else
        API.LLVMPositionBuilderBefore(builder, inst)
    end
    return
end

"""
    position!(builder::IRBuilder, bb::BasicBlock)

Position the instruction builder at the end of the given basic block.
"""
position!(builder::IRBuilder, bb::BasicBlock) =
    API.LLVMPositionBuilderAtEnd(builder, bb)

"""
    position!(builder::IRBuilder)

Clear the current position of the instruction builder.
"""
position!(builder::IRBuilder) = API.LLVMClearInsertionPosition(builder)

"""
    insert!(builder::IRBuilder, inst::Instruction, [name::String])

Insert an instruction into the current basic block at the current position, optionally
giving it a name.
"""
Base.insert!(builder::IRBuilder, inst::Instruction, name::String="") =
    API.LLVMInsertIntoBuilderWithName(builder, inst, name)

function debug_location(builder::IRBuilder)
    ref = API.LLVMGetCurrentDebugLocation2(builder)
    ref == C_NULL ? nothing : Metadata(ref)
end

debug_location!(builder::IRBuilder) =
    API.LLVMSetCurrentDebugLocation2(builder, C_NULL)
debug_location!(builder::IRBuilder, loc::Metadata) =
    API.LLVMSetCurrentDebugLocation2(builder, loc)
debug_location!(builder::IRBuilder, loc::MetadataAsValue) =
    API.LLVMSetCurrentDebugLocation2(builder, Metadata(loc))

@property IRBuilder debug_location (builder, loc::Union{Metadata,MetadataAsValue,Nothing}) ->
    loc === nothing ? debug_location!(builder) : debug_location!(builder, loc)


## build methods

# TODO/IDEAS:
# - dynamic dispatch based on `value_type` (eg. disambiguating `add!` and `fadd!`)

# NOTE: the return values for these operations are, according to the C API, always a Value.
#       however, the C++ API learns us that we can be more strict.

@vocabulary Build ret!, br!, switch!, indirectbr!, invoke!, resume!, unreachable!,

                  binop!, add!, nswadd!, nuwadd!, fadd!, sub!, nswsub!, nuwsub!, fsub!,
                  mul!, nswmul!, nuwmul!, fmul!, udiv!, exactudiv!, sdiv!, exactsdiv!, fdiv!,
                  urem!, srem!, frem!, neg!, nswneg!, fneg!,

                  shl!, lshr!, ashr!, and!, or!, xor!, not!,

                  extract_element!, insert_element!, shuffle_vector!,

                  extract_value!, insert_value!,

                  alloca!, array_alloca!, malloc!, array_malloc!, memset!, memcpy!,
                  memmove!, free!, load!, store!, fence!, atomic_rmw!, atomic_cmpxchg!,
                  gep!, inbounds_gep!, struct_gep!,

                  trunc!, zext!, sext!, fptoui!, fptosi!, uitofp!, sitofp!, fptrunc!,
                  fpext!, ptrtoint!, inttoptr!, bitcast!, addrspacecast!, zextorbitcast!,
                  sextorbitcast!, truncorbitcast!, cast!, pointercast!, intcast!, fpcast!,

                  icmp!, fcmp!, phi!, select!, call!, va_arg!, landingpad!,

                  globalstring!, globalstring_ptr!, isnull!, isnotnull!, ptrdiff!


# terminator instructions

ret!(builder::IRBuilder) =
    Instruction(API.LLVMBuildRetVoid(builder))

ret!(builder::IRBuilder, V::Value) =
    Instruction(API.LLVMBuildRet(builder, V))

ret!(builder::IRBuilder, RetVals::Vector{<:Value}) =
    Instruction(API.LLVMBuildAggregateRet(builder, RetVals, length(RetVals)))

br!(builder::IRBuilder, Dest::BasicBlock) =
    Instruction(API.LLVMBuildBr(builder, Dest))

br!(builder::IRBuilder, If::Value, Then::BasicBlock, Else::BasicBlock) =
    Instruction(API.LLVMBuildCondBr(builder, If, Then, Else))

switch!(builder::IRBuilder, V::Value, Else::BasicBlock, NumCases::Integer=10) =
    Instruction(API.LLVMBuildSwitch(builder, V, Else, NumCases))

indirectbr!(builder::IRBuilder, Addr::Value, NumDests::Integer=10) =
    Instruction(API.LLVMBuildIndirectBr(builder, Addr, NumDests))

function invoke!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::AbstractVector{<:Value},
                 Then::BasicBlock, Catch::BasicBlock, Name::String="")
    Instruction(API.LLVMBuildInvoke2(builder, Ty, Fn, as_vector(Args), length(Args), Then,
                                     Catch, Name))
end

resume!(builder::IRBuilder, Exn::Value) =
    Instruction(API.LLVMBuildResume(builder, Exn))

unreachable!(builder::IRBuilder) =
    Instruction(API.LLVMBuildUnreachable(builder))


# binary operations

binop!(builder::IRBuilder, Op::API.LLVMOpcode, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildBinOp(builder, Op, LHS, RHS, Name))

add!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildAdd(builder, LHS, RHS, Name))

nswadd!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNSWAdd(builder, LHS, RHS, Name))

nuwadd!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNUWAdd(builder, LHS, RHS, Name))

fadd!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFAdd(builder, LHS, RHS, Name))

sub!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildSub(builder, LHS, RHS, Name))

nswsub!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNSWSub(builder, LHS, RHS, Name))

nuwsub!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNUWSub(builder, LHS, RHS, Name))

fsub!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFSub(builder, LHS, RHS, Name))

mul!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildMul(builder, LHS, RHS, Name))

nswmul!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNSWMul(builder, LHS, RHS, Name))

nuwmul!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildNUWMul(builder, LHS, RHS, Name))

fmul!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFMul(builder, LHS, RHS, Name))

udiv!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildUDiv(builder, LHS, RHS, Name))

sdiv!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildSDiv(builder, LHS, RHS, Name))

exactudiv!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildExactUDiv(builder, LHS, RHS, Name))

exactsdiv!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildExactSDiv(builder, LHS, RHS, Name))

fdiv!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFDiv(builder, LHS, RHS, Name))

urem!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildURem(builder, LHS, RHS, Name))

srem!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildSRem(builder, LHS, RHS, Name))

frem!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFRem(builder, LHS, RHS, Name))


# bitwise binary operations

shl!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildShl(builder, LHS, RHS, Name))

lshr!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildLShr(builder, LHS, RHS, Name))

ashr!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildAShr(builder, LHS, RHS, Name))

and!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildAnd(builder, LHS, RHS, Name))

or!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildOr(builder, LHS, RHS, Name))

xor!(builder::IRBuilder, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildXor(builder, LHS, RHS, Name))


# vector operations

extract_element!(builder::IRBuilder, VecVal::Value, Index::Value, Name::String="") =
    Value(API.LLVMBuildExtractElement(builder, VecVal, Index, Name))

insert_element!(builder::IRBuilder, VecVal::Value, EltVal::Value, Index::Value, Name::String="") =
    Value(API.LLVMBuildInsertElement(builder, VecVal, EltVal, Index, Name))

shuffle_vector!(builder::IRBuilder, V1::Value, V2::Value, Mask::Value, Name::String="") =
    Value(API.LLVMBuildShuffleVector(builder, V1, V2, Mask, Name))


# aggregate operations

# check that indices select an element of an aggregate type, as LLVM asserts this
function check_aggregate_indices(typ::LLVMType, indices)
    isempty(indices) && throw(ArgumentError("At least one index is required"))
    for idx in indices
        n = if typ isa StructType
            length(typ.elements)
        elseif typ isa ArrayType
            array_length(typ)
        else
            throw(ArgumentError("Cannot index into non-aggregate type $typ"))
        end
        0 <= idx < n && idx <= typemax(Cuint) ||
            throw(ArgumentError("Index $idx is out of bounds for type $typ"))
        typ = typ isa StructType ? typ.elements[idx+1] : element_type(typ)
    end
    return typ
end

function check_inserted_value(agg::Value, val::Value, indices)
    elty = check_aggregate_indices(value_type(agg), indices)
    value_type(val) == elty ||
        throw(ArgumentError("Cannot insert a value of type $(value_type(val)) into an element of type $elty"))
end

"""
    extract_value!(builder::IRBuilder, agg::Value, index::Integer, [name::String])
    extract_value!(builder::IRBuilder, agg::Value, indices::AbstractVector{<:Integer},
                   [name::String])

Extract an element from an aggregate value. The zero-based indices select the element,
like in textual IR: e.g., `extract_value!(builder, agg, [1, 0])` extracts the first element
of the second element of `agg`.
"""
function extract_value!(builder::IRBuilder, AggVal::Value, Index::Integer, Name::String="")
    check_aggregate_indices(value_type(AggVal), (Index,))
    Value(API.LLVMBuildExtractValue(builder, AggVal, Index, Name))
end

function extract_value!(builder::IRBuilder, AggVal::Value,
                        Indices::AbstractVector{<:Integer}, Name::String="")
    check_aggregate_indices(value_type(AggVal), Indices)
    idxs = Vector{Cuint}(Indices)
    Value(API.LLVMExtraBuildExtractValue(builder, AggVal, idxs, length(idxs), Name))
end

"""
    insert_value!(builder::IRBuilder, agg::Value, val::Value, index::Integer,
                  [name::String])
    insert_value!(builder::IRBuilder, agg::Value, val::Value,
                  indices::AbstractVector{<:Integer}, [name::String])

Insert a value into an aggregate value, returning the updated aggregate. The zero-based
indices select the element to replace, like for [`extract_value!`](@ref).
"""
function insert_value!(builder::IRBuilder, AggVal::Value, EltVal::Value, Index::Integer,
                       Name::String="")
    check_inserted_value(AggVal, EltVal, (Index,))
    Value(API.LLVMBuildInsertValue(builder, AggVal, EltVal, Index, Name))
end

function insert_value!(builder::IRBuilder, AggVal::Value, EltVal::Value,
                       Indices::AbstractVector{<:Integer}, Name::String="")
    check_inserted_value(AggVal, EltVal, Indices)
    idxs = Vector{Cuint}(Indices)
    Value(API.LLVMExtraBuildInsertValue(builder, AggVal, EltVal, idxs, length(idxs), Name))
end


# memory access and addressing operations

"""
    alloca!(builder::IRBuilder, T::LLVMType, name::String=""; align=nothing)

Allocate stack memory for a value of type `T`. By default, the allocation is aligned to the
preferred alignment of `T`; use `align` to specify a different alignment in bytes.
"""
function alloca!(builder::IRBuilder, Ty::LLVMType, Name::String=""; align=nothing)
    check_alignment(align)
    inst = Instruction(API.LLVMBuildAlloca(builder, Ty, Name))
    align === nothing || alignment!(inst, align)
    return inst
end

"""
    array_alloca!(builder::IRBuilder, T::LLVMType, count::Value, name::String="";
                  align=nothing)

Allocate stack memory for `count` values of type `T`. See [`alloca!`](@ref) for the meaning
of `align`.
"""
function array_alloca!(builder::IRBuilder, Ty::LLVMType, Val::Value, Name::String="";
                       align=nothing)
    check_alignment(align)
    inst = Instruction(API.LLVMBuildArrayAlloca(builder, Ty, Val, Name))
    align === nothing || alignment!(inst, align)
    return inst
end

malloc!(builder::IRBuilder, Ty::LLVMType, Name::String="") =
    Instruction(API.LLVMBuildMalloc(builder, Ty, Name))

array_malloc!(builder::IRBuilder, Ty::LLVMType, Val::Value, Name::String="") =
    Instruction(API.LLVMBuildArrayMalloc(builder, Ty, Val, Name))

memset!(builder::IRBuilder, Ptr::Value, Val::Value, Len::Value, Align::Integer) =
    Instruction(API.LLVMBuildMemSet(builder, Ptr, Val, Len, Align))

memcpy!(builder::IRBuilder, Dst::Value, DstAlign::Integer, Src::Value, SrcAlign::Integer, Size::Value) =
    Instruction(API.LLVMBuildMemCpy(builder, Dst, DstAlign, Src, SrcAlign, Size))

memmove!(builder::IRBuilder, Dst::Value, DstAlign::Integer, Src::Value, SrcAlign::Integer,
         Size::Value) =
    Instruction(API.LLVMBuildMemMove(builder, Dst, DstAlign, Src, SrcAlign, Size))

free!(builder::IRBuilder, PointerVal::Value) =
    Instruction(API.LLVMBuildFree(builder, PointerVal))

## memory accesses and atomics

# The keyword arguments of the builders below are validated before creating instructions,
# throwing an ArgumentError for IR that no supported version of LLVM accepts. Rules that
# depend on the LLVM version (e.g. vector operands) or the data layout are left to the
# verifier.

const NotAtomic = API.LLVMAtomicOrderingNotAtomic
const Unordered = API.LLVMAtomicOrderingUnordered
const Acquire = API.LLVMAtomicOrderingAcquire
const Release = API.LLVMAtomicOrderingRelease
const AcquireRelease = API.LLVMAtomicOrderingAcquireRelease

# `nothing` is the default, system scope
atomic_scope(::Nothing) = SyncScope(1)
atomic_scope(scope::SyncScope) = scope
atomic_scope(name::Union{AbstractString,Symbol}) = SyncScope(String(name))

# atomic accesses must be of a byte-sized power-of-two size. only integers are checked, as
# the size of other types can depend on the data layout.
function check_atomic_type(@nospecialize(T::LLVMType), what::String)
    if T isa StructType || T isa ArrayType || T isa VoidType || T isa LabelType
        throw(ArgumentError("$what does not support values of type $(string(T))"))
    end
    if T isa IntegerType && !(width(T) >= 8 && ispow2(width(T)))
        throw(ArgumentError("$what requires a power-of-two number of bytes, got $(string(T))"))
    end
end

function set_access_flags!(inst::Instruction, ordering, scope, align, volatile)
    align === nothing || alignment!(inst, align)
    if ordering != NotAtomic
        ordering!(inst, ordering)
        scope === nothing || syncscope!(inst, atomic_scope(scope))
    end
    volatile && volatile!(inst, true)
    return inst
end

"""
    load!(builder::IRBuilder, T::LLVMType, ptr::Value, name::String="";
          ordering=LLVM.AtomicOrdering.NotAtomic, scope=nothing, align=nothing,
          volatile=false)

Load a value of type `T` from `ptr`. The load is atomic if an `ordering` other than
`not_atomic` is given, in the synchronization `scope` (a [`SyncScope`](@ref), its name, or
`nothing` for the default system scope). By default, the load is aligned to the ABI
alignment of `T`.
"""
function load!(builder::IRBuilder, Ty::LLVMType, PointerVal::Value, Name::String="";
               ordering::API.LLVMAtomicOrdering=NotAtomic, scope=nothing, align=nothing,
               volatile::Bool=false)
    if ordering != NotAtomic
        (ordering == Release || ordering == AcquireRelease) &&
            throw(ArgumentError("Atomic loads cannot have release semantics, got $ordering"))
        check_atomic_type(Ty, "An atomic load")
    elseif scope !== nothing && atomic_scope(scope) != SyncScope(1)
        throw(ArgumentError("Non-atomic loads cannot have a synchronization scope"))
    end
    check_alignment(align)
    inst = Instruction(API.LLVMBuildLoad2(builder, Ty, PointerVal, Name))
    set_access_flags!(inst, ordering, scope, align, volatile)
end

"""
    store!(builder::IRBuilder, val::Value, ptr::Value;
           ordering=LLVM.AtomicOrdering.NotAtomic, scope=nothing, align=nothing,
           volatile=false)

Store `val` to `ptr`. See [`load!`](@ref) for the meaning of the keyword arguments.
"""
function store!(builder::IRBuilder, Val::Value, Ptr::Value;
                ordering::API.LLVMAtomicOrdering=NotAtomic, scope=nothing, align=nothing,
                volatile::Bool=false)
    if ordering != NotAtomic
        (ordering == Acquire || ordering == AcquireRelease) &&
            throw(ArgumentError("Atomic stores cannot have acquire semantics, got $ordering"))
        check_atomic_type(value_type(Val), "An atomic store")
    elseif scope !== nothing && atomic_scope(scope) != SyncScope(1)
        throw(ArgumentError("Non-atomic stores cannot have a synchronization scope"))
    end
    check_alignment(align)
    inst = Instruction(API.LLVMBuildStore(builder, Val, Ptr))
    set_access_flags!(inst, ordering, scope, align, volatile)
end

"""
    fence!(builder::IRBuilder, ordering::LLVM.AtomicOrdering.T; scope=nothing)

Create a fence with the given ordering, which must be `acquire`, `release`, `acq_rel` or
`seq_cst`, in the given synchronization `scope` (see [`load!`](@ref)).
"""
function fence!(builder::IRBuilder, ordering::API.LLVMAtomicOrdering,
                singleThread::Bool=false, Name::String=""; scope=nothing)
    check_fence_ordering(ordering)
    if scope === nothing
        Instruction(API.LLVMBuildFence(builder, ordering, singleThread, Name))
    else
        singleThread && throw(ArgumentError("Cannot specify both singleThread and a scope"))
        fence!(builder, ordering, atomic_scope(scope), Name)
    end
end

function fence!(builder::IRBuilder, ordering::API.LLVMAtomicOrdering, syncscope::SyncScope,
                Name::String="")
    check_fence_ordering(ordering)
    Instruction(API.LLVMBuildFenceSyncScope(builder, ordering, syncscope, Name))
end

check_available(op::API.LLVMAtomicRMWBinOp) =
    isavailable(op) ||
        throw(ArgumentError("atomicrmw operation $(Integer(op)) is not supported by LLVM $(version())"))

function atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value, Val::Value,
                     ordering::API.LLVMAtomicOrdering, singleThread::Bool)
    check_available(op)
    # only LLVMExtra's builder knows about operations that the C API doesn't define yet
    if version() < v"19" && Integer(op) > Integer(API.LLVMAtomicRMWBinOpFMin)
        scope = SyncScope(singleThread ? 0 : 1)     # SyncScope::SingleThread or ::System
        return atomic_rmw!(builder, op, Ptr, Val, ordering, scope)
    end
    Instruction(API.LLVMBuildAtomicRMW(builder, op, Ptr, Val, ordering, singleThread))
end

function atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value, Val::Value,
                     ordering::API.LLVMAtomicOrdering, syncscope::SyncScope)
    check_available(op)
    @static if v"16" <= version() < v"19"
        # operations that this C API doesn't define have to be passed as integers
        if Integer(op) > Integer(API.LLVMAtomicRMWBinOpFMin)
            return Instruction(API.LLVMExtraBuildAtomicRMWSyncScope(builder, Integer(op), Ptr,
                                                                   Val, ordering, syncscope))
        end
    end
    Instruction(API.LLVMBuildAtomicRMWSyncScope(builder, op, Ptr, Val, ordering, syncscope))
end

"""
    atomic_rmw!(builder::IRBuilder, op::LLVM.AtomicRMWBinOp.T, ptr::Value, val::Value,
                ordering::LLVM.AtomicOrdering.T; scope=nothing, align=nothing, volatile=false)

Atomically apply the operation `op` to the value at `ptr` and `val`, returning the old
value. The operation must be supported by the version of LLVM in use (see
[`LLVM.isavailable`](@ref)), and the ordering at least `monotonic`. See [`load!`](@ref)
for the meaning of the other keyword arguments; by default, the operation is aligned to the
size of the value.
"""
function atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value, Val::Value,
                     ordering::API.LLVMAtomicOrdering; scope=nothing, align=nothing,
                     volatile::Bool=false)
    check_available(op)
    is_stronger(ordering, Unordered) ||
        throw(ArgumentError("atomicrmw requires an ordering of at least monotonic, got $ordering"))
    T = value_type(Val)
    scalar_T = T isa VectorType ? element_type(T) : T
    if op == API.LLVMAtomicRMWBinOpXchg
        scalar_T isa Union{IntegerType,FloatingPointType,PointerType} ||
            throw(ArgumentError("atomicrmw xchg requires an integer, floating-point or pointer value, got $(string(T))"))
    elseif is_fp_rmw(op)
        scalar_T isa FloatingPointType ||
            throw(ArgumentError("atomicrmw operation $op requires a floating-point value, got $(string(T))"))
    else
        scalar_T isa IntegerType ||
            throw(ArgumentError("atomicrmw operation $op requires an integer value, got $(string(T))"))
    end
    check_atomic_type(T, "atomicrmw")
    check_alignment(align)
    inst = if scope === nothing
        atomic_rmw!(builder, op, Ptr, Val, ordering, false)
    else
        atomic_rmw!(builder, op, Ptr, Val, ordering, atomic_scope(scope))
    end
    align === nothing || alignment!(inst, align)
    volatile && volatile!(inst, true)
    return inst
end

"""
    atomic_cmpxchg!(builder::IRBuilder, ptr::Value, cmp::Value, new::Value,
                    success::LLVM.AtomicOrdering.T,
                    failure::LLVM.AtomicOrdering.T=strongest_failure_ordering(success);
                    scope=nothing, align=nothing, volatile=false, weak=false)

Atomically compare the value at `ptr` with `cmp`, and if equal, replace it with `new`.
Returns a `{T, i1}` pair of the old value and whether it was replaced. The values must be
integers or pointers (compare floating-point values by bitcasting them to integers). Both
orderings must be at least `monotonic`, and the failure ordering cannot be `release` or
`acq_rel`. A `weak` cmpxchg may fail spuriously. See [`load!`](@ref) for the meaning of the
other keyword arguments; by default, the operation is aligned to the size of the value.
"""
function atomic_cmpxchg!(builder::IRBuilder, Ptr::Value, Cmp::Value, New::Value,
                         success::API.LLVMAtomicOrdering,
                         failure::API.LLVMAtomicOrdering=strongest_failure_ordering(success);
                         scope=nothing, align=nothing, volatile::Bool=false, weak::Bool=false)
    (is_stronger(success, Unordered) && is_stronger(failure, Unordered)) ||
        throw(ArgumentError("cmpxchg requires orderings of at least monotonic, got $success and $failure"))
    (failure == Release || failure == AcquireRelease) &&
        throw(ArgumentError("The failure ordering of a cmpxchg cannot be release or acq_rel, got $failure"))
    T = value_type(Cmp)
    T == value_type(New) ||
        throw(ArgumentError("cmpxchg requires values of the same type, got $(string(T)) and $(string(value_type(New)))"))
    T isa Union{IntegerType,PointerType} ||
        throw(ArgumentError("cmpxchg requires integer or pointer values, got $(string(T))"))
    check_atomic_type(T, "cmpxchg")
    check_alignment(align)
    inst = if scope === nothing
        atomic_cmpxchg!(builder, Ptr, Cmp, New, success, failure, false)
    else
        atomic_cmpxchg!(builder, Ptr, Cmp, New, success, failure, atomic_scope(scope))
    end
    align === nothing || alignment!(inst, align)
    volatile && volatile!(inst, true)
    weak && weak!(inst, true)
    return inst
end

atomic_cmpxchg!(builder::IRBuilder, Ptr::Value, Cmp::Value, New::Value,
                SuccessOrdering::API.LLVMAtomicOrdering,
                FailureOrdering::API.LLVMAtomicOrdering, SingleThread::Bool) =
    Instruction(API.LLVMBuildAtomicCmpXchg(builder, Ptr, Cmp, New, SuccessOrdering,
                                           FailureOrdering, SingleThread))

atomic_cmpxchg!(builder::IRBuilder, Ptr::Value, Cmp::Value, New::Value,
                SuccessOrdering::API.LLVMAtomicOrdering,
                FailureOrdering::API.LLVMAtomicOrdering, syncscope::SyncScope) =
    Instruction(API.LLVMBuildAtomicCmpXchgSyncScope(builder, Ptr, Cmp, New, SuccessOrdering,
                                                    FailureOrdering, syncscope))

function gep!(builder::IRBuilder, Ty::LLVMType, Pointer::Value, Indices::Vector{<:Value},
              Name::String="")
    Value(API.LLVMBuildGEP2(builder, Ty, Pointer, Indices, length(Indices), Name))
end

function inbounds_gep!(builder::IRBuilder, Ty::LLVMType, Pointer::Value,
                       Indices::Vector{<:Value}, Name::String="")
    Value(API.LLVMBuildInBoundsGEP2(builder, Ty, Pointer, Indices, length(Indices), Name))
end

function struct_gep!(builder::IRBuilder, Ty::LLVMType, Pointer::Value, Idx, Name::String="")
    Value(API.LLVMBuildStructGEP2(builder, Ty, Pointer, Idx, Name))
end

# conversion operations

trunc!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildTrunc(builder, Val, DestTy, Name))

zext!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildZExt(builder, Val, DestTy, Name))

sext!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildSExt(builder, Val, DestTy, Name))

fptoui!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildFPToUI(builder, Val, DestTy, Name))

fptosi!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildFPToSI(builder, Val, DestTy, Name))

uitofp!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildUIToFP(builder, Val, DestTy, Name))

sitofp!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildSIToFP(builder, Val, DestTy, Name))

fptrunc!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildFPTrunc(builder, Val, DestTy, Name))

fpext!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildFPExt(builder, Val, DestTy, Name))

ptrtoint!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildPtrToInt(builder, Val, DestTy, Name))

inttoptr!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildIntToPtr(builder, Val, DestTy, Name))

bitcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildBitCast(builder, Val, DestTy, Name))

addrspacecast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildAddrSpaceCast(builder, Val, DestTy, Name))

zextorbitcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildZExtOrBitCast(builder, Val, DestTy, Name))

sextorbitcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildSExtOrBitCast(builder, Val, DestTy, Name))

truncorbitcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildTruncOrBitCast(builder, Val, DestTy, Name))

cast!(builder::IRBuilder, Op::API.LLVMOpcode, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildCast(builder, Op, Val, DestTy, Name))

# XXX: make this error with opaque pointers?
pointercast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildPointerCast(builder, Val, DestTy, Name))

intcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildIntCast(builder, Val, DestTy, Name))

fpcast!(builder::IRBuilder, Val::Value, DestTy::LLVMType, Name::String="") =
    Value(API.LLVMBuildFPCast(builder, Val, DestTy, Name))


# other operations

icmp!(builder::IRBuilder, Op::API.LLVMIntPredicate, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildICmp(builder, Op, LHS, RHS, Name))

fcmp!(builder::IRBuilder, Op::API.LLVMRealPredicate, LHS::Value, RHS::Value, Name::String="") =
    Value(API.LLVMBuildFCmp(builder, Op, LHS, RHS, Name))

phi!(builder::IRBuilder, Ty::LLVMType, Name::String="") =
    Instruction(API.LLVMBuildPhi(builder, Ty, Name))

select!(builder::IRBuilder, If::Value, Then::Value, Else::Value, Name::String="") =
    Value(API.LLVMBuildSelect(builder, If, Then, Else, Name))

function call!(builder::IRBuilder, Ty::LLVMType, Fn::Value,
               Args::AbstractVector{<:Value}=Value[], Name::String="")
    Instruction(API.LLVMBuildCall2(builder, Ty, Fn, as_vector(Args), length(Args), Name))
end

function call!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::AbstractVector{<:Value},
               Bundles::Vector{OperandBundle}, Name::String="")
    Instruction(API.LLVMBuildCallWithOperandBundles(builder, Ty, Fn, as_vector(Args),
                                                    length(Args), Bundles, length(Bundles),
                                                    Name))
end

# convenience function to be able to call `call!` with a `call.operand_bundles` argument
call!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::AbstractVector{<:Value},
      Bundles::OperandBundleIterator, Name::String="") =
    call!(builder, Ty, Fn, Args, collect(Bundles), Name)

va_arg!(builder::IRBuilder, List::Value, Ty::LLVMType, Name::String="") =
    Instruction(API.LLVMBuildVAArg(builder, List, Ty, Name))

landingpad!(builder::IRBuilder, Ty::LLVMType, PersFn::Value, NumClauses::Integer,
            Name::String="") =
    Instruction(API.LLVMBuildLandingPad(builder, Ty, PersFn, NumClauses, Name))

neg!(builder::IRBuilder, V::Value, Name::String="") =
    Value(API.LLVMBuildNeg(builder, V, Name))

nswneg!(builder::IRBuilder, V::Value, Name::String="") =
    Value(API.LLVMBuildNSWNeg(builder, V, Name))

fneg!(builder::IRBuilder, V::Value, Name::String="") =
    Value(API.LLVMBuildFNeg(builder, V, Name))

not!(builder::IRBuilder, V::Value, Name::String="") =
    Value(API.LLVMBuildNot(builder, V, Name))


# other build methods

#globalstring!(builder::IRBuilder, Str::String, Name::String="") =
#    Value(API.LLVMBuildGlobalString(builder, Str, Name))

# re-implementation for flexibility (exposing addrspace, add_null)
function globalstring!(mod::LLVM.Module, str::String, name::String="";
                       addrspace::Union{Integer,Nothing}=nothing, add_null::Bool=true)
    bytes = Vector{UInt8}(str)
    if add_null
        push!(bytes, 0x00)
    end
    constant = ConstantDataArray(bytes)

    gv = GlobalVariable(mod, value_type(constant), name,
                        something(addrspace, globals_addrspace(datalayout(mod))))
    alignment!(gv, 1)
    unnamed_addr!(gv, API.LLVMGlobalUnnamedAddr)
    initializer!(gv, constant)
    constant!(gv, true)
    linkage!(gv, LLVM.API.LLVMPrivateLinkage)

    return gv
end
function globalstring!(builder::IRBuilder, args...; kwargs...)
    mod = parent(parent(position(builder)))
    globalstring!(mod, args...; kwargs...)
end

# with opaque pointers, a pointer to the string is the global itself, so this is identical
# to `globalstring!` (which is why LLVM 20 deprecated `LLVMBuildGlobalStringPtr`). only
# contexts with typed pointers need the GEP to get an `i8*`.
function globalstring_ptr!(args...; kwargs...)
    gv = globalstring!(args...; kwargs...)
    zero = LLVM.ConstantInt(LLVM.IntType(32), 0)
    indices = [zero, zero]
    const_inbounds_gep(global_value_type(gv), gv, indices)
end

isnull!(builder::IRBuilder, Val::Value, Name::String="") =
    Value(API.LLVMBuildIsNull(builder, Val, Name))

isnotnull!(builder::IRBuilder, Val::Value, Name::String="") =
    Value(API.LLVMBuildIsNotNull(builder, Val, Name))

function ptrdiff!(builder::IRBuilder, Ty::LLVMType, LHS::Value, RHS::Value, Name::String="")
    Value(API.LLVMBuildPtrDiff2(builder, Ty, LHS, RHS, Name))
end

@specialize
