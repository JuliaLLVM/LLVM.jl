# An instruction builder represents a point within a basic block and is the exclusive means
# of building instructions using the C interface.

export IRBuilder,
       position!,
       debuglocation, debuglocation!

"""
    IRBuilder

An instruction builder, which is used to build instructions within a basic block.
"""
@checked struct IRBuilder
    ref::API.LLVMBuilderRef
end

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

"""
    context(builder::IRBuilder)

Get the context associated with an instruction builder.
"""
context(builder::IRBuilder) = Context(API.LLVMGetBuilderContext(builder))

function IRBuilder(f::Core.Function, args...; kwargs...)
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
    position!(builder::IRBuilder, inst::Instruction)

Position the instruction builder before the given instruction.
"""
position!(builder::IRBuilder, inst::Instruction) =
    API.LLVMPositionBuilderBefore(builder, inst)

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

"""
    debuglocation(builder::IRBuilder)

Get the current debug location of the instruction builder, or `nothing` if no location is
set.
"""
function debuglocation(builder::IRBuilder)
    ref = API.LLVMGetCurrentDebugLocation2(builder)
    ref == C_NULL ? nothing : Metadata(ref)
end

"""
    debuglocation!(builder::IRBuilder)

Clear the current debug location of the instruction builder.
"""
debuglocation!(builder::IRBuilder) =
    API.LLVMSetCurrentDebugLocation2(builder, C_NULL)

"""
    debuglocation!(builder::IRBuilder, loc)

Set the current debug location of the instruction builder to `loc`, which can be a
`Metadata` or `MetadataAsValue`.
"""
debuglocation!(builder::IRBuilder, loc::Union{Metadata,MetadataAsValue})
debuglocation!(builder::IRBuilder, loc::Metadata) =
    API.LLVMSetCurrentDebugLocation2(builder, loc)
debuglocation!(builder::IRBuilder, loc::MetadataAsValue) =
    API.LLVMSetCurrentDebugLocation2(builder, Metadata(loc))

"""
    debuglocation!(builder::IRBuilder, inst::Instruction)

Set the current debug location of the instruction builder to the location of the given
instruction.
"""
debuglocation!(builder::IRBuilder, inst::Instruction) =
    API.LLVMSetInstDebugLocation(builder, inst)


## build methods

# TODO/IDEAS:
# - dynamic dispatch based on `value_type` (eg. disambiguating `add!` and `fadd!`)

# NOTE: the return values for these operations are, according to the C API, always a Value.
#       however, the C++ API learns us that we can be more strict.

export ret!, br!, switch!, indirectbr!, invoke!, resume!, unreachable!,

       binop!, add!, nswadd!, nuwadd!, fadd!, sub!, nswsub!, nuwsub!, fsub!, mul!, nswmul!,
       nuwmul!, fmul!, udiv!, sdiv!, exactsdiv!, fdiv!, urem!, srem!, frem!, neg!, nswneg!,
       nuwneg!, fneg!,

       shl!, lshr!, ashr!, and!, or!, xor!, not!,

       extract_element!, insert_element!, shuffle_vector!,

       extract_value!, insert_value!,

       alloca!, array_alloca!, malloc!, array_malloc!, memset!, memcpy!, memmove!, free!,
       load!, store!, fence!, atomic_rmw!, atomic_cmpxchg!, gep!, inbounds_gep!, struct_gep!,

       trunc!, zext!, sext!, fptoui!, fptosi!, uitofp!, sitofp!, fptrunc!, fpext!,
       ptrtoint!, inttoptr!, bitcast!, addrspacecast!, zextorbitcast!, sextorbitcast!,
       truncorbitcast!, cast!, pointercast!, intcast!, fpcast!,

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

function invoke!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::Vector{<:Value},
                 Then::BasicBlock, Catch::BasicBlock, Name::String="")
    Instruction(API.LLVMBuildInvoke2(builder, Ty, Fn, Args, length(Args), Then, Catch, Name))
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

extract_value!(builder::IRBuilder, AggVal::Value, Index, Name::String="") =
    Value(API.LLVMBuildExtractValue(builder, AggVal, Index, Name))

insert_value!(builder::IRBuilder, AggVal::Value, EltVal::Value, Index, Name::String="") =
    Value(API.LLVMBuildInsertValue(builder, AggVal, EltVal, Index, Name))


# memory access and addressing operations

alloca!(builder::IRBuilder, Ty::LLVMType, Name::String="") =
    Instruction(API.LLVMBuildAlloca(builder, Ty, Name))

array_alloca!(builder::IRBuilder, Ty::LLVMType, Val::Value, Name::String="") =
    Instruction(API.LLVMBuildArrayAlloca(builder, Ty, Val, Name))

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

function load!(builder::IRBuilder, Ty::LLVMType, PointerVal::Value, Name::String="")
    @static if version() >= v"11"
        Instruction(API.LLVMBuildLoad2(builder, Ty, PointerVal, Name))
    else
        Instruction(API.LLVMBuildLoad(builder, PointerVal, Name))
    end
end

store!(builder::IRBuilder, Val::Value, Ptr::Value) =
    Instruction(API.LLVMBuildStore(builder, Val, Ptr))

fence!(builder::IRBuilder, ordering::API.LLVMAtomicOrdering, singleThread::Bool=false,
       Name::String="") =
    Instruction(API.LLVMBuildFence(builder, ordering, singleThread, Name))

fence!(builder::IRBuilder, ordering::API.LLVMAtomicOrdering, syncscope::SyncScope,
       Name::String="") =
    Instruction(API.LLVMBuildFenceSyncScope(builder, ordering, syncscope, Name))

atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value, Val::Value,
            ordering::API.LLVMAtomicOrdering, singleThread::Bool) =
    Instruction(API.LLVMBuildAtomicRMW(builder, op, Ptr, Val, ordering, singleThread))

atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value, Val::Value,
            ordering::API.LLVMAtomicOrdering, syncscope::SyncScope) =
    Instruction(API.LLVMBuildAtomicRMWSyncScope(builder, op, Ptr, Val, ordering, syncscope))

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
    @static if version() >= v"11"
        Value(API.LLVMBuildGEP2(builder, Ty, Pointer, Indices, length(Indices), Name))
    else
        Value(API.LLVMBuildGEP(builder, Pointer, Indices, length(Indices), Name))
    end
end

function inbounds_gep!(builder::IRBuilder, Ty::LLVMType, Pointer::Value,
                       Indices::Vector{<:Value}, Name::String="")
    @static if version() >= v"11"
        Value(API.LLVMBuildInBoundsGEP2(builder, Ty, Pointer, Indices, length(Indices), Name))
    else
        Value(API.LLVMBuildInBoundsGEP(builder, Pointer, Indices, length(Indices), Name))
    end
end

function struct_gep!(builder::IRBuilder, Ty::LLVMType, Pointer::Value, Idx, Name::String="")
    @static if version() >= v"11"
        Value(API.LLVMBuildStructGEP2(builder, Ty, Pointer, Idx, Name))
    else
        Value(API.LLVMBuildStructGEP(builder, Pointer, Idx, Name))
    end
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

function call!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::Vector{<:Value}=Value[],
               Name::String="")
    @static if version() >= v"11"
        Instruction(API.LLVMBuildCall2(builder, Ty, Fn, Args, length(Args), Name))
    else
        Instruction(API.LLVMBuildCall(builder, Fn, Args, length(Args), Name))
    end
end

function call!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::Vector{<:Value},
               Bundles::Vector{OperandBundle}, Name::String="")
    Instruction(API.LLVMBuildCallWithOperandBundles(builder, Ty, Fn, Args, length(Args), Bundles,
                                                    length(Bundles), Name))
end

# convenience function to be able to call `call!` with an `operand_bundles(call)` argument
call!(builder::IRBuilder, Ty::LLVMType, Fn::Value, Args::Vector{<:Value},
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

nuwneg!(builder::IRBuilder, V::Value, Name::String="") =
    Value(API.LLVMBuildNUWNeg(builder, V, Name))

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
    unnamed_addr!(gv, true)
    initializer!(gv, constant)
    constant!(gv, true)
    linkage!(gv, LLVM.API.LLVMPrivateLinkage)

    return gv
end
function globalstring!(builder::IRBuilder, args...; kwargs...)
    mod = parent(parent(position(builder)))
    globalstring!(mod, args...; kwargs...)
end

#globalstring_ptr!(builder::IRBuilder, Str::String, Name::String="") =
#    Value(API.LLVMBuildGlobalStringPtr(builder, Str, Name))

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
