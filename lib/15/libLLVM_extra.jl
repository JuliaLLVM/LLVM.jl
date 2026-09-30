using CEnum: CEnum, @cenum

function LLVMExtraInitializeNativeTarget()
    ccall((:LLVMExtraInitializeNativeTarget, libLLVMExtra), LLVMBool, ())
end

function LLVMExtraInitializeNativeAsmParser()
    ccall((:LLVMExtraInitializeNativeAsmParser, libLLVMExtra), LLVMBool, ())
end

function LLVMExtraInitializeNativeAsmPrinter()
    ccall((:LLVMExtraInitializeNativeAsmPrinter, libLLVMExtra), LLVMBool, ())
end

function LLVMExtraInitializeNativeDisassembler()
    ccall((:LLVMExtraInitializeNativeDisassembler, libLLVMExtra), LLVMBool, ())
end

@cenum LLVMDebugEmissionKind::UInt32 begin
    LLVMDebugEmissionKindNoDebug = 0
    LLVMDebugEmissionKindFullDebug = 1
    LLVMDebugEmissionKindLineTablesOnly = 2
    LLVMDebugEmissionKindDebugDirectivesOnly = 3
end

function LLVMAppendToUsed(Mod, Values, Count)
    ccall((:LLVMAppendToUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMAppendToCompilerUsed(Mod, Values, Count)
    ccall((:LLVMAppendToCompilerUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMGetNumUsed(Mod)
    ccall((:LLVMGetNumUsed, libLLVMExtra), Csize_t, (LLVMModuleRef,), Mod)
end

function LLVMGetUsed(Mod, Dest)
    ccall((:LLVMGetUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}), Mod, Dest)
end

function LLVMGetNumCompilerUsed(Mod)
    ccall((:LLVMGetNumCompilerUsed, libLLVMExtra), Csize_t, (LLVMModuleRef,), Mod)
end

function LLVMGetCompilerUsed(Mod, Dest)
    ccall((:LLVMGetCompilerUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}), Mod, Dest)
end

function LLVMRemoveFromUsed(Mod, Values, Count)
    ccall((:LLVMRemoveFromUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMRemoveFromCompilerUsed(Mod, Values, Count)
    ccall((:LLVMRemoveFromCompilerUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMDumpMetadata(MD)
    ccall((:LLVMDumpMetadata, libLLVMExtra), Cvoid, (LLVMMetadataRef,), MD)
end

function LLVMPrintMetadataToString(MD)
    ccall((:LLVMPrintMetadataToString, libLLVMExtra), Cstring, (LLVMMetadataRef,), MD)
end

function LLVMDIScopeGetName(File, Len)
    ccall((:LLVMDIScopeGetName, libLLVMExtra), Cstring, (LLVMMetadataRef, Ptr{Cuint}), File, Len)
end

function LLVMFunctionDeleteBody(Func)
    ccall((:LLVMFunctionDeleteBody, libLLVMExtra), Cvoid, (LLVMValueRef,), Func)
end

function LLVMDestroyConstant(Const)
    ccall((:LLVMDestroyConstant, libLLVMExtra), Cvoid, (LLVMValueRef,), Const)
end

function LLVMGetFunctionType(Fn)
    ccall((:LLVMGetFunctionType, libLLVMExtra), LLVMTypeRef, (LLVMValueRef,), Fn)
end

function LLVMGetGlobalValueType(Fn)
    ccall((:LLVMGetGlobalValueType, libLLVMExtra), LLVMTypeRef, (LLVMValueRef,), Fn)
end

function LLVMExtraMoveFunction(Fn, Mod, Before)
    ccall((:LLVMExtraMoveFunction, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMModuleRef, LLVMValueRef), Fn, Mod, Before)
end

function LLVMExtraMoveGlobal(GlobalVar, Mod, Before)
    ccall((:LLVMExtraMoveGlobal, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMModuleRef, LLVMValueRef), GlobalVar, Mod, Before)
end

function LLVMSetInitializer2(GlobalVar, ConstantVal)
    ccall((:LLVMSetInitializer2, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), GlobalVar, ConstantVal)
end

function LLVMSetPersonalityFn2(Fn, PersonalityFn)
    ccall((:LLVMSetPersonalityFn2, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Fn, PersonalityFn)
end

function LLVMGetMDString2(MD, Length)
    ccall((:LLVMGetMDString2, libLLVMExtra), Cstring, (LLVMMetadataRef, Ptr{Cuint}), MD, Length)
end

function LLVMGetMDNodeNumOperands2(MD)
    ccall((:LLVMGetMDNodeNumOperands2, libLLVMExtra), Cuint, (LLVMMetadataRef,), MD)
end

function LLVMGetMDNodeOperands2(MD, Dest)
    ccall((:LLVMGetMDNodeOperands2, libLLVMExtra), Cvoid, (LLVMMetadataRef, Ptr{LLVMMetadataRef}), MD, Dest)
end

function LLVMGetMDNodeOperand2(MD, I)
    ccall((:LLVMGetMDNodeOperand2, libLLVMExtra), LLVMMetadataRef, (LLVMMetadataRef, Cuint), MD, I)
end

function LLVMGetNamedMetadataNumOperands2(NMD)
    ccall((:LLVMGetNamedMetadataNumOperands2, libLLVMExtra), Cuint, (LLVMNamedMDNodeRef,), NMD)
end

function LLVMGetNamedMetadataOperands2(NMD, Dest)
    ccall((:LLVMGetNamedMetadataOperands2, libLLVMExtra), Cvoid, (LLVMNamedMDNodeRef, Ptr{LLVMMetadataRef}), NMD, Dest)
end

function LLVMGetNamedMetadataOperand2(NMD, I)
    ccall((:LLVMGetNamedMetadataOperand2, libLLVMExtra), LLVMMetadataRef, (LLVMNamedMDNodeRef, Cuint), NMD, I)
end

function LLVMAddNamedMetadataOperand2(NMD, Val)
    ccall((:LLVMAddNamedMetadataOperand2, libLLVMExtra), Cvoid, (LLVMNamedMDNodeRef, LLVMMetadataRef), NMD, Val)
end

function LLVMClearNamedMetadataOperands(NMD)
    ccall((:LLVMClearNamedMetadataOperands, libLLVMExtra), Cvoid, (LLVMNamedMDNodeRef,), NMD)
end

function LLVMSetNamedMetadataOperand2(NMD, I, Val)
    ccall((:LLVMSetNamedMetadataOperand2, libLLVMExtra), Cvoid, (LLVMNamedMDNodeRef, Cuint, LLVMMetadataRef), NMD, I, Val)
end

function LLVMReplaceMDNodeOperandWith2(MD, I, New)
    ccall((:LLVMReplaceMDNodeOperandWith2, libLLVMExtra), Cvoid, (LLVMMetadataRef, Cuint, LLVMMetadataRef), MD, I, New)
end

mutable struct LLVMOrcOpaqueIRCompileLayer end

const LLVMOrcIRCompileLayerRef = Ptr{LLVMOrcOpaqueIRCompileLayer}

function LLVMOrcIRCompileLayerEmit(IRLayer, MR, TSM)
    ccall((:LLVMOrcIRCompileLayerEmit, libLLVMExtra), Cvoid, (LLVMOrcIRCompileLayerRef, LLVMOrcMaterializationResponsibilityRef, LLVMOrcThreadSafeModuleRef), IRLayer, MR, TSM)
end

function LLVMDumpJitDylibToString(JD)
    ccall((:LLVMDumpJitDylibToString, libLLVMExtra), Cstring, (LLVMOrcJITDylibRef,), JD)
end

function LLVMExtraThreadSafeModuleGetModuleUnlocked(TSM)
    ccall((:LLVMExtraThreadSafeModuleGetModuleUnlocked, libLLVMExtra), LLVMModuleRef, (LLVMOrcThreadSafeModuleRef,), TSM)
end

function LLVMOrcRTDyldObjectLinkingLayerSetOverrideObjectFlagsWithResponsibilityFlags(RTDyldObjLinkingLayer, OverrideObjectFlags)
    ccall((:LLVMOrcRTDyldObjectLinkingLayerSetOverrideObjectFlagsWithResponsibilityFlags, libLLVMExtra), Cvoid, (LLVMOrcObjectLayerRef, LLVMBool), RTDyldObjLinkingLayer, OverrideObjectFlags)
end

function LLVMOrcRTDyldObjectLinkingLayerSetAutoClaimResponsibilityForObjectSymbols(RTDyldObjLinkingLayer, AutoClaimObjectSymbols)
    ccall((:LLVMOrcRTDyldObjectLinkingLayerSetAutoClaimResponsibilityForObjectSymbols, libLLVMExtra), Cvoid, (LLVMOrcObjectLayerRef, LLVMBool), RTDyldObjLinkingLayer, AutoClaimObjectSymbols)
end

function LLVMOrcRTDyldObjectLinkingLayerApplyTargetDefaults(RTDyldObjLinkingLayer, Triple)
    ccall((:LLVMOrcRTDyldObjectLinkingLayerApplyTargetDefaults, libLLVMExtra), Cvoid, (LLVMOrcObjectLayerRef, Cstring), RTDyldObjLinkingLayer, Triple)
end

function LLVMExtraDisposeRTDyldObjectLinkingLayer(RTDyldObjLinkingLayer)
    ccall((:LLVMExtraDisposeRTDyldObjectLinkingLayer, libLLVMExtra), Cvoid, (LLVMOrcObjectLayerRef,), RTDyldObjLinkingLayer)
end

@cenum LLVMCloneFunctionChangeType::UInt32 begin
    LLVMCloneFunctionChangeTypeLocalChangesOnly = 0
    LLVMCloneFunctionChangeTypeGlobalChanges = 1
    LLVMCloneFunctionChangeTypeDifferentModule = 2
    LLVMCloneFunctionChangeTypeClonedModule = 3
end

function LLVMCloneFunctionInto(NewFunc, OldFunc, ValueMap, ValueMapElements, Changes, NameSuffix, TypeMapper, TypeMapperData, Materializer, MaterializerData)
    ccall((:LLVMCloneFunctionInto, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, LLVMCloneFunctionChangeType, Cstring, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), NewFunc, OldFunc, ValueMap, ValueMapElements, Changes, NameSuffix, TypeMapper, TypeMapperData, Materializer, MaterializerData)
end

function LLVMCloneBasicBlock(BB, NameSuffix, ValueMap, ValueMapElements, F)
    ccall((:LLVMCloneBasicBlock, libLLVMExtra), LLVMBasicBlockRef, (LLVMBasicBlockRef, Cstring, Ptr{LLVMValueRef}, Cuint, LLVMValueRef), BB, NameSuffix, ValueMap, ValueMapElements, F)
end

mutable struct LLVMOpaqueOperandBundle end

const LLVMOperandBundleRef = Ptr{LLVMOpaqueOperandBundle}

function LLVMCreateOperandBundle(Tag, TagLen, Args, NumArgs)
    ccall((:LLVMCreateOperandBundle, libLLVMExtra), LLVMOperandBundleRef, (Cstring, Csize_t, Ptr{LLVMValueRef}, Cuint), Tag, TagLen, Args, NumArgs)
end

function LLVMDisposeOperandBundle(Bundle)
    ccall((:LLVMDisposeOperandBundle, libLLVMExtra), Cvoid, (LLVMOperandBundleRef,), Bundle)
end

function LLVMGetOperandBundleTag(Bundle, Len)
    ccall((:LLVMGetOperandBundleTag, libLLVMExtra), Cstring, (LLVMOperandBundleRef, Ptr{Csize_t}), Bundle, Len)
end

function LLVMGetNumOperandBundleArgs(Bundle)
    ccall((:LLVMGetNumOperandBundleArgs, libLLVMExtra), Cuint, (LLVMOperandBundleRef,), Bundle)
end

function LLVMGetOperandBundleArgAtIndex(Bundle, Index)
    ccall((:LLVMGetOperandBundleArgAtIndex, libLLVMExtra), LLVMValueRef, (LLVMOperandBundleRef, Cuint), Bundle, Index)
end

function LLVMGetNumOperandBundles(C)
    ccall((:LLVMGetNumOperandBundles, libLLVMExtra), Cuint, (LLVMValueRef,), C)
end

function LLVMGetOperandBundleAtIndex(C, Index)
    ccall((:LLVMGetOperandBundleAtIndex, libLLVMExtra), LLVMOperandBundleRef, (LLVMValueRef, Cuint), C, Index)
end

function LLVMBuildInvokeWithOperandBundles(arg1, Ty, Fn, Args, NumArgs, Then, Catch, Bundles, NumBundles, Name)
    ccall((:LLVMBuildInvokeWithOperandBundles, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, LLVMBasicBlockRef, LLVMBasicBlockRef, Ptr{LLVMOperandBundleRef}, Cuint, Cstring), arg1, Ty, Fn, Args, NumArgs, Then, Catch, Bundles, NumBundles, Name)
end

function LLVMBuildCallWithOperandBundles(arg1, arg2, Fn, Args, NumArgs, Bundles, NumBundles, Name)
    ccall((:LLVMBuildCallWithOperandBundles, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Ptr{LLVMOperandBundleRef}, Cuint, Cstring), arg1, arg2, Fn, Args, NumArgs, Bundles, NumBundles, Name)
end

function LLVMMetadataAsValue2(C, Metadata)
    ccall((:LLVMMetadataAsValue2, libLLVMExtra), LLVMValueRef, (LLVMContextRef, LLVMMetadataRef), C, Metadata)
end

function LLVMReplaceAllMetadataUsesWith(Old, New)
    ccall((:LLVMReplaceAllMetadataUsesWith, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Old, New)
end

function LLVMReplaceMDNodeOperandWith(V, Index, Replacement)
    ccall((:LLVMReplaceMDNodeOperandWith, libLLVMExtra), Cvoid, (LLVMValueRef, Cuint, LLVMMetadataRef), V, Index, Replacement)
end

function LLVMConstDataArray(ElementTy, Data, SizeInBytes)
    ccall((:LLVMConstDataArray, libLLVMExtra), LLVMValueRef, (LLVMTypeRef, Ptr{Cvoid}, Csize_t), ElementTy, Data, SizeInBytes)
end

function LLVMContextSupportsTypedPointers(C)
    ccall((:LLVMContextSupportsTypedPointers, libLLVMExtra), LLVMBool, (LLVMContextRef,), C)
end

mutable struct LLVMOpaqueDominatorTree end

const LLVMDominatorTreeRef = Ptr{LLVMOpaqueDominatorTree}

function LLVMCreateDominatorTree(Fn)
    ccall((:LLVMCreateDominatorTree, libLLVMExtra), LLVMDominatorTreeRef, (LLVMValueRef,), Fn)
end

function LLVMDisposeDominatorTree(Tree)
    ccall((:LLVMDisposeDominatorTree, libLLVMExtra), Cvoid, (LLVMDominatorTreeRef,), Tree)
end

function LLVMDominatorTreeInstructionDominates(Tree, InstA, InstB)
    ccall((:LLVMDominatorTreeInstructionDominates, libLLVMExtra), LLVMBool, (LLVMDominatorTreeRef, LLVMValueRef, LLVMValueRef), Tree, InstA, InstB)
end

mutable struct LLVMOpaquePostDominatorTree end

const LLVMPostDominatorTreeRef = Ptr{LLVMOpaquePostDominatorTree}

function LLVMCreatePostDominatorTree(Fn)
    ccall((:LLVMCreatePostDominatorTree, libLLVMExtra), LLVMPostDominatorTreeRef, (LLVMValueRef,), Fn)
end

function LLVMDisposePostDominatorTree(Tree)
    ccall((:LLVMDisposePostDominatorTree, libLLVMExtra), Cvoid, (LLVMPostDominatorTreeRef,), Tree)
end

function LLVMPostDominatorTreeInstructionDominates(Tree, InstA, InstB)
    ccall((:LLVMPostDominatorTreeInstructionDominates, libLLVMExtra), LLVMBool, (LLVMPostDominatorTreeRef, LLVMValueRef, LLVMValueRef), Tree, InstA, InstB)
end

@cenum __JL_Ctag_2::UInt32 begin
    LLVMFastMathAllowReassoc = 1
    LLVMFastMathNoNaNs = 2
    LLVMFastMathNoInfs = 4
    LLVMFastMathNoSignedZeros = 8
    LLVMFastMathAllowReciprocal = 16
    LLVMFastMathAllowContract = 32
    LLVMFastMathApproxFunc = 64
    LLVMFastMathNone = 0
    LLVMFastMathAll = 127
end

const LLVMFastMathFlags = Cuint

function LLVMGetFastMathFlags(FPMathInst)
    ccall((:LLVMGetFastMathFlags, libLLVMExtra), LLVMFastMathFlags, (LLVMValueRef,), FPMathInst)
end

function LLVMSetFastMathFlags(FPMathInst, FMF)
    ccall((:LLVMSetFastMathFlags, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMFastMathFlags), FPMathInst, FMF)
end

function LLVMCanValueUseFastMathFlags(Inst)
    ccall((:LLVMCanValueUseFastMathFlags, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraSetFastMathFlags(FPMathInst, FMF)
    ccall((:LLVMExtraSetFastMathFlags, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMFastMathFlags), FPMathInst, FMF)
end

@cenum LLVMTailCallKind::UInt32 begin
    LLVMTailCallKindNone = 0
    LLVMTailCallKindTail = 1
    LLVMTailCallKindMustTail = 2
    LLVMTailCallKindNoTail = 3
end

function LLVMGetTailCallKind(CallInst)
    ccall((:LLVMGetTailCallKind, libLLVMExtra), LLVMTailCallKind, (LLVMValueRef,), CallInst)
end

function LLVMSetTailCallKind(CallInst, kind)
    ccall((:LLVMSetTailCallKind, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMTailCallKind), CallInst, kind)
end

function LLVMGetSyncScopeID(C, Name, SLen)
    ccall((:LLVMGetSyncScopeID, libLLVMExtra), Cuint, (LLVMContextRef, Cstring, Csize_t), C, Name, SLen)
end

function LLVMBuildFenceSyncScope(B, ordering, SSID, Name)
    ccall((:LLVMBuildFenceSyncScope, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMAtomicOrdering, Cuint, Cstring), B, ordering, SSID, Name)
end

function LLVMBuildAtomicRMWSyncScope(B, op, PTR, Val, ordering, SSID)
    ccall((:LLVMBuildAtomicRMWSyncScope, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMAtomicRMWBinOp, LLVMValueRef, LLVMValueRef, LLVMAtomicOrdering, Cuint), B, op, PTR, Val, ordering, SSID)
end

function LLVMBuildAtomicCmpXchgSyncScope(B, Ptr, Cmp, New, SuccessOrdering, FailureOrdering, SSID)
    ccall((:LLVMBuildAtomicCmpXchgSyncScope, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMAtomicOrdering, LLVMAtomicOrdering, Cuint), B, Ptr, Cmp, New, SuccessOrdering, FailureOrdering, SSID)
end

function LLVMIsAtomic(Inst)
    ccall((:LLVMIsAtomic, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMGetAtomicSyncScopeID(AtomicInst)
    ccall((:LLVMGetAtomicSyncScopeID, libLLVMExtra), Cuint, (LLVMValueRef,), AtomicInst)
end

function LLVMSetAtomicSyncScopeID(AtomicInst, SSID)
    ccall((:LLVMSetAtomicSyncScopeID, libLLVMExtra), Cvoid, (LLVMValueRef, Cuint), AtomicInst, SSID)
end

function LLVMExtraGetOrdering(MemAccessInst)
    ccall((:LLVMExtraGetOrdering, libLLVMExtra), LLVMAtomicOrdering, (LLVMValueRef,), MemAccessInst)
end

function LLVMExtraSetOrdering(MemAccessInst, Ordering)
    ccall((:LLVMExtraSetOrdering, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMAtomicOrdering), MemAccessInst, Ordering)
end

function LLVMGetValueContext(Val)
    ccall((:LLVMGetValueContext, libLLVMExtra), LLVMContextRef, (LLVMValueRef,), Val)
end

function LLVMGetBuilderContext(Builder)
    ccall((:LLVMGetBuilderContext, libLLVMExtra), LLVMContextRef, (LLVMBuilderRef,), Builder)
end

function LLVMExtraBuildAtomicRMWValue(B, Op, Loaded, Val)
    ccall((:LLVMExtraBuildAtomicRMWValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, Cuint, LLVMValueRef, LLVMValueRef), B, Op, Loaded, Val)
end

function LLVMExtraBuildCmpXchgValue(B, PointerVal, Cmp, Val, Alignment, Success)
    ccall((:LLVMExtraBuildCmpXchgValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, Cuint, Ptr{LLVMValueRef}), B, PointerVal, Cmp, Val, Alignment, Success)
end

function LLVMExtraLowerAtomicRMWInst(RMWI)
    ccall((:LLVMExtraLowerAtomicRMWInst, libLLVMExtra), LLVMBool, (LLVMValueRef,), RMWI)
end

function LLVMExtraLowerAtomicCmpXchgInst(CXI)
    ccall((:LLVMExtraLowerAtomicCmpXchgInst, libLLVMExtra), LLVMBool, (LLVMValueRef,), CXI)
end

function LLVMExtraExpandAtomicRMWToCmpXchg(RMWI)
    ccall((:LLVMExtraExpandAtomicRMWToCmpXchg, libLLVMExtra), LLVMBool, (LLVMValueRef,), RMWI)
end

function LLVMExtraCastAtomicToInteger(Inst)
    ccall((:LLVMExtraCastAtomicToInteger, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), Inst)
end

struct LLVMExtraPartwordMaskValues
    WordType::LLVMTypeRef
    ValueType::LLVMTypeRef
    IntValueType::LLVMTypeRef
    AlignedAddr::LLVMValueRef
    AlignedAddrAlignment::Cuint
    ShiftAmt::LLVMValueRef
    Mask::LLVMValueRef
    InvMask::LLVMValueRef
end

function LLVMExtraCreatePartwordMaskValues(B, ValueType, Addr, AddrAlign, MinWordSize, PMV)
    ccall((:LLVMExtraCreatePartwordMaskValues, libLLVMExtra), Cvoid, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cuint, Cuint, Ptr{LLVMExtraPartwordMaskValues}), B, ValueType, Addr, AddrAlign, MinWordSize, PMV)
end

function LLVMExtraExtractMaskedValue(B, WideWord, PMV)
    ccall((:LLVMExtraExtractMaskedValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMExtraPartwordMaskValues}), B, WideWord, PMV)
end

function LLVMExtraInsertMaskedValue(B, WideWord, Updated, PMV)
    ccall((:LLVMExtraInsertMaskedValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Ptr{LLVMExtraPartwordMaskValues}), B, WideWord, Updated, PMV)
end

function LLVMExtraExpandPartwordAtomicRMW(RMWI, MinWordSize)
    ccall((:LLVMExtraExpandPartwordAtomicRMW, libLLVMExtra), LLVMBool, (LLVMValueRef, Cuint), RMWI, MinWordSize)
end

function LLVMExtraExpandPartwordCmpXchg(CXI, MinWordSize)
    ccall((:LLVMExtraExpandPartwordCmpXchg, libLLVMExtra), LLVMBool, (LLVMValueRef, Cuint), CXI, MinWordSize)
end

function LLVMExtraGetSyncScopeName(C, SSID, Len)
    ccall((:LLVMExtraGetSyncScopeName, libLLVMExtra), Cstring, (LLVMContextRef, Cuint, Ptr{Csize_t}), C, SSID, Len)
end

mutable struct LLVMOpaquePassBuilderExtensions end

const LLVMPassBuilderExtensionsRef = Ptr{LLVMOpaquePassBuilderExtensions}

function LLVMCreatePassBuilderExtensions()
    ccall((:LLVMCreatePassBuilderExtensions, libLLVMExtra), LLVMPassBuilderExtensionsRef, ())
end

function LLVMDisposePassBuilderExtensions(Extensions)
    ccall((:LLVMDisposePassBuilderExtensions, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef,), Extensions)
end

function LLVMPassBuilderExtensionsPushRegistrationCallbacks(Options, RegistrationCallback)
    ccall((:LLVMPassBuilderExtensionsPushRegistrationCallbacks, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Ptr{Cvoid}), Options, RegistrationCallback)
end

# typedef LLVMBool ( * LLVMJuliaModulePassCallback ) ( LLVMModuleRef M , void * Thunk )
const LLVMJuliaModulePassCallback = Ptr{Cvoid}

# typedef LLVMBool ( * LLVMJuliaFunctionPassCallback ) ( LLVMValueRef F , void * Thunk )
const LLVMJuliaFunctionPassCallback = Ptr{Cvoid}

function LLVMPassBuilderExtensionsRegisterModulePass(Options, PassName, Callback, Thunk)
    ccall((:LLVMPassBuilderExtensionsRegisterModulePass, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaModulePassCallback, Ptr{Cvoid}), Options, PassName, Callback, Thunk)
end

function LLVMPassBuilderExtensionsRegisterFunctionPass(Options, PassName, Callback, Thunk)
    ccall((:LLVMPassBuilderExtensionsRegisterFunctionPass, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaFunctionPassCallback, Ptr{Cvoid}), Options, PassName, Callback, Thunk)
end

function LLVMPassBuilderExtensionsSetAAPipeline(Extensions, AAPipeline)
    ccall((:LLVMPassBuilderExtensionsSetAAPipeline, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring), Extensions, AAPipeline)
end

function LLVMRunJuliaPasses(M, Passes, TM, Options, Extensions)
    ccall((:LLVMRunJuliaPasses, libLLVMExtra), LLVMErrorRef, (LLVMModuleRef, Cstring, LLVMTargetMachineRef, LLVMPassBuilderOptionsRef, LLVMPassBuilderExtensionsRef), M, Passes, TM, Options, Extensions)
end

function LLVMRunJuliaPassesOnFunction(F, Passes, TM, Options, Extensions)
    ccall((:LLVMRunJuliaPassesOnFunction, libLLVMExtra), LLVMErrorRef, (LLVMValueRef, Cstring, LLVMTargetMachineRef, LLVMPassBuilderOptionsRef, LLVMPassBuilderExtensionsRef), F, Passes, TM, Options, Extensions)
end

# typedef LLVMBool ( * LLVMTTIASPairPredicateFn ) ( unsigned FromAS , unsigned ToAS , void * UserData )
const LLVMTTIASPairPredicateFn = Ptr{Cvoid}

# typedef LLVMBool ( * LLVMTTIASPredicateFn ) ( unsigned AS , void * UserData )
const LLVMTTIASPredicateFn = Ptr{Cvoid}

# typedef LLVMBool ( * LLVMTTIValuePredicateFn ) ( LLVMValueRef V , void * UserData )
const LLVMTTIValuePredicateFn = Ptr{Cvoid}

# typedef unsigned ( * LLVMTTIGetAssumedAddressSpaceFn ) ( LLVMValueRef V , void * UserData )
const LLVMTTIGetAssumedAddressSpaceFn = Ptr{Cvoid}

# typedef unsigned ( * LLVMTTIGetPredicatedAddressSpaceFn ) ( LLVMValueRef V , LLVMValueRef * OutPredicate , void * UserData )
const LLVMTTIGetPredicatedAddressSpaceFn = Ptr{Cvoid}

# typedef LLVMValueRef ( * LLVMTTIRewriteIntrinsicFn ) ( LLVMValueRef II , LLVMValueRef OldV , LLVMValueRef NewV , void * UserData )
const LLVMTTIRewriteIntrinsicFn = Ptr{Cvoid}

# typedef LLVMBool ( * LLVMTTICollectFlatAddressOperandsFn ) ( unsigned IID , int * OutOps , unsigned MaxCount , unsigned * OutCount , void * UserData )
const LLVMTTICollectFlatAddressOperandsFn = Ptr{Cvoid}

mutable struct LLVMOpaqueTTIOptions end

const LLVMTTIOptionsRef = Ptr{LLVMOpaqueTTIOptions}

function LLVMCreateTTIOptions()
    ccall((:LLVMCreateTTIOptions, libLLVMExtra), LLVMTTIOptionsRef, ())
end

function LLVMDisposeTTIOptions(Options)
    ccall((:LLVMDisposeTTIOptions, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef,), Options)
end

function LLVMTTIOptionsSetFlatAddressSpace(Options, AS)
    ccall((:LLVMTTIOptionsSetFlatAddressSpace, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, Cuint), Options, AS)
end

function LLVMTTIOptionsSetHasBranchDivergence(Options, Value)
    ccall((:LLVMTTIOptionsSetHasBranchDivergence, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMBool), Options, Value)
end

function LLVMTTIOptionsSetIsSingleThreaded(Options, Value)
    ccall((:LLVMTTIOptionsSetIsSingleThreaded, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMBool), Options, Value)
end

function LLVMTTIOptionsSetIsNoopAddrSpaceCast(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetIsNoopAddrSpaceCast, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIASPairPredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetIsValidAddrSpaceCast(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetIsValidAddrSpaceCast, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIASPairPredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetAddrSpacesMayAlias(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetAddrSpacesMayAlias, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIASPairPredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetCanHaveGlobalInitializerInAS(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetCanHaveGlobalInitializerInAS, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIASPredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetIsSourceOfDivergence(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetIsSourceOfDivergence, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIValuePredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetIsAlwaysUniform(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetIsAlwaysUniform, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIValuePredicateFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetGetAssumedAddressSpace(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetGetAssumedAddressSpace, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIGetAssumedAddressSpaceFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetGetPredicatedAddressSpace(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetGetPredicatedAddressSpace, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIGetPredicatedAddressSpaceFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetRewriteIntrinsicWithAS(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetRewriteIntrinsicWithAS, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTIRewriteIntrinsicFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMTTIOptionsSetCollectFlatAddressOperands(Options, Callback, UserData)
    ccall((:LLVMTTIOptionsSetCollectFlatAddressOperands, libLLVMExtra), Cvoid, (LLVMTTIOptionsRef, LLVMTTICollectFlatAddressOperandsFn, Ptr{Cvoid}), Options, Callback, UserData)
end

function LLVMPassBuilderExtensionsSetTTI(Extensions, Options)
    ccall((:LLVMPassBuilderExtensionsSetTTI, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, LLVMTTIOptionsRef), Extensions, Options)
end

function LLVMGlobalsAddressSpace(TD)
    ccall((:LLVMGlobalsAddressSpace, libLLVMExtra), Cuint, (LLVMTargetDataRef,), TD)
end

@cenum LLVMLinkerFlags::UInt32 begin
    LLVMLinkerNone = 0
    LLVMLinkerOverrideFromSrc = 1
    LLVMLinkerLinkOnlyNeeded = 2
end

function LLVMLinkModules3(Dest, Src, Flags)
    ccall((:LLVMLinkModules3, libLLVMExtra), LLVMBool, (LLVMModuleRef, LLVMModuleRef, Cuint), Dest, Src, Flags)
end

function LLVMGetNUW(ArithInst)
    ccall((:LLVMGetNUW, libLLVMExtra), LLVMBool, (LLVMValueRef,), ArithInst)
end

function LLVMSetNUW(ArithInst, HasNUW)
    ccall((:LLVMSetNUW, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBool), ArithInst, HasNUW)
end

function LLVMGetNSW(ArithInst)
    ccall((:LLVMGetNSW, libLLVMExtra), LLVMBool, (LLVMValueRef,), ArithInst)
end

function LLVMSetNSW(ArithInst, HasNSW)
    ccall((:LLVMSetNSW, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBool), ArithInst, HasNSW)
end

function LLVMGetExact(DivOrShrInst)
    ccall((:LLVMGetExact, libLLVMExtra), LLVMBool, (LLVMValueRef,), DivOrShrInst)
end

function LLVMSetExact(DivOrShrInst, IsExact)
    ccall((:LLVMSetExact, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBool), DivOrShrInst, IsExact)
end

function LLVMGetSwitchCaseValue(Switch, i)
    ccall((:LLVMGetSwitchCaseValue, libLLVMExtra), LLVMValueRef, (LLVMValueRef, Cuint), Switch, i)
end

function LLVMSetSwitchCaseValue(Switch, i, CaseValue)
    ccall((:LLVMSetSwitchCaseValue, libLLVMExtra), Cvoid, (LLVMValueRef, Cuint, LLVMValueRef), Switch, i, CaseValue)
end

function LLVMConstFPFromBits(Ty, N)
    ccall((:LLVMConstFPFromBits, libLLVMExtra), LLVMValueRef, (LLVMTypeRef, Ptr{UInt64}), Ty, N)
end

function LLVMExtraConstFPGetBits(ConstantVal, N)
    ccall((:LLVMExtraConstFPGetBits, libLLVMExtra), Cvoid, (LLVMValueRef, Ptr{UInt64}), ConstantVal, N)
end

function LLVMExtraConstIntGetWords(ConstantVal, N)
    ccall((:LLVMExtraConstIntGetWords, libLLVMExtra), Cvoid, (LLVMValueRef, Ptr{UInt64}), ConstantVal, N)
end

function LLVMExtraGetAttributeKindName(KindID, Len)
    ccall((:LLVMExtraGetAttributeKindName, libLLVMExtra), Cstring, (Cuint, Ptr{Csize_t}), KindID, Len)
end

function LLVMExtraIsEnumAttributeKind(KindID)
    ccall((:LLVMExtraIsEnumAttributeKind, libLLVMExtra), LLVMBool, (Cuint,), KindID)
end

function LLVMExtraIsIntAttributeKind(KindID)
    ccall((:LLVMExtraIsIntAttributeKind, libLLVMExtra), LLVMBool, (Cuint,), KindID)
end

function LLVMExtraIsTypeAttributeKind(KindID)
    ccall((:LLVMExtraIsTypeAttributeKind, libLLVMExtra), LLVMBool, (Cuint,), KindID)
end

function LLVMExtraBuildExtractValue(B, AggVal, Idxs, NumIdxs, Name)
    ccall((:LLVMExtraBuildExtractValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{Cuint}, Cuint, Cstring), B, AggVal, Idxs, NumIdxs, Name)
end

function LLVMExtraBuildInsertValue(B, AggVal, EltVal, Idxs, NumIdxs, Name)
    ccall((:LLVMExtraBuildInsertValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Ptr{Cuint}, Cuint, Cstring), B, AggVal, EltVal, Idxs, NumIdxs, Name)
end

function LLVMExtraConstVectorSplat(VecTy, Elt)
    ccall((:LLVMExtraConstVectorSplat, libLLVMExtra), LLVMValueRef, (LLVMTypeRef, LLVMValueRef), VecTy, Elt)
end

function LLVMExtraBuildAlloca(B, Ty, AddrSpace, ArraySize, Name)
    ccall((:LLVMExtraBuildAlloca, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, Cuint, LLVMValueRef, Cstring), B, Ty, AddrSpace, ArraySize, Name)
end

function LLVMExtraGetIndexSizeInBits(TD, AddrSpace)
    ccall((:LLVMExtraGetIndexSizeInBits, libLLVMExtra), Cuint, (LLVMTargetDataRef, Cuint), TD, AddrSpace)
end

function LLVMExtraGEPAccumulateConstantOffset(GEP, TD, Words)
    ccall((:LLVMExtraGEPAccumulateConstantOffset, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMTargetDataRef, Ptr{UInt64}), GEP, TD, Words)
end

function LLVMExtraMoveInstruction(Inst, BB, Before, Head)
    ccall((:LLVMExtraMoveInstruction, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Inst, BB, Before, Head)
end

function LLVMExtraMoveBasicBlock(BB, Fn, Before)
    ccall((:LLVMExtraMoveBasicBlock, libLLVMExtra), Cvoid, (LLVMBasicBlockRef, LLVMValueRef, LLVMBasicBlockRef), BB, Fn, Before)
end

function LLVMExtraDeleteBasicBlock(BB)
    ccall((:LLVMExtraDeleteBasicBlock, libLLVMExtra), Cvoid, (LLVMBasicBlockRef,), BB)
end

function LLVMExtraPositionBuilder(Builder, BB, Before, Head)
    ccall((:LLVMExtraPositionBuilder, libLLVMExtra), Cvoid, (LLVMBuilderRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Builder, BB, Before, Head)
end

function LLVMExtraGetInsertPoint(Builder, Before, Head)
    ccall((:LLVMExtraGetInsertPoint, libLLVMExtra), LLVMBasicBlockRef, (LLVMBuilderRef, Ptr{LLVMValueRef}, Ptr{LLVMBool}), Builder, Before, Head)
end

function LLVMExtraGetFirstInsertionPt(BB, Before, Head)
    ccall((:LLVMExtraGetFirstInsertionPt, libLLVMExtra), LLVMBool, (LLVMBasicBlockRef, Ptr{LLVMValueRef}, Ptr{LLVMBool}), BB, Before, Head)
end

function LLVMExtraInstructionComesBefore(Inst, Other)
    ccall((:LLVMExtraInstructionComesBefore, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMValueRef), Inst, Other)
end

function LLVMExtraMayReadFromMemory(Inst)
    ccall((:LLVMExtraMayReadFromMemory, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraMayWriteToMemory(Inst)
    ccall((:LLVMExtraMayWriteToMemory, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraMayHaveSideEffects(Inst)
    ccall((:LLVMExtraMayHaveSideEffects, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraTakeName(Val, From)
    ccall((:LLVMExtraTakeName, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Val, From)
end

function LLVMExtraStripPointerCasts(Val)
    ccall((:LLVMExtraStripPointerCasts, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMExtraStripPointerCastsAndAliases(Val)
    ccall((:LLVMExtraStripPointerCastsAndAliases, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMExtraGetArgNo(Arg)
    ccall((:LLVMExtraGetArgNo, libLLVMExtra), Cuint, (LLVMValueRef,), Arg)
end

function LLVMExtraCopyAttributesFrom(Dst, Src)
    ccall((:LLVMExtraCopyAttributesFrom, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Dst, Src)
end

function LLVMExtraRemoveDeadConstantUsers(C)
    ccall((:LLVMExtraRemoveDeadConstantUsers, libLLVMExtra), Cvoid, (LLVMValueRef,), C)
end

function LLVMExtraVerifyFunction(Fn, OutMessage)
    ccall((:LLVMExtraVerifyFunction, libLLVMExtra), LLVMBool, (LLVMValueRef, Ptr{Cstring}), Fn, OutMessage)
end

