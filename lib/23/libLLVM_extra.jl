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

function LLVMExtraIntrinsicGetOverloadTypes(ID, FunctionTy, OverloadTypes, OverloadCount)
    ccall((:LLVMExtraIntrinsicGetOverloadTypes, libLLVMExtra), LLVMBool, (Cuint, LLVMTypeRef, Ptr{LLVMTypeRef}, Ptr{Csize_t}), ID, FunctionTy, OverloadTypes, OverloadCount)
end

function LLVMConvertUsersOfConstantsToInstructions(Consts, Count, RestrictToFunc, RemoveDeadConstants, IncludeSelf)
    ccall((:LLVMConvertUsersOfConstantsToInstructions, libLLVMExtra), LLVMBool, (Ptr{LLVMValueRef}, Csize_t, LLVMValueRef, LLVMBool, LLVMBool), Consts, Count, RestrictToFunc, RemoveDeadConstants, IncludeSelf)
end

function LLVMIsConstantRangeAttribute(A)
    ccall((:LLVMIsConstantRangeAttribute, libLLVMExtra), LLVMBool, (LLVMAttributeRef,), A)
end

function LLVMIsConstantRangeListAttribute(A)
    ccall((:LLVMIsConstantRangeListAttribute, libLLVMExtra), LLVMBool, (LLVMAttributeRef,), A)
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

function LLVMExtraThreadSafeModuleTakeModule(TSM)
    ccall((:LLVMExtraThreadSafeModuleTakeModule, libLLVMExtra), LLVMModuleRef, (LLVMOrcThreadSafeModuleRef,), TSM)
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

function LLVMMetadataAsValue2(C, Metadata)
    ccall((:LLVMMetadataAsValue2, libLLVMExtra), LLVMValueRef, (LLVMContextRef, LLVMMetadataRef), C, Metadata)
end

function LLVMReplaceAllMetadataUsesWith(Old, New)
    ccall((:LLVMReplaceAllMetadataUsesWith, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Old, New)
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

function LLVMExtraSetFastMathFlags(FPMathInst, FMF)
    ccall((:LLVMExtraSetFastMathFlags, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMFastMathFlags), FPMathInst, FMF)
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

function LLVMExtraIsNonIntegralPointerType(M, T)
    ccall((:LLVMExtraIsNonIntegralPointerType, libLLVMExtra), LLVMBool, (LLVMModuleRef, LLVMTypeRef), M, T)
end

function LLVMExtraMustNotIntroducePtrToInt(M, T)
    ccall((:LLVMExtraMustNotIntroducePtrToInt, libLLVMExtra), LLVMBool, (LLVMModuleRef, LLVMTypeRef), M, T)
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

@cenum LLVMExtraRemarkKind::UInt32 begin
    LLVMExtraRemarkPassed = 0
    LLVMExtraRemarkMissed = 1
    LLVMExtraRemarkAnalysis = 2
end

function LLVMExtraGetRemarkInfo(DI, Kind, PassName, RemarkName, FunctionName, Message, File, Line, Column)
    ccall((:LLVMExtraGetRemarkInfo, libLLVMExtra), LLVMBool, (LLVMDiagnosticInfoRef, Ptr{LLVMExtraRemarkKind}, Ptr{Cstring}, Ptr{Cstring}, Ptr{Cstring}, Ptr{Cstring}, Ptr{Cstring}, Ptr{Cuint}, Ptr{Cuint}), DI, Kind, PassName, RemarkName, FunctionName, Message, File, Line, Column)
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

function LLVMPassBuilderExtensionsRegisterModulePassWithRequired(Options, PassName, Callback, Thunk, Required)
    ccall((:LLVMPassBuilderExtensionsRegisterModulePassWithRequired, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaModulePassCallback, Ptr{Cvoid}, LLVMBool), Options, PassName, Callback, Thunk, Required)
end

function LLVMPassBuilderExtensionsRegisterFunctionPassWithRequired(Options, PassName, Callback, Thunk, Required)
    ccall((:LLVMPassBuilderExtensionsRegisterFunctionPassWithRequired, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaFunctionPassCallback, Ptr{Cvoid}, LLVMBool), Options, PassName, Callback, Thunk, Required)
end

mutable struct LLVMOpaqueFunctionAnalysisManager end

const LLVMFunctionAnalysisManagerRef = Ptr{LLVMOpaqueFunctionAnalysisManager}

mutable struct LLVMOpaquePreservedAnalyses end

const LLVMPreservedAnalysesRef = Ptr{LLVMOpaquePreservedAnalyses}

# typedef void ( * LLVMJuliaFunctionPassWithAnalysesCallback ) ( LLVMValueRef F , LLVMFunctionAnalysisManagerRef AM , LLVMPreservedAnalysesRef PA , void * Thunk )
const LLVMJuliaFunctionPassWithAnalysesCallback = Ptr{Cvoid}

function LLVMExtraPassBuilderExtensionsRegisterFunctionPassWithAnalyses(Extensions, PassName, Callback, Thunk, Required)
    ccall((:LLVMExtraPassBuilderExtensionsRegisterFunctionPassWithAnalyses, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaFunctionPassWithAnalysesCallback, Ptr{Cvoid}, LLVMBool), Extensions, PassName, Callback, Thunk, Required)
end

@cenum LLVMExtraFunctionAnalysis::UInt32 begin
    LLVMExtraDominatorTreeAnalysis = 0
    LLVMExtraPostDominatorTreeAnalysis = 1
    LLVMExtraAssumptionAnalysis = 2
    LLVMExtraLazyValueAnalysis = 3
    LLVMExtraScalarEvolutionAnalysis = 4
    LLVMExtraLoopAnalysis = 5
end

function LLVMExtraFunctionAnalysisManagerGetResult(AM, F, Analysis)
    ccall((:LLVMExtraFunctionAnalysisManagerGetResult, libLLVMExtra), Ptr{Cvoid}, (LLVMFunctionAnalysisManagerRef, LLVMValueRef, LLVMExtraFunctionAnalysis), AM, F, Analysis)
end

function LLVMExtraFunctionAnalysisManagerGetCachedResult(AM, F, Analysis)
    ccall((:LLVMExtraFunctionAnalysisManagerGetCachedResult, libLLVMExtra), Ptr{Cvoid}, (LLVMFunctionAnalysisManagerRef, LLVMValueRef, LLVMExtraFunctionAnalysis), AM, F, Analysis)
end

function LLVMExtraSetPreservedAnalyses(PA, All, CFG, Analyses, NumAnalyses)
    ccall((:LLVMExtraSetPreservedAnalyses, libLLVMExtra), Cvoid, (LLVMPreservedAnalysesRef, LLVMBool, LLVMBool, Ptr{LLVMExtraFunctionAnalysis}, Cuint), PA, All, CFG, Analyses, NumAnalyses)
end

function LLVMExtraFunctionAnalysisManagerInvalidate(AM, F, All, CFG, Analyses, NumAnalyses)
    ccall((:LLVMExtraFunctionAnalysisManagerInvalidate, libLLVMExtra), Cvoid, (LLVMFunctionAnalysisManagerRef, LLVMValueRef, LLVMBool, LLVMBool, Ptr{LLVMExtraFunctionAnalysis}, Cuint), AM, F, All, CFG, Analyses, NumAnalyses)
end

function LLVMExtraFunctionAnalysisManagerClear(AM, F)
    ccall((:LLVMExtraFunctionAnalysisManagerClear, libLLVMExtra), Cvoid, (LLVMFunctionAnalysisManagerRef, LLVMValueRef), AM, F)
end

function LLVMExtraFunctionAnalysisManagerEmitRemark(AM, Kind, PassName, RemarkName, Inst, Message)
    ccall((:LLVMExtraFunctionAnalysisManagerEmitRemark, libLLVMExtra), Cvoid, (LLVMFunctionAnalysisManagerRef, LLVMExtraRemarkKind, Cstring, Cstring, LLVMValueRef, Cstring), AM, Kind, PassName, RemarkName, Inst, Message)
end

mutable struct LLVMOpaqueModuleAnalysisManager end

const LLVMModuleAnalysisManagerRef = Ptr{LLVMOpaqueModuleAnalysisManager}

# typedef void ( * LLVMJuliaModulePassWithAnalysesCallback ) ( LLVMModuleRef M , LLVMModuleAnalysisManagerRef AM , LLVMPreservedAnalysesRef PA , void * Thunk )
const LLVMJuliaModulePassWithAnalysesCallback = Ptr{Cvoid}

function LLVMExtraPassBuilderExtensionsRegisterModulePassWithAnalyses(Extensions, PassName, Callback, Thunk, Required)
    ccall((:LLVMExtraPassBuilderExtensionsRegisterModulePassWithAnalyses, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaModulePassWithAnalysesCallback, Ptr{Cvoid}, LLVMBool), Extensions, PassName, Callback, Thunk, Required)
end

function LLVMExtraModuleAnalysisManagerGetFunctionAnalysisManager(AM, M)
    ccall((:LLVMExtraModuleAnalysisManagerGetFunctionAnalysisManager, libLLVMExtra), LLVMFunctionAnalysisManagerRef, (LLVMModuleAnalysisManagerRef, LLVMModuleRef), AM, M)
end

function LLVMExtraSetModulePreservedAnalyses(PA, All, Functions, CFG, Analyses, NumAnalyses)
    ccall((:LLVMExtraSetModulePreservedAnalyses, libLLVMExtra), Cvoid, (LLVMPreservedAnalysesRef, LLVMBool, LLVMBool, LLVMBool, Ptr{LLVMExtraFunctionAnalysis}, Cuint), PA, All, Functions, CFG, Analyses, NumAnalyses)
end

# typedef void * ( * LLVMJuliaPassParametersCallback ) ( const char * Parameters , void * Thunk )
const LLVMJuliaPassParametersCallback = Ptr{Cvoid}

function LLVMExtraPassBuilderExtensionsRegisterParametrizedModulePass(Extensions, PassName, Callback, ParametersCallback, Thunk, Required)
    ccall((:LLVMExtraPassBuilderExtensionsRegisterParametrizedModulePass, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaModulePassWithAnalysesCallback, LLVMJuliaPassParametersCallback, Ptr{Cvoid}, LLVMBool), Extensions, PassName, Callback, ParametersCallback, Thunk, Required)
end

function LLVMExtraPassBuilderExtensionsRegisterParametrizedFunctionPass(Extensions, PassName, Callback, ParametersCallback, Thunk, Required)
    ccall((:LLVMExtraPassBuilderExtensionsRegisterParametrizedFunctionPass, libLLVMExtra), Cvoid, (LLVMPassBuilderExtensionsRef, Cstring, LLVMJuliaFunctionPassWithAnalysesCallback, LLVMJuliaPassParametersCallback, Ptr{Cvoid}, LLVMBool), Extensions, PassName, Callback, ParametersCallback, Thunk, Required)
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

function LLVMOrcThreadSafeContextGetContext(TSCtx)
    ccall((:LLVMOrcThreadSafeContextGetContext, libLLVMExtra), LLVMContextRef, (LLVMOrcThreadSafeContextRef,), TSCtx)
end

function LLVMExtraConstFPGetBits(ConstantVal, N)
    ccall((:LLVMExtraConstFPGetBits, libLLVMExtra), Cvoid, (LLVMValueRef, Ptr{UInt64}), ConstantVal, N)
end

function LLVMExtraConstIntGetWords(ConstantVal, N)
    ccall((:LLVMExtraConstIntGetWords, libLLVMExtra), Cvoid, (LLVMValueRef, Ptr{UInt64}), ConstantVal, N)
end

function LLVMExtraDbgVariableRecordGetNumValues(Rec)
    ccall((:LLVMExtraDbgVariableRecordGetNumValues, libLLVMExtra), Cuint, (LLVMDbgRecordRef,), Rec)
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

function LLVMExtraIsConstantRangeAttributeKind(KindID)
    ccall((:LLVMExtraIsConstantRangeAttributeKind, libLLVMExtra), LLVMBool, (Cuint,), KindID)
end

function LLVMExtraBuildExtractValue(B, AggVal, Idxs, NumIdxs, Name)
    ccall((:LLVMExtraBuildExtractValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{Cuint}, Cuint, Cstring), B, AggVal, Idxs, NumIdxs, Name)
end

function LLVMExtraBuildInsertValue(B, AggVal, EltVal, Idxs, NumIdxs, Name)
    ccall((:LLVMExtraBuildInsertValue, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Ptr{Cuint}, Cuint, Cstring), B, AggVal, EltVal, Idxs, NumIdxs, Name)
end

function LLVMExtraBuildAssumption(B, Cond)
    ccall((:LLVMExtraBuildAssumption, libLLVMExtra), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef), B, Cond)
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

function LLVMExtraStripAndAccumulateConstantOffsets(V, TD, AllowNonInbounds, Words)
    ccall((:LLVMExtraStripAndAccumulateConstantOffsets, libLLVMExtra), LLVMValueRef, (LLVMValueRef, LLVMTargetDataRef, LLVMBool, Ptr{UInt64}), V, TD, AllowNonInbounds, Words)
end

function LLVMExtraMoveInstruction(Inst, BB, Before, Head)
    ccall((:LLVMExtraMoveInstruction, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Inst, BB, Before, Head)
end

function LLVMExtraMoveInstructionPreserving(Inst, BB, Before, Head)
    ccall((:LLVMExtraMoveInstructionPreserving, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Inst, BB, Before, Head)
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

function LLVMExtraGetInsertionPointAfterDef(Inst, BB, Before, Head)
    ccall((:LLVMExtraGetInsertionPointAfterDef, libLLVMExtra), LLVMBool, (LLVMValueRef, Ptr{LLVMBasicBlockRef}, Ptr{LLVMValueRef}, Ptr{LLVMBool}), Inst, BB, Before, Head)
end

function LLVMExtraDIBuilderInsertDeclareRecordAt(Builder, Storage, VarInfo, Expr, DL, BB, Before, Head)
    ccall((:LLVMExtraDIBuilderInsertDeclareRecordAt, libLLVMExtra), LLVMDbgRecordRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Builder, Storage, VarInfo, Expr, DL, BB, Before, Head)
end

function LLVMExtraDIBuilderInsertDbgValueRecordAt(Builder, Val, VarInfo, Expr, DL, BB, Before, Head)
    ccall((:LLVMExtraDIBuilderInsertDbgValueRecordAt, libLLVMExtra), LLVMDbgRecordRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Builder, Val, VarInfo, Expr, DL, BB, Before, Head)
end

function LLVMExtraDIBuilderInsertLabelAt(Builder, LabelInfo, DL, BB, Before, Head)
    ccall((:LLVMExtraDIBuilderInsertLabelAt, libLLVMExtra), LLVMDbgRecordRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, LLVMBasicBlockRef, LLVMValueRef, LLVMBool), Builder, LabelInfo, DL, BB, Before, Head)
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

function LLVMExtraIsCommutative(Inst)
    ccall((:LLVMExtraIsCommutative, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraDropUnknownNonDebugMetadata(Inst, KnownIDs, NumKnownIDs)
    ccall((:LLVMExtraDropUnknownNonDebugMetadata, libLLVMExtra), Cvoid, (LLVMValueRef, Ptr{Cuint}, Cuint), Inst, KnownIDs, NumKnownIDs)
end

function LLVMExtraCanInstructionHaveMMRAs(Inst)
    ccall((:LLVMExtraCanInstructionHaveMMRAs, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraConstantExprGetAsInstruction(CE)
    ccall((:LLVMExtraConstantExprGetAsInstruction, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), CE)
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

function LLVMExtraGetOperandNo(U)
    ccall((:LLVMExtraGetOperandNo, libLLVMExtra), Cuint, (LLVMUseRef,), U)
end

function LLVMExtraOnlyReadsMemory(V, ArgNo)
    ccall((:LLVMExtraOnlyReadsMemory, libLLVMExtra), LLVMBool, (LLVMValueRef, Cint), V, ArgNo)
end

function LLVMExtraDoesNotCapture(V, ArgNo)
    ccall((:LLVMExtraDoesNotCapture, libLLVMExtra), LLVMBool, (LLVMValueRef, Cint), V, ArgNo)
end

function LLVMExtraGetArgNo(Arg)
    ccall((:LLVMExtraGetArgNo, libLLVMExtra), Cuint, (LLVMValueRef,), Arg)
end

function LLVMExtraCopyAttributesFrom(Dst, Src)
    ccall((:LLVMExtraCopyAttributesFrom, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Dst, Src)
end

function LLVMExtraIsInterposable(GV)
    ccall((:LLVMExtraIsInterposable, libLLVMExtra), LLVMBool, (LLVMValueRef,), GV)
end

function LLVMExtraIsDSOLocal(GV)
    ccall((:LLVMExtraIsDSOLocal, libLLVMExtra), LLVMBool, (LLVMValueRef,), GV)
end

function LLVMExtraSetDSOLocal(GV, Local)
    ccall((:LLVMExtraSetDSOLocal, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMBool), GV, Local)
end

function LLVMExtraAddFunctionInAddressSpace(M, Name, FunctionTy, AddrSpace)
    ccall((:LLVMExtraAddFunctionInAddressSpace, libLLVMExtra), LLVMValueRef, (LLVMModuleRef, Cstring, LLVMTypeRef, Cuint), M, Name, FunctionTy, AddrSpace)
end

function LLVMExtraAddAlias(M, ValueTy, AddrSpace, Aliasee, Name)
    ccall((:LLVMExtraAddAlias, libLLVMExtra), LLVMValueRef, (LLVMModuleRef, LLVMTypeRef, Cuint, LLVMValueRef, Cstring), M, ValueTy, AddrSpace, Aliasee, Name)
end

function LLVMExtraEraseGlobalAlias(Alias)
    ccall((:LLVMExtraEraseGlobalAlias, libLLVMExtra), Cvoid, (LLVMValueRef,), Alias)
end

function LLVMExtraGlobalAddMetadata(Global, KindID, MD)
    ccall((:LLVMExtraGlobalAddMetadata, libLLVMExtra), Cvoid, (LLVMValueRef, Cuint, LLVMMetadataRef), Global, KindID, MD)
end

function LLVMExtraGetComdatName(C, Len)
    ccall((:LLVMExtraGetComdatName, libLLVMExtra), Cstring, (LLVMComdatRef, Ptr{Csize_t}), C, Len)
end

function LLVMExtraRemangleIntrinsicFunction(Fn)
    ccall((:LLVMExtraRemangleIntrinsicFunction, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), Fn)
end

function LLVMExtraStructSetName(StructTy, Name)
    ccall((:LLVMExtraStructSetName, libLLVMExtra), Cvoid, (LLVMTypeRef, Cstring), StructTy, Name)
end

function LLVMExtraRemoveDeadConstantUsers(C)
    ccall((:LLVMExtraRemoveDeadConstantUsers, libLLVMExtra), Cvoid, (LLVMValueRef,), C)
end

function LLVMExtraConstantExprGetWithOperands(CE, Ops, NumOps, Ty, SrcTy)
    ccall((:LLVMExtraConstantExprGetWithOperands, libLLVMExtra), LLVMValueRef, (LLVMValueRef, Ptr{LLVMValueRef}, Cuint, LLVMTypeRef, LLVMTypeRef), CE, Ops, NumOps, Ty, SrcTy)
end

@cenum LLVMExtraGlobalReferenceKind::UInt32 begin
    LLVMExtraNoGlobalReference = 0
    LLVMExtraBlockAddressReference = 1
    LLVMExtraDSOLocalEquivalentReference = 2
    LLVMExtraNoCFIValueReference = 3
end

function LLVMExtraGetGlobalReference(C, GV, BB)
    ccall((:LLVMExtraGetGlobalReference, libLLVMExtra), LLVMExtraGlobalReferenceKind, (LLVMValueRef, Ptr{LLVMValueRef}, Ptr{LLVMValueRef}), C, GV, BB)
end

function LLVMExtraGetDSOLocalEquivalent(GV)
    ccall((:LLVMExtraGetDSOLocalEquivalent, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), GV)
end

function LLVMExtraGetNoCFIValue(GV)
    ccall((:LLVMExtraGetNoCFIValue, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), GV)
end

function LLVMExtraVerifyFunction(Fn, OutMessage)
    ccall((:LLVMExtraVerifyFunction, libLLVMExtra), LLVMBool, (LLVMValueRef, Ptr{Cstring}), Fn, OutMessage)
end

@cenum LLVMExtraNoWrapKind::UInt32 begin
    LLVMExtraNoUnsignedWrap = 1
    LLVMExtraNoSignedWrap = 2
end

@cenum LLVMExtraPreferredRangeType::UInt32 begin
    LLVMExtraSmallestRange = 0
    LLVMExtraUnsignedRange = 1
    LLVMExtraSignedRange = 2
end

function LLVMExtraConstantRangeBinaryOp(Opcode, NoWrapKind, NumBits, LowerA, UpperA, LowerB, UpperB, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeBinaryOp, libLLVMExtra), LLVMBool, (LLVMOpcode, Cuint, Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}), Opcode, NoWrapKind, NumBits, LowerA, UpperA, LowerB, UpperB, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeCastOp(Opcode, NumBits, Lower, Upper, ResultBits, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeCastOp, libLLVMExtra), LLVMBool, (LLVMOpcode, Cuint, Ptr{UInt64}, Ptr{UInt64}, Cuint, Ptr{UInt64}, Ptr{UInt64}), Opcode, NumBits, Lower, Upper, ResultBits, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeIntersectWith(NumBits, LowerA, UpperA, LowerB, UpperB, Type, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeIntersectWith, libLLVMExtra), Cvoid, (Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, LLVMExtraPreferredRangeType, Ptr{UInt64}, Ptr{UInt64}), NumBits, LowerA, UpperA, LowerB, UpperB, Type, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeUnionWith(NumBits, LowerA, UpperA, LowerB, UpperB, Type, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeUnionWith, libLLVMExtra), Cvoid, (Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, LLVMExtraPreferredRangeType, Ptr{UInt64}, Ptr{UInt64}), NumBits, LowerA, UpperA, LowerB, UpperB, Type, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeMakeICmpRegion(Predicate, Satisfying, NumBits, Lower, Upper, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeMakeICmpRegion, libLLVMExtra), Cvoid, (LLVMIntPredicate, LLVMBool, Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}), Predicate, Satisfying, NumBits, Lower, Upper, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeFromKnownBits(NumBits, Zero, One, Signed, LowerOut, UpperOut)
    ccall((:LLVMExtraConstantRangeFromKnownBits, libLLVMExtra), Cvoid, (Cuint, Ptr{UInt64}, Ptr{UInt64}, LLVMBool, Ptr{UInt64}, Ptr{UInt64}), NumBits, Zero, One, Signed, LowerOut, UpperOut)
end

function LLVMExtraConstantRangeToKnownBits(NumBits, Lower, Upper, ZeroOut, OneOut)
    ccall((:LLVMExtraConstantRangeToKnownBits, libLLVMExtra), Cvoid, (Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}), NumBits, Lower, Upper, ZeroOut, OneOut)
end

function LLVMExtraConstantRangeContains(NumBits, LowerA, UpperA, LowerB, UpperB)
    ccall((:LLVMExtraConstantRangeContains, libLLVMExtra), LLVMBool, (Cuint, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}), NumBits, LowerA, UpperA, LowerB, UpperB)
end

function LLVMExtraGetConstantRangeFromMetadata(MD, Lower, Upper)
    ccall((:LLVMExtraGetConstantRangeFromMetadata, libLLVMExtra), Cuint, (LLVMMetadataRef, Ptr{UInt64}, Ptr{UInt64}), MD, Lower, Upper)
end

function LLVMExtraCreateRangeMetadata(C, NumBits, Lower, Upper)
    ccall((:LLVMExtraCreateRangeMetadata, libLLVMExtra), LLVMMetadataRef, (LLVMContextRef, Cuint, Ptr{UInt64}, Ptr{UInt64}), C, NumBits, Lower, Upper)
end

function LLVMExtraGetConstantRangeAttributeValue(A, Lower, Upper)
    ccall((:LLVMExtraGetConstantRangeAttributeValue, libLLVMExtra), Cuint, (LLVMAttributeRef, Ptr{UInt64}, Ptr{UInt64}), A, Lower, Upper)
end

mutable struct LLVMOpaqueAssumptionCache end

const LLVMAssumptionCacheRef = Ptr{LLVMOpaqueAssumptionCache}

function LLVMExtraAssumptionCacheGetAssumptions(AC, Assumes)
    ccall((:LLVMExtraAssumptionCacheGetAssumptions, libLLVMExtra), Cuint, (LLVMAssumptionCacheRef, Ptr{LLVMValueRef}), AC, Assumes)
end

function LLVMExtraAssumptionCacheGetAssumptionsFor(AC, V, Assumes, Indices)
    ccall((:LLVMExtraAssumptionCacheGetAssumptionsFor, libLLVMExtra), Cuint, (LLVMAssumptionCacheRef, LLVMValueRef, Ptr{LLVMValueRef}, Ptr{Cint}), AC, V, Assumes, Indices)
end

function LLVMExtraAssumptionCacheRegisterAssumption(AC, Assume)
    ccall((:LLVMExtraAssumptionCacheRegisterAssumption, libLLVMExtra), LLVMBool, (LLVMAssumptionCacheRef, LLVMValueRef), AC, Assume)
end

function LLVMExtraAssumptionCacheClear(AC)
    ccall((:LLVMExtraAssumptionCacheClear, libLLVMExtra), Cvoid, (LLVMAssumptionCacheRef,), AC)
end

function LLVMExtraComputeConstantRange(V, ForSigned, UseInstrInfo, AC, CxtI, DT, DL, Lower, Upper)
    ccall((:LLVMExtraComputeConstantRange, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMBool, LLVMBool, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef, LLVMTargetDataRef, Ptr{UInt64}, Ptr{UInt64}), V, ForSigned, UseInstrInfo, AC, CxtI, DT, DL, Lower, Upper)
end

function LLVMExtraComputeKnownBits(V, UseInstrInfo, AC, CxtI, DT, DL, Zero, One)
    ccall((:LLVMExtraComputeKnownBits, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMBool, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef, LLVMTargetDataRef, Ptr{UInt64}, Ptr{UInt64}), V, UseInstrInfo, AC, CxtI, DT, DL, Zero, One)
end

function LLVMExtraIsValidAssumeForContext(Assume, CxtI, DT)
    ccall((:LLVMExtraIsValidAssumeForContext, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMValueRef, LLVMDominatorTreeRef), Assume, CxtI, DT)
end

function LLVMExtraIsGuaranteedNotToBePoison(V, AC, CxtI, DT)
    ccall((:LLVMExtraIsGuaranteedNotToBePoison, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef), V, AC, CxtI, DT)
end

function LLVMExtraProgramUndefinedIfPoison(Inst)
    ccall((:LLVMExtraProgramUndefinedIfPoison, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraIsGuaranteedNotToBeUndefOrPoison(V, AC, CxtI, DT)
    ccall((:LLVMExtraIsGuaranteedNotToBeUndefOrPoison, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef), V, AC, CxtI, DT)
end

@cenum LLVMExtraKnownFact::UInt32 begin
    LLVMExtraKnownNonNegative = 0
    LLVMExtraKnownPositive = 1
    LLVMExtraKnownNonZero = 2
end

function LLVMExtraIsKnown(Fact, V, UseInstrInfo, AC, CxtI, DT, DL, Known)
    ccall((:LLVMExtraIsKnown, libLLVMExtra), LLVMBool, (LLVMExtraKnownFact, LLVMValueRef, LLVMBool, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef, LLVMTargetDataRef, Ptr{LLVMBool}), Fact, V, UseInstrInfo, AC, CxtI, DT, DL, Known)
end

function LLVMExtraComputeNumSignBits(V, UseInstrInfo, AC, CxtI, DT, DL)
    ccall((:LLVMExtraComputeNumSignBits, libLLVMExtra), Cuint, (LLVMValueRef, LLVMBool, LLVMAssumptionCacheRef, LLVMValueRef, LLVMDominatorTreeRef, LLVMTargetDataRef), V, UseInstrInfo, AC, CxtI, DT, DL)
end

mutable struct LLVMOpaqueLazyValueInfo end

const LLVMLazyValueInfoRef = Ptr{LLVMOpaqueLazyValueInfo}

function LLVMExtraLazyValueInfoGetConstantRange(LVI, V, CxtI, UndefAllowed, Lower, Upper)
    ccall((:LLVMExtraLazyValueInfoGetConstantRange, libLLVMExtra), LLVMBool, (LLVMLazyValueInfoRef, LLVMValueRef, LLVMValueRef, LLVMBool, Ptr{UInt64}, Ptr{UInt64}), LVI, V, CxtI, UndefAllowed, Lower, Upper)
end

function LLVMExtraLazyValueInfoGetConstantRangeOnEdge(LVI, V, From, To, CxtI, Lower, Upper)
    ccall((:LLVMExtraLazyValueInfoGetConstantRangeOnEdge, libLLVMExtra), LLVMBool, (LLVMLazyValueInfoRef, LLVMValueRef, LLVMBasicBlockRef, LLVMBasicBlockRef, LLVMValueRef, Ptr{UInt64}, Ptr{UInt64}), LVI, V, From, To, CxtI, Lower, Upper)
end

function LLVMExtraLazyValueInfoGetConstantRangeAtUse(LVI, U, UndefAllowed, Lower, Upper)
    ccall((:LLVMExtraLazyValueInfoGetConstantRangeAtUse, libLLVMExtra), LLVMBool, (LLVMLazyValueInfoRef, LLVMUseRef, LLVMBool, Ptr{UInt64}, Ptr{UInt64}), LVI, U, UndefAllowed, Lower, Upper)
end

mutable struct LLVMOpaqueScalarEvolution end

const LLVMScalarEvolutionRef = Ptr{LLVMOpaqueScalarEvolution}

mutable struct LLVMOpaqueSCEV end

const LLVMSCEVRef = Ptr{LLVMOpaqueSCEV}

mutable struct LLVMOpaqueLoop end

const LLVMLoopRef = Ptr{LLVMOpaqueLoop}

@cenum LLVMExtraSCEVKind::UInt32 begin
    LLVMExtraSCEVConstantKind = 0
    LLVMExtraSCEVTruncateKind = 1
    LLVMExtraSCEVZeroExtendKind = 2
    LLVMExtraSCEVSignExtendKind = 3
    LLVMExtraSCEVAddKind = 4
    LLVMExtraSCEVMulKind = 5
    LLVMExtraSCEVUDivKind = 6
    LLVMExtraSCEVAddRecKind = 7
    LLVMExtraSCEVUMaxKind = 8
    LLVMExtraSCEVSMaxKind = 9
    LLVMExtraSCEVUMinKind = 10
    LLVMExtraSCEVSMinKind = 11
    LLVMExtraSCEVSequentialUMinKind = 12
    LLVMExtraSCEVUnknownKind = 13
    LLVMExtraSCEVCouldNotComputeKind = 14
    LLVMExtraSCEVVScaleKind = 15
    LLVMExtraSCEVPtrToIntKind = 16
    LLVMExtraSCEVOtherKind = 17
end

function LLVMExtraScalarEvolutionIsSCEVable(SE, Ty)
    ccall((:LLVMExtraScalarEvolutionIsSCEVable, libLLVMExtra), LLVMBool, (LLVMScalarEvolutionRef, LLVMTypeRef), SE, Ty)
end

function LLVMExtraScalarEvolutionGetSCEV(SE, V)
    ccall((:LLVMExtraScalarEvolutionGetSCEV, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, LLVMValueRef), SE, V)
end

function LLVMExtraScalarEvolutionGetAddExpr(SE, Ops, NumOps)
    ccall((:LLVMExtraScalarEvolutionGetAddExpr, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, Ptr{LLVMSCEVRef}, Cuint), SE, Ops, NumOps)
end

function LLVMExtraScalarEvolutionGetMinusSCEV(SE, LHS, RHS)
    ccall((:LLVMExtraScalarEvolutionGetMinusSCEV, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, LLVMSCEVRef, LLVMSCEVRef), SE, LHS, RHS)
end

function LLVMExtraScalarEvolutionGetMulExpr(SE, Ops, NumOps)
    ccall((:LLVMExtraScalarEvolutionGetMulExpr, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, Ptr{LLVMSCEVRef}, Cuint), SE, Ops, NumOps)
end

function LLVMExtraScalarEvolutionGetCastExpr(SE, Opcode, S, Ty)
    ccall((:LLVMExtraScalarEvolutionGetCastExpr, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, LLVMOpcode, LLVMSCEVRef, LLVMTypeRef), SE, Opcode, S, Ty)
end

function LLVMExtraScalarEvolutionIsLoopInvariant(SE, S, L)
    ccall((:LLVMExtraScalarEvolutionIsLoopInvariant, libLLVMExtra), LLVMBool, (LLVMScalarEvolutionRef, LLVMSCEVRef, LLVMLoopRef), SE, S, L)
end

@cenum LLVMExtraBackedgeTakenCountKind::UInt32 begin
    LLVMExtraExactBackedgeTakenCount = 0
    LLVMExtraConstantMaxBackedgeTakenCount = 1
    LLVMExtraSymbolicMaxBackedgeTakenCount = 2
end

function LLVMExtraScalarEvolutionGetBackedgeTakenCount(SE, L, Kind)
    ccall((:LLVMExtraScalarEvolutionGetBackedgeTakenCount, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, LLVMLoopRef, LLVMExtraBackedgeTakenCountKind), SE, L, Kind)
end

function LLVMExtraSCEVAddRecGetStepRecurrence(SE, S)
    ccall((:LLVMExtraSCEVAddRecGetStepRecurrence, libLLVMExtra), LLVMSCEVRef, (LLVMScalarEvolutionRef, LLVMSCEVRef), SE, S)
end

function LLVMExtraSCEVGetNoWrapFlags(S)
    ccall((:LLVMExtraSCEVGetNoWrapFlags, libLLVMExtra), Cuint, (LLVMSCEVRef,), S)
end

function LLVMExtraScalarEvolutionGetRange(SE, S, Signed, Lower, Upper)
    ccall((:LLVMExtraScalarEvolutionGetRange, libLLVMExtra), Cuint, (LLVMScalarEvolutionRef, LLVMSCEVRef, LLVMBool, Ptr{UInt64}, Ptr{UInt64}), SE, S, Signed, Lower, Upper)
end

function LLVMExtraSCEVGetKind(S)
    ccall((:LLVMExtraSCEVGetKind, libLLVMExtra), LLVMExtraSCEVKind, (LLVMSCEVRef,), S)
end

function LLVMExtraSCEVGetType(S)
    ccall((:LLVMExtraSCEVGetType, libLLVMExtra), LLVMTypeRef, (LLVMSCEVRef,), S)
end

function LLVMExtraSCEVGetOperands(S, Ops)
    ccall((:LLVMExtraSCEVGetOperands, libLLVMExtra), Cuint, (LLVMSCEVRef, Ptr{LLVMSCEVRef}), S, Ops)
end

function LLVMExtraSCEVGetValue(S)
    ccall((:LLVMExtraSCEVGetValue, libLLVMExtra), LLVMValueRef, (LLVMSCEVRef,), S)
end

function LLVMExtraSCEVAddRecGetLoop(S)
    ccall((:LLVMExtraSCEVAddRecGetLoop, libLLVMExtra), LLVMLoopRef, (LLVMSCEVRef,), S)
end

function LLVMExtraSCEVContains(S, Kind)
    ccall((:LLVMExtraSCEVContains, libLLVMExtra), LLVMBool, (LLVMSCEVRef, LLVMExtraSCEVKind), S, Kind)
end

function LLVMExtraPrintSCEVToString(S)
    ccall((:LLVMExtraPrintSCEVToString, libLLVMExtra), Cstring, (LLVMSCEVRef,), S)
end

mutable struct LLVMOpaqueLoopInfo end

const LLVMLoopInfoRef = Ptr{LLVMOpaqueLoopInfo}

function LLVMExtraLoopInfoGetLoopFor(LI, BB)
    ccall((:LLVMExtraLoopInfoGetLoopFor, libLLVMExtra), LLVMLoopRef, (LLVMLoopInfoRef, LLVMBasicBlockRef), LI, BB)
end

function LLVMExtraLoopGetHeader(L)
    ccall((:LLVMExtraLoopGetHeader, libLLVMExtra), LLVMBasicBlockRef, (LLVMLoopRef,), L)
end

function LLVMExtraLoopGetParent(L)
    ccall((:LLVMExtraLoopGetParent, libLLVMExtra), LLVMLoopRef, (LLVMLoopRef,), L)
end

function LLVMExtraLoopGetDepth(L)
    ccall((:LLVMExtraLoopGetDepth, libLLVMExtra), Cuint, (LLVMLoopRef,), L)
end

function LLVMExtraLoopContains(L, BB)
    ccall((:LLVMExtraLoopContains, libLLVMExtra), LLVMBool, (LLVMLoopRef, LLVMBasicBlockRef), L, BB)
end

function LLVMExtraLoopGetNumBlocks(L)
    ccall((:LLVMExtraLoopGetNumBlocks, libLLVMExtra), Cuint, (LLVMLoopRef,), L)
end

function LLVMExtraLoopGetBlock(L, Index)
    ccall((:LLVMExtraLoopGetBlock, libLLVMExtra), LLVMBasicBlockRef, (LLVMLoopRef, Cuint), L, Index)
end

function LLVMExtraLoopGetNumSubLoops(L)
    ccall((:LLVMExtraLoopGetNumSubLoops, libLLVMExtra), Cuint, (LLVMLoopRef,), L)
end

function LLVMExtraLoopGetSubLoop(L, Index)
    ccall((:LLVMExtraLoopGetSubLoop, libLLVMExtra), LLVMLoopRef, (LLVMLoopRef, Cuint), L, Index)
end

function LLVMExtraLoopGetUniqueExitBlocks(L, Blocks)
    ccall((:LLVMExtraLoopGetUniqueExitBlocks, libLLVMExtra), Cuint, (LLVMLoopRef, Ptr{LLVMBasicBlockRef}), L, Blocks)
end

function LLVMExtraLoopInfoGetTopLevelLoops(LI, Loops)
    ccall((:LLVMExtraLoopInfoGetTopLevelLoops, libLLVMExtra), Cuint, (LLVMLoopInfoRef, Ptr{LLVMLoopRef}), LI, Loops)
end

function LLVMExtraLoopGetLatch(L)
    ccall((:LLVMExtraLoopGetLatch, libLLVMExtra), LLVMBasicBlockRef, (LLVMLoopRef,), L)
end

function LLVMExtraLoopGetPreheader(L)
    ccall((:LLVMExtraLoopGetPreheader, libLLVMExtra), LLVMBasicBlockRef, (LLVMLoopRef,), L)
end

function LLVMExtraLoopIsLoopInvariant(L, V)
    ccall((:LLVMExtraLoopIsLoopInvariant, libLLVMExtra), LLVMBool, (LLVMLoopRef, LLVMValueRef), L, V)
end

function LLVMExtraDominatorTreeInstructionDominatesUse(Tree, Inst, U)
    ccall((:LLVMExtraDominatorTreeInstructionDominatesUse, libLLVMExtra), LLVMBool, (LLVMDominatorTreeRef, LLVMValueRef, LLVMUseRef), Tree, Inst, U)
end

function LLVMExtraDominatorTreeBlockDominates(Tree, A, B)
    ccall((:LLVMExtraDominatorTreeBlockDominates, libLLVMExtra), LLVMBool, (LLVMDominatorTreeRef, LLVMBasicBlockRef, LLVMBasicBlockRef), Tree, A, B)
end

function LLVMExtraDominatorTreeRecalculate(Tree, F)
    ccall((:LLVMExtraDominatorTreeRecalculate, libLLVMExtra), Cvoid, (LLVMDominatorTreeRef, LLVMValueRef), Tree, F)
end

function LLVMExtraSplitBlockAndInsertIfThen(Cond, SplitBefore, Unreachable, BranchWeights, NumBranchWeights, Tree)
    ccall((:LLVMExtraSplitBlockAndInsertIfThen, libLLVMExtra), LLVMValueRef, (LLVMValueRef, LLVMValueRef, LLVMBool, Ptr{Cuint}, Cuint, LLVMDominatorTreeRef), Cond, SplitBefore, Unreachable, BranchWeights, NumBranchWeights, Tree)
end

function LLVMExtraSplitBlockAndInsertIfThenElse(Cond, SplitBefore, BranchWeights, NumBranchWeights, Tree, ThenTerm, ElseTerm)
    ccall((:LLVMExtraSplitBlockAndInsertIfThenElse, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMValueRef, Ptr{Cuint}, Cuint, LLVMDominatorTreeRef, Ptr{LLVMValueRef}, Ptr{LLVMValueRef}), Cond, SplitBefore, BranchWeights, NumBranchWeights, Tree, ThenTerm, ElseTerm)
end

function LLVMExtraIsInstructionTriviallyDead(Inst)
    ccall((:LLVMExtraIsInstructionTriviallyDead, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraRecursivelyDeleteTriviallyDeadInstructions(Inst)
    ccall((:LLVMExtraRecursivelyDeleteTriviallyDeadInstructions, libLLVMExtra), LLVMBool, (LLVMValueRef,), Inst)
end

function LLVMExtraRecursivelyDeleteTriviallyDeadInstructionsPermissive(Insts, NumInsts)
    ccall((:LLVMExtraRecursivelyDeleteTriviallyDeadInstructionsPermissive, libLLVMExtra), LLVMBool, (Ptr{LLVMValueRef}, Cuint), Insts, NumInsts)
end

function LLVMExtraIsAllocaPromotable(Alloca)
    ccall((:LLVMExtraIsAllocaPromotable, libLLVMExtra), LLVMBool, (LLVMValueRef,), Alloca)
end

function LLVMExtraPromoteMemToReg(Allocas, NumAllocas, Tree, AC)
    ccall((:LLVMExtraPromoteMemToReg, libLLVMExtra), LLVMBool, (Ptr{LLVMValueRef}, Cuint, LLVMDominatorTreeRef, LLVMAssumptionCacheRef), Allocas, NumAllocas, Tree, AC)
end

function LLVMExtraConstantFoldCastOperand(Opcode, C, DestTy, TD, Result)
    ccall((:LLVMExtraConstantFoldCastOperand, libLLVMExtra), LLVMBool, (LLVMOpcode, LLVMValueRef, LLVMTypeRef, LLVMTargetDataRef, Ptr{LLVMValueRef}), Opcode, C, DestTy, TD, Result)
end

function LLVMExtraSimplifyInstruction(Inst)
    ccall((:LLVMExtraSimplifyInstruction, libLLVMExtra), LLVMValueRef, (LLVMValueRef,), Inst)
end

function LLVMExtraSplitBasicBlock(BB, Before, Head, Name)
    ccall((:LLVMExtraSplitBasicBlock, libLLVMExtra), LLVMBasicBlockRef, (LLVMBasicBlockRef, LLVMValueRef, LLVMBool, Cstring), BB, Before, Head, Name)
end

function LLVMExtraInlineFunction(Call, MergeAttributes, InsertLifetime, FailureReason)
    ccall((:LLVMExtraInlineFunction, libLLVMExtra), LLVMBool, (LLVMValueRef, LLVMBool, LLVMBool, Ptr{Cstring}), Call, MergeAttributes, InsertLifetime, FailureReason)
end

function LLVMExtraIsInlineViable(Fn, FailureReason)
    ccall((:LLVMExtraIsInlineViable, libLLVMExtra), LLVMBool, (LLVMValueRef, Ptr{Cstring}), Fn, FailureReason)
end

function LLVMExtraTripleGetArchTypeName(Triple)
    ccall((:LLVMExtraTripleGetArchTypeName, libLLVMExtra), Cstring, (Cstring,), Triple)
end

function LLVMExtraTripleGetVendorTypeName(Triple)
    ccall((:LLVMExtraTripleGetVendorTypeName, libLLVMExtra), Cstring, (Cstring,), Triple)
end

function LLVMExtraTripleGetOSTypeName(Triple)
    ccall((:LLVMExtraTripleGetOSTypeName, libLLVMExtra), Cstring, (Cstring,), Triple)
end

function LLVMExtraTripleGetEnvironmentTypeName(Triple)
    ccall((:LLVMExtraTripleGetEnvironmentTypeName, libLLVMExtra), Cstring, (Cstring,), Triple)
end

function LLVMExtraTripleGetObjectFormatTypeName(Triple)
    ccall((:LLVMExtraTripleGetObjectFormatTypeName, libLLVMExtra), Cstring, (Cstring,), Triple)
end

function LLVMExtraTripleIsX86(Triple)
    ccall((:LLVMExtraTripleIsX86, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsAArch64(Triple)
    ccall((:LLVMExtraTripleIsAArch64, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsARM(Triple)
    ccall((:LLVMExtraTripleIsARM, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsThumb(Triple)
    ccall((:LLVMExtraTripleIsThumb, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsRISCV(Triple)
    ccall((:LLVMExtraTripleIsRISCV, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsPPC(Triple)
    ccall((:LLVMExtraTripleIsPPC, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsNVPTX(Triple)
    ccall((:LLVMExtraTripleIsNVPTX, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsAMDGPU(Triple)
    ccall((:LLVMExtraTripleIsAMDGPU, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsSPIRV(Triple)
    ccall((:LLVMExtraTripleIsSPIRV, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsWasm(Triple)
    ccall((:LLVMExtraTripleIsWasm, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsOSDarwin(Triple)
    ccall((:LLVMExtraTripleIsOSDarwin, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsOSWindows(Triple)
    ccall((:LLVMExtraTripleIsOSWindows, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

function LLVMExtraTripleIsOSLinux(Triple)
    ccall((:LLVMExtraTripleIsOSLinux, libLLVMExtra), LLVMBool, (Cstring,), Triple)
end

mutable struct LLVMOpaqueMCSubtargetInfo end

const LLVMMCSubtargetInfoRef = Ptr{LLVMOpaqueMCSubtargetInfo}

function LLVMExtraCreateMCSubtargetInfo(T, Triple, CPU, Features)
    ccall((:LLVMExtraCreateMCSubtargetInfo, libLLVMExtra), LLVMMCSubtargetInfoRef, (LLVMTargetRef, Cstring, Cstring, Cstring), T, Triple, CPU, Features)
end

function LLVMExtraDisposeMCSubtargetInfo(STI)
    ccall((:LLVMExtraDisposeMCSubtargetInfo, libLLVMExtra), Cvoid, (LLVMMCSubtargetInfoRef,), STI)
end

function LLVMExtraGetTargetMachineSubtargetInfo(TM, F)
    ccall((:LLVMExtraGetTargetMachineSubtargetInfo, libLLVMExtra), LLVMMCSubtargetInfoRef, (LLVMTargetMachineRef, LLVMValueRef), TM, F)
end

function LLVMExtraMCSubtargetInfoGetTriple(STI)
    ccall((:LLVMExtraMCSubtargetInfoGetTriple, libLLVMExtra), Cstring, (LLVMMCSubtargetInfoRef,), STI)
end

function LLVMExtraMCSubtargetInfoGetCPU(STI)
    ccall((:LLVMExtraMCSubtargetInfoGetCPU, libLLVMExtra), Cstring, (LLVMMCSubtargetInfoRef,), STI)
end

function LLVMExtraMCSubtargetInfoGetFeatureString(STI)
    ccall((:LLVMExtraMCSubtargetInfoGetFeatureString, libLLVMExtra), Cstring, (LLVMMCSubtargetInfoRef,), STI)
end

function LLVMExtraMCSubtargetInfoGetNumProcessorFeatures(STI)
    ccall((:LLVMExtraMCSubtargetInfoGetNumProcessorFeatures, libLLVMExtra), Cuint, (LLVMMCSubtargetInfoRef,), STI)
end

function LLVMExtraMCSubtargetInfoGetProcessorFeatureName(STI, Index)
    ccall((:LLVMExtraMCSubtargetInfoGetProcessorFeatureName, libLLVMExtra), Cstring, (LLVMMCSubtargetInfoRef, Cuint), STI, Index)
end

function LLVMExtraMCSubtargetInfoLookupFeature(STI, Name, Enabled)
    ccall((:LLVMExtraMCSubtargetInfoLookupFeature, libLLVMExtra), LLVMBool, (LLVMMCSubtargetInfoRef, Cstring, Ptr{LLVMBool}), STI, Name, Enabled)
end
