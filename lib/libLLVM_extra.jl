using CEnum

function LLVMInitializeAllTargetInfos()
    ccall((:LLVMInitializeAllTargetInfos, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeAllTargets()
    ccall((:LLVMInitializeAllTargets, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeAllTargetMCs()
    ccall((:LLVMInitializeAllTargetMCs, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeAllAsmPrinters()
    ccall((:LLVMInitializeAllAsmPrinters, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeAllAsmParsers()
    ccall((:LLVMInitializeAllAsmParsers, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeAllDisassemblers()
    ccall((:LLVMInitializeAllDisassemblers, libLLVMExtra), Cvoid, ())
end

function LLVMInitializeNativeTarget()
    ccall((:LLVMInitializeNativeTarget, libLLVMExtra), LLVMBool, ())
end

function LLVMInitializeNativeAsmParser()
    ccall((:LLVMInitializeNativeAsmParser, libLLVMExtra), LLVMBool, ())
end

function LLVMInitializeNativeAsmPrinter()
    ccall((:LLVMInitializeNativeAsmPrinter, libLLVMExtra), LLVMBool, ())
end

function LLVMInitializeNativeDisassembler()
    ccall((:LLVMInitializeNativeDisassembler, libLLVMExtra), LLVMBool, ())
end

function LLVMAddBarrierNoopPass(PM)
    ccall((:LLVMAddBarrierNoopPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddDivRemPairsPass(PM)
    ccall((:LLVMAddDivRemPairsPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddLoopDistributePass(PM)
    ccall((:LLVMAddLoopDistributePass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddLoopFusePass(PM)
    ccall((:LLVMAddLoopFusePass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddLoopLoadEliminationPass(PM)
    ccall((:LLVMAddLoopLoadEliminationPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddLoadStoreVectorizerPass(PM)
    ccall((:LLVMAddLoadStoreVectorizerPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMAddVectorCombinePass(PM)
    ccall((:LLVMAddVectorCombinePass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

if version() < v"12"
function LLVMAddInstructionSimplifyPass(PM)
    ccall((:LLVMAddInstructionSimplifyPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end
end

mutable struct LLVMOpaquePass end

const LLVMPassRef = Ptr{LLVMOpaquePass}

function LLVMAddPass(PM, P)
    ccall((:LLVMAddPass, libLLVMExtra), Cvoid, (LLVMPassManagerRef, LLVMPassRef), PM, P)
end

# typedef LLVMBool ( * LLVMPassCallback ) ( void * Ref , void * Data )
const LLVMPassCallback = Ptr{Cvoid}

function LLVMCreateModulePass2(Name, Callback, Data)
    ccall((:LLVMCreateModulePass2, libLLVMExtra), LLVMPassRef, (Cstring, LLVMPassCallback, Ptr{Cvoid}), Name, Callback, Data)
end

function LLVMCreateFunctionPass2(Name, Callback, Data)
    ccall((:LLVMCreateFunctionPass2, libLLVMExtra), LLVMPassRef, (Cstring, LLVMPassCallback, Ptr{Cvoid}), Name, Callback, Data)
end

function LLVMGetDebugMDVersion()
    ccall((:LLVMGetDebugMDVersion, libLLVMExtra), Cuint, ())
end

function LLVMGetValueContext(V)
    ccall((:LLVMGetValueContext, libLLVMExtra), LLVMContextRef, (LLVMValueRef,), V)
end

function LLVMAddTargetLibraryInfoByTriple(T, PM)
    ccall((:LLVMAddTargetLibraryInfoByTriple, libLLVMExtra), Cvoid, (Cstring, LLVMPassManagerRef), T, PM)
end

function LLVMAddInternalizePassWithExportList(PM, ExportList, Length)
    ccall((:LLVMAddInternalizePassWithExportList, libLLVMExtra), Cvoid, (LLVMPassManagerRef, Ptr{Cstring}, Csize_t), PM, ExportList, Length)
end

function LLVMExtraAppendToUsed(Mod, Values, Count)
    ccall((:LLVMExtraAppendToUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMExtraAppendToCompilerUsed(Mod, Values, Count)
    ccall((:LLVMExtraAppendToCompilerUsed, libLLVMExtra), Cvoid, (LLVMModuleRef, Ptr{LLVMValueRef}, Csize_t), Mod, Values, Count)
end

function LLVMExtraAddGenericAnalysisPasses(PM)
    ccall((:LLVMExtraAddGenericAnalysisPasses, libLLVMExtra), Cvoid, (LLVMPassManagerRef,), PM)
end

function LLVMExtraSetInitializer(GlobalVar, ConstantVal)
    ccall((:LLVMExtraSetInitializer, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), GlobalVar, ConstantVal)
end

function LLVMExtraSetPersonalityFn(Fn, PersonalityFn)
    ccall((:LLVMExtraSetPersonalityFn, libLLVMExtra), Cvoid, (LLVMValueRef, LLVMValueRef), Fn, PersonalityFn)
end

function LLVMExtraDIScopeGetName(Scope, Len)
    ccall((:LLVMExtraDIScopeGetName, libLLVMExtra), Cstring, (LLVMMetadataRef, Ptr{Cuint}), Scope, Len)
end

if version().major == 12

struct LLVMOrcCSymbolFlagsMapPair
    Name::LLVMOrcSymbolStringPoolEntryRef
    Flags::LLVMJITSymbolFlags
end

const LLVMOrcCSymbolFlagsMapPairs = Ptr{LLVMOrcCSymbolFlagsMapPair}

struct LLVMOrcCSymbolAliasMapEntry
    Name::LLVMOrcSymbolStringPoolEntryRef
    Flags::LLVMJITSymbolFlags
end

struct LLVMOrcCSymbolAliasMapPair
    Name::LLVMOrcSymbolStringPoolEntryRef
    Entry::LLVMOrcCSymbolAliasMapEntry
end

const LLVMOrcCSymbolAliasMapPairs = Ptr{LLVMOrcCSymbolAliasMapPair}

mutable struct LLVMOrcOpaqueMaterializationResponsibility end

const LLVMOrcMaterializationResponsibilityRef = Ptr{LLVMOrcOpaqueMaterializationResponsibility}

# typedef void ( * LLVMOrcMaterializationUnitMaterializeFunction ) ( void * Ctx , LLVMOrcMaterializationResponsibilityRef MR )
const LLVMOrcMaterializationUnitMaterializeFunction = Ptr{Cvoid}

# typedef void ( * LLVMOrcMaterializationUnitDiscardFunction ) ( void * Ctx , LLVMOrcJITDylibRef JD , LLVMOrcSymbolStringPoolEntryRef Symbol )
const LLVMOrcMaterializationUnitDiscardFunction = Ptr{Cvoid}

# typedef void ( * LLVMOrcMaterializationUnitDestroyFunction ) ( void * Ctx )
const LLVMOrcMaterializationUnitDestroyFunction = Ptr{Cvoid}

# typedef LLVMErrorRef ( * LLVMOrcGenericIRModuleOperationFunction ) ( void * Ctx , LLVMModuleRef M )
const LLVMOrcGenericIRModuleOperationFunction = Ptr{Cvoid}

mutable struct LLVMOrcOpaqueIRTransformLayer end

const LLVMOrcIRTransformLayerRef = Ptr{LLVMOrcOpaqueIRTransformLayer}

# typedef LLVMErrorRef ( * LLVMOrcIRTransformLayerTransformFunction ) ( void * Ctx , LLVMOrcThreadSafeModuleRef * ModInOut , LLVMOrcMaterializationResponsibilityRef MR )
const LLVMOrcIRTransformLayerTransformFunction = Ptr{Cvoid}

mutable struct LLVMOrcOpaqueIndirectStubsManager end

const LLVMOrcIndirectStubsManagerRef = Ptr{LLVMOrcOpaqueIndirectStubsManager}

mutable struct LLVMOrcOpaqueLazyCallThroughManager end

const LLVMOrcLazyCallThroughManagerRef = Ptr{LLVMOrcOpaqueLazyCallThroughManager}

function LLVMOrcCreateCustomMaterializationUnit(Name, Ctx, Syms, NumSyms, InitSym, Materialize, Discard, Destroy)
    ccall((:LLVMOrcCreateCustomMaterializationUnit, libLLVMExtra), LLVMOrcMaterializationUnitRef, (Cstring, Ptr{Cvoid}, LLVMOrcCSymbolFlagsMapPairs, Csize_t, LLVMOrcSymbolStringPoolEntryRef, LLVMOrcMaterializationUnitMaterializeFunction, LLVMOrcMaterializationUnitDiscardFunction, LLVMOrcMaterializationUnitDestroyFunction), Name, Ctx, Syms, NumSyms, InitSym, Materialize, Discard, Destroy)
end

function LLVMOrcCreateIRMaterializationUnit(IL, TSM)
    ccall((:LLVMOrcCreateIRMaterializationUnit, libLLVMExtra), LLVMOrcMaterializationUnitRef, (LLVMOrcIRTransformLayerRef, LLVMOrcThreadSafeModuleRef), IL, TSM)
end

function LLVMOrcLazyReexports(LCTM, ISM, SourceRef, CallableAliases, NumPairs)
    ccall((:LLVMOrcLazyReexports, libLLVMExtra), LLVMOrcMaterializationUnitRef, (LLVMOrcLazyCallThroughManagerRef, LLVMOrcIndirectStubsManagerRef, LLVMOrcJITDylibRef, LLVMOrcCSymbolAliasMapPairs, Csize_t), LCTM, ISM, SourceRef, CallableAliases, NumPairs)
end

function LLVMOrcMaterializationResponsibilityGetTargetDylib(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetTargetDylib, libLLVMExtra), LLVMOrcJITDylibRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end


function LLVMOrcMaterializationResponsibilityGetExecutionSession(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetExecutionSession, libLLVMExtra), LLVMOrcExecutionSessionRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

function LLVMOrcMaterializationResponsibilityGetInitializerSymbol(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetInitializerSymbol, libLLVMExtra), LLVMOrcSymbolStringPoolEntryRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

function LLVMOrcMaterializationResponsibilityGetRequestedSymbols(MR, NumSymbols)
    ccall((:LLVMOrcMaterializationResponsibilityGetRequestedSymbols, libLLVMExtra), Ptr{LLVMOrcSymbolStringPoolEntryRef}, (LLVMOrcMaterializationResponsibilityRef, Ptr{Csize_t}), MR, NumSymbols)
end

function LLVMOrcDisposeSymbols(Symbols)
    ccall((:LLVMOrcDisposeSymbols, libLLVMExtra), Cvoid, (Ptr{LLVMOrcSymbolStringPoolEntryRef},), Symbols)
end

function LLVMOrcMaterializationResponsibilityNotifyResolved(MR, Symbols, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityNotifyResolved, libLLVMExtra), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcCSymbolMapPairs, Csize_t), MR, Symbols, NumPairs)
end

function LLVMOrcMaterializationResponsibilityNotifyEmitted(MR)
    ccall((:LLVMOrcMaterializationResponsibilityNotifyEmitted, libLLVMExtra), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

function LLVMOrcMaterializationResponsibilityDefineMaterializing(MR, Syms, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityDefineMaterializing, libLLVMExtra), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcCSymbolMapPairs, Csize_t), MR, Syms, NumPairs)
end

function LLVMOrcMaterializationResponsibilityDefineNonExistent(MR, Symbols, NumSymbols)
    ccall((:LLVMOrcMaterializationResponsibilityDefineNonExistent, libLLVMExtra), Cvoid, (LLVMOrcMaterializationResponsibilityRef, Ptr{LLVMOrcSymbolStringPoolEntryRef}, Csize_t), MR, Symbols, NumSymbols)
end

function LLVMOrcMaterializationResponsibilityFailMaterialization(MR)
    ccall((:LLVMOrcMaterializationResponsibilityFailMaterialization, libLLVMExtra), Cvoid, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

function LLVMOrcMaterializationResponsibilityReplace(MR, MU)
    ccall((:LLVMOrcMaterializationResponsibilityReplace, libLLVMExtra), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcMaterializationUnitRef), MR, MU)
end

function LLVMOrcMaterializationResponsibilityDelegate(MR, Symbols, NumSymbols)
    ccall((:LLVMOrcMaterializationResponsibilityDelegate, libLLVMExtra), LLVMOrcMaterializationResponsibilityRef, (LLVMOrcMaterializationResponsibilityRef, Ptr{LLVMOrcSymbolStringPoolEntryRef}, Csize_t), MR, Symbols, NumSymbols)
end

function LLVMOrcThreadSafeModuleWithModuleDo(TSM, F, Ctx)
    ccall((:LLVMOrcThreadSafeModuleWithModuleDo, libLLVMExtra), LLVMErrorRef, (LLVMOrcThreadSafeModuleRef, LLVMOrcGenericIRModuleOperationFunction, Ptr{Cvoid}), TSM, F, Ctx)
end

function LLVMOrcIRTransformLayerEmit(IRTransformLayer, MR, TSM)
    ccall((:LLVMOrcIRTransformLayerEmit, libLLVMExtra), Cvoid, (LLVMOrcIRTransformLayerRef, LLVMOrcMaterializationResponsibilityRef, LLVMOrcThreadSafeModuleRef), IRTransformLayer, MR, TSM)
end

function LLVMOrcIRTransformLayerSetTransform(IRTransformLayer, TransformFunction, Ctx)
    ccall((:LLVMOrcIRTransformLayerSetTransform, libLLVMExtra), Cvoid, (LLVMOrcIRTransformLayerRef, LLVMOrcIRTransformLayerTransformFunction, Ptr{Cvoid}), IRTransformLayer, TransformFunction, Ctx)
end

function LLVMOrcCreateLocalIndirectStubsManager(TargetTriple)
    ccall((:LLVMOrcCreateLocalIndirectStubsManager, libLLVMExtra), LLVMOrcIndirectStubsManagerRef, (Cstring,), TargetTriple)
end

function LLVMOrcDisposeIndirectStubsManager(ISM)
    ccall((:LLVMOrcDisposeIndirectStubsManager, libLLVMExtra), Cvoid, (LLVMOrcIndirectStubsManagerRef,), ISM)
end

function LLVMOrcCreateLocalLazyCallThroughManager(TargetTriple, ES, ErrorHandlerAddr, Result)
    ccall((:LLVMOrcCreateLocalLazyCallThroughManager, libLLVMExtra), LLVMErrorRef, (Cstring, LLVMOrcExecutionSessionRef, LLVMOrcJITTargetAddress, Ptr{LLVMOrcLazyCallThroughManagerRef}), TargetTriple, ES, ErrorHandlerAddr, Result)
end

function LLVMOrcDisposeLazyCallThroughManager(LCM)
    ccall((:LLVMOrcDisposeLazyCallThroughManager, libLLVMExtra), Cvoid, (LLVMOrcLazyCallThroughManagerRef,), LCM)
end

function LLVMOrcLLJITGetIRTransformLayer(J)
    ccall((:LLVMOrcLLJITGetIRTransformLayer, libLLVMExtra), LLVMOrcIRTransformLayerRef, (LLVMOrcLLJITRef,), J)
end

function LLVMOrcLLJITApplyDataLayout(J, Mod)
    ccall((:LLVMOrcLLJITApplyDataLayout, libLLVMExtra), LLVMErrorRef, (LLVMOrcLLJITRef, LLVMModuleRef), J, Mod)
end

end # version
