#ifndef LLVMEXTRA_H
#define LLVMEXTRA_H

#include "llvm/Config/llvm-config.h"
#include <llvm-c/Core.h>
#include <llvm-c/Orc.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/PassBuilder.h>
#include <llvm-c/Types.h>
#include <llvm/Support/CBindingWrapping.h>

LLVM_C_EXTERN_C_BEGIN

// XXX: without this, Clang.jl doesn't emit LLVMExtraInitializeNativeTarget.
//      maybe LLVM_C_EXTERN_C_BEGIN somehow eats the function definition?
void dummy();

// Initialization functions
LLVMBool LLVMExtraInitializeNativeTarget(void);
LLVMBool LLVMExtraInitializeNativeAsmParser(void);
LLVMBool LLVMExtraInitializeNativeAsmPrinter(void);
LLVMBool LLVMExtraInitializeNativeDisassembler(void);

typedef enum {
  LLVMDebugEmissionKindNoDebug = 0,
  LLVMDebugEmissionKindFullDebug = 1,
  LLVMDebugEmissionKindLineTablesOnly = 2,
  LLVMDebugEmissionKindDebugDirectivesOnly = 3
} LLVMDebugEmissionKind;

// Missing LegacyPM passes
void LLVMAddBarrierNoopPass(LLVMPassManagerRef PM);
#if LLVM_VERSION_MAJOR < 17
void LLVMAddDivRemPairsPass(LLVMPassManagerRef PM);
void LLVMAddLoopDistributePass(LLVMPassManagerRef PM);
void LLVMAddLoopFusePass(LLVMPassManagerRef PM);
void LLVMAddLoopLoadEliminationPass(LLVMPassManagerRef PM);
#endif
void LLVMAddLoadStoreVectorizerPass(LLVMPassManagerRef PM);
#if LLVM_VERSION_MAJOR < 17
void LLVMAddVectorCombinePass(LLVMPassManagerRef PM);
#endif
void LLVMAddSpeculativeExecutionIfHasBranchDivergencePass(LLVMPassManagerRef PM);
#if LLVM_VERSION_MAJOR < 17
void LLVMAddSimpleLoopUnrollPass(LLVMPassManagerRef PM);
void LLVMAddInductiveRangeCheckEliminationPass(LLVMPassManagerRef PM);
#endif
void LLVMAddSimpleLoopUnswitchLegacyPass(LLVMPassManagerRef PM);
void LLVMAddExpandReductionsPass(LLVMPassManagerRef PM);
#if LLVM_VERSION_MAJOR >= 17
void LLVMAddCFGSimplificationPass2(LLVMPassManagerRef PM, int BonusInstThreshold,
                                   LLVMBool ForwardSwitchCondToPhi,
                                   LLVMBool ConvertSwitchToLookupTable,
                                   LLVMBool NeedCanonicalLoop, LLVMBool HoistCommonInsts,
                                   LLVMBool SinkCommonInsts, LLVMBool SimplifyCondBranch,
                                   LLVMBool SpeculateBlocks);
#else
void LLVMAddCFGSimplificationPass2(LLVMPassManagerRef PM, int BonusInstThreshold,
                                   LLVMBool ForwardSwitchCondToPhi,
                                   LLVMBool ConvertSwitchToLookupTable,
                                   LLVMBool NeedCanonicalLoop, LLVMBool HoistCommonInsts,
                                   LLVMBool SinkCommonInsts, LLVMBool SimplifyCondBranch,
                                   LLVMBool FoldTwoEntryPHINode);
#endif
#if LLVM_VERSION_MAJOR < 17
void LLVMAddInternalizePassWithExportList(LLVMPassManagerRef PM, const char **ExportList,
                                          size_t Length);
#endif

// Custom LegacyPM pass infrastructure
typedef struct LLVMOpaquePass *LLVMPassRef;
void LLVMAddPass(LLVMPassManagerRef PM, LLVMPassRef P);
typedef LLVMBool (*LLVMPassCallback)(void *Ref, void *Data);
LLVMPassRef LLVMCreateModulePass2(const char *Name, LLVMPassCallback Callback, void *Data);
LLVMPassRef LLVMCreateFunctionPass2(const char *Name, LLVMPassCallback Callback,
                                    void *Data);

// Missing functionality
void LLVMAddTargetLibraryInfoByTriple(const char *T, LLVMPassManagerRef PM);
void LLVMAppendToUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
void LLVMAppendToCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
void LLVMAddGenericAnalysisPasses(LLVMPassManagerRef PM);
void LLVMDumpMetadata(LLVMMetadataRef MD);
char *LLVMPrintMetadataToString(LLVMMetadataRef MD);
const char *LLVMDIScopeGetName(LLVMMetadataRef File, unsigned *Len);
void LLVMFunctionDeleteBody(LLVMValueRef Func);
void LLVMDestroyConstant(LLVMValueRef Const);
LLVMTypeRef LLVMGetFunctionType(LLVMValueRef Fn);
LLVMTypeRef LLVMGetGlobalValueType(LLVMValueRef Fn);

// Bug fixes
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#105521
void LLVMSetInitializer2(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal);
void LLVMSetPersonalityFn2(LLVMValueRef Fn, LLVMValueRef PersonalityFn);
#endif

// APIs without MetadataAsValue
const char *LLVMGetMDString2(LLVMMetadataRef MD, unsigned *Length);
unsigned LLVMGetMDNodeNumOperands2(LLVMMetadataRef MD);
void LLVMGetMDNodeOperands2(LLVMMetadataRef MD, LLVMMetadataRef *Dest);
unsigned LLVMGetNamedMetadataNumOperands2(LLVMNamedMDNodeRef NMD);
void LLVMGetNamedMetadataOperands2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef *Dest);
void LLVMAddNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef Val);
void LLVMReplaceMDNodeOperandWith2(LLVMMetadataRef MD, unsigned I, LLVMMetadataRef New);

// ORC API extensions
typedef struct LLVMOrcOpaqueIRCompileLayer *LLVMOrcIRCompileLayerRef;
void LLVMOrcIRCompileLayerEmit(LLVMOrcIRCompileLayerRef IRLayer,
                               LLVMOrcMaterializationResponsibilityRef MR,
                               LLVMOrcThreadSafeModuleRef TSM);
char *LLVMDumpJitDylibToString(LLVMOrcJITDylibRef JD);

// Cloning functionality
typedef enum {
  LLVMCloneFunctionChangeTypeLocalChangesOnly = 0,
  LLVMCloneFunctionChangeTypeGlobalChanges = 1,
  LLVMCloneFunctionChangeTypeDifferentModule = 2,
  LLVMCloneFunctionChangeTypeClonedModule = 3
} LLVMCloneFunctionChangeType;
void LLVMCloneFunctionInto(LLVMValueRef NewFunc, LLVMValueRef OldFunc,
                           LLVMValueRef *ValueMap, unsigned ValueMapElements,
                           LLVMCloneFunctionChangeType Changes, const char *NameSuffix,
                           LLVMTypeRef (*TypeMapper)(LLVMTypeRef, void *),
                           void *TypeMapperData,
                           LLVMValueRef (*Materializer)(LLVMValueRef, void *),
                           void *MaterializerData);
LLVMBasicBlockRef LLVMCloneBasicBlock(LLVMBasicBlockRef BB, const char *NameSuffix,
                                      LLVMValueRef *ValueMap, unsigned ValueMapElements,
                                      LLVMValueRef F);

// Operand bundles
#if LLVM_VERSION_MAJOR < 18 // llvm-project/llvm#73914
typedef struct LLVMOpaqueOperandBundle *LLVMOperandBundleRef;
LLVMOperandBundleRef LLVMCreateOperandBundle(const char *Tag, size_t TagLen,
                                             LLVMValueRef *Args,
                                             unsigned NumArgs);
void LLVMDisposeOperandBundle(LLVMOperandBundleRef Bundle);
const char *LLVMGetOperandBundleTag(LLVMOperandBundleRef Bundle, size_t *Len);
unsigned LLVMGetNumOperandBundleArgs(LLVMOperandBundleRef Bundle);
LLVMValueRef LLVMGetOperandBundleArgAtIndex(LLVMOperandBundleRef Bundle,
                                            unsigned Index);
unsigned LLVMGetNumOperandBundles(LLVMValueRef C);
LLVMOperandBundleRef LLVMGetOperandBundleAtIndex(LLVMValueRef C,
                                                 unsigned Index);
LLVMValueRef LLVMBuildInvokeWithOperandBundles(
    LLVMBuilderRef, LLVMTypeRef Ty, LLVMValueRef Fn, LLVMValueRef *Args,
    unsigned NumArgs, LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch,
    LLVMOperandBundleRef *Bundles, unsigned NumBundles, const char *Name);
LLVMValueRef
LLVMBuildCallWithOperandBundles(LLVMBuilderRef, LLVMTypeRef, LLVMValueRef Fn,
                                LLVMValueRef *Args, unsigned NumArgs,
                                LLVMOperandBundleRef *Bundles,
                                unsigned NumBundles, const char *Name);
#endif

// Metadata API extensions
LLVMValueRef LLVMMetadataAsValue2(LLVMContextRef C, LLVMMetadataRef Metadata);
void LLVMReplaceAllMetadataUsesWith(LLVMValueRef Old, LLVMValueRef New);
#if LLVM_VERSION_MAJOR < 17 // D136637
void LLVMReplaceMDNodeOperandWith(LLVMValueRef V, unsigned Index,
                                  LLVMMetadataRef Replacement);
#endif

// Constant data
LLVMValueRef LLVMConstDataArray(LLVMTypeRef ElementTy, const void *Data,
                                unsigned NumElements);

// Missing opaque pointer APIs
#if LLVM_VERSION_MAJOR < 17
LLVMBool LLVMContextSupportsTypedPointers(LLVMContextRef C);
#endif

// (Post)DominatorTree
typedef struct LLVMOpaqueDominatorTree *LLVMDominatorTreeRef;
LLVMDominatorTreeRef LLVMCreateDominatorTree(LLVMValueRef Fn);
void LLVMDisposeDominatorTree(LLVMDominatorTreeRef Tree);
LLVMBool LLVMDominatorTreeInstructionDominates(LLVMDominatorTreeRef Tree,
                                               LLVMValueRef InstA, LLVMValueRef InstB);
typedef struct LLVMOpaquePostDominatorTree *LLVMPostDominatorTreeRef;
LLVMPostDominatorTreeRef LLVMCreatePostDominatorTree(LLVMValueRef Fn);
void LLVMDisposePostDominatorTree(LLVMPostDominatorTreeRef Tree);
LLVMBool LLVMPostDominatorTreeInstructionDominates(LLVMPostDominatorTreeRef Tree,
                                                   LLVMValueRef InstA, LLVMValueRef InstB);

// fastmath
#if LLVM_VERSION_MAJOR < 18 // llvm/llvm-project#75123
enum {
  LLVMFastMathAllowReassoc = (1 << 0),
  LLVMFastMathNoNaNs = (1 << 1),
  LLVMFastMathNoInfs = (1 << 2),
  LLVMFastMathNoSignedZeros = (1 << 3),
  LLVMFastMathAllowReciprocal = (1 << 4),
  LLVMFastMathAllowContract = (1 << 5),
  LLVMFastMathApproxFunc = (1 << 6),
  LLVMFastMathNone = 0,
  LLVMFastMathAll = LLVMFastMathAllowReassoc | LLVMFastMathNoNaNs | LLVMFastMathNoInfs |
                    LLVMFastMathNoSignedZeros | LLVMFastMathAllowReciprocal |
                    LLVMFastMathAllowContract | LLVMFastMathApproxFunc,
};
typedef unsigned LLVMFastMathFlags;
LLVMFastMathFlags LLVMGetFastMathFlags(LLVMValueRef FPMathInst);
void LLVMSetFastMathFlags(LLVMValueRef FPMathInst, LLVMFastMathFlags FMF);
LLVMBool LLVMCanValueUseFastMathFlags(LLVMValueRef Inst);
#endif

// atomics with syncscope
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#104775
unsigned LLVMGetSyncScopeID(LLVMContextRef C, const char *Name, size_t SLen);
LLVMValueRef LLVMBuildFenceSyncScope(LLVMBuilderRef B, LLVMAtomicOrdering ordering,
                                     unsigned SSID, const char *Name);
LLVMValueRef LLVMBuildAtomicRMWSyncScope(LLVMBuilderRef B, LLVMAtomicRMWBinOp op,
                                         LLVMValueRef PTR, LLVMValueRef Val,
                                         LLVMAtomicOrdering ordering, unsigned SSID);
LLVMValueRef LLVMBuildAtomicCmpXchgSyncScope(LLVMBuilderRef B, LLVMValueRef Ptr,
                                             LLVMValueRef Cmp, LLVMValueRef New,
                                             LLVMAtomicOrdering SuccessOrdering,
                                             LLVMAtomicOrdering FailureOrdering,
                                             unsigned SSID);
LLVMBool LLVMIsAtomic(LLVMValueRef Inst);
unsigned LLVMGetAtomicSyncScopeID(LLVMValueRef AtomicInst);
void LLVMSetAtomicSyncScopeID(LLVMValueRef AtomicInst, unsigned SSID);
#endif

// more LLVMContextRef APIs
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#99087
LLVMContextRef LLVMGetValueContext(LLVMValueRef Val);
LLVMContextRef LLVMGetBuilderContext(LLVMBuilderRef Builder);
#endif

// NewPM extensions
typedef struct LLVMOpaquePassBuilderExtensions *LLVMPassBuilderExtensionsRef;
LLVMPassBuilderExtensionsRef LLVMCreatePassBuilderExtensions(void);
void LLVMDisposePassBuilderExtensions(LLVMPassBuilderExtensionsRef Extensions);
void LLVMPassBuilderExtensionsPushRegistrationCallbacks(LLVMPassBuilderExtensionsRef Options,
                                                      void (*RegistrationCallback)(void *));
typedef LLVMBool (*LLVMJuliaModulePassCallback)(LLVMModuleRef M, void *Thunk);
typedef LLVMBool (*LLVMJuliaFunctionPassCallback)(LLVMValueRef F, void *Thunk);
void LLVMPassBuilderExtensionsRegisterModulePass(LLVMPassBuilderExtensionsRef Options,
                                                 const char *PassName,
                                                 LLVMJuliaModulePassCallback Callback,
                                                 void *Thunk);
void LLVMPassBuilderExtensionsRegisterFunctionPass(LLVMPassBuilderExtensionsRef Options,
                                                   const char *PassName,
                                                   LLVMJuliaFunctionPassCallback Callback,
                                                   void *Thunk);
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#102482
void LLVMPassBuilderExtensionsSetAAPipeline(LLVMPassBuilderExtensionsRef Extensions,
                                            const char *AAPipeline);
#endif
LLVMErrorRef LLVMRunJuliaPasses(LLVMModuleRef M, const char *Passes,
                                LLVMTargetMachineRef TM, LLVMPassBuilderOptionsRef Options,
                                LLVMPassBuilderExtensionsRef Extensions);
LLVMErrorRef LLVMRunJuliaPassesOnFunction(LLVMValueRef F, const char *Passes,
                                          LLVMTargetMachineRef TM,
                                          LLVMPassBuilderOptionsRef Options,
                                          LLVMPassBuilderExtensionsRef Extensions);

// More DataLayout queries
unsigned LLVMGlobalsAddressSpace(LLVMTargetDataRef TD);

LLVM_C_EXTERN_C_END
#endif
