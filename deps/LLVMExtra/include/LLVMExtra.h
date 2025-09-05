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
#if LLVM_VERSION_MAJOR < 21
LLVMValueRef LLVMConstDataArray(LLVMTypeRef ElementTy, const void *Data,
                                size_t SizeInBytes);
#endif

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

// Custom TargetTransformInfo
//
// Pipelines that don't have a TargetMachine (e.g. out-of-tree backends invoked
// through a CLI) normally see the baseline `TargetTransformInfoImplBase`, which
// reports conservative defaults (e.g. no flat address space, no branch
// divergence). That disables TTI-sensitive passes like InferAddressSpaces and
// UniformityAnalysis.
//
// Create an options handle with `LLVMCreateTTIOptions`, populate it through
// the `LLVMTTIOptionsSet*` setters, and attach it with
// `LLVMPassBuilderExtensionsSetTTI`. Unset fields fall back to the
// `TargetTransformInfoImplBase` behavior. Each callback is paired with its
// own `UserData` pointer (LLVM C-API convention); the caller must keep any
// pointee alive for the duration of each `LLVMRunJuliaPasses` call.

// Callback signatures.

// Predicate over an (FromAS, ToAS) pair. Used by isNoopAddrSpaceCast,
// isValidAddrSpaceCast, addrspacesMayAlias.
typedef LLVMBool (*LLVMTTIASPairPredicateFn)(unsigned FromAS, unsigned ToAS,
                                             void *UserData);

// Predicate over a single AS. Used by
// canHaveNonUndefGlobalInitializerInAddressSpace.
typedef LLVMBool (*LLVMTTIASPredicateFn)(unsigned AS, void *UserData);

// Predicate over a single Value. Used by isSourceOfDivergence, isAlwaysUniform.
typedef LLVMBool (*LLVMTTIValuePredicateFn)(LLVMValueRef V, void *UserData);

// Returns the inferred AS for a Value, or ~0u if no inference is made.
typedef unsigned (*LLVMTTIGetAssumedAddressSpaceFn)(LLVMValueRef V,
                                                    void *UserData);

// Returns the inferred AS for a Value guarded by a predicate. Writes the
// predicate value to `*OutPredicate` (or leaves it null if none), and returns
// the AS (or ~0u if no inference is made).
typedef unsigned (*LLVMTTIGetPredicatedAddressSpaceFn)(
    LLVMValueRef V, LLVMValueRef *OutPredicate, void *UserData);

// Rewrites an intrinsic call `II` after its operand `OldV` has been replaced
// by `NewV` (in a different address space). Returns the rewritten Value, or
// null if no rewrite is needed.
typedef LLVMValueRef (*LLVMTTIRewriteIntrinsicFn)(LLVMValueRef II,
                                                  LLVMValueRef OldV,
                                                  LLVMValueRef NewV,
                                                  void *UserData);

// Reports which operand indices of an intrinsic call are flat-AS pointer
// operands. The callback writes up to `MaxCount` indices into `OutOps`, stores
// the number written in `*OutCount`, and returns true if any operands were
// reported. If the target needs to report more than `MaxCount` operands
// (currently 32), the excess is silently truncated.
typedef LLVMBool (*LLVMTTICollectFlatAddressOperandsFn)(
    unsigned IID, int *OutOps, unsigned MaxCount, unsigned *OutCount,
    void *UserData);

// Opaque options handle; allocate with LLVMCreateTTIOptions, free with
// LLVMDisposeTTIOptions.
typedef struct LLVMOpaqueTTIOptions *LLVMTTIOptionsRef;

LLVMTTIOptionsRef LLVMCreateTTIOptions(void);
void LLVMDisposeTTIOptions(LLVMTTIOptionsRef Options);

// Field setters. Unset fields fall back to TargetTransformInfoImplBase
// behavior. Each callback setter takes a `(Callback, UserData)` pair; the
// user data is threaded back to that callback on every invocation.
void LLVMTTIOptionsSetFlatAddressSpace(LLVMTTIOptionsRef Options, unsigned AS);
void LLVMTTIOptionsSetHasBranchDivergence(LLVMTTIOptionsRef Options,
                                          LLVMBool Value);
void LLVMTTIOptionsSetIsSingleThreaded(LLVMTTIOptionsRef Options,
                                       LLVMBool Value);
void LLVMTTIOptionsSetIsNoopAddrSpaceCast(LLVMTTIOptionsRef Options,
                                          LLVMTTIASPairPredicateFn Callback,
                                          void *UserData);
void LLVMTTIOptionsSetIsValidAddrSpaceCast(LLVMTTIOptionsRef Options,
                                           LLVMTTIASPairPredicateFn Callback,
                                           void *UserData);
void LLVMTTIOptionsSetAddrSpacesMayAlias(LLVMTTIOptionsRef Options,
                                         LLVMTTIASPairPredicateFn Callback,
                                         void *UserData);
void LLVMTTIOptionsSetCanHaveGlobalInitializerInAS(
    LLVMTTIOptionsRef Options, LLVMTTIASPredicateFn Callback, void *UserData);
void LLVMTTIOptionsSetIsSourceOfDivergence(LLVMTTIOptionsRef Options,
                                           LLVMTTIValuePredicateFn Callback,
                                           void *UserData);
void LLVMTTIOptionsSetIsAlwaysUniform(LLVMTTIOptionsRef Options,
                                      LLVMTTIValuePredicateFn Callback,
                                      void *UserData);
void LLVMTTIOptionsSetGetAssumedAddressSpace(
    LLVMTTIOptionsRef Options, LLVMTTIGetAssumedAddressSpaceFn Callback,
    void *UserData);
void LLVMTTIOptionsSetGetPredicatedAddressSpace(
    LLVMTTIOptionsRef Options, LLVMTTIGetPredicatedAddressSpaceFn Callback,
    void *UserData);
void LLVMTTIOptionsSetRewriteIntrinsicWithAS(LLVMTTIOptionsRef Options,
                                             LLVMTTIRewriteIntrinsicFn Callback,
                                             void *UserData);
void LLVMTTIOptionsSetCollectFlatAddressOperands(
    LLVMTTIOptionsRef Options, LLVMTTICollectFlatAddressOperandsFn Callback,
    void *UserData);

// Attach a custom TTI to the pass builder. Copies the options into internal
// state; the caller retains ownership of the handle. Pass `NULL` to revert
// to the default TTI.
void LLVMPassBuilderExtensionsSetTTI(LLVMPassBuilderExtensionsRef Extensions,
                                     LLVMTTIOptionsRef Options);

// Testing hooks for the pipeline EP pseudo-passes. `LLVMExtraInstallTestEPCallbacks`
// has the signature expected by `LLVMPassBuilderExtensionsPushRegistrationCallbacks`
// and registers callbacks at every extension point that simply increment a
// global counter. `LLVMExtraReadTestEPCounter` returns that counter and resets
// it to zero.
void LLVMExtraInstallTestEPCallbacks(void *PBRef);
unsigned LLVMExtraReadTestEPCounter(void);

// More DataLayout queries
unsigned LLVMGlobalsAddressSpace(LLVMTargetDataRef TD);

// Linker flags (mirrors `llvm::Linker::Flags`).
typedef enum {
  LLVMLinkerNone = 0,
  LLVMLinkerOverrideFromSrc = (1 << 0),
  LLVMLinkerLinkOnlyNeeded = (1 << 1),
} LLVMLinkerFlags;

// Extended variant of `LLVMLinkModules2` that accepts a bitmask of
// `LLVMLinkerFlags`. Destroys `Src` on success or failure, matching
// `LLVMLinkModules2`. Returns true on error.
LLVMBool LLVMLinkModules3(LLVMModuleRef Dest, LLVMModuleRef Src, unsigned Flags);

#if LLVM_VERSION_MAJOR >= 21
LLVMContextRef LLVMOrcThreadSafeContextGetContext(LLVMOrcThreadSafeContextRef TSCtx);
#endif

LLVM_C_EXTERN_C_END
#endif
