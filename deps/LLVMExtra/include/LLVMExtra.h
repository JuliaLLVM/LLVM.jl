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

// Missing functionality
void LLVMAppendToUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
void LLVMAppendToCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
size_t LLVMGetNumUsed(LLVMModuleRef Mod);
void LLVMGetUsed(LLVMModuleRef Mod, LLVMValueRef *Dest);
size_t LLVMGetNumCompilerUsed(LLVMModuleRef Mod);
void LLVMGetCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Dest);
void LLVMRemoveFromUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
void LLVMRemoveFromCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count);
void LLVMDumpMetadata(LLVMMetadataRef MD);
char *LLVMPrintMetadataToString(LLVMMetadataRef MD);
const char *LLVMDIScopeGetName(LLVMMetadataRef File, unsigned *Len);
void LLVMFunctionDeleteBody(LLVMValueRef Func);
void LLVMDestroyConstant(LLVMValueRef Const);
LLVMTypeRef LLVMGetFunctionType(LLVMValueRef Fn);
LLVMTypeRef LLVMGetGlobalValueType(LLVMValueRef Fn);

// Move a function or global variable to the position before `Before` (NULL for the end) in
// the given module's list, or insert it there if it isn't part of a module. Moving a value
// before itself is a no-op.
void LLVMExtraMoveFunction(LLVMValueRef Fn, LLVMModuleRef Mod, LLVMValueRef Before);
void LLVMExtraMoveGlobal(LLVMValueRef GlobalVar, LLVMModuleRef Mod, LLVMValueRef Before);

// Replace constant-expression/aggregate users of the given constants with
// equivalent instructions at each point of use; phi operands are materialized
// in their incoming block. Mirrors llvm::convertUsersOfConstantsToInstructions
// (llvm/IR/ReplaceConstant.h). Returns true if anything changed.
//
// The full four-parameter semantics are only available on LLVM 19+. On LLVM
// 17/18 only the single-argument overload exists, so RestrictToFunc,
// RemoveDeadConstants and IncludeSelf are fixed at nullptr/true/false
// respectively (the Julia wrapper rejects other values there). LLVM < 17 does
// not export the utility at all, so the function is unavailable.
#if LLVM_VERSION_MAJOR >= 17
LLVMBool LLVMConvertUsersOfConstantsToInstructions(LLVMValueRef *Consts,
                                                   size_t Count,
                                                   LLVMValueRef RestrictToFunc,
                                                   LLVMBool RemoveDeadConstants,
                                                   LLVMBool IncludeSelf);
#endif

// Attribute type detection (ConstantRange/ConstantRangeList kinds not exposed in C API)
#if LLVM_VERSION_MAJOR >= 19
LLVMBool LLVMIsConstantRangeAttribute(LLVMAttributeRef A);
#endif
#if LLVM_VERSION_MAJOR >= 20
LLVMBool LLVMIsConstantRangeListAttribute(LLVMAttributeRef A);
#endif

// Bug fixes
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#105521
void LLVMSetInitializer2(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal);
void LLVMSetPersonalityFn2(LLVMValueRef Fn, LLVMValueRef PersonalityFn);
#endif

// APIs without MetadataAsValue
const char *LLVMGetMDString2(LLVMMetadataRef MD, unsigned *Length);
unsigned LLVMGetMDNodeNumOperands2(LLVMMetadataRef MD);
void LLVMGetMDNodeOperands2(LLVMMetadataRef MD, LLVMMetadataRef *Dest);
LLVMMetadataRef LLVMGetMDNodeOperand2(LLVMMetadataRef MD, unsigned I);
unsigned LLVMGetNamedMetadataNumOperands2(LLVMNamedMDNodeRef NMD);
void LLVMGetNamedMetadataOperands2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef *Dest);
LLVMMetadataRef LLVMGetNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, unsigned I);
void LLVMAddNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef Val);
void LLVMClearNamedMetadataOperands(LLVMNamedMDNodeRef NMD);
void LLVMSetNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, unsigned I, LLVMMetadataRef Val);
void LLVMReplaceMDNodeOperandWith2(LLVMMetadataRef MD, unsigned I, LLVMMetadataRef New);

// ORC API extensions
typedef struct LLVMOrcOpaqueIRCompileLayer *LLVMOrcIRCompileLayerRef;
void LLVMOrcIRCompileLayerEmit(LLVMOrcIRCompileLayerRef IRLayer,
                               LLVMOrcMaterializationResponsibilityRef MR,
                               LLVMOrcThreadSafeModuleRef TSM);
char *LLVMDumpJitDylibToString(LLVMOrcJITDylibRef JD);

// Configuration of layers created by LLVMOrcCreateRTDyldObjectLinkingLayer*.
// The object layer must be an RTDyldObjectLinkingLayer.
void LLVMOrcRTDyldObjectLinkingLayerSetOverrideObjectFlagsWithResponsibilityFlags(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, LLVMBool OverrideObjectFlags);
void LLVMOrcRTDyldObjectLinkingLayerSetAutoClaimResponsibilityForObjectSymbols(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, LLVMBool AutoClaimObjectSymbols);
// Apply the settings LLJIT uses for its default object layer on the given target triple.
void LLVMOrcRTDyldObjectLinkingLayerApplyTargetDefaults(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, const char *Triple);

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
// replace the fast-math flags of an instruction (`LLVMSetFastMathFlags` only adds flags)
void LLVMExtraSetFastMathFlags(LLVMValueRef FPMathInst, LLVMFastMathFlags FMF);

// tail call kinds
#if LLVM_VERSION_MAJOR < 18 // D153107
typedef enum {
  LLVMTailCallKindNone = 0,
  LLVMTailCallKindTail = 1,
  LLVMTailCallKindMustTail = 2,
  LLVMTailCallKindNoTail = 3,
} LLVMTailCallKind;
LLVMTailCallKind LLVMGetTailCallKind(LLVMValueRef CallInst);
void LLVMSetTailCallKind(LLVMValueRef CallInst, LLVMTailCallKind kind);
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

// atomicrmw operations that LLVM supports before the C API does. These functions take and
// return the LLVMAtomicRMWBinOp values of LLVM 19 as integers, as they are out of range of
// the enum of the C API they are used with.
#if LLVM_VERSION_MAJOR >= 16 && LLVM_VERSION_MAJOR < 19
LLVMValueRef LLVMExtraBuildAtomicRMWSyncScope(LLVMBuilderRef B, unsigned op, LLVMValueRef PTR,
                                              LLVMValueRef Val, LLVMAtomicOrdering ordering,
                                              unsigned SSID);
unsigned LLVMExtraGetAtomicRMWBinOp(LLVMValueRef AtomicRMWInst);
#endif

// orderings of fences and atomicrmw instructions
#if LLVM_VERSION_MAJOR < 18 // llvm/llvm-project#65228
LLVMAtomicOrdering LLVMExtraGetOrdering(LLVMValueRef MemAccessInst);
void LLVMExtraSetOrdering(LLVMValueRef MemAccessInst, LLVMAtomicOrdering Ordering);
#endif

// more LLVMContextRef APIs
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#99087
LLVMContextRef LLVMGetValueContext(LLVMValueRef Val);
LLVMContextRef LLVMGetBuilderContext(LLVMBuilderRef Builder);
#endif

// expansion of atomics, using LLVM's own utilities or copies of AtomicExpandPass code. The
// operations take the LLVMAtomicRMWBinOp values of the most recent C API, as integers.
LLVMValueRef LLVMExtraBuildAtomicRMWValue(LLVMBuilderRef B, unsigned Op, LLVMValueRef Loaded,
                                          LLVMValueRef Val);
LLVMValueRef LLVMExtraBuildCmpXchgValue(LLVMBuilderRef B, LLVMValueRef PointerVal, LLVMValueRef Cmp,
                                        LLVMValueRef Val, unsigned Alignment,
                                        LLVMValueRef *Success);
LLVMBool LLVMExtraLowerAtomicRMWInst(LLVMValueRef RMWI);
LLVMBool LLVMExtraLowerAtomicCmpXchgInst(LLVMValueRef CXI);
LLVMBool LLVMExtraExpandAtomicRMWToCmpXchg(LLVMValueRef RMWI);
LLVMValueRef LLVMExtraCastAtomicToInteger(LLVMValueRef Inst);
typedef struct {
  LLVMTypeRef WordType;
  LLVMTypeRef ValueType;
  LLVMTypeRef IntValueType;
  LLVMValueRef AlignedAddr;
  unsigned AlignedAddrAlignment;
  LLVMValueRef ShiftAmt;
  LLVMValueRef Mask;
  LLVMValueRef InvMask;
} LLVMExtraPartwordMaskValues;
void LLVMExtraCreatePartwordMaskValues(LLVMBuilderRef B, LLVMTypeRef ValueType,
                                       LLVMValueRef Addr, unsigned AddrAlign,
                                       unsigned MinWordSize, LLVMExtraPartwordMaskValues *PMV);
LLVMValueRef LLVMExtraExtractMaskedValue(LLVMBuilderRef B, LLVMValueRef WideWord,
                                         const LLVMExtraPartwordMaskValues *PMV);
LLVMValueRef LLVMExtraInsertMaskedValue(LLVMBuilderRef B, LLVMValueRef WideWord,
                                        LLVMValueRef Updated,
                                        const LLVMExtraPartwordMaskValues *PMV);
LLVMBool LLVMExtraExpandPartwordAtomicRMW(LLVMValueRef RMWI, unsigned MinWordSize);
LLVMBool LLVMExtraExpandPartwordCmpXchg(LLVMValueRef CXI, unsigned MinWordSize);

// the name of a synchronization scope, or NULL if the context does not know it
const char *LLVMExtraGetSyncScopeName(LLVMContextRef C, unsigned SSID, size_t *Len);

// NewPM extensions
typedef struct LLVMOpaquePassBuilderExtensions *LLVMPassBuilderExtensionsRef;
LLVMPassBuilderExtensionsRef LLVMCreatePassBuilderExtensions(void);
void LLVMDisposePassBuilderExtensions(LLVMPassBuilderExtensionsRef Extensions);
void LLVMPassBuilderExtensionsPushRegistrationCallbacks(LLVMPassBuilderExtensionsRef Options,
                                                        void (*RegistrationCallback)(void *));
typedef LLVMBool (*LLVMJuliaModulePassCallback)(LLVMModuleRef M, void *Thunk);
typedef LLVMBool (*LLVMJuliaFunctionPassCallback)(LLVMValueRef F, void *Thunk);
// Callbacks must not allow exceptions implemented with setjmp/longjmp to
// escape. Doing so bypasses destructors in LLVM's C++ pass runner. Clients
// should catch exceptions in the callback and propagate failures out-of-band.
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

// poison-generating flags
#if LLVM_VERSION_MAJOR < 17 // D89252
LLVMBool LLVMGetNUW(LLVMValueRef ArithInst);
void LLVMSetNUW(LLVMValueRef ArithInst, LLVMBool HasNUW);
LLVMBool LLVMGetNSW(LLVMValueRef ArithInst);
void LLVMSetNSW(LLVMValueRef ArithInst, LLVMBool HasNSW);
LLVMBool LLVMGetExact(LLVMValueRef DivOrShrInst);
void LLVMSetExact(LLVMValueRef DivOrShrInst, LLVMBool IsExact);
#endif
#if LLVM_VERSION_MAJOR == 20 // llvm/llvm-project#145247
LLVMBool LLVMGetICmpSameSign(LLVMValueRef Inst);
void LLVMSetICmpSameSign(LLVMValueRef Inst, LLVMBool SameSign);
#endif

// switch case values (which stopped being operands in LLVM 22)
#if LLVM_VERSION_MAJOR < 22 // llvm/llvm-project#166842
LLVMValueRef LLVMGetSwitchCaseValue(LLVMValueRef Switch, unsigned i);
void LLVMSetSwitchCaseValue(LLVMValueRef Switch, unsigned i, LLVMValueRef CaseValue);
#endif

// floating-point constants from and to their bit pattern, as ceil(bits/64) words with
// the least significant word first
#if LLVM_VERSION_MAJOR < 22 // llvm/llvm-project#164381
LLVMValueRef LLVMConstFPFromBits(LLVMTypeRef Ty, const uint64_t N[]);
#endif
void LLVMExtraConstFPGetBits(LLVMValueRef ConstantVal, uint64_t N[]);
// the words of an integer constant, which can be wider than 64 bits
void LLVMExtraConstIntGetWords(LLVMValueRef ConstantVal, uint64_t N[]);

// debug records
#if LLVM_VERSION_MAJOR >= 19
#if LLVM_VERSION_MAJOR < 22 // llvm/llvm-project#151101
// LLVMGetFirstDbgRecord crashes on instructions that never had debug records attached
LLVMDbgRecordRef LLVMGetFirstDbgRecord2(LLVMValueRef Inst);
#endif
#if LLVM_VERSION_MAJOR < 20 // llvm/llvm-project#107802
LLVMDbgRecordRef LLVMGetNextDbgRecord(LLVMDbgRecordRef DbgRecord);
#endif
#if LLVM_VERSION_MAJOR < 22 // llvm/llvm-project#166383
typedef enum {
  LLVMDbgRecordLabel,
  LLVMDbgRecordDeclare,
  LLVMDbgRecordValue,
  LLVMDbgRecordAssign,
} LLVMDbgRecordKind;
LLVMMetadataRef LLVMDbgRecordGetDebugLoc(LLVMDbgRecordRef Rec);
LLVMDbgRecordKind LLVMDbgRecordGetKind(LLVMDbgRecordRef Rec);
LLVMValueRef LLVMDbgVariableRecordGetValue(LLVMDbgRecordRef Rec, unsigned OpIdx);
LLVMMetadataRef LLVMDbgVariableRecordGetVariable(LLVMDbgRecordRef Rec);
LLVMMetadataRef LLVMDbgVariableRecordGetExpression(LLVMDbgRecordRef Rec);
#endif
// the number of location operands of a variable record (more than one with DIArgList)
unsigned LLVMExtraDbgVariableRecordGetNumValues(LLVMDbgRecordRef Rec);
#endif

// the name of an attribute kind, as used in textual IR, or NULL for an invalid kind
const char *LLVMExtraGetAttributeKindName(unsigned KindID, size_t *Len);

// the category of an attribute kind, which determines the attributes it can be used for
LLVMBool LLVMExtraIsEnumAttributeKind(unsigned KindID);
LLVMBool LLVMExtraIsIntAttributeKind(unsigned KindID);
LLVMBool LLVMExtraIsTypeAttributeKind(unsigned KindID);
#if LLVM_VERSION_MAJOR >= 19
LLVMBool LLVMExtraIsConstantRangeAttributeKind(unsigned KindID);
#endif

// extractvalue and insertvalue with a path of indices
LLVMValueRef LLVMExtraBuildExtractValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                        const unsigned *Idxs, unsigned NumIdxs,
                                        const char *Name);
LLVMValueRef LLVMExtraBuildInsertValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                       LLVMValueRef EltVal, const unsigned *Idxs,
                                       unsigned NumIdxs, const char *Name);

// insertion points: a block, the instruction to insert before (NULL for the end of the
// block), and whether to insert before the debug records at that position (the head bit
// of the iterator, which is ignored before LLVM 19)
//
// move an instruction to an insertion point, or insert it there if it isn't part of a block
void LLVMExtraMoveInstruction(LLVMValueRef Inst, LLVMBasicBlockRef BB, LLVMValueRef Before,
                              LLVMBool Head);
// move a basic block before `Before` (NULL for the end) in the given function, which may
// differ from its current one, or insert it there if it isn't part of a function
void LLVMExtraMoveBasicBlock(LLVMBasicBlockRef BB, LLVMValueRef Fn, LLVMBasicBlockRef Before);
// delete a basic block, also if it isn't part of a function
void LLVMExtraDeleteBasicBlock(LLVMBasicBlockRef BB);
// set and get the insertion point of an instruction builder; the getter returns NULL if the
// builder isn't positioned
void LLVMExtraPositionBuilder(LLVMBuilderRef Builder, LLVMBasicBlockRef BB,
                              LLVMValueRef Before, LLVMBool Head);
LLVMBasicBlockRef LLVMExtraGetInsertPoint(LLVMBuilderRef Builder, LLVMValueRef *Before,
                                          LLVMBool *Head);
// the first insertion point of a block after its PHI nodes and EH pads; returns false if
// there is none (e.g., in a block that is terminated by a catchswitch)
LLVMBool LLVMExtraGetFirstInsertionPt(LLVMBasicBlockRef BB, LLVMValueRef *Before,
                                      LLVMBool *Head);
#if LLVM_VERSION_MAJOR >= 19
// insert a debug record at an insertion point, which must not be the end of a terminated
// block; requires the new debug info format
LLVMDbgRecordRef LLVMExtraDIBuilderInsertDeclareRecordAt(
    LLVMDIBuilderRef Builder, LLVMValueRef Storage, LLVMMetadataRef VarInfo,
    LLVMMetadataRef Expr, LLVMMetadataRef DL, LLVMBasicBlockRef BB, LLVMValueRef Before,
    LLVMBool Head);
LLVMDbgRecordRef LLVMExtraDIBuilderInsertDbgValueRecordAt(
    LLVMDIBuilderRef Builder, LLVMValueRef Val, LLVMMetadataRef VarInfo,
    LLVMMetadataRef Expr, LLVMMetadataRef DL, LLVMBasicBlockRef BB, LLVMValueRef Before,
    LLVMBool Head);
#if LLVM_VERSION_MAJOR >= 20
LLVMDbgRecordRef LLVMExtraDIBuilderInsertLabelAt(LLVMDIBuilderRef Builder,
                                                 LLVMMetadataRef LabelInfo,
                                                 LLVMMetadataRef DL, LLVMBasicBlockRef BB,
                                                 LLVMValueRef Before, LLVMBool Head);
#endif
#endif

// instructions
LLVMBool LLVMExtraInstructionComesBefore(LLVMValueRef Inst, LLVMValueRef Other);
LLVMBool LLVMExtraMayReadFromMemory(LLVMValueRef Inst);
LLVMBool LLVMExtraMayWriteToMemory(LLVMValueRef Inst);
LLVMBool LLVMExtraMayHaveSideEffects(LLVMValueRef Inst);

// values
void LLVMExtraTakeName(LLVMValueRef Val, LLVMValueRef From);
LLVMValueRef LLVMExtraStripPointerCasts(LLVMValueRef Val);
LLVMValueRef LLVMExtraStripPointerCastsAndAliases(LLVMValueRef Val);
unsigned LLVMExtraGetArgNo(LLVMValueRef Arg);

// functions and global variables
void LLVMExtraCopyAttributesFrom(LLVMValueRef Dst, LLVMValueRef Src);

// constants
void LLVMExtraRemoveDeadConstantUsers(LLVMValueRef C);

// verify a function, returning true and the verifier's message (to be disposed of using
// LLVMDisposeMessage) if it is broken
LLVMBool LLVMExtraVerifyFunction(LLVMValueRef Fn, char **OutMessage);

LLVM_C_EXTERN_C_END
#endif
