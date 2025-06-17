#include "LLVMExtra.h"

#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/CodeGen/Passes.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/SimpleLoopUnswitch.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#if LLVM_VERSION_MAJOR < 18
#include <llvm/Transforms/Vectorize.h>
#else
#include <llvm/Transforms/Vectorize/LoadStoreVectorizer.h>
#endif

using namespace llvm;
using namespace llvm::legacy;

//
// Initialization functions
//

// The LLVMInitialize* functions and friends are defined `static inline`

LLVMBool LLVMExtraInitializeNativeTarget() { return InitializeNativeTarget(); }

LLVMBool LLVMExtraInitializeNativeAsmParser() { return InitializeNativeTargetAsmParser(); }

LLVMBool LLVMExtraInitializeNativeAsmPrinter() {
  return InitializeNativeTargetAsmPrinter();
}

LLVMBool LLVMExtraInitializeNativeDisassembler() {
  return InitializeNativeTargetDisassembler();
}


//
// Missing LegacyPM passes
//

void LLVMAddBarrierNoopPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createBarrierNoopPass());
}

#if LLVM_VERSION_MAJOR < 17
void LLVMAddDivRemPairsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createDivRemPairsPass());
}

void LLVMAddLoopDistributePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopDistributePass());
}

void LLVMAddLoopFusePass(LLVMPassManagerRef PM) { unwrap(PM)->add(createLoopFusePass()); }

void LLVMAddLoopLoadEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopLoadEliminationPass());
}
#endif

void LLVMAddLoadStoreVectorizerPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoadStoreVectorizerPass());
}

#if LLVM_VERSION_MAJOR < 17
void LLVMAddVectorCombinePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createVectorCombinePass());
}
#endif

void LLVMAddSpeculativeExecutionIfHasBranchDivergencePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSpeculativeExecutionIfHasBranchDivergencePass());
}

#if LLVM_VERSION_MAJOR < 17
void LLVMAddSimpleLoopUnrollPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSimpleLoopUnrollPass());
}

void LLVMAddInductiveRangeCheckEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createInductiveRangeCheckEliminationPass());
}
#endif

#if LLVM_VERSION_MAJOR < 18
void LLVMAddSimpleLoopUnswitchLegacyPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSimpleLoopUnswitchLegacyPass());
}
#endif

void LLVMAddExpandReductionsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createExpandReductionsPass());
}
#if LLVM_VERSION_MAJOR >= 17
void LLVMAddCFGSimplificationPass2(LLVMPassManagerRef PM, int BonusInstThreshold,
                                   LLVMBool ForwardSwitchCondToPhi,
                                   LLVMBool ConvertSwitchToLookupTable,
                                   LLVMBool NeedCanonicalLoop, LLVMBool HoistCommonInsts,
                                   LLVMBool SinkCommonInsts, LLVMBool SimplifyCondBranch,
                                   LLVMBool SpeculateBlocks) {
  auto simplifyCFGOptions = SimplifyCFGOptions()
                                .bonusInstThreshold(BonusInstThreshold)
                                .forwardSwitchCondToPhi(ForwardSwitchCondToPhi)
                                .convertSwitchToLookupTable(ConvertSwitchToLookupTable)
                                .needCanonicalLoops(NeedCanonicalLoop)
                                .hoistCommonInsts(HoistCommonInsts)
                                .sinkCommonInsts(SinkCommonInsts)
                                .setSimplifyCondBranch(SimplifyCondBranch)
                                .speculateBlocks(SpeculateBlocks);
  unwrap(PM)->add(createCFGSimplificationPass(simplifyCFGOptions));
}
#else
void LLVMAddCFGSimplificationPass2(LLVMPassManagerRef PM, int BonusInstThreshold,
                                   LLVMBool ForwardSwitchCondToPhi,
                                   LLVMBool ConvertSwitchToLookupTable,
                                   LLVMBool NeedCanonicalLoop, LLVMBool HoistCommonInsts,
                                   LLVMBool SinkCommonInsts, LLVMBool SimplifyCondBranch,
                                   LLVMBool FoldTwoEntryPHINode) {
  auto simplifyCFGOptions = SimplifyCFGOptions()
                                .bonusInstThreshold(BonusInstThreshold)
                                .forwardSwitchCondToPhi(ForwardSwitchCondToPhi)
                                .convertSwitchToLookupTable(ConvertSwitchToLookupTable)
                                .needCanonicalLoops(NeedCanonicalLoop)
                                .hoistCommonInsts(HoistCommonInsts)
                                .sinkCommonInsts(SinkCommonInsts)
                                .setSimplifyCondBranch(SimplifyCondBranch)
                                .setFoldTwoEntryPHINode(FoldTwoEntryPHINode);
  unwrap(PM)->add(createCFGSimplificationPass(simplifyCFGOptions));
}
#endif

#if LLVM_VERSION_MAJOR < 17
void LLVMAddInternalizePassWithExportList(LLVMPassManagerRef PM, const char **ExportList,
                                          size_t Length) {
  auto PreserveFobj = [=](const GlobalValue &GV) {
    for (size_t i = 0; i < Length; i++) {
      if (strcmp(ExportList[i], GV.getName().data()) == 0)
        return true;
    }
    return false;
  };
  unwrap(PM)->add(createInternalizePass(PreserveFobj));
}
#endif


//
// Custom LegacyPM pass infrastructure
//

typedef struct LLVMOpaquePass *LLVMPassRef;
DEFINE_STDCXX_CONVERSION_FUNCTIONS(Pass, LLVMPassRef)

void LLVMAddPass(LLVMPassManagerRef PM, LLVMPassRef P) { unwrap(PM)->add(unwrap(P)); }

typedef LLVMBool (*LLVMPassCallback)(void *Ref, void *Data);

namespace {
StringMap<char *> PassIDs;
char &CreatePassID(const char *Name) {
  std::string NameStr(Name);
  if (PassIDs.find(NameStr) != PassIDs.end())
    return *PassIDs[NameStr];
  else
    return *(PassIDs[NameStr] = new char);
}

class JuliaModulePass : public ModulePass {
public:
  JuliaModulePass(const char *Name, LLVMPassCallback Callback, void *Data)
      : ModulePass(CreatePassID(Name)), Callback(Callback), Data(Data) {}

  bool runOnModule(Module &M) override {
    void *Ref = (void *)wrap(&M);
    bool Changed = Callback(Ref, Data);
    return Changed;
  }

private:
  LLVMPassCallback Callback;
  void *Data;
};

class JuliaFunctionPass : public FunctionPass {
public:
  JuliaFunctionPass(const char *Name, LLVMPassCallback Callback, void *Data)
      : FunctionPass(CreatePassID(Name)), Callback(Callback), Data(Data) {}

  bool runOnFunction(Function &Fn) override {
    void *Ref = (void *)wrap(&Fn);
    bool Changed = Callback(Ref, Data);
    return Changed;
  }

private:
  LLVMPassCallback Callback;
  void *Data;
};

}; // namespace

LLVMPassRef LLVMCreateModulePass2(const char *Name, LLVMPassCallback Callback, void *Data) {
  return wrap(new JuliaModulePass(Name, Callback, Data));
}

LLVMPassRef LLVMCreateFunctionPass2(const char *Name, LLVMPassCallback Callback,
                                    void *Data) {
  return wrap(new JuliaFunctionPass(Name, Callback, Data));
}


//
// Missing functionality
//

void LLVMAddTargetLibraryInfoByTriple(const char *T, LLVMPassManagerRef PM) {
  unwrap(PM)->add(new TargetLibraryInfoWrapperPass(Triple(T)));
}

void LLVMAppendToUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count) {
  SmallVector<GlobalValue *, 1> GlobalValues;
  for (auto *Value : ArrayRef(Values, Count))
    GlobalValues.push_back(cast<GlobalValue>(unwrap(Value)));
  appendToUsed(*unwrap(Mod), GlobalValues);
}

void LLVMAppendToCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count) {
  SmallVector<GlobalValue *, 1> GlobalValues;
  for (auto *Value : ArrayRef(Values, Count))
    GlobalValues.push_back(cast<GlobalValue>(unwrap(Value)));
  appendToCompilerUsed(*unwrap(Mod), GlobalValues);
}

void LLVMAddGenericAnalysisPasses(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createTargetTransformInfoWrapperPass(TargetIRAnalysis()));
}

const char *LLVMDIScopeGetName(LLVMMetadataRef File, unsigned *Len) {
  auto Name = unwrap<DIScope>(File)->getName();
  *Len = Name.size();
  return Name.data();
}

void LLVMDumpMetadata(LLVMMetadataRef MD) {
  unwrap<Metadata>(MD)->print(errs(), /*M=*/nullptr, /*IsForDebug=*/true);
}

char *LLVMPrintMetadataToString(LLVMMetadataRef MD) {
  std::string buf;
  raw_string_ostream os(buf);

  if (unwrap<Metadata>(MD))
    unwrap<Metadata>(MD)->print(os);
  else
    os << "Printing <null> Metadata";

  os.flush();

  return strdup(buf.c_str());
}

void LLVMFunctionDeleteBody(LLVMValueRef Func) { unwrap<Function>(Func)->deleteBody(); }

void LLVMDestroyConstant(LLVMValueRef Const) { unwrap<Constant>(Const)->destroyConstant(); }

LLVMTypeRef LLVMGetFunctionType(LLVMValueRef Fn) {
  auto Ftype = unwrap<Function>(Fn)->getFunctionType();
  return wrap(Ftype);
}

LLVMTypeRef LLVMGetGlobalValueType(LLVMValueRef GV) {
  auto Ftype = unwrap<GlobalValue>(GV)->getValueType();
  return wrap(Ftype);
}


//
// Bug fixes
//


#if LLVM_VERSION_MAJOR < 20

void LLVMSetInitializer2(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal) {
  unwrap<GlobalVariable>(GlobalVar)->setInitializer(
      ConstantVal ? unwrap<Constant>(ConstantVal) : nullptr);
}

void LLVMSetPersonalityFn2(LLVMValueRef Fn, LLVMValueRef PersonalityFn) {
  unwrap<Function>(Fn)->setPersonalityFn(PersonalityFn ? unwrap<Constant>(PersonalityFn)
                                                       : nullptr);
}

#endif


//
// APIs without MetadataAsValue
//

const char *LLVMGetMDString2(LLVMMetadataRef MD, unsigned *Length) {
  const MDString *S = unwrap<MDString>(MD);
  *Length = S->getString().size();
  return S->getString().data();
}

unsigned LLVMGetMDNodeNumOperands2(LLVMMetadataRef MD) {
  return unwrap<MDNode>(MD)->getNumOperands();
}

void LLVMGetMDNodeOperands2(LLVMMetadataRef MD, LLVMMetadataRef *Dest) {
  const auto *N = unwrap<MDNode>(MD);
  const unsigned numOperands = N->getNumOperands();
  for (unsigned i = 0; i < numOperands; i++)
    Dest[i] = wrap(N->getOperand(i));
}

unsigned LLVMGetNamedMetadataNumOperands2(LLVMNamedMDNodeRef NMD) {
  return unwrap<NamedMDNode>(NMD)->getNumOperands();
}

void LLVMGetNamedMetadataOperands2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef *Dest) {
  NamedMDNode *N = unwrap<NamedMDNode>(NMD);
  for (unsigned i = 0; i < N->getNumOperands(); i++)
    Dest[i] = wrap(N->getOperand(i));
}

void LLVMAddNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef Val) {
  unwrap<NamedMDNode>(NMD)->addOperand(unwrap<MDNode>(Val));
}

void LLVMReplaceMDNodeOperandWith2(LLVMMetadataRef MD, unsigned I, LLVMMetadataRef New) {
  unwrap<MDNode>(MD)->replaceOperandWith(I, unwrap(New));
}


//
// ORC API extensions
//

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::MaterializationResponsibility,
                                   LLVMOrcMaterializationResponsibilityRef)

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeModule, LLVMOrcThreadSafeModuleRef)

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::IRCompileLayer, LLVMOrcIRCompileLayerRef)

void LLVMOrcIRCompileLayerEmit(LLVMOrcIRCompileLayerRef IRLayer,
                               LLVMOrcMaterializationResponsibilityRef MR,
                               LLVMOrcThreadSafeModuleRef TSM) {
  std::unique_ptr<orc::ThreadSafeModule> TmpTSM(unwrap(TSM));
  unwrap(IRLayer)->emit(std::unique_ptr<orc::MaterializationResponsibility>(unwrap(MR)),
                        std::move(*TmpTSM));
}

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::JITDylib, LLVMOrcJITDylibRef)

char *LLVMDumpJitDylibToString(LLVMOrcJITDylibRef JD) {
  std::string str;
  llvm::raw_string_ostream rso(str);
  auto jd = unwrap(JD);
  jd->dump(rso);
  rso.flush();
  return strdup(str.c_str());
}


//
// Cloning functionality
//

class ExternalTypeRemapper : public ValueMapTypeRemapper {
public:
  ExternalTypeRemapper(LLVMTypeRef (*fptr)(LLVMTypeRef, void *), void *data)
      : fptr(fptr), data(data) {}

private:
  Type *remapType(Type *SrcTy) override { return unwrap(fptr(wrap(SrcTy), data)); }

  LLVMTypeRef (*fptr)(LLVMTypeRef, void *);
  void *data;
};

class ExternalValueMaterializer : public ValueMaterializer {
public:
  ExternalValueMaterializer(LLVMValueRef (*fptr)(LLVMValueRef, void *), void *data)
      : fptr(fptr), data(data) {}
  virtual ~ExternalValueMaterializer() = default;
  Value *materialize(Value *V) override { return unwrap(fptr(wrap(V), data)); }

private:
  LLVMValueRef (*fptr)(LLVMValueRef, void *);
  void *data;
};

void LLVMCloneFunctionInto(LLVMValueRef NewFunc, LLVMValueRef OldFunc,
                           LLVMValueRef *ValueMap, unsigned ValueMapElements,
                           LLVMCloneFunctionChangeType Changes, const char *NameSuffix,
                           LLVMTypeRef (*TypeMapper)(LLVMTypeRef, void *),
                           void *TypeMapperData,
                           LLVMValueRef (*Materializer)(LLVMValueRef, void *),
                           void *MaterializerData) {
  // NOTE: we ignore returns cloned, and don't return the code info
  SmallVector<ReturnInst *, 8> Returns;

  CloneFunctionChangeType CFGT;
  switch (Changes) {
  case LLVMCloneFunctionChangeTypeLocalChangesOnly:
    CFGT = CloneFunctionChangeType::LocalChangesOnly;
    break;
  case LLVMCloneFunctionChangeTypeGlobalChanges:
    CFGT = CloneFunctionChangeType::GlobalChanges;
    break;
  case LLVMCloneFunctionChangeTypeDifferentModule:
    CFGT = CloneFunctionChangeType::DifferentModule;
    break;
  case LLVMCloneFunctionChangeTypeClonedModule:
    CFGT = CloneFunctionChangeType::ClonedModule;
    break;
  }

  ValueToValueMapTy VMap;
  for (unsigned i = 0; i < ValueMapElements; ++i)
    VMap[unwrap(ValueMap[2 * i])] = unwrap(ValueMap[2 * i + 1]);
  ExternalTypeRemapper TheTypeRemapper(TypeMapper, TypeMapperData);
  ExternalValueMaterializer TheMaterializer(Materializer, MaterializerData);
  CloneFunctionInto(unwrap<Function>(NewFunc), unwrap<Function>(OldFunc), VMap, CFGT,
                    Returns, NameSuffix, nullptr, TypeMapper ? &TheTypeRemapper : nullptr,
                    Materializer ? &TheMaterializer : nullptr);
}

LLVMBasicBlockRef LLVMCloneBasicBlock(LLVMBasicBlockRef BB, const char *NameSuffix,
                                      LLVMValueRef *ValueMap, unsigned ValueMapElements,
                                      LLVMValueRef F) {
  ValueToValueMapTy VMap;
  BasicBlock *NewBB =
      CloneBasicBlock(unwrap(BB), VMap, NameSuffix, F ? unwrap<Function>(F) : nullptr);
  for (unsigned i = 0; i < ValueMapElements; ++i)
    VMap[unwrap(ValueMap[2 * i])] = unwrap(ValueMap[2 * i + 1]);
  SmallVector<BasicBlock *, 1> Blocks = {NewBB};
  remapInstructionsInBlocks(Blocks, VMap);
  return wrap(NewBB);
}


//
// Operand bundles
//

#if LLVM_VERSION_MAJOR < 18

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(OperandBundleDef, LLVMOperandBundleRef)

LLVMOperandBundleRef LLVMCreateOperandBundle(const char *Tag, size_t TagLen,
                                             LLVMValueRef *Args,
                                             unsigned NumArgs) {
  return wrap(new OperandBundleDef(std::string(Tag, TagLen),
                                   ArrayRef(unwrap(Args), NumArgs)));
}

void LLVMDisposeOperandBundle(LLVMOperandBundleRef Bundle) {
  delete unwrap(Bundle);
}

const char *LLVMGetOperandBundleTag(LLVMOperandBundleRef Bundle, size_t *Len) {
  StringRef Str = unwrap(Bundle)->getTag();
  *Len = Str.size();
  return Str.data();
}

unsigned LLVMGetNumOperandBundleArgs(LLVMOperandBundleRef Bundle) {
  return unwrap(Bundle)->inputs().size();
}

LLVMValueRef LLVMGetOperandBundleArgAtIndex(LLVMOperandBundleRef Bundle,
                                            unsigned Index) {
  return wrap(unwrap(Bundle)->inputs()[Index]);
}

unsigned LLVMGetNumOperandBundles(LLVMValueRef C) {
  return unwrap<CallBase>(C)->getNumOperandBundles();
}

LLVMOperandBundleRef LLVMGetOperandBundleAtIndex(LLVMValueRef C,
                                                 unsigned Index) {
  return wrap(
      new OperandBundleDef(unwrap<CallBase>(C)->getOperandBundleAt(Index)));
}

LLVMValueRef LLVMBuildInvokeWithOperandBundles(
    LLVMBuilderRef B, LLVMTypeRef Ty, LLVMValueRef Fn, LLVMValueRef *Args,
    unsigned NumArgs, LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch,
    LLVMOperandBundleRef *Bundles, unsigned NumBundles, const char *Name) {
  SmallVector<OperandBundleDef, 8> OBs;
  for (auto *Bundle : ArrayRef(Bundles, NumBundles)) {
    OperandBundleDef *OB = unwrap(Bundle);
    OBs.push_back(*OB);
  }
  return wrap(unwrap(B)->CreateInvoke(
      unwrap<FunctionType>(Ty), unwrap(Fn), unwrap(Then), unwrap(Catch),
      ArrayRef(unwrap(Args), NumArgs), OBs, Name));
}

LLVMValueRef
LLVMBuildCallWithOperandBundles(LLVMBuilderRef B, LLVMTypeRef Ty,
                                LLVMValueRef Fn, LLVMValueRef *Args,
                                unsigned NumArgs, LLVMOperandBundleRef *Bundles,
                                unsigned NumBundles, const char *Name) {
  FunctionType *FTy = unwrap<FunctionType>(Ty);
  SmallVector<OperandBundleDef, 8> OBs;
  for (auto *Bundle : ArrayRef(Bundles, NumBundles)) {
    OperandBundleDef *OB = unwrap(Bundle);
    OBs.push_back(*OB);
  }
  return wrap(unwrap(B)->CreateCall(
      FTy, unwrap(Fn), ArrayRef(unwrap(Args), NumArgs), OBs, Name));
}

#endif


//
// Metadata API extensions
//

LLVMValueRef LLVMMetadataAsValue2(LLVMContextRef C, LLVMMetadataRef Metadata) {
  auto *MD = unwrap(Metadata);
  if (auto *VAM = dyn_cast<ValueAsMetadata>(MD))
    return wrap(VAM->getValue());
  else
    return wrap(MetadataAsValue::get(*unwrap(C), MD));
}

void LLVMReplaceAllMetadataUsesWith(LLVMValueRef Old, LLVMValueRef New) {
  ValueAsMetadata::handleRAUW(unwrap<Value>(Old), unwrap<Value>(New));
}

#if LLVM_VERSION_MAJOR < 17
void LLVMReplaceMDNodeOperandWith(LLVMValueRef V, unsigned Index,
                                  LLVMMetadataRef Replacement) {
  auto *MD = cast<MetadataAsValue>(unwrap(V));
  auto *N = cast<MDNode>(MD->getMetadata());
  N->replaceOperandWith(Index, unwrap<Metadata>(Replacement));
}
#endif


//
// Constant data
//

LLVMValueRef LLVMConstDataArray(LLVMTypeRef ElementTy, const void *Data,
                                unsigned NumElements) {
  StringRef S((const char *)Data,
              NumElements * unwrap(ElementTy)->getPrimitiveSizeInBits() / 8);
  return wrap(ConstantDataArray::getRaw(S, NumElements, unwrap(ElementTy)));
}


//
// Missing opaque pointer APIs
//

#if LLVM_VERSION_MAJOR < 17
LLVMBool LLVMContextSupportsTypedPointers(LLVMContextRef C) {
  return unwrap(C)->supportsTypedPointers();
}
#endif


//
// DominatorTree and PostDominatorTree
//

DEFINE_STDCXX_CONVERSION_FUNCTIONS(DominatorTree, LLVMDominatorTreeRef)

LLVMDominatorTreeRef LLVMCreateDominatorTree(LLVMValueRef Fn) {
  return wrap(new DominatorTree(*unwrap<Function>(Fn)));
}

void LLVMDisposeDominatorTree(LLVMDominatorTreeRef Tree) { delete unwrap(Tree); }

LLVMBool LLVMDominatorTreeInstructionDominates(LLVMDominatorTreeRef Tree,
                                               LLVMValueRef InstA, LLVMValueRef InstB) {
  return unwrap(Tree)->dominates(unwrap<Instruction>(InstA), unwrap<Instruction>(InstB));
}

DEFINE_STDCXX_CONVERSION_FUNCTIONS(PostDominatorTree, LLVMPostDominatorTreeRef)

LLVMPostDominatorTreeRef LLVMCreatePostDominatorTree(LLVMValueRef Fn) {
  return wrap(new PostDominatorTree(*unwrap<Function>(Fn)));
}

void LLVMDisposePostDominatorTree(LLVMPostDominatorTreeRef Tree) { delete unwrap(Tree); }

LLVMBool LLVMPostDominatorTreeInstructionDominates(LLVMPostDominatorTreeRef Tree,
                                                   LLVMValueRef InstA, LLVMValueRef InstB) {
  return unwrap(Tree)->dominates(unwrap<Instruction>(InstA), unwrap<Instruction>(InstB));
}


//
// fastmath
//

#if LLVM_VERSION_MAJOR < 18

static FastMathFlags mapFromLLVMFastMathFlags(LLVMFastMathFlags FMF) {
  FastMathFlags NewFMF;
  NewFMF.setAllowReassoc((FMF & LLVMFastMathAllowReassoc) != 0);
  NewFMF.setNoNaNs((FMF & LLVMFastMathNoNaNs) != 0);
  NewFMF.setNoInfs((FMF & LLVMFastMathNoInfs) != 0);
  NewFMF.setNoSignedZeros((FMF & LLVMFastMathNoSignedZeros) != 0);
  NewFMF.setAllowReciprocal((FMF & LLVMFastMathAllowReciprocal) != 0);
  NewFMF.setAllowContract((FMF & LLVMFastMathAllowContract) != 0);
  NewFMF.setApproxFunc((FMF & LLVMFastMathApproxFunc) != 0);

  return NewFMF;
}

static LLVMFastMathFlags mapToLLVMFastMathFlags(FastMathFlags FMF) {
  LLVMFastMathFlags NewFMF = LLVMFastMathNone;
  if (FMF.allowReassoc())
    NewFMF |= LLVMFastMathAllowReassoc;
  if (FMF.noNaNs())
    NewFMF |= LLVMFastMathNoNaNs;
  if (FMF.noInfs())
    NewFMF |= LLVMFastMathNoInfs;
  if (FMF.noSignedZeros())
    NewFMF |= LLVMFastMathNoSignedZeros;
  if (FMF.allowReciprocal())
    NewFMF |= LLVMFastMathAllowReciprocal;
  if (FMF.allowContract())
    NewFMF |= LLVMFastMathAllowContract;
  if (FMF.approxFunc())
    NewFMF |= LLVMFastMathApproxFunc;

  return NewFMF;
}

LLVMFastMathFlags LLVMGetFastMathFlags(LLVMValueRef FPMathInst) {
  Value *P = unwrap<Value>(FPMathInst);
  FastMathFlags FMF = cast<Instruction>(P)->getFastMathFlags();
  return mapToLLVMFastMathFlags(FMF);
}

void LLVMSetFastMathFlags(LLVMValueRef FPMathInst, LLVMFastMathFlags FMF) {
  Value *P = unwrap<Value>(FPMathInst);
  cast<Instruction>(P)->setFastMathFlags(mapFromLLVMFastMathFlags(FMF));
}

LLVMBool LLVMCanValueUseFastMathFlags(LLVMValueRef V) {
  Value *Val = unwrap<Value>(V);
  return isa<FPMathOperator>(Val);
}

#endif


// atomics with syncscope

#if LLVM_VERSION_MAJOR < 20

static AtomicOrdering mapFromLLVMOrdering(LLVMAtomicOrdering Ordering) {
  switch (Ordering) {
    case LLVMAtomicOrderingNotAtomic: return AtomicOrdering::NotAtomic;
    case LLVMAtomicOrderingUnordered: return AtomicOrdering::Unordered;
    case LLVMAtomicOrderingMonotonic: return AtomicOrdering::Monotonic;
    case LLVMAtomicOrderingAcquire: return AtomicOrdering::Acquire;
    case LLVMAtomicOrderingRelease: return AtomicOrdering::Release;
    case LLVMAtomicOrderingAcquireRelease:
      return AtomicOrdering::AcquireRelease;
    case LLVMAtomicOrderingSequentiallyConsistent:
      return AtomicOrdering::SequentiallyConsistent;
  }

  llvm_unreachable("Invalid LLVMAtomicOrdering value!");
}

static AtomicRMWInst::BinOp mapFromLLVMRMWBinOp(LLVMAtomicRMWBinOp BinOp) {
  switch (BinOp) {
    case LLVMAtomicRMWBinOpXchg: return AtomicRMWInst::Xchg;
    case LLVMAtomicRMWBinOpAdd: return AtomicRMWInst::Add;
    case LLVMAtomicRMWBinOpSub: return AtomicRMWInst::Sub;
    case LLVMAtomicRMWBinOpAnd: return AtomicRMWInst::And;
    case LLVMAtomicRMWBinOpNand: return AtomicRMWInst::Nand;
    case LLVMAtomicRMWBinOpOr: return AtomicRMWInst::Or;
    case LLVMAtomicRMWBinOpXor: return AtomicRMWInst::Xor;
    case LLVMAtomicRMWBinOpMax: return AtomicRMWInst::Max;
    case LLVMAtomicRMWBinOpMin: return AtomicRMWInst::Min;
    case LLVMAtomicRMWBinOpUMax: return AtomicRMWInst::UMax;
    case LLVMAtomicRMWBinOpUMin: return AtomicRMWInst::UMin;
    case LLVMAtomicRMWBinOpFAdd: return AtomicRMWInst::FAdd;
    case LLVMAtomicRMWBinOpFSub: return AtomicRMWInst::FSub;
    case LLVMAtomicRMWBinOpFMax: return AtomicRMWInst::FMax;
    case LLVMAtomicRMWBinOpFMin: return AtomicRMWInst::FMin;
#if LLVM_VERSION_MAJOR >= 19
    case LLVMAtomicRMWBinOpUIncWrap: return AtomicRMWInst::UIncWrap;
    case LLVMAtomicRMWBinOpUDecWrap: return AtomicRMWInst::UDecWrap;
#endif
  }

  llvm_unreachable("Invalid LLVMAtomicRMWBinOp value!");
}

inline void setAtomicSyncScopeID(Instruction *I, SyncScope::ID SSID) {
  assert(I->isAtomic());
  if (auto *AI = dyn_cast<LoadInst>(I))
    AI->setSyncScopeID(SSID);
  else if (auto *AI = dyn_cast<StoreInst>(I))
    AI->setSyncScopeID(SSID);
  else if (auto *AI = dyn_cast<FenceInst>(I))
    AI->setSyncScopeID(SSID);
  else if (auto *AI = dyn_cast<AtomicCmpXchgInst>(I))
    AI->setSyncScopeID(SSID);
  else if (auto *AI = dyn_cast<AtomicRMWInst>(I))
    AI->setSyncScopeID(SSID);
  else
    llvm_unreachable("unhandled atomic operation");
}

unsigned LLVMGetSyncScopeID(LLVMContextRef C, const char *Name, size_t SLen) {
  return unwrap(C)->getOrInsertSyncScopeID(StringRef(Name, SLen));
}

LLVMValueRef LLVMBuildFenceSyncScope(LLVMBuilderRef B, LLVMAtomicOrdering Ordering,
                                     unsigned SSID, const char *Name) {
  return wrap(unwrap(B)->CreateFence(mapFromLLVMOrdering(Ordering), SSID, Name));
}

LLVMValueRef LLVMBuildAtomicRMWSyncScope(LLVMBuilderRef B, LLVMAtomicRMWBinOp op,
                                         LLVMValueRef PTR, LLVMValueRef Val,
                                         LLVMAtomicOrdering ordering, unsigned SSID) {
  AtomicRMWInst::BinOp intop = mapFromLLVMRMWBinOp(op);
  return wrap(unwrap(B)->CreateAtomicRMW(intop, unwrap(PTR), unwrap(Val), MaybeAlign(),
                                         mapFromLLVMOrdering(ordering), SSID));
}

LLVMValueRef LLVMBuildAtomicCmpXchgSyncScope(LLVMBuilderRef B, LLVMValueRef Ptr,
                                             LLVMValueRef Cmp, LLVMValueRef New,
                                             LLVMAtomicOrdering SuccessOrdering,
                                             LLVMAtomicOrdering FailureOrdering,
                                             unsigned SSID) {
  return wrap(unwrap(B)->CreateAtomicCmpXchg(
      unwrap(Ptr), unwrap(Cmp), unwrap(New), MaybeAlign(),
      mapFromLLVMOrdering(SuccessOrdering), mapFromLLVMOrdering(FailureOrdering), SSID));
}

LLVMBool LLVMIsAtomic(LLVMValueRef Inst) { return unwrap<Instruction>(Inst)->isAtomic(); }

unsigned LLVMGetAtomicSyncScopeID(LLVMValueRef AtomicInst) {
  Instruction *I = unwrap<Instruction>(AtomicInst);
  assert(I->isAtomic() && "Expected an atomic instruction");
  return *getAtomicSyncScopeID(I);
}

void LLVMSetAtomicSyncScopeID(LLVMValueRef AtomicInst, unsigned SSID) {
  Instruction *I = unwrap<Instruction>(AtomicInst);
  assert(I->isAtomic() && "Expected an atomic instruction");
  setAtomicSyncScopeID(I, SSID);
}

#endif


//
// more LLVMContextRef getters
//

#if LLVM_VERSION_MAJOR < 20

LLVMContextRef LLVMGetValueContext(LLVMValueRef Val) {
  return wrap(&unwrap(Val)->getContext());
}

LLVMContextRef LLVMGetBuilderContext(LLVMBuilderRef Builder) {
  return wrap(&unwrap(Builder)->getContext());
}

#endif


//
// More DataLayout queries
//

unsigned LLVMGlobalsAddressSpace(LLVMTargetDataRef TD) {
  return unwrap(TD)->getDefaultGlobalsAddressSpace();
}
