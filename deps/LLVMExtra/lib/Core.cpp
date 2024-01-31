#include <LLVMExtra.h>

#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
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
#include <llvm/Transforms/Vectorize.h>

using namespace llvm;
using namespace llvm::legacy;

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

// Various missing passes (being upstreamed)

void LLVMAddBarrierNoopPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createBarrierNoopPass());
}

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

void LLVMAddLoadStoreVectorizerPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoadStoreVectorizerPass());
}

void LLVMAddVectorCombinePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createVectorCombinePass());
}

void LLVMAddSpeculativeExecutionIfHasBranchDivergencePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSpeculativeExecutionIfHasBranchDivergencePass());
}

void LLVMAddSimpleLoopUnrollPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSimpleLoopUnrollPass());
}

void LLVMAddInductiveRangeCheckEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createInductiveRangeCheckEliminationPass());
}

void LLVMAddSimpleLoopUnswitchLegacyPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSimpleLoopUnswitchLegacyPass());
}

// Infrastructure for writing LLVM passes in Julia

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

// Various missing functions

unsigned int LLVMGetDebugMDVersion() { return DEBUG_METADATA_VERSION; }

LLVMContextRef LLVMGetValueContext(LLVMValueRef V) {
  return wrap(&unwrap(V)->getContext());
}

LLVMContextRef LLVMGetBuilderContext(LLVMBuilderRef B) {
  return wrap(&unwrap(B)->getContext());
}

void LLVMAddTargetLibraryInfoByTriple(const char *T, LLVMPassManagerRef PM) {
  unwrap(PM)->add(new TargetLibraryInfoWrapperPass(Triple(T)));
}

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

// versions of API without MetadataAsValue

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

// Bug fixes (TODO: upstream these)

void LLVMSetInitializer2(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal) {
  unwrap<GlobalVariable>(GlobalVar)->setInitializer(
      ConstantVal ? unwrap<Constant>(ConstantVal) : nullptr);
}

void LLVMSetPersonalityFn2(LLVMValueRef Fn, LLVMValueRef PersonalityFn) {
  unwrap<Function>(Fn)->setPersonalityFn(PersonalityFn ? unwrap<Constant>(PersonalityFn)
                                                       : nullptr);
}

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

void LLVMFunctionDeleteBody(LLVMValueRef Func) { unwrap<Function>(Func)->deleteBody(); }

void LLVMDestroyConstant(LLVMValueRef Const) { unwrap<Constant>(Const)->destroyConstant(); }


// operand bundles

DEFINE_STDCXX_CONVERSION_FUNCTIONS(OperandBundleUse, LLVMOperandBundleUseRef)

unsigned LLVMGetNumOperandBundles(LLVMValueRef Instr) {
  return unwrap<CallBase>(Instr)->getNumOperandBundles();
}

LLVMOperandBundleUseRef LLVMGetOperandBundle(LLVMValueRef Val, unsigned Index) {
  CallBase *CB = unwrap<CallBase>(Val);
  return wrap(new OperandBundleUse(CB->getOperandBundleAt(Index)));
}

void LLVMDisposeOperandBundleUse(LLVMOperandBundleUseRef Bundle) {
  delete unwrap<OperandBundleUse>(Bundle);
  return;
}

uint32_t LLVMGetOperandBundleUseTagID(LLVMOperandBundleUseRef Bundle) {
  const OperandBundleUse *S = unwrap<OperandBundleUse>(Bundle);
  return S->getTagID();
}

const char *LLVMGetOperandBundleUseTagName(LLVMOperandBundleUseRef Bundle,
                                           unsigned *Length) {
  const OperandBundleUse *S = unwrap<OperandBundleUse>(Bundle);
  *Length = S->getTagName().size();
  return S->getTagName().data();
}

unsigned LLVMGetOperandBundleUseNumInputs(LLVMOperandBundleUseRef Bundle) {
  return unwrap<OperandBundleUse>(Bundle)->Inputs.size();
}

void LLVMGetOperandBundleUseInputs(LLVMOperandBundleUseRef Bundle, LLVMValueRef *Dest) {
  size_t i = 0;
  for (auto &input : unwrap<OperandBundleUse>(Bundle)->Inputs)
    Dest[i++] = wrap(input);
}

DEFINE_STDCXX_CONVERSION_FUNCTIONS(OperandBundleDef, LLVMOperandBundleDefRef)

LLVMOperandBundleDefRef LLVMCreateOperandBundleDef(const char *Tag, LLVMValueRef *Inputs,
                                                   unsigned NumInputs) {
  SmallVector<Value *, 1> InputArray;
  for (auto *Input : ArrayRef(Inputs, NumInputs))
    InputArray.push_back(unwrap(Input));
  return wrap(new OperandBundleDef(std::string(Tag), InputArray));
}

LLVMOperandBundleDefRef LLVMOperandBundleDefFromUse(LLVMOperandBundleUseRef Bundle) {
  return wrap(new OperandBundleDef(*unwrap<OperandBundleUse>(Bundle)));
}

void LLVMDisposeOperandBundleDef(LLVMOperandBundleDefRef Bundle) {
  delete unwrap<OperandBundleDef>(Bundle);
  return;
}

const char *LLVMGetOperandBundleDefTag(LLVMOperandBundleDefRef Bundle, unsigned *Length) {
  const OperandBundleDef *S = unwrap<OperandBundleDef>(Bundle);
  *Length = S->getTag().size();
  return S->getTag().data();
}

unsigned LLVMGetOperandBundleDefNumInputs(LLVMOperandBundleDefRef Bundle) {
  return unwrap<OperandBundleDef>(Bundle)->input_size();
}

void LLVMGetOperandBundleDefInputs(LLVMOperandBundleDefRef Bundle, LLVMValueRef *Dest) {
  size_t i = 0;
  for (auto input : unwrap<OperandBundleDef>(Bundle)->inputs())
    Dest[i++] = wrap(input);
}

LLVMValueRef LLVMBuildCallWithOpBundle(LLVMBuilderRef B, LLVMValueRef Fn,
                                       LLVMValueRef *Args, unsigned NumArgs,
                                       LLVMOperandBundleDefRef *Bundles,
                                       unsigned NumBundles, const char *Name) {
  SmallVector<OperandBundleDef, 1> BundleArray;
  for (auto *Bundle : ArrayRef(Bundles, NumBundles))
    BundleArray.push_back(*unwrap<OperandBundleDef>(Bundle));

  llvm::IRBuilder<> *Builder = unwrap(B);
  llvm::ArrayRef<llvm::Value *> args = ArrayRef(unwrap(Args), NumArgs);

  Value *V = unwrap(Fn);
#if LLVM_VERSION_MAJOR >= 15
  FunctionType *FnT = cast<Function>(V)->getFunctionType();
#else
  FunctionType *FnT = cast<FunctionType>(V->getType()->getPointerElementType());
#endif
  llvm::CallInst *CI = Builder->CreateCall(FnT, unwrap(Fn), args, BundleArray, Name);
  return wrap(CI);
}

LLVMValueRef LLVMBuildCallWithOpBundle2(LLVMBuilderRef B, LLVMTypeRef Ty, LLVMValueRef Fn,
                                        LLVMValueRef *Args, unsigned NumArgs,
                                        LLVMOperandBundleDefRef *Bundles,
                                        unsigned NumBundles, const char *Name) {
  SmallVector<OperandBundleDef, 1> BundleArray;
  for (auto *Bundle : ArrayRef(Bundles, NumBundles))
    BundleArray.push_back(*unwrap<OperandBundleDef>(Bundle));

  llvm::IRBuilder<> *Builder = unwrap(B);
  llvm::ArrayRef<llvm::Value *> args = ArrayRef(unwrap(Args), NumArgs);

  FunctionType *FTy = unwrap<FunctionType>(Ty);
  llvm::CallInst *CI = Builder->CreateCall(FTy, unwrap(Fn), args, BundleArray, Name);
  return wrap(CI);
}


// metadata

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

void LLVMReplaceMDNodeOperandWith(LLVMMetadataRef MD, unsigned I, LLVMMetadataRef New) {
  unwrap<MDNode>(MD)->replaceOperandWith(I, unwrap(New));
}


// constant data

LLVMValueRef LLVMConstDataArray(LLVMTypeRef ElementTy, const void *Data,
                                unsigned NumElements) {
  StringRef S((const char *)Data,
              NumElements * unwrap(ElementTy)->getPrimitiveSizeInBits() / 8);
  return wrap(ConstantDataArray::getRaw(S, NumElements, unwrap(ElementTy)));
}


// missing opaque pointer APIs

LLVMBool LLVMContextSupportsTypedPointers(LLVMContextRef C) {
  return unwrap(C)->supportsTypedPointers();
}

LLVMTypeRef LLVMGetFunctionType(LLVMValueRef Fn) {
  auto Ftype = unwrap<Function>(Fn)->getFunctionType();
  return wrap(Ftype);
}

LLVMTypeRef LLVMGetGlobalValueType(LLVMValueRef GV) {
  auto Ftype = unwrap<GlobalValue>(GV)->getValueType();
  return wrap(Ftype);
}

#if LLVM_VERSION_MAJOR < 15
LLVMBool LLVMPointerTypeIsOpaque(LLVMTypeRef Ty) { return unwrap(Ty)->isOpaquePointerTy(); }
LLVMTypeRef LLVMPointerTypeInContext(LLVMContextRef C, unsigned AddressSpace) {
  return wrap(PointerType::get(*unwrap(C), AddressSpace));
}
#endif


// DominatorTree and PostDominatorTree

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
