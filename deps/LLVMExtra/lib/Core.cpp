#include "LLVMExtra.h"

#include <algorithm>
#include <iterator>

#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/ADT/SetVector.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfo.h>
#if LLVM_VERSION_MAJOR >= 19
#include <llvm/IR/DebugProgramInstruction.h>
#endif
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/ReplaceConstant.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Linker/Linker.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

using namespace llvm;

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
// Missing functionality
//


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

// the list may contain duplicates, which are only returned once
static SmallSetVector<GlobalValue *, 16> getUsedList(Module &M, bool CompilerUsed) {
  SmallVector<GlobalValue *, 16> Vec;
  collectUsedGlobalVariables(M, Vec, CompilerUsed);
  return SmallSetVector<GlobalValue *, 16>(Vec.begin(), Vec.end());
}

size_t LLVMGetNumUsed(LLVMModuleRef Mod) { return getUsedList(*unwrap(Mod), false).size(); }

void LLVMGetUsed(LLVMModuleRef Mod, LLVMValueRef *Dest) {
  for (auto *GV : getUsedList(*unwrap(Mod), false))
    *Dest++ = wrap(GV);
}

size_t LLVMGetNumCompilerUsed(LLVMModuleRef Mod) {
  return getUsedList(*unwrap(Mod), true).size();
}

void LLVMGetCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Dest) {
  for (auto *GV : getUsedList(*unwrap(Mod), true))
    *Dest++ = wrap(GV);
}

// like the static removeFromUsedList in ModuleUtils.cpp, which is only exposed (as
// removeFromUsedLists) since LLVM 16, and only for both lists at once
static void removeFromUsedList(Module &M, StringRef Name, ArrayRef<LLVMValueRef> Values) {
  GlobalVariable *GV = M.getNamedGlobal(Name);
  if (!GV || !GV->hasInitializer())
    return;
  auto *Init = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!Init)
    return;

  SmallPtrSet<Value *, 8> ToRemove;
  for (auto *V : Values)
    ToRemove.insert(unwrap(V));

  SmallVector<Constant *, 16> NewInit;
  for (Value *Op : Init->operands())
    if (!ToRemove.count(Op->stripPointerCasts()))
      NewInit.push_back(cast<Constant>(Op));
  if (NewInit.size() == Init->getNumOperands())
    return;

  if (!NewInit.empty()) {
    ArrayType *ATy = ArrayType::get(Init->getType()->getElementType(), NewInit.size());
    auto *NewGV = new GlobalVariable(M, ATy, false, GlobalValue::AppendingLinkage,
                                     ConstantArray::get(ATy, NewInit), "", GV,
                                     GV->getThreadLocalMode(), GV->getAddressSpace());
    NewGV->setSection(GV->getSection());
    NewGV->takeName(GV);
  }
  GV->eraseFromParent();
}

void LLVMRemoveFromUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count) {
  removeFromUsedList(*unwrap(Mod), "llvm.used", ArrayRef(Values, Count));
}

void LLVMRemoveFromCompilerUsed(LLVMModuleRef Mod, LLVMValueRef *Values, size_t Count) {
  removeFromUsedList(*unwrap(Mod), "llvm.compiler.used", ArrayRef(Values, Count));
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

void LLVMExtraMoveFunction(LLVMValueRef Fn, LLVMModuleRef Mod, LLVMValueRef Before) {
  Function *F = unwrap<Function>(Fn);
  Module *M = unwrap(Mod);
  if (Before == Fn)
    return;
  auto Pos = Before ? unwrap<Function>(Before)->getIterator() : M->getFunctionList().end();
  if (Module *Parent = F->getParent())
    M->getFunctionList().splice(Pos, Parent->getFunctionList(), F->getIterator());
  else
    M->getFunctionList().insert(Pos, F);
}

void LLVMExtraMoveGlobal(LLVMValueRef GlobalVar, LLVMModuleRef Mod, LLVMValueRef Before) {
  GlobalVariable *GV = unwrap<GlobalVariable>(GlobalVar);
  Module *M = unwrap(Mod);
  if (Before == GlobalVar)
    return;
#if LLVM_VERSION_MAJOR >= 17
  if (GV->getParent())
    GV->removeFromParent();
  M->insertGlobalVariable(Before ? unwrap<GlobalVariable>(Before)->getIterator()
                                 : M->global_end(),
                          GV);
#else
  auto Pos = Before ? unwrap<GlobalVariable>(Before)->getIterator()
                    : M->getGlobalList().end();
  if (Module *Parent = GV->getParent())
    M->getGlobalList().splice(Pos, Parent->getGlobalList(), GV->getIterator());
  else
    M->getGlobalList().insert(Pos, GV);
#endif
}

// mirrors LLVMIntrinsicGetOverloadTypes, proposed in llvm/llvm-project#230425
LLVMBool LLVMExtraIntrinsicGetOverloadTypes(unsigned ID, LLVMTypeRef FunctionTy,
                                            LLVMTypeRef *OverloadTypes,
                                            size_t *OverloadCount) {
  if (ID == Intrinsic::not_intrinsic || ID >= Intrinsic::num_intrinsics)
    return false;
  auto IID = static_cast<Intrinsic::ID>(ID);
  auto *FTy = unwrap<FunctionType>(FunctionTy);
  SmallVector<Type *, 4> OverloadTys;
#if LLVM_VERSION_MAJOR >= 23
  if (!Intrinsic::isSignatureValid(IID, FTy, OverloadTys))
    return false;
#else
  // what Intrinsic::isSignatureValid (getIntrinsicSignature before LLVM 23) does
  SmallVector<Intrinsic::IITDescriptor, 8> Table;
  Intrinsic::getIntrinsicInfoTableEntries(IID, Table);
  ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
  if (Intrinsic::matchIntrinsicSignature(FTy, TableRef, OverloadTys) !=
      Intrinsic::MatchIntrinsicTypes_Match)
    return false;
  if (Intrinsic::matchIntrinsicVarArg(FTy->isVarArg(), TableRef))
    return false;
#endif
  *OverloadCount = OverloadTys.size();
  if (OverloadTypes)
    std::copy(OverloadTys.begin(), OverloadTys.end(), unwrap(OverloadTypes));
  return true;
}

#if LLVM_VERSION_MAJOR >= 17
LLVMBool LLVMConvertUsersOfConstantsToInstructions(LLVMValueRef *Consts,
                                                   size_t Count,
                                                   LLVMValueRef RestrictToFunc,
                                                   LLVMBool RemoveDeadConstants,
                                                   LLVMBool IncludeSelf) {
  SmallVector<Constant *> Cs;
  for (auto *C : ArrayRef(Consts, Count))
    Cs.push_back(cast<Constant>(unwrap(C)));
#if LLVM_VERSION_MAJOR >= 19
  return convertUsersOfConstantsToInstructions(
      Cs, RestrictToFunc ? unwrap<Function>(RestrictToFunc) : nullptr,
      RemoveDeadConstants, IncludeSelf);
#else
  // LLVM 17/18 only expose the single-argument overload, whose behavior is
  // fixed at RestrictToFunc=nullptr, RemoveDeadConstants=true,
  // IncludeSelf=false. The Julia wrapper rejects non-default values for these
  // on LLVM < 19, so it is safe to ignore them here.
  (void)RestrictToFunc;
  (void)RemoveDeadConstants;
  (void)IncludeSelf;
  return convertUsersOfConstantsToInstructions(Cs);
#endif
}
#endif


//
// Attribute type detection
//

#if LLVM_VERSION_MAJOR >= 19

LLVMBool LLVMIsConstantRangeAttribute(LLVMAttributeRef A) {
  return unwrap(A).isConstantRangeAttribute();
}

#endif

#if LLVM_VERSION_MAJOR >= 20

LLVMBool LLVMIsConstantRangeListAttribute(LLVMAttributeRef A) {
  return unwrap(A).isConstantRangeListAttribute();
}

#endif


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

LLVMMetadataRef LLVMGetMDNodeOperand2(LLVMMetadataRef MD, unsigned I) {
  return wrap(unwrap<MDNode>(MD)->getOperand(I));
}

unsigned LLVMGetNamedMetadataNumOperands2(LLVMNamedMDNodeRef NMD) {
  return unwrap<NamedMDNode>(NMD)->getNumOperands();
}

void LLVMGetNamedMetadataOperands2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef *Dest) {
  NamedMDNode *N = unwrap<NamedMDNode>(NMD);
  for (unsigned i = 0; i < N->getNumOperands(); i++)
    Dest[i] = wrap(N->getOperand(i));
}

LLVMMetadataRef LLVMGetNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, unsigned I) {
  return wrap(unwrap<NamedMDNode>(NMD)->getOperand(I));
}

void LLVMAddNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, LLVMMetadataRef Val) {
  unwrap<NamedMDNode>(NMD)->addOperand(unwrap<MDNode>(Val));
}

void LLVMClearNamedMetadataOperands(LLVMNamedMDNodeRef NMD) {
  unwrap<NamedMDNode>(NMD)->clearOperands();
}

void LLVMSetNamedMetadataOperand2(LLVMNamedMDNodeRef NMD, unsigned I, LLVMMetadataRef Val) {
  unwrap<NamedMDNode>(NMD)->setOperand(I, unwrap<MDNode>(Val));
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

LLVMModuleRef LLVMExtraThreadSafeModuleGetModuleUnlocked(LLVMOrcThreadSafeModuleRef TSM) {
  return wrap(unwrap(TSM)->getModuleUnlocked());
}

#if LLVM_VERSION_MAJOR >= 16
LLVMModuleRef LLVMExtraThreadSafeModuleTakeModule(LLVMOrcThreadSafeModuleRef TSM) {
  return unwrap(TSM)->consumingModuleDo(
      [](std::unique_ptr<Module> M) { return wrap(M.release()); });
}
#endif

char *LLVMDumpJitDylibToString(LLVMOrcJITDylibRef JD) {
  std::string str;
  llvm::raw_string_ostream rso(str);
  auto jd = unwrap(JD);
  jd->dump(rso);
  rso.flush();
  return strdup(str.c_str());
}

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ObjectLayer, LLVMOrcObjectLayerRef)

static orc::RTDyldObjectLinkingLayer *unwrapRTDyld(LLVMOrcObjectLayerRef Layer) {
  return static_cast<orc::RTDyldObjectLinkingLayer *>(unwrap(Layer));
}

void LLVMOrcRTDyldObjectLinkingLayerSetOverrideObjectFlagsWithResponsibilityFlags(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, LLVMBool OverrideObjectFlags) {
  unwrapRTDyld(RTDyldObjLinkingLayer)
      ->setOverrideObjectFlagsWithResponsibilityFlags(OverrideObjectFlags);
}

void LLVMOrcRTDyldObjectLinkingLayerSetAutoClaimResponsibilityForObjectSymbols(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, LLVMBool AutoClaimObjectSymbols) {
  unwrapRTDyld(RTDyldObjLinkingLayer)
      ->setAutoClaimResponsibilityForObjectSymbols(AutoClaimObjectSymbols);
}

// Workaround: RTDyldObjectLinkingLayer registers itself as a resource manager of its
// execution session, but unlike LinkGraphLinkingLayer, its destructor doesn't deregister
// it, so the session calls into the destroyed layer when it ends. That's harmless when a
// JIT owns the layer, as the JIT ends the session first, but not for a layer that was
// never handed over to a JIT. See JuliaLLVM/LLVM.jl#629.
void LLVMExtraDisposeRTDyldObjectLinkingLayer(LLVMOrcObjectLayerRef RTDyldObjLinkingLayer) {
  auto *Layer = unwrapRTDyld(RTDyldObjLinkingLayer);
  // ResourceManager is a private base of RTDyldObjectLinkingLayer, which only a C-style
  // cast can convert to (this is well-defined for an unambiguous base, see [expr.cast])
  Layer->getExecutionSession().deregisterResourceManager(
      *(orc::ResourceManager *)Layer);
  delete Layer;
}

// Mirrors LLJIT::createObjectLinkingLayer.
void LLVMOrcRTDyldObjectLinkingLayerApplyTargetDefaults(
    LLVMOrcObjectLayerRef RTDyldObjLinkingLayer, const char *TripleStr) {
  auto *Layer = unwrapRTDyld(RTDyldObjLinkingLayer);
  // Normalize so that, e.g., the default x86_64-w64-mingw32 triple is recognized as COFF.
  Triple TT(Triple::normalize(TripleStr));
  if (TT.isOSBinFormatCOFF()) {
    Layer->setOverrideObjectFlagsWithResponsibilityFlags(true);
    Layer->setAutoClaimResponsibilityForObjectSymbols(true);
  }
#if LLVM_VERSION_MAJOR >= 16
  if (TT.isOSBinFormatELF() &&
      (TT.getArch() == Triple::ArchType::ppc64 ||
       TT.getArch() == Triple::ArchType::ppc64le))
    Layer->setAutoClaimResponsibilityForObjectSymbols(true);
#endif
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
  // like remapInstructionsInBlocks, which crashes on a detached block (it looks up the
  // module of the instructions it remaps)
#if LLVM_VERSION_MAJOR >= 18
  BasicBlock *Src = unwrap(BB);
  Module *M = Src->getParent() ? Src->getModule() : nullptr;
#endif
  for (Instruction &I : *NewBB) {
#if LLVM_VERSION_MAJOR >= 19
    RemapDbgRecordRange(M, I.getDbgRecordRange(), VMap,
                        RF_NoModuleLevelChanges | RF_IgnoreMissingLocals);
#elif LLVM_VERSION_MAJOR >= 18
    RemapDPValueRange(M, I.getDbgValueRange(), VMap,
                      RF_NoModuleLevelChanges | RF_IgnoreMissingLocals);
#endif
    RemapInstruction(&I, VMap, RF_NoModuleLevelChanges | RF_IgnoreMissingLocals);
  }
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

#if LLVM_VERSION_MAJOR < 21
LLVMValueRef LLVMConstDataArray(LLVMTypeRef ElementTy, const void *Data,
                                size_t SizeInBytes) {
  Type *Ty = unwrap(ElementTy);
  size_t Len = SizeInBytes / (Ty->getPrimitiveSizeInBits() / 8);
  return wrap(ConstantDataArray::getRaw(StringRef((const char*)Data, SizeInBytes), Len, Ty));
}
#endif


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

#if LLVM_VERSION_MAJOR < 18

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

void LLVMExtraSetFastMathFlags(LLVMValueRef FPMathInst, LLVMFastMathFlags FMF) {
  Value *P = unwrap<Value>(FPMathInst);
  cast<Instruction>(P)->copyFastMathFlags(mapFromLLVMFastMathFlags(FMF));
}


//
// tail calls
//

#if LLVM_VERSION_MAJOR < 18

LLVMTailCallKind LLVMGetTailCallKind(LLVMValueRef Call) {
  return (LLVMTailCallKind)unwrap<CallInst>(Call)->getTailCallKind();
}

void LLVMSetTailCallKind(LLVMValueRef Call, LLVMTailCallKind kind) {
  unwrap<CallInst>(Call)->setTailCallKind((CallInst::TailCallKind)kind);
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

#if LLVM_VERSION_MAJOR >= 16 && LLVM_VERSION_MAJOR < 19
// operations that the C API only exposes from LLVM 19 on, using the values it assigns them
enum {
  LLVMExtraAtomicRMWBinOpUIncWrap = 15,
  LLVMExtraAtomicRMWBinOpUDecWrap = 16,
};
#endif

// takes an integer, because LLVMExtraBuildAtomicRMWSyncScope passes values that are out of
// range of the LLVMAtomicRMWBinOp enum
static AtomicRMWInst::BinOp mapFromLLVMRMWBinOp(unsigned BinOp) {
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
#elif LLVM_VERSION_MAJOR >= 16
    case LLVMExtraAtomicRMWBinOpUIncWrap: return AtomicRMWInst::UIncWrap;
    case LLVMExtraAtomicRMWBinOpUDecWrap: return AtomicRMWInst::UDecWrap;
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

#if LLVM_VERSION_MAJOR >= 16 && LLVM_VERSION_MAJOR < 19
LLVMValueRef LLVMExtraBuildAtomicRMWSyncScope(LLVMBuilderRef B, unsigned op, LLVMValueRef PTR,
                                              LLVMValueRef Val, LLVMAtomicOrdering ordering,
                                              unsigned SSID) {
  return wrap(unwrap(B)->CreateAtomicRMW(mapFromLLVMRMWBinOp(op), unwrap(PTR), unwrap(Val),
                                         MaybeAlign(), mapFromLLVMOrdering(ordering), SSID));
}

// LLVMGetAtomicRMWBinOp hits an llvm_unreachable on these operations
unsigned LLVMExtraGetAtomicRMWBinOp(LLVMValueRef Inst) {
  switch (unwrap<AtomicRMWInst>(Inst)->getOperation()) {
    case AtomicRMWInst::UIncWrap: return LLVMExtraAtomicRMWBinOpUIncWrap;
    case AtomicRMWInst::UDecWrap: return LLVMExtraAtomicRMWBinOpUDecWrap;
    default: return LLVMGetAtomicRMWBinOp(Inst);
  }
}
#endif

#if LLVM_VERSION_MAJOR < 18
static LLVMAtomicOrdering mapToLLVMOrdering(AtomicOrdering Ordering) {
  switch (Ordering) {
    case AtomicOrdering::NotAtomic: return LLVMAtomicOrderingNotAtomic;
    case AtomicOrdering::Unordered: return LLVMAtomicOrderingUnordered;
    case AtomicOrdering::Monotonic: return LLVMAtomicOrderingMonotonic;
    case AtomicOrdering::Acquire: return LLVMAtomicOrderingAcquire;
    case AtomicOrdering::Release: return LLVMAtomicOrderingRelease;
    case AtomicOrdering::AcquireRelease: return LLVMAtomicOrderingAcquireRelease;
    case AtomicOrdering::SequentiallyConsistent:
      return LLVMAtomicOrderingSequentiallyConsistent;
    default: break;
  }
  llvm_unreachable("Invalid AtomicOrdering value!");
}

// the C API versions don't handle fences, and can't set the ordering of atomicrmw
LLVMAtomicOrdering LLVMExtraGetOrdering(LLVMValueRef MemAccessInst) {
  Value *P = unwrap(MemAccessInst);
  if (FenceInst *FI = dyn_cast<FenceInst>(P))
    return mapToLLVMOrdering(FI->getOrdering());
  return LLVMGetOrdering(MemAccessInst);
}

void LLVMExtraSetOrdering(LLVMValueRef MemAccessInst, LLVMAtomicOrdering Ordering) {
  Value *P = unwrap(MemAccessInst);
  if (FenceInst *FI = dyn_cast<FenceInst>(P))
    return FI->setOrdering(mapFromLLVMOrdering(Ordering));
  if (AtomicRMWInst *RMWI = dyn_cast<AtomicRMWInst>(P))
    return RMWI->setOrdering(mapFromLLVMOrdering(Ordering));
  LLVMSetOrdering(MemAccessInst, Ordering);
}
#endif

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


const char *LLVMExtraGetSyncScopeName(LLVMContextRef C, unsigned SSID, size_t *Len) {
  // the names are indexed by ID, and owned by the context
  SmallVector<StringRef> Names;
  unwrap(C)->getSyncScopeNames(Names);
  if (SSID >= Names.size()) {
    *Len = 0;
    return nullptr;
  }
  *Len = Names[SSID].size();
  return Names[SSID].data();
}


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

//
// Linker extensions
//

LLVMBool LLVMLinkModules3(LLVMModuleRef Dest, LLVMModuleRef Src, unsigned Flags) {
  Module *D = unwrap(Dest);
  std::unique_ptr<Module> M(unwrap(Src));
  return Linker::linkModules(*D, std::move(M), Flags);
}

#if LLVM_VERSION_MAJOR >= 21

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeContext, LLVMOrcThreadSafeContextRef)

//
// Removed from LLVM and unsafe but it's only used to provide an unsafe API
// on the julia side anyway
//

LLVMContextRef LLVMOrcThreadSafeContextGetContext(LLVMOrcThreadSafeContextRef TSCtx) {
  return wrap(unwrap(TSCtx)->withContextDo([] (LLVMContext *ctx) { return ctx; }));
}

#endif


//
// Poison-generating flags
//

#if LLVM_VERSION_MAJOR < 17

LLVMBool LLVMGetNUW(LLVMValueRef ArithInst) {
  return unwrap<Instruction>(ArithInst)->hasNoUnsignedWrap();
}

void LLVMSetNUW(LLVMValueRef ArithInst, LLVMBool HasNUW) {
  unwrap<Instruction>(ArithInst)->setHasNoUnsignedWrap(HasNUW);
}

LLVMBool LLVMGetNSW(LLVMValueRef ArithInst) {
  return unwrap<Instruction>(ArithInst)->hasNoSignedWrap();
}

void LLVMSetNSW(LLVMValueRef ArithInst, LLVMBool HasNSW) {
  unwrap<Instruction>(ArithInst)->setHasNoSignedWrap(HasNSW);
}

LLVMBool LLVMGetExact(LLVMValueRef DivOrShrInst) {
  return unwrap<Instruction>(DivOrShrInst)->isExact();
}

void LLVMSetExact(LLVMValueRef DivOrShrInst, LLVMBool IsExact) {
  unwrap<Instruction>(DivOrShrInst)->setIsExact(IsExact);
}

#endif

#if LLVM_VERSION_MAJOR == 20

LLVMBool LLVMGetICmpSameSign(LLVMValueRef Inst) {
  return unwrap<ICmpInst>(Inst)->hasSameSign();
}

void LLVMSetICmpSameSign(LLVMValueRef Inst, LLVMBool SameSign) {
  unwrap<ICmpInst>(Inst)->setSameSign(SameSign);
}

#endif


//
// Switch case values
//

#if LLVM_VERSION_MAJOR < 22

LLVMValueRef LLVMGetSwitchCaseValue(LLVMValueRef Switch, unsigned i) {
  assert(i > 0 && i <= unwrap<SwitchInst>(Switch)->getNumCases());
  auto It = unwrap<SwitchInst>(Switch)->case_begin() + (i - 1);
  return wrap(It->getCaseValue());
}

void LLVMSetSwitchCaseValue(LLVMValueRef Switch, unsigned i, LLVMValueRef CaseValue) {
  assert(i > 0 && i <= unwrap<SwitchInst>(Switch)->getNumCases());
  auto It = unwrap<SwitchInst>(Switch)->case_begin() + (i - 1);
  It->setValue(unwrap<ConstantInt>(CaseValue));
}

#endif


//
// Floating-point constants
//

#if LLVM_VERSION_MAJOR < 22
LLVMValueRef LLVMConstFPFromBits(LLVMTypeRef Ty, const uint64_t N[]) {
  Type *T = unwrap(Ty);
  unsigned SB = T->getScalarSizeInBits();
  APInt AI(SB, ArrayRef<uint64_t>(N, divideCeil(SB, 64)));
  APFloat Quad(T->getFltSemantics(), AI);
  return wrap(ConstantFP::get(T, Quad));
}
#endif

void LLVMExtraConstFPGetBits(LLVMValueRef ConstantVal, uint64_t N[]) {
  APInt AI = unwrap<ConstantFP>(ConstantVal)->getValueAPF().bitcastToAPInt();
  std::copy_n(AI.getRawData(), AI.getNumWords(), N);
}

void LLVMExtraConstIntGetWords(LLVMValueRef ConstantVal, uint64_t N[]) {
  const APInt &AI = unwrap<ConstantInt>(ConstantVal)->getValue();
  std::copy_n(AI.getRawData(), AI.getNumWords(), N);
}


//
// Debug records
//

#if LLVM_VERSION_MAJOR >= 19

#if LLVM_VERSION_MAJOR < 22
LLVMDbgRecordRef LLVMGetFirstDbgRecord2(LLVMValueRef Inst) {
  Instruction *Instr = unwrap<Instruction>(Inst);
  if (!Instr->DebugMarker)
    return nullptr;
  auto I = Instr->DebugMarker->StoredDbgRecords.begin();
  if (I == Instr->DebugMarker->StoredDbgRecords.end())
    return nullptr;
  return wrap(&*I);
}
#endif

#if LLVM_VERSION_MAJOR < 20
LLVMDbgRecordRef LLVMGetNextDbgRecord(LLVMDbgRecordRef Rec) {
  DbgRecord *Record = unwrap<DbgRecord>(Rec);
  simple_ilist<DbgRecord>::iterator I(Record);
  if (++I == Record->getMarker()->StoredDbgRecords.end())
    return nullptr;
  return wrap(&*I);
}
#endif

#if LLVM_VERSION_MAJOR < 22
LLVMMetadataRef LLVMDbgRecordGetDebugLoc(LLVMDbgRecordRef Rec) {
  return wrap(unwrap<DbgRecord>(Rec)->getDebugLoc().getAsMDNode());
}

LLVMDbgRecordKind LLVMDbgRecordGetKind(LLVMDbgRecordRef Rec) {
  DbgRecord *Record = unwrap<DbgRecord>(Rec);
  if (isa<DbgLabelRecord>(Record))
    return LLVMDbgRecordLabel;
  DbgVariableRecord *VariableRecord = cast<DbgVariableRecord>(Record);
  if (VariableRecord->isDbgDeclare())
    return LLVMDbgRecordDeclare;
  if (VariableRecord->isDbgValue())
    return LLVMDbgRecordValue;
  assert(VariableRecord->isDbgAssign() && "unexpected record");
  return LLVMDbgRecordAssign;
}

LLVMValueRef LLVMDbgVariableRecordGetValue(LLVMDbgRecordRef Rec, unsigned OpIdx) {
  return wrap(unwrap<DbgVariableRecord>(Rec)->getValue(OpIdx));
}

LLVMMetadataRef LLVMDbgVariableRecordGetVariable(LLVMDbgRecordRef Rec) {
  return wrap(unwrap<DbgVariableRecord>(Rec)->getRawVariable());
}

LLVMMetadataRef LLVMDbgVariableRecordGetExpression(LLVMDbgRecordRef Rec) {
  return wrap(unwrap<DbgVariableRecord>(Rec)->getRawExpression());
}
#endif

unsigned LLVMExtraDbgVariableRecordGetNumValues(LLVMDbgRecordRef Rec) {
  return unwrap<DbgVariableRecord>(Rec)->getNumVariableLocationOps();
}

#endif


//
// attributes
//

const char *LLVMExtraGetAttributeKindName(unsigned KindID, size_t *Len) {
  if (KindID == Attribute::None || KindID >= Attribute::EndAttrKinds)
    return nullptr;
  StringRef Name = Attribute::getNameFromAttrKind((Attribute::AttrKind)KindID);
  *Len = Name.size();
  return Name.data();
}

LLVMBool LLVMExtraIsEnumAttributeKind(unsigned KindID) {
  return Attribute::isEnumAttrKind((Attribute::AttrKind)KindID);
}

LLVMBool LLVMExtraIsIntAttributeKind(unsigned KindID) {
  return Attribute::isIntAttrKind((Attribute::AttrKind)KindID);
}

LLVMBool LLVMExtraIsTypeAttributeKind(unsigned KindID) {
  return Attribute::isTypeAttrKind((Attribute::AttrKind)KindID);
}

#if LLVM_VERSION_MAJOR >= 19
LLVMBool LLVMExtraIsConstantRangeAttributeKind(unsigned KindID) {
  return Attribute::isConstantRangeAttrKind((Attribute::AttrKind)KindID);
}
#endif


//
// aggregates
//

LLVMValueRef LLVMExtraBuildExtractValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                        const unsigned *Idxs, unsigned NumIdxs,
                                        const char *Name) {
  return wrap(
      unwrap(B)->CreateExtractValue(unwrap(AggVal), ArrayRef<unsigned>(Idxs, NumIdxs), Name));
}

LLVMValueRef LLVMExtraBuildInsertValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                       LLVMValueRef EltVal, const unsigned *Idxs,
                                       unsigned NumIdxs, const char *Name) {
  return wrap(unwrap(B)->CreateInsertValue(unwrap(AggVal), unwrap(EltVal),
                                           ArrayRef<unsigned>(Idxs, NumIdxs), Name));
}

LLVMValueRef LLVMExtraConstVectorSplat(LLVMTypeRef VecTy, LLVMValueRef Elt) {
  return wrap(ConstantVector::getSplat(cast<VectorType>(unwrap(VecTy))->getElementCount(),
                                       unwrap<Constant>(Elt)));
}


//
// memory
//

LLVMValueRef LLVMExtraBuildAlloca(LLVMBuilderRef B, LLVMTypeRef Ty, unsigned AddrSpace,
                                  LLVMValueRef ArraySize, const char *Name) {
  return wrap(unwrap(B)->CreateAlloca(unwrap(Ty), AddrSpace,
                                      ArraySize ? unwrap(ArraySize) : nullptr, Name));
}

unsigned LLVMExtraGetIndexSizeInBits(LLVMTargetDataRef TD, unsigned AddrSpace) {
  return unwrap(TD)->getIndexSizeInBits(AddrSpace);
}

LLVMBool LLVMExtraGEPAccumulateConstantOffset(LLVMValueRef GEP, LLVMTargetDataRef TD,
                                              uint64_t *Words) {
  auto *Op = cast<GEPOperator>(unwrap(GEP));
  const DataLayout &DL = *unwrap(TD);
  APInt Offset(DL.getIndexSizeInBits(Op->getPointerAddressSpace()), 0);
  if (!Op->accumulateConstantOffset(DL, Offset))
    return false;
  std::copy_n(Offset.getRawData(), Offset.getNumWords(), Words);
  return true;
}


//
// instructions
//

//
// insertion points
//
// An insertion point is a block, the instruction to insert before (NULL for the end of the
// block), and on LLVM 19+ the head bit of the iterator, which selects whether to insert
// before the debug records attached to that instruction (or trailing the block).

static BasicBlock::iterator insertionPoint(LLVMBasicBlockRef BB, LLVMValueRef Before,
                                           LLVMBool Head) {
  BasicBlock::iterator It =
      Before ? unwrap<Instruction>(Before)->getIterator() : unwrap(BB)->end();
#if LLVM_VERSION_MAJOR >= 19
  It.setHeadBit(Head);
#endif
  return It;
}

static void readInsertionPoint(BasicBlock *BB, BasicBlock::iterator It, LLVMValueRef *Before,
                               LLVMBool *Head) {
  *Before = It == BB->end() ? nullptr : wrap(&*It);
#if LLVM_VERSION_MAJOR >= 19
  *Head = It.getHeadBit();
#else
  *Head = false;
#endif
}

void LLVMExtraMoveInstruction(LLVMValueRef Inst, LLVMBasicBlockRef BB, LLVMValueRef Before,
                              LLVMBool Head) {
  Instruction *I = unwrap<Instruction>(Inst);
  BasicBlock *B = unwrap(BB);
  if (Before == Inst) {
#if LLVM_VERSION_MAJOR >= 19
    // moving an instruction ahead of its own debug records: splicing an instruction
    // before itself isn't allowed, so move it before the next one instead
    if (Head)
      I->moveBefore(*B, insertionPoint(BB, wrap(I->getNextNode()), true));
#endif
    return;
  }
  BasicBlock::iterator It = insertionPoint(BB, Before, Head);
  if (I->getParent())
    I->moveBefore(*B, It);
  else
#if LLVM_VERSION_MAJOR >= 16
    I->insertInto(B, It);
#else
    B->getInstList().insert(It, I);
#endif
}

void LLVMExtraMoveBasicBlock(LLVMBasicBlockRef BB, LLVMValueRef Fn, LLVMBasicBlockRef Before) {
  BasicBlock *B = unwrap(BB);
  Function *F = unwrap<Function>(Fn);
  if (Before == BB)
    return;
  if (Function *Parent = B->getParent()) {
    // unlike BasicBlock::moveBefore, splice into the destination function
    auto Pos = Before ? unwrap(Before)->getIterator() : F->end();
#if LLVM_VERSION_MAJOR >= 16
    F->splice(Pos, Parent, B->getIterator());
#else
    F->getBasicBlockList().splice(Pos, Parent->getBasicBlockList(), B->getIterator());
#endif
  } else {
    B->insertInto(F, Before ? unwrap(Before) : nullptr);
  }
}

void LLVMExtraDeleteBasicBlock(LLVMBasicBlockRef BB) {
  BasicBlock *B = unwrap(BB);
  if (B->getParent())
    B->eraseFromParent();
  else
    delete B;
}

void LLVMExtraPositionBuilder(LLVMBuilderRef Builder, LLVMBasicBlockRef BB,
                              LLVMValueRef Before, LLVMBool Head) {
  unwrap(Builder)->SetInsertPoint(unwrap(BB), insertionPoint(BB, Before, Head));
}

LLVMBasicBlockRef LLVMExtraGetInsertPoint(LLVMBuilderRef Builder, LLVMValueRef *Before,
                                          LLVMBool *Head) {
  IRBuilder<> *B = unwrap(Builder);
  BasicBlock *BB = B->GetInsertBlock();
  *Before = nullptr;
  *Head = false;
  if (!BB)
    return nullptr;
  readInsertionPoint(BB, B->GetInsertPoint(), Before, Head);
  return wrap(BB);
}

LLVMBool LLVMExtraGetFirstInsertionPt(LLVMBasicBlockRef BB, LLVMValueRef *Before,
                                      LLVMBool *Head) {
  BasicBlock *B = unwrap(BB);
  BasicBlock::iterator It = B->getFirstInsertionPt();
  // the end of a terminated block is not a legal insertion point, which happens when the
  // terminator is an EH pad (e.g., a catchswitch)
  if (It == B->end() && B->getTerminator())
    return false;
  readInsertionPoint(B, It, Before, Head);
  return true;
}

#if LLVM_VERSION_MAJOR >= 19
// insert a debug record using a DIBuilder function, which is passed the position
template <typename InsertFn>
static LLVMDbgRecordRef insertRecordAt(InsertFn Insert, LLVMBasicBlockRef BB,
                                       LLVMValueRef Before, LLVMBool Head) {
#if LLVM_VERSION_MAJOR >= 20
  DbgInstPtr DbgInst = Insert(InsertPosition(insertionPoint(BB, Before, Head)));
#else
  // LLVM 19's DIBuilder only inserts before an instruction, or at the end of a block
  // (before its terminator, but the caller checked that there is none)
  DbgInstPtr DbgInst = Before ? Insert(unwrap<Instruction>(Before)) : Insert(unwrap(BB));
#endif
  // like the C API, this requires the new debug info format
  assert(isa<DbgRecord *>(DbgInst) && "Function unexpectedly in old debug info format");
  DbgRecord *DR = cast<DbgRecord *>(DbgInst);
#if LLVM_VERSION_MAJOR < 20
  // move the record in front of the other records at that position
  if (Head) {
    DR->removeFromParent();
    unwrap(BB)->insertDbgRecordBefore(DR, insertionPoint(BB, Before, Head));
  }
#endif
  return wrap(DR);
}

LLVMDbgRecordRef LLVMExtraDIBuilderInsertDeclareRecordAt(
    LLVMDIBuilderRef Builder, LLVMValueRef Storage, LLVMMetadataRef VarInfo,
    LLVMMetadataRef Expr, LLVMMetadataRef DL, LLVMBasicBlockRef BB, LLVMValueRef Before,
    LLVMBool Head) {
  return insertRecordAt(
      [&](auto Pos) {
        return unwrap(Builder)->insertDeclare(unwrap(Storage), unwrap<DILocalVariable>(VarInfo),
                                              unwrap<DIExpression>(Expr),
                                              unwrap<DILocation>(DL), Pos);
      },
      BB, Before, Head);
}

LLVMDbgRecordRef LLVMExtraDIBuilderInsertDbgValueRecordAt(
    LLVMDIBuilderRef Builder, LLVMValueRef Val, LLVMMetadataRef VarInfo,
    LLVMMetadataRef Expr, LLVMMetadataRef DL, LLVMBasicBlockRef BB, LLVMValueRef Before,
    LLVMBool Head) {
  return insertRecordAt(
      [&](auto Pos) {
        return unwrap(Builder)->insertDbgValueIntrinsic(
            unwrap(Val), unwrap<DILocalVariable>(VarInfo), unwrap<DIExpression>(Expr),
            unwrap<DILocation>(DL), Pos);
      },
      BB, Before, Head);
}

#if LLVM_VERSION_MAJOR >= 20
LLVMDbgRecordRef LLVMExtraDIBuilderInsertLabelAt(LLVMDIBuilderRef Builder,
                                                 LLVMMetadataRef LabelInfo,
                                                 LLVMMetadataRef DL, LLVMBasicBlockRef BB,
                                                 LLVMValueRef Before, LLVMBool Head) {
  return insertRecordAt(
      [&](auto Pos) {
        return unwrap(Builder)->insertLabel(unwrap<DILabel>(LabelInfo),
                                            unwrap<DILocation>(DL), Pos);
      },
      BB, Before, Head);
}
#endif
#endif

LLVMBool LLVMExtraInstructionComesBefore(LLVMValueRef Inst, LLVMValueRef Other) {
  return unwrap<Instruction>(Inst)->comesBefore(unwrap<Instruction>(Other));
}

LLVMBool LLVMExtraMayReadFromMemory(LLVMValueRef Inst) {
  return unwrap<Instruction>(Inst)->mayReadFromMemory();
}

LLVMBool LLVMExtraMayWriteToMemory(LLVMValueRef Inst) {
  return unwrap<Instruction>(Inst)->mayWriteToMemory();
}

LLVMBool LLVMExtraMayHaveSideEffects(LLVMValueRef Inst) {
  return unwrap<Instruction>(Inst)->mayHaveSideEffects();
}


//
// values
//

void LLVMExtraTakeName(LLVMValueRef Val, LLVMValueRef From) {
  Value *V = unwrap(Val);
  Value *F = unwrap(From);
  if (V == F)
    return;
  V->takeName(F);
  // functions cache their intrinsic ID based on their name, which `setName` updates, but
  // `takeName` doesn't
  for (Value *Val : {V, F}) {
    if (auto *Fn = dyn_cast<Function>(Val)) {
#if LLVM_VERSION_MAJOR >= 18 // llvm/llvm-project#72867
      Fn->updateAfterNameChange();
#else
      Fn->recalculateIntrinsicID();
#endif
    }
  }
}

LLVMValueRef LLVMExtraStripPointerCasts(LLVMValueRef Val) {
  return wrap(unwrap(Val)->stripPointerCasts());
}

LLVMValueRef LLVMExtraStripPointerCastsAndAliases(LLVMValueRef Val) {
  return wrap(unwrap(Val)->stripPointerCastsAndAliases());
}

unsigned LLVMExtraGetArgNo(LLVMValueRef Arg) { return unwrap<Argument>(Arg)->getArgNo(); }


//
// functions and global variables
//

void LLVMExtraCopyAttributesFrom(LLVMValueRef Dst, LLVMValueRef Src) {
  Value *D = unwrap(Dst);
  if (auto *F = dyn_cast<Function>(D))
    F->copyAttributesFrom(unwrap<Function>(Src));
  else
    unwrap<GlobalVariable>(Dst)->copyAttributesFrom(unwrap<GlobalVariable>(Src));
}


//
// constants
//

void LLVMExtraRemoveDeadConstantUsers(LLVMValueRef C) {
  Constant *Const = unwrap<Constant>(C);
#if LLVM_VERSION_MAJOR >= 21 // llvm/llvm-project#137313
  // constant data doesn't track its uses anymore
  if (!Const->hasUseList())
    return;
#endif
  Const->removeDeadConstantUsers();
}


//
// verification
//

LLVMBool LLVMExtraVerifyFunction(LLVMValueRef Fn, char **OutMessage) {
  std::string Message;
  raw_string_ostream OS(Message);
  bool Broken = verifyFunction(*unwrap<Function>(Fn), &OS);
  OS.flush();
  *OutMessage = Broken ? strdup(Message.c_str()) : nullptr;
  return Broken;
}
