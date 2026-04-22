#include "LLVMExtra.h"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetTransformInfoImpl.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/CBindingWrapping.h>

#include <optional>

using namespace llvm;

// Backing struct for the opaque `LLVMTTIOptionsRef` handle. Unset scalar
// fields are `std::nullopt`; unset callbacks are `nullptr`. In both cases
// `CustomTargetTransformInfo` falls back to `TargetTransformInfoImplCRTPBase`.
// Each callback is paired with its own UserData (LLVM C-API convention).
struct LLVMTTIOptions {
  std::optional<unsigned> FlatAddressSpace;
  std::optional<bool> HasBranchDivergence;
  std::optional<bool> IsSingleThreaded;
  LLVMTTIASPairPredicateFn IsNoopAddrSpaceCast = nullptr;
  void *IsNoopAddrSpaceCastUD = nullptr;
  LLVMTTIASPairPredicateFn IsValidAddrSpaceCast = nullptr;
  void *IsValidAddrSpaceCastUD = nullptr;
  LLVMTTIASPairPredicateFn AddrSpacesMayAlias = nullptr;
  void *AddrSpacesMayAliasUD = nullptr;
  LLVMTTIASPredicateFn CanHaveGlobalInitializerInAS = nullptr;
  void *CanHaveGlobalInitializerInASUD = nullptr;
  LLVMTTIValuePredicateFn IsSourceOfDivergence = nullptr;
  void *IsSourceOfDivergenceUD = nullptr;
  LLVMTTIValuePredicateFn IsAlwaysUniform = nullptr;
  void *IsAlwaysUniformUD = nullptr;
  LLVMTTIGetAssumedAddressSpaceFn GetAssumedAddressSpace = nullptr;
  void *GetAssumedAddressSpaceUD = nullptr;
  LLVMTTIGetPredicatedAddressSpaceFn GetPredicatedAddressSpace = nullptr;
  void *GetPredicatedAddressSpaceUD = nullptr;
  LLVMTTIRewriteIntrinsicFn RewriteIntrinsicWithAS = nullptr;
  void *RewriteIntrinsicWithASUD = nullptr;
  LLVMTTICollectFlatAddressOperandsFn CollectFlatAddressOperands = nullptr;
  void *CollectFlatAddressOperandsUD = nullptr;
};
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(LLVMTTIOptions, LLVMTTIOptionsRef)

// Minimal TargetTransformInfo for pipelines that don't have a TargetMachine.
// Wraps an `LLVMTTIOptions` value and falls back to the default
// `TargetTransformInfoImplCRTPBase` behavior for any field left unset.
namespace {

// Fixed-size scratch buffer for CollectFlatAddressOperands. More than enough
// for any real intrinsic (pointer operand counts are tiny). If a caller
// produces more, the tail is silently dropped — callbacks know the buffer
// size in advance and can clamp themselves.
constexpr unsigned CollectFlatAddressOperandsBufSize = 32;

class CustomTargetTransformInfo final
    : public TargetTransformInfoImplCRTPBase<CustomTargetTransformInfo> {
  typedef TargetTransformInfoImplCRTPBase<CustomTargetTransformInfo> BaseT;

public:
  CustomTargetTransformInfo(const DataLayout &DL, LLVMTTIOptions Opts)
      : BaseT(DL), Opts(Opts) {}

  unsigned getFlatAddressSpace() const {
    if (Opts.FlatAddressSpace) return *Opts.FlatAddressSpace;
    return BaseT::getFlatAddressSpace();
  }

  bool isNoopAddrSpaceCast(unsigned FromAS, unsigned ToAS) const {
    if (Opts.IsNoopAddrSpaceCast)
      return Opts.IsNoopAddrSpaceCast(FromAS, ToAS,
                                      Opts.IsNoopAddrSpaceCastUD) != 0;
    return BaseT::isNoopAddrSpaceCast(FromAS, ToAS);
  }

  bool canHaveNonUndefGlobalInitializerInAddressSpace(unsigned AS) const {
    if (Opts.CanHaveGlobalInitializerInAS)
      return Opts.CanHaveGlobalInitializerInAS(
                 AS, Opts.CanHaveGlobalInitializerInASUD) != 0;
    return BaseT::canHaveNonUndefGlobalInitializerInAddressSpace(AS);
  }

  // `isValidAddrSpaceCast` and `addrspacesMayAlias` were added in LLVM 17.
  // On earlier versions the pass framework doesn't query them, so we skip
  // providing the override entirely.
#if LLVM_VERSION_MAJOR >= 17
  bool isValidAddrSpaceCast(unsigned FromAS, unsigned ToAS) const {
    if (Opts.IsValidAddrSpaceCast)
      return Opts.IsValidAddrSpaceCast(FromAS, ToAS,
                                       Opts.IsValidAddrSpaceCastUD) != 0;
    return BaseT::isValidAddrSpaceCast(FromAS, ToAS);
  }

  bool addrspacesMayAlias(unsigned AS0, unsigned AS1) const {
    if (Opts.AddrSpacesMayAlias)
      return Opts.AddrSpacesMayAlias(AS0, AS1,
                                     Opts.AddrSpacesMayAliasUD) != 0;
    return BaseT::addrspacesMayAlias(AS0, AS1);
  }
#endif

  // `hasBranchDivergence` grew a `Function*` parameter in LLVM 17.
#if LLVM_VERSION_MAJOR >= 17
  bool hasBranchDivergence(const Function *F = nullptr) const {
    if (Opts.HasBranchDivergence) return *Opts.HasBranchDivergence;
    return BaseT::hasBranchDivergence(F);
  }
#else
  bool hasBranchDivergence() const {
    if (Opts.HasBranchDivergence) return *Opts.HasBranchDivergence;
    return BaseT::hasBranchDivergence();
  }
#endif

  // `isSingleThreaded` was added to the CRTP base in LLVM 16.
#if LLVM_VERSION_MAJOR >= 16
  bool isSingleThreaded() const {
    if (Opts.IsSingleThreaded) return *Opts.IsSingleThreaded;
    return BaseT::isSingleThreaded();
  }
#endif

  bool isSourceOfDivergence(const Value *V) const {
    if (Opts.IsSourceOfDivergence)
      return Opts.IsSourceOfDivergence(wrap(V),
                                       Opts.IsSourceOfDivergenceUD) != 0;
    return BaseT::isSourceOfDivergence(V);
  }

  bool isAlwaysUniform(const Value *V) const {
    if (Opts.IsAlwaysUniform)
      return Opts.IsAlwaysUniform(wrap(V), Opts.IsAlwaysUniformUD) != 0;
    return BaseT::isAlwaysUniform(V);
  }

  unsigned getAssumedAddrSpace(const Value *V) const {
    if (Opts.GetAssumedAddressSpace)
      return Opts.GetAssumedAddressSpace(wrap(V),
                                         Opts.GetAssumedAddressSpaceUD);
    return BaseT::getAssumedAddrSpace(V);
  }

  std::pair<const Value *, unsigned>
  getPredicatedAddrSpace(const Value *V) const {
    if (Opts.GetPredicatedAddressSpace) {
      LLVMValueRef Predicate = nullptr;
      unsigned AS = Opts.GetPredicatedAddressSpace(
          wrap(V), &Predicate, Opts.GetPredicatedAddressSpaceUD);
      return {unwrap(Predicate), AS};
    }
    return BaseT::getPredicatedAddrSpace(V);
  }

  Value *rewriteIntrinsicWithAddressSpace(IntrinsicInst *II, Value *OldV,
                                          Value *NewV) const {
    if (Opts.RewriteIntrinsicWithAS) {
      LLVMValueRef Result = Opts.RewriteIntrinsicWithAS(
          wrap(static_cast<Value *>(II)), wrap(OldV), wrap(NewV),
          Opts.RewriteIntrinsicWithASUD);
      return unwrap(Result);
    }
    return BaseT::rewriteIntrinsicWithAddressSpace(II, OldV, NewV);
  }

  bool collectFlatAddressOperands(SmallVectorImpl<int> &OpIndexes,
                                  Intrinsic::ID IID) const {
    if (Opts.CollectFlatAddressOperands) {
      int Buf[CollectFlatAddressOperandsBufSize];
      unsigned Count = 0;
      LLVMBool Any = Opts.CollectFlatAddressOperands(
          static_cast<unsigned>(IID), Buf,
          CollectFlatAddressOperandsBufSize, &Count,
          Opts.CollectFlatAddressOperandsUD);
      if (!Any)
        return false;
      if (Count > CollectFlatAddressOperandsBufSize)
        Count = CollectFlatAddressOperandsBufSize;
      OpIndexes.append(Buf, Buf + Count);
      return Count > 0;
    }
    return BaseT::collectFlatAddressOperands(OpIndexes, IID);
  }

private:
  LLVMTTIOptions Opts;
};
} // namespace

static TargetMachine *unwrap(LLVMTargetMachineRef P) {
  return reinterpret_cast<TargetMachine *>(P);
}

// Extension object

namespace llvm {

// Keep this in sync with PassBuilderBindings.cpp!
class LLVMPassBuilderOptions {
public:
  bool DebugLogging;
  bool VerifyEach;
#if LLVM_VERSION_MAJOR >= 20
  const char *AAPipeline;
#endif
  PipelineTuningOptions PTO;
};
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(LLVMPassBuilderOptions, LLVMPassBuilderOptionsRef)

class LLVMPassBuilderExtensions {
public:
  // A callback to register additional pipeline parsing callbacks with the pass builder.
  // This is used to support Julia's passes.
  SmallVector<std::function<void(void*)>> RegistrationCallbacks;

  // A list of callbacks that each register a single custom module or function pass.
  // These callbacks are generated here in C++, and match against a pass name.
  // This is used to enable custom LLVM passes implemented in Julia.
  SmallVector<std::function<bool(StringRef, ModulePassManager &,
                                 ArrayRef<PassBuilder::PipelineElement>)>,
              2>
      ModulePipelineParsingCallbacks;
  SmallVector<std::function<bool(StringRef, FunctionPassManager &,
                                 ArrayRef<PassBuilder::PipelineElement>)>,
              2>
      FunctionPipelineParsingCallbacks;

#if LLVM_VERSION_MAJOR < 20
  // A pipeline describing the alias analysis passes to run.
  const char *AAPipeline;
#endif

  // If set, passes requesting TargetTransformInfo will see this custom TTI
  // instead of the default one derived from the (possibly-missing) TargetMachine.
  std::optional<LLVMTTIOptions> TTI;
};
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(LLVMPassBuilderExtensions, LLVMPassBuilderExtensionsRef)
} // namespace llvm

LLVMPassBuilderExtensionsRef LLVMCreatePassBuilderExtensions() {
  return wrap(new LLVMPassBuilderExtensions());
}

void LLVMDisposePassBuilderExtensions(LLVMPassBuilderExtensionsRef Extensions) {
  delete unwrap(Extensions);
}


// Pass registration

void LLVMPassBuilderExtensionsPushRegistrationCallbacks(
    LLVMPassBuilderExtensionsRef Extensions, void (*RegistrationCallback)(void *)) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->RegistrationCallbacks.push_back(RegistrationCallback);
  return;
}


// Custom passes

struct JuliaCustomModulePass : llvm::PassInfoMixin<JuliaCustomModulePass> {
  LLVMJuliaModulePassCallback Callback;
  void *Thunk;
  JuliaCustomModulePass(LLVMJuliaModulePassCallback Callback, void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
    auto changed = Callback(wrap(&M), Thunk);
    return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
  }
};

struct JuliaCustomFunctionPass : llvm::PassInfoMixin<JuliaCustomFunctionPass> {
  LLVMJuliaFunctionPassCallback Callback;
  void *Thunk;
  JuliaCustomFunctionPass(LLVMJuliaFunctionPassCallback Callback, void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &) {
    auto changed = Callback(wrap(&F), Thunk);
    return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
  }
};

void LLVMPassBuilderExtensionsRegisterModulePass(LLVMPassBuilderExtensionsRef Extensions,
                                                 const char *PassName,
                                                 LLVMJuliaModulePassCallback Callback,
                                                 void *Thunk) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->ModulePipelineParsingCallbacks.push_back(
      [PassName, Callback, Thunk](StringRef Name, ModulePassManager &PM,
                                  ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name.consume_front(PassName)) {
          PM.addPass(JuliaCustomModulePass(Callback, Thunk));
          return true;
        }
        return false;
      });
  return;
}

void LLVMPassBuilderExtensionsRegisterFunctionPass(LLVMPassBuilderExtensionsRef Extensions,
                                                   const char *PassName,
                                                   LLVMJuliaFunctionPassCallback Callback,
                                                   void *Thunk) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->FunctionPipelineParsingCallbacks.push_back(
      [PassName, Callback, Thunk](StringRef Name, FunctionPassManager &PM,
                                  ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name.consume_front(PassName)) {
          PM.addPass(JuliaCustomFunctionPass(Callback, Thunk));
          return true;
        }
        return false;
      });
  return;
}

// Alias analysis pipeline (back-port of llvm/llvm-project#102482)

#if LLVM_VERSION_MAJOR < 20
void LLVMPassBuilderExtensionsSetAAPipeline(LLVMPassBuilderExtensionsRef Extensions,
                                            const char *AAPipeline) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->AAPipeline = AAPipeline;
  return;
}
#endif

// Custom TargetTransformInfo

LLVMTTIOptionsRef LLVMCreateTTIOptions() {
  return wrap(new LLVMTTIOptions());
}

void LLVMDisposeTTIOptions(LLVMTTIOptionsRef Options) {
  delete unwrap(Options);
}

void LLVMTTIOptionsSetFlatAddressSpace(LLVMTTIOptionsRef Options, unsigned AS) {
  unwrap(Options)->FlatAddressSpace = AS;
}
void LLVMTTIOptionsSetHasBranchDivergence(LLVMTTIOptionsRef Options,
                                          LLVMBool Value) {
  unwrap(Options)->HasBranchDivergence = (Value != 0);
}
void LLVMTTIOptionsSetIsSingleThreaded(LLVMTTIOptionsRef Options,
                                       LLVMBool Value) {
  unwrap(Options)->IsSingleThreaded = (Value != 0);
}
void LLVMTTIOptionsSetIsNoopAddrSpaceCast(LLVMTTIOptionsRef Options,
                                          LLVMTTIASPairPredicateFn Callback,
                                          void *UserData) {
  unwrap(Options)->IsNoopAddrSpaceCast = Callback;
  unwrap(Options)->IsNoopAddrSpaceCastUD = UserData;
}
void LLVMTTIOptionsSetIsValidAddrSpaceCast(LLVMTTIOptionsRef Options,
                                           LLVMTTIASPairPredicateFn Callback,
                                           void *UserData) {
  unwrap(Options)->IsValidAddrSpaceCast = Callback;
  unwrap(Options)->IsValidAddrSpaceCastUD = UserData;
}
void LLVMTTIOptionsSetAddrSpacesMayAlias(LLVMTTIOptionsRef Options,
                                         LLVMTTIASPairPredicateFn Callback,
                                         void *UserData) {
  unwrap(Options)->AddrSpacesMayAlias = Callback;
  unwrap(Options)->AddrSpacesMayAliasUD = UserData;
}
void LLVMTTIOptionsSetCanHaveGlobalInitializerInAS(
    LLVMTTIOptionsRef Options, LLVMTTIASPredicateFn Callback, void *UserData) {
  unwrap(Options)->CanHaveGlobalInitializerInAS = Callback;
  unwrap(Options)->CanHaveGlobalInitializerInASUD = UserData;
}
void LLVMTTIOptionsSetIsSourceOfDivergence(LLVMTTIOptionsRef Options,
                                           LLVMTTIValuePredicateFn Callback,
                                           void *UserData) {
  unwrap(Options)->IsSourceOfDivergence = Callback;
  unwrap(Options)->IsSourceOfDivergenceUD = UserData;
}
void LLVMTTIOptionsSetIsAlwaysUniform(LLVMTTIOptionsRef Options,
                                      LLVMTTIValuePredicateFn Callback,
                                      void *UserData) {
  unwrap(Options)->IsAlwaysUniform = Callback;
  unwrap(Options)->IsAlwaysUniformUD = UserData;
}
void LLVMTTIOptionsSetGetAssumedAddressSpace(
    LLVMTTIOptionsRef Options, LLVMTTIGetAssumedAddressSpaceFn Callback,
    void *UserData) {
  unwrap(Options)->GetAssumedAddressSpace = Callback;
  unwrap(Options)->GetAssumedAddressSpaceUD = UserData;
}
void LLVMTTIOptionsSetGetPredicatedAddressSpace(
    LLVMTTIOptionsRef Options, LLVMTTIGetPredicatedAddressSpaceFn Callback,
    void *UserData) {
  unwrap(Options)->GetPredicatedAddressSpace = Callback;
  unwrap(Options)->GetPredicatedAddressSpaceUD = UserData;
}
void LLVMTTIOptionsSetRewriteIntrinsicWithAS(LLVMTTIOptionsRef Options,
                                             LLVMTTIRewriteIntrinsicFn Callback,
                                             void *UserData) {
  unwrap(Options)->RewriteIntrinsicWithAS = Callback;
  unwrap(Options)->RewriteIntrinsicWithASUD = UserData;
}
void LLVMTTIOptionsSetCollectFlatAddressOperands(
    LLVMTTIOptionsRef Options, LLVMTTICollectFlatAddressOperandsFn Callback,
    void *UserData) {
  unwrap(Options)->CollectFlatAddressOperands = Callback;
  unwrap(Options)->CollectFlatAddressOperandsUD = UserData;
}

void LLVMPassBuilderExtensionsSetTTI(LLVMPassBuilderExtensionsRef Extensions,
                                     LLVMTTIOptionsRef Options) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  if (Options)
    PassExts->TTI = *unwrap(Options);
  else
    PassExts->TTI.reset();
}


// Vendored API entrypoint

static LLVMErrorRef runJuliaPasses(Module *Mod, Function *Fun, const char *Passes,
                                   TargetMachine *Machine, LLVMPassBuilderOptions *PassOpts,
                                   LLVMPassBuilderExtensions *PassExts) {
  bool Debug = PassOpts->DebugLogging;
  bool VerifyEach = PassOpts->VerifyEach;

  PassInstrumentationCallbacks PIC;
#if LLVM_VERSION_MAJOR >= 16
  PassBuilder PB(Machine, PassOpts->PTO, std::nullopt, &PIC);
#else
  PassBuilder PB(Machine, PassOpts->PTO, None, &PIC);
#endif

  for (auto &Callback : PassExts->RegistrationCallbacks)
    Callback(&PB);
  for (auto &Callback : PassExts->ModulePipelineParsingCallbacks)
    PB.registerPipelineParsingCallback(Callback);
  for (auto &Callback : PassExts->FunctionPipelineParsingCallbacks)
    PB.registerPipelineParsingCallback(Callback);

  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;

  // If a custom TTI was requested, register it with FAM *before*
  // PB.registerFunctionAnalyses, so our TargetIRAnalysis wins over the default
  // one (which would otherwise be derived from the TargetMachine, if any).
  if (PassExts->TTI) {
    auto Opts = *PassExts->TTI;
    FAM.registerPass([Opts] {
      return TargetIRAnalysis([Opts](const Function &F) {
        return TargetTransformInfo(
            CustomTargetTransformInfo(F.getParent()->getDataLayout(), Opts));
      });
    });
  }
  const char *AAPipeline =
#if LLVM_VERSION_MAJOR >= 20
    PassOpts->AAPipeline;
#else
    PassExts->AAPipeline;
#endif
  if (AAPipeline) {
    // If we have a custom AA pipeline, we need to register it _before_ calling
    // registerFunctionAnalyses, or the default alias analysis pipeline is used.
    AAManager AA;
    if (auto Err = PB.parseAAPipeline(AA, AAPipeline)) {
      return wrap(std::move(Err));
    }
    FAM.registerPass([&] { return std::move(AA); });
  }
  PB.registerLoopAnalyses(LAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerModuleAnalyses(MAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

#if LLVM_VERSION_MAJOR >= 16
  StandardInstrumentations SI(Mod->getContext(), Debug, VerifyEach);
#else
  StandardInstrumentations SI(Debug, VerifyEach);
#endif
#if LLVM_VERSION_MAJOR >= 17
  SI.registerCallbacks(PIC, &MAM);
#else
  SI.registerCallbacks(PIC, &FAM);
#endif

  if (Fun) {
    FunctionPassManager FPM;
    if (VerifyEach)
      FPM.addPass(VerifierPass());
    if (auto Err = PB.parsePassPipeline(FPM, Passes))
      return wrap(std::move(Err));
    FPM.run(*Fun, FAM);
  } else {
    ModulePassManager MPM;
    if (VerifyEach)
      MPM.addPass(VerifierPass());
    if (auto Err = PB.parsePassPipeline(MPM, Passes))
      return wrap(std::move(Err));
    MPM.run(*Mod, MAM);
  }

  return LLVMErrorSuccess;
}

LLVMErrorRef LLVMRunJuliaPasses(LLVMModuleRef M, const char *Passes,
                                LLVMTargetMachineRef TM, LLVMPassBuilderOptionsRef Options,
                                LLVMPassBuilderExtensionsRef Extensions) {
  TargetMachine *Machine = unwrap(TM);
  LLVMPassBuilderOptions *PassOpts = unwrap(Options);
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  Module *Mod = unwrap(M);
  return runJuliaPasses(Mod, nullptr, Passes, Machine, PassOpts, PassExts);
}

LLVMErrorRef LLVMRunJuliaPassesOnFunction(LLVMValueRef F, const char *Passes,
                                          LLVMTargetMachineRef TM,
                                          LLVMPassBuilderOptionsRef Options,
                                          LLVMPassBuilderExtensionsRef Extensions) {
  TargetMachine *Machine = unwrap(TM);
  LLVMPassBuilderOptions *PassOpts = unwrap(Options);
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  Function *Fun = unwrap<Function>(F);
  return runJuliaPasses(Fun->getParent(), Fun, Passes, Machine, PassOpts, PassExts);
}
