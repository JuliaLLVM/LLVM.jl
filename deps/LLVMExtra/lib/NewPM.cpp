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

// Minimal TargetTransformInfo for pipelines that don't have a TargetMachine.
// Wraps an `LLVMTTIOptions` POD struct (see LLVMExtra.h) and falls back
// to the default `TargetTransformInfoImplCRTPBase` behavior for any field
// left at its sentinel value.
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

  unsigned getFlatAddressSpace() const { return Opts.FlatAddressSpace; }

  bool isNoopAddrSpaceCast(unsigned FromAS, unsigned ToAS) const {
    if (Opts.IsNoopAddrSpaceCast)
      return Opts.IsNoopAddrSpaceCast(FromAS, ToAS, Opts.UserData) != 0;
    return BaseT::isNoopAddrSpaceCast(FromAS, ToAS);
  }

  bool canHaveNonUndefGlobalInitializerInAddressSpace(unsigned AS) const {
    if (Opts.CanHaveGlobalInitializerInAS)
      return Opts.CanHaveGlobalInitializerInAS(AS, Opts.UserData) != 0;
    return BaseT::canHaveNonUndefGlobalInitializerInAddressSpace(AS);
  }

  // `isValidAddrSpaceCast` and `addrspacesMayAlias` were added in LLVM 17.
  // On earlier versions the pass framework doesn't query them, so we skip
  // providing the override entirely.
#if LLVM_VERSION_MAJOR >= 17
  bool isValidAddrSpaceCast(unsigned FromAS, unsigned ToAS) const {
    if (Opts.IsValidAddrSpaceCast)
      return Opts.IsValidAddrSpaceCast(FromAS, ToAS, Opts.UserData) != 0;
    return BaseT::isValidAddrSpaceCast(FromAS, ToAS);
  }

  bool addrspacesMayAlias(unsigned AS0, unsigned AS1) const {
    if (Opts.AddrSpacesMayAlias)
      return Opts.AddrSpacesMayAlias(AS0, AS1, Opts.UserData) != 0;
    return BaseT::addrspacesMayAlias(AS0, AS1);
  }
#endif

  // `hasBranchDivergence` grew a `Function*` parameter in LLVM 17.
#if LLVM_VERSION_MAJOR >= 17
  bool hasBranchDivergence(const Function *F = nullptr) const {
    if (Opts.HasBranchDivergence < 0) return BaseT::hasBranchDivergence(F);
    return Opts.HasBranchDivergence != 0;
  }
#else
  bool hasBranchDivergence() const {
    if (Opts.HasBranchDivergence < 0) return BaseT::hasBranchDivergence();
    return Opts.HasBranchDivergence != 0;
  }
#endif

  // `isSingleThreaded` was added to the CRTP base in LLVM 16.
#if LLVM_VERSION_MAJOR >= 16
  bool isSingleThreaded() const {
    if (Opts.IsSingleThreaded < 0) return BaseT::isSingleThreaded();
    return Opts.IsSingleThreaded != 0;
  }
#endif

  bool isSourceOfDivergence(const Value *V) const {
    if (Opts.IsSourceOfDivergence)
      return Opts.IsSourceOfDivergence(wrap(V), Opts.UserData) != 0;
    return BaseT::isSourceOfDivergence(V);
  }

  bool isAlwaysUniform(const Value *V) const {
    if (Opts.IsAlwaysUniform)
      return Opts.IsAlwaysUniform(wrap(V), Opts.UserData) != 0;
    return BaseT::isAlwaysUniform(V);
  }

  unsigned getAssumedAddrSpace(const Value *V) const {
    if (Opts.GetAssumedAddressSpace)
      return Opts.GetAssumedAddressSpace(wrap(V), Opts.UserData);
    return BaseT::getAssumedAddrSpace(V);
  }

  std::pair<const Value *, unsigned>
  getPredicatedAddrSpace(const Value *V) const {
    if (Opts.GetPredicatedAddressSpace) {
      LLVMValueRef Predicate = nullptr;
      unsigned AS = Opts.GetPredicatedAddressSpace(wrap(V), &Predicate,
                                                   Opts.UserData);
      return {unwrap(Predicate), AS};
    }
    return BaseT::getPredicatedAddrSpace(V);
  }

  Value *rewriteIntrinsicWithAddressSpace(IntrinsicInst *II, Value *OldV,
                                          Value *NewV) const {
    if (Opts.RewriteIntrinsicWithAS) {
      LLVMValueRef Result = Opts.RewriteIntrinsicWithAS(
          wrap(static_cast<Value *>(II)), wrap(OldV), wrap(NewV),
          Opts.UserData);
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
          CollectFlatAddressOperandsBufSize, &Count, Opts.UserData);
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

void LLVMPassBuilderExtensionsSetTTI(
    LLVMPassBuilderExtensionsRef Extensions,
    const LLVMTTIOptions *Options) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  if (Options)
    PassExts->TTI = *Options;
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
