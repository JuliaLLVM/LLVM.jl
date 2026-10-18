#include "LLVMExtra.h"
#include <llvm/ADT/StringSet.h>

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/AssumptionCache.h>
#include <llvm/Analysis/LazyValueInfo.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetTransformInfoImpl.h>
#include <llvm/CodeGen/ExpandReductions.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Transforms/IPO/Internalize.h>
#include <llvm/Transforms/Vectorize/VectorCombine.h>
#include <llvm/Support/CBindingWrapping.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/Regex.h>
#include <llvm/Target/TargetMachine.h>

#include <memory>
#include <optional>

using namespace llvm;

// Backing struct for the opaque `LLVMTTIOptionsRef` handle. Unset scalar
// fields are `std::nullopt`; unset callbacks are `nullptr`. In both cases
// `CustomTargetTransformInfo` falls back to `TargetTransformInfoImplCRTPBase`.
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

// The CRTP base's methods went from non-virtual (template dispatch via
// `Model<T>`) to virtual in LLVM 21. Use `override` only where it's a
// real override; otherwise it's a compile error.
#if LLVM_VERSION_MAJOR >= 21
#define TTI_OVERRIDE override
#else
#define TTI_OVERRIDE
#endif

class CustomTargetTransformInfo final
    : public TargetTransformInfoImplCRTPBase<CustomTargetTransformInfo> {
  typedef TargetTransformInfoImplCRTPBase<CustomTargetTransformInfo> BaseT;

public:
  CustomTargetTransformInfo(const DataLayout &DL, LLVMTTIOptions Opts)
      : BaseT(DL), Opts(Opts) {}

  unsigned getFlatAddressSpace() const TTI_OVERRIDE {
    if (Opts.FlatAddressSpace) return *Opts.FlatAddressSpace;
    return BaseT::getFlatAddressSpace();
  }

  bool isNoopAddrSpaceCast(unsigned FromAS, unsigned ToAS) const TTI_OVERRIDE {
    if (Opts.IsNoopAddrSpaceCast)
      return Opts.IsNoopAddrSpaceCast(FromAS, ToAS,
                                      Opts.IsNoopAddrSpaceCastUD) != 0;
    return BaseT::isNoopAddrSpaceCast(FromAS, ToAS);
  }

  bool canHaveNonUndefGlobalInitializerInAddressSpace(unsigned AS) const TTI_OVERRIDE {
    if (Opts.CanHaveGlobalInitializerInAS)
      return Opts.CanHaveGlobalInitializerInAS(
                 AS, Opts.CanHaveGlobalInitializerInASUD) != 0;
    return BaseT::canHaveNonUndefGlobalInitializerInAddressSpace(AS);
  }

#if LLVM_VERSION_MAJOR >= 17
  bool isValidAddrSpaceCast(unsigned FromAS, unsigned ToAS) const TTI_OVERRIDE {
    if (Opts.IsValidAddrSpaceCast)
      return Opts.IsValidAddrSpaceCast(FromAS, ToAS,
                                       Opts.IsValidAddrSpaceCastUD) != 0;
    return BaseT::isValidAddrSpaceCast(FromAS, ToAS);
  }

  bool addrspacesMayAlias(unsigned AS0, unsigned AS1) const TTI_OVERRIDE {
    if (Opts.AddrSpacesMayAlias)
      return Opts.AddrSpacesMayAlias(AS0, AS1,
                                     Opts.AddrSpacesMayAliasUD) != 0;
    return BaseT::addrspacesMayAlias(AS0, AS1);
  }
#endif

  // `hasBranchDivergence` grew a `Function*` parameter in LLVM 17.
#if LLVM_VERSION_MAJOR >= 17
  bool hasBranchDivergence(const Function *F = nullptr) const TTI_OVERRIDE {
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
  bool isSingleThreaded() const TTI_OVERRIDE {
    if (Opts.IsSingleThreaded) return *Opts.IsSingleThreaded;
    return BaseT::isSingleThreaded();
  }
#endif

#if LLVM_VERSION_MAJOR >= 23
  ValueUniformity getValueUniformity(const Value *V) const override {
    if (Opts.IsSourceOfDivergence &&
        Opts.IsSourceOfDivergence(wrap(V), Opts.IsSourceOfDivergenceUD) != 0)
      return ValueUniformity::NeverUniform;
    if (Opts.IsAlwaysUniform && Opts.IsAlwaysUniform(wrap(V), Opts.IsAlwaysUniformUD) != 0)
      return ValueUniformity::AlwaysUniform;
    return BaseT::getValueUniformity(V);
  }
#elif LLVM_VERSION_MAJOR >= 22
  InstructionUniformity getInstructionUniformity(const Value *V) const override {
    if (Opts.IsSourceOfDivergence &&
        Opts.IsSourceOfDivergence(wrap(V), Opts.IsSourceOfDivergenceUD) != 0)
      return InstructionUniformity::NeverUniform;
    if (Opts.IsAlwaysUniform && Opts.IsAlwaysUniform(wrap(V), Opts.IsAlwaysUniformUD) != 0)
      return InstructionUniformity::AlwaysUniform;
    return BaseT::getInstructionUniformity(V);
  }
#else
  bool isSourceOfDivergence(const Value *V) const TTI_OVERRIDE {
    if (Opts.IsSourceOfDivergence)
      return Opts.IsSourceOfDivergence(wrap(V),
                                       Opts.IsSourceOfDivergenceUD) != 0;
    return BaseT::isSourceOfDivergence(V);
  }

  bool isAlwaysUniform(const Value *V) const TTI_OVERRIDE {
    if (Opts.IsAlwaysUniform)
      return Opts.IsAlwaysUniform(wrap(V), Opts.IsAlwaysUniformUD) != 0;
    return BaseT::isAlwaysUniform(V);
  }
#endif

  unsigned getAssumedAddrSpace(const Value *V) const TTI_OVERRIDE {
    if (Opts.GetAssumedAddressSpace)
      return Opts.GetAssumedAddressSpace(wrap(V),
                                         Opts.GetAssumedAddressSpaceUD);
    return BaseT::getAssumedAddrSpace(V);
  }

  std::pair<const Value *, unsigned>
  getPredicatedAddrSpace(const Value *V) const TTI_OVERRIDE {
    if (Opts.GetPredicatedAddressSpace) {
      LLVMValueRef Predicate = nullptr;
      unsigned AS = Opts.GetPredicatedAddressSpace(
          wrap(V), &Predicate, Opts.GetPredicatedAddressSpaceUD);
      return {unwrap(Predicate), AS};
    }
    return BaseT::getPredicatedAddrSpace(V);
  }

  Value *rewriteIntrinsicWithAddressSpace(IntrinsicInst *II, Value *OldV,
                                          Value *NewV) const TTI_OVERRIDE {
    if (Opts.RewriteIntrinsicWithAS) {
      LLVMValueRef Result = Opts.RewriteIntrinsicWithAS(
          wrap(static_cast<Value *>(II)), wrap(OldV), wrap(NewV),
          Opts.RewriteIntrinsicWithASUD);
      return unwrap(Result);
    }
    return BaseT::rewriteIntrinsicWithAddressSpace(II, OldV, NewV);
  }

  bool collectFlatAddressOperands(SmallVectorImpl<int> &OpIndexes,
                                  Intrinsic::ID IID) const TTI_OVERRIDE {
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

  // The names of the custom passes, which identify them in instrumentation.
  SmallVector<std::string, 2> CustomPassNames;

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

// A pass under a name of its own, rather than that of its C++ class. LLVM identifies passes
// by their class name in instrumentation (`-print-before`, `-time-passes`, `-opt-bisect-limit`
// and the like) and when printing a pipeline, so without this, every custom pass would be
// reported as the class wrapping it. `Name` identifies the pass in instrumentation, and is
// mapped to itself as pipeline name (see `registerExtensions`); `Text` is the pass as it
// was written in the pipeline, with its parameters, which is how it is printed.
template <typename PassT> struct NamedPass {
  std::string Name;
  std::string Text;
  bool Required;
  PassT Pass;
  NamedPass(StringRef Name, StringRef Text, bool Required, PassT Pass)
      : Name(Name), Text(Text), Required(Required), Pass(std::move(Pass)) {}

  // only used by the loop pass manager, to detect whether this is a loop or loop nest pass
  template <typename IRUnitT, typename AnalysisManagerT, typename... ExtraArgTs>
  auto run(IRUnitT &IR, AnalysisManagerT &AM, ExtraArgTs &&...ExtraArgs)
      -> decltype(Pass.run(IR, AM, std::forward<ExtraArgTs>(ExtraArgs)...)) {
    return Pass.run(IR, AM, std::forward<ExtraArgTs>(ExtraArgs)...);
  }
};
template <typename PassT>
NamedPass(StringRef, StringRef, bool, PassT) -> NamedPass<PassT>;

// The pass managers wrap passes in a `PassModel`, whose `name()` returns the static name
// of the pass class. Specialize it for named passes, to return the name of the instance.
namespace llvm {
namespace detail {
#if LLVM_VERSION_MAJOR >= 19
template <typename IRUnitT, typename PassT, typename AnalysisManagerT,
          typename... ExtraArgTs>
struct PassModel<IRUnitT, NamedPass<PassT>, AnalysisManagerT, ExtraArgTs...>
#else
template <typename IRUnitT, typename PassT, typename PreservedAnalysesT,
          typename AnalysisManagerT, typename... ExtraArgTs>
struct PassModel<IRUnitT, NamedPass<PassT>, PreservedAnalysesT, AnalysisManagerT,
                 ExtraArgTs...>
#endif
    : PassConcept<IRUnitT, AnalysisManagerT, ExtraArgTs...> {
  explicit PassModel(NamedPass<PassT> Pass) : Pass(std::move(Pass)) {}

  PreservedAnalyses run(IRUnitT &IR, AnalysisManagerT &AM,
                        ExtraArgTs... ExtraArgs) override {
    return Pass.Pass.run(IR, AM, ExtraArgs...);
  }

  void printPipeline(raw_ostream &OS, function_ref<StringRef(StringRef)>) override {
    OS << Pass.Text;
  }

  StringRef name() const override { return Pass.Name; }

  bool isRequired() const override { return Pass.Required; }

  NamedPass<PassT> Pass;
};
} // namespace detail
} // namespace llvm

struct JuliaCustomModulePass {
  LLVMJuliaModulePassCallback Callback;
  void *Thunk;
  JuliaCustomModulePass(LLVMJuliaModulePassCallback Callback, void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
    auto changed = Callback(wrap(&M), Thunk);
    return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
  }
};

struct JuliaCustomFunctionPass {
  LLVMJuliaFunctionPassCallback Callback;
  void *Thunk;
  JuliaCustomFunctionPass(LLVMJuliaFunctionPassCallback Callback, void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &) {
    auto changed = Callback(wrap(&F), Thunk);
    return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
  }
};

void LLVMPassBuilderExtensionsRegisterModulePassWithRequired(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaModulePassCallback Callback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->ModulePipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, Thunk, Required](StringRef Name, ModulePassManager &PM,
                                  ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name == PassName && Pipeline.empty()) {
          PM.addPass(NamedPass(PassName, Name, Required,
                               JuliaCustomModulePass(Callback, Thunk)));
          return true;
        }
        return false;
      });
  return;
}

void LLVMPassBuilderExtensionsRegisterModulePass(LLVMPassBuilderExtensionsRef Extensions,
                                                 const char *PassName,
                                                 LLVMJuliaModulePassCallback Callback,
                                                 void *Thunk) {
  LLVMPassBuilderExtensionsRegisterModulePassWithRequired(Extensions, PassName, Callback,
                                                          Thunk, false);
}

void LLVMPassBuilderExtensionsRegisterFunctionPassWithRequired(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaFunctionPassCallback Callback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->FunctionPipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, Thunk, Required](StringRef Name, FunctionPassManager &PM,
                                                   ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name == PassName && Pipeline.empty()) {
          PM.addPass(NamedPass(PassName, Name, Required,
                               JuliaCustomFunctionPass(Callback, Thunk)));
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
  LLVMPassBuilderExtensionsRegisterFunctionPassWithRequired(Extensions, PassName, Callback,
                                                            Thunk, false);
}

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(FunctionAnalysisManager, LLVMFunctionAnalysisManagerRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(PreservedAnalyses, LLVMPreservedAnalysesRef)

struct JuliaCustomFunctionPassWithAnalyses {
  LLVMJuliaFunctionPassWithAnalysesCallback Callback;
  void *Thunk;
  JuliaCustomFunctionPassWithAnalyses(LLVMJuliaFunctionPassWithAnalysesCallback Callback,
                                      void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM) {
    auto PA = llvm::PreservedAnalyses::none();
    Callback(wrap(&F), wrap(&AM), wrap(&PA), Thunk);
    return PA;
  }
};

void LLVMExtraPassBuilderExtensionsRegisterFunctionPassWithAnalyses(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaFunctionPassWithAnalysesCallback Callback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->FunctionPipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, Thunk,
       Required](StringRef Name, FunctionPassManager &PM,
                 ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name == PassName && Pipeline.empty()) {
          PM.addPass(NamedPass(PassName, Name, Required,
                               JuliaCustomFunctionPassWithAnalyses(Callback, Thunk)));
          return true;
        }
        return false;
      });
}

void *LLVMExtraFunctionAnalysisManagerGetResult(LLVMFunctionAnalysisManagerRef AM,
                                                LLVMValueRef F,
                                                LLVMExtraFunctionAnalysis Analysis) {
  FunctionAnalysisManager &FAM = *unwrap(AM);
  Function &Fn = *unwrap<Function>(F);
  switch (Analysis) {
  case LLVMExtraDominatorTreeAnalysis:
    return &FAM.getResult<DominatorTreeAnalysis>(Fn);
  case LLVMExtraPostDominatorTreeAnalysis:
    return &FAM.getResult<PostDominatorTreeAnalysis>(Fn);
  case LLVMExtraAssumptionAnalysis:
    return &FAM.getResult<AssumptionAnalysis>(Fn);
  case LLVMExtraLazyValueAnalysis:
    return &FAM.getResult<LazyValueAnalysis>(Fn);
  case LLVMExtraScalarEvolutionAnalysis:
    return &FAM.getResult<ScalarEvolutionAnalysis>(Fn);
  case LLVMExtraLoopAnalysis:
    return &FAM.getResult<LoopAnalysis>(Fn);
  }
  llvm_unreachable("unknown function analysis");
}

void *LLVMExtraFunctionAnalysisManagerGetCachedResult(LLVMFunctionAnalysisManagerRef AM,
                                                      LLVMValueRef F,
                                                      LLVMExtraFunctionAnalysis Analysis) {
  FunctionAnalysisManager &FAM = *unwrap(AM);
  Function &Fn = *unwrap<Function>(F);
  switch (Analysis) {
  case LLVMExtraDominatorTreeAnalysis:
    return FAM.getCachedResult<DominatorTreeAnalysis>(Fn);
  case LLVMExtraPostDominatorTreeAnalysis:
    return FAM.getCachedResult<PostDominatorTreeAnalysis>(Fn);
  case LLVMExtraAssumptionAnalysis:
    return FAM.getCachedResult<AssumptionAnalysis>(Fn);
  case LLVMExtraLazyValueAnalysis:
    return FAM.getCachedResult<LazyValueAnalysis>(Fn);
  case LLVMExtraScalarEvolutionAnalysis:
    return FAM.getCachedResult<ScalarEvolutionAnalysis>(Fn);
  case LLVMExtraLoopAnalysis:
    return FAM.getCachedResult<LoopAnalysis>(Fn);
  }
  llvm_unreachable("unknown function analysis");
}

static PreservedAnalyses buildPreservedAnalyses(LLVMBool All, LLVMBool Functions,
                                                LLVMBool CFG,
                                                const LLVMExtraFunctionAnalysis *Analyses,
                                                unsigned NumAnalyses) {
  if (All)
    return PreservedAnalyses::all();
  PreservedAnalyses PA;
  if (Functions)
    PA.preserveSet<AllAnalysesOn<Function>>();
  if (CFG)
    PA.preserveSet<CFGAnalyses>();
  for (unsigned I = 0; I < NumAnalyses; ++I) {
    switch (Analyses[I]) {
    case LLVMExtraDominatorTreeAnalysis:
      PA.preserve<DominatorTreeAnalysis>();
      break;
    case LLVMExtraPostDominatorTreeAnalysis:
      PA.preserve<PostDominatorTreeAnalysis>();
      break;
    case LLVMExtraAssumptionAnalysis:
      PA.preserve<AssumptionAnalysis>();
      break;
    case LLVMExtraLazyValueAnalysis:
      PA.preserve<LazyValueAnalysis>();
      break;
    case LLVMExtraScalarEvolutionAnalysis:
      PA.preserve<ScalarEvolutionAnalysis>();
      break;
    case LLVMExtraLoopAnalysis:
      PA.preserve<LoopAnalysis>();
      break;
    }
  }
  return PA;
}

void LLVMExtraSetPreservedAnalyses(LLVMPreservedAnalysesRef PA, LLVMBool All,
                                   LLVMBool Functions, LLVMBool CFG,
                                   const LLVMExtraFunctionAnalysis *Analyses,
                                   unsigned NumAnalyses) {
  *unwrap(PA) = buildPreservedAnalyses(All, Functions, CFG, Analyses, NumAnalyses);
}

void LLVMExtraGetPreservedAnalyses(LLVMPreservedAnalysesRef PA, LLVMBool *All,
                                   LLVMBool *Functions, LLVMBool *CFG) {
  *All = unwrap(PA)->areAllPreserved();
  *Functions = unwrap(PA)->allAnalysesInSetPreserved<AllAnalysesOn<Function>>();
  *CFG = unwrap(PA)->allAnalysesInSetPreserved<CFGAnalyses>();
}

template <typename AnalysisT> static bool isPreserved(const PreservedAnalyses &PA) {
  return PA.getChecker<AnalysisT>().preserved();
}

LLVMBool LLVMExtraPreservedAnalysesIsPreserved(LLVMPreservedAnalysesRef PA,
                                               LLVMExtraFunctionAnalysis Analysis) {
  switch (Analysis) {
  case LLVMExtraDominatorTreeAnalysis:
    return isPreserved<DominatorTreeAnalysis>(*unwrap(PA));
  case LLVMExtraPostDominatorTreeAnalysis:
    return isPreserved<PostDominatorTreeAnalysis>(*unwrap(PA));
  case LLVMExtraAssumptionAnalysis:
    return isPreserved<AssumptionAnalysis>(*unwrap(PA));
  case LLVMExtraLazyValueAnalysis:
    return isPreserved<LazyValueAnalysis>(*unwrap(PA));
  case LLVMExtraScalarEvolutionAnalysis:
    return isPreserved<ScalarEvolutionAnalysis>(*unwrap(PA));
  case LLVMExtraLoopAnalysis:
    return isPreserved<LoopAnalysis>(*unwrap(PA));
  }
  llvm_unreachable("unknown function analysis");
}

void LLVMExtraDisposePreservedAnalyses(LLVMPreservedAnalysesRef PA) { delete unwrap(PA); }

void LLVMExtraFunctionAnalysisManagerInvalidate(LLVMFunctionAnalysisManagerRef AM,
                                                LLVMValueRef F, LLVMBool All,
                                                LLVMBool Functions, LLVMBool CFG,
                                                const LLVMExtraFunctionAnalysis *Analyses,
                                                unsigned NumAnalyses) {
  unwrap(AM)->invalidate(*unwrap<Function>(F),
                         buildPreservedAnalyses(All, Functions, CFG, Analyses, NumAnalyses));
}


void LLVMExtraFunctionAnalysisManagerClear(LLVMFunctionAnalysisManagerRef AM,
                                           LLVMValueRef F) {
  Function &Fn = *unwrap<Function>(F);
  unwrap(AM)->clear(Fn, Fn.getName());
}

void LLVMExtraFunctionAnalysisManagerEmitRemark(LLVMFunctionAnalysisManagerRef AM,
                                                LLVMExtraRemarkKind Kind,
                                                const char *PassName,
                                                const char *RemarkName, LLVMValueRef Inst,
                                                const char *Message) {
  Instruction *I = unwrap<Instruction>(Inst);
  auto &ORE = unwrap(AM)->getResult<OptimizationRemarkEmitterAnalysis>(*I->getFunction());
  // remarks keep a pointer to the pass name, and are passed to the diagnostic handler
  // before `emit` returns
  switch (Kind) {
  case LLVMExtraRemarkPassed: {
    OptimizationRemark R(PassName, RemarkName, I);
    R << Message;
    ORE.emit(R);
    break;
  }
  case LLVMExtraRemarkMissed: {
    OptimizationRemarkMissed R(PassName, RemarkName, I);
    R << Message;
    ORE.emit(R);
    break;
  }
  case LLVMExtraRemarkAnalysis: {
    OptimizationRemarkAnalysis R(PassName, RemarkName, I);
    R << Message;
    ORE.emit(R);
    break;
  }
  }
}

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(ModuleAnalysisManager, LLVMModuleAnalysisManagerRef)

struct JuliaCustomModulePassWithAnalyses {
  LLVMJuliaModulePassWithAnalysesCallback Callback;
  void *Thunk;
  JuliaCustomModulePassWithAnalyses(LLVMJuliaModulePassWithAnalysesCallback Callback,
                                    void *Thunk)
      : Callback(Callback), Thunk(Thunk) {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM) {
    auto PA = llvm::PreservedAnalyses::none();
    Callback(wrap(&M), wrap(&AM), wrap(&PA), Thunk);
    return PA;
  }
};

void LLVMExtraPassBuilderExtensionsRegisterModulePassWithAnalyses(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaModulePassWithAnalysesCallback Callback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->ModulePipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, Thunk,
       Required](StringRef Name, ModulePassManager &PM,
                 ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        if (Name == PassName && Pipeline.empty()) {
          PM.addPass(NamedPass(PassName, Name, Required,
                               JuliaCustomModulePassWithAnalyses(Callback, Thunk)));
          return true;
        }
        return false;
      });
}

LLVMFunctionAnalysisManagerRef
LLVMExtraModuleAnalysisManagerGetFunctionAnalysisManager(LLVMModuleAnalysisManagerRef AM,
                                                         LLVMModuleRef M) {
  ModuleAnalysisManager &MAM = *unwrap(AM);
  return wrap(&MAM.getResult<FunctionAnalysisManagerModuleProxy>(*unwrap(M)).getManager());
}

void LLVMExtraSetModulePreservedAnalyses(LLVMPreservedAnalysesRef PA, LLVMBool All,
                                         LLVMBool Functions, LLVMBool CFG,
                                         const LLVMExtraFunctionAnalysis *Analyses,
                                         unsigned NumAnalyses) {
  if (All) {
    *unwrap(PA) = PreservedAnalyses::all();
    return;
  }
  if (!Functions && !CFG && NumAnalyses == 0) {
    // without the proxy, every cached function analysis is cleared
    *unwrap(PA) = PreservedAnalyses::none();
    return;
  }
  // preserving the proxy makes the module pass manager invalidate each function's results
  // with this set, instead of clearing all of them
  PreservedAnalyses Result =
      buildPreservedAnalyses(false, Functions, CFG, Analyses, NumAnalyses);
  Result.preserve<FunctionAnalysisManagerModuleProxy>();
  *unwrap(PA) = std::move(Result);
}

// Match `Name` against a pass that takes parameters, i.e., `PassName` or
// `PassName<Parameters>`. Like PassBuilder::checkParametrizedPassName, which is only public
// since LLVM 19.
static bool matchParametrizedPassName(StringRef Name, StringRef PassName,
                                      StringRef &Parameters) {
  if (!Name.consume_front(PassName))
    return false;
  if (Name.empty()) {
    Parameters = Name;
    return true;
  }
  if (!Name.consume_front("<") || !Name.consume_back(">"))
    return false;
  Parameters = Name;
  return true;
}

void LLVMExtraPassBuilderExtensionsRegisterParametrizedModulePass(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaModulePassWithAnalysesCallback Callback,
    LLVMJuliaPassParametersCallback ParametersCallback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->ModulePipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, ParametersCallback, Thunk,
       Required](StringRef Name, ModulePassManager &PM,
                 ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        StringRef Parameters;
        if (!Pipeline.empty() || !matchParametrizedPassName(Name, PassName, Parameters))
          return false;
        void *PassThunk = ParametersCallback(Parameters.str().c_str(), Thunk);
        if (!PassThunk)
          return false;
        PM.addPass(NamedPass(PassName, Name, Required,
                             JuliaCustomModulePassWithAnalyses(Callback, PassThunk)));
        return true;
      });
}

void LLVMExtraPassBuilderExtensionsRegisterParametrizedFunctionPass(
    LLVMPassBuilderExtensionsRef Extensions, const char *PassName,
    LLVMJuliaFunctionPassWithAnalysesCallback Callback,
    LLVMJuliaPassParametersCallback ParametersCallback, void *Thunk, LLVMBool Required) {
  LLVMPassBuilderExtensions *PassExts = unwrap(Extensions);
  PassExts->CustomPassNames.push_back(PassName);
  PassExts->FunctionPipelineParsingCallbacks.push_back(
      [PassName = std::string(PassName), Callback, ParametersCallback, Thunk,
       Required](StringRef Name, FunctionPassManager &PM,
                 ArrayRef<PassBuilder::PipelineElement> Pipeline) {
        StringRef Parameters;
        if (!Pipeline.empty() || !matchParametrizedPassName(Name, PassName, Parameters))
          return false;
        void *PassThunk = ParametersCallback(Parameters.str().c_str(), Thunk);
        if (!PassThunk)
          return false;
        PM.addPass(NamedPass(PassName, Name, Required,
                             JuliaCustomFunctionPassWithAnalyses(Callback, PassThunk)));
        return true;
      });
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

// Pipeline extension-point callbacks as named pseudo-passes (back-port of
// llvm/llvm-project#157153, merged for LLVM 22). These let string-API
// pipelines invoke the callbacks registered at each EP — mirroring what the
// default pipelines do — by writing e.g. `pipeline-start-callbacks<O2>`.

#if LLVM_VERSION_MAJOR >= 17 && LLVM_VERSION_MAJOR < 22
namespace {

bool checkParametrizedPassName(StringRef Name, StringRef PassName) {
  if (!Name.consume_front(PassName))
    return false;
  // bare name == default (empty) parameter list
  if (Name.empty())
    return true;
  return Name.starts_with("<") && Name.ends_with(">");
}

Expected<OptimizationLevel> parseOptLevelPass(StringRef Name,
                                              StringRef PassName) {
  if (!Name.consume_front(PassName))
    llvm_unreachable("pass name was already verified");
  if (!Name.empty() &&
      !(Name.consume_front("<") && Name.consume_back(">")))
    llvm_unreachable("invalid format for parametrized pass name");

  auto Level = StringSwitch<std::optional<OptimizationLevel>>(Name)
                   .Case("", OptimizationLevel::O0)
                   .Case("O0", OptimizationLevel::O0)
                   .Case("O1", OptimizationLevel::O1)
                   .Case("O2", OptimizationLevel::O2)
                   .Case("O3", OptimizationLevel::O3)
                   .Case("Os", OptimizationLevel::Os)
                   .Case("Oz", OptimizationLevel::Oz)
                   .Default(std::nullopt);
  if (Level)
    return *Level;
  return make_error<StringError>(
      formatv("invalid optimization level '{}'", Name).str(),
      inconvertibleErrorCode());
}

#define EP_CALLBACK_BODY(NAME, ARGS)                                        \
  if (checkParametrizedPassName(Name, NAME)) {                              \
    auto L = parseOptLevelPass(Name, NAME);                                 \
    if (!L) {                                                               \
      errs() << NAME ": " << toString(L.takeError()) << '\n';               \
      return false;                                                         \
    }                                                                       \
    PB.ARGS;                                                                \
    return true;                                                            \
  }

void registerCallbackParsing(PassBuilder &PB) {
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, ModulePassManager &PM,
          ArrayRef<PassBuilder::PipelineElement>) {
#define MODULE_CALLBACK(NAME, INVOKE) EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get()))
#include "callbacks.inc"
        return false;
      });

  PB.registerPipelineParsingCallback(
      [&](StringRef Name, ModulePassManager &PM,
          ArrayRef<PassBuilder::PipelineElement>) {
#if LLVM_VERSION_MAJOR >= 20
#define MODULE_LTO_CALLBACK(NAME, INVOKE)                                   \
  EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get(), ThinOrFullLTOPhase::None))
#else
#define MODULE_LTO_CALLBACK(NAME, INVOKE)                                   \
  EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get()))
#endif
#include "callbacks.inc"
        return false;
      });

  PB.registerPipelineParsingCallback(
      [&](StringRef Name, FunctionPassManager &PM,
          ArrayRef<PassBuilder::PipelineElement>) {
#define FUNCTION_CALLBACK(NAME, INVOKE) EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get()))
#include "callbacks.inc"
        return false;
      });

  PB.registerPipelineParsingCallback(
      [&](StringRef Name, CGSCCPassManager &PM,
          ArrayRef<PassBuilder::PipelineElement>) {
#define CGSCC_CALLBACK(NAME, INVOKE) EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get()))
#include "callbacks.inc"
        return false;
      });

  PB.registerPipelineParsingCallback(
      [&](StringRef Name, LoopPassManager &PM,
          ArrayRef<PassBuilder::PipelineElement>) {
#define LOOP_CALLBACK(NAME, INVOKE) EP_CALLBACK_BODY(NAME, INVOKE(PM, L.get()))
#include "callbacks.inc"
        return false;
      });
}

#undef EP_CALLBACK_BODY

} // namespace
#endif // LLVM 17..21

// Vendored API entrypoint

// Pass instrumentation (e.g. `-print-after-all`) receives the IR unit as an `llvm::Any`,
// which identifies types by the address of `Any::TypeId<T>::Id`. Before LLVM 20
// (llvm/llvm-project#108051), each shared library creating an `Any` can end up with its own
// copy of that variable. On Linux, Julia makes libLLVM use the one from libjulia-codegen,
// but LLVMExtra is loaded with RTLD_DEEPBIND and binds to a different one. So if
// `PassManager::run` gets inlined here, the instrumentation fails to recognize the IR it
// is passed and crashes. Avoid that by calling libLLVM's instantiation of `run` through an
// opaque function pointer.
template <typename IRUnitT>
static PreservedAnalyses runPassManager(PassManager<IRUnitT> &PM, IRUnitT &IR,
                                        AnalysisManager<IRUnitT> &AM) {
#if LLVM_VERSION_MAJOR < 20
  using RunFn =
      PreservedAnalyses (PassManager<IRUnitT>::*)(IRUnitT &, AnalysisManager<IRUnitT> &);
  static RunFn volatile Run = &PassManager<IRUnitT>::run;
  return (PM.*Run)(IR, AM);
#else
  return PM.run(IR, AM);
#endif
}

// The instrumentation of a pipeline, which also instruments the passes that a custom pass
// runs with the pipeline's analysis manager (see `LLVMExtraRunJuliaPassesWithAnalyses`).
struct PipelineInstrumentation {
  PassInstrumentationCallbacks *PIC;
  // The names of the custom passes of such nested runs. The instrumentation refers to the
  // names it is given (without copying them, since LLVM 19), so they need to live as long
  // as the pipeline, rather than as long as the extensions of the nested run.
  StringSet<> NestedPassNames;
};

// An analysis that provides the instrumentation of the pipeline that a function analysis
// manager belongs to.
struct PipelineInstrumentationAnalysis
    : AnalysisInfoMixin<PipelineInstrumentationAnalysis> {
  struct Result {
    PipelineInstrumentation *PI;
    bool invalidate(Function &, const PreservedAnalyses &,
                    FunctionAnalysisManager::Invalidator &) {
      return false;
    }
  };
  PipelineInstrumentation *PI;
  explicit PipelineInstrumentationAnalysis(PipelineInstrumentation *PI) : PI(PI) {}
  Result run(Function &, FunctionAnalysisManager &) { return {PI}; }

private:
  friend AnalysisInfoMixin<PipelineInstrumentationAnalysis>;
  static AnalysisKey Key;
};
AnalysisKey PipelineInstrumentationAnalysis::Key;

static PassBuilder createPassBuilder(TargetMachine *Machine, LLVMPassBuilderOptions *PassOpts,
                                     PassInstrumentationCallbacks *PIC) {
#if LLVM_VERSION_MAJOR >= 16
  return PassBuilder(Machine, PassOpts->PTO, std::nullopt, PIC);
#else
  return PassBuilder(Machine, PassOpts->PTO, None, PIC);
#endif
}

// Set up a pass builder with the passes and callbacks of the extensions, and the passes
// that LLVMExtra adds to those of LLVM.
static void registerExtensions(PassBuilder &PB, TargetMachine *Machine,
                               PassInstrumentationCallbacks *PIC,
                               LLVMPassBuilderExtensions *PassExts,
                               StringSet<> *NameStorage = nullptr) {
  for (auto &Callback : PassExts->RegistrationCallbacks)
    Callback(&PB);
  // instrumentation selects passes by their pipeline name, e.g. `-print-before=NAME`, which
  // it looks up by class name. custom passes are reported under their own name, so map
  // that to itself. the names need to outlive the instrumentation, which refers to them:
  // those of the extensions do, unless they are of a nested run (see `NameStorage`).
  if (PIC)
    for (auto &Name : PassExts->CustomPassNames) {
      StringRef Stored = NameStorage ? NameStorage->insert(Name).first->getKey() : Name;
      PIC->addClassToPassName(Stored, Stored);
    }
  for (auto &Callback : PassExts->ModulePipelineParsingCallbacks)
    PB.registerPipelineParsingCallback(Callback);
  for (auto &Callback : PassExts->FunctionPipelineParsingCallbacks)
    PB.registerPipelineParsingCallback(Callback);
#if LLVM_VERSION_MAJOR >= 17 && LLVM_VERSION_MAJOR < 22
  registerCallbackParsing(PB);
#endif
  // LLVM's default alias analysis pipeline includes those that the target machine provides,
  // but an explicit pipeline cannot name them. make them available as `target-aa`.
  PB.registerParseAACallback([Machine](StringRef Name, AAManager &AA) {
    if (Name != "target-aa")
      return false;
    if (Machine)
      Machine->registerDefaultAliasAnalyses(AA);
    return true;
  });
#if LLVM_VERSION_MAJOR >= 16
  // LLVM's pipelines use `VectorCombinePass(/*TryEarlyFoldsOnly=*/true)`, which its parser
  // cannot express (before LLVM 16, the flag had a different meaning). LLVM prints it like
  // the full pass, so name it after its parameters to print it as it is parsed.
  PB.registerPipelineParsingCallback(
      [](StringRef Name, FunctionPassManager &PM,
         ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
        StringRef Params;
        if (!InnerPipeline.empty() ||
            !matchParametrizedPassName(Name, "vector-combine", Params))
          return false;
        bool Early = false;
        while (!Params.empty()) {
          StringRef Param;
          std::tie(Param, Params) = Params.split(';');
          if (Param == "early")
            Early = true;
          else if (Param == "no-early")
            Early = false;
          else
            return false;
        }
        PM.addPass(NamedPass("VectorCombinePass",
                             Early ? "vector-combine<early>" : "vector-combine", false,
                             VectorCombinePass(Early)));
        return true;
      });
#endif
#if LLVM_VERSION_MAJOR < 21
  // `expand-reductions` is only registered with the new pass manager since LLVM 21
  // (llvm/llvm-project#148109), but the pass itself is available
  PB.registerPipelineParsingCallback(
      [](StringRef Name, FunctionPassManager &PM,
         ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
        if (Name != "expand-reductions" || !InnerPipeline.empty())
          return false;
        PM.addPass(ExpandReductionsPass());
        return true;
      });
#endif
#if LLVM_VERSION_MAJOR < 19
  // `internalize` only takes `preserve-gv` parameters since LLVM 19 (llvm/llvm-project#92383,
  // which Julia's LLVM 18 has backported), but the pass itself supports preserving global
  // values, so parse them like LLVM 19 does. This is only reached if LLVM didn't parse it.
  PB.registerPipelineParsingCallback(
      [](StringRef Name, ModulePassManager &PM,
         ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
        if (!InnerPipeline.empty() || !Name.consume_front("internalize<") ||
            !Name.consume_back(">"))
          return false;
        std::vector<std::string> PreservedGVs;
        while (!Name.empty()) {
          StringRef Param;
          std::tie(Param, Name) = Name.split(';');
          if (!Param.consume_front("preserve-gv="))
            return false;
          PreservedGVs.push_back(Param.str());
        }
        PM.addPass(InternalizePass([PreservedGVs](const GlobalValue &GV) {
          return llvm::is_contained(PreservedGVs, GV.getName());
        }));
        return true;
      });
#endif
}

static LLVMErrorRef runJuliaPasses(Module *Mod, Function *Fun, const char *Passes,
                                   TargetMachine *Machine, LLVMPassBuilderOptions *PassOpts,
                                   LLVMPassBuilderExtensions *PassExts) {
  bool Debug = PassOpts->DebugLogging;
  bool VerifyEach = PassOpts->VerifyEach;

  PassInstrumentationCallbacks PIC;
  PipelineInstrumentation PI{&PIC, {}};
  PassBuilder PB = createPassBuilder(Machine, PassOpts, &PIC);

  registerExtensions(PB, Machine, &PIC, PassExts);

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
#if LLVM_VERSION_MAJOR >= 21
        // LLVM 21 replaced the templated `Model<T>` constructor with one that
        // takes a `unique_ptr<const TargetTransformInfoImplBase>`; the impl
        // base now dispatches through virtual methods rather than type erasure.
        return TargetTransformInfo(std::make_unique<CustomTargetTransformInfo>(
            F.getParent()->getDataLayout(), Opts));
#else
        return TargetTransformInfo(
            CustomTargetTransformInfo(F.getParent()->getDataLayout(), Opts));
#endif
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
  FAM.registerPass([&] { return PipelineInstrumentationAnalysis(&PI); });
  PB.registerLoopAnalyses(LAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerModuleAnalyses(MAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

#if LLVM_VERSION_MAJOR >= 16
  auto *SI = new StandardInstrumentations(Mod->getContext(), Debug, VerifyEach);
#else
  auto *SI = new StandardInstrumentations(Debug, VerifyEach);
#endif
  // StandardInstrumentations may register members in process-global state.
  // Julia exceptions use longjmp and can therefore skip C++ destructors when
  // they escape a pass callback. Keep SI off the stack so that a non-local
  // exit leaks valid storage instead of leaving dangling global pointers.
#if LLVM_VERSION_MAJOR >= 17
  SI->registerCallbacks(PIC, &MAM);
#else
  SI->registerCallbacks(PIC, &FAM);
#endif

  if (Fun) {
    FunctionPassManager FPM;
    if (VerifyEach)
      FPM.addPass(VerifierPass());
    if (auto Err = PB.parsePassPipeline(FPM, Passes)) {
      delete SI;
      return wrap(std::move(Err));
    }
    runPassManager(FPM, *Fun, FAM);
  } else {
    ModulePassManager MPM;
    if (VerifyEach)
      MPM.addPass(VerifierPass());
    if (auto Err = PB.parsePassPipeline(MPM, Passes)) {
      delete SI;
      return wrap(std::move(Err));
    }
    runPassManager(MPM, *Mod, MAM);
  }

  delete SI;
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

LLVMErrorRef LLVMExtraRunJuliaPassesWithAnalyses(LLVMValueRef F, const char *Passes,
                                                LLVMTargetMachineRef TM,
                                                LLVMPassBuilderOptionsRef Options,
                                                LLVMPassBuilderExtensionsRef Extensions,
                                                LLVMFunctionAnalysisManagerRef AM,
                                                LLVMPreservedAnalysesRef *PA) {
  *PA = nullptr;
  Function &Fun = *unwrap<Function>(F);
  FunctionAnalysisManager &FAM = *unwrap(AM);
  // the passes are instrumented by the analysis manager's instrumentation, i.e., that of
  // the pipeline that the analysis manager belongs to, which needs to know the names of
  // the custom passes
  PipelineInstrumentation *PI = FAM.getResult<PipelineInstrumentationAnalysis>(Fun).PI;
  PassBuilder PB = createPassBuilder(unwrap(TM), unwrap(Options), nullptr);
  registerExtensions(PB, unwrap(TM), PI->PIC, unwrap(Extensions), &PI->NestedPassNames);

  FunctionPassManager FPM;
  if (auto Err = PB.parsePassPipeline(FPM, Passes))
    return wrap(std::move(Err));
  *PA = wrap(new PreservedAnalyses(runPassManager(FPM, Fun, FAM)));
  return LLVMErrorSuccess;
}

#if LLVM_VERSION_MAJOR < 19
// Before LLVM 19, a pass builder only registers the pipeline names of LLVM's pass classes
// with the instrumentation callbacks when an option asks for them, like
// `-print-pipeline-passes` (a global option, which cannot be set safely while other threads
// may use LLVM). Register them here instead: parse every pass and analysis that the pass
// builder knows, and record the class name that it prints itself with.
static void registerClassToPassNames(PassBuilder &PB, PassInstrumentationCallbacks &PIC) {
  std::string Names;
  raw_string_ostream NamesOS(Names);
  PB.printPassNames(NamesOS);
  NamesOS.flush();

  SmallVector<StringRef, 0> Lines;
  StringRef(Names).split(Lines, '\n');
  std::optional<Regex> Printed;
  bool AreAnalyses = false, HaveParams = false;
  for (StringRef Line : Lines) {
    // sections are headed like "Function passes:", "Function passes with params:" or
    // "Function analyses:"
    if (!Line.consume_front("  ")) {
      AreAnalyses = Line.contains("analyses");
      HaveParams = Line.contains("with params");
      // how a pass or analysis that is parsed at the module level prints, using `@` for
      // its class name: in the adaptors for its kind. this leaves out composite passes,
      // like the module inliner, which print other passes.
      StringRef Adaptors = StringSwitch<StringRef>(Line.split(' ').first)
                               .Case("Module", "()")
                               .Case("CGSCC", "(cgscc\\()")
                               .Case("Function", "(function\\()")
                               .Cases("LoopNest", "Loop", "(function\\(loop(-mssa)?\\()")
                               .Default("");
      if (Adaptors.empty())
        Printed.reset();
      else
        Printed.emplace(("^" + Adaptors + "(require<)?@(<.*>)?>?\\)*$").str());
      continue;
    }
    if (!Printed)
      continue;
    // passes with parameters are listed with the parameters they take
    StringRef Name = HaveParams ? Line.split('<').first : Line;
    ModulePassManager MPM;
    std::string Pipeline = AreAnalyses ? ("require<" + Name + ">").str() : Name.str();
    if (auto Err = PB.parsePassPipeline(MPM, Pipeline)) {
      consumeError(std::move(Err));
      continue;
    }
    std::string Text;
    raw_string_ostream TextOS(Text);
    StringRef ClassName;
    MPM.printPipeline(TextOS, [&](StringRef Class) {
      ClassName = Class;
      return "@";
    });
    TextOS.flush();
    if (!ClassName.empty() && Printed->match(Text) &&
        PIC.getPassNameForClassName(ClassName).empty())
      PIC.addClassToPassName(ClassName, Name);
  }
}
#endif

LLVMErrorRef LLVMExtraPrintJuliaPasses(const char *Passes, LLVMTargetMachineRef TM,
                                       LLVMPassBuilderOptionsRef Options,
                                       LLVMPassBuilderExtensionsRef Extensions,
                                       char **Pipeline) {
  PassInstrumentationCallbacks PIC;
  PassBuilder PB = createPassBuilder(unwrap(TM), unwrap(Options), &PIC);
#if LLVM_VERSION_MAJOR < 19
  registerClassToPassNames(PB, PIC);
#endif
  registerExtensions(PB, unwrap(TM), &PIC, unwrap(Extensions));

  ModulePassManager MPM;
  if (auto Err = PB.parsePassPipeline(MPM, Passes))
    return wrap(std::move(Err));

  // like `opt -print-pipeline-passes`
  std::string Str;
  raw_string_ostream OS(Str);
  MPM.printPipeline(OS, [&PIC](StringRef ClassName) {
    auto PassName = PIC.getPassNameForClassName(ClassName);
    return PassName.empty() ? ClassName : PassName;
  });
  OS.flush();
  *Pipeline = strdup(Str.c_str());
  return LLVMErrorSuccess;
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
