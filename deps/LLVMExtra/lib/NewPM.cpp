#include "LLVMExtra.h"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/CBindingWrapping.h>
#include <llvm/Support/FormatVariadic.h>
#include <optional>

using namespace llvm;

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

static bool checkParametrizedPassName(StringRef Name, StringRef PassName) {
  if (!Name.consume_front(PassName))
    return false;
  // normal pass name w/o parameters == default parameters
  if (Name.empty())
    return true;
#if LLVM_VERSION_MAJOR >= 16
  return Name.starts_with("<") && Name.ends_with(">");
#else
  return Name.startswith("<") && Name.endswith(">");
#endif
}

static std::optional<OptimizationLevel> parseOptLevel(StringRef S) {
  return StringSwitch<std::optional<OptimizationLevel>>(S)
      .Case("O0", OptimizationLevel::O0)
      .Case("O1", OptimizationLevel::O1)
      .Case("O2", OptimizationLevel::O2)
      .Case("O3", OptimizationLevel::O3)
      .Case("Os", OptimizationLevel::Os)
      .Case("Oz", OptimizationLevel::Oz)
      .Default(std::nullopt);
}

static Expected<OptimizationLevel> parseOptLevelParam(StringRef S) {
  std::optional<OptimizationLevel> OptLevel = parseOptLevel(S);
  if (OptLevel)
    return *OptLevel;
  return make_error<StringError>(
      formatv("invalid optimization level '{}'", S).str(),
      inconvertibleErrorCode());
}

template <typename ParametersParseCallableT>
static auto parsePassParameters(ParametersParseCallableT &&Parser,
                                StringRef Name, StringRef PassName)
    -> decltype(Parser(StringRef{})) {
  using ParametersT = typename decltype(Parser(StringRef{}))::value_type;

  StringRef Params = Name;
  if (!Params.consume_front(PassName)) {
    llvm_unreachable(
        "unable to strip pass name from parametrized pass specification");
  }
  if (!Params.empty() &&
      (!Params.consume_front("<") || !Params.consume_back(">"))) {
    llvm_unreachable("invalid format for parametrized pass name");
  }

  Expected<ParametersT> Result = Parser(Params);
  assert((Result || Result.template errorIsA<StringError>()) &&
          "Pass parameter parser can only return StringErrors.");
  return Result;
}


// Register target specific parsing callbacks
static void registerCallbackParsing(PassBuilder &PB) {
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, ModulePassManager &PM,
             ArrayRef<PassBuilder::PipelineElement>) {
#define MODULE_CALLBACK(NAME, INVOKE)                                          \
    if (checkParametrizedPassName(Name, NAME)) {                    \
      auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
      if (!L) {                                                                  \
        errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
        return false;                                                            \
      }                                                                          \
      PB.INVOKE(PM, L.get());                                                       \
      return true;                                                               \
    }
    #include "callbacks.inc"
    return false;
  });

  // Module-level callbacks with LTO phase (use Phase::None for string API)
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, ModulePassManager &PM,
             ArrayRef<PassBuilder::PipelineElement>) {
#if LLVM_VERSION_MAJOR >= 20
#define MODULE_LTO_CALLBACK(NAME, INVOKE)                                      \
    if (checkParametrizedPassName(Name, NAME)) {                    \
      auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
      if (!L) {                                                                  \
        errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
        return false;                                                            \
      }                                                                          \
      PB.INVOKE(PM, L.get(), ThinOrFullLTOPhase::None);                             \
      return true;                                                               \
    }
    #include "callbacks.inc"
#else
#define MODULE_LTO_CALLBACK(NAME, INVOKE)                                      \
    if (checkParametrizedPassName(Name, NAME)) {                    \
      auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
      if (!L) {                                                                  \
        errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
        return false;                                                            \
      }                                                                          \
      PB.INVOKE(PM, L.get());                             \
      return true;                                                               \
    }
    #include "callbacks.inc"
#endif
    return false;
  });

  // Function-level callbacks
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, FunctionPassManager &PM,
             ArrayRef<PassBuilder::PipelineElement>) {
#define FUNCTION_CALLBACK(NAME, INVOKE)                                        \
    if (checkParametrizedPassName(Name, NAME)) {                    \
      auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
      if (!L) {                                                                  \
        errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
        return false;                                                            \
      }                                                                          \
      PB.INVOKE(PM, L.get());                                                       \
      return true;                                                               \
    }
    #include "callbacks.inc"
    return false;
  });

  // CGSCC-level callbacks
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, CGSCCPassManager &PM,
             ArrayRef<PassBuilder::PipelineElement>) {
#define CGSCC_CALLBACK(NAME, INVOKE)                                           \
    if (checkParametrizedPassName(Name, NAME)) {                    \
      auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
      if (!L) {                                                                  \
        errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
        return false;                                                            \
      }                                                                          \
      PB.INVOKE(PM, L.get());                                                       \
      return true;                                                               \
    }
    #include "callbacks.inc"
    return false;
  });

  // Loop-level callbacks
  PB.registerPipelineParsingCallback(
      [&](StringRef Name, LoopPassManager &PM,
             ArrayRef<PassBuilder::PipelineElement>) {
#define LOOP_CALLBACK(NAME, INVOKE)                                            \
      if (checkParametrizedPassName(Name, NAME)) {                    \
        auto L = parsePassParameters(parseOptLevelParam, Name, NAME); \
        if (!L) {                                                                  \
          errs() << NAME ": " << toString(L.takeError()) << '\n';                  \
          return false;                                                            \
        }                                                                          \
        PB.INVOKE(PM, L.get());                                                       \
        return true;                                                               \
      }
    #include "callbacks.inc"
    return false;
  });
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
  registerCallbackParsing(PB); // Parsing for target-specific callbacks
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
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
