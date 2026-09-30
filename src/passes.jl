# pass builders, pass managers and passes


## pass managers

@vocabulary Passes ModulePassManager, CGSCCPassManager, FunctionPassManager,
                   LoopPassManager, AAManager

abstract type AbstractPassManager end

"""
    add!(pm::AbstractPassManager, pass)

Adds a pass or pipeline to a pass builder or pass manager, and returns the pass builder or
pass manager.

The pass or pipeline should be a string or string-convertible object known by LLVM. These
can be constructed by using pass constructors, e.g., `InternalizePass()`, or by manually
specifying names like `default<O3>`.

When using custom passes, remember that they need to be registered with the pass builder
before they can be used.

See also: [`register!`](@ref)
"""
function add!(pm::AbstractPassManager, pass)
    push!(pm.passes, string(pass))
    return pm
end

"""
    ModulePassManager()
    CGSCCPassManager()
    FunctionPassManager()
    LoopPassManager(; use_memory_ssa=false)
    AAManager()

Create a new pass manager of the specified type. These objects can be used to construct
pass pipelines, by `add!`ing passes to them, and finally `add!`ing them to a parent
pass manager or pass builder.

Creating a pass manager and adding it to a parent manager or builder can be shortened
using a single `add!`:

```julia
add!(parent, ModulePassManager()) do mpm
    add!(mpm, SomeModulePass())
end
```

See also: [`add!`](@ref), [`PassBuilder`](@ref)
"""
struct PassManager <: AbstractPassManager
    type::String
    passes::Vector{String}

    PassManager(type::String) = new(type, [])
end
@vocabulary Passes PassManager

Base.string(pm::PassManager) = "$(pm.type)($(join(pm.passes, ",")))"

function add!(f::Base.Callable, parent::AbstractPassManager, nested::AbstractPassManager)
    f(nested)
    if !isempty(nested.passes)
        add!(parent, nested)
    end
    return parent
end

@doc (@doc PassManager)
ModulePassManager() = PassManager("module")

@doc (@doc PassManager)
CGSCCPassManager() = PassManager("cgscc")

@doc (@doc PassManager)
FunctionPassManager() = PassManager("function")

@doc (@doc PassManager)
LoopPassManager(; use_memory_ssa=false) =
    PassManager(use_memory_ssa ? "loop-mssa" : "loop")


## custom passes

# TODO: support for options

@vocabulary Passes ModulePass, FunctionPass

"""
    ModulePass(name, callback)
    FunctionPass(name, callback)

Create a new custom pass. The `name` is a string that will be used to identify the pass
in the pass manager. The `callback` is a function that will be called when the pass is
run. The function should take a single argument, the module or function to be processed,
and return a boolean indicating whether the pass made any changes.

Before using a custom pass, it must be registered with a pass builder using `register!`.
LLVM.jl catches exceptions from these callbacks and rethrows them as `PassException`
after control has returned from LLVM. Callbacks registered directly through the
`LLVM.API.LLVMPassBuilderExtensionsRegister*Pass` APIs must provide an equivalent exception
barrier: Julia exceptions must not escape a callback, because they bypass C++ destructors in
LLVM's pass runner.

See also: [`register!`](@ref)
"""
struct CustomPass
  type::Symbol
  name::String
  callback::Any
end
@vocabulary Passes CustomPass

Base.string(pass::CustomPass) = pass.name

@doc (@doc CustomPass)
ModulePass(name, callback)   = CustomPass(:module, name, callback)

@doc (@doc CustomPass)
FunctionPass(name, callback) = CustomPass(:function, name, callback)

# State struct to store callback and any caught exception
mutable struct CustomPassState
    callback::Any
    exception::Union{Nothing, Tuple{Any, Vector}}  # (exception, backtrace)
    CustomPassState(callback) = new(callback, nothing)
end

# Exception type to preserve original error and backtrace
@vocabulary Passes PassException
struct PassException <: Exception
    ex::Any
    processed_bt::Vector{Base.StackTraces.StackFrame}

    function PassException(ex, bt)
        # `stacktrace` accepts either a raw bt from `catch_backtrace()` or
        # an already-processed frame vector, and is stable across Julia
        # versions (unlike `Base.process_backtrace`, whose method for the
        # raw bt type was dropped in 1.14).
        bt_processed = stacktrace(bt)
        new(ex, bt_processed[1:min(100, end)])
    end
end

function Base.showerror(io::IO, e::PassException)
    print(io, "PassException: exception in custom pass callback\n\n    nested exception: ")
    showerror(io, e.ex, e.processed_bt, backtrace=true)
end

function module_callback(ref::API.LLVMModuleRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return true
    try
        mod = LLVM.Module(ref)
        return state.callback(mod)::Bool
    catch err
        _capture_callback_exception!(state, err)
        # The callback may have changed IR before throwing. Invalidate all
        # analyses before surfacing the exception after LLVM returns.
        return true
    end
end

function function_callback(ref::API.LLVMValueRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return true
    try
        fun = LLVM.Function(ref)
        return state.callback(fun)::Bool
    catch err
        _capture_callback_exception!(state, err)
        # The callback may have changed IR before throwing. Invalidate all
        # analyses before surfacing the exception after LLVM returns.
        return true
    end
end


## pass builder

@vocabulary Passes PassBuilder, register!, add!, run!

"""
    PassBuilder(; verify_each=false, debug_logging=false, pipeline_tuning_kwargs...)
    PassBuilder(f; kwargs...)

Create a new pass builder. The pass builder is the main object used to construct and run
pass pipelines. The `verify_each` keyword argument enables module verification after each
pass, while `debug_logging` can be used to enable more output. Pass builder objects need to
be disposed of after use, e.g., using `@dispose` or the do-block form.

Several other keyword arguments can be used to tune the pipeline. This only has an effect
when using one of LLVM's default pipelines, like `default<O3>`:

- `loop_interleaving::Bool=false`: Enable loop interleaving.
- `loop_vectorization::Bool=false`: Enable loop vectorization.
- `slp_vectorization::Bool=false`: Enable SLP vectorization.
- `loop_unrolling::Bool=false`: Enable loop unrolling.
- `forget_all_scev_in_loop_unroll::Bool=false`: Forget all SCEV information in loop
  unrolling.
- `licm_mssa_opt_cap::Int=0`: LICM MSSA optimization cap.
- `licm_mssa_no_acc_for_promotion_cap::Int=0`: LICM MSSA no access for promotion cap.
- `call_graph_profile::Bool=false`: Enable call graph profiling.
- `merge_functions::Bool=false`: Enable function merging.

After a pass builder is constructed, custom passes can be registered with `register!`,
passes or nested pass managers can be added with `add!`, and finally the passes can be run
with `run!`:

```julia
@dispose pb = PassBuilder(verify_each=true) begin
    register!(pb, SomeCustomPass())
    add!(pb, SomeModulePass())
    add!(pb, FunctionPassManager()) do fpm
        add!(fpm, SomeFunctionPass())
    end
    run!(pb, mod, tm)
end
```

For quickly running a simple pass or pipeline, a shorthand `run!` method is provided that
obviates the construction of a `PassBuilder`:

```julia
run!("some-pass", mod, tm; verify_each=true)
```

See also: [`register!`](@ref), [`add!`](@ref), [`run!`](@ref)
"""
mutable struct PassBuilder <: AbstractPassManager
    opts::API.LLVMPassBuilderOptionsRef
    passes::Vector{String}
    aa_passes::Vector{String}
    custom_passes::Vector{CustomPass}
    custom_tti::Union{AbstractTargetTransformInfo,Nothing}
    registration_callbacks::Vector{Ptr{Cvoid}}
end

Base.string(pm::PassBuilder) = join(pm.passes, ",")

Base.unsafe_convert(::Type{API.LLVMPassBuilderOptionsRef}, pb::PassBuilder) =
    mark_use(pb).opts

function PassBuilder(; kwargs...)
    opts = API.LLVMCreatePassBuilderOptions()
    obj = mark_alloc(PassBuilder(opts, [], [], [], nothing, []))

    # dispose of the options if a keyword argument is invalid
    try
        for (name, value) in pairs(kwargs)
            if name == :verify_each
                API.LLVMPassBuilderOptionsSetVerifyEach(obj, value)
            elseif name == :debug_logging
                API.LLVMPassBuilderOptionsSetDebugLogging(obj, value)
            elseif name == :loop_interleaving
                API.LLVMPassBuilderOptionsSetLoopInterleaving(obj, value)
            elseif name == :loop_vectorization
                API.LLVMPassBuilderOptionsSetLoopVectorization(obj, value)
            elseif name == :slp_vectorization
                API.LLVMPassBuilderOptionsSetSLPVectorization(obj, value)
            elseif name == :loop_unrolling
                API.LLVMPassBuilderOptionsSetLoopUnrolling(obj, value)
            elseif name == :forget_all_scev_in_loop_unroll
                API.LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll(obj, value)
            elseif name == :licm_mssa_opt_cap
                API.LLVMPassBuilderOptionsSetLicmMSSAOptCap(obj, value)
            elseif name == :licm_mssa_no_acc_for_promotion_cap
                API.LLVMPassBuilderOptionsSetLicmMSSANoAccForPromotionCap(obj, value)
            elseif name == :call_graph_profile
                API.LLVMPassBuilderOptionsSetCallGraphProfile(obj, value)
            elseif name == :merge_functions
                API.LLVMPassBuilderOptionsSetMergeFunctions(obj, value)
            else
                throw(ArgumentError("invalid keyword argument $name"))
            end
        end
    catch
        dispose(obj)
        rethrow()
    end

    return obj
end

PassBuilder(f::Core.Function; kwargs...) = with_disposal(f, PassBuilder(; kwargs...))

function dispose(pb::PassBuilder)
    API.LLVMDisposePassBuilderOptions(pb.opts)
    mark_dispose(pb)
end

"""
    register!(pb, custom_pass)

Register a custom pass with the pass builder. This is necessary before the pass can be
used in a pass pipeline.

See also: [`ModulePass`](@ref), [`FunctionPass`](@ref)
"""
function register!(pb::PassBuilder, pass::CustomPass)
    push!(pb.custom_passes, pass)
    return pb
end

@vocabulary Passes register_callbacks!

"""
    register_callbacks!(pb::PassBuilder, callback::Ptr{Cvoid})

Register a native callback that is called with LLVM's C++ `PassBuilder` (as a `void *`)
when the pass builder is used to run passes. This makes it possible to use passes that are
implemented in C++, by calling the `PassBuilder`'s `register*Callback` methods, like pass
plugins do. For example, for a library that provides a `registerCallbacks` function:

```julia
register_callbacks!(pb, cglobal((:registerCallbacks, libfoo)))
```

The callback needs to have the signature `void (*)(void *)`, and the library that provides
it needs to be built against the same version of LLVM as the one that Julia uses, and remain
loaded while the pass builder is used. Callbacks are called in the order they were
registered, after LLVM.jl registers Julia's passes, and must not throw Julia exceptions. To
implement a pass in Julia instead, use [`register!`](@ref).
"""
function register_callbacks!(pb::PassBuilder, callback::Ptr{Cvoid})
    callback == C_NULL && throw(ArgumentError("Registration callback cannot be NULL"))
    push!(pb.registration_callbacks, callback)
    return pb
end

@vocabulary Passes target_transform_info!

function install_custom_tti!(exts::API.LLVMPassBuilderExtensionsRef,
                              tti::AbstractTargetTransformInfo)
    state, opts = build_custom_tti_options(tti)
    API.LLVMPassBuilderExtensionsSetTTI(exts, opts)
    # `SetTTI` copies the options into the extensions, so `opts` can be freed
    # right away. `state` backs the `UserData` pointer the C++ side kept and
    # must stay GC-rooted until `run!` completes.
    API.LLVMDisposeTTIOptions(opts)
    return state
end

"""
    target_transform_info!(pb::PassBuilder, tti::AbstractTargetTransformInfo)
    target_transform_info!(pb::PassBuilder, ::Nothing)

Attach an [`AbstractTargetTransformInfo`](@ref) subtype instance to the pass
builder, replacing any previously-attached custom TTI. Pass `nothing` to
revert to LLVM's native TTI (derived from the `TargetMachine`, if any;
otherwise `TargetTransformInfoImplBase` with full `DataLayout`/`Module`-aware
defaults).
"""
function target_transform_info!(pb::PassBuilder,
                                tti::AbstractTargetTransformInfo)
    pb.custom_tti = tti
    return pb
end

function target_transform_info!(pb::PassBuilder, ::Nothing)
    pb.custom_tti = nothing
    return pb
end

"""
    run!(pb::PassBuilder, mod::Module, [tm::TargetMachine])
    run!(pipeline::String, mod::Module, [tm::TargetMachine])

Run passes on a module. The passes are specified by a pass builder or a string that
represents a pass pipeline. The target machine is used to optimize the passes.
"""
run!

function run!(pb::PassBuilder, target::Union{Module,Function}, tm::Union{Nothing,TargetMachine}=nothing)
    isempty(pb.passes) && return
    pipeline = join(pb.passes, ",")
    aa_pipeline = join(pb.aa_passes, ",")

    # XXX: The Base API is too restricted, not supporting custom passes
    #      or Julia's pass registration callback
    #@check API.LLVMRunPasses(mod, string(pb), tm, pb.opts)

    # the extensions only live for the duration of this run, as they hold references to
    # the state of the callbacks (which would be stale during a later run)
    exts = API.LLVMCreatePassBuilderExtensions()
    try
        run_passes!(pb, exts, target, tm, pipeline, aa_pipeline)
    finally
        API.LLVMDisposePassBuilderExtensions(exts)
    end
end

function run_passes!(pb::PassBuilder, exts::API.LLVMPassBuilderExtensionsRef,
                     target::Union{Module,Function}, tm::Union{Nothing,TargetMachine},
                     pipeline::String, aa_pipeline::String)
    # Create state objects to hold callbacks and any caught exceptions
    states = [CustomPassState(pass.callback) for pass in pb.custom_passes]
    tti_state = pb.custom_tti === nothing ? nothing :
                install_custom_tti!(exts, pb.custom_tti)
    ctx = context(target)
    prepare_diagnostic(ctx)
    GC.@preserve states tti_state aa_pipeline begin
        # register custom passes
        for (i,pass) in enumerate(pb.custom_passes)
            if pass.type === :module
                cb = @cfunction(module_callback, Bool, (API.LLVMModuleRef, Ptr{Cvoid}))
                api = API.LLVMPassBuilderExtensionsRegisterModulePass
            elseif pass.type === :function
                cb = @cfunction(function_callback, Bool, (API.LLVMValueRef, Ptr{Cvoid}))
                api = API.LLVMPassBuilderExtensionsRegisterFunctionPass
            else
                throw(ArgumentError("invalid pass type $(pass.type)"))
            end
            api(exts, pass.name, cb, Ref(states, i))
        end

        # register Julia passes
        julia_callback = cglobal(:jl_register_passbuilder_callbacks)
        API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, julia_callback)

        # register native callbacks
        for callback in pb.registration_callbacks
            API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, callback)
        end

        # register AA pipeline
        if !isempty(aa_pipeline)
            if version() >= v"20"
                API.LLVMPassBuilderOptionsSetAAPipeline(pb.opts, aa_pipeline)
            else
                API.LLVMPassBuilderExtensionsSetAAPipeline(exts, aa_pipeline)
            end
        end

        try
            if target isa Module
                @check API.LLVMRunJuliaPasses(target, pipeline, something(tm, C_NULL),
                                              pb.opts, exts)
            elseif target isa Function
                @check API.LLVMRunJuliaPassesOnFunction(target, pipeline,
                                                        something(tm, C_NULL), pb.opts, exts)
            end
        finally
            # the options keep a pointer to the AA pipeline, which is only valid during the run
            if !isempty(aa_pipeline) && version() >= v"20"
                API.LLVMPassBuilderOptionsSetAAPipeline(pb.opts, C_NULL)
            end
        end

        # Check for any exceptions caught in custom pass callbacks
        for state in states
            if state.exception !== nothing
                (err, bt) = state.exception
                throw(PassException(err, bt))
            end
        end
        if tti_state !== nothing && tti_state.exception !== nothing
            (err, bt) = tti_state.exception
            throw(PassException(err, bt))
        end
        check_diagnostic(ctx)
    end
end

function run!(pass::String, args...; kwargs...)
    @dispose pb=PassBuilder(; kwargs...) begin
        add!(pb, pass)
        run!(pb, args...)
    end
end


## pass definitions

# convert Julia keyword arguments to a LLVM pass parameter string
function kwargs_to_params(kwargs; allow_empty=false)
    isempty(kwargs) && return ""

    params = String[]
    for (k, v) in kwargs
        # Julia uses `_` in kwargs, while LLVM always uses `-`
        k = replace(string(k), "_" => "-")

        if v isa Bool
            push!(params, v ? k : "no-$k")
        else
            push!(params, "$k=$v")
        end
    end
    "<" * join(params, ";") * ">"
end

function define_pass(mod, pass_name, class_name, define_class=true)
    # don't re-define passes (some work with multiple types of managers,
    # or could be manually-defined)
    if isdefined(LLVM, class_name)
        return
    end

    # LLVM's passes are part of the Passes vocabulary, while passes defined elsewhere
    # (e.g., Julia's passes in LLVM.Interop) are exported by their module
    ex = if mod === LLVM
        quote
            $(esc(:(@vocabulary Passes $class_name)))
        end
    else
        quote
            export $(esc(class_name))
        end
    end
    if define_class
        push!(ex.args, :(
            function $(esc(class_name))(; kwargs...)
                return $pass_name * kwargs_to_params(kwargs)
            end
        ))
    end
    return ex
end

# for testing purposes, keep track of all defined passes
const module_passes = String[]
const cgscc_passes = String[]
const function_passes = String[]
const loop_passes = String[]

macro module_pass(pass_name, class_name, define_class=true)
    push!(module_passes, pass_name)
    define_pass(__module__, pass_name, class_name, define_class)
end
macro cgscc_pass(pass_name, class_name, define_class=true)
    push!(cgscc_passes, pass_name)
    define_pass(__module__, pass_name, class_name, define_class)
end
macro function_pass(pass_name, class_name, define_class=true)
    push!(function_passes, pass_name)
    define_pass(__module__, pass_name, class_name, define_class)
end
macro loop_pass(pass_name, class_name, define_class=true)
    push!(loop_passes, pass_name)
    define_pass(__module__, pass_name, class_name, define_class)
end

# module passes

@module_pass "always-inline" AlwaysInlinerPass
@module_pass "attributor" AttributorPass
@module_pass "annotation2metadata" Annotation2MetadataPass
@module_pass "openmp-opt" OpenMPOptPass
@module_pass "called-value-propagation" CalledValuePropagationPass
@module_pass "canonicalize-aliases" CanonicalizeAliasesPass
@module_pass "cg-profile" CGProfilePass
@module_pass "check-debugify" CheckDebugifyPass
@module_pass "constmerge" ConstantMergePass
@module_pass "coro-early" CoroEarlyPass
@module_pass "coro-cleanup" CoroCleanupPass
@module_pass "cross-dso-cfi" CrossDSOCFIPass
@module_pass "deadargelim" DeadArgumentEliminationPass
@module_pass "debugify" DebugifyPass
@module_pass "dot-callgraph" CallGraphDOTPrinterPass
@module_pass "elim-avail-extern" EliminateAvailableExternallyPass
@module_pass "extract-blocks" BlockExtractorPass
@module_pass "forceattrs" ForceFunctionAttrsPass
@module_pass "function-import" FunctionImportPass
@static if version() < v"16"
    @module_pass "function-specialization" FunctionSpecializationPass
else
    @module_pass "ipsccp<func-spec>" FunctionSpecializationPass
end
@module_pass "globaldce" GlobalDCEPass
@module_pass "globalopt" GlobalOptPass
@module_pass "globalsplit" GlobalSplitPass
@module_pass "hotcoldsplit" HotColdSplittingPass
@module_pass "inferattrs" InferFunctionAttrsPass
@module_pass "inliner-wrapper" ModuleInlinerWrapperPass
@module_pass "inliner-ml-advisor-release" ModuleInlinerMLAdvisorReleasePass
@module_pass "print<inline-advisor>" InlineAdvisorAnalysisPrinterPass
@module_pass "inliner-wrapper-no-mandatory-first" ModuleInlinerWrapperNoMandatoryFirstPass
@module_pass "insert-gcov-profiling" GCOVProfilerPass
@static if version() < v"21"
    @module_pass "instrorderfile" InstrOrderFilePass
end
@module_pass "instrprof" InstrProfiling
@module_pass "invalidate<all>" InvalidateAllAnalysesPass
@module_pass "ipsccp" IPSCCPPass
@module_pass "iroutliner" IROutlinerPass
@module_pass "print-ir-similarity" IRSimilarityAnalysisPrinterPass
@module_pass "lower-global-dtors" LowerGlobalDtorsPass
@module_pass "lowertypetests" LowerTypeTestsPass
@module_pass "metarenamer" MetaRenamerPass
@module_pass "mergefunc" MergeFunctionsPass
@module_pass "name-anon-globals" NameAnonGlobalPass
@module_pass "no-op-module" NoOpModulePass
@static if version() < v"22"
    @module_pass "objc-arc-apelim" ObjCARCAPElimPass
end
@module_pass "partial-inliner" PartialInlinerPass
@module_pass "pgo-icall-prom" PGOIndirectCallPromotion
@module_pass "pgo-instr-gen" PGOInstrumentationGen
@module_pass "pgo-instr-use" PGOInstrumentationUse
@module_pass "print-profile-summary" ProfileSummaryPrinterPass
@module_pass "print-callgraph" CallGraphPrinterPass
@module_pass "print" PrintModulePass
@module_pass "print-lcg" LazyCallGraphPrinterPass
@module_pass "print-lcg-dot" LazyCallGraphDOTPrinterPass
@module_pass "print-must-be-executed-contexts" MustBeExecutedContextPrinterPass
@module_pass "print-stack-safety" StackSafetyGlobalPrinterPass
@module_pass "print<module-debuginfo>" ModuleDebugInfoPrinterPass
@module_pass "recompute-globalsaa" RecomputeGlobalsAAPass
@module_pass "rel-lookup-table-converter" RelLookupTableConverterPass
@module_pass "rewrite-statepoints-for-gc" RewriteStatepointsForGC
@module_pass "rewrite-symbols" RewriteSymbolPass
@module_pass "rpo-function-attrs" ReversePostOrderFunctionAttrsPass
@module_pass "sample-profile" SampleProfileLoaderPass
@module_pass "strip" StripSymbolsPass
@module_pass "strip-dead-debug-info" StripDeadDebugInfoPass
@module_pass "pseudo-probe" SampleProfileProbePass
@module_pass "strip-dead-prototypes" StripDeadPrototypesPass
@module_pass "strip-debug-declare" StripDebugDeclarePass
@module_pass "strip-nondebug" StripNonDebugSymbolsPass
@module_pass "strip-nonlinetable-debuginfo" StripNonLineTableDebugInfoPass
@static if version() < v"20"
    @module_pass "synthetic-counts-propagation" SyntheticCountsPropagation
end
@static if version() < v"19"
    @module_pass "trigger-crash" TriggerCrashPass
else
    @module_pass "trigger-crash-module" TriggerCrashModulePass
end
@module_pass "verify" VerifierPass
@module_pass "view-callgraph" CallGraphViewerPass
@module_pass "wholeprogramdevirt" WholeProgramDevirtPass
@module_pass "dfsan" DataFlowSanitizerPass
@module_pass "module-inline" ModuleInlinerPass
@module_pass "tsan-module" ModuleThreadSanitizerPass
@module_pass "sancov-module" SanitizerCoveragePass
@module_pass "memprof-module" ModuleMemProfilerPass
@static if version() < v"20"
    @module_pass "poison-checking" PoisonCheckingPass
end
@module_pass "pseudo-probe-update" PseudoProbeUpdatePass
@module_pass "loop-extract" LoopExtractorPass
@module_pass "hwasan" HWAddressSanitizerPass
@static if version() < v"16"
    @module_pass "asan-module" AddressSanitizerPass
else
    @module_pass "asan" AddressSanitizerPass
end
@static if version() < v"16"
    @function_pass "msan" MemorySanitizerPass
else
    @module_pass "msan" MemorySanitizerPass
end
@module_pass "internalize" InternalizePass false
function InternalizePass(; preserved_gvs::Vector=String[], kwargs...)
    kwargs = [kwargs...]

    # map a single `preserved_gvs` to many `preserve_gv` options
    for gv in preserved_gvs
        push!(kwargs, :preserve_gv => gv)
    end

    "internalize" * kwargs_to_params(kwargs)
end

# Helper for extension-point callback passes (not part of general pass sweep).
# Back-ported by LLVMExtra to LLVM 17..21; supported upstream from LLVM 22 on.
function ep_callbacks_pass(name; opt_level=0)
    name * kwargs_to_params(Dict{Symbol,Any}(Symbol("O$opt_level") => true))
end

# module callbacks
@static if version() >= v"17"
@vocabulary Passes PipelineStartCallbacks, PipelineEarlySimplificationCallbacks,
                   OptimizerEarlyCallbacks, OptimizerLastCallbacks
PipelineStartCallbacks(; opt_level=0) =
    ep_callbacks_pass("pipeline-start-callbacks"; opt_level)
PipelineEarlySimplificationCallbacks(; opt_level=0) =
    ep_callbacks_pass("pipeline-early-simplification-callbacks"; opt_level)
OptimizerEarlyCallbacks(; opt_level=0) =
    ep_callbacks_pass("optimizer-early-callbacks"; opt_level)
OptimizerLastCallbacks(; opt_level=0) =
    ep_callbacks_pass("optimizer-last-callbacks"; opt_level)
end

# CGSCC passes

@cgscc_pass "argpromotion" ArgumentPromotionPass
@cgscc_pass "invalidate<all>" InvalidateAllAnalysesPass
@cgscc_pass "function-attrs" PostOrderFunctionAttrsPass
@cgscc_pass "attributor-cgscc" AttributorCGSCCPass
@cgscc_pass "openmp-opt-cgscc" OpenMPOptCGSCCPass
@cgscc_pass "no-op-cgscc" NoOpCGSCCPass
@cgscc_pass "inline" InlinerPass
@cgscc_pass "coro-split" CoroSplitPass

# CGSCC callbacks
@static if version() >= v"17"
@vocabulary Passes CGSCCOptimizerLateCallbacks
CGSCCOptimizerLateCallbacks(; opt_level=0) =
    ep_callbacks_pass("cgscc-optimizer-late-callbacks"; opt_level)
end

# function passes

@function_pass "aa-eval" AAEvaluator
@function_pass "adce" ADCEPass
@function_pass "add-discriminators" AddDiscriminatorsPass
@function_pass "aggressive-instcombine" AggressiveInstCombinePass
@function_pass "assume-builder" AssumeBuilderPass
@function_pass "assume-simplify" AssumeSimplifyPass
@function_pass "alignment-from-assumptions" AlignmentFromAssumptionsPass
@function_pass "annotation-remarks" AnnotationRemarksPass
@function_pass "bdce" BDCEPass
@function_pass "bounds-checking" BoundsCheckingPass
@function_pass "break-crit-edges" BreakCriticalEdgesPass
@function_pass "callsite-splitting" CallSiteSplittingPass
@function_pass "consthoist" ConstantHoistingPass
@function_pass "constraint-elimination" ConstraintEliminationPass
@function_pass "chr" ControlHeightReductionPass
@function_pass "coro-elide" CoroElidePass
@function_pass "correlated-propagation" CorrelatedValuePropagationPass
@function_pass "dce" DCEPass
@function_pass "dfa-jump-threading" DFAJumpThreadingPass
@function_pass "div-rem-pairs" DivRemPairsPass
@function_pass "dse" DSEPass
@function_pass "dot-cfg" CFGPrinterPass
@function_pass "dot-cfg-only" CFGOnlyPrinterPass
@function_pass "dot-dom" DomPrinter
@function_pass "dot-dom-only" DomOnlyPrinter
@function_pass "dot-post-dom" PostDomPrinter
@function_pass "dot-post-dom-only" PostDomOnlyPrinter
@function_pass "view-dom" DomViewer
@function_pass "view-dom-only" DomOnlyViewer
@function_pass "view-post-dom" PostDomViewer
@function_pass "view-post-dom-only" PostDomOnlyViewer
# LLVM only registers this pass since LLVM 21, but LLVMExtra makes it available on all
# supported versions
@function_pass "expand-reductions" ExpandReductionsPass
@function_pass "fix-irreducible" FixIrreduciblePass
@static if version() < v"19"
    @function_pass "flattencfg" FlattenCFGPass
else
    @function_pass "flatten-cfg" FlattenCFGPass
end
@function_pass "make-guards-explicit" MakeGuardsExplicitPass
@function_pass "gvn-hoist" GVNHoistPass
@function_pass "gvn-sink" GVNSinkPass
@function_pass "helloworld" HelloWorldPass
@static if version() >= v"19"
    @function_pass "trigger-crash-function" TriggerCrashFunctionPass
end
@function_pass "infer-address-spaces" InferAddressSpacesPass
@function_pass "instcombine" InstCombinePass false
function InstCombinePass(; kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)
    if version() >= v"18"
        # XXX: LLVM "helpfully" enables fixpoint verification by default when using the C API
        #      https://github.com/llvm/llvm-project/blob/3c3fb357a0ed4dbf640bdb6c61db2a430f7eb298/llvm/lib/Passes/PassBuilder.cpp#L1034-L1036
        #      https://github.com/llvm/llvm-project/issues/92648
        kwargs[:verify_fixpoint] = get(kwargs, :verify_fixpoint, false)
    end
    "instcombine" * kwargs_to_params(kwargs)
end
@function_pass "instcount" InstCountPass
@function_pass "instsimplify" InstSimplifyPass
@function_pass "invalidate<all>" InvalidateAllAnalysesPass
@function_pass "irce" IRCEPass
@function_pass "float2int" Float2IntPass
@function_pass "no-op-function" NoOpFunctionPass
@function_pass "libcalls-shrinkwrap" LibCallsShrinkWrapPass
@function_pass "lint" LintPass
@function_pass "inject-tli-mappings" InjectTLIMappings
@function_pass "instnamer" InstructionNamerPass
@static if version() < v"19"
    @function_pass "loweratomic" LowerAtomicPass
else
    @function_pass "lower-atomic" LowerAtomicPass
end
@function_pass "lower-expect" LowerExpectIntrinsicPass
@function_pass "lower-guard-intrinsic" LowerGuardIntrinsicPass
@function_pass "lower-constant-intrinsics" LowerConstantIntrinsicsPass
@function_pass "lower-widenable-condition" LowerWidenableConditionPass
@function_pass "guard-widening" GuardWideningPass
@function_pass "load-store-vectorizer" LoadStoreVectorizerPass
@function_pass "loop-simplify" LoopSimplifyPass
@function_pass "loop-sink" LoopSinkPass
@static if version() < v"19"
    @function_pass "lowerinvoke" LowerInvokePass
    @function_pass "lowerswitch" LowerSwitchPass
else
    @function_pass "lower-invoke" LowerInvokePass
    @function_pass "lower-switch" LowerSwitchPass
end
@function_pass "mem2reg" PromotePass
@function_pass "memcpyopt" MemCpyOptPass
@function_pass "mergeicmps" MergeICmpsPass
@function_pass "mergereturn" UnifyFunctionExitNodesPass
@function_pass "nary-reassociate" NaryReassociatePass
@function_pass "newgvn" NewGVNPass
@function_pass "jump-threading" JumpThreadingPass
@function_pass "partially-inline-libcalls" PartiallyInlineLibCallsPass
@function_pass "lcssa" LCSSAPass
@function_pass "loop-data-prefetch" LoopDataPrefetchPass
@function_pass "loop-load-elim" LoopLoadEliminationPass
@function_pass "loop-fusion" LoopFusePass
@function_pass "loop-distribute" LoopDistributePass
@function_pass "loop-versioning" LoopVersioningPass
@function_pass "objc-arc" ObjCARCOptPass
@function_pass "objc-arc-contract" ObjCARCContractPass
@function_pass "objc-arc-expand" ObjCARCExpandPass
@function_pass "pgo-memop-opt" PGOMemOPSizeOpt
@function_pass "print" PrintFunctionPass
@function_pass "print<assumptions>" AssumptionPrinterPass
@function_pass "print<block-freq>" BlockFrequencyPrinterPass
@function_pass "print<branch-prob>" BranchProbabilityPrinterPass
@function_pass "print<cost-model>" CostModelPrinterPass
@function_pass "print<cycles>" CycleInfoPrinterPass
@function_pass "print<da>" DependenceAnalysisPrinterPass
@static if version() < v"17"
    @function_pass "print<divergence>" DivergenceAnalysisPrinterPass
end
@function_pass "print<domtree>" DominatorTreePrinterPass
@function_pass "print<postdomtree>" PostDominatorTreePrinterPass
@function_pass "print<delinearization>" DelinearizationPrinterPass
@function_pass "print<demanded-bits>" DemandedBitsPrinterPass
@function_pass "print<domfrontier>" DominanceFrontierPrinterPass
@function_pass "print<func-properties>" FunctionPropertiesPrinterPass
@function_pass "print<inline-cost>" InlineCostAnnotationPrinterPass
@function_pass "print<loops>" LoopPrinterPass
@function_pass "print<memoryssa>" MemorySSAPrinterPass
@function_pass "print<memoryssa-walker>" MemorySSAWalkerPrinterPass
@function_pass "print<phi-values>" PhiValuesPrinterPass
@function_pass "print<regions>" RegionInfoPrinterPass
@function_pass "print<scalar-evolution>" ScalarEvolutionPrinterPass
@function_pass "print<stack-safety-local>" StackSafetyPrinterPass
@function_pass "print-alias-sets" AliasSetsPrinterPass
@function_pass "print-predicateinfo" PredicateInfoPrinterPass
@function_pass "print-mustexecute" MustExecutePrinterPass
@function_pass "print-memderefs" MemDerefPrinterPass
@static if version() < v"16"
    @loop_pass "print-access-info" LoopAccessInfoPrinterPass
else
    @function_pass "print<access-info>" LoopAccessInfoPrinterPass
end
@function_pass "reassociate" ReassociatePass
@function_pass "redundant-dbg-inst-elim" RedundantDbgInstEliminationPass
@function_pass "reg2mem" RegToMemPass
@function_pass "scalarize-masked-mem-intrin" ScalarizeMaskedMemIntrinPass
@function_pass "scalarizer" ScalarizerPass
@function_pass "separate-const-offset-from-gep" SeparateConstOffsetFromGEPPass
@function_pass "sccp" SCCPPass
@function_pass "sink" SinkingPass
@function_pass "slp-vectorizer" SLPVectorizerPass
@function_pass "slsr" StraightLineStrengthReducePass
@function_pass "speculative-execution" SpeculativeExecutionPass
@function_pass "sroa" SROAPass
@function_pass "strip-gc-relocates" StripGCRelocates
@function_pass "structurizecfg" StructurizeCFGPass
@function_pass "tailcallelim" TailCallElimPass
@function_pass "unify-loop-exits" UnifyLoopExitsPass
@function_pass "vector-combine" VectorCombinePass
@function_pass "verify" VerifierPass
@function_pass "verify<domtree>" DominatorTreeVerifierPass
@function_pass "verify<loops>" LoopVerifierPass
@function_pass "verify<memoryssa>" MemorySSAVerifierPass
@function_pass "verify<regions>" RegionInfoVerifierPass
@function_pass "verify<safepoint-ir>" SafepointIRVerifierPass
@function_pass "verify<scalar-evolution>" ScalarEvolutionVerifierPass
@function_pass "view-cfg" CFGViewerPass
@function_pass "view-cfg-only" CFGOnlyViewerPass
@static if version() < v"20"
    @function_pass "tlshoist" TLSVariableHoistPass
end
@function_pass "transform-warning" WarnMissedTransformationsPass
@function_pass "tsan" ThreadSanitizerPass
@function_pass "memprof" MemProfilerPass
@function_pass "early-cse" EarlyCSEPass
@function_pass "ee-instrument" EntryExitInstrumenterPass
@function_pass "lower-matrix-intrinsics" LowerMatrixIntrinsicsPass
@function_pass "loop-unroll" LoopUnrollPass false
function LoopUnrollPass(; opt_level=0, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)
    kwargs[Symbol("O$opt_level")] = true
    "loop-unroll" * kwargs_to_params(kwargs)
end
@function_pass "simplifycfg" SimplifyCFGPass
@function_pass "loop-vectorize" LoopVectorizePass
@function_pass "mldst-motion" MergedLoadStoreMotionPass
@function_pass "gvn" GVNPass
@function_pass "print<stack-lifetime>" StackLifetimePrinterPass

# Function pass callbacks
@static if version() >= v"17"
@vocabulary Passes PeepholeCallbacks, ScalarOptimizerLateCallbacks, VectorizerStartCallbacks
PeepholeCallbacks(; opt_level=0) =
    ep_callbacks_pass("peephole-callbacks"; opt_level)
ScalarOptimizerLateCallbacks(; opt_level=0) =
    ep_callbacks_pass("scalar-optimizer-late-callbacks"; opt_level)
VectorizerStartCallbacks(; opt_level=0) =
    ep_callbacks_pass("vectorizer-start-callbacks"; opt_level)
@static if version() >= v"21"
    @vocabulary Passes VectorizerEndCallbacks
    VectorizerEndCallbacks(; opt_level=0) =
        ep_callbacks_pass("vectorizer-end-callbacks"; opt_level)
end
end # version() >= v"17"
# loop nest passes

@loop_pass "loop-flatten" LoopFlattenPass
@loop_pass "loop-interchange" LoopInterchangePass
@loop_pass "loop-unroll-and-jam" LoopUnrollAndJamPass
@loop_pass "no-op-loopnest" NoOpLoopNestPass

# loop passes

@loop_pass "canon-freeze" CanonicalizeFreezeInLoopsPass
@loop_pass "dot-ddg" DDGDotPrinterPass
@loop_pass "invalidate<all>" InvalidateAllAnalysesPass
@loop_pass "loop-idiom" LoopIdiomRecognizePass
@loop_pass "loop-instsimplify" LoopInstSimplifyPass
@loop_pass "loop-rotate" LoopRotatePass
@loop_pass "no-op-loop" NoOpLoopPass
@loop_pass "print" PrintLoopPass
@loop_pass "loop-deletion" LoopDeletionPass
@loop_pass "loop-simplifycfg" LoopSimplifyCFGPass
@loop_pass "loop-reduce" LoopStrengthReducePass
@loop_pass "indvars" IndVarSimplifyPass
@loop_pass "loop-unroll-full" LoopFullUnrollPass
@loop_pass "print<ddg>" DDGAnalysisPrinterPass
@loop_pass "print<iv-users>" IVUsersPrinterPass
@loop_pass "print<loopnest>" LoopNestPrinterPass
@loop_pass "print<loop-cache-cost>" LoopCachePrinterPass
@loop_pass "loop-predication" LoopPredicationPass
@loop_pass "guard-widening" GuardWideningPass
@loop_pass "loop-bound-split" LoopBoundSplitPass
@static if version() < v"19"
    @loop_pass "loop-reroll" LoopRerollPass
end
@loop_pass "loop-versioning-licm" LoopVersioningLICMPass
@loop_pass "simple-loop-unswitch" SimpleLoopUnswitchPass
@loop_pass "licm" LICMPass
@loop_pass "lnicm" LNICMPass

# loop callbacks
@static if version() >= v"17"
@vocabulary Passes LateLoopOptimizationsCallbacks, LoopOptimizerEndCallbacks
LateLoopOptimizationsCallbacks(; opt_level=0) =
    ep_callbacks_pass("late-loop-optimizations-callbacks"; opt_level)
LoopOptimizerEndCallbacks(; opt_level=0) =
    ep_callbacks_pass("loop-optimizer-end-callbacks"; opt_level)
end


## alias analyses

@doc (@doc PassManager)
struct AAManager <: AbstractPassManager
    passes::Vector{String}

    AAManager() = new([])
end

Base.string(pb::AAManager) = join(pb.passes, ",")

function add!(pb::PassBuilder, aa::AAManager)
    push!(pb.aa_passes, string(aa))
    return pb
end
add!(pm::AAManager, aa::AAManager) =
    error("Alias analyses can only be added to the top-level pass builder")

macro aa_pass(pass_name, class_name)
    define_pass(__module__, pass_name, class_name)
end

@aa_pass "basic-aa" BasicAA
@aa_pass "objc-arc-aa" ObjCARCAA
@aa_pass "scev-aa" SCEVAA
@aa_pass "scoped-noalias-aa" ScopedNoAliasAA
@aa_pass "tbaa" TypeBasedAA


## pipelines

@vocabulary Passes DefaultPipeline

function DefaultPipeline(; opt_level=0, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)

    # `opt_level` => `O` flag (which is mandatory)
    kwargs[Symbol("O$opt_level")] = true

    "default" * kwargs_to_params(kwargs)
end
