# (new) pass manager interface


## pass managers

export NewPMModulePassManager, NewPMCGSCCPassManager, NewPMFunctionPassManager,
       NewPMLoopPassManager, NewPMAAManager

abstract type AbstractPassManager end

"""
    add!(pm::AbstractPassManager, pass)

Adds a pass or pipeline to a pass builder or pass manager.

The pass or pipeline should be a string or string-convertible object known by LLVM. These
can be constructed by using pass constructors, e.g., `InternalizePass()`, or by manually
specifying names like `default<O3>`.

When using custom passes, remember that they need to be registered with the pass builder
before they can be used.

See also: [`register!`](@ref)
"""
add!(pm::AbstractPassManager, pass) = push!(pm.passes, string(pass))

"""
    NewPMModulePassManager()
    NewPMCGSCCPassManager()
    NewPMFunctionPassManager()
    NewPMLoopPassManager(; use_memory_ssa=false)
    NewPMAAManager()

Create a new pass manager of the specified type. These objects can be used to construct
pass pipelines, by `add!`ing passes to them, and finally `add!`ing them to a parent
pass manager or pass builder.

Creating a pass manager and adding it to a parent manager or builder can be shortened
using a single `add!`:

```julia
add!(parent, NewPMModulePassManager()) do mpm
    add!(mpm, SomeModulePass())
end
```

See also: [`add!`](@ref), [`NewPMPassBuilder`](@ref)
"""
struct NewPMPassManager <: AbstractPassManager
    type::String
    passes::Vector{String}

    NewPMPassManager(type::String) = new(type, [])
end

Base.string(pm::NewPMPassManager) = "$(pm.type)($(join(pm.passes, ",")))"

function add!(f::Base.Callable, parent::AbstractPassManager, nested::AbstractPassManager)
    f(nested)
    if !isempty(nested.passes)
        add!(parent, nested)
    end
end

@doc (@doc NewPMPassManager)
NewPMModulePassManager() = NewPMPassManager("module")

@doc (@doc NewPMPassManager)
NewPMCGSCCPassManager() = NewPMPassManager("cgscc")

@doc (@doc NewPMPassManager)
NewPMFunctionPassManager() = NewPMPassManager("function")

@doc (@doc NewPMPassManager)
NewPMLoopPassManager(; use_memory_ssa=false) =
    NewPMPassManager(use_memory_ssa ? "loop-mssa" : "loop")


## custom passes

# TODO: support for options

export NewPMModulePass, NewPMFunctionPass

"""
    NewPMModulePass(name, callback)
    NewPMFunctionPass(name, callback)

Create a new custom pass. The `name` is a string that will be used to identify the pass
in the pass manager. The `callback` is a function that will be called when the pass is
run. The function should take a single argument, the module or function to be processed,
and return a boolean indicating whether the pass made any changes.

Before using a custom pass, it must be registered with a pass builder using `register!`.

See also: [`register!`](@ref)
"""
struct NewPMCustomPass
  type::Symbol
  name::String
  callback::Any
end

Base.string(pass::NewPMCustomPass) = pass.name

@doc (@doc NewPMCustomPass)
NewPMModulePass(name, callback)   = NewPMCustomPass(:module, name, callback)

@doc (@doc NewPMCustomPass)
NewPMFunctionPass(name, callback) = NewPMCustomPass(:function, name, callback)

function module_callback(ref::API.LLVMModuleRef, thunk::Ptr{Any})
    mod = LLVM.Module(ref)
    f = Base.unsafe_load(thunk)
    f(mod)::Bool
end

function function_callback(ref::API.LLVMValueRef, thunk::Ptr{Any})
    fun = LLVM.Function(ref)
    f = Base.unsafe_load(thunk)
    f(fun)::Bool
end


## pass builder

export NewPMPassBuilder, register!, add!, run!

"""
    NewPMPassBuilder(; verify_each=false, debug_logging=false, pipeline_tuning_kwargs...)

Create a new pass builder. The pass builder is the main object used to construct and run
pass pipelines. The `verify_each` keyword argument enables module verification after each
pass, while `debug_logging` can be used to enable more output. Pass builder objects needs to
be disposed after use.

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
@dispose pb = NewPMPassBuilder(verify_each=true) begin
    register!(pb, SomeCustomPass())
    add!(pb, SomeModulePass())
    add!(pb, NewPMFunctionPassManager()) do fpm
        add!(fpm, SomeFunctionPass())
    end
    run!(pb, mod, tm)
end
```

For quickly running a simple pass or pipeline, a shorthand `run!` method is provided that
obviates the construction of a `NewPMPassBuilder`:

```julia
run!("some-pass", mod, tm; verify_each=true)
```

See also: [`register!`](@ref), [`add!`](@ref), [`run!`](@ref)
"""
mutable struct NewPMPassBuilder <: AbstractPassManager
    opts::API.LLVMPassBuilderOptionsRef
    exts::API.LLVMPassBuilderExtensionsRef
    passes::Vector{String}
    aa_passes::Vector{String}
    custom_passes::Vector{NewPMCustomPass}
end

Base.string(pm::NewPMPassBuilder) = join(pm.passes, ",")

Base.unsafe_convert(::Type{API.LLVMPassBuilderOptionsRef}, pb::NewPMPassBuilder) =
    mark_use(pb).opts

function NewPMPassBuilder(; kwargs...)
    opts = API.LLVMCreatePassBuilderOptions()
    exts = API.LLVMCreatePassBuilderExtensions()
    obj = mark_alloc(NewPMPassBuilder(opts, exts, [], [], []))

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

    return obj
end

function dispose(pb::NewPMPassBuilder)
    API.LLVMDisposePassBuilderOptions(pb.opts)
    API.LLVMDisposePassBuilderExtensions(pb.exts)
    mark_dispose(pb)
end

"""
    register!(pb, custom_pass)

Register a custom pass with the pass builder. This is necessary before the pass can be
used in a pass pipeline.

See also: [`NewPMModulePass`](@ref), [`NewPMFunctionPass`](@ref)
"""
function register!(pb::NewPMPassBuilder, pass::NewPMCustomPass)
    push!(pb.custom_passes, pass)
end

"""
    run!(pb::NewPMPassBuilder, mod::Module, [tm::TargetMachine])
    run!(pipeline::String, mod::Module, [tm::TargetMachine])

Run passes on a module. The passes are specified by a pass builder or a string that
represents a pass pipeline. The target machine is used to optimize the passes.
"""
run!

function run!(pb::NewPMPassBuilder, target::Union{Module,Function}, tm::Union{Nothing,TargetMachine}=nothing)
    isempty(pb.passes) && return
    pipeline = join(pb.passes, ",")
    aa_pipeline = join(pb.aa_passes, ",")

    # XXX: The Base API is too restricted, not supporting custom passes
    #      or Julia's pass registration callback
    #@check API.LLVMRunPasses(mod, string(pb), tm, pb.opts)

    thunks = Vector{Any}(undef, length(pb.custom_passes))
    GC.@preserve thunks aa_pipeline begin
        # register custom passes
        for (i,pass) in enumerate(pb.custom_passes)
            if pass.type === :module
                cb = @cfunction(module_callback, Bool, (API.LLVMModuleRef, Ptr{Any}))
                api = API.LLVMPassBuilderExtensionsRegisterModulePass
            elseif pass.type === :function
                cb = @cfunction(function_callback, Bool, (API.LLVMValueRef, Ptr{Any}))
                api = API.LLVMPassBuilderExtensionsRegisterFunctionPass
            else
                throw(ArgumentError("invalid pass type $(pass.type)"))
            end
            thunks[i] = pass.callback
            api(pb.exts, pass.name, cb, pointer(thunks, i))
        end

        # register Julia passes
        julia_callback = cglobal(:jl_register_passbuilder_callbacks)
        API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(pb.exts, julia_callback)

        # register AA pipeline
        if !isempty(aa_pipeline)
            if version() >= v"20"
                API.LLVMPassBuilderOptionsSetAAPipeline(pb.opts, aa_pipeline)
            else
                API.LLVMPassBuilderExtensionsSetAAPipeline(pb.exts, aa_pipeline)
            end
        end

        if target isa Module
            @check API.LLVMRunJuliaPasses(target, pipeline, something(tm, C_NULL),
                                          pb.opts, pb.exts)
        elseif target isa Function
            @check API.LLVMRunJuliaPassesOnFunction(target, pipeline, something(tm, C_NULL),
                                                    pb.opts, pb.exts)
        end
    end
end

function run!(pass::String, args...; kwargs...)
    @dispose pb=NewPMPassBuilder(; kwargs...) begin
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
        # deprecated option names
        deprecated_bindings = Dict{Symbol, Symbol}(
            :eagerchecks => :eager_checks,
            :trackorigins => :track_origins,
            :onlymandatory => :only_mandatory,
            :reusestorage => :reuse_storage,
            :postinline => :post_inline,
            :full_unroll_max_count => :full_unroll_max,
            :allow_partial => :partial,
            :allow_peeling => :peeling,
            :allow_profile_based_peeling => :profile_peeling,
            :allow_runtime => :runtime,
            :allow_upper_bound => :upper_bound,
            :forward_switch_cond_to_phi => :forward_switch_cond,
            :convert_switch_range_to_icmp => :switch_range_to_icmp,
            :convert_switch_to_lookup_table => :switch_to_lookup,
            :interleaveforcedonly => :interleave_forced_only,
            :vectorizeforcedonly => :vectorize_forced_only,
            :splitfooterbb => :split_footer_bb,
            :allowpre => :pre,
            :allowloadpre => :load_pre,
            :allowloadpresplitbackedge => :split_backedge_load_pre,
            :allowmemdep => :memdep
        )
        if haskey(deprecated_bindings, k)
            new = deprecated_bindings[k]
            Base.depwarn(
                "LLVM pass keyword argument $k is deprecated, use $new instead.",
                k
            )
            k = new
        end

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

function define_pass(pass_name, class_name, define_class=true)
    # don't re-define passes (some work with multiple types of managers,
    # or could be manually-defined)
    if isdefined(LLVM, class_name)
        return
    end

    ex = quote
        export $(esc(class_name))
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
    define_pass(pass_name, class_name, define_class)
end
macro cgscc_pass(pass_name, class_name, define_class=true)
    push!(cgscc_passes, pass_name)
    define_pass(pass_name, class_name, define_class)
end
macro function_pass(pass_name, class_name, define_class=true)
    push!(function_passes, pass_name)
    define_pass(pass_name, class_name, define_class)
end
macro loop_pass(pass_name, class_name, define_class=true)
    push!(loop_passes, pass_name)
    define_pass(pass_name, class_name, define_class)
end

# module passes

@module_pass "always-inline" AlwaysInlinerPass
@module_pass "attributor" AttributorPass
@module_pass "annotation2metadata" Annotation2MetadataPass
@module_pass "openmp-opt" OpenMPOptPass
@module_pass "called-value-propagation" CalledValuePropagationPass
@module_pass "canonicalize-aliases" CanonicalizeAliasesPass
@module_pass "cg-profile" CGProfilePass
@module_pass "check-debugify" NewPMCheckDebugifyPass
@module_pass "constmerge" ConstantMergePass
@module_pass "coro-early" CoroEarlyPass
@module_pass "coro-cleanup" CoroCleanupPass
@module_pass "cross-dso-cfi" CrossDSOCFIPass
@module_pass "deadargelim" DeadArgumentEliminationPass
@module_pass "debugify" NewPMDebugifyPass
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
@module_pass "instrorderfile" InstrOrderFilePass
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
@module_pass "objc-arc-apelim" ObjCARCAPElimPass
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
@module_pass "trigger-crash" TriggerCrashPass
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

# CGSCC passes

@cgscc_pass "argpromotion" ArgumentPromotionPass
@cgscc_pass "invalidate<all>" InvalidateAllAnalysesPass
@cgscc_pass "function-attrs" PostOrderFunctionAttrsPass
@cgscc_pass "attributor-cgscc" AttributorCGSCCPass
@cgscc_pass "openmp-opt-cgscc" OpenMPOptCGSCCPass
@cgscc_pass "no-op-cgscc" NoOpCGSCCPass
@cgscc_pass "inline" InlinerPass
@cgscc_pass "coro-split" CoroSplitPass

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


## alias analyses

@doc (@doc NewPMPassManager)
struct NewPMAAManager <: AbstractPassManager
    passes::Vector{String}

    NewPMAAManager() = new([])
end

Base.string(pb::NewPMAAManager) = join(pb.passes, ",")

add!(pb::NewPMPassBuilder, aa::NewPMAAManager) = push!(pb.aa_passes, string(aa))
add!(pm::NewPMAAManager, aa::NewPMAAManager) =
    error("Alias analyses can only be added to the top-level pass builder")

macro aa_pass(pass_name, class_name)
    define_pass(pass_name, class_name)
end

@aa_pass "basic-aa" BasicAA
@aa_pass "objc-arc-aa" ObjCARCAA
@aa_pass "scev-aa" SCEVAA
@aa_pass "scoped-noalias-aa" ScopedNoAliasAA
@aa_pass "tbaa" TypeBasedAA


## pipelines

export DefaultPipeline

function DefaultPipeline(; opt_level=0, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)

    # `opt_level` => `O` flag (which is mandatory)
    kwargs[Symbol("O$opt_level")] = true

    "default" * kwargs_to_params(kwargs)
end
