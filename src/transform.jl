## pass manager builder

export PassManagerBuilder, dispose,
       optlevel!, sizelevel!,
       unit_at_a_time!, unroll_loops!, simplify_libcalls!, inliner!,
       populate!

@checked struct PassManagerBuilder
    ref::API.LLVMPassManagerBuilderRef
end

Base.unsafe_convert(::Type{API.LLVMPassManagerBuilderRef}, pmb::PassManagerBuilder) = mark_use(pmb).ref

PassManagerBuilder() = mark_alloc(PassManagerBuilder(API.LLVMPassManagerBuilderCreate()))

dispose(pmb::PassManagerBuilder) = mark_dispose(API.LLVMPassManagerBuilderDispose, pmb)

function PassManagerBuilder(f::Core.Function)
    pmb = PassManagerBuilder()
    try
        f(pmb)
    finally
        dispose(pmb)
    end
end

# 0 = -O0, 1 = -O1, 2 = -O2, 3 = -O3
optlevel!(pmb::PassManagerBuilder, level::Integer) =
    API.LLVMPassManagerBuilderSetOptLevel(pmb, level)

# 0 = none, 1 = -Os, 2 = -Oz
sizelevel!(pmb::PassManagerBuilder, level::Integer) =
    API.LLVMPassManagerBuilderSetSizeLevel(pmb, level)

unit_at_a_time!(pmb::PassManagerBuilder, flag::Bool=true) =
    API.LLVMPassManagerBuilderSetDisableUnitAtATime(pmb, !flag)

unroll_loops!(pmb::PassManagerBuilder, flag::Bool=true) =
    API.LLVMPassManagerBuilderSetDisableUnrollLoops(pmb, !flag)

simplify_libcalls!(pmb::PassManagerBuilder, flag::Bool=true) =
    API.LLVMPassManagerBuilderSetDisableSimplifyLibCalls(pmb, !flag)

inliner!(pmb::PassManagerBuilder, threshold::Integer) =
    API.LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, threshold)

populate!(fpm::FunctionPassManager, pmb::PassManagerBuilder) =
    API.LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, fpm)

populate!(mpm::ModulePassManager, pmb::PassManagerBuilder) =
    API.LLVMPassManagerBuilderPopulateModulePassManager(pmb, mpm)


## auxiliary

function define_transforms(transforms, available=true; exported=true)
    for transform in transforms
        api_fname = Symbol(:LLVM, :Add, transform, :Pass)

        # deconstruct the camel-casing to get a nice function name
        str = string(transform)
        groups = Vector{SubString}()
        i = 1
        while i <= length(str)
            j = i
            while j < length(str) && all(isuppercase, str[i:j+1])
                # upper-case part
                j += 1
            end
            k = j
            while j < length(str) && all(islowercase, str[k+1:j+1])
                # optional lower-case part
                j += 1
            end

            push!(groups, SubString(str, i, j))
            i = j+1
        end
        jl_fname = Symbol(join(lowercase.(groups), '_'), '!')

        if available
            exported && @eval export $jl_fname
            @eval begin
                $jl_fname(pm::PassManager) = API.$api_fname(pm)
            end
        else
            # generate a run-time error?
        end
    end

    return nothing
end


## scalar transformations

define_transforms([
    :AggressiveDCE, :BitTrackingDCE, :AlignmentFromAssumptions,
    :DeadStoreElimination, :Scalarizer, :MergedLoadStoreMotion, :GVN, :IndVarSimplify,
    :InstructionCombining, :JumpThreading, :LICM, :LoopDeletion, :LoopIdiom, :LoopRotate,
    :LoopReroll, :LoopUnroll, :MemCpyOpt, :PartiallyInlineLibCalls,
    :LowerSwitch, :PromoteMemoryToRegister, :Reassociate, :SCCP, :ScalarReplAggregates,
    :SimplifyLibCalls, :TailCallElimination, :ConstantPropagation, :DemoteMemoryToRegister,
    :Verifier, :CorrelatedValuePropagation, :EarlyCSE, :EarlyCSEMemSSA,
    :LowerExpectIntrinsic, :TypeBasedAliasAnalysis, :ScopedNoAliasAA, :BasicAliasAnalysis,
    :MergeFunctions, :SpeculativeExecutionIfHasBranchDivergence, :SimpleLoopUnroll,
    :InductiveRangeCheckElimination, :SimpleLoopUnswitchLegacy,
])
export scalar_repl_aggregates!, scalar_repl_aggregates_ssa!, cfgsimplification!

scalar_repl_aggregates!(pm::PassManager, threshold::Integer) =
    API.LLVMAddScalarReplAggregatesPassWithThreshold(pm, Cint(threshold))

scalar_repl_aggregates_ssa!(pm::PassManager) = API.LLVMAddScalarReplAggregatesPassSSA(pm)

define_transforms([:DCE])

define_transforms([:DivRemPairs, :LoopDistribute, :LoopFuse, :LoopLoadElimination])

define_transforms([:InstructionSimplify])

if version() >= v"12"
    cfgsimplification!(pm::PassManager;
                       bonus_inst_threshold=1,
                       forward_switch_cond_to_phi=false,
                       convert_switch_to_lookup_table=false,
                       need_canonical_loop=true,
                       hoist_common_insts=false,
                       sink_common_insts=false,
                       simplify_cond_branch=true,
                       fold_two_entry_phi_node=true) =
        API.LLVMAddCFGSimplificationPass2(pm, bonus_inst_threshold,
                                          forward_switch_cond_to_phi,
                                          convert_switch_to_lookup_table,
                                          need_canonical_loop,
                                          hoist_common_insts,
                                          sink_common_insts,
                                          simplify_cond_branch,
                                          fold_two_entry_phi_node)
else
    define_transforms([:CFGSimplification])
end

## vectorization transformations

define_transforms([:LoopVectorize, :SLPVectorize])

define_transforms([:LoadStoreVectorizer])

## interprocedural transformations

define_transforms([
    :ConstantMerge, :DeadArgElimination, :FunctionAttrs,
    :FunctionInlining, :AlwaysInliner, :GlobalDCE, :GlobalOptimizer, :IPConstantPropagation,
    :IPSCCP, :StripDeadPrototypes, :StripSymbols
])

if version() < v"16"
    define_transforms([:PruneEH])
end

## codegen passes

define_transforms([:ExpandReductions])

## other

export internalize!

internalize!(pm::PassManager, allbutmain::Bool=true) =
    API.LLVMAddInternalizePass(pm, allbutmain)

internalize!(pm::PassManager, exports::Vector{String}) =
    API.LLVMAddInternalizePassWithExportList(pm, exports, Csize_t(length(exports)))
