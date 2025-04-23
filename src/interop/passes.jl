# Julia's LLVM passes and pipelines

using ..LLVM: @pipeline, @module_pass, @function_pass, @loop_pass

@module_pass "CPUFeatures" CPUFeaturesPass
@module_pass "RemoveNI" RemoveNIPass
@module_pass "RemoveJuliaAddrspaces" RemoveJuliaAddrspacesPass
@module_pass "RemoveAddrspaces" RemoveAddrspacesPass
@static if VERSION < v"1.11.0-DEV.208"
    @module_pass "FinalLowerGC" FinalLowerGCPass
end

struct MultiVersioningPassOptions
    external::Bool
end
MultiVersioningPassOptions(; external::Bool=false) = MultiVersioningPassOptions(external)
Base.string(options::MultiVersioningPassOptions) = options.external ? "<external>" : ""
@module_pass "JuliaMultiVersioning" MultiVersioningPass MultiVersioningPassOptions

struct LowerPTLSPassOptions
    imaging::Bool
end
LowerPTLSPassOptions(; imaging::Bool=false) = LowerPTLSPassOptions(imaging)
Base.string(options::LowerPTLSPassOptions) = options.imaging ? "<imaging>" : ""
@module_pass "LowerPTLSPass" LowerPTLSPass LowerPTLSPassOptions

@function_pass "DemoteFloat16" DemoteFloat16Pass
@static if VERSION < v"1.12.0-DEV.1390"
@function_pass "CombineMulAdd" CombineMulAddPass
end
@function_pass "LateLowerGCFrame" LateLowerGCPass
@function_pass "AllocOpt" AllocOptPass
@function_pass "PropagateJuliaAddrspaces" PropagateJuliaAddrspacesPass
@static if VERSION < v"1.13.0-DEV.36"
    @function_pass "LowerExcHandlers" LowerExcHandlersPass
end
@static if VERSION >= v"1.11.0-DEV.208"
    @function_pass "FinalLowerGC" FinalLowerGCPass
end

struct GCInvariantVerifierPassOptions
    strong::Bool
end
GCInvariantVerifierPassOptions(; strong::Bool=false) =
    GCInvariantVerifierPassOptions(strong)
Base.string(options::GCInvariantVerifierPassOptions) = options.strong ? "<strong>" : ""
@function_pass "GCInvariantVerifier" GCInvariantVerifierPass GCInvariantVerifierPassOptions

@loop_pass "JuliaLICM" JuliaLICMPass
@loop_pass "LowerSIMDLoop" LowerSIMDLoopPass

Base.@kwdef struct JuliaPipelineOptions
    opt_level::Int = Base.JLOptions().opt_level

    lower_intrinsics::Union{Nothing,Bool} = nothing
    dump_native::Union{Nothing,Bool} = nothing
    external_use::Union{Nothing,Bool} = nothing
    llvm_only::Union{Nothing,Bool} = nothing
    always_inline::Union{Nothing,Bool} = nothing
    enable_early_simplifications::Union{Nothing,Bool} = nothing
    enable_early_optimizations::Union{Nothing,Bool} = nothing
    enable_scalar_optimizations::Union{Nothing,Bool} = nothing
    enable_loop_optimizations::Union{Nothing,Bool} = nothing
    enable_vector_pipeline::Union{Nothing,Bool} = nothing
    remove_ni::Union{Nothing,Bool} = nothing
    cleanup::Union{Nothing,Bool} = nothing
    warn_missed_transformations::Union{Nothing,Bool} = nothing
end

function Base.string(options::JuliaPipelineOptions)
    final_options = String[]
    push!(final_options, "level=$(options.opt_level)")

    function handle_option(name::Symbol)
        val = getfield(options, name)
        if val !== nothing
            push!(final_options, val ? String(name) : "no_$name")
        end
    end
    handle_option(:lower_intrinsics)
    handle_option(:dump_native)
    handle_option(:external_use)
    handle_option(:llvm_only)
    if VERSION >= v"1.12.0-DEV.1029" ||         # JuliaLang/julia#55407
       (v"1.11.0-rc3" <= VERSION < v"1.12-")    # backport
        handle_option(:always_inline)
        handle_option(:enable_early_simplifications)
        handle_option(:enable_early_optimizations)
        handle_option(:enable_scalar_optimizations)
        handle_option(:enable_loop_optimizations)
        handle_option(:enable_vector_pipeline)
        handle_option(:remove_ni)
        handle_option(:cleanup)
        handle_option(:warn_missed_transformations)
    end

    "<" * join(final_options, ";") * ">"
end

@pipeline "julia" JuliaPipeline JuliaPipelineOptions

# XXX: if we go through the PassBuilder parser, Julia won't insert the PassBuilder's
# callbacks in the right spots. that's why Julia also provides `jl_build_newpm_pipeline`.
# is this still true? can we fix that, and continue using the PassBuilder interface?

Base.@deprecate_binding JuliaPipelinePass JuliaPipeline
Base.@deprecate_binding JuliaPipelinePassOptions JuliaPipelineOptions false


## legacy passes

@static if VERSION < v"1.11.0-DEV.428"

export alloc_opt!, barrier_noop!, gc_invariant_verifier!, lower_exc_handlers!,
       combine_mul_add!, multi_versioning!, propagate_julia_addrsp!, lower_ptls!,
       lower_simdloop!, late_lower_gc_frame!, final_lower_gc!, remove_julia_addrspaces!,
       demote_float16!, remove_ni!, julia_licm!, cpu_features!

alloc_opt!(pm::PassManager) = API.LLVMAddAllocOptPass(pm)

barrier_noop!(pm::PassManager) = API.LLVMAddBarrierNoopPass(pm)

gc_invariant_verifier!(pm::PassManager, strong::Bool=false) =
    API.LLVMAddGCInvariantVerifierPass(pm, strong)

lower_exc_handlers!(pm::PassManager) = API.LLVMAddLowerExcHandlersPass(pm)

combine_mul_add!(pm::PassManager) = API.LLVMAddCombineMulAddPass(pm)

multi_versioning!(pm::PassManager) = API.LLVMAddMultiVersioningPass(pm)

propagate_julia_addrsp!(pm::PassManager) = API.LLVMAddPropagateJuliaAddrspaces(pm)

lower_ptls!(pm::PassManager, imaging_mode::Bool=false) =
    API.LLVMAddLowerPTLSPass(pm, imaging_mode)

lower_simdloop!(pm::PassManager) = API.LLVMAddLowerSimdLoopPass(pm)

late_lower_gc_frame!(pm::PassManager) = API.LLVMAddLateLowerGCFramePass(pm)

final_lower_gc!(pm::PassManager) = API.LLVMAddFinalLowerGCPass(pm)

remove_julia_addrspaces!(pm::PassManager) = API.LLVMAddRemoveJuliaAddrspacesPass(pm)

demote_float16!(pm::PassManager) = API.LLVMAddDemoteFloat16Pass(pm)

remove_ni!(pm::PassManager) = API.LLVMAddRemoveNIPass(pm)

julia_licm!(pm::PassManager) = API.LLVMAddJuliaLICMPass(pm)

cpu_features!(pm::PassManager) = API.LLVMAddCPUFeaturesPass(pm)

end
