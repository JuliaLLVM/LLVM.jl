# Julia's LLVM passes and pipelines

using ..LLVM: @module_pass, @function_pass, @loop_pass

@module_pass "CPUFeatures" CPUFeaturesPass
@module_pass "RemoveNI" RemoveNIPass
@module_pass "RemoveJuliaAddrspaces" RemoveJuliaAddrspacesPass
@module_pass "RemoveAddrspaces" RemoveAddrspacesPass
@static if VERSION < v"1.11.0-DEV.208"
    @module_pass "FinalLowerGC" FinalLowerGCPass
end
@module_pass "JuliaMultiVersioning" MultiVersioningPass
@module_pass "LowerPTLSPass" LowerPTLSPass

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
@function_pass "GCInvariantVerifier" GCInvariantVerifierPass

@loop_pass "JuliaLICM" JuliaLICMPass
@loop_pass "LowerSIMDLoop" LowerSIMDLoopPass

# convert Julia keyword arguments to a Julia/LLVM pass parameter string
# XXX: annoyingly, Julia's LLVM passes use `-` while LLVM uses `_`. Fix this?
function kwargs_to_params(kwargs)
    isempty(kwargs) && return ""

    params = String[]
    for (k, v) in kwargs
        if v isa Bool
            push!(params, v ? k : "no_$k")
        else
            push!(params, "$k=$v")
        end
    end
    "<" * join(params, ";") * ">"
end

export JuliaPipeline
function JuliaPipeline(; opt_level=nothing, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)

    # `opt_level` => `level` for consistency with LLVM's passes
    if opt_level !== nothing
        kwargs[:level] = opt_level
    end

    "julia" * kwargs_to_params(kwargs)
end

# XXX: if we go through the PassBuilder parser, Julia won't insert the PassBuilder's
# callbacks in the right spots. that's why Julia also provides `jl_build_newpm_pipeline`.
# is this still true? can we fix that, and continue using the PassBuilder interface?

Base.@deprecate_binding JuliaPipelinePass JuliaPipeline


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
