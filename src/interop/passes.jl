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
@static if VERSION >= v"1.13.0-DEV.321"
    @function_pass "ExpandAtomicModify" ExpandAtomicModifyPass
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
"""
    JuliaPipeline(; opt_level=nothing, options...) -> String

Julia's optimization pipeline, as a string for use with [`add!`](@ref LLVM.add!) or
[`run!`](@ref LLVM.run!), for the optimization level `opt_level` (Julia's default if
`nothing`). Other keyword arguments become options of the pipeline, e.g.,
`enable_vector_pipeline=false` or `enable_early_simplifications=false`.
"""
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


