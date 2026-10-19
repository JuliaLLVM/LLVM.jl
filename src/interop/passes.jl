# Julia's LLVM passes and pipelines

using ..LLVM: @module_pass, @function_pass, @loop_pass

@module_pass "CPUFeatures" CPUFeaturesPass julia=true
@module_pass "RemoveNI" RemoveNIPass julia=true
@module_pass "RemoveJuliaAddrspaces" RemoveJuliaAddrspacesPass julia=true
@module_pass "RemoveAddrspaces" RemoveAddrspacesPass julia=true
@static if VERSION < v"1.11.0-DEV.208"
    @module_pass "FinalLowerGC" FinalLowerGCPass julia=true
end
@module_pass "JuliaMultiVersioning" MultiVersioningPass julia=true
@module_pass "LowerPTLSPass" LowerPTLSPass julia=true

@function_pass "DemoteFloat16" DemoteFloat16Pass julia=true
@static if VERSION < v"1.12.0-DEV.1390"
@function_pass "CombineMulAdd" CombineMulAddPass julia=true
end
@function_pass "LateLowerGCFrame" LateLowerGCPass julia=true
@function_pass "AllocOpt" AllocOptPass julia=true
@function_pass "PropagateJuliaAddrspaces" PropagateJuliaAddrspacesPass julia=true
@static if VERSION < v"1.13.0-DEV.36"
    @function_pass "LowerExcHandlers" LowerExcHandlersPass julia=true
end
@static if VERSION >= v"1.11.0-DEV.208"
    @function_pass "FinalLowerGC" FinalLowerGCPass julia=true
end
@static if VERSION >= v"1.13.0-DEV.321"
    @function_pass "ExpandAtomicModify" ExpandAtomicModifyPass julia=true
end
@function_pass "GCInvariantVerifier" GCInvariantVerifierPass julia=true

@loop_pass "JuliaLICM" JuliaLICMPass julia=true
@loop_pass "LowerSIMDLoop" LowerSIMDLoopPass julia=true

LLVM.document_passes(@__MODULE__)

# Julia's parser uses underscores in option names, unlike ordinary LLVM passes.
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
    LLVM.check_julia_library("Julia's pipeline")
    kwargs = Dict{Symbol, Any}(kwargs)

    # `opt_level` => `level` for consistency with LLVM's passes
    if opt_level !== nothing
        kwargs[:level] = opt_level
    end

    "julia" * kwargs_to_params(kwargs)
end

# Julia builds its own pipeline because its textual parser passes no extension points
# to `buildPipeline`, so it does not inherit the surrounding PassBuilder's callbacks.

