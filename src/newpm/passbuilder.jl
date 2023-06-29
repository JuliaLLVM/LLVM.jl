export PassBuilder

export dispose, register!, cross_register_proxies!, parse!

@checked struct PassBuilder
    ref::API.LLVMPassBuilderRef
    roots::Vector{Any}
end

Base.unsafe_convert(::Type{API.LLVMPassBuilderRef}, pb::PassBuilder) = pb.ref

PassBuilder(tm::TargetMachine) = PassBuilder(API.LLVMCreatePassBuilder(tm, C_NULL), [])
PassBuilder(tm::TargetMachine, pic::PassInstrumentationCallbacks) = PassBuilder(API.LLVMCreatePassBuilder(tm, pic), [])
PassBuilder() = PassBuilder(API.LLVMCreatePassBuilder(C_NULL, C_NULL), [])

dispose(pb::PassBuilder) = API.LLVMDisposePassBuilder(pb)

function PassBuilder(f::Core.Function, args...; kwargs...)
    pb = PassBuilder(args...; kwargs...)
    try
        f(pb)
    finally
        dispose(pb)
    end
end

register!(pb::PassBuilder, am::ModuleAnalysisManager) = API.LLVMPassBuilderRegisterModuleAnalyses(pb, am)
register!(pb::PassBuilder, am::CGSCCAnalysisManager) = API.LLVMPassBuilderRegisterCGSCCAnalyses(pb, am)
register!(pb::PassBuilder, am::FunctionAnalysisManager) = API.LLVMPassBuilderRegisterFunctionAnalyses(pb, am)
register!(pb::PassBuilder, am::LoopAnalysisManager) = API.LLVMPassBuilderRegisterLoopAnalyses(pb, am)

cross_register_proxies!(pb::PassBuilder, lam::LoopAnalysisManager, fam::FunctionAnalysisManager, cgam::CGSCCAnalysisManager, mam::ModuleAnalysisManager) = API.LLVMPassBuilderCrossRegisterProxies(pb, lam, fam, cgam, mam)

function register!(pb::PassBuilder, lam::LoopAnalysisManager, fam::FunctionAnalysisManager, cgam::CGSCCAnalysisManager, mam::ModuleAnalysisManager)
    register!(pb, lam)
    register!(pb, fam)
    register!(pb, cgam)
    register!(pb, mam)
    cross_register_proxies!(pb, lam, fam, cgam, mam)
end

parse!(pb::PassBuilder, pm::NewPMModulePassManager, s::String) = @check API.LLVMPassBuilderParseModulePassPipeline(pb, pm, s, length(s))
parse!(pb::PassBuilder, pm::NewPMCGSCCPassManager, s::String) = @check API.LLVMPassBuilderParseCGSCCPassPipeline(pb, pm, s, length(s))
parse!(pb::PassBuilder, pm::NewPMFunctionPassManager, s::String) = @check API.LLVMPassBuilderParseFunctionPassPipeline(pb, pm, s, length(s))
parse!(pb::PassBuilder, pm::NewPMLoopPassManager, s::String) = @check API.LLVMPassBuilderParseLoopPassPipeline(pb, pm, s, length(s))
