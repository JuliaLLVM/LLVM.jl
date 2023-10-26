export NewPMPassManager, NewPMModulePassManager, NewPMCGSCCPassManager,
       NewPMFunctionPassManager, NewPMLoopPassManager

export dispose, add!

abstract type NewPMPassManager end

@checked struct NewPMModulePassManager <: NewPMPassManager
    ref::API.LLVMModulePassManagerRef
    roots::Vector{Any}
    pb::Any # Union{PassBuilder, Nothing}
end
@checked struct NewPMCGSCCPassManager <: NewPMPassManager
    ref::API.LLVMCGSCCPassManagerRef
    roots::Vector{Any}
    pb::Any # Union{PassBuilder, Nothing}
end
@checked struct NewPMFunctionPassManager <: NewPMPassManager
    ref::API.LLVMFunctionPassManagerRef
    roots::Vector{Any}
    pb::Any # Union{PassBuilder, Nothing}
end
@checked struct NewPMLoopPassManager <: NewPMPassManager
    ref::API.LLVMLoopPassManagerRef
    roots::Vector{Any}
    pb::Any # Union{PassBuilder, Nothing}
end

Base.unsafe_convert(::Type{API.LLVMModulePassManagerRef}, pm::NewPMModulePassManager) = pm.ref
Base.unsafe_convert(::Type{API.LLVMCGSCCPassManagerRef}, pm::NewPMCGSCCPassManager) = pm.ref
Base.unsafe_convert(::Type{API.LLVMFunctionPassManagerRef}, pm::NewPMFunctionPassManager) = pm.ref
Base.unsafe_convert(::Type{API.LLVMLoopPassManagerRef}, pm::NewPMLoopPassManager) = pm.ref

NewPMModulePassManager(pb) =
    NewPMModulePassManager(API.LLVMCreateNewPMModulePassManager(), [], pb)
NewPMCGSCCPassManager(pb) =
    NewPMCGSCCPassManager(API.LLVMCreateNewPMCGSCCPassManager(), [], pb)
NewPMFunctionPassManager(pb) =
    NewPMFunctionPassManager(API.LLVMCreateNewPMFunctionPassManager(), [], pb)
NewPMLoopPassManager(pb) =
    NewPMLoopPassManager(API.LLVMCreateNewPMLoopPassManager(), [], pb)

dispose(pm::NewPMModulePassManager) = API.LLVMDisposeNewPMModulePassManager(pm)
dispose(pm::NewPMCGSCCPassManager) = API.LLVMDisposeNewPMCGSCCPassManager(pm)
dispose(pm::NewPMFunctionPassManager) = API.LLVMDisposeNewPMFunctionPassManager(pm)
dispose(pm::NewPMLoopPassManager) = API.LLVMDisposeNewPMLoopPassManager(pm)

NewPMModulePassManager() = NewPMModulePassManager(nothing)
NewPMCGSCCPassManager() = NewPMCGSCCPassManager(nothing)
NewPMFunctionPassManager() = NewPMFunctionPassManager(nothing)
NewPMLoopPassManager() = NewPMLoopPassManager(nothing)

function NewPMModulePassManager(f::Core.Function, args...; kwargs...)
    am = NewPMModulePassManager(args...; kwargs...)
    try
        f(am)
    finally
        dispose(am)
    end
end
function NewPMCGSCCPassManager(f::Core.Function, args...; kwargs...)
    am = NewPMCGSCCPassManager(args...; kwargs...)
    try
        f(am)
    finally
        dispose(am)
    end
end
function NewPMFunctionPassManager(f::Core.Function, args...; kwargs...)
    am = NewPMFunctionPassManager(args...; kwargs...)
    try
        f(am)
    finally
        dispose(am)
    end
end
function NewPMLoopPassManager(f::Core.Function, args...; kwargs...)
    am = NewPMLoopPassManager(args...; kwargs...)
    try
        f(am)
    finally
        dispose(am)
    end
end

function add!(pm::NewPMModulePassManager, pm2::NewPMModulePassManager)
    API.LLVMMPMAddMPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMCGSCCPassManager, pm2::NewPMCGSCCPassManager)
    API.LLVMCGPMAddCGPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMFunctionPassManager, pm2::NewPMFunctionPassManager)
    API.LLVMFPMAddFPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMLoopPassManager, pm2::NewPMLoopPassManager)
    API.LLVMLPMAddLPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end

function add!(pm::NewPMModulePassManager, pm2::NewPMCGSCCPassManager)
    API.LLVMMPMAddCGPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMModulePassManager, pm2::NewPMFunctionPassManager)
    API.LLVMMPMAddFPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMCGSCCPassManager, pm2::NewPMFunctionPassManager)
    API.LLVMCGPMAddFPM(pm, pm2)
    append!(pm.roots, pm2.roots)
end
function add!(pm::NewPMFunctionPassManager, pm2::NewPMLoopPassManager,
              UseMemorySSA::Bool=false)
    API.LLVMFPMAddLPM(pm, pm2, UseMemorySSA)
    append!(pm.roots, pm2.roots)
end

function add!(f::Core.Function, pm::NewPMModulePassManager, ::Type{NewPMModulePassManager})
    pm2 = NewPMModulePassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMCGSCCPassManager, ::Type{NewPMCGSCCPassManager})
    pm2 = NewPMCGSCCPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMFunctionPassManager, ::Type{NewPMFunctionPassManager})
    pm2 = NewPMFunctionPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMLoopPassManager, ::Type{NewPMLoopPassManager})
    pm2 = NewPMLoopPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMModulePassManager, ::Type{NewPMCGSCCPassManager})
    pm2 = NewPMCGSCCPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMModulePassManager, ::Type{NewPMFunctionPassManager})
    pm2 = NewPMFunctionPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMCGSCCPassManager, ::Type{NewPMFunctionPassManager})
    pm2 = NewPMFunctionPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2)
    finally
        dispose(pm2)
    end
end

function add!(f::Core.Function, pm::NewPMFunctionPassManager, ::Type{NewPMLoopPassManager}, UseMemorySSA::Bool=false)
    pm2 = NewPMLoopPassManager(pm.pb)
    try
        f(pm2)
        add!(pm, pm2, UseMemorySSA)
    finally
        dispose(pm2)
    end
end

run!(pm::NewPMModulePassManager, m::Module, am::ModuleAnalysisManager) =
    PreservedAnalyses(API.LLVMRunNewPMModulePassManager(pm, m, am))

run!(pm::NewPMFunctionPassManager, f::Function, am::FunctionAnalysisManager) =
    PreservedAnalyses(API.LLVMRunNewPMFunctionPassManager(pm, f, am))
