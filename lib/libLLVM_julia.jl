# Julia wrapper for source: julia/src/llvm-api.cpp

function LLVMAddAllocOptPass(PM)
    ccall(:LLVMExtraAddAllocOptPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddGCInvariantVerifierPass(PM, Strong)
    ccall(:LLVMExtraAddGCInvariantVerifierPass,Cvoid,(LLVMPassManagerRef,LLVMBool), PM, Strong)
end

function LLVMAddLowerExcHandlersPass(PM)
    ccall(:LLVMExtraAddLowerExcHandlersPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddCombineMulAddPass(PM)
    ccall(:LLVMExtraAddCombineMulAddPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddMultiVersioningPass(PM)
    ccall(:LLVMExtraAddMultiVersioningPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddPropagateJuliaAddrspaces(PM)
    ccall(:LLVMExtraAddPropagateJuliaAddrspaces,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddLowerPTLSPass(PM, imaging_mode)
    ccall(:LLVMExtraAddLowerPTLSPass,Cvoid,(LLVMPassManagerRef,LLVMBool), PM, imaging_mode)
end

function LLVMAddLowerSimdLoopPass(PM)
    ccall(:LLVMExtraAddLowerSimdLoopPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddLateLowerGCFramePass(PM)
    ccall(:LLVMExtraAddLateLowerGCFramePass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddFinalLowerGCPass(PM)
    ccall(:LLVMExtraAddFinalLowerGCPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddRemoveJuliaAddrspacesPass(PM)
    ccall(:LLVMExtraAddRemoveJuliaAddrspacesPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddDemoteFloat16Pass(PM)
    ccall(:LLVMExtraAddDemoteFloat16Pass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddRemoveNIPass(PM)
    ccall(:LLVMExtraAddRemoveNIPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddJuliaLICMPass(PM)
    ccall(:LLVMExtraJuliaLICMPass,Cvoid,(LLVMPassManagerRef,), PM)
end

function LLVMAddCPUFeaturesPass(PM)
    ccall(:LLVMExtraAddCPUFeaturesPass,Cvoid,(LLVMPassManagerRef,), PM)
end

if VERSION >= v"1.10.0-DEV.1622"

function LLVMRegisterJuliaPassBuilderCallbacks(PB)
    ccall(:jl_register_passbuilder_callbacks,Cvoid,(LLVMPassBuilderRef,), PB)
end

Base.@kwdef struct PipelineConfig
    Speedup::Cint
    Size::Cint
    lower_intrinsics::Cint=1
    dump_native::Cint=0
    external_use::Cint=0
    llvm_only::Cint=0
    always_inline::Cint=1
    enable_early_simplifications::Cint=1
    enable_early_optimizations::Cint=1
    enable_scalar_optimizations::Cint=1
    enable_loop_optimizations::Cint=1
    enable_vector_pipeline::Cint=1
    remove_ni::Cint=1
    cleanup::Cint=1
end

if VERSION >= v"1.11.0-DEV.1296"

function LLVMAddJuliaPipelinePass(PM, PB, cfg::PipelineConfig)
    ccall(:jl_build_newpm_pipeline,Cvoid,
          (LLVMModulePassManagerRef,LLVMPassBuilderRef,Ref{PipelineConfig}),
          PM, PB, cfg)
end

else

function LLVMAddJuliaPipelinePass(PM, PB, cfg::PipelineConfig)
    ccall(:jl_build_newpm_pipeline,Cvoid,
          (LLVMModulePassManagerRef,LLVMPassBuilderRef,Cint,Cint,Cint,Cint,Cint,Cint),
          PM, PB, cfg.Speedup, cfg.Size, cfg.lower_intrinsics, cfg.dump_native, cfg.external_use, cfg.llvm_only)
end

end

end


if VERSION >= v"1.10.0-DEV.1395"

mutable struct JLOpaqueJuliaOJIT end

const JuliaOJITRef = Ptr{JLOpaqueJuliaOJIT}

function JLJITGetJuliaOJIT()
    ccall(:JLJITGetJuliaOJIT, JuliaOJITRef, ())
end

function JLJITGetLLVMOrcExecutionSession(JIT)
    ccall(:JLJITGetLLVMOrcExecutionSession, LLVMOrcExecutionSessionRef, (JuliaOJITRef,), JIT)
end

function JLJITGetExternalJITDylib(JIT)
    ccall(:JLJITGetExternalJITDylib, LLVMOrcJITDylibRef, (JuliaOJITRef,), JIT)
end

function JLJITAddObjectFile(JIT, JD, ObjBuffer)
    ccall(:JLJITAddObjectFile, LLVMErrorRef, (JuliaOJITRef, LLVMOrcJITDylibRef, LLVMMemoryBufferRef), JIT, JD, ObjBuffer)
end

function JLJITAddLLVMIRModule(JIT, JD, TSM)
    ccall(:JLJITAddLLVMIRModule, LLVMErrorRef, (JuliaOJITRef, LLVMOrcJITDylibRef,  LLVMOrcThreadSafeModuleRef), JIT, JD, TSM)
end

function JLJITLookup(JIT, Result, Name, ExternalJDOnly)
    ccall(:JLJITLookup, LLVMErrorRef, (JuliaOJITRef, Ptr{LLVMOrcExecutorAddress}, Cstring, Cint), JIT, Result, Name, ExternalJDOnly)
end

function JLJITMangleAndIntern(JIT, UnmangledName)
    ccall(:JLJITMangleAndIntern, LLVMOrcSymbolStringPoolEntryRef, (JuliaOJITRef, Cstring), JIT, UnmangledName)
end

function JLJITGetTripleString(JIT)
    ccall(:JLJITGetTripleString, Cstring, (JuliaOJITRef,), JIT)
end

function JLJITGetGlobalPrefix(JIT)
    ccall(:JLJITGetGlobalPrefix, Cchar, (JuliaOJITRef,), JIT)
end

function JLJITGetDataLayoutString(JIT)
    ccall(:JLJITGetDataLayoutString, Cstring, (JuliaOJITRef,), JIT)
end

function JLJITGetIRCompileLayer(JIT)
    ccall(:JLJITGetIRCompileLayer, LLVMOrcIRCompileLayerRef, (JuliaOJITRef,), JIT)
end

end
