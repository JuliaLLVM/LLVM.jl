## instruction debug location

# extends the `debug_location` / `debug_location!` functions for IRBuilder.

function debug_location(inst::Instruction)
    ref = API.LLVMInstructionGetDebugLoc(inst)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end

debug_location!(inst::Instruction, loc::DILocation) =
    API.LLVMInstructionSetDebugLoc(inst, loc)
debug_location!(inst::Instruction) =
    API.LLVMInstructionSetDebugLoc(inst, C_NULL)

@property Instruction debug_location (inst, loc::Union{DILocation,Nothing}) ->
    loc === nothing ? debug_location!(inst) : debug_location!(inst, loc)


## other

@vocabulary IR DEBUG_METADATA_VERSION, strip_debuginfo!

"""
    DEBUG_METADATA_VERSION()

The current debug info version number, as supported by LLVM.
"""
DEBUG_METADATA_VERSION() = API.LLVMDebugMetadataVersion()

debug_metadata_version(mod::Module) = Int(API.LLVMGetModuleDebugMetadataVersion(mod))

@property Module debug_metadata_version

"""
    strip_debuginfo!(mod::Module)

Strip the debug information from the given module.
"""
strip_debuginfo!(mod::Module) = API.LLVMStripModuleDebugInfo(mod)

function subprogram(func::Function)
    ref = API.LLVMGetSubprogram(func)
    ref==C_NULL ? nothing : Metadata(ref)::DISubprogram
end

# `subprogram!` is the `DIBuilder` function that creates a subprogram. `LLVMSetSubprogram`
# can't clear the subprogram, which is the function's `!dbg` attachment.
@property Function subprogram (func, sp::Union{DISubprogram,Nothing}) ->
    sp === nothing ? API.LLVMGlobalEraseMetadata(func, MD_dbg) :
                     API.LLVMSetSubprogram(func, sp)
