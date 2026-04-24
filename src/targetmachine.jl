## target machine

export TargetMachine, dispose,
       target, triple, cpu, features, asm_verbosity!, normalize,
       emit, add_transform_info!, add_library_info!
export JITTargetMachine

"""
    TargetMachine

Primary interface to the complete machine description for the target machine.

All target-specific information should be accessible through this interface.
"""
@checked struct TargetMachine
    ref::API.LLVMTargetMachineRef
end

Base.unsafe_convert(::Type{API.LLVMTargetMachineRef}, tm::TargetMachine) = mark_use(tm).ref

"""
    TargetMachine(t::Target, triple::String,
                  [cpu::String], [features::String],
                  [optlevel::LLVMCodeGenOptLevel],
                  [reloc::LLVMRelocMode],
                  [code::LLVMCodeModel])

Create a target machine for the given target, triple, CPU, and features.

This object needs to be disposed of using [`dispose`](@ref).
"""
function TargetMachine(t::Target, triple::String, cpu::String="", features::String="";
                       optlevel::API.LLVMCodeGenOptLevel=API.LLVMCodeGenLevelDefault,
                       reloc::API.LLVMRelocMode=API.LLVMRelocDefault,
                       code::API.LLVMCodeModel=API.LLVMCodeModelDefault)
    ref = API.LLVMCreateTargetMachine(t, triple, cpu, features, optlevel, reloc, code)
    if ref === C_NULL
        throw(ArgumentError("Target $t does not have a target machine"))
    end
    mark_alloc(TargetMachine(ref))
end

"""
    dispose(tm::TargetMachine)

Dispose of the given target machine.
"""
dispose(tm::TargetMachine) = mark_dispose(API.LLVMDisposeTargetMachine, tm)

function TargetMachine(f::Core.Function, args...; kwargs...)
    tm = TargetMachine(args...; kwargs...)
    try
        f(tm)
    finally
        dispose(tm)
    end
end

"""
    target(tm::TargetMachine)

Get the target of the given target machine.
"""
target(tm::TargetMachine) = Target(API.LLVMGetTargetMachineTarget(tm))

"""
    triple(tm::TargetMachine)

Get the triple of the given target machine.
"""
triple(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineTriple(tm))

"""
    triple()

Get the default target triple.
"""
triple() = unsafe_message(API.LLVMGetDefaultTargetTriple())

"""
    normalize(triple::String)

Normalize the given target triple.
"""
normalize(triple::String) = unsafe_message(API.LLVMNormalizeTargetTriple(triple))

"""
    cpu(tm::TargetMachine)

Get the CPU of the given target machine.
"""
cpu(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineCPU(tm))

"""
    features(tm::TargetMachine)

Get the feature string of the given target machine.
"""
features(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineFeatureString(tm))

"""
    asm_verbosity!(tm::TargetMachine, verbose::Bool)

Set the verbosity of the target machine's assembly output.
"""
asm_verbosity!(tm::TargetMachine, verbose::Bool) =
    API.LLVMSetTargetMachineAsmVerbosity(tm, verbose)

"""
    emit(tm::TargetMachine, mod::Module, filetype::LLVMCodeGenFileType) -> UInt8[]

Generate code for the given module using the target machine, returning the binary data.
If assembly code was requested, the binary data can be converted back using `String`.
"""
function emit(tm::TargetMachine, mod::Module, filetype::API.LLVMCodeGenFileType)
    out_error = Ref{Cstring}()
    out_membuf = Ref{API.LLVMMemoryBufferRef}()
    status = API.LLVMTargetMachineEmitToMemoryBuffer(tm, mod, filetype,
                                                     out_error, out_membuf) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    membuf = mark_alloc(MemoryBuffer(out_membuf[]))
    try
        convert(Vector{UInt8}, membuf)
    finally
        dispose(membuf)
    end
end

"""
    emit(tm::TargetMachine, mod::Module, filetype::LLVMCodeGenFileType, path::String)

Generate code for the given module using the target machine, writing it to the given file.
"""
function emit(tm::TargetMachine, mod::Module, filetype::API.LLVMCodeGenFileType, path::String)
    out_error = Ref{Cstring}()
    status = API.LLVMTargetMachineEmitToFile(tm, mod, path, filetype, out_error) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return nothing
end

"""
    add_transform_info!(pm::PassManager, [tm::TargetMachine])

Add target-specific analysis passes to the given pass manager.
"""
function add_transform_info!(pm::PassManager, tm::Union{Nothing,TargetMachine}=nothing)
    if tm !== nothing
        API.LLVMAddAnalysisPasses(tm, pm)
    else
        API.LLVMAddGenericAnalysisPasses(pm)
    end
end

"""
    add_library_info!(pm::PassManager, triple::String)

Add target-specific library information to the given pass manager.
"""
add_library_info!(pm::PassManager, triple::String) =
    API.LLVMAddTargetLibraryInfoByTriple(triple, pm)

"""
    JITTargetMachine(; triple=LLVM.triple(), cpu="", features="",
                     optlevel=API.LLVMCodeGenLevelDefault)

Create a target machine suitable for JIT compilation with the ORC JIT.
"""
function JITTargetMachine(triple = LLVM.triple(),
                          cpu = "", features = "";
                          optlevel = API.LLVMCodeGenLevelDefault)

    # Force ELF on windows,
    # Note: Without this call to normalize Orc get's confused
    #       and chooses the x86_64 SysV ABI on Win x64
    triple = LLVM.normalize(triple)
    if Sys.iswindows()
        triple *= "-elf"
    end
    target = LLVM.Target(triple=triple)
    @debug "Configuring OrcJIT with" triple cpu features optlevel

    TargetMachine(target, triple, cpu, features;
                       optlevel,
                       reloc = API.LLVMRelocStatic, # Generate simpler code for JIT
                       code = API.LLVMCodeModelJITDefault) # Required to init TM as JIT
end
