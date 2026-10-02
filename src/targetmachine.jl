## target machine

@public TargetMachine, dispose,
        asm_verbosity!, normalize, default_triple, host_cpu_name, host_cpu_features,
        emit
@public JITTargetMachine

"""
    TargetMachine

Primary interface to the complete machine description for the target machine.

All target-specific information should be accessible through this interface.

# Properties

    tm.target

The target of the target machine.

    tm.triple

The target triple of the target machine.

    tm.cpu

The CPU of the target machine.

    tm.features

The feature string of the target machine.

# Ownership

A target machine is consumed by [`TargetMachineBuilder(tm)`](@ref TargetMachineBuilder),
and thus by `LLJIT(; tm)`, which take ownership of it. A consumed target machine can't be
used anymore, and disposing of it does nothing, so that it can be disposed of
unconditionally, e.g., using the do-block form of its constructor.
"""
mutable struct TargetMachine
    ref::API.LLVMTargetMachineRef
    owned::Bool

    function TargetMachine(ref::API.LLVMTargetMachineRef)
        ref == C_NULL && throw(UndefRefError())
        new(ref, true)
    end
end
@properties TargetMachine

Base.unsafe_convert(::Type{API.LLVMTargetMachineRef}, tm::TargetMachine) =
    check_owned(tm).ref

consume!(tm::TargetMachine) = consume_owned!(tm)

"""
    TargetMachine(t::Target, triple::AbstractString; cpu::AbstractString="",
                  features::AbstractString="", opt_level=LLVM.CodeGenOptLevel.Default, reloc=LLVM.RelocMode.Default,
                  code=LLVM.CodeModel.Default)
    TargetMachine(f, t, triple; kwargs...)

Create a target machine for the given target and triple, targeting the given CPU (e.g.,
[`LLVM.host_cpu_name()`](@ref)) and features (e.g., `"+avx2,-sse4a"`), and with the given
optimization level, relocation model and code model.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form.
"""
function TargetMachine(t::Target, triple::AbstractString; cpu::AbstractString="",
                       features::AbstractString="",
                       opt_level::API.LLVMCodeGenOptLevel=API.LLVMCodeGenLevelDefault,
                       reloc::API.LLVMRelocMode=API.LLVMRelocDefault,
                       code::API.LLVMCodeModel=API.LLVMCodeModelDefault)
    ref = API.LLVMCreateTargetMachine(t, triple, cpu, features, opt_level, reloc, code)
    if ref === C_NULL
        throw(ArgumentError("Target $t does not have a target machine"))
    end
    mark_alloc(TargetMachine(ref))
end

"""
    dispose(tm::TargetMachine)

Dispose of the given target machine, unless it has been consumed.
"""
dispose(tm::TargetMachine) = dispose_owned(API.LLVMDisposeTargetMachine, tm)

TargetMachine(f::Core.Function, args...; kwargs...) =
    with_disposal(f, TargetMachine(args...; kwargs...))

target(tm::TargetMachine) = Target(API.LLVMGetTargetMachineTarget(tm))

triple(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineTriple(tm))

"""
    LLVM.default_triple()

Get the default target triple, i.e., the triple of the host that LLVM was configured for.
"""
default_triple() = unsafe_message(API.LLVMGetDefaultTargetTriple())

"""
    LLVM.host_cpu_name()

Get the name of the CPU of the host, e.g., `"znver4"`, for use with a `TargetMachine`.
"""
host_cpu_name() = unsafe_message(API.LLVMGetHostCPUName())

"""
    LLVM.host_cpu_features()

Get the features of the CPU of the host, as a string of comma-separated features that are
enabled (`+feature`) or disabled (`-feature`), for use with a `TargetMachine`.
"""
host_cpu_features() = unsafe_message(API.LLVMGetHostCPUFeatures())

"""
    normalize(triple::AbstractString)

Normalize the given target triple.
"""
normalize(triple::AbstractString) = unsafe_message(API.LLVMNormalizeTargetTriple(triple))

cpu(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineCPU(tm))

features(tm::TargetMachine) = unsafe_message(API.LLVMGetTargetMachineFeatureString(tm))

@property TargetMachine target
@property TargetMachine triple
@property TargetMachine cpu
@property TargetMachine features

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
    ctx = context(mod)
    prepare_diagnostic(ctx)
    out_error = Ref{Cstring}(C_NULL)
    out_membuf = Ref{API.LLVMMemoryBufferRef}(C_NULL)
    status = API.LLVMTargetMachineEmitToMemoryBuffer(tm, mod, filetype,
                                                     out_error, out_membuf) |> Bool
    error = out_error[] == C_NULL ? nothing : unsafe_message(out_error[])
    membuf = out_membuf[] == C_NULL ? nothing : mark_alloc(MemoryBuffer(out_membuf[]))
    try
        check_diagnostic(ctx, status, something(error, "target emission failed"))
        membuf === nothing && throw(LLVMException("target emission returned no buffer"))
        return convert(Vector{UInt8}, membuf)
    finally
        membuf === nothing || dispose(membuf)
    end
end

"""
    emit(tm::TargetMachine, mod::Module, filetype::LLVMCodeGenFileType,
         path::AbstractString)

Generate code for the given module using the target machine, writing it to the given file.
"""
function emit(tm::TargetMachine, mod::Module, filetype::API.LLVMCodeGenFileType,
              path::AbstractString)
    ctx = context(mod)
    prepare_diagnostic(ctx)
    out_error = Ref{Cstring}(C_NULL)
    status = API.LLVMTargetMachineEmitToFile(tm, mod, path, filetype, out_error) |> Bool
    error = out_error[] == C_NULL ? nothing : unsafe_message(out_error[])
    check_diagnostic(ctx, status, something(error, "target emission failed"))

    return nothing
end

"""
    JITTargetMachine(; triple=LLVM.default_triple(), cpu="", features="",
                     opt_level=LLVM.CodeGenOptLevel.Default)
    JITTargetMachine(f; kwargs...)

Create a target machine suitable for JIT compilation with the ORC JIT: like a
[`TargetMachine`](@ref), but with the static relocation model and the JIT's default code
model, and an ELF triple on Windows.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form.
"""
function JITTargetMachine(; triple::AbstractString=LLVM.default_triple(),
                          cpu::AbstractString="",
                          features::AbstractString="",
                          opt_level::API.LLVMCodeGenOptLevel=API.LLVMCodeGenLevelDefault)

    # Force ELF on windows,
    # Note: Without this call to normalize Orc get's confused
    #       and chooses the x86_64 SysV ABI on Win x64
    triple = LLVM.normalize(triple)
    if Sys.iswindows()
        triple *= "-elf"
    end
    target = LLVM.Target(triple=triple)
    @debug "Configuring OrcJIT with" triple cpu features opt_level

    TargetMachine(target, triple; cpu, features, opt_level,
                  reloc = API.LLVMRelocStatic, # Generate simpler code for JIT
                  code = API.LLVMCodeModelJITDefault) # Required to init TM as JIT
end

JITTargetMachine(f::Core.Function; kwargs...) =
    with_disposal(f, JITTargetMachine(; kwargs...))
