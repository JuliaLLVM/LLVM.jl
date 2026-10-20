## subtarget information

@public SubtargetInfo, dispose, processor_features, hasfeature, hasfeatures

"""
    LLVM.SubtargetInfo

The CPU and features of a target, as LLVM's tables for that target describe them (C++'s
`MCSubtargetInfo`). Enabling a feature also enables the features that it implies, and
disabling a feature also disables the features that imply it; when a feature string sets a
feature more than once, the last setting wins.

# Properties

    sti.triple

The target triple of the subtarget, as a string like `tm.triple` (use [`LLVM.Triple`](@ref)
to interpret it).

    sti.cpu

The name of the CPU. Targets may fill it in, e.g., X86 uses `"generic"` when no CPU is
given.

    sti.features

The feature string of the subtarget, e.g., `"+avx2,-fma"`. Targets may extend the feature
string that the subtarget was created with, e.g., X86 prepends the features that the triple
implies, like `+64bit-mode`.

See also: [`LLVM.hasfeature`](@ref), [`LLVM.hasfeatures`](@ref),
[`LLVM.processor_features`](@ref).
"""
mutable struct SubtargetInfo
    ref::API.LLVMMCSubtargetInfoRef
    # whether this handle owns the subtarget, or the subtarget belongs to a target machine
    owned::Bool
    library::Library
end
@properties SubtargetInfo

argument_library(sti::SubtargetInfo) = sti.library

Base.unsafe_convert(::Type{API.LLVMMCSubtargetInfoRef}, sti::SubtargetInfo) =
    mark_use(sti).ref

"""
    LLVM.SubtargetInfo(target::LLVM.Target, triple; cpu::AbstractString="",
                       features::AbstractString="")
    LLVM.SubtargetInfo(f, target, triple; kwargs...)

Create the subtarget information of the given target for the given triple (an
[`LLVM.Triple`](@ref) or a string), CPU (e.g., `"haswell"`, or `""` for the generic CPU)
and features (e.g., `"+avx2,-fma"`), like C++'s `Target::createMCSubtargetInfo`. The
triple is used as it is, without normalizing it.

LLVM prints a warning to `stderr` for CPUs and features that the target doesn't know about,
and ignores them.

The target has to be registered (see [`LLVM.Target`](@ref), which throws an
`ArgumentError` for a triple whose target isn't), and its machine code layer initialized
(e.g., using `LLVM.InitializeX86TargetMC()`); an `ArgumentError` is thrown otherwise.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form.
"""
function SubtargetInfo(target::Target, triple::Union{Triple,AbstractString};
                       cpu::AbstractString="", features::AbstractString="")
    ref = API.LLVMExtraCreateMCSubtargetInfo(target, string(triple), cpu, features)
    if ref == C_NULL
        throw(ArgumentError("Target $(name(target)) does not provide subtarget information; " *
                            "initialize its machine code layer first (e.g., using " *
                            "`LLVM.InitializeAllTargetMCs()`)"))
    end
    mark_alloc(SubtargetInfo(ref, true, target.library))
end

"""
    LLVM.SubtargetInfo(tm::LLVM.TargetMachine, f::LLVM.Function)

Get the subtarget information that the target machine uses to generate code for the given
function, like C++'s `TargetMachine::getSubtargetImpl(F)`. For most targets, the
function's `"target-cpu"` and `"target-features"` attributes replace the target machine's
CPU and features when they are present.

The subtarget belongs to the target machine, and can be used as long as the target machine
is alive. It doesn't need to be disposed of; disposing of it does nothing.
"""
function SubtargetInfo(tm::TargetMachine, f::Function)
    ref = API.LLVMExtraGetTargetMachineSubtargetInfo(tm, f)
    if ref == C_NULL
        throw(ArgumentError("Target machine for $(triple(tm)) does not have subtargets"))
    end
    mark_alloc(SubtargetInfo(ref, false, tm.library); owner=tm)
end

SubtargetInfo(f::Core.Function, args...; kwargs...) =
    with_disposal(f, SubtargetInfo(args...; kwargs...))

"""
    dispose(sti::LLVM.SubtargetInfo)

Dispose of the given subtarget information, unless it belongs to a target machine.
"""
function dispose(sti::SubtargetInfo)
    sti.owned || return
    mark_dispose(API.LLVMExtraDisposeMCSubtargetInfo, sti)
end

triple(sti::SubtargetInfo) = unsafe_string(API.LLVMExtraMCSubtargetInfoGetTriple(sti))

cpu(sti::SubtargetInfo) = unsafe_string(API.LLVMExtraMCSubtargetInfoGetCPU(sti))

features(sti::SubtargetInfo) =
    unsafe_string(API.LLVMExtraMCSubtargetInfoGetFeatureString(sti))

@property SubtargetInfo triple
@property SubtargetInfo cpu
@property SubtargetInfo features

"""
    LLVM.processor_features(sti::LLVM.SubtargetInfo) -> Vector{String}

Get the names of the features that the target of the subtarget knows about, sorted by name,
like the keys of C++'s `MCSubtargetInfo::getAllProcessorFeatures()`. These are the names
that [`LLVM.hasfeature`](@ref) and [`LLVM.hasfeatures`](@ref) accept.
"""
function processor_features(sti::SubtargetInfo)
    n = API.LLVMExtraMCSubtargetInfoGetNumProcessorFeatures(sti)
    return String[unsafe_string(API.LLVMExtraMCSubtargetInfoGetProcessorFeatureName(sti, i))
                  for i in 0:n-1]
end

"""
    LLVM.hasfeature(sti::LLVM.SubtargetInfo, name::AbstractString)

Check whether the subtarget has the feature of the given name (e.g., `"avx2"`, without a
`+` or `-` flag) enabled, including when it is enabled because another feature implies it.
Throws an `ArgumentError` if the target doesn't have a feature of that name (see
[`LLVM.processor_features`](@ref)).
"""
function hasfeature(sti::SubtargetInfo, name::AbstractString)
    enabled = Ref{API.LLVMBool}()
    if !Bool(API.LLVMExtraMCSubtargetInfoLookupFeature(sti, name, enabled))
        hint = startswith(name, r"[+-]") ? " (pass the name without the flag)" : ""
        throw(ArgumentError("'$name' is not a feature of this target$hint"))
    end
    return Bool(enabled[])
end

"""
    LLVM.hasfeatures(sti::LLVM.SubtargetInfo, flags::AbstractString)

Check whether the subtarget matches all flags of a comma-separated feature string, e.g.,
`"+avx2,-fma"`: whether it has every feature that is flagged with `+` enabled, and every
feature flagged with `-` disabled. This is C++'s `MCSubtargetInfo::checkFeatures` as of
LLVM 21, which checks every flag by itself, on every version of LLVM. Earlier versions of
`checkFeatures` also checked the features that a flagged feature implies, and ignored
unknown features, while later versions abort on them.

Throws an `ArgumentError` if a flag doesn't start with `+` or `-`, or names a feature that
the target doesn't have (see [`LLVM.processor_features`](@ref)). Empty flags are ignored, so
an empty string matches every subtarget.
"""
function hasfeatures(sti::SubtargetInfo, flags::AbstractString)
    # check every flag before evaluating any of them, so that invalid flags are reported
    # regardless of the subtarget's features
    checks = map(split(flags, ','; keepempty=false)) do flag
        startswith(flag, r"[+-]") ||
            throw(ArgumentError("Feature flag '$flag' should start with '+' or '-'"))
        hasfeature(sti, flag[nextind(flag, 1):end]) == startswith(flag, '+')
    end
    return all(checks)
end
