module LLVM

using Preferences
using Unicode
using Printf
using Libdl

using CEnum


## source code includes

include("base.jl")
include("version.jl")

# we don't embed the full path to LLVM, because the location might be different at run time.
const libllvm = basename(String(Base.libllvm_path()))
const libllvm_version = Base.libllvm_version

module API
using CEnum
using Preferences

using ..LLVM

# library handles
using ..LLVM: libllvm
using LLVMExtra_jll
if has_preference(LLVM, "libLLVMExtra")
    const libLLVMExtra = load_preference(LLVM, "libLLVMExtra")
else
    if isdefined(LLVMExtra_jll, :libLLVMExtra)
        import LLVMExtra_jll: libLLVMExtra
    end
end

# auto-generated wrappers
let
    if version().major < 15
        error("LLVM.jl only supports LLVM 15 and later.")
    end
    dir = if version().major > 20
        @warn "LLVM.jl has not been tested with LLVM versions newer than 20."
        joinpath(@__DIR__, "..", "lib", "20")
    else
        joinpath(@__DIR__, "..", "lib", string(version().major))
    end
    @assert isdir(dir)

    include(joinpath(dir, "libLLVM.jl"))
    include(joinpath(dir, "libLLVM_extra.jl"))
end
include(joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))

end # module API

has_oldpm() = LLVM.version() < v"17"

# helpers
include("debug.jl")

# LLVM API wrappers
include("support.jl")
include("buffer.jl")
include("init.jl")
include("core.jl")
include("linker.jl")
include("irbuilder.jl")
include("analysis.jl")
include("pass.jl")
include("passmanager.jl")
include("execution.jl")
include("target.jl")
include("targetmachine.jl")
include("datalayout.jl")
if has_oldpm()
    include("transform.jl")
end
include("debuginfo.jl")
include("utils.jl")
include("orc.jl")
include("newpm.jl")

# high-level functionality
include("state.jl")
include("interop.jl")

include("deprecated.jl")


## initialization

function __init__()
    @debug "Using LLVM $libllvm_version at $(Base.libllvm_path())"

    # sanity checks
    if !isdefined(API, :libLLVMExtra)
        @error """LLVM extensions library unavailable for your platform:
                    $(Base.BinaryPlatforms.triplet(API.LLVMExtra_jll.host_platform))
                  LLVM.jl will not be functional.

                  If you are using a custom version of LLVM, try building a
                  custom version of LLVMExtra_jll using `deps/build_local.jl`"""
    end
    if libllvm_version != Base.libllvm_version
        # this checks that the precompilation image isn't being used
        # after having upgraded Julia and the contained LLVM library.
        @error """LLVM.jl was precompiled for LLVM $libllvm_version, whereas you are now using LLVM $(Base.libllvm_version).
                  Please re-compile LLVM.jl."""
    end
    if version() !== runtime_version()
        # this is probably caused by a combination of USE_SYSTEM_LLVM
        # and an LLVM upgrade without recompiling Julia.
        @error """Julia was compiled for LLVM $(version()), whereas you are now using LLVM $(runtime_version()).
                  Please re-compile Julia and LLVM.jl (but note that USE_SYSTEM_LLVM is not a supported configuration)."""
    end

    _install_handlers()
    atexit(report_leaks)
end

end
