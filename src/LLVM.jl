module LLVM

using Preferences
using Unicode
using Printf
using Libdl

using CEnum
using PrecompileTools


## source code includes

include("base.jl")
include("version.jl")

# we don't embed the full path to LLVM, because the location might be different at run time.
const libllvm = basename(String(Base.libllvm_path()))
const libllvm_version = Base.libllvm_version

module API
using CEnum
using Preferences

# library handles
import ..LLVM
using ..LLVM: libllvm, version
using LLVMExtra_jll
if has_preference(LLVM, "libLLVMExtra")
    const libLLVMExtra = load_preference(LLVM, "libLLVMExtra")
elseif isdefined(LLVMExtra_jll, :libLLVMExtra)
    import LLVMExtra_jll: libLLVMExtra
else
    error("""LLVM.jl requires the LLVM extensions library, which LLVMExtra_jll does not provide for your platform:
               $(Base.BinaryPlatforms.triplet(LLVMExtra_jll.host_platform))
             If you are using a custom version of LLVM, build the library using `deps/build_local.jl`.""")
end

# auto-generated wrappers. these only convert their arguments and call into the library, so
# we don't specialize them on the concrete type of LLVM.jl objects (which would compile them
# for every combination of, e.g., value types). that makes calls with abstractly-typed
# arguments resolve statically, and inlining them makes the argument conversions do so too.
function inline_wrapper(ex)
    if Meta.isexpr(ex, :function) && Meta.isexpr(ex.args[2], :block)
        pushfirst!(ex.args[2].args, Expr(:meta, :inline))
    end
    return ex
end
@nospecialize
let
    if version().major < 15
        error("LLVM.jl only supports LLVM 15 and later.")
    end
    dir = if version().major > 22
        @warn "LLVM.jl has not been tested with LLVM versions newer than 22."
        joinpath(@__DIR__, "..", "lib", "22")
    else
        joinpath(@__DIR__, "..", "lib", string(version().major))
    end
    @assert isdir(dir)

    include(inline_wrapper, joinpath(dir, "libLLVM.jl"))
    include(inline_wrapper, joinpath(dir, "libLLVM_extra.jl"))
end
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))
@specialize

# atomicrmw operations that older C APIs lack, numbered as in newer ones, so that they can be
# named on every LLVM version (use `LLVM.isavailable` to check whether LLVM supports them)
for (name, val) in ((:LLVMAtomicRMWBinOpUIncWrap, 15), (:LLVMAtomicRMWBinOpUDecWrap, 16),
                    (:LLVMAtomicRMWBinOpUSubCond, 17), (:LLVMAtomicRMWBinOpUSubSat, 18),
                    (:LLVMAtomicRMWBinOpFMaximum, 19), (:LLVMAtomicRMWBinOpFMinimum, 20),
                    (:LLVMAtomicRMWBinOpFMaximumNum, 21), (:LLVMAtomicRMWBinOpFMinimumNum, 22))
    isdefined(@__MODULE__, name) || @eval const $name = LLVMAtomicRMWBinOp($val)
end

end # module API
@public API

include("enums.jl")

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
include("atomics.jl")
include("analysis.jl")
include("pass.jl")
include("passmanager.jl")
include("execution.jl")
include("target.jl")
include("targetmachine.jl")
include("datalayout.jl")
include("disasm.jl")
if has_oldpm()
    include("transform.jl")
end
include("debuginfo.jl")
include("utils.jl")
include("orc.jl")
include("targetinfo.jl")
include("newpm.jl")

# high-level functionality
include("state.jl")
include("vocabularies.jl")
include("interop.jl")
@public Interop

include("precompile.jl")


## initialization

function __init__()
    @debug "Using LLVM $libllvm_version at $(Base.libllvm_path())"

    # sanity checks
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

    register_eh_frame_stubs()
    _install_handlers()
    atexit(report_leaks)
end

end
