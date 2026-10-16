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

# the wrappers are generated for every supported version of LLVM, and merged into a single
# set of definitions by `res/merge.jl`. functions that are not available on every version
# check the version first, and enums whose members are not available on every version, or
# whose values differ between versions, are translated by the wrappers taking or returning
# them (see `res/ABI.md` for the differences).

@inline function check_available(name::Symbol, since::Int, until::Union{Int,Nothing}=nothing)
    major = Int(version().major)
    if major < since || (until !== nothing && major >= until)
        throw_unavailable(name, since, until)
    end
end
@noinline function throw_unavailable(name, since, until)
    supported = until === nothing ? "$since and later" : "$since to $(until-1)"
    error("$name is not available on LLVM $(version()) (only on LLVM $supported)")
end

# translation tables of an enum for a version of LLVM, generated for enums with members that
# are not available on every version, or with values that differ. `nothing` means that the
# values of the C API are those defined by the wrappers.
abi_tables(::Type, major::Integer) = nothing

@inline function to_abi(::Type{T}, x) where {T}
    x = x isa T ? x : T(x)
    tables = abi_tables(T, version().major)
    tables === nothing && return x
    val = Integer(x)
    0 <= val < length(tables.to) || return x
    raw = @inbounds tables.to[val+1]
    raw < 0 && throw_unavailable(x)
    return T(raw)
end
@noinline throw_unavailable(x) =
    throw(ArgumentError("$x is not available on LLVM $(version())"))

@inline function from_abi(::Type{T}, x::T) where {T}
    tables = abi_tables(T, version().major)
    tables === nothing && return x
    raw = Integer(x)
    0 <= raw < length(tables.from) || return x
    return T(@inbounds tables.from[raw+1])
end

@nospecialize
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM.jl"))
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM_extra.jl"))
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))
@specialize

if version().major < first(WRAPPED_VERSIONS)
    error("LLVM.jl only supports LLVM $(first(WRAPPED_VERSIONS)) and later.")
elseif version().major > last(WRAPPED_VERSIONS)
    @warn "LLVM.jl has not been tested with LLVM versions newer than $(last(WRAPPED_VERSIONS))."
end

end # module API
@public API

include("enums.jl")

# helpers
include("debug.jl")

# LLVM API wrappers
include("support.jl")
include("buffer.jl")
include("init.jl")
include("range.jl")
include("core.jl")
include("linker.jl")
include("irbuilder.jl")
include("atomics.jl")
include("execution.jl")
include("target.jl")
include("targetmachine.jl")
include("datalayout.jl")
include("analysis.jl")
include("disasm.jl")
include("debuginfo.jl")
include("utils.jl")
include("orc.jl")
include("targetinfo.jl")
include("passes.jl")
define_properties()

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
    Base.Experimental.register_error_hint(position_hint, MethodError)
    atexit(report_leaks)
end

end
