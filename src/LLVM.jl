module LLVM

using Preferences
using Unicode
using Printf
using Libdl

using CEnum
using PrecompileTools
using ScopedValues


## source code includes

include("base.jl")
include("library.jl")
include("version.jl")

using LLVMExtra_jll

# the library that LLVM.jl uses by default is the one Julia uses, unless the `libLLVM`
# preference selects another one, e.g., a standalone library built by
# `deps/build_standalone.jl` to test LLVM.jl with another version of LLVM. such a library
# also provides LLVMExtra.
const libllvm_preference = @load_preference("libLLVM", nothing)

# whether the default library is Julia's. code that depends on the version of LLVM at
# compile time (`@static if version() ...`) uses the version of the default library.
const julia_llvm = libllvm_preference === nothing

# the default library, for code that calls it directly, e.g., `ccall((:LLVMFoo,
# LLVM.API.libllvm), ...)`. we don't embed the full path to Julia's LLVM, because the
# location might be different at run time.
const libllvm = julia_llvm ? basename(String(Base.libllvm_path())) : libllvm_preference

# the LLVMExtra library for Julia's LLVM, or `nothing` if there is none. like the path of
# Julia's LLVM, this is only known at run time.
function julia_extra_path()
    if has_preference(LLVM, "libLLVMExtra")
        load_preference(LLVM, "libLLVMExtra")
    elseif isdefined(LLVMExtra_jll, :libLLVMExtra)
        LLVMExtra_jll.libLLVMExtra_path
    else
        nothing
    end
end

const julia_library = Library(:julia, "", "", Base.libllvm_version)

function load_julia!(lib::Library)
    extra = julia_extra_path()
    if extra === nothing
        error("""LLVM.jl requires the LLVM extensions library, which LLVMExtra_jll does not provide for your platform:
                   $(Base.BinaryPlatforms.triplet(LLVMExtra_jll.host_platform))
                 If you are using a custom version of LLVM, build the library using `deps/build_local.jl`.""")
    end
    lib.path = realpath(String(Base.libllvm_path()))
    lib.extra_path = realpath(extra)
    load!(lib)
end

const default_library = if julia_llvm
    julia_library
else
    let path = realpath(libllvm_preference), version = library_version(path)
        Library(Symbol(:llvm, version.major), path, path, version)
    end
end

# the wrappers depend on the version of another library, so recompile when it changes
julia_llvm || include_dependency(libllvm_preference)

const libllvm_version = default_library.version

const current_library = ScopedValue{Library}(default_library)

module API
using CEnum

import ..LLVM
using ..LLVM: Library, @llvmapi, libllvm

# auto-generated wrappers. these only convert their arguments and call into the library, so
# we don't specialize them on the concrete type of LLVM.jl objects (which would compile them
# for every combination of, e.g., value types). that makes calls with abstractly-typed
# arguments resolve statically, and inlining them makes the argument conversions do so too
# (`@llvmapi` marks them as inlined).
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

@inline function check_available(lib::Library, name::Symbol, since::Int,
                                 until::Union{Int,Nothing}=nothing)
    major = Int(lib.version.major)
    if major < since || (until !== nothing && major >= until)
        throw_unavailable(lib, name, since, until)
    end
end
@noinline function throw_unavailable(lib, name, since, until)
    supported = until === nothing ? "$since and later" : "$since to $(until-1)"
    error("$name is not available on LLVM $(lib.version) (only on LLVM $supported)")
end

# translation tables of an enum for a version of LLVM, generated for enums with members that
# are not available on every version, or with values that differ. `nothing` means that the
# values of the C API are those defined by the wrappers.
abi_tables(::Type, major::Integer) = nothing

@inline function to_abi(lib::Library, ::Type{T}, x) where {T}
    x = x isa T ? x : T(x)
    tables = abi_tables(T, lib.version.major)
    tables === nothing && return x
    val = Integer(x)
    0 <= val < length(tables.to) || return x
    raw = @inbounds tables.to[val+1]
    raw < 0 && throw_unavailable(lib, x)
    return T(raw)
end
@noinline throw_unavailable(lib, x) =
    throw(ArgumentError("$x is not available on LLVM $(lib.version)"))

@inline function from_abi(lib::Library, ::Type{T}, x::T) where {T}
    tables = abi_tables(T, lib.version.major)
    tables === nothing && return x
    raw = Integer(x)
    0 <= raw < length(tables.from) || return x
    return T(@inbounds tables.from[raw+1])
end

@nospecialize
include(joinpath(@__DIR__, "..", "lib", "libLLVM.jl"))
include(joinpath(@__DIR__, "..", "lib", "libLLVM_extra.jl"))
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))
@specialize

end # module API
@public API

check_supported(default_library.version)

# load and register the libraries that are available by default: Julia's, if LLVMExtra is
# available for it, and the one that the `libLLVM` preference selects
function load_libraries!()
    empty!(libraries)
    if julia_llvm || julia_extra_path() !== nothing
        load_julia!(julia_library)
        libraries[:julia] = julia_library
    end
    if !julia_llvm
        load!(default_library)
        libraries[default_library.name] = default_library
    end
    @atomic libraries_state.multiple = length(unique(values(libraries))) > 1
    return
end

# the wrappers are used while defining the package, e.g., by the precompilation workload
load_libraries!()

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
    init_library_caches!()

    # code that depends on the version of the default library is specific to it, so don't
    # use another one
    if !julia_llvm && library_version(default_library.path) != default_library.version
        error("""LLVM.jl was precompiled for LLVM $(default_library.version), whereas $(default_library.path) is now LLVM $(library_version(default_library.path)).
                 Please re-compile LLVM.jl.""")
    end

    # the libraries were loaded in another process when precompiling
    load_libraries!()
    if !julia_llvm
        initialize!(default_library)
        # like Julia, this exits the process if the options are invalid
        API.LLVMExtraParseEnvironmentOptions(default_library, "julia", "JULIA_LLVM_ARGS")
    end
    @debug "Using LLVM $(default_library.version) at $(default_library.path)"

    # sanity checks
    if julia_llvm && libllvm_version != Base.libllvm_version
        # this checks that the precompilation image isn't being used
        # after having upgraded Julia and the contained LLVM library.
        @error """LLVM.jl was precompiled for LLVM $libllvm_version, whereas you are now using LLVM $(Base.libllvm_version).
                  Please re-compile LLVM.jl."""
    end
    if julia_llvm && version() !== runtime_version()
        # this is probably caused by a combination of USE_SYSTEM_LLVM
        # and an LLVM upgrade without recompiling Julia.
        @error """Julia was compiled for LLVM $(version()), whereas you are now using LLVM $(runtime_version()).
                  Please re-compile Julia and LLVM.jl (but note that USE_SYSTEM_LLVM is not a supported configuration)."""
    end

    haskey(libraries, :julia) && register_eh_frame_stubs(julia_library)
    _install_handlers()
    Base.Experimental.register_error_hint(position_hint, MethodError)
    atexit(report_leaks)
end

end
