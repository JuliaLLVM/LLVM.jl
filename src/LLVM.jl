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

# the LLVM library is the one Julia uses, unless the `libLLVM` preference selects another one,
# e.g., a standalone library built by `deps/build_standalone.jl` to test LLVM.jl with another
# version of LLVM. such a library also provides LLVMExtra.
const libllvm_preference = @load_preference("libLLVM", nothing)

# whether LLVM.jl uses Julia's LLVM, which functionality that integrates with Julia's code
# generator requires
const julia_llvm = libllvm_preference === nothing

# we don't embed the full path to Julia's LLVM, because the location might be different at
# run time.
const libllvm = julia_llvm ? basename(String(Base.libllvm_path())) : libllvm_preference

# the version of an LLVM library that provides the C API (or LLVMExtra, which provides the
# function that reports it on versions of LLVM whose C API does not)
function library_version(lib)
    major, minor, patch = Ref{Cuint}(), Ref{Cuint}(), Ref{Cuint}()
    # the library is not closed again, as LLVM.jl is going to use it anyway, and unloading
    # LLVM would run its static destructors
    handle = Libdl.dlopen(lib)
    ccall(Libdl.dlsym(handle, :LLVMGetVersion), Cvoid,
          (Ptr{Cuint}, Ptr{Cuint}, Ptr{Cuint}), major, minor, patch)
    VersionNumber(major[], minor[], patch[])
end

const libllvm_version = julia_llvm ? Base.libllvm_version : library_version(libllvm)

# the wrappers depend on the version of another library, so recompile when it changes
julia_llvm || include_dependency(libllvm)

function check_julia_llvm(what)
    julia_llvm && return
    error("$what requires the LLVM library that Julia uses, but LLVM.jl uses $libllvm (LLVM $libllvm_version)")
end

module API
using CEnum
using Preferences

# library handles
import ..LLVM
using ..LLVM: libllvm, version
using LLVMExtra_jll
if !LLVM.julia_llvm
    # another LLVM library also provides LLVMExtra, built for it
    const libLLVMExtra = libllvm
elseif has_preference(LLVM, "libLLVMExtra")
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
    def = undocumented(ex)
    if Meta.isexpr(def, :function) && Meta.isexpr(def.args[2], :block)
        pushfirst!(def.args[2].args, Expr(:meta, :inline))
    end
    return ex
end

# the definition in a top-level expression of the wrappers, which is wrapped in a call to
# `@doc` if it is documented
undocumented(ex) =
    Meta.isexpr(ex, :macrocall) && ex.args[1] == GlobalRef(Core, Symbol("@doc")) ?
        ex.args[end] : ex

# enum members that no C API names, numbered as their next values, so that they can be named
# and enumerated. they are added to the definitions of the enums in the generated wrappers,
# so that CEnum knows them too, e.g., for `instances`.
const backfilled_enum_members = (
    # module flag behaviors that every supported LLVM has (`Module::ModFlagBehavior`)
    :LLVMModuleFlagBehavior => (
        :LLVMModuleFlagBehaviorMax => 6, :LLVMModuleFlagBehaviorMin => 7),
)
const backfilled_enum_definitions = Dict{Symbol,Int}()
function backfill_enum_members(ex)
    # the definition is `@cenum Name::UInt32 begin ... end`
    def = undocumented(ex)
    Meta.isexpr(def, :macrocall) && def.args[1] === Symbol("@cenum") &&
        Meta.isexpr(def.args[3], :(::)) || return ex
    enum = def.args[3].args[1]
    i = findfirst(entry -> entry.first === enum, backfilled_enum_members)
    i === nothing && return ex
    backfilled_enum_definitions[enum] = get(backfilled_enum_definitions, enum, 0) + 1
    members = def.args[4].args
    defined = Dict{Any,Any}()
    for m in members
        Meta.isexpr(m, :(=)) && (defined[m.args[1]] = m.args[2])
    end
    for (name, val) in backfilled_enum_members[i].second
        if haskey(defined, name)
            defined[name] == val || error("LLVM defines $name as $(defined[name]), not $val")
        else
            val in values(defined) && error("LLVM defines another member of $enum as $val")
            push!(members, :($name = $val))
        end
    end
    return ex
end
wrapper_mapexpr(ex) = inline_wrapper(backfill_enum_members(ex))

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
include(wrapper_mapexpr, joinpath(@__DIR__, "..", "lib", "libLLVM.jl"))
include(wrapper_mapexpr, joinpath(@__DIR__, "..", "lib", "libLLVM_extra.jl"))
include(wrapper_mapexpr, joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))
@specialize

if version().major < first(WRAPPED_VERSIONS)
    error("LLVM.jl only supports LLVM $(first(WRAPPED_VERSIONS)) and later.")
elseif version().major > last(WRAPPED_VERSIONS)
    @warn "LLVM.jl has not been tested with LLVM versions newer than $(last(WRAPPED_VERSIONS))."
end
for (enum, _) in backfilled_enum_members
    n = get(backfilled_enum_definitions, enum, 0)
    n == 1 || error("Expected one definition of $enum in the wrappers, found $n")
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
include("triple.jl")
include("subtarget.jl")
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
    @debug "Using LLVM $libllvm_version at $(julia_llvm ? Base.libllvm_path() : libllvm)"

    # sanity checks
    if !julia_llvm
        # the wrappers are specific to the version of LLVM, so don't use another one
        if library_version(libllvm) != libllvm_version
            error("""LLVM.jl was precompiled for LLVM $libllvm_version, whereas $libllvm is now LLVM $(library_version(libllvm)).
                     Please re-compile LLVM.jl.""")
        end
    elseif libllvm_version != Base.libllvm_version
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

    # initialize another LLVM library like Julia initializes its own
    if !julia_llvm
        # LLVM's JIT looks up functions that support JIT-compiled code by name, e.g., LLVM 17
        # in the process. make it find those of this library, which isn't loaded globally.
        load_library_permanently(libllvm)
        InitializeNativeTarget()
        InitializeNativeAsmPrinter()
        InitializeNativeAsmParser()
        InitializeNativeDisassembler()
        # like Julia, this exits the process if the options are invalid
        API.LLVMExtraParseEnvironmentOptions("julia", "JULIA_LLVM_ARGS")
    end

    register_eh_frame_stubs()
    init_attribute_kinds()
    _install_handlers()
    Base.Experimental.register_error_hint(position_hint, MethodError)
    atexit(report_leaks)
end

end
