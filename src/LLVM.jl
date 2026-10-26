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

# whether the default library is Julia's. LLVM.jl checks the version of the library it uses
# at run time, but packages that check it at compile time (`@static if LLVM.version() ...`)
# get the version of the default library.
const julia_llvm = libllvm_preference === nothing

# the default library, for code that calls it directly, e.g., `ccall((:LLVMFoo,
# LLVM.API.libllvm), ...)`. we don't embed the full path to Julia's LLVM, because the
# location might be different at run time.
const libllvm = julia_llvm ? basename(String(Base.libllvm_path())) : libllvm_preference

# the LLVMExtra library for Julia's LLVM that the `libLLVMExtra` preference selects, e.g.,
# one built by `deps/build_local.jl`. this is a compile-time preference, like `libLLVM`.
const julia_extra_preference = @load_preference("libLLVMExtra", nothing)

# the LLVMExtra library for Julia's LLVM, or `nothing` if there is none. like the path of
# Julia's LLVM, that of LLVMExtra_jll is only known at run time.
function julia_extra_path()
    if julia_extra_preference !== nothing
        julia_extra_preference
    elseif isdefined(LLVMExtra_jll, :libLLVMExtra)
        LLVMExtra_jll.libLLVMExtra_path
    else
        nothing
    end
end

const julia_library = Library(:julia, "", "", Base.libllvm_version)

# the path of Julia's LLVM library, or `nothing` if Julia's code generator is not loaded,
# e.g., in executables that juliac trims
julia_llvm_path() = Base.libllvm_path()::Union{Nothing,Symbol}

function load_julia!(lib::Library, path::Symbol)
    extra = julia_extra_path()
    if extra === nothing
        error("""LLVM.jl requires the LLVM extensions library, which LLVMExtra_jll does not provide for your platform:
                   $(Base.BinaryPlatforms.triplet(LLVMExtra_jll.host_platform))
                 If you are using a custom version of LLVM, build the library using `deps/build_local.jl`.""")
    end
    lib.path = realpath(String(path))
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

# the default library, and its version, are part of the precompilation image, so recompile
# when another library is selected
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

@inline function check_available(lib::Library, name::Symbol, since::Int,
                                 until::Union{Int,Nothing}=nothing)
    major = Int(lib.version.major)
    if major < since || (until !== nothing && major >= until)
        throw_unavailable(lib, name, since, until)
    end
end
@noinline function throw_unavailable(lib, name, since, until)
    supported = until === nothing ? "$since and later" : "$since to $(until-1)"
    error("$name is not available on LLVM $(lib.version.major) (only on LLVM $supported)")
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
include(backfill_enum_members, joinpath(@__DIR__, "..", "lib", "libLLVM.jl"))
include(joinpath(@__DIR__, "..", "lib", "libLLVM_extra.jl"))
include(inline_wrapper, joinpath(@__DIR__, "..", "lib", "libLLVM_julia.jl"))
@specialize

for (enum, _) in backfilled_enum_members
    n = get(backfilled_enum_definitions, enum, 0)
    n == 1 || error("Expected one definition of $enum in the wrappers, found $n")
end

end # module API
@public API

check_supported(default_library.version)

# load and register the libraries that are available by default: Julia's, if it is loaded
# and LLVMExtra is available for it, and the one that the `libLLVM` preference selects
function load_libraries!()
    empty!(libraries)
    path = julia_llvm_path()
    if path !== nothing && (julia_llvm || julia_extra_path() !== nothing)
        load_julia!(julia_library, path)
        libraries[:julia] = julia_library
    else
        # calling the library throws (see `fptr`)
        fill!(resize!(julia_library.table, length(library_functions)), C_NULL)
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
    init_library_caches!()

    # the default library was created when precompiling, with the version it had then
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
    if haskey(libraries, :julia) && julia_library.version != runtime_version(julia_library)
        # this is probably caused by a combination of USE_SYSTEM_LLVM
        # and an LLVM upgrade without recompiling Julia.
        @error """Julia was compiled for LLVM $(julia_library.version), whereas you are now using LLVM $(runtime_version(julia_library)).
                  Please re-compile Julia and LLVM.jl (but note that USE_SYSTEM_LLVM is not a supported configuration)."""
    end

    haskey(libraries, :julia) && register_eh_frame_stubs(julia_library)
    foreach(init_attribute_kinds!, unique(values(libraries)))
    _install_handlers()
    Base.Experimental.register_error_hint(position_hint, MethodError)
    atexit(report_leaks)
end

end
