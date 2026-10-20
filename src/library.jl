# LLVM libraries
#
# LLVM.jl can use several LLVM libraries in one process: the one Julia uses, and others,
# e.g., a standalone library built by `deps/build_standalone.jl` (which links LLVM
# statically and hides its C++ symbols, so that it does not clash with Julia's LLVM). Every
# wrapper of the C API has a method that takes the library to call explicitly, and a
# method that picks the library from its arguments, or uses the current one (see
# `@llvmapi`).

@public Library, library, library!, register_library!

# the attribute kinds of a library: their names and categories by ID, and IDs by name
struct AttributeKinds
    names::Vector{Symbol}
    categories::Vector{Symbol}
    ids::Dict{Symbol,Cuint}
end
AttributeKinds() = AttributeKinds(Symbol[], Symbol[], Dict{Symbol,Cuint}())

"""
    LLVM.Library

An LLVM library that LLVM.jl can use, together with the LLVMExtra library built for it.
Libraries are registered under a name, e.g., `:julia` for the library Julia uses, and
looked up by calling `LLVM.Library(name)`.

Objects that own LLVM resources, like contexts, modules and target machines, remember the
library that created them in their `library` property, and are always used with it.
Values, types and metadata do not: they need to be used with the current library (see
[`LLVM.library!`](@ref)), typically by working with them inside a
[`context!`](@ref) block.

# Properties

    lib.name
    lib.version
    lib.path

The name the library was registered under, its version of LLVM, and its path.
"""
mutable struct Library
    const name::Symbol
    # the paths of the library and of the LLVMExtra library built for it, which can be the
    # same library. those of Julia's library are only known at run time.
    path::String
    extra_path::String
    const version::VersionNumber

    handle::Ptr{Cvoid}
    extra_handle::Ptr{Cvoid}
    # the address of each function that the wrappers call, by slot (see `@llvmapi`), or
    # `C_NULL` when the library does not provide it. filled in place when loading the
    # library, so that the table of a constant library is a constant too.
    const table::Vector{Ptr{Cvoid}}
    # the attribute kinds of the library, which differ between versions of LLVM. filled in
    # place too, when initializing the library (see `init_attribute_kinds!`).
    const attribute_kinds::AttributeKinds

    Library(name::Symbol, path::String, extra_path::String, version::VersionNumber) =
        new(name, path, extra_path, version, C_NULL, C_NULL, Ptr{Cvoid}[], AttributeKinds())
end

Base.show(io::IO, lib::Library) = print(io, "LLVM.Library(:", lib.name, ")")

function Base.show(io::IO, ::MIME"text/plain", lib::Library)
    print(io, "LLVM ", lib.version, " library :", lib.name)
    isempty(lib.path) || print(io, " at ", lib.path)
end


## function tables

# the functions that the wrappers call, by slot: their name, and whether the LLVMExtra
# library provides them. the slots are assigned when the wrappers are defined.
const library_functions = Tuple{Symbol,Bool}[]
const library_slots = Dict{Tuple{Symbol,Bool},Int}()

function function_slot(name::Symbol, extra::Bool)
    get!(library_slots, (name, extra)) do
        push!(library_functions, (name, extra))
        length(library_functions)
    end
end

# open a library and look up the addresses of all functions. this is done before a library
# is used, so that the tables are only read afterwards.
function load!(lib::Library)
    lib.handle = Libdl.dlopen(lib.path)
    lib.extra_handle = lib.extra_path == lib.path ? lib.handle :
                                                   Libdl.dlopen(lib.extra_path)
    table = resize!(lib.table, length(library_functions))
    for (slot, (name, extra)) in enumerate(library_functions)
        handle = extra ? lib.extra_handle : lib.handle
        ptr = Libdl.dlsym(handle, name; throw_error=false)
        table[slot] = ptr === nothing ? C_NULL : ptr
    end
    return lib
end

@inline function fptr(lib::Library, slot::Int)
    # libraries are loaded before they are used, so their table has every slot
    ptr = @inbounds lib.table[slot]
    ptr == C_NULL && throw_missing(lib, slot)
    return ptr
end

@noinline function throw_missing(lib::Library, slot::Int)
    name, extra = library_functions[slot]
    path = extra ? lib.extra_path : lib.path
    error("$name is not available in LLVM library :$(lib.name) ($path)")
end


## wrappers

#   @llvmapi function LLVMFoo(lib::Library, args...)
#       ccall((:LLVMFoo, libllvm), ...)
#   end
#
# defines a wrapper of a function of the C API of `lib`, where `ccall`s name the function
# and either `libllvm` or `libLLVMExtra` (meaning the respective library of `lib`). this also
# defines a method without the `lib` argument, `LLVMFoo(args...)`, which calls the library
# of the arguments that remember one (e.g., contexts and modules), or the current library if
# there are none. both methods check that all such arguments belong to the library called.
macro llvmapi(ex)
    Meta.isexpr(ex, :function) || error("@llvmapi expects a function definition")
    sig, body = ex.args
    Meta.isexpr(sig, :call) || error("@llvmapi expects a function definition")
    name, params... = sig.args
    (!isempty(params) && params[1] == :(lib::Library)) ||
        error("the first argument of $name should be `lib::Library`")
    args = params[2:end]
    all(arg -> arg isa Symbol, args) || error("the arguments of $name should be untyped")

    body = rewrite_ccalls(body, name)
    # both methods are located at the wrapper, e.g., for memcheck to tell them from user code
    explicit = Expr(:function, sig, Expr(:block, Expr(:meta, :inline), __source__,
                    :($(GlobalRef(LLVM, :check_library))(lib, $(args...))),
                    body))
    ambient = Expr(:function, Expr(:call, name, args...),
                   Expr(:block, Expr(:meta, :inline), __source__,
                        :($name($(GlobalRef(LLVM, :route_library))($(args...)), $(args...)))))
    return esc(Expr(:block, explicit, :(Base.@__doc__ $ambient)))
end

# rewrite `ccall((:LLVMFoo, lib), ...)` into `ccall(fptr(lib, slot), ...)`
function rewrite_ccalls(ex, fname)
    ex isa Expr || return ex
    if Meta.isexpr(ex, :call) && ex.args[1] === :ccall
        target = ex.args[2]
        if !(Meta.isexpr(target, :tuple) && length(target.args) == 2 &&
             target.args[1] isa QuoteNode && target.args[2] in (:libllvm, :libLLVMExtra))
            error("unsupported ccall target in $fname: $target")
        end
        slot = function_slot(target.args[1].value, target.args[2] === :libLLVMExtra)
        return Expr(:call, :ccall, :($(GlobalRef(LLVM, :fptr))(lib, $slot)),
                    map(arg -> rewrite_ccalls(arg, fname), ex.args[3:end])...)
    end
    return Expr(ex.head, map(arg -> rewrite_ccalls(arg, fname), ex.args)...)
end

# the library that an argument of a wrapper belongs to, for objects that remember it
argument_library(@nospecialize(x)) = nothing

# the library that an object is used with: the one it remembers, or the current one (e.g.,
# for values, which are used with the current library)
@inline object_library(@nospecialize(x)) =
    (lib = argument_library(x); lib === nothing ? library() : lib)

# whether `lib` is LLVM `major` or later. LLVM.jl's version-dependent behavior only depends
# on the major version, which is cheaper to check than a version number, and this is used
# in places where that matters (e.g., when wrapping values).
@inline atleast(major::Int, lib::Library=library()) = Int(lib.version.major) >= major

# the library to call: that of the arguments, if they remember one, or the current one
@inline function route_library(args...)
    lib = tagged_library(args...)
    return lib === nothing ? library() : lib
end
@inline tagged_library() = nothing
@inline function tagged_library(x, rest...)
    lib = argument_library(x)
    return lib === nothing ? tagged_library(rest...) : lib
end

# check that all arguments that remember a library belong to `lib`. operations that consume
# objects (and pass their handles to the wrappers) call this before doing so.
@inline check_library(lib::Library) = nothing
@inline function check_library(lib::Library, x, rest...)
    other = argument_library(x)
    other === nothing || other === lib || throw_library_mismatch(x, other, lib)
    check_library(lib, rest...)
end

@noinline throw_library_mismatch(@nospecialize(x), other::Library, lib::Library) =
    throw(ArgumentError("$(typeof(x)) belongs to LLVM library :$(other.name) (LLVM $(other.version)), but is used with :$(lib.name) (LLVM $(lib.version))"))


# for a message returned by a library that may not be the current one
function unsafe_message(lib::Library, ptr, args...)
    str = unsafe_string(ptr, args...)
    API.LLVMDisposeMessage(lib, ptr)
    str
end


## the current library

"""
    LLVM.library() -> LLVM.Library

The current LLVM library: the one that is used by operations whose arguments do not
determine it, like creating a context. This is the library Julia uses, unless the
`libLLVM` preference selects another one, and can be changed for the duration of a block
using [`LLVM.library!`](@ref) or [`context!`](@ref).
"""
@inline library() =
    # most processes only use one library, so avoid looking at the scope then. this is part
    # of every call of a wrapper, so keep what is inlined small.
    (@atomic :monotonic libraries_state.multiple) ? current_scope_library() : default_library

@noinline function current_scope_library()
    ScopedValues.current_scope() isa ScopedValues.Scope || return default_library
    return scoped_library()
end

# whether a library other than the default one has been registered. it is only set before
# registering returns that library, so no scope can select it before it is set.
mutable struct LibrariesState
    @atomic multiple::Bool
end
const libraries_state = LibrariesState(false)

@noinline function scoped_library()
    scope = ScopedValues.current_scope()
    tid = Threads.threadid()
    tid <= length(library_caches) || return current_library[]
    entry = @atomic :acquire library_caches[tid].entry
    entry.scope === scope && return entry.library
    return update_library_cache!(tid, scope)
end

# reading a scoped value in a scope is slow on some versions of Julia (on 1.11 to 1.13, it
# takes tens of nanoseconds and allocates), while every call of a wrapper whose arguments
# do not determine the library reads the current library. as scopes are immutable, each
# thread remembers the library of the last scope it looked up.
mutable struct LibraryCacheEntry
    const scope::Any
    const library::Library
end
mutable struct LibraryCache
    @atomic entry::LibraryCacheEntry
end
const library_caches = LibraryCache[]

@noinline function update_library_cache!(tid, scope)
    lib = current_library[]
    @atomic :release library_caches[tid].entry = LibraryCacheEntry(scope, lib)
    return lib
end

function init_library_caches!()
    empty!(library_caches)
    for _ in 1:Threads.maxthreadid()
        push!(library_caches, LibraryCache(LibraryCacheEntry(nothing, default_library)))
    end
end

"""
    LLVM.library!(f, lib::Union{LLVM.Library,Symbol})

Make `lib` (or the library registered under that name) the current library while calling
`f`, and the tasks it starts.
"""
library!(f, lib::Library) = lib === library() ? f() : with(f, current_library => lib)
library!(f, name::Symbol) = library!(f, Library(name))


## registry

const libraries = Dict{Symbol,Library}()
const libraries_lock = ReentrantLock()

"""
    LLVM.Library(name::Symbol)

Look up the library registered under `name`, e.g., `:julia` for the one Julia uses.
"""
function Library(name::Symbol)
    lib = @lock libraries_lock get(libraries, name, nothing)
    lib === nothing && error("No LLVM library is registered as :$name")
    return lib
end

# the version of an LLVM library, which either the library or LLVMExtra provides (the C API
# of LLVM 15 does not)
function library_version(handle::Ptr{Cvoid}, extra_handle::Ptr{Cvoid}=handle)
    fn = Libdl.dlsym(handle, :LLVMGetVersion; throw_error=false)
    fn === nothing && (fn = Libdl.dlsym(extra_handle, :LLVMGetVersion))
    major, minor, patch = Ref{Cuint}(), Ref{Cuint}(), Ref{Cuint}()
    ccall(fn, Cvoid, (Ptr{Cuint}, Ptr{Cuint}, Ptr{Cuint}), major, minor, patch)
    VersionNumber(major[], minor[], patch[])
end
library_version(path::AbstractString) = library_version(Libdl.dlopen(path))

"""
    LLVM.register_library!(name::Symbol, path; extra=path) -> LLVM.Library

Register the LLVM library at `path` under `name`, with the LLVMExtra library at `extra`,
and return it. The library is loaded and initialized like Julia initializes its own:
the native target is initialized if the library supports it. Registering the same library
under another name returns the same `Library`.

The library should link LLVM statically and only export the C API, like the libraries built
by `deps/build_standalone.jl`; loading another shared libLLVM in the process clashes with
Julia's.
"""
function register_library!(name::Symbol, path::AbstractString;
                           extra::AbstractString=path)
    name === :julia && error("The name :julia is reserved for the library Julia uses")
    path = realpath(path)
    extra = realpath(extra)
    @lock libraries_lock begin
        lib = get(libraries, name, nothing)
        if lib !== nothing
            lib.path == path && lib.extra_path == extra ||
                error("Another LLVM library is registered as :$name: $(lib.path)")
            return lib
        end
        for other in values(libraries)
            if other.path == path && other.extra_path == extra
                libraries[name] = other
                return other
            end
        end

        handle = Libdl.dlopen(path)
        version = library_version(handle, Libdl.dlopen(extra))
        check_supported(version)
        lib = load!(Library(name, path, extra, version))
        initialize!(lib)
        init_attribute_kinds!(lib)
        libraries[name] = lib
        @atomic libraries_state.multiple = true
        return lib
    end
end

function check_supported(version::VersionNumber)
    if version.major < first(API.WRAPPED_VERSIONS)
        error("LLVM.jl only supports LLVM $(first(API.WRAPPED_VERSIONS)) and later.")
    elseif version.major > last(API.WRAPPED_VERSIONS)
        @warn "LLVM.jl has not been tested with LLVM versions newer than $(last(API.WRAPPED_VERSIONS))."
    end
end

# initialize a library other than Julia's, which Julia does not initialize
function initialize!(lib::Library)
    # LLVM's JIT looks up functions that support JIT-compiled code by name, e.g., LLVM 17
    # in the process. make it find those of this library, which isn't loaded globally.
    API.LLVMLoadLibraryPermanently(lib, lib.path)
    # the library may not include the native target, e.g., when built for GPUs only
    API.LLVMExtraInitializeNativeTarget(lib)
    API.LLVMExtraInitializeNativeAsmPrinter(lib)
    API.LLVMExtraInitializeNativeAsmParser(lib)
    API.LLVMExtraInitializeNativeDisassembler(lib)
    register_eh_frame_stubs(lib)
    return
end

# functionality that integrates with Julia's code generator only works with Julia's LLVM
function check_julia_library(what, lib::Library=library())
    lib === julia_library && return
    error("$what requires the LLVM library that Julia uses, but uses LLVM library :$(lib.name) (LLVM $(lib.version))")
end

resolve_library(::Nothing) = library()
resolve_library(lib::Library) = lib
resolve_library(name::Symbol) = Library(name)
