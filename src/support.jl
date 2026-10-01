## support routines

@public clopts

"""
    clopts(opts...)

Parse the given arguments using the LLVM command-line parser.

Note that this function modifies the global state of the LLVM library. It is also not safe
to rely on the stability of the command-line options between different versions of LLVM.
"""
function clopts(opts...)
    args = ["", opts...]
    API.LLVMParseCommandLineOptions(length(args), args, C_NULL)
end


## process-wide symbols

@public load_library_permanently, add_symbol, find_symbol

"""
    LLVM.load_library_permanently(path::AbstractString)

Load the dynamic library at `path`, and make its symbols available to the legacy execution
engines (e.g., `LLVM.JIT`) for resolving external symbols. This uses LLVM's process-wide
symbol search (`sys::DynamicLibrary`): the library stays loaded for the remainder of the
process, and it is safe to load the same library multiple times. Throws an `LLVMException`
if the library cannot be loaded.

For ORC JITs, add a [`DynamicLibrarySearchGenerator(jit, path)`](@ref
DynamicLibrarySearchGenerator) to the JITDylib that should see the library instead.

See also: [`LLVM.add_symbol`](@ref), [`LLVM.find_symbol`](@ref).
"""
function load_library_permanently(path::AbstractString)
    if API.LLVMLoadLibraryPermanently(path) |> Bool
        throw(LLVMException("Could not load library $(repr(path))"))
    end
    return
end

"""
    LLVM.add_symbol(name::AbstractString, ptr::Ptr)

Make the symbol `name` resolve to the address `ptr` for the legacy execution engines (e.g.,
`LLVM.JIT`). Like [`LLVM.load_library_permanently`](@ref), this uses LLVM's process-wide
symbol search, where symbols added this way take precedence over those of libraries. The
symbol remains defined for the remainder of the process (adding it again replaces its
address), and LLVM.jl does not keep what `ptr` points to alive, e.g., a Julia object or a
closure passed to `@cfunction`.

This does not affect ORC JITs: to define a symbol in a JITDylib, use
[`absolute_symbols`](@ref).
"""
function add_symbol(name::AbstractString, ptr::Ptr)
    API.LLVMAddSymbol(name, ptr)
    return
end

"""
    LLVM.find_symbol(name::AbstractString) -> Ptr{Cvoid}

Look up the address of the symbol `name` the way the legacy execution engines do: in the
symbols added with [`LLVM.add_symbol`](@ref), and then in the process and the libraries
loaded with [`LLVM.load_library_permanently`](@ref). Returns `C_NULL` if the symbol cannot
be found.
"""
find_symbol(name::AbstractString) = API.LLVMSearchForAddressOfSymbol(name)

# No-op stubs for DWARF EH frame registration. Windows x86_64 uses SEH for unwinding,
# so registering DWARF frames is unnecessary. Since LLVM 21, JITLink's EHFrameRegistration
# plugin treats a missing `__register_frame` as a hard error (previously, the void-typed
# SPS wrapper silently swallowed it), so we publish no-op symbols to satisfy the lookup.
noop_register_frame(::Ptr{Cvoid})::Cvoid = nothing

function register_eh_frame_stubs()
    Sys.iswindows() || return
    fp = @cfunction(noop_register_frame, Cvoid, (Ptr{Cvoid},))
    add_symbol("__register_frame", fp)
    add_symbol("__deregister_frame", fp)
    return
end
