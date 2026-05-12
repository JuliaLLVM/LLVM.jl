## support routines

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

# Permanently add the symbol `name` with the value `ptr`. These symbols are searched
# before any libraries.
add_symbol(name, ptr) = API.LLVMAddSymbol(name, ptr)

# Permanently load the dynamic library at the given path. It is safe to call this function
# multiple times for the same library.
load_library_permanently(path) = API.LLVMLoadLibraryPermanently(path)

# Search the global symbols for `name` and return the pointer to it.
find_symbol(name) = API.LLVMSearchForAddressOfSymbol(name)

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
