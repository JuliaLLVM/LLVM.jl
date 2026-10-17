# Version management

@public version

"""
    LLVM.version() -> VersionNumber
    LLVM.version(lib::LLVM.Library) -> VersionNumber
    LLVM.version(obj) -> VersionNumber

The version of LLVM of the current library (see [`LLVM.library`](@ref)), of `lib`, or of the
library that `obj` belongs to: an object that remembers its library, like a context, module,
builder or target machine.

Code that depends on the version of LLVM should check it at run time, for the library it
works with: e.g., `LLVM.version(mod) >= v"21"` when adding passes that run on `mod`.
Checking it at compile time (`@static if LLVM.version() ...`) uses the library that LLVM.jl
uses by default, while other libraries can be used at the same time.
"""
version() = library().version
version(lib::Library) = lib.version
function version(@nospecialize(obj))
    lib = argument_library(obj)
    lib === nothing && throw(ArgumentError("$(typeof(obj)) does not remember its LLVM library; use the version of the current library, `LLVM.version()`"))
    return lib.version
end

"""
    LLVM.isavailable(x)

Check whether `x`, e.g. a value of one of the enums in `LLVM.API`, is supported by the
version of LLVM in use. Such values can be named on every LLVM version, even ones that
predate or removed them, so that code does not need to be version-gated just to compile,
but passing them to a version of LLVM that lacks them throws an error.
"""
function isavailable end

# the version that a shared libLLVM reports, which can differ from the version of the headers
# it was built from (and that Julia was built for) when Julia uses a system LLVM
function runtime_version(lib::Library=library())
    # FIXME: add a proper C API to LLVM
    fn = Libdl.dlsym(lib.handle, :_ZN4llvm16LTOCodeGenerator16getVersionStringEv)
    version_print = unsafe_string(ccall(fn, Cstring, ()))
    m = match(r"LLVM version (?<version>.+)", version_print)
    m === nothing && error("Unrecognized version string: '$version_print'")
    version = m[:version]::AbstractString
    if endswith(version, "jl")
        # strip the "jl" SONAME suffix (JuliaLang/julia#33058)
        # (LLVM does never report a prerelease version anyway)
        VersionNumber(version[1:end-2])
    else
        VersionNumber(version)
    end
end

is_asserts(lib::Library=library()) =
    Libdl.dlsym(lib.handle, "_ZN4llvm23EnableABIBreakingChecksE"; throw_error=false) !== nothing
