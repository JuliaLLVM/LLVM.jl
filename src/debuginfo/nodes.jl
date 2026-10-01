## nodes

@vocabulary IR DINode

"""
    DINode

a tagged DWARF-like metadata node.

# Properties

    node.tag

The DWARF tag of the node, or `0` if it has none. Requires LLVM 17+.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
abstract type DINode <: MDNode end


# LLVM line numbers are unsigned, with 0 meaning "no line". Julia's codegen additionally
# emits -1 (all ones) for an unknown line, which its DWARF reader reads back as a signed
# `int`. Return that as -1, like `Base.StackTraces` does, instead of 4294967295 or, on
# 32-bit platforms, an InexactError.
line_number(x::Cuint) = x == typemax(Cuint) ? -1 : Int(x)


## scopes

@vocabulary IR DIScope

"""
    DIScope

Abstract supertype for lexical scopes and types (which are also declaration contexts).

# Properties

    scope.file

The file associated with the scope, or `nothing` if it has none.

    scope.name

The name of the scope, or `nothing` if it has none.

The properties of [`DINode`](@ref LLVM.DINode) and [`MDNode`](@ref LLVM.MDNode) are
available too.
"""
abstract type DIScope <: DINode end

function file(scope::DIScope)
    ref = API.LLVMDIScopeGetFile(scope)
    ref == C_NULL ? nothing : Metadata(ref)::DIFile
end

function name(scope::DIScope)
    len = Ref{Cuint}()
    data = API.LLVMDIScopeGetName(scope, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property DIScope file
@property DIScope name

@vocabulary IR DILocalScope

"""
    DILocalScope

Abstract supertype for scopes that can contain local variables, labels and source
locations: subprograms ([`DISubprogram`](@ref)) and the lexical blocks nested in them
([`LLVM.DILexicalBlock`](@ref) and [`LLVM.DILexicalBlockFile`](@ref)).

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available.
"""
abstract type DILocalScope <: DIScope end


## location information

@vocabulary IR DILocation

"""
    DILocation

A location in the source code.

# Properties

    loc.line

The line number of the debug location, or -1 if unknown.

    loc.column

The column number of the debug location.

    loc.scope

The local scope of the debug location, a [`DILocalScope`](@ref).

    loc.inlined_at

The location that the code at this debug location has been inlined at, or `nothing` if it
hasn't been inlined.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DILocation <: MDNode
    ref::API.LLVMMetadataRef
end
register(DILocation, API.LLVMDILocationMetadataKind)

"""
    DILocation(line::Integer, col::Integer, scope::DILocalScope,
               [inlined_at::DILocation]) -> DILocation

Creates a new debug location that describes a source location in the local scope `scope`,
e.g., a [`DISubprogram`](@ref).
"""
function DILocation(line::Integer, col::Integer, scope::DILocalScope,
                    inlined_at::Union{DILocation,Nothing}=nothing)
    DILocation(API.LLVMDIBuilderCreateDebugLocation(context(), line, col, scope,
                                                    something(inlined_at, C_NULL)))
end

line(location::DILocation) = line_number(API.LLVMDILocationGetLine(location))

column(location::DILocation) = Int(API.LLVMDILocationGetColumn(location))

function scope(location::DILocation)
    ref = API.LLVMDILocationGetScope(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocalScope
end

function inlined_at(location::DILocation)
    ref = API.LLVMDILocationGetInlinedAt(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end

@property DILocation line
@property DILocation column
@property DILocation scope
@property DILocation inlined_at


## file

@vocabulary IR DIFile
@vocabulary Build file!

"""
    DIFile

A file in the source code.

# Properties

    file.directory

The directory of the file.

    file.filename

The name of the file.

    file.source

The source code of the file, or `nothing` if it is not available.

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DIFile <: DIScope
    ref::API.LLVMMetadataRef
end
register(DIFile, API.LLVMDIFileMetadataKind)

"""
    file!(builder::DIBuilder, filename::AbstractString, directory::AbstractString) -> DIFile

Create a new [`DIFile`](@ref) describing the given source file.
"""
function file!(builder::DIBuilder, filename::AbstractString, directory::AbstractString)
    filename = String(filename)
    directory = String(directory)
    DIFile(API.LLVMDIBuilderCreateFile(builder,
                                       filename, Csize_t(ncodeunits(filename)),
                                       directory, Csize_t(ncodeunits(directory))))
end

function directory(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetDirectory(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

function filename(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetFilename(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

function source(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetSource(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property DIFile directory
@property DIFile filename
@property DIFile source
