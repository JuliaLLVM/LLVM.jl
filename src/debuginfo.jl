## debug info builder

export DIBuilder

"""
    DIBuilder

A builder for constructing debug information metadata.

This object needs to be disposed of using [`dispose`](@ref), after first having
called [`finalize!`](@ref).
"""
@checked struct DIBuilder
    ref::API.LLVMDIBuilderRef
end

Base.unsafe_convert(::Type{API.LLVMDIBuilderRef}, builder::DIBuilder) =
    mark_use(builder).ref

"""
    DIBuilder(mod::Module; allow_unresolved::Bool=true)

Create a new debug info builder that emits metadata into `mod`.

When `allow_unresolved` is `true` (the default), the builder collects unresolved
metadata nodes attached to the module so that cycles can be resolved during a
call to [`finalize!`](@ref). When `false`, the builder errors on unresolved
nodes instead.
"""
function DIBuilder(mod::Module; allow_unresolved::Bool=true)
    ref = allow_unresolved ? API.LLVMCreateDIBuilder(mod) :
                             API.LLVMCreateDIBuilderDisallowUnresolved(mod)
    mark_alloc(DIBuilder(ref))
end

"""
    dispose(builder::DIBuilder)

Dispose of a debug info builder. [`finalize!`](@ref) must be called first.
"""
dispose(builder::DIBuilder) = mark_dispose(API.LLVMDisposeDIBuilder, builder)

function DIBuilder(f::Core.Function, args...; kwargs...)
    builder = DIBuilder(args...; kwargs...)
    try
        f(builder)
    finally
        dispose(builder)
    end
end

Base.show(io::IO, builder::DIBuilder) = @printf(io, "DIBuilder(%p)", builder.ref)

"""
    finalize!(builder::DIBuilder)

Resolve any unresolved metadata nodes and mark all compile units finalized.
Must be called before [`dispose`](@ref) and before using the emitted debug
information.
"""
finalize!(builder::DIBuilder) = API.LLVMDIBuilderFinalize(builder)


## location information

export DILocation, line, column, scope, inlined_at

"""
    DILocation

A location in the source code.
"""
@checked struct DILocation <: MDNode
    ref::API.LLVMMetadataRef
end
register(DILocation, API.LLVMDILocationMetadataKind)

"""
    DILocation([line::Int], [col::Int], [scope::Metadata], [inlined_at::Metadata])

Creates a new DebugLocation that describes a source location.
"""
function DILocation(line=0, col=0, scope=nothing, inlined_at=nothing)
    # XXX: are null scopes valid? they crash LLVM:
    #      DILocation(Context(), 1, 2).scope
    DILocation(API.LLVMDIBuilderCreateDebugLocation(context(), line, col,
                                                    something(scope, C_NULL),
                                                    something(inlined_at, C_NULL)))
end

"""
    line(location::DILocation)

Get the line number of this debug location.
"""
line(location::DILocation) = Int(API.LLVMDILocationGetLine(location))

"""
    column(location::DILocation)

Get the column number of this debug location.
"""
column(location::DILocation) = Int(API.LLVMDILocationGetColumn(location))

"""
    scope(location::DILocation)

Get the local scope associated with this debug location.
"""
function scope(location::DILocation)
    ref = API.LLVMDILocationGetScope(location)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

"""
    inlined_at(location::DILocation)

Get the "inline at" location associated with this debug location.
"""
function inlined_at(location::DILocation)
    ref = API.LLVMDILocationGetInlinedAt(location)
    ref == C_NULL ? nothing : Metadata(ref)::DILocation
end


## nodes

export DINode

"""
    DINode

a tagged DWARF-like metadata node.
"""
abstract type DINode <: MDNode end


## variables

export DIVariable, file, scope, line

"""
    DIVariable

Abstract supertype for all variable-like metadata nodes.
"""
abstract type DIVariable <: DINode end

for var in (:Local, :Global)
    var_name = Symbol("DI$(var)Variable")
    var_kind = Symbol("LLVM$(var_name)MetadataKind")
    @eval begin
        @checked struct $var_name <: DIVariable
            ref::API.LLVMMetadataRef
        end
        register($var_name, API.$var_kind)
    end
end

"""
    DILocalVariable <: DIVariable

A local variable in the source code.
"""
DILocalVariable

"""
    DIGlobalVariable <: DIVariable

A global variable in the source code.
"""
DIGlobalVariable

"""
    file(var::DIVariable)

Get the file of the given variable.
"""
function file(var::DIVariable)
    ref = API.LLVMDIVariableGetFile(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIFile
end

"""
    name(var::DIVariable)

Get the name of the given variable.
"""
function scope(var::DIVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

"""
    line(var::DIVariable)

Get the line number of the given variable.
"""
line(var::DIVariable) = Int(API.LLVMDIVariableGetLine(var))


## scopes

export DIScope, file, name

"""
    DIScope

Abstract supertype for lexical scopes and types (which are also declaration contexts).
"""
abstract type DIScope <: DINode end

"""
    file(scope::DIScope)

Get the metadata of the file associated with a given scope.
"""
file(scope::DIScope) = DIFile(API.LLVMDIScopeGetFile(scope))

"""
    name(scope::DIScope)

Get the name of the given scope.
"""
function name(scope::DIScope)
    len = Ref{Cuint}()
    data = API.LLVMDIScopeGetName(scope, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

abstract type DILocalScope <: DIScope end


## file

export DIFile, directory, filename, source

"""
    DIFile

A file in the source code.
"""
@checked struct DIFile <: DIScope
    ref::API.LLVMMetadataRef
end
register(DIFile, API.LLVMDIFileMetadataKind)

"""
    directory(file::DIFile)

Get the directory of a given file.
"""
function directory(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetDirectory(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    filename(file::DIFile)

Get the filename of the given file.
"""
function filename(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetFilename(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    source(file::DIFile)

Get the source of the given file, or `nothing` if the source is not available.
"""
function source(file::DIFile)
    len = Ref{Cuint}()
    data = API.LLVMDIFileGetSource(file, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end


## type

export DIType, name, offset, line, flags

"""
    DIType

Abstract supertype for all type-like metadata nodes.
"""
abstract type DIType <: DIScope end

for typ in (:Basic, :Derived, :Composite, :Subroutine)
    typ_name = Symbol("DI$(typ)Type")
    typ_kind = Symbol("LLVM$(typ_name)MetadataKind")
    @eval begin
        @checked struct $typ_name <: DIType
            ref::API.LLVMMetadataRef
        end
        register($typ_name, API.$typ_kind)
    end
end

"""
    name(typ::DIType)

Get the name of the given type.
"""
function name(typ::DIType)
    len = Ref{Csize_t}()
    data = API.LLVMDITypeGetName(typ, len)
    data == C_NULL && return nothing
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

"""
    sizeof(typ::DIType)

Get the size in bits of the given type.
"""
Base.sizeof(typ::DIType) = 8*Int(API.LLVMDITypeGetSizeInBits(typ))

"""
    offset(typ::DIType)

Get the offset in bits of the given type.
"""
offset(typ::DIType) = Int(API.LLVMDITypeGetOffsetInBits(typ))

"""
    line(typ::DIType)

Get the line number of the given type.
"""
line(typ::DIType) = Int(API.LLVMDITypeGetLine(typ))

"""
    flags(typ::DIType)

Get the flags of the given type.
"""
flags(typ::DIType) = API.LLVMDITypeGetFlags(typ)


## subprogram

export DISubProgram, line
@public finalize_subprogram!

"""
    DISubProgram

A subprogram in the source code.
"""
@checked struct DISubProgram <: DIScope
    ref::API.LLVMMetadataRef
end
register(DISubProgram, API.LLVMDISubprogramMetadataKind)

"""
    line(subprogram::DISubProgram)

Get the line number of the given subprogram.
"""
line(subprogram::DISubProgram) = Int(API.LLVMDISubprogramGetLine(subprogram))

"""
    finalize_subprogram!(builder::DIBuilder, sp::DISubProgram)

Finalize the given subprogram, allowing later changes to be disallowed. Must be
called before [`finalize!`](@ref).
"""
finalize_subprogram!(builder::DIBuilder, sp::DISubProgram) =
    API.LLVMDIBuilderFinalizeSubprogram(builder, sp)


## compile unit

export DICompileUnit

"""
    DICompileUnit

A compilation unit in the source code.
"""
@checked struct DICompileUnit <: DIScope
    ref::API.LLVMMetadataRef
end
register(DICompileUnit, API.LLVMDICompileUnitMetadataKind)


## other

export DEBUG_METADATA_VERSION, strip_debuginfo!, subprogram, subprogram!

"""
    DEBUG_METADATA_VERSION()

The current debug info version number, as supported by LLVM.
"""
DEBUG_METADATA_VERSION() = API.LLVMDebugMetadataVersion()

"""
    strip_debuginfo!(mod::Module)

Strip the debug information from the given module.
"""
strip_debuginfo!(mod::Module) = API.LLVMStripModuleDebugInfo(mod)

"""
    subprogram(func::Function) -> DISubProgram

Get the subprogram of the given function, or `nothing` if the function has no subprogram.
"""
function subprogram(func::Function)
    ref = API.LLVMGetSubprogram(func)
    ref==C_NULL ? nothing : Metadata(ref)::DISubProgram
end

"""
    subprogram!(func::Function, sp::DISubProgram)

Set the subprogram of the given function.
"""
subprogram!(func::Function, sp::DISubProgram) = API.LLVMSetSubprogram(func, sp)
