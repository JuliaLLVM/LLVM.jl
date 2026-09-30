## subprogram

@vocabulary IR DISubprogram
@vocabulary Build subprogram!, finalize_subprogram!

"""
    DISubprogram <: DILocalScope

A subprogram (a function) in the source code.

# Properties

    sp.line

The line number of the subprogram, or -1 if unknown.

The properties of [`DIScope`](@ref LLVM.DIScope), [`DINode`](@ref LLVM.DINode) and
[`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DISubprogram <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DISubprogram, API.LLVMDISubprogramMetadataKind)

line(subprogram::DISubprogram) = line_number(API.LLVMDISubprogramGetLine(subprogram))

@property DISubprogram line

"""
    subprogram!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                file::DIFile, line::Integer, type::DISubroutineType;
                linkage_name::AbstractString="", scope_line::Integer=line,
                local_to_unit::Bool=false, definition::Bool=true,
                flags=API.LLVMDIFlagZero, optimized::Bool=false) -> DISubprogram

Create a new [`DISubprogram`](@ref) describing a function. When
`linkage_name` is empty, LLVM falls back to `name`. `scope_line`
defaults to the function's `line`, which is the usual case.
"""
function subprogram!(builder::DIBuilder, scope::Union{DIScope,Nothing}, name::AbstractString,
                     file::DIFile, line::Integer, type::DISubroutineType;
                     linkage_name::AbstractString="", scope_line::Integer=line,
                     local_to_unit::Bool=false, definition::Bool=true,
                     flags=API.LLVMDIFlagZero, optimized::Bool=false)
    DISubprogram(API.LLVMDIBuilderCreateFunction(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        linkage_name, Csize_t(ncodeunits(linkage_name)),
        file, Cuint(line), type,
        local_to_unit, definition, Cuint(scope_line),
        flags, optimized))
end

"""
    finalize_subprogram!(builder::DIBuilder, sp::DISubprogram)

Finalize a single subprogram early, sealing its retained-nodes list. After
this, no more local variables can be added to `sp`. A no-op if `sp` was not
tracked by `builder` (e.g. created elsewhere or already finalized).

Calling this is never required for correctness — [`dispose`](@ref) /
[`finalize!`](@ref) finalize every tracked subprogram automatically. Use it
only when streaming many subprograms through the builder and wanting to
release their bookkeeping early.
"""
finalize_subprogram!(builder::DIBuilder, sp::DISubprogram) =
    API.LLVMDIBuilderFinalizeSubprogram(builder, sp)

@static if version() >= v"21"

@vocabulary Build replace_type!

@doc """
    replace_type!(sp::DISubprogram, ty::DISubroutineType)

Replace the type of the given subprogram. Requires LLVM 21+.
"""
replace_type!(sp::DISubprogram, ty::DISubroutineType) =
    API.LLVMDISubprogramReplaceType(sp, ty)

end # @static version check


## compile unit

@vocabulary IR DICompileUnit
@vocabulary Build compile_unit!

"""
    DICompileUnit

A compilation unit in the source code.
"""
@checked struct DICompileUnit <: DIScope
    ref::API.LLVMMetadataRef
end
register(DICompileUnit, API.LLVMDICompileUnitMetadataKind)

"""
    compile_unit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                 optimized::Bool=true, cmdline::AbstractString="",
                 runtime_version::Integer=0,
                 split_name::Union{AbstractString,Nothing}=nothing,
                 emission_kind=LLVM.DWARFEmissionKind.Full,
                 dwo_id::Integer=0,
                 split_debug_inlining::Bool=true,
                 debug_info_for_profiling::Bool=false,
                 sysroot::AbstractString="", sdk::AbstractString="") -> DICompileUnit

Create a new [`DICompileUnit`](@ref). `lang` is a `LLVM.DWARFSourceLanguage.T`
value (e.g. `LLVM.DWARFSourceLanguage.Julia`). `cmdline` is a
command-line string embedded verbatim in the emitted debug info.
"""
function compile_unit!(builder::DIBuilder, lang, file::DIFile, producer::AbstractString;
                      optimized::Bool=true,
                      cmdline::AbstractString="",
                      runtime_version::Integer=0,
                      split_name::Union{AbstractString,Nothing}=nothing,
                      emission_kind=API.LLVMDWARFEmissionFull,
                      dwo_id::Integer=0,
                      split_debug_inlining::Bool=true,
                      debug_info_for_profiling::Bool=false,
                      sysroot::AbstractString="",
                      sdk::AbstractString="")
    split_name_ptr = split_name === nothing ? C_NULL : split_name
    split_name_len = split_name === nothing ? Csize_t(0) : Csize_t(ncodeunits(split_name))
    cu = DICompileUnit(API.LLVMDIBuilderCreateCompileUnit(
        builder, lang, file,
        producer, Csize_t(ncodeunits(producer)),
        optimized,
        cmdline, Csize_t(ncodeunits(cmdline)),
        Cuint(runtime_version),
        split_name_ptr, split_name_len,
        emission_kind,
        Cuint(dwo_id),
        split_debug_inlining,
        debug_info_for_profiling,
        sysroot, Csize_t(ncodeunits(sysroot)),
        sdk, Csize_t(ncodeunits(sdk))))
    builder.needs_finalization = true
    return cu
end


## module

@vocabulary IR DIModule
@vocabulary Build dimodule!

"""
    DIModule

A module in the source code (Clang modules / Fortran modules / Swift modules).
"""
@checked struct DIModule <: DIScope
    ref::API.LLVMMetadataRef
end
register(DIModule, API.LLVMDIModuleMetadataKind)

"""
    dimodule!(builder::DIBuilder, parent_scope::Union{DIScope,Nothing}, name::AbstractString;
              config_macros::AbstractString="", include_path::AbstractString="",
              api_notes_file::AbstractString="") -> DIModule

Create a new [`DIModule`](@ref) describing a module in the source code.
"""
function dimodule!(builder::DIBuilder, parent_scope::Union{DIScope,Nothing}, name::AbstractString;
                   config_macros::AbstractString="",
                   include_path::AbstractString="",
                   api_notes_file::AbstractString="")
    DIModule(API.LLVMDIBuilderCreateModule(
        builder, something(parent_scope, C_NULL),
        name, Csize_t(ncodeunits(name)),
        config_macros, Csize_t(ncodeunits(config_macros)),
        include_path, Csize_t(ncodeunits(include_path)),
        api_notes_file, Csize_t(ncodeunits(api_notes_file))))
end


## lexical block

@vocabulary IR DILexicalBlock, DILexicalBlockFile
@vocabulary Build lexical_block!, lexical_block_file!

"""
    DILexicalBlock

A lexical block (a nested scope, typically a compound statement) in the source code.
"""
@checked struct DILexicalBlock <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DILexicalBlock, API.LLVMDILexicalBlockMetadataKind)

"""
    DILexicalBlockFile

A lexical block that changes the current source file, e.g. due to an `#include`.
"""
@checked struct DILexicalBlockFile <: DILocalScope
    ref::API.LLVMMetadataRef
end
register(DILexicalBlockFile, API.LLVMDILexicalBlockFileMetadataKind)

"""
    lexical_block!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                  line::Integer, column::Integer) -> DILexicalBlock

Create a new [`DILexicalBlock`](@ref) describing a nested source scope.
"""
function lexical_block!(builder::DIBuilder, scope::DILocalScope, file::DIFile,
                       line::Integer, column::Integer)
    DILexicalBlock(API.LLVMDIBuilderCreateLexicalBlock(
        builder, scope, file, Cuint(line), Cuint(column)))
end

"""
    lexical_block_file!(builder::DIBuilder, scope::DILocalScope, file::DIFile;
                      discriminator::Integer=0) -> DILexicalBlockFile

Create a new [`DILexicalBlockFile`](@ref) for tracking source-file changes
within a lexical scope.
"""
function lexical_block_file!(builder::DIBuilder, scope::DILocalScope, file::DIFile;
                           discriminator::Integer=0)
    DILexicalBlockFile(API.LLVMDIBuilderCreateLexicalBlockFile(
        builder, scope, file, Cuint(discriminator)))
end


## namespace

@vocabulary IR DINamespace
@vocabulary Build namespace!

"""
    DINamespace

A namespace in the source code.
"""
@checked struct DINamespace <: DIScope
    ref::API.LLVMMetadataRef
end
register(DINamespace, API.LLVMDINamespaceMetadataKind)

"""
    namespace!(builder::DIBuilder, parent_scope::Union{DIScope,Nothing}, name::AbstractString;
               export_symbols::Bool=false) -> DINamespace

Create a new [`DINamespace`](@ref) describing a namespace in the source code.
"""
function namespace!(builder::DIBuilder, parent_scope::Union{DIScope,Nothing}, name::AbstractString;
                    export_symbols::Bool=false)
    DINamespace(API.LLVMDIBuilderCreateNameSpace(
        builder, something(parent_scope, C_NULL),
        name, Csize_t(ncodeunits(name)),
        export_symbols))
end


## imported entity

@vocabulary IR DIImportedEntity
@vocabulary Build imported_module!, imported_declaration!

"""
    DIImportedEntity

An imported entity, such as a C++ `using` declaration or module import.
"""
@checked struct DIImportedEntity <: DINode
    ref::API.LLVMMetadataRef
end
register(DIImportedEntity, API.LLVMDIImportedEntityMetadataKind)

"""
    imported_module!(builder::DIBuilder, scope::Union{DIScope,Nothing}, ns::DINamespace,
                     file::DIFile, line::Integer) -> DIImportedEntity
    imported_module!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                     entity::Union{DIModule,DIImportedEntity}, file::DIFile, line::Integer;
                     elements::AbstractVector{<:Metadata}=Metadata[]) -> DIImportedEntity

Create a new [`DIImportedEntity`](@ref LLVM.DIImportedEntity) that imports a namespace
(like C++'s `using namespace`), a module, or an alias of another imported entity into
`scope`. The `elements` of an imported module or alias are its renamed entities.
"""
imported_module!(builder::DIBuilder, scope::Union{DIScope,Nothing}, ns::DINamespace,
                 file::DIFile, line::Integer) =
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromNamespace(
        builder, something(scope, C_NULL), ns, file, Cuint(line)))

function imported_module!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                          alias::DIImportedEntity, file::DIFile, line::Integer;
                          elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromAlias(
        builder, something(scope, C_NULL), alias, file, Cuint(line),
        elts, Cuint(length(elts))))
end

function imported_module!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                          mod::DIModule, file::DIFile, line::Integer;
                          elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedModuleFromModule(
        builder, something(scope, C_NULL), mod, file, Cuint(line),
        elts, Cuint(length(elts))))
end

"""
    imported_declaration!(builder::DIBuilder, scope::Union{DIScope,Nothing}, decl::DINode,
                          file::DIFile, line::Integer, name::AbstractString;
                          elements::AbstractVector{<:Metadata}=Metadata[])
        -> DIImportedEntity

Create a new [`DIImportedEntity`](@ref LLVM.DIImportedEntity) that imports the declaration
`decl` (e.g., a variable, subprogram or type) into `scope`, as `name` (like C++'s
`using`).
"""
function imported_declaration!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                               decl::DINode, file::DIFile, line::Integer,
                               name::AbstractString;
                               elements::AbstractVector{<:Metadata}=Metadata[])
    elts = convert(Vector{Metadata}, elements)
    DIImportedEntity(API.LLVMDIBuilderCreateImportedDeclaration(
        builder, something(scope, C_NULL), decl, file, Cuint(line),
        name, Csize_t(ncodeunits(name)),
        elts, Cuint(length(elts))))
end
