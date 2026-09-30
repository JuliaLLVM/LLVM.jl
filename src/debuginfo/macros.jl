## macro

@vocabulary IR DIMacro, DIMacroFile
@vocabulary Build macro!, temp_macro_file!

"""
    DIMacro

A single preprocessor macro definition or undefinition.
"""
@checked struct DIMacro <: DINode
    ref::API.LLVMMetadataRef
end
register(DIMacro, API.LLVMDIMacroMetadataKind)

"""
    DIMacroFile

A collection of macro records corresponding to a single source file.
"""
@checked struct DIMacroFile <: DINode
    ref::API.LLVMMetadataRef
end
register(DIMacroFile, API.LLVMDIMacroFileMetadataKind)

"""
    macro!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
           line::Integer, record_type, name::AbstractString,
           value::AbstractString) -> DIMacro

Create a new [`DIMacro`](@ref). `record_type` is a
`LLVMDWARFMacinfoRecordType` value (e.g. `LLVM.API.LLVMDWARFMacinfoRecordTypeDefine`).
"""
function macro!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
                line::Integer, record_type, name::AbstractString,
                value::AbstractString)
    DIMacro(API.LLVMDIBuilderCreateMacro(
        builder, something(parent_macrofile, C_NULL), Cuint(line), record_type,
        name, Csize_t(ncodeunits(name)),
        value, Csize_t(ncodeunits(value))))
end

"""
    temp_macro_file!(builder::DIBuilder,
                   parent_macrofile::Union{DIMacroFile,Nothing},
                   line::Integer, file::DIFile) -> DIMacroFile

Create a new temporary [`DIMacroFile`](@ref), for use as the parent of [`macro!`](@ref).
Unlike other temporary nodes, the builder replaces it when it is finalized, which
invalidates the returned node.
"""
temp_macro_file!(builder::DIBuilder, parent_macrofile::Union{DIMacroFile,Nothing},
               line::Integer, file::DIFile) =
    DIMacroFile(API.LLVMDIBuilderCreateTempMacroFile(
        builder, something(parent_macrofile, C_NULL), Cuint(line), file))
