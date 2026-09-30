## debug info builder

@vocabulary Build DIBuilder, finalize!

"""
    DIBuilder

A builder for constructing debug information metadata.

This object needs to be disposed of using [`dispose`](@ref), which also
finalizes the debug info. Call [`finalize!`](@ref) explicitly only if you
need to use the finalized debug info (e.g. emit code) *before* disposing of
the builder.

The scope of declarations (types, subprograms, global variables, namespaces, modules and
imported entities) can be `nothing`, for a declaration at the top level. Local variables,
labels, lexical blocks and locations require a [`DILocalScope`](@ref).
"""
@checked mutable struct DIBuilder
    ref::API.LLVMDIBuilderRef
    needs_finalization::Bool
end

Base.unsafe_convert(::Type{API.LLVMDIBuilderRef}, builder::DIBuilder) =
    mark_use(builder).ref

"""
    DIBuilder(mod::Module; allow_unresolved::Bool=true)

Create a new debug info builder that emits metadata into `mod`.

When `allow_unresolved` is `true` (the default), the builder collects unresolved
metadata nodes attached to the module so that cycles can be resolved during
[`dispose`](@ref). When `false`, the builder errors on unresolved nodes instead.
"""
function DIBuilder(mod::Module; allow_unresolved::Bool=true)
    ref = allow_unresolved ? API.LLVMCreateDIBuilder(mod) :
                             API.LLVMCreateDIBuilderDisallowUnresolved(mod)
    mark_alloc(DIBuilder(ref, false))
end

"""
    dispose(builder::DIBuilder)

Finalize the debug info and dispose of the builder. Finalization populates
the compile unit's enum/retained-type/global/imported-entity/macro arrays,
seals each subprogram's retained-nodes list, and resolves remaining cycles.
If no compile unit was registered with the builder, or the debug info was
already finalized through [`finalize!`](@ref), finalization is skipped:
`DIBuilder::finalize` is not idempotent (e.g., re-finalizing accesses
already-deleted temporary macro files).
"""
function dispose(builder::DIBuilder)
    finalize!(builder)
    mark_dispose(API.LLVMDisposeDIBuilder, builder)
end

DIBuilder(f::Core.Function, args...; kwargs...) =
    with_disposal(f, DIBuilder(args...; kwargs...))

Base.show(io::IO, builder::DIBuilder) = @printf(io, "DIBuilder(%p)", builder.ref)

"""
    finalize!(builder::DIBuilder)

Resolve any unresolved metadata nodes and mark all compile units finalized.
Called automatically by [`dispose`](@ref); call explicitly only if the
DI-enriched module must be consumed (e.g. for code emission) before the
builder is disposed of. Skipped if no compile unit has been registered, or
if the debug info has already been finalized.
"""
function finalize!(builder::DIBuilder)
    if builder.needs_finalization
        API.LLVMDIBuilderFinalize(builder)
        builder.needs_finalization = false
    end
    return
end
