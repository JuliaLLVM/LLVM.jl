## core type

export Metadata

# subtypes are expected to have a 'ref::API.LLVMMetadataRef' field
abstract type Metadata end

Base.unsafe_convert(::Type{API.LLVMMetadataRef}, md::Metadata) = md.ref

# XXX: LLVMMetadataKind is simply unsigned, so we don't know the max enum
const metadata_kinds = Vector{Type}(fill(Nothing, 64))
function identify(::Type{Metadata}, ref::API.LLVMMetadataRef)
    kind = API.LLVMGetMetadataKind(ref)
    typ = @inbounds metadata_kinds[kind+1]
    typ === Nothing && error("Unknown metadata kind $kind")
    return typ
end
function register(T::Type{<:Metadata}, kind)
    metadata_kinds[kind+1] = T
end

function refcheck(::Type{T}, ref::API.LLVMMetadataRef) where T<:Metadata
    ref==C_NULL && throw(UndefRefError())
    if typecheck_enabled
        T′ = identify(Metadata, ref)
        if T != T′
            error("invalid conversion of $T′ metadata reference to $T")
        end
    end
end

# Construct a concretely typed metadata object from an abstract metadata ref
function Metadata(ref::API.LLVMMetadataRef)
    ref == C_NULL && throw(UndefRefError())
    T = identify(Metadata, ref)
    return T(ref)
end

Base.string(md::Metadata) = unsafe_message(API.LLVMPrintMetadataToString(md))

function Base.show(io::IO, ::MIME"text/plain", md::Metadata)
    print(io, strip(string(md)))
end


## metadata as value

# this is for interfacing with (older) APIs that accept a Value*, not a Metadata*

@checked struct MetadataAsValue <: Value
    ref::API.LLVMValueRef
end
register(MetadataAsValue, API.LLVMMetadataAsValueValueKind)

# NOTE: this can be used to both pack e.g. metadata as a value, and to extract the
#       value from an ValueAsMetadata, so we don't type-assert narrowly here
Value(md::Metadata) = Value(API.LLVMMetadataAsValue2(context(), md))

Base.convert(T::Type{<:Value}, val::Metadata) = Value(val)::T

# NOTE: we can't do this automatically, as we can't query the context of metadata...
#       add wrappers to do so? would also simplify, e.g., `string(::MDString)`


## value as metadata

abstract type ValueAsMetadata <: Metadata end

@checked struct ConstantAsMetadata <: ValueAsMetadata
    ref::API.LLVMMetadataRef
end
register(ConstantAsMetadata, API.LLVMConstantAsMetadataMetadataKind)

@checked struct LocalAsMetadata <: ValueAsMetadata
    ref::API.LLVMMetadataRef
end
register(LocalAsMetadata, API.LLVMLocalAsMetadataMetadataKind)

# NOTE: this can be used to both pack e.g. constants as metadata, and to extract the
#       metadata from an MetadataAsValue, so we don't type-assert narrowly here
Metadata(val::Value) = Metadata(API.LLVMValueAsMetadata(val))

Base.convert(T::Type{<:Metadata}, val::Value) = Metadata(val)::T


## strings

export MDString

@checked struct MDString <: Metadata
    ref::API.LLVMMetadataRef
end
register(MDString, API.LLVMMDStringMetadataKind)

MDString(val::String) =
    MDString(API.LLVMMDStringInContext2(context(), val, length(val)))

function Base.convert(::Type{String}, md::MDString)
    len = Ref{Cuint}()
    ptr = API.LLVMGetMDString2(md, len)
    return unsafe_string(convert(Ptr{Int8}, ptr), len[])
end


## nodes

export MDNode, operands

abstract type MDNode <: Metadata end

function operands(md::MDNode)
    nops = API.LLVMGetMDNodeNumOperands2(md)
    ops = Vector{API.LLVMMetadataRef}(undef, nops)
    API.LLVMGetMDNodeOperands2(md, ops)
    return [op == C_NULL ? nothing : Metadata(op) for op in ops]
end

# TODO: setindex?
function replace_operand(md::MDNode, i, new::Metadata)
    API.LLVMReplaceMDNodeOperandWith2(md, i-1, new)
end


## tuples

export MDTuple

@checked struct MDTuple <: MDNode
    ref::API.LLVMMetadataRef
end
register(MDTuple, API.LLVMMDTupleMetadataKind)

# MDTuples are commonly referred to as MDNodes, so keep that name
MDNode(mds::Vector{<:Metadata}) =
    MDTuple(API.LLVMMDNodeInContext2(context(), mds, length(mds)))
MDNode(vals::Vector) =
    MDNode(convert(Vector{Metadata}, vals))

# we support passing `nothing`, but convert it to a non-exported `MDNull` instance
# so that we can keep everything as a subtype of `Metadata`
struct MDNull <: Metadata end
Base.convert(::Type{Metadata}, ::Nothing) = MDNull()
Base.unsafe_convert(::Type{API.LLVMMetadataRef}, md::MDNull) =
    convert(API.LLVMMetadataRef, C_NULL)


## metadata

export metadata, MDKind

@cenum(MDKind, MD_dbg = 0,
               MD_tbaa = 1,
               MD_prof = 2,
               MD_fpmath = 3,
               MD_range = 4,
               MD_tbaa_struct = 5,
               MD_invariant_load = 6,
               MD_alias_scope = 7,
               MD_noalias = 8,
               MD_nontemporal = 9,
               MD_mem_parallel_loop_access = 10,
               MD_nonnull = 11,
               MD_dereferenceable = 12,
               MD_dereferenceable_or_null = 13,
               MD_make_implicit = 14,
               MD_unpredictable = 15,
               MD_invariant_group = 16,
               MD_align = 17,
               MD_loop = 18,
               MD_type = 19,
               MD_section_prefix = 20,
               MD_absolute_symbol = 21,
               MD_associated = 22)
MDKind(name::String) = API.LLVMGetMDKindIDInContext(context(), name, length(name))
MDKind(kind::MDKind) = kind

# XXX: automatically converting string keys to MDKind is too automagical

# instructions (using MetadataAsValue values)

# TODO: doesn't actually iterate, since we can't list the available keys
struct InstructionMetadataDict <: AbstractDict{MDKind,MetadataAsValue}
    val::Value
end

metadata(val::Value) = InstructionMetadataDict(val)

Base.isempty(md::InstructionMetadataDict) = !Bool(API.LLVMHasMetadata(md.val))

Base.haskey(md::InstructionMetadataDict, key) =
  API.LLVMGetMetadata(md.val, MDKind(key)) != C_NULL

function Base.getindex(md::InstructionMetadataDict, key)
    kind = MDKind(key)
    objref = API.LLVMGetMetadata(md.val, kind)
    objref == C_NULL && throw(KeyError(kind))
    return Metadata(MetadataAsValue(objref))
  end

Base.setindex!(md::InstructionMetadataDict, node::Metadata, key) =
    API.LLVMSetMetadata(md.val, MDKind(key), Value(node))

Base.delete!(md::InstructionMetadataDict, key) =
    API.LLVMSetMetadata(md.val, MDKind(key), C_NULL)

# global objects (using Metadata values)

struct GlobalMetadataDict <: AbstractDict{MDKind,Metadata}
    val::GlobalObject
end

metadata(val::GlobalObject) = GlobalMetadataDict(val)

function Base.length(md::GlobalMetadataDict)
    num_entries = Ref{Csize_t}()
    valptr = API.LLVMGlobalCopyAllMetadata(md.val, num_entries)
    API.LLVMDisposeValueMetadataEntries(valptr)
    Int(num_entries[])
end

Base.empty!(md::GlobalMetadataDict) = API.LLVMGlobalClearMetadata(md.val)

function Base.iterate(md::GlobalMetadataDict)
    num_entries = Ref{Csize_t}()
    entries = API.LLVMGlobalCopyAllMetadata(md.val, num_entries)
    num_entries[] == 0 && return nothing

    metadata = Pair{MDKind,Metadata}[]
    for i in 1:num_entries[]
        kind = API.LLVMValueMetadataEntriesGetKind(entries, i-1)
        entry = API.LLVMValueMetadataEntriesGetMetadata(entries, i-1)
        metadata = push!(metadata, MDKind(kind) => Metadata(entry))
    end
    API.LLVMDisposeValueMetadataEntries(entries)

    val, state = iterate(metadata)
    val, (state, metadata)
end
function Base.iterate(md::GlobalMetadataDict, (state, metadata))
    out = iterate(metadata, state)
    out === nothing && return nothing
    val, state = out
    val, (state, metadata)
end

Base.setindex!(md::GlobalMetadataDict, node::Metadata, key) =
    API.LLVMGlobalSetMetadata(md.val, MDKind(key), node)

Base.delete!(md::GlobalMetadataDict, key) =
    API.LLVMGlobalEraseMetadata(md.val, MDKind(key))


## named metadata

export NamedMDNode, operands

# a named metadata note, tying together a name and a MDNode

struct NamedMDNode
    mod::LLVM.Module # not exposed by the API
    ref::API.LLVMNamedMDNodeRef
end

Base.unsafe_convert(::Type{API.LLVMNamedMDNodeRef}, node::NamedMDNode) = node.ref

function name(node::NamedMDNode)
    len = Ref{Csize_t}()
    data = API.LLVMGetNamedMetadataName(node, len)
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

function Base.show(io::IO, mime::MIME"text/plain", node::NamedMDNode)
    print(io, "!$(name(node)) = !{")
    for (i, op) in enumerate(operands(node))
        i > 1 && print(io, ", ")
        show(io, mime, op)
    end
    print(io, "}")
    return io
end

function operands(node::NamedMDNode)
    nops = API.LLVMGetNamedMetadataNumOperands2(node)
    ops = Vector{API.LLVMMetadataRef}(undef, nops)
    if nops > 0
        API.LLVMGetNamedMetadataOperands2(node, ops)
    end
    return [Metadata(op) for op in ops]
end

Base.push!(node::NamedMDNode, val::MDNode) =
    API.LLVMAddNamedMetadataOperand2(node, val)


## module named metadata

export metadata

struct ModuleMetadataIterator <: AbstractDict{String,NamedMDNode}
    mod::Module
end

"""
    metadata(mod)

Fetch the module-level named metadata. This can be inspected using a Dict-like interface.
Mutation is different: There is no `setindex!` method, as named metadata is append-only.
Instead, fetch the named metadata node using `getindex`, and `push!` to it.
"""
metadata(mod::Module) = ModuleMetadataIterator(mod)

function Base.show(io::IO, mime::MIME"text/plain", iter::ModuleMetadataIterator)
    print(io, "ModuleMetadataIterator for module $(name(iter.mod))")
    if !isempty(iter)
        print(io, ":")
        for (key,val) in iter
            print(io, "\n  ")
            show(io, mime, val)
        end
    end
    return io
end

function Base.iterate(iter::ModuleMetadataIterator, state=API.LLVMGetFirstNamedMetadata(iter.mod))
    if state == C_NULL
        nothing
    else
        node = NamedMDNode(iter.mod, state)
        (name(node) => node, API.LLVMGetNextNamedMetadata(state))
    end
end

function Base.first(iter::ModuleMetadataIterator)
    ref = API.LLVMGetFirstNamedMetadata(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    NamedMDNode(iter.mod, ref)
end

function Base.last(iter::ModuleMetadataIterator)
    ref = API.LLVMGetLastNamedMetadata(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    NamedMDNode(iter.mod, ref)
end

Base.isempty(iter::ModuleMetadataIterator) =
    API.LLVMGetLastNamedMetadata(iter.mod) == C_NULL

Base.IteratorSize(::ModuleMetadataIterator) = Base.SizeUnknown()

function Base.haskey(iter::ModuleMetadataIterator, name::String)
    return API.LLVMGetNamedMetadata(iter.mod, name, length(name)) != C_NULL
end

function Base.getindex(iter::ModuleMetadataIterator, name::String)
    ref = API.LLVMGetOrInsertNamedMetadata(iter.mod, name, length(name))
    @assert ref != C_NULL
    node = NamedMDNode(iter.mod, ref)
    return node
end
