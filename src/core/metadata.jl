## core type

@vocabulary IR Metadata

"""
    Metadata

Abstract supertype for all metadata types.
"""
abstract type Metadata end
@properties Metadata

# subtypes must be immutable structs with a single `ref::API.LLVMMetadataRef` field (see
# `check_layout`), except for the field-less `MDNull`
@inline function Base.unsafe_convert(::Type{API.LLVMMetadataRef},
                                      @nospecialize(md::Metadata))
    md isa MDNull && return convert(API.LLVMMetadataRef, C_NULL)
    typecheck_enabled && check_layout(typeof(md), API.LLVMMetadataRef)
    unsafe_load_ref(API.LLVMMetadataRef, md)
end

@inline propref(@nospecialize(x::Metadata)) = Base.unsafe_convert(API.LLVMMetadataRef, x)

# avoid specializing the conversions performed by `ccall` on the concrete wrapper type.
# wrappers consist of nothing but their reference, so there's nothing else to keep alive.
Base.cconvert(::Type{API.LLVMMetadataRef}, @nospecialize(obj::Metadata)) = obj
function Base.cconvert(::Type{Ptr{API.LLVMMetadataRef}},
                       @nospecialize(objs::Vector{<:Metadata}))
    R = API.LLVMMetadataRef
    R[Base.unsafe_convert(R, obj) for obj in objs]
end

# XXX: LLVMMetadataKind is simply unsigned, so we don't know the max enum
const metadata_kinds = Vector{Type}(fill(Nothing, 64))
function identify(::Type{Metadata}, ref::API.LLVMMetadataRef)
    kind = API.LLVMGetMetadataKind(ref)
    typ = @inbounds metadata_kinds[kind+1]
    typ === Nothing && error("Unknown metadata kind $kind")
    return typ
end
function register(T::Type{<:Metadata}, kind)
    check_layout(T, API.LLVMMetadataRef)
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
    return unsafe_wrap_ref(T, ref)::Metadata
end

Base.string(md::Metadata) = unsafe_message(API.LLVMPrintMetadataToString(md))

function Base.show(io::IO, ::MIME"text/plain", md::Metadata)
    print(io, strip(string(md)))
end


## metadata as value

# this is for interfacing with (older) APIs that accept a Value*, not a Metadata*

"""
    LLVM.MetadataAsValue

Metadata wrapped as a regular value, for use in APIs that expect a `LLVM.Value`.

See also: [`Value(::Metadata)`](@ref) to convert back to a value.
"""
@checked struct MetadataAsValue <: Value
    ref::API.LLVMValueRef
end
@vocabulary IR MetadataAsValue
register(MetadataAsValue, API.LLVMMetadataAsValueValueKind)

"""
    Value(md::Metadata)

Wrap the given metadata as a value, for use in APIs that expect a `LLVM.Value`.

When the metadata is already a value wrapped as metadata, this will simply return the
original value.
"""
Value(md::Metadata) = Value(API.LLVMMetadataAsValue2(context(), md))

Base.convert(T::Type{<:Value}, val::Metadata) = Value(val)::T

# NOTE: we can't do this automatically, as we can't query the context of metadata...
#       add wrappers to do so? would also simplify, e.g., `string(::MDString)`


## value as metadata

"""
    LLVM.ValueAsMetadata

Abstract type for values wrapped as metadata, for use in APIs that expect a `LLVM.Metadata`.

See also: [`Metadata(::Value)`](@ref) to convert back to a metadata.
"""
abstract type ValueAsMetadata <: Metadata end
@vocabulary IR ValueAsMetadata

@checked struct ConstantAsMetadata <: ValueAsMetadata
    ref::API.LLVMMetadataRef
end
register(ConstantAsMetadata, API.LLVMConstantAsMetadataMetadataKind)

@checked struct LocalAsMetadata <: ValueAsMetadata
    ref::API.LLVMMetadataRef
end
register(LocalAsMetadata, API.LLVMLocalAsMetadataMetadataKind)

"""
    Metadata(val::Value)

Wrap the given value as metadata, for use in APIs that expect a `LLVM.Metadata`.

When the value is already metadata wrapped as a value, this will simply return the
original metadata.
"""
Metadata(val::Value) = Metadata(API.LLVMValueAsMetadata(val))

Base.convert(T::Type{<:Metadata}, val::Value) = Metadata(val)::T


## strings

@vocabulary IR MDString

"""
    MDString

A string metadata node.
"""
@checked struct MDString <: Metadata
    ref::API.LLVMMetadataRef
end
register(MDString, API.LLVMMDStringMetadataKind)

"""
    MDString(val::String)

Create a new string metadata node from the given Julia string.
"""
MDString(val::String) =
    MDString(API.LLVMMDStringInContext2(context(), val, ncodeunits(val)))

"""
    convert(String, md::MDString)

Get the string value of the given string metadata node.
"""
function Base.convert(::Type{String}, md::MDString)
    len = Ref{Cuint}()
    ptr = API.LLVMGetMDString2(md, len)
    return unsafe_string(convert(Ptr{Int8}, ptr), len[])
end


## nodes

@vocabulary IR MDNode

"""
    MDNode

Abstract supertype for metadata nodes that can have operands.

See also: [`MDTuple`](@ref) for a concrete subtype.

# Properties

    md.operands

The operands of the metadata node, as a view of the node that supports indexing and
iteration. Null operands are represented by `nothing`.

The view is mutable: assigning an operand, `md.operands[i] = new`, replaces it in place.
LLVM keeps uniqued nodes (i.e., nodes that are neither distinct nor temporary) unique, so if
the change makes the node identical to an existing one, the node is made distinct instead.
Nodes that refer to temporary nodes are replaced by that existing node and deleted.
"""
abstract type MDNode <: Metadata end

struct MDNodeOperandSet <: AbstractVector{Union{Metadata,Nothing}}
    md::MDNode
end

operands(md::MDNode) = MDNodeOperandSet(md)

@property MDNode operands

Base.size(iter::MDNodeOperandSet) = (Int(API.LLVMGetMDNodeNumOperands2(iter.md)),)

Base.IndexStyle(::Type{MDNodeOperandSet}) = IndexLinear()

function Base.getindex(iter::MDNodeOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    ref = API.LLVMGetMDNodeOperand2(iter.md, i-1)
    return ref == C_NULL ? nothing : Metadata(ref)
end

function Base.setindex!(iter::MDNodeOperandSet, new::Union{Metadata,Nothing}, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    API.LLVMReplaceMDNodeOperandWith2(iter.md, i-1, something(new, MDNull()))
    return iter
end

# NOTE: optimized `collect`
function Base.collect(iter::MDNodeOperandSet)
    ops = Vector{API.LLVMMetadataRef}(undef, length(iter))
    API.LLVMGetMDNodeOperands2(iter.md, ops)
    return Union{Metadata,Nothing}[op == C_NULL ? nothing : Metadata(op) for op in ops]
end


## tuples

@vocabulary IR MDTuple

"""
    MDTuple

A tuple metadata node.
"""
@checked struct MDTuple <: MDNode
    ref::API.LLVMMetadataRef
end
register(MDTuple, API.LLVMMDTupleMetadataKind)

"""
    MDNode(vals::Vector) -> MDTuple

Create a new tuple metadata node from the given operands.

Passing `nothing` as a value will result in a null operand.
"""
MDNode(vals::AbstractVector) =
    MDNode(convert(Vector{Metadata}, vals))
MDNode(mds::Vector{<:Metadata}) =
    MDTuple(API.LLVMMDNodeInContext2(context(), mds, length(mds)))

# we support passing `nothing`, but convert it to a non-exported `MDNull` instance
# so that we can keep everything as a subtype of `Metadata`
struct MDNull <: Metadata end
Base.convert(::Type{Metadata}, ::Nothing) = MDNull()


## metadata

@vocabulary IR MDKind

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
MDKind(name::String) = MDKind(API.LLVMGetMDKindIDInContext(context(), name, ncodeunits(name)))
MDKind(kind::MDKind) = kind

# instructions (using MetadataAsValue values)

struct InstructionMetadataDict <: AbstractDict{MDKind,Metadata}
    val::Instruction
end

metadata(inst::Instruction) = InstructionMetadataDict(inst)

@property Instruction metadata

Base.isempty(md::InstructionMetadataDict) = !Bool(API.LLVMHasMetadata(md.val))

Base.haskey(md::InstructionMetadataDict, key) =
  API.LLVMGetMetadata(md.val, MDKind(key)) != C_NULL

function Base.getindex(md::InstructionMetadataDict, key)
    kind = MDKind(key)
    objref = API.LLVMGetMetadata(md.val, kind)
    objref == C_NULL && throw(KeyError(kind))
    return Metadata(MetadataAsValue(objref))
  end

function Base.setindex!(md::InstructionMetadataDict, node::MDNode, key)
    API.LLVMSetMetadata(md.val, MDKind(key), Value(node))
    return md
end

function Base.delete!(md::InstructionMetadataDict, key)
    API.LLVMSetMetadata(md.val, MDKind(key), C_NULL)
    return md
end

# LLVM only supports fetching all metadata at once. despite its name, the C API function
# includes the debug location on some versions, so handle it separately.
function Base.iterate(md::InstructionMetadataDict)
    entries = Pair{MDKind,Metadata}[]
    haskey(md, MD_dbg) && push!(entries, MD_dbg => md[MD_dbg])
    num_entries = Ref{Csize_t}()
    ptr = API.LLVMInstructionGetAllMetadataOtherThanDebugLoc(md.val, num_entries)
    for i in 1:num_entries[]
        kind = MDKind(API.LLVMValueMetadataEntriesGetKind(ptr, i-1))
        kind == MD_dbg && continue
        entry = API.LLVMValueMetadataEntriesGetMetadata(ptr, i-1)
        push!(entries, kind => Metadata(entry))
    end
    API.LLVMDisposeValueMetadataEntries(ptr)
    iterate(md, (entries, 1))
end
function Base.iterate(::InstructionMetadataDict, (entries, i))
    i > length(entries) ? nothing : (entries[i], (entries, i+1))
end

Base.length(md::InstructionMetadataDict) = count(Returns(true), md)

# global objects (using Metadata values)

struct GlobalMetadataDict <: AbstractDict{MDKind,Metadata}
    val::GlobalObject
end

metadata(val::GlobalObject) = GlobalMetadataDict(val)

@property GlobalObject metadata

function Base.length(md::GlobalMetadataDict)
    num_entries = Ref{Csize_t}()
    valptr = API.LLVMGlobalCopyAllMetadata(md.val, num_entries)
    API.LLVMDisposeValueMetadataEntries(valptr)
    Int(num_entries[])
end

function Base.empty!(md::GlobalMetadataDict)
    API.LLVMGlobalClearMetadata(md.val)
    return md
end

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

function Base.setindex!(md::GlobalMetadataDict, node::Metadata, key)
    API.LLVMGlobalSetMetadata(md.val, MDKind(key), node)
    return md
end

Base.get(md::GlobalMetadataDict, key, default) = get(md, MDKind(key), default)
function Base.get(md::GlobalMetadataDict, key::MDKind, default)
    for (k, v) in md
        if k == key
            return v
        end
    end
    return default
end

Base.haskey(md::GlobalMetadataDict, key) = get(md, key, nothing) !== nothing
function Base.getindex(md::GlobalMetadataDict, key)
    val = get(md, key, nothing)
    val === nothing && throw(KeyError(key))
    return val
end

function Base.delete!(md::GlobalMetadataDict, key)
    API.LLVMGlobalEraseMetadata(md.val, MDKind(key))
    return md
end


## named metadata

@vocabulary IR NamedMDNode

"""
    NamedMDNode

A named metadata node, which is a collection of metadata nodes with a name.

# Properties

    node.name

The name of the named metadata node.

    node.operands

The operands of the named metadata node, as a view of the node that supports indexing and
iteration. The view is mutable, and supports:

- `push!(node.operands, md::MDNode)`: append an operand;
- `node.operands[i] = md::MDNode`: replace an operand;
- `empty!(node.operands)`: remove all operands.

    node.next
    node.prev

The next or previous named metadata node in the module, or `nothing` if there is none.
"""
struct NamedMDNode
    mod::LLVM.Module # not exposed by the API
    ref::API.LLVMNamedMDNodeRef
end
@properties NamedMDNode

Base.unsafe_convert(::Type{API.LLVMNamedMDNodeRef}, node::NamedMDNode) = node.ref

function name(node::NamedMDNode)
    len = Ref{Csize_t}()
    data = API.LLVMGetNamedMetadataName(node, len)
    unsafe_string(convert(Ptr{Int8}, data), len[])
end

@property NamedMDNode name

function Base.show(io::IO, mime::MIME"text/plain", node::NamedMDNode)
    print(io, "!$(name(node)) = !{")
    for (i, op) in enumerate(operands(node))
        i > 1 && print(io, ", ")
        show(io, mime, op)
    end
    print(io, "}")
    return io
end

struct NamedMDNodeOperandSet <: AbstractVector{MDNode}
    node::NamedMDNode
end

operands(node::NamedMDNode) = NamedMDNodeOperandSet(node)

@property NamedMDNode operands

function next(node::NamedMDNode)
    ref = API.LLVMGetNextNamedMetadata(node)
    ref == C_NULL ? nothing : NamedMDNode(node.mod, ref)
end

function prev(node::NamedMDNode)
    ref = API.LLVMGetPreviousNamedMetadata(node)
    ref == C_NULL ? nothing : NamedMDNode(node.mod, ref)
end

@property NamedMDNode next
@property NamedMDNode prev

Base.size(iter::NamedMDNodeOperandSet) =
    (Int(API.LLVMGetNamedMetadataNumOperands2(iter.node)),)

Base.IndexStyle(::Type{NamedMDNodeOperandSet}) = IndexLinear()

function Base.getindex(iter::NamedMDNodeOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Metadata(API.LLVMGetNamedMetadataOperand2(iter.node, i-1))::MDNode
end

function Base.setindex!(iter::NamedMDNodeOperandSet, md::MDNode, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    API.LLVMSetNamedMetadataOperand2(iter.node, i-1, md)
    return iter
end

function Base.push!(iter::NamedMDNodeOperandSet, md::MDNode)
    API.LLVMAddNamedMetadataOperand2(iter.node, md)
    return iter
end

function Base.empty!(iter::NamedMDNodeOperandSet)
    API.LLVMClearNamedMetadataOperands(iter.node)
    return iter
end

# NOTE: optimized `collect`
function Base.collect(iter::NamedMDNodeOperandSet)
    ops = Vector{API.LLVMMetadataRef}(undef, length(iter))
    isempty(ops) || API.LLVMGetNamedMetadataOperands2(iter.node, ops)
    return MDNode[Metadata(op) for op in ops]
end


## module named metadata

struct ModuleMetadataIterator <: AbstractDict{String,NamedMDNode}
    mod::Module
end

metadata(mod::Module) = ModuleMetadataIterator(mod)

@property Module metadata

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

Base.isempty(iter::ModuleMetadataIterator) =
    API.LLVMGetLastNamedMetadata(iter.mod) == C_NULL

Base.length(iter::ModuleMetadataIterator) = count(Returns(true), iter)

function Base.haskey(iter::ModuleMetadataIterator, name::String)
    return API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name)) != C_NULL
end

function Base.getindex(iter::ModuleMetadataIterator, name::String)
    ref = API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name))
    ref == C_NULL && throw(KeyError(name))
    return NamedMDNode(iter.mod, ref)
end

function Base.get(iter::ModuleMetadataIterator, name::String, default)
    ref = API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name))
    ref == C_NULL ? default : NamedMDNode(iter.mod, ref)
end

"""
    get!(mod.metadata, name::String)

Look up the named metadata node called `name`, or create an empty one if the module doesn't
contain it, e.g., to add metadata to it: `push!(get!(mod.metadata, name).operands, node)`.
"""
function Base.get!(iter::ModuleMetadataIterator, name::String)
    ref = API.LLVMGetOrInsertNamedMetadata(iter.mod, name, ncodeunits(name))
    return NamedMDNode(iter.mod, ref)
end
