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
Base.@nospecializeinfer function register(@nospecialize(T::Type{<:Metadata}), kind)
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
    MDString(val::AbstractString)

Create a new string metadata node from the given Julia string.
"""
function MDString(val::AbstractString)
    val = String(val)
    MDString(API.LLVMMDStringInContext2(context(), val, ncodeunits(val)))
end

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
    MDTuple(elements::AbstractVector) -> MDTuple

Create a new tuple metadata node with the given elements as its operands, or get the
existing one, in the task-local [`context`](@ref).

Passing `nothing` as an element will result in a null operand.
"""
function MDTuple(elements::AbstractVector)
    mds = convert(Vector{Metadata}, elements)
    MDTuple(API.LLVMMDNodeInContext2(context(), mds, length(mds)))
end

"""
    MDNode(elements::AbstractVector) -> MDTuple

Create a new tuple metadata node. Equivalent to [`MDTuple(elements)`](@ref
MDTuple(::AbstractVector)).
"""
MDNode(elements::AbstractVector) = MDTuple(elements)

# we support passing `nothing`, but convert it to a non-exported `MDNull` instance
# so that we can keep everything as a subtype of `Metadata`
struct MDNull <: Metadata end
Base.convert(::Type{Metadata}, ::Nothing) = MDNull()


## temporary nodes

@vocabulary IR TemporaryMDNode, replace_temporary!

"""
    TemporaryMDNode{T<:MDNode}

A temporary metadata node: a placeholder that other metadata can refer to before the node
it stands for can be created, e.g., to create metadata that refers to itself.

The handle owns the temporary node, which needs to be replaced with
[`replace_temporary!`](@ref), or disposed of with [`dispose`](@ref), which replaces its
uses with null operands. Either one consumes the handle, after which disposing of it
again does nothing, so the handle can be disposed of with `@dispose` or the do-block form
of the constructor even if it was replaced.

# Properties

    temp.node

The temporary node, a `T`, for use as an operand of other metadata. Throws an
`ArgumentError` once the handle has been consumed, which also invalidates the nodes that
were obtained from it before.
"""
mutable struct TemporaryMDNode{T<:MDNode}
    ref::API.LLVMMetadataRef
    owned::Bool

    function TemporaryMDNode{T}(ref::API.LLVMMetadataRef) where {T<:MDNode}
        ref == C_NULL && throw(UndefRefError())
        mark_alloc(new{T}(ref, true))
    end
end
@properties TemporaryMDNode

"""
    TemporaryMDNode(operands=Metadata[]) -> TemporaryMDNode{MDTuple}
    TemporaryMDNode(f::Function, operands=Metadata[])

Create a temporary tuple node with the given operands, in the task-local
[`context`](@ref). Passing `nothing` as an operand results in a null operand.

The do-block form calls `f` with the handle and disposes of it afterwards, unless `f`
replaced it, and returns the result of `f`:

```julia
node = TemporaryMDNode() do temp
    replace_temporary!(temp, MDNode([temp.node, MDString("loop")]))
end
```
"""
function TemporaryMDNode(operands::AbstractVector=Metadata[])
    ops = convert(Vector{Metadata}, operands)
    TemporaryMDNode{MDTuple}(API.LLVMTemporaryMDNode(context(), ops, length(ops)))
end

TemporaryMDNode(f::Core.Function, args...) =
    with_disposal(f, TemporaryMDNode(args...))

function Base.show(io::IO, temp::TemporaryMDNode)
    print(io, typeof(temp), "(")
    if getfield(temp, :owned)
        print(io, strip(string(node(temp))))
    else
        print(io, "consumed")
    end
    print(io, ")")
end

function check_owned(temp::TemporaryMDNode)
    getfield(temp, :owned) ||
        throw(ArgumentError("The temporary metadata node has been replaced or disposed of"))
    return mark_use(temp)
end

node(temp::TemporaryMDNode{T}) where {T} = T(check_owned(temp).ref)

@property TemporaryMDNode node

"""
    replace_temporary!(temp::TemporaryMDNode, replacement::Metadata) -> replacement

Replace every use of the temporary node `temp` with `replacement`, and delete the
temporary node, consuming the handle.

A uniqued node that used `temp` can become identical to an existing node, in which case
LLVM replaces it by that node and deletes it, invalidating it.
"""
function replace_temporary!(temp::TemporaryMDNode, replacement::Metadata)
    check_owned(temp)
    Base.unsafe_convert(API.LLVMMetadataRef, replacement) == temp.ref &&
        throw(ArgumentError("Cannot replace a temporary metadata node with itself"))
    setfield!(temp, :owned, false)
    mark_dispose(temp) do temp
        API.LLVMMetadataReplaceAllUsesWith(temp.ref, replacement)
    end
    return replacement
end

"""
    dispose(temp::TemporaryMDNode)

Delete the temporary node `temp`, replacing its uses with null operands, unless the
handle has already been consumed.
"""
function dispose(temp::TemporaryMDNode)
    getfield(temp, :owned) || return
    setfield!(temp, :owned, false)
    mark_dispose(temp) do temp
        API.LLVMDisposeTemporaryMDNode(temp.ref)
    end
end


## metadata

@vocabulary IR MDKind, MD_dbg, MD_tbaa, MD_prof, MD_fpmath, MD_range, MD_tbaa_struct,
               MD_invariant_load, MD_alias_scope, MD_noalias, MD_nontemporal,
               MD_mem_parallel_loop_access, MD_nonnull, MD_dereferenceable,
               MD_dereferenceable_or_null, MD_make_implicit, MD_unpredictable,
               MD_invariant_group, MD_align, MD_loop, MD_type, MD_section_prefix,
               MD_absolute_symbol, MD_associated

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

"""
    MDKind
    MDKind(name::AbstractString; context=context())

The kinds of metadata, which index the metadata of instructions and global objects. The
kinds that LLVM knows about have a fixed value, like `MD_dbg` (for `!dbg`), `MD_tbaa` or
`MD_invariant_load`, while the kinds of other names are specific to a context, and can be
looked up by name (in the active context by default).

Instead of a kind, the metadata of instructions and global objects can also be indexed by
the name of the kind, like `inst.metadata["tbaa"]`, which is looked up in the context of
the instruction or global object.
"""
MDKind

function MDKind(name::AbstractString; context::Context=LLVM.context())
    str = String(name)
    MDKind(API.LLVMGetMDKindIDInContext(context, str, ncodeunits(str)))
end
MDKind(kind::MDKind) = kind

# the names of the fixed metadata kinds, from llvm/IR/FixedMetadataKinds.def
const fixed_md_kind_names = (
    :MD_dbg => "dbg", :MD_tbaa => "tbaa", :MD_prof => "prof", :MD_fpmath => "fpmath",
    :MD_range => "range", :MD_tbaa_struct => "tbaa.struct",
    :MD_invariant_load => "invariant.load", :MD_alias_scope => "alias.scope",
    :MD_noalias => "noalias", :MD_nontemporal => "nontemporal",
    :MD_mem_parallel_loop_access => "llvm.mem.parallel_loop_access",
    :MD_nonnull => "nonnull", :MD_dereferenceable => "dereferenceable",
    :MD_dereferenceable_or_null => "dereferenceable_or_null",
    :MD_make_implicit => "make.implicit", :MD_unpredictable => "unpredictable",
    :MD_invariant_group => "invariant.group", :MD_align => "align",
    :MD_loop => "llvm.loop", :MD_type => "type", :MD_section_prefix => "section_prefix",
    :MD_absolute_symbol => "absolute_symbol", :MD_associated => "associated")

for (sym, name) in fixed_md_kind_names
    doc = """
        $sym

    The kind of `!$name` metadata, which has a fixed value. See [`MDKind`](@ref).
    """
    @eval @doc $doc $sym
end

# the metadata kind for a key of the metadata of a value, looking up names in its context
md_kind(val::Value, kind) = MDKind(kind)
md_kind(val::Value, name::AbstractString) = MDKind(name; context=context(val))

# instructions (using MetadataAsValue values)

struct InstructionMetadataDict <: AbstractDict{MDKind,Metadata}
    val::Instruction
end

metadata(inst::Instruction) = InstructionMetadataDict(inst)

@property Instruction metadata

Base.isempty(md::InstructionMetadataDict) = !Bool(API.LLVMHasMetadata(md.val))

Base.haskey(md::InstructionMetadataDict, key) =
  API.LLVMGetMetadata(md.val, md_kind(md.val, key)) != C_NULL

function Base.getindex(md::InstructionMetadataDict, key)
    kind = md_kind(md.val, key)
    objref = API.LLVMGetMetadata(md.val, kind)
    objref == C_NULL && throw(KeyError(kind))
    return Metadata(MetadataAsValue(objref))
  end

function Base.setindex!(md::InstructionMetadataDict, node::MDNode, key)
    API.LLVMSetMetadata(md.val, md_kind(md.val, key), Value(node))
    return md
end

function Base.delete!(md::InstructionMetadataDict, key)
    API.LLVMSetMetadata(md.val, md_kind(md.val, key), C_NULL)
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
    metadata = Pair{MDKind,Metadata}[]
    try
        for i in 1:num_entries[]
            kind = API.LLVMValueMetadataEntriesGetKind(entries, i-1)
            entry = API.LLVMValueMetadataEntriesGetMetadata(entries, i-1)
            push!(metadata, MDKind(kind) => Metadata(entry))
        end
    finally
        API.LLVMDisposeValueMetadataEntries(entries)
    end
    iterate(md, (metadata, 1))
end
function Base.iterate(md::GlobalMetadataDict, (metadata, i))
    i > length(metadata) ? nothing : (metadata[i], (metadata, i+1))
end

function Base.setindex!(md::GlobalMetadataDict, node::Metadata, key)
    API.LLVMGlobalSetMetadata(md.val, md_kind(md.val, key), node)
    return md
end

Base.get(md::GlobalMetadataDict, key, default) = get(md, md_kind(md.val, key), default)
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
    API.LLVMGlobalEraseMetadata(md.val, md_kind(md.val, key))
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

function Base.haskey(iter::ModuleMetadataIterator, name::AbstractString)
    name = String(name)
    return API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name)) != C_NULL
end

function Base.getindex(iter::ModuleMetadataIterator, name::AbstractString)
    name = String(name)
    ref = API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name))
    ref == C_NULL && throw(KeyError(name))
    return NamedMDNode(iter.mod, ref)
end

function Base.get(iter::ModuleMetadataIterator, name::AbstractString, default)
    name = String(name)
    ref = API.LLVMGetNamedMetadata(iter.mod, name, ncodeunits(name))
    ref == C_NULL ? default : NamedMDNode(iter.mod, ref)
end

"""
    get!(mod.metadata, name::AbstractString)

Look up the named metadata node called `name`, or create an empty one if the module doesn't
contain it, e.g., to add metadata to it: `push!(get!(mod.metadata, name).operands, node)`.
"""
function Base.get!(iter::ModuleMetadataIterator, name::AbstractString)
    name = String(name)
    ref = API.LLVMGetOrInsertNamedMetadata(iter.mod, name, ncodeunits(name))
    return NamedMDNode(iter.mod, ref)
end
