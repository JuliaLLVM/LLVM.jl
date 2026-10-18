# Utilities for working with LLVM source code

## value maps

@vocabulary IR ValueMap, map_value, map_metadata

"""
    ValueMap()

Create a map from values (and metadata) to the values (and metadata) that replace them, like
LLVM's `ValueToValueMapTy`. Value maps are used by [`map_value`](@ref),
[`map_metadata`](@ref) and [`clone_into!`](@ref), which look up the replacements of values
in it, and extend it with the replacements they create. Using a single map for several
operations, e.g., when rebuilding the global values of a module and then cloning the
bodies of its functions, makes them use each other's replacements.

A value map is a dictionary from `Value` to `Value`. Its entries track their key and value:
when the key is replaced using [`replace_uses!`](@ref), the entry moves to the
replacement, and it is removed when the key is deleted; when the value is replaced, the
entry refers to the replacement, and it is ignored when the value is deleted. The entries
for metadata are available as a separate dictionary, the `metadata` property.

This object needs to be disposed of using [`dispose`](@ref), or by using the do-block form
`ValueMap() do vmap ... end`.

# Properties

    vmap.metadata

The part of the map for metadata, as a mutable dictionary from `Metadata` to `Metadata`,
e.g., `vmap.metadata[md] = md` to have metadata mapped to itself instead of being cloned.
"""
struct ValueMap <: AbstractDict{Value,Value}
    ref::API.LLVMValueMapRef

    ValueMap(ref::API.LLVMValueMapRef) = (refcheck(ValueMap, ref); new(ref))
end
@properties ValueMap

Base.unsafe_convert(::Type{API.LLVMValueMapRef}, vmap::ValueMap) = mark_use(vmap).ref

ValueMap() = mark_alloc(ValueMap(API.LLVMExtraCreateValueMap()))

ValueMap(f::Core.Function) = with_disposal(f, ValueMap())

"""
    dispose(vmap::ValueMap)

Dispose of the given value map.
"""
dispose(vmap::ValueMap) = mark_dispose(API.LLVMExtraDisposeValueMap, vmap)

function Base.get(vmap::ValueMap, key::Value, default)
    ref = API.LLVMExtraValueMapLookup(vmap, key)
    ref == C_NULL ? default : Value(ref)
end
Base.get(vmap::ValueMap, key, default) = default

function Base.getindex(vmap::ValueMap, key::Value)
    val = get(vmap, key, nothing)
    val === nothing && throw(KeyError(key))
    return val
end

Base.haskey(vmap::ValueMap, key) = get(vmap, key, nothing) !== nothing

function Base.setindex!(vmap::ValueMap, val::Value, key::Value)
    API.LLVMExtraValueMapSet(vmap, key, val)
    return vmap
end

function Base.delete!(vmap::ValueMap, key::Value)
    API.LLVMExtraValueMapErase(vmap, key)
    return vmap
end

function Base.empty!(vmap::ValueMap)
    API.LLVMExtraValueMapClear(vmap)
    return vmap
end

Base.length(vmap::ValueMap) = Int(API.LLVMExtraValueMapGetNumEntries(vmap))

function Base.iterate(vmap::ValueMap)
    n = length(vmap)
    keys = Vector{API.LLVMValueRef}(undef, n)
    vals = Vector{API.LLVMValueRef}(undef, n)
    n > 0 && API.LLVMExtraValueMapGetEntries(vmap, keys, vals)
    entries = Pair{Value,Value}[Value(k) => Value(v) for (k, v) in zip(keys, vals)]
    iterate(vmap, (entries, 1))
end
function Base.iterate(::ValueMap, (entries, i))
    i > length(entries) ? nothing : (entries[i], (entries, i+1))
end

struct ValueMapMetadata <: AbstractDict{Metadata,Metadata}
    vmap::ValueMap
end

metadata(vmap::ValueMap) = ValueMapMetadata(vmap)

@property ValueMap metadata

function Base.get(md::ValueMapMetadata, key::Metadata, default)
    ref = API.LLVMExtraValueMapLookupMetadata(md.vmap, key)
    ref == C_NULL ? default : Metadata(ref)
end
Base.get(md::ValueMapMetadata, key, default) = default

function Base.getindex(md::ValueMapMetadata, key::Metadata)
    val = get(md, key, nothing)
    val === nothing && throw(KeyError(key))
    return val
end

Base.haskey(md::ValueMapMetadata, key) = get(md, key, nothing) !== nothing

function Base.setindex!(md::ValueMapMetadata, val::Metadata, key::Metadata)
    API.LLVMExtraValueMapSetMetadata(md.vmap, key, val)
    return md
end

function Base.delete!(md::ValueMapMetadata, key::Metadata)
    API.LLVMExtraValueMapEraseMetadata(md.vmap, key)
    return md
end

function Base.empty!(md::ValueMapMetadata)
    for key in collect(keys(md))
        delete!(md, key)
    end
    return md
end

Base.length(md::ValueMapMetadata) = Int(API.LLVMExtraValueMapGetNumMetadataEntries(md.vmap))

function Base.iterate(md::ValueMapMetadata)
    n = length(md)
    keys = Vector{API.LLVMMetadataRef}(undef, n)
    vals = Vector{API.LLVMMetadataRef}(undef, n)
    n > 0 && API.LLVMExtraValueMapGetMetadataEntries(md.vmap, keys, vals)
    entries = Pair{Metadata,Metadata}[Metadata(k) => Metadata(v) for (k, v) in zip(keys, vals)]
    iterate(md, (entries, 1))
end
function Base.iterate(::ValueMapMetadata, (entries, i))
    i > length(entries) ? nothing : (entries[i], (entries, i+1))
end


## value mapping

mutable struct MapperCallbackState
    type_mapper
    materializer
    vmap::ValueMap
    flags::UInt32
    exception::Union{Nothing,Tuple{Any,Vector}}
    mapped_types::Dict{API.LLVMTypeRef,API.LLVMTypeRef}

    MapperCallbackState(type_mapper, materializer, vmap, flags) =
        new(type_mapper, materializer, vmap, flags, nothing,
            Dict{API.LLVMTypeRef,API.LLVMTypeRef}())
end

# the type that `typ` maps to, and whether the type mapper succeeded (if not, the type is
# not mapped)
function mapped_type(state::MapperCallbackState, typ::API.LLVMTypeRef)
    state.type_mapper === nothing && return typ, true
    new = get(state.mapped_types, typ, C_NULL)
    new == C_NULL || return new, true
    try
        new = Base.unsafe_convert(API.LLVMTypeRef, state.type_mapper(LLVMType(typ)))
        state.mapped_types[typ] = new
        return new, true
    catch err
        _capture_callback_exception!(state, err)
        return typ, false
    end
end

function type_mapper_callback(typ, state::MapperCallbackState)
    return first(mapped_type(state, typ))
end

# LLVM cannot be stopped once it maps a value, so after the materializer failed, mapping
# goes on with values of the types that the constants being rebuilt expect: constants whose
# type maps to another one are mapped to poison of that type, instead of being rebuilt from
# operands that may or may not have been mapped. That includes global values that aren't in
# the map, which LLVM would map to themselves. Other values are mapped by LLVM as usual.
function failed_mapping(state::MapperCallbackState, val::API.LLVMValueRef)
    null = Base.unsafe_convert(API.LLVMValueRef, C_NULL)
    API.LLVMIsAConstant(val) == C_NULL && return null
    typ = API.LLVMTypeOf(val)
    new_typ, ok = mapped_type(state, typ)
    (!ok || new_typ == typ) && return val
    API.LLVMGetTypeKind(new_typ) in (API.LLVMVoidTypeKind, API.LLVMLabelTypeKind,
                                     API.LLVMMetadataTypeKind, API.LLVMTokenTypeKind,
                                     API.LLVMFunctionTypeKind) && return null
    return API.LLVMGetPoison(new_typ)
end

function materializer_callback(val, state::MapperCallbackState)
    null = Base.unsafe_convert(API.LLVMValueRef, C_NULL)
    gv, bb = Ref{API.LLVMValueRef}(), Ref{API.LLVMValueRef}()
    kind = API.LLVMExtraGetGlobalReference(val, gv, bb)
    kind == API.LLVMExtraNoGlobalReference ||
        return materialize_global_reference(state, val, kind, gv[])
    state.exception === nothing || return failed_mapping(state, val)
    state.materializer === nothing && return null
    try
        new_val = state.materializer(Value(val))
        if new_val === nothing
            return null
        else
            return Base.unsafe_convert(API.LLVMValueRef, new_val)
        end
    catch err
        _capture_callback_exception!(state, err)
        return failed_mapping(state, val)
    end
end

# LLVM requires the global value that a `blockaddress`, `dso_local_equivalent` or `no_cfi`
# refers to, to map to a global value (a function for the former two), which `poison` from
# `failed_mapping` isn't. So such constants are handled here, mapping the global value with
# a nested mapping first, and mapping the whole constant to `poison` (or itself, if its type
# doesn't change) when that fails, or failed before.
function materialize_global_reference(state::MapperCallbackState, val::API.LLVMValueRef,
                                      kind, gv::API.LLVMValueRef)
    null = Base.unsafe_convert(API.LLVMValueRef, C_NULL)
    function failed()
        typ = API.LLVMTypeOf(val)
        new_typ, _ = mapped_type(state, typ)
        return new_typ == typ ? val : API.LLVMGetPoison(new_typ)
    end
    state.exception === nothing || return failed()
    try
        if state.materializer !== nothing
            new_val = state.materializer(Value(val))
            new_val === nothing || return Base.unsafe_convert(API.LLVMValueRef, new_val)
        end
        new_gv = map_value(Value(gv), state.vmap, state.flags, state.type_mapper,
                           state.materializer)
        ok = if kind == API.LLVMExtraNoCFIValueReference
            new_gv isa GlobalValue
        elseif kind == API.LLVMExtraBlockAddressReference
            new_gv isa Function
        else
            new_gv isa GlobalValue || (new_gv !== nothing &&
                Value(API.LLVMExtraStripPointerCastsAndAliases(new_gv)) isa Function)
        end
        ok || throw(ArgumentError("$(Value(gv)) must map to a $(kind == API.LLVMExtraNoCFIValueReference ? "global value" : "function") as it is used by $(Value(val)), got $(something(new_gv, "nothing"))"))
        # LLVM finds the mapping of the global value in the map
        return null
    catch err
        _capture_callback_exception!(state, err)
        return failed()
    end
end

# Before LLVM maps anything, walk what it will map, like the value mapper (ValueMapper.cpp)
# and CloneFunctionInto do, to call the type mapper on the types that they will ask for (so
# that a type mapper that fails does so before anything is mapped), and to remove the
# entries for deleted values that they would look up (the value mapper expects every entry
# to have a value). The walk stops where LLVM stops: at values and metadata in the map, and
# at global values, which LLVM maps to themselves (their types are only needed when the
# materializer failed, see `failed_mapping`).
struct MapperPrepass
    state::MapperCallbackState
    vmap::ValueMap
    values::Set{API.LLVMValueRef}
    metadata::Set{API.LLVMMetadataRef}
    # compile units that LLVM doesn't map (when cloning within a module)
    keep_compile_units::Bool

    MapperPrepass(state, vmap; keep_compile_units=false) =
        new(state, vmap, Set{API.LLVMValueRef}(), Set{API.LLVMMetadataRef}(),
            keep_compile_units)
end

prepare_type!(p::MapperPrepass, typ::API.LLVMTypeRef) = (mapped_type(p.state, typ); nothing)

# Mapper::mapValue
function prepare_value!(p::MapperPrepass, val::API.LLVMValueRef)
    val == C_NULL && return
    val in p.values && return
    push!(p.values, val)
    API.LLVMExtraValueMapLookup(p.vmap, val) == C_NULL || return
    # the entry of a value that was deleted, if any
    API.LLVMExtraValueMapErase(p.vmap, val)

    if API.LLVMGetValueKind(val) == API.LLVMMetadataAsValueValueKind
        prepare_metadata!(p, API.LLVMValueAsMetadata(val))
        return
    end
    if API.LLVMIsAInlineAsm(val) != C_NULL
        prepare_type!(p, API.LLVMExtraGetInlineAsmFunctionType(val))
        return
    end
    # other values that aren't constants are not rebuilt
    API.LLVMIsAConstant(val) == C_NULL && return
    prepare_type!(p, API.LLVMTypeOf(val))
    API.LLVMIsAGlobalValue(val) == C_NULL || return
    # the operands of a `blockaddress` aren't the same in every version of LLVM
    gv, bb = Ref{API.LLVMValueRef}(), Ref{API.LLVMValueRef}()
    if API.LLVMExtraGetGlobalReference(val, gv, bb) != API.LLVMExtraNoGlobalReference
        prepare_value!(p, gv[])
        prepare_value!(p, bb[])
        return
    end
    if API.LLVMIsAConstantExpr(val) != C_NULL &&
       API.LLVMGetConstOpcode(val) == API.LLVMGetElementPtr
        prepare_type!(p, API.LLVMGetGEPSourceElementType(val))
    end
    for i in 0:API.LLVMGetNumOperands(val)-1
        prepare_value!(p, API.LLVMGetOperand(val, i))
    end
end

# Mapper::mapMetadata, and the DIArgList case of Mapper::mapValue
function prepare_metadata!(p::MapperPrepass, md::API.LLVMMetadataRef)
    md == C_NULL && return
    md in p.metadata && return
    push!(p.metadata, md)
    API.LLVMExtraValueMapLookupMetadata(p.vmap, md) == C_NULL || return
    kind = API.LLVMGetMetadataKind(md)
    if kind == API.LLVMConstantAsMetadataMetadataKind ||
       kind == API.LLVMLocalAsMetadataMetadataKind
        prepare_value!(p, API.LLVMMetadataAsValue2(C_NULL, md))
    elseif kind == API.LLVMDIArgListMetadataKind
        for i in 0:API.LLVMExtraDIArgListGetNumArgs(md)-1
            prepare_metadata!(p, API.LLVMExtraDIArgListGetArg(md, i))
        end
    elseif kind == API.LLVMMDStringMetadataKind ||
           kind == API.LLVMDistinctMDOperandPlaceholderMetadataKind ||
           (kind == API.LLVMDICompileUnitMetadataKind && p.keep_compile_units)
    else
        for i in 0:API.LLVMGetMDNodeNumOperands2(md)-1
            prepare_metadata!(p, API.LLVMGetMDNodeOperand2(md, i))
        end
    end
end

function prepare_metadata_entries!(p::MapperPrepass, entries, n)
    try
        for i in 1:n
            prepare_metadata!(p, API.LLVMValueMetadataEntriesGetMetadata(entries, i-1))
        end
    finally
        API.LLVMDisposeValueMetadataEntries(entries)
    end
end

# CloneFunctionInto: the personality, prefix and prologue data and the metadata of the
# function, and Mapper::remapInstruction and Mapper::remapDbgRecord for its body
function prepare_function!(p::MapperPrepass, f::Function)
    ref = f.ref
    for i in 0:API.LLVMGetNumOperands(ref)-1
        prepare_value!(p, API.LLVMGetOperand(ref, i))
    end
    n = Ref{Csize_t}()
    prepare_metadata_entries!(p, API.LLVMGlobalCopyAllMetadata(ref, n), n[])
    for arg in parameters(f)
        prepare_value!(p, arg.ref)
    end
    for bb in blocks(f), inst in instructions(bb)
        prepare_instruction!(p, inst.ref)
    end
end

function prepare_instruction!(p::MapperPrepass, inst::API.LLVMValueRef)
    prepare_type!(p, API.LLVMTypeOf(inst))
    if API.LLVMIsAAllocaInst(inst) != C_NULL
        prepare_type!(p, API.LLVMGetAllocatedType(inst))
    elseif API.LLVMIsAGetElementPtrInst(inst) != C_NULL
        prepare_type!(p, API.LLVMGetGEPSourceElementType(inst))
        prepare_type!(p, API.LLVMExtraGetGEPResultElementType(inst))
    elseif API.LLVMIsACallInst(inst) != C_NULL || API.LLVMIsAInvokeInst(inst) != C_NULL ||
           API.LLVMIsACallBrInst(inst) != C_NULL
        # LLVM rebuilds the function type of a call from its mapped parameter types and the
        # mapped type of the call, while inline assembly is mapped with its function type
        fty = API.LLVMGetCalledFunctionType(inst)
        params = Vector{API.LLVMTypeRef}(undef, API.LLVMCountParamTypes(fty))
        isempty(params) || API.LLVMGetParamTypes(fty, params)
        foreach(typ -> prepare_type!(p, typ), params)
        API.LLVMIsAInlineAsm(API.LLVMGetCalledValue(inst)) == C_NULL ||
            prepare_type!(p, fty)
        # the types of attributes like `byval` and `elementtype`
        for idx in 0:API.LLVMGetNumArgOperands(inst)
            n = API.LLVMGetCallSiteAttributeCount(inst, idx)
            n == 0 && continue
            attrs = Vector{API.LLVMAttributeRef}(undef, n)
            API.LLVMGetCallSiteAttributes(inst, idx, attrs)
            for attr in attrs
                Bool(API.LLVMIsTypeAttribute(attr)) &&
                    prepare_type!(p, API.LLVMGetTypeAttributeValue(attr))
            end
        end
    end
    for i in 0:API.LLVMGetNumOperands(inst)-1
        prepare_value!(p, API.LLVMGetOperand(inst, i))
    end
    if API.LLVMIsAPHINode(inst) != C_NULL
        for i in 0:API.LLVMCountIncoming(inst)-1
            prepare_value!(p, API.LLVMBasicBlockAsValue(API.LLVMGetIncomingBlock(inst, i)))
        end
    end
    prepare_metadata!(p, API.LLVMInstructionGetDebugLoc(inst))
    n = Ref{Csize_t}()
    prepare_metadata_entries!(p,
        API.LLVMInstructionGetAllMetadataOtherThanDebugLoc(inst, n), n[])

    num_values, num_mds = Ref{Cuint}(), Ref{Cuint}()
    API.LLVMExtraGetDbgRecordOperands(inst, C_NULL, num_values, C_NULL, num_mds)
    if num_values[] > 0 || num_mds[] > 0
        values = Vector{API.LLVMValueRef}(undef, num_values[])
        mds = Vector{API.LLVMMetadataRef}(undef, num_mds[])
        API.LLVMExtraGetDbgRecordOperands(inst, values, num_values, mds, num_mds)
        foreach(val -> prepare_value!(p, val), values)
        foreach(md -> prepare_metadata!(p, md), mds)
    end
end

# call `prepare!` with a prepass, and `f(type_mapper_ptr, type_mapper_data,
# materializer_ptr, materializer_data)`, which passes the callbacks to LLVM, and rethrow
# what they threw
function with_mapper_callbacks(f, prepare!, what, vmap, flags, type_mapper, materializer;
                               keep_compile_units=false)
    callback_state = Ref(MapperCallbackState(type_mapper, materializer, vmap, flags))
    prepare!(MapperPrepass(callback_state[], vmap; keep_compile_units))
    if callback_state[].exception !== nothing
        err, bt = callback_state[].exception
        throw(CallbackException(what, err, bt))
    end

    if type_mapper === nothing
        type_mapper_ptr = C_NULL
        type_mapper_data = C_NULL
    else
        type_mapper_ptr = @cfunction(type_mapper_callback, API.LLVMTypeRef,
                                     (API.LLVMTypeRef, Any))
        type_mapper_data = callback_state
    end
    # the materializer is also needed to handle failures of the type mapper
    if type_mapper === nothing && materializer === nothing
        materializer_ptr = C_NULL
        materializer_data = C_NULL
    else
        materializer_ptr = @cfunction(materializer_callback, API.LLVMValueRef,
                                      (API.LLVMValueRef, Any))
        materializer_data = callback_state
    end
    ret = GC.@preserve callback_state begin
        f(type_mapper_ptr, type_mapper_data, materializer_ptr, materializer_data)
    end
    if callback_state[].exception !== nothing
        err, bt = callback_state[].exception
        throw(CallbackException(what, err, bt))
    end
    return ret
end

function remap_flags(no_module_level_changes, ignore_missing_locals,
                     reuse_and_mutate_distinct_mds, null_map_missing_globals)
    flags = UInt32(0)
    no_module_level_changes && (flags |= UInt32(API.LLVMRemapFlagsNoModuleLevelChanges))
    ignore_missing_locals && (flags |= UInt32(API.LLVMRemapFlagsIgnoreMissingLocals))
    reuse_and_mutate_distinct_mds &&
        (flags |= UInt32(API.LLVMRemapFlagsReuseAndMutateDistinctMDs))
    null_map_missing_globals &&
        (flags |= UInt32(API.LLVMRemapFlagsNullMapMissingGlobalValues))
    return flags
end

"""
    map_value(val::Value, vmap::ValueMap; [type_mapper::Function],
              [materializer::Function], [no_module_level_changes::Bool],
              [ignore_missing_locals::Bool], [reuse_and_mutate_distinct_mds::Bool],
              [null_map_missing_globals::Bool])

Map a value using the value map `vmap`, like LLVM's `MapValue`, and return the result, or
`nothing` if the value cannot be mapped. Values that are in the map are mapped to their
entry. Other constants are rebuilt with their operands mapped (and their type mapped by the
type mapper), and metadata is mapped like [`map_metadata`](@ref) does. The mappings that
are made are added to `vmap`. Global values that aren't in the map are mapped to
themselves, while other values that aren't in the map can't be mapped.

The `type_mapper` callback maps the types of the values that are rebuilt: it is called with
a type, and returns the type to use instead. The `materializer` callback is called with
every value that isn't in the map yet, and returns the value to map it to, or `nothing` to
have it mapped as usual. A materializer can call `map_value` itself, e.g., to map the
operands of a value that it rebuilds, which uses and extends the same map. The type
mapper is not called for the types that a type contains, so it should map those itself
(e.g., the element type of an array type).

The mappings must leave the constant expressions that LLVM rebuilds valid: LLVM keeps
their opcode. For example, when the type mapper or the map makes both sides of an
`addrspacecast` the same address space, the materializer should replace the cast with its
mapped operand, or LLVM creates an invalid cast (which a build of LLVM with assertions
aborts on).

As LLVM cannot be stopped once it maps a value, the type mapper is called before LLVM maps
anything, with the types of the constants (including global values) that are reachable
from `val` through the operands of constants and metadata, stopping at entries of the map.
Its results are cached, and if it throws, nothing is mapped. This may call it for types
that LLVM doesn't end up asking for, e.g., below a value that the materializer replaces.
When the materializer throws, LLVM goes on mapping without the callbacks: that value and
the remaining constants whose type maps to another one are mapped to `poison` of that type,
so that the constants that are rebuilt are well-formed, but the map and anything mapped
with it should be discarded. (A `blockaddress`, `dso_local_equivalent` or `no_cfi`, whose
global value LLVM requires to map to a global value, is mapped to `poison` as a whole
instead.) Exceptions thrown by either callback are rethrown as a
[`CallbackException`](@ref) after LLVM returns to Julia.

The flags correspond to LLVM's `RemapFlags`:

- `no_module_level_changes`: assume that global values and metadata don't change, so
  that only function-local values and metadata need to be mapped;
- `ignore_missing_locals`: keep references to function-local values that aren't in the
  map (e.g., in metadata) instead of failing to map them;
- `reuse_and_mutate_distinct_mds`: map distinct metadata to itself (updating its
  operands) instead of duplicating it;
- `null_map_missing_globals`: don't map global values that aren't in the map to
  themselves.
"""
function map_value(val::Value, vmap::ValueMap; type_mapper=nothing, materializer=nothing,
                   no_module_level_changes::Bool=false, ignore_missing_locals::Bool=false,
                   reuse_and_mutate_distinct_mds::Bool=false,
                   null_map_missing_globals::Bool=false)
    flags = remap_flags(no_module_level_changes, ignore_missing_locals,
                        reuse_and_mutate_distinct_mds, null_map_missing_globals)
    map_value(val, vmap, flags, type_mapper, materializer)
end

function map_value(val::Value, vmap::ValueMap, flags::UInt32, type_mapper, materializer)
    prepare!(p) = prepare_value!(p, val.ref)
    ref = with_mapper_callbacks(prepare!, "value mapping", vmap, flags, type_mapper,
                                materializer) do tm, tmd, mat, matd
        API.LLVMExtraMapValue(val, vmap, flags, tm, tmd, mat, matd)
    end
    ref == C_NULL ? nothing : Value(ref)
end

"""
    map_metadata(md::Metadata, vmap::ValueMap; [type_mapper::Function],
                 [materializer::Function], [flags...])

Map metadata using the value map `vmap`, like LLVM's `MapMetadata`, and return the result,
or `nothing` if it cannot be mapped. Uniqued nodes are rebuilt if any of their operands
change, while distinct nodes are duplicated (unless `reuse_and_mutate_distinct_mds` is
set), so to keep a distinct node, like a compile unit, map it to itself first:
`vmap.metadata[cu] = cu`. The values that metadata refers to are mapped like
[`map_value`](@ref) does, and the keyword arguments are the same as those of `map_value`.
"""
function map_metadata(md::Metadata, vmap::ValueMap; type_mapper=nothing,
                      materializer=nothing, no_module_level_changes::Bool=false,
                      ignore_missing_locals::Bool=false,
                      reuse_and_mutate_distinct_mds::Bool=false,
                      null_map_missing_globals::Bool=false)
    flags = remap_flags(no_module_level_changes, ignore_missing_locals,
                        reuse_and_mutate_distinct_mds, null_map_missing_globals)
    prepare!(p) = prepare_metadata!(p, md.ref)
    ref = with_mapper_callbacks(prepare!, "metadata mapping", vmap, flags, type_mapper,
                                materializer) do tm, tmd, mat, matd
        API.LLVMExtraMapMetadata(md, vmap, flags, tm, tmd, mat, matd)
    end
    ref == C_NULL ? nothing : Metadata(ref)
end


## cloning

@vocabulary IR clone_into!, clone

"""
    clone_into!(new::LLVM.Function, old::LLVM.Function; [suffix::AbstractString],
                [value_map::Union{ValueMap,AbstractDict{<:Value,<:Value}}],
                [changes::LLVM.CloneFunctionChangeType.T],
                [type_mapper::Function],
                [materializer::Function])

Clone the contents of a function `old` into a new function `new`. The `value_map` maps
values of the old function to values to use in the new function, and must map the
parameters of `old`, while `suffix` is appended to the names of all values cloned. The
`type_mapper` and `materializer` functions can be used to respectively map types and
materialize values on demand, as with [`map_value`](@ref).

If `value_map` is a [`ValueMap`](@ref), it is extended with the mappings that are made
(e.g., of the instructions of `old` to those of `new`), and the functions and global values
it maps are used when cloning, which makes it possible to rebuild several functions and
global values that refer to each other. Other dictionaries are not modified.

Exceptions thrown by either callback are captured and rethrown as a
[`CallbackException`](@ref) after LLVM returns to Julia. As with [`map_value`](@ref), the
type mapper is called before anything is cloned, with the types that LLVM maps: those of
the constants and metadata that `old` refers to (including its personality function and
prefix and prologue data), and those of its instructions, including the allocated types
of `alloca`s, the source and result element types of GEPs, the parameter types of calls,
the function types of inline assembly, and the types of call attributes like `byval`. If
it throws, nothing is cloned. A materializer that throws leaves `new` and the map with
constants that are mapped to `poison`, so they should be discarded.

The `changes` argument determines how this function behaves; refer to the LLVM documentation
of `CloneFunctionInto` for more details.
"""
function clone_into!(new::Function, old::Function;
                     value_map::Union{ValueMap,AbstractDict{<:Value,<:Value}}=Dict{Value,Value}(),
                     changes=API.LLVMCloneFunctionChangeTypeLocalChangesOnly,
                     suffix::AbstractString="", type_mapper=nothing, materializer=nothing)
    if value_map isa ValueMap
        clone_into!(new, old, value_map, changes, suffix, type_mapper, materializer)
    else
        ValueMap() do vmap
            for (src, dest) in value_map
                vmap[src] = dest
            end
            clone_into!(new, old, vmap, changes, suffix, type_mapper, materializer)
        end
    end
    return nothing
end

function clone_into!(new::Function, old::Function, vmap::ValueMap, changes, suffix,
                     type_mapper, materializer)
    prepare!(p) = prepare_function!(p, old)
    # when cloning within a module, LLVM doesn't map compile units
    keep_compile_units = changes in (API.LLVMCloneFunctionChangeTypeLocalChangesOnly,
                                     API.LLVMCloneFunctionChangeTypeGlobalChanges)
    # the flags that CloneFunctionInto maps with (for nested mappings, see
    # `materialize_global_reference`)
    flags = UInt32(0)
    with_mapper_callbacks(prepare!, "function cloning", vmap, flags, type_mapper,
                          materializer; keep_compile_units) do tm, tmd, mat, matd
        API.LLVMExtraCloneFunctionInto(new, old, vmap, changes, suffix, tm, tmd, mat, matd)
    end
end

"""
    clone(f::Function; [value_map::AbstractDict{Value,Value}])

Simpler version of [`clone_into!`](@ref) that clones a function `f` into a new function,
optionally mapping values according to the `value_map` dictionary.
"""
function clone(f::Function; value_map::AbstractDict{<:Value,<:Value}=Dict{Value,Value}(), kwargs...)
    argtypes = LLVMType[]

    # The user might be deleting arguments to the function by specifying them in
    # the VMap. If so, we need to not add the arguments to the arg ty vector
    remaining_parameters = Value[]
    for arg in parameters(f)
        if !in(arg, keys(value_map))    # Haven't mapped the argument to anything yet?
            push!(argtypes, value_type(arg))
            push!(remaining_parameters, arg)
        end
    end

    # Create a new function type...
    oldfty = function_type(f)
    vararg = isvararg(oldfty)
    fty = FunctionType(return_type(oldfty), argtypes; vararg)

    # Create the new function...
    new_f = Function(parent(f), name(f), fty)
    linkage!(new_f, linkage(f))
    # TODO: address space

    # Loop over the arguments, copying the names of the mapped arguments over...
    value_map = Dict{Value,Value}(value_map)
    for (arg, new_arg) in zip(remaining_parameters, parameters(new_f))
        if !in(arg, keys(value_map))    # Is this argument preserved?
            name!(new_arg, name(arg))   # Copy the name over...
            value_map[arg] = new_arg    # Add mapping to VMap
        end
    end

    clone_into!(new_f, f; value_map, kwargs...)

   return new_f
end

"""
    clone(bb::BasicBlock]; dest=parent(bb), [suffix::AbstractString],
          [value_map::AbstractDict{Value,Value}])

Clone a basic block `bb` by copying all instructions. The new block is inserted at the end
of the parent function; this can be altered by setting `dest` to a different function, or to
`nothing` to create a detached block. The `suffix` is appended to the name of the cloned
basic block.

!!! warn

    This function only remaps values that are defined in the cloned basic block. Values
    defined outside the basic block (e.g. function arguments) are not remapped by default.
    This means that the cloned basic block can generally only be used within the same
    function that it was cloned from, unless you manually remap other values.
    This can be done passing a `value_map` dictionary.
"""
function clone(bb::BasicBlock; dest::Union{Nothing,Function}=parent(bb),
               suffix::AbstractString="",
               value_map::AbstractDict{<:Value,<:Value}=Dict{Value,Value}())
    value_map_array = API.LLVMValueRef[]
    for (src, dest) in value_map
        push!(value_map_array, Base.unsafe_convert(API.LLVMValueRef, src))
        push!(value_map_array, Base.unsafe_convert(API.LLVMValueRef, dest))
    end
    BasicBlock(API.LLVMCloneBasicBlock(bb, suffix, value_map_array, length(value_map),
                                       something(dest, C_NULL)))
end
