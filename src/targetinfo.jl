# Custom TargetTransformInfo
#
# Abstract type + query function declarations, plus the @cfunction trampolines
# that let passes reach those methods through the C API. Subtypes override
# only the queries they care about; anything they don't override is not wired
# up on the C side and LLVM's `TargetTransformInfoImplBase` handles it.

export AbstractTargetTransformInfo

# The query methods below are meant to be overridden on subtypes.
@public flat_address_space, has_branch_divergence, is_single_threaded,
        is_noop_addr_space_cast, is_valid_addr_space_cast, addrspaces_may_alias,
        can_have_non_undef_global_initializer_in_address_space,
        is_source_of_divergence, is_always_uniform,
        get_assumed_addr_space, get_predicated_addr_space,
        rewrite_intrinsic_with_address_space, collect_flat_address_operands

"""
    abstract type AbstractTargetTransformInfo

Subtype this to supply a custom `TargetTransformInfo` to pass pipelines that
would otherwise see only LLVM's conservative baseline.

Most LLVM back-ends (host CPU, NVPTX, AMDGPU) carry their own TTI through a
`TargetMachine`; callers that already have one and pass it to
[`run!`](@ref) don't need this. The feature is aimed at **out-of-tree
back-ends not linked into libLLVM** (e.g. Metal) and at tooling that runs
passes without a `TargetMachine` at all. In those pipelines, the baseline TTI
reports no flat address space, no branch divergence, etc., and TTI-sensitive
passes such as `InferAddressSpacesPass` or `UniformityAnalysis` silently
no-op.

Subtypes override only the queries they care about; any query left
un-overridden is handled by LLVM's `TargetTransformInfoImplBase`.

Attach an instance with [`target_transform_info!`](@ref); attaching `nothing`
reverts to LLVM's native TTI. When a `TargetMachine` is also supplied to
[`run!`](@ref), a custom TTI takes precedence.

Overridable queries:

- Target-level knobs: [`flat_address_space`](@ref),
  [`has_branch_divergence`](@ref), [`is_single_threaded`](@ref).
- Address-space predicates: [`is_noop_addr_space_cast`](@ref),
  [`is_valid_addr_space_cast`](@ref), [`addrspaces_may_alias`](@ref),
  [`can_have_non_undef_global_initializer_in_address_space`](@ref).
- Per-`Value` queries: [`is_source_of_divergence`](@ref),
  [`is_always_uniform`](@ref), [`get_assumed_addr_space`](@ref),
  [`get_predicated_addr_space`](@ref).
- Intrinsic hooks: [`rewrite_intrinsic_with_address_space`](@ref),
  [`collect_flat_address_operands`](@ref).
"""
abstract type AbstractTargetTransformInfo end

"""
    flat_address_space(tti::AbstractTargetTransformInfo) -> Unsigned

Address space the target treats as "flat" / generic. Required — alongside
[`is_noop_addr_space_cast`](@ref) — for `InferAddressSpacesPass` to fold
`addrspacecast`s. If not defined, LLVM reports no flat AS.
"""
function flat_address_space end

"""
    has_branch_divergence(tti::AbstractTargetTransformInfo) -> Bool

Whether the target can produce divergent control flow. Enables
divergence-aware passes (`SimplifyCFG`, loop analyses) when true. If not
defined, falls back to LLVM's baseline.
"""
function has_branch_divergence end

"""
    is_single_threaded(tti::AbstractTargetTransformInfo) -> Bool

Whether the target is single-threaded. A few passes skip concurrency-related
transformations when true. If not defined, falls back to LLVM's baseline
(which consults the module's `"single-thread"` flag).
"""
function is_single_threaded end

"""
    is_noop_addr_space_cast(tti::AbstractTargetTransformInfo,
                            from::Unsigned, to::Unsigned) -> Bool

Whether an `addrspacecast` from `from` to `to` is a noop at runtime. If not
defined, falls back to LLVM's baseline.
"""
function is_noop_addr_space_cast end

"""
    is_valid_addr_space_cast(tti::AbstractTargetTransformInfo,
                             from::Unsigned, to::Unsigned) -> Bool

Whether an `addrspacecast` from `from` to `to` is permitted. If not defined,
falls back to LLVM's baseline.
"""
function is_valid_addr_space_cast end

"""
    addrspaces_may_alias(tti::AbstractTargetTransformInfo,
                         as0::Unsigned, as1::Unsigned) -> Bool

Whether pointers in address spaces `as0` and `as1` may alias. If not defined,
falls back to LLVM's (conservative) baseline.
"""
function addrspaces_may_alias end

"""
    can_have_non_undef_global_initializer_in_address_space(
        tti::AbstractTargetTransformInfo, as::Unsigned) -> Bool

Whether globals in address space `as` may have non-`undef` initializers. If
not defined, falls back to LLVM's baseline (which consults
`DataLayout::isNonIntegralAddressSpace`).
"""
function can_have_non_undef_global_initializer_in_address_space end

"""
    is_source_of_divergence(tti::AbstractTargetTransformInfo, v::Value) -> Bool

Whether `v` is a source of divergence. Consulted by `UniformityAnalysis`. If
not defined, falls back to LLVM's baseline.
"""
function is_source_of_divergence end

"""
    is_always_uniform(tti::AbstractTargetTransformInfo, v::Value) -> Bool

Whether `v` is known to hold the same value across all threads. Consulted by
`UniformityAnalysis`. If not defined, falls back to LLVM's baseline.
"""
function is_always_uniform end

"""
    get_assumed_addr_space(tti::AbstractTargetTransformInfo, v::Value) -> Unsigned

Address space statically known to hold for `v`, or `typemax(UInt)` for "no
assumption". If not defined, falls back to LLVM's baseline.
"""
function get_assumed_addr_space end

"""
    get_predicated_addr_space(tti::AbstractTargetTransformInfo, v::Value)
        -> (predicate::Union{Value,Nothing}, as::Unsigned)

Address space that holds for `v` when `predicate` is true. Return
`(nothing, typemax(UInt))` for "no inference". If not defined, falls back to
LLVM's baseline.
"""
function get_predicated_addr_space end

"""
    rewrite_intrinsic_with_address_space(tti::AbstractTargetTransformInfo,
        ii::Value, old::Value, new::Value) -> Union{Value,Nothing}

Rewrite an intrinsic call `ii` after its pointer operand `old` has been
replaced by `new` (in a different address space). Return the rewritten value,
or `nothing` to keep the existing call. If not defined, falls back to LLVM's
baseline.
"""
function rewrite_intrinsic_with_address_space end

"""
    collect_flat_address_operands(tti::AbstractTargetTransformInfo, iid::Unsigned)
        -> Vector{Int}

Operand indices of intrinsic `iid` that are flat-address-space pointer
operands. Capped at 32 entries by the underlying C API. If not defined, falls
back to LLVM's baseline.
"""
function collect_flat_address_operands end

# Mutable shim that holds the `AbstractTargetTransformInfo` alongside any
# caught exception. A pointer to this object is passed to C as the `UserData`
# for every callback, and the trampolines unwrap it to dispatch.
mutable struct CustomTTIState
    tti::AbstractTargetTransformInfo
    exception::Union{Nothing,Tuple{Any,Vector}}
    CustomTTIState(tti) = new(tti, nothing)
end

# Each trampoline is a top-level (non-closure) function so it can be
# `@cfunction`'d. They swallow exceptions and record them on the state —
# LLVM must not see a Julia exception unwind into C.

function capture_exception!(state::CustomTTIState, err)
    # Keep the first caught exception; ignore subsequent ones from the same run.
    if state.exception === nothing
        state.exception = (err, Base.catch_backtrace())
    end
end

function custom_tti_is_noop_addr_space_cast_callback(from::Cuint, to::Cuint,
                                                     ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_noop_addr_space_cast(state.tti, UInt(from), UInt(to))::Bool
    catch err
        capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_valid_addr_space_cast_callback(from::Cuint, to::Cuint,
                                                      ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_valid_addr_space_cast(state.tti, UInt(from), UInt(to))::Bool
    catch err
        capture_exception!(state, err)
        return false
    end
end

function custom_tti_addrspaces_may_alias_callback(as0::Cuint, as1::Cuint,
                                                  ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return addrspaces_may_alias(state.tti, UInt(as0), UInt(as1))::Bool
    catch err
        # Conservative default on exception: may alias. Matches LLVM's BaseT
        # and avoids the risk of incorrect optimization on partially-transformed
        # IR before the captured exception is re-raised.
        capture_exception!(state, err)
        return true
    end
end

function custom_tti_can_have_global_initializer_in_as_callback(as::Cuint,
                                                               ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return can_have_non_undef_global_initializer_in_address_space(
                   state.tti, UInt(as))::Bool
    catch err
        capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_source_of_divergence_callback(ref::API.LLVMValueRef,
                                                     ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_source_of_divergence(state.tti, Value(ref))::Bool
    catch err
        capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_always_uniform_callback(ref::API.LLVMValueRef,
                                               ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_always_uniform(state.tti, Value(ref))::Bool
    catch err
        capture_exception!(state, err)
        return false
    end
end

function custom_tti_get_assumed_address_space_callback(ref::API.LLVMValueRef,
                                                       ud::Ptr{Cvoid})::Cuint
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        # `% Cuint` truncates modulo 2^32 rather than throwing on
        # `typemax(UInt)` — users naturally reach for that as the "no
        # assumption" sentinel, and we want both 32- and 64-bit spellings
        # of ~0 to work.
        return get_assumed_addr_space(state.tti, Value(ref))::Integer % Cuint
    catch err
        capture_exception!(state, err)
        return typemax(Cuint)
    end
end

function custom_tti_get_predicated_address_space_callback(
        ref::API.LLVMValueRef,
        out_predicate::Ptr{API.LLVMValueRef},
        ud::Ptr{Cvoid})::Cuint
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        (pred, as) = get_predicated_addr_space(state.tti, Value(ref))
        pred_ref = pred === nothing ? API.LLVMValueRef(C_NULL) :
                                      Base.unsafe_convert(API.LLVMValueRef, pred::Value)
        unsafe_store!(out_predicate, pred_ref)
        return as::Integer % Cuint
    catch err
        capture_exception!(state, err)
        unsafe_store!(out_predicate, API.LLVMValueRef(C_NULL))
        return typemax(Cuint)
    end
end

function custom_tti_rewrite_intrinsic_with_as_callback(
        ii::API.LLVMValueRef, old::API.LLVMValueRef, new::API.LLVMValueRef,
        ud::Ptr{Cvoid})::API.LLVMValueRef
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        result = rewrite_intrinsic_with_address_space(state.tti,
                     Value(ii), Value(old), Value(new))
        result === nothing && return API.LLVMValueRef(C_NULL)
        return Base.unsafe_convert(API.LLVMValueRef, result::Value)
    catch err
        capture_exception!(state, err)
        return API.LLVMValueRef(C_NULL)
    end
end

function custom_tti_collect_flat_address_operands_callback(
        iid::Cuint, out_ops::Ptr{Cint},
        max_count::Cuint, out_count::Ptr{Cuint},
        ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        ops = collect_flat_address_operands(state.tti, UInt(iid))::AbstractVector
        n = min(length(ops), Int(max_count))
        for i in 1:n
            unsafe_store!(out_ops, Cint(ops[i]), i)
        end
        unsafe_store!(out_count, Cuint(n))
        return n > 0
    catch err
        capture_exception!(state, err)
        unsafe_store!(out_count, Cuint(0))
        return false
    end
end

# Allocate an `LLVMTTIOptionsRef` populated from the TTI object, along with
# the `CustomTTIState` that backs its `UserData` pointer. The caller owns both:
# `state` must stay GC-rooted for the duration of the pipeline run, and `opts`
# must be freed with `API.LLVMDisposeTTIOptions` afterwards.
function build_custom_tti_options(tti::AbstractTargetTransformInfo)
    state = CustomTTIState(tti)
    ud = Ref(state)
    opts = API.LLVMCreateTTIOptions()

    T = typeof(tti)

    # Scalar fields: only set when the subtype has an override. Unset fields
    # leave LLVM's `TargetTransformInfoImplBase` to answer the query.
    if hasmethod(flat_address_space, Tuple{T})
        API.LLVMTTIOptionsSetFlatAddressSpace(opts, flat_address_space(tti) % Cuint)
    end
    if hasmethod(has_branch_divergence, Tuple{T})
        API.LLVMTTIOptionsSetHasBranchDivergence(opts, has_branch_divergence(tti))
    end
    if hasmethod(is_single_threaded, Tuple{T})
        API.LLVMTTIOptionsSetIsSingleThreaded(opts, is_single_threaded(tti))
    end

    # Callbacks: install the @cfunction trampoline only when a concrete
    # override exists for this subtype.
    if hasmethod(is_noop_addr_space_cast, Tuple{T, Unsigned, Unsigned})
        cb = @cfunction(custom_tti_is_noop_addr_space_cast_callback,
                        API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetIsNoopAddrSpaceCast(opts, cb, ud)
    end
    if hasmethod(is_valid_addr_space_cast, Tuple{T, Unsigned, Unsigned})
        cb = @cfunction(custom_tti_is_valid_addr_space_cast_callback,
                        API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetIsValidAddrSpaceCast(opts, cb, ud)
    end
    if hasmethod(addrspaces_may_alias, Tuple{T, Unsigned, Unsigned})
        cb = @cfunction(custom_tti_addrspaces_may_alias_callback,
                        API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetAddrSpacesMayAlias(opts, cb, ud)
    end
    if hasmethod(can_have_non_undef_global_initializer_in_address_space,
                 Tuple{T, Unsigned})
        cb = @cfunction(custom_tti_can_have_global_initializer_in_as_callback,
                        API.LLVMBool, (Cuint, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetCanHaveGlobalInitializerInAS(opts, cb, ud)
    end
    if hasmethod(is_source_of_divergence, Tuple{T, Value})
        cb = @cfunction(custom_tti_is_source_of_divergence_callback,
                        API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetIsSourceOfDivergence(opts, cb, ud)
    end
    if hasmethod(is_always_uniform, Tuple{T, Value})
        cb = @cfunction(custom_tti_is_always_uniform_callback,
                        API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetIsAlwaysUniform(opts, cb, ud)
    end
    if hasmethod(get_assumed_addr_space, Tuple{T, Value})
        cb = @cfunction(custom_tti_get_assumed_address_space_callback,
                        Cuint, (API.LLVMValueRef, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetGetAssumedAddressSpace(opts, cb, ud)
    end
    if hasmethod(get_predicated_addr_space, Tuple{T, Value})
        cb = @cfunction(custom_tti_get_predicated_address_space_callback,
                        Cuint, (API.LLVMValueRef, Ptr{API.LLVMValueRef}, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetGetPredicatedAddressSpace(opts, cb, ud)
    end
    if hasmethod(rewrite_intrinsic_with_address_space,
                 Tuple{T, Value, Value, Value})
        cb = @cfunction(custom_tti_rewrite_intrinsic_with_as_callback,
                        API.LLVMValueRef,
                        (API.LLVMValueRef, API.LLVMValueRef, API.LLVMValueRef, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetRewriteIntrinsicWithAS(opts, cb, ud)
    end
    if hasmethod(collect_flat_address_operands, Tuple{T, Unsigned})
        cb = @cfunction(custom_tti_collect_flat_address_operands_callback,
                        API.LLVMBool,
                        (Cuint, Ptr{Cint}, Cuint, Ptr{Cuint}, Ptr{Cvoid}))
        API.LLVMTTIOptionsSetCollectFlatAddressOperands(opts, cb, ud)
    end

    return state, opts
end
