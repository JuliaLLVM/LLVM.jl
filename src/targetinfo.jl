# Custom TargetTransformInfo
#
# Abstract type + default methods that mirror LLVM's `TargetTransformInfoImplBase`,
# plus the @cfunction trampolines that let passes reach those methods through
# the C API. Consumers (e.g. `newpm.jl`) plug these into whichever pipeline
# they wrap.

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

Each exposed query has a default method on `AbstractTargetTransformInfo` that
matches LLVM's `TargetTransformInfoImplBase`, so subtypes override only what
they need.

Attach an instance with [`target_transform_info!`](@ref); attaching `nothing`
reverts to LLVM's native TTI. When a `TargetMachine` is also supplied to
[`run!`](@ref), a custom TTI takes precedence — so this type is usable as a
one-off override on top of an existing target as well.

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

# Defaults matching LLVM's `TargetTransformInfoImplBase`. Two queries
# (`can_have_non_undef_global_initializer_in_address_space`, `is_single_threaded`)
# have LLVM baselines that depend on `DataLayout`/`Module` state not reachable
# from these callbacks; we pick the common-case static answer and document that
# subtypes must override if they need the full LLVM behavior.

"""
    flat_address_space(tti::AbstractTargetTransformInfo) -> UInt

Address space the target treats as "flat" / generic. Required — alongside
[`is_noop_addr_space_cast`](@ref) — for `InferAddressSpacesPass` to fold
`addrspacecast`s. Default: `typemax(UInt)` (no flat AS).
"""
flat_address_space(::AbstractTargetTransformInfo) = typemax(UInt)

"""
    has_branch_divergence(tti::AbstractTargetTransformInfo) -> Bool

Whether the target can produce divergent control flow. Enables
divergence-aware passes (`SimplifyCFG`, loop analyses) when true. Default:
`false`.
"""
has_branch_divergence(::AbstractTargetTransformInfo) = false

"""
    is_single_threaded(tti::AbstractTargetTransformInfo) -> Bool

Whether the target is single-threaded. A few passes skip concurrency-related
transformations when true. Default: `false`. LLVM's actual baseline also
consults the module's `"single-thread"` flag — override if you rely on that.
"""
is_single_threaded(::AbstractTargetTransformInfo) = false

"""
    is_noop_addr_space_cast(tti::AbstractTargetTransformInfo,
                            from::Unsigned, to::Unsigned) -> Bool

Whether an `addrspacecast` from `from` to `to` is a noop at runtime. Default:
`false`.
"""
is_noop_addr_space_cast(::AbstractTargetTransformInfo, from::Unsigned, to::Unsigned) = false

"""
    is_valid_addr_space_cast(tti::AbstractTargetTransformInfo,
                             from::Unsigned, to::Unsigned) -> Bool

Whether an `addrspacecast` from `from` to `to` is permitted. Default: `false`.
"""
is_valid_addr_space_cast(::AbstractTargetTransformInfo, from::Unsigned, to::Unsigned) = false

"""
    addrspaces_may_alias(tti::AbstractTargetTransformInfo,
                         as0::Unsigned, as1::Unsigned) -> Bool

Whether pointers in address spaces `as0` and `as1` may alias. Default: `true`
(conservative — pointers may alias unless proven otherwise).
"""
addrspaces_may_alias(::AbstractTargetTransformInfo, as0::Unsigned, as1::Unsigned) = true

"""
    can_have_non_undef_global_initializer_in_address_space(
        tti::AbstractTargetTransformInfo, as::Unsigned) -> Bool

Whether globals in address space `as` may have non-`undef` initializers.
Default: `true`. LLVM's real baseline additionally consults
`DataLayout::isNonIntegralAddressSpace`, which this default can't reach —
override if the target has non-integral address spaces.
"""
can_have_non_undef_global_initializer_in_address_space(::AbstractTargetTransformInfo, as::Unsigned) = true

"""
    is_source_of_divergence(tti::AbstractTargetTransformInfo, v::Value) -> Bool

Whether `v` is a source of divergence. Consulted by `UniformityAnalysis`.
Default: `false`.
"""
is_source_of_divergence(::AbstractTargetTransformInfo, v::Value) = false

"""
    is_always_uniform(tti::AbstractTargetTransformInfo, v::Value) -> Bool

Whether `v` is known to hold the same value across all threads. Consulted by
`UniformityAnalysis`. Default: `false`.
"""
is_always_uniform(::AbstractTargetTransformInfo, v::Value) = false

"""
    get_assumed_addr_space(tti::AbstractTargetTransformInfo, v::Value) -> Unsigned

Address space statically known to hold for `v`, or `typemax(UInt)` for "no
assumption". Default: `typemax(UInt)`.
"""
get_assumed_addr_space(::AbstractTargetTransformInfo, v::Value) = typemax(UInt)

"""
    get_predicated_addr_space(tti::AbstractTargetTransformInfo, v::Value)
        -> (predicate::Union{Value,Nothing}, as::Unsigned)

Address space that holds for `v` when `predicate` is true. Return
`(nothing, typemax(UInt))` for "no inference". Default: that sentinel.
"""
get_predicated_addr_space(::AbstractTargetTransformInfo, v::Value) =
    (nothing, typemax(UInt))

"""
    rewrite_intrinsic_with_address_space(tti::AbstractTargetTransformInfo,
        ii::Value, old::Value, new::Value) -> Union{Value,Nothing}

Rewrite an intrinsic call `ii` after its pointer operand `old` has been
replaced by `new` (in a different address space). Return the rewritten value,
or `nothing` to keep the existing call. Default: `nothing`.
"""
rewrite_intrinsic_with_address_space(::AbstractTargetTransformInfo,
                                     ii::Value, old::Value, new::Value) = nothing

"""
    collect_flat_address_operands(tti::AbstractTargetTransformInfo, iid::Unsigned)
        -> Vector{Int}

Operand indices of intrinsic `iid` that are flat-address-space pointer
operands. Capped at 32 entries by the underlying C API. Default: `Int[]`.
"""
collect_flat_address_operands(::AbstractTargetTransformInfo, iid::Unsigned) = Int[]

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

function custom_tti_capture_exception!(state::CustomTTIState, err)
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
        custom_tti_capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_valid_addr_space_cast_callback(from::Cuint, to::Cuint,
                                                      ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_valid_addr_space_cast(state.tti, UInt(from), UInt(to))::Bool
    catch err
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_source_of_divergence_callback(ref::API.LLVMValueRef,
                                                     ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_source_of_divergence(state.tti, Value(ref))::Bool
    catch err
        custom_tti_capture_exception!(state, err)
        return false
    end
end

function custom_tti_is_always_uniform_callback(ref::API.LLVMValueRef,
                                               ud::Ptr{Cvoid})::API.LLVMBool
    state = Base.unsafe_pointer_to_objref(ud)::CustomTTIState
    try
        return is_always_uniform(state.tti, Value(ref))::Bool
    catch err
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
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
        custom_tti_capture_exception!(state, err)
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
    ud = Base.pointer_from_objref(state)
    opts = API.LLVMCreateTTIOptions()

    API.LLVMTTIOptionsSetFlatAddressSpace(opts, flat_address_space(tti) % Cuint)
    API.LLVMTTIOptionsSetHasBranchDivergence(opts, has_branch_divergence(tti))
    API.LLVMTTIOptionsSetIsSingleThreaded(opts, is_single_threaded(tti))

    API.LLVMTTIOptionsSetIsNoopAddrSpaceCast(opts,
        @cfunction(custom_tti_is_noop_addr_space_cast_callback,
                   API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetIsValidAddrSpaceCast(opts,
        @cfunction(custom_tti_is_valid_addr_space_cast_callback,
                   API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetAddrSpacesMayAlias(opts,
        @cfunction(custom_tti_addrspaces_may_alias_callback,
                   API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetCanHaveGlobalInitializerInAS(opts,
        @cfunction(custom_tti_can_have_global_initializer_in_as_callback,
                   API.LLVMBool, (Cuint, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetIsSourceOfDivergence(opts,
        @cfunction(custom_tti_is_source_of_divergence_callback,
                   API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetIsAlwaysUniform(opts,
        @cfunction(custom_tti_is_always_uniform_callback,
                   API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetGetAssumedAddressSpace(opts,
        @cfunction(custom_tti_get_assumed_address_space_callback,
                   Cuint, (API.LLVMValueRef, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetGetPredicatedAddressSpace(opts,
        @cfunction(custom_tti_get_predicated_address_space_callback,
                   Cuint, (API.LLVMValueRef, Ptr{API.LLVMValueRef}, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetRewriteIntrinsicWithAS(opts,
        @cfunction(custom_tti_rewrite_intrinsic_with_as_callback,
                   API.LLVMValueRef,
                   (API.LLVMValueRef, API.LLVMValueRef, API.LLVMValueRef, Ptr{Cvoid})), ud)
    API.LLVMTTIOptionsSetCollectFlatAddressOperands(opts,
        @cfunction(custom_tti_collect_flat_address_operands_callback,
                   API.LLVMBool,
                   (Cuint, Ptr{Cint}, Cuint, Ptr{Cuint}, Ptr{Cvoid})), ud)

    return state, opts
end
