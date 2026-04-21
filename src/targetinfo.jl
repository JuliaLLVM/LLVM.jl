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

Subtype this and override the query methods to supply a `TargetTransformInfo`
for pipelines that lack a `TargetMachine` (e.g. out-of-tree backends invoked
through a CLI) where the default TTI would otherwise be the conservative
baseline and disable TTI-sensitive passes such as `InferAddressSpacesPass` or
`UniformityAnalysis`.

Only the subset of TTI hooks relevant to those passes is exposed; each has a
default method on `AbstractTargetTransformInfo` that matches LLVM's
`TargetTransformInfoImplBase` behavior, so subtypes only need to override the
queries they care about.

Attach an instance with [`target_transform_info!`](@ref). To run a pipeline
against LLVM's native TTI (including `TargetMachine`-derived and
`DataLayout`/`Module`-dependent defaults), pass `nothing` instead of an
instance — no callbacks are registered in that case.

# Simple knobs

- [`flat_address_space`](@ref)`(tti) -> UInt`: address space LLVM should treat
  as "flat" / generic. Required for `InferAddressSpacesPass` and for folding
  `addrspacecast`s. Default: `typemax(UInt)` (no flat AS).
- [`has_branch_divergence`](@ref)`(tti) -> Bool`: whether this target can
  produce divergent control flow. Default: `false`.
- [`is_single_threaded`](@ref)`(tti) -> Bool`: whether this target is
  single-threaded. Default: `false`.

# Address-space predicates

- [`is_noop_addr_space_cast`](@ref)`(tti, from, to) -> Bool`: whether an
  addrspacecast between the given AS pair is a noop. Required (alongside
  `flat_address_space`) for `InferAddressSpacesPass` to actually fold casts.
  Default: `false`.
- [`is_valid_addr_space_cast`](@ref)`(tti, from, to) -> Bool`. Default: `false`.
- [`addrspaces_may_alias`](@ref)`(tti, as0, as1) -> Bool`. Default: `true`
  (conservative — pointers may alias unless proven otherwise).
- [`can_have_non_undef_global_initializer_in_address_space`](@ref)`(tti, as) -> Bool`.
  Default: `true`. LLVM's real baseline also considers
  `DataLayout::isNonIntegralAddressSpace`, which isn't reachable from a
  Julia-side default; override if your target has non-integral ASes.

# Value-oriented queries

- [`is_source_of_divergence`](@ref)`(tti, v::Value) -> Bool`. Default: `false`.
- [`is_always_uniform`](@ref)`(tti, v::Value) -> Bool`. Default: `false`.
- [`get_assumed_addr_space`](@ref)`(tti, v::Value) -> Unsigned`: an AS known to
  hold for this value, or `typemax(UInt)` for "no assumption". Default:
  `typemax(UInt)`.
- [`get_predicated_addr_space`](@ref)`(tti, v::Value) -> (predicate, as)`: an
  AS that holds when `predicate::Union{Value,Nothing}` is true. Default:
  `(nothing, typemax(UInt))`.

# Intrinsic hooks

- [`rewrite_intrinsic_with_address_space`](@ref)`(tti, ii::Value, old::Value, new::Value) -> Union{Value,Nothing}`.
  Default: `nothing` (no rewrite).
- [`collect_flat_address_operands`](@ref)`(tti, iid::UInt) -> Vector{Int}`:
  flat-AS pointer operand indices of intrinsic `iid`. Capped at 32 entries.
  Default: `Int[]`.
"""
abstract type AbstractTargetTransformInfo end

# Defaults matching LLVM's `TargetTransformInfoImplBase`. Two queries
# (`can_have_non_undef_global_initializer_in_address_space`, `is_single_threaded`)
# have LLVM baselines that depend on `DataLayout`/`Module` state not reachable
# from these callbacks; we pick the common-case static answer and document that
# subtypes must override if they need the full LLVM behavior.
flat_address_space(::AbstractTargetTransformInfo) = typemax(UInt)
has_branch_divergence(::AbstractTargetTransformInfo) = false
is_single_threaded(::AbstractTargetTransformInfo) = false

is_noop_addr_space_cast(::AbstractTargetTransformInfo, from::Unsigned, to::Unsigned) = false
is_valid_addr_space_cast(::AbstractTargetTransformInfo, from::Unsigned, to::Unsigned) = false
addrspaces_may_alias(::AbstractTargetTransformInfo, as0::Unsigned, as1::Unsigned) = true
can_have_non_undef_global_initializer_in_address_space(::AbstractTargetTransformInfo, as::Unsigned) = true

is_source_of_divergence(::AbstractTargetTransformInfo, v::Value) = false
is_always_uniform(::AbstractTargetTransformInfo, v::Value) = false
get_assumed_addr_space(::AbstractTargetTransformInfo, v::Value) = typemax(UInt)
get_predicated_addr_space(::AbstractTargetTransformInfo, v::Value) =
    (nothing, typemax(UInt))

rewrite_intrinsic_with_address_space(::AbstractTargetTransformInfo,
                                     ii::Value, old::Value, new::Value) = nothing
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

# Build the C-side `LLVMTTIOptions` struct from the TTI object. Returns a
# `(state, opts_ref)` pair; callers pass `opts_ref` to whatever
# `…SetTTI` entrypoint their pipeline wrapper exposes, and must keep both
# `state` and `opts_ref` alive for the duration of the pipeline run.
function build_custom_tti_options(tti::AbstractTargetTransformInfo)
    state = CustomTTIState(tti)
    ud = Base.pointer_from_objref(state)

    noop_cb = @cfunction(custom_tti_is_noop_addr_space_cast_callback,
                         API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
    valid_cb = @cfunction(custom_tti_is_valid_addr_space_cast_callback,
                          API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
    alias_cb = @cfunction(custom_tti_addrspaces_may_alias_callback,
                          API.LLVMBool, (Cuint, Cuint, Ptr{Cvoid}))
    ginit_cb = @cfunction(custom_tti_can_have_global_initializer_in_as_callback,
                          API.LLVMBool, (Cuint, Ptr{Cvoid}))
    srcdiv_cb = @cfunction(custom_tti_is_source_of_divergence_callback,
                           API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid}))
    unif_cb = @cfunction(custom_tti_is_always_uniform_callback,
                         API.LLVMBool, (API.LLVMValueRef, Ptr{Cvoid}))
    assas_cb = @cfunction(custom_tti_get_assumed_address_space_callback,
                          Cuint, (API.LLVMValueRef, Ptr{Cvoid}))
    predas_cb = @cfunction(custom_tti_get_predicated_address_space_callback,
                           Cuint, (API.LLVMValueRef, Ptr{API.LLVMValueRef}, Ptr{Cvoid}))
    rewrite_cb = @cfunction(custom_tti_rewrite_intrinsic_with_as_callback,
                            API.LLVMValueRef,
                            (API.LLVMValueRef, API.LLVMValueRef, API.LLVMValueRef, Ptr{Cvoid}))
    collect_cb = @cfunction(custom_tti_collect_flat_address_operands_callback,
                            API.LLVMBool,
                            (Cuint, Ptr{Cint}, Cuint, Ptr{Cuint}, Ptr{Cvoid}))

    p(cf) = Base.unsafe_convert(Ptr{Cvoid}, cf)

    opts = Ref(API.LLVMTTIOptions(
        flat_address_space(tti) % Cuint,
        Int32(has_branch_divergence(tti)),
        Int32(is_single_threaded(tti)),
        p(noop_cb),    ud,
        p(valid_cb),   ud,
        p(alias_cb),   ud,
        p(ginit_cb),   ud,
        p(srcdiv_cb),  ud,
        p(unif_cb),    ud,
        p(assas_cb),   ud,
        p(predas_cb),  ud,
        p(rewrite_cb), ud,
        p(collect_cb), ud,
    ))

    return state, opts
end
