# Contexts are execution states for the core LLVM IR system.

export Context, dispose

"""
    LLVM.Context

Execution state for the core LLVM IR system. Created by calling the `Context()` constructor,
and should be disposed of.

Most types are tied to a context instance. Multiple contexts can exist simultaneously. A
single context is not thread safe. However, different contexts can execute on different
threads simultaneously.
"""
@checked struct Context
    ref::API.LLVMContextRef
end

Base.unsafe_convert(::Type{API.LLVMContextRef}, ctx::Context) = mark_use(ctx).ref

"""
    LLVM.Context(; opaque_pointers=nothing)

Create a new LLVM context. If `opaque_pointers` is `true`, the context will use opaque
pointers instead of typed pointers (if suppoprted). Otherwise the behavior of the context
depends on the LLVM version.

This object needs to be disposed of using [`dispose(::Context)`](@ref).
"""
function Context(; opaque_pointers=nothing)
    ctx = mark_alloc(Context(API.LLVMContextCreate()))
    if opaque_pointers !== nothing
        opaque_pointers!(ctx, opaque_pointers)
    end
    _install_handlers(ctx)
    activate(ctx)
    ctx
end

# whether a context being disposed should be leaked instead of freed. this is the case
# when an exception is in flight (i.e., when disposing from a `finally` block during
# unwinding): the exception, or test machinery recording it, may have captured objects
# from this context and will only display them after the context has been disposed,
# which would crash the process.
leak_context() = !isempty(current_exceptions())

"""
    dispose(ctx::Context)

Dispose of the context, releasing all resources associated with it. The context should not
be used after this operation.

If an exception is in flight (e.g., when the context is disposed of by a `finally` block
during stack unwinding), the context is popped from the context stack but intentionally
leaked instead of freed. This keeps any values captured by the exception valid, so that
error reporting does not crash the process.
"""
function dispose(ctx::Context)
    deactivate(ctx)
    leak = leak_context()
    leak || _remove_handlers(ctx)
    # in the leak path we still record the dispose in memcheck bookkeeping,
    # just without actually freeing, so report_leaks stays quiet and any real
    # missing dispose still stands out.
    mark_dispose(leak ? Returns(nothing) : API.LLVMContextDispose, ctx)
end

function Context(f::Core.Function; kwargs...)
    ctx = Context(; kwargs...)
    try
        f(ctx)
    finally
        dispose(ctx)
    end
end

function Base.show(io::IO, ctx::Context)
    @printf(io, "LLVM.Context(%p", ctx.ref)
    if version() < v"17"
        # migration to opaque pointers
        print(io, ", ", supports_typed_pointers(ctx) ? "typed ptrs" : "opaque ptrs")
    end
    print(io, ")")
end

@noinline throw_typedpointererror() =
    error("""Typed pointers are not supported.

             You are invoking an API without specifying the pointer type, but this LLVM context
             uses opaque pointers. You should either pass the element type of the pointer as an
             argument, or use an environment that sypports typed pointers.""")


## opaque pointer handling

export supports_typed_pointers

"""
    supports_typed_pointers()

Check whether the current context supports typed pointers.
"""
supports_typed_pointers() = supports_typed_pointers(context())

if version() >= v"17"
    supports_typed_pointers(ctx::Context) = false
elseif version() >= v"13"
    supports_typed_pointers(ctx::Context) =
        API.LLVMContextSupportsTypedPointers(ctx) |> Bool

    unsafe_opaque_pointers!(ctx::Context, enable::Bool) =
        API.LLVMContextSetOpaquePointers(ctx, enable)
else
    supports_typed_pointers(ctx::Context) = true
end

function opaque_pointers!(ctx::Context, opaque_pointers::Bool)
    @static if version() < v"13"
        if opaque_pointers
            error("LLVM <13 does not support opaque pointers")
        end
    end

    @static if version() >= v"17"
        if !opaque_pointers
            error("LLVM >=17 does not support typed pointers")
        end
    end

    @static if v"13" <= version() < v"17"
        unsafe_opaque_pointers!(ctx, opaque_pointers)
    end
end


## wrapper exception type

export LLVMException

"""
    LLVMException

Exception type for errors reported by the LLVM API.
"""
struct LLVMException <: Exception
    info::String
end

function Base.showerror(io::IO, err::LLVMException)
    @printf(io, "LLVM error: %s", err.info)
end


## diagnostic handling

@checked struct DiagnosticInfo
    ref::API.LLVMDiagnosticInfoRef
end

Base.unsafe_convert(::Type{API.LLVMDiagnosticInfoRef}, di::DiagnosticInfo) = di.ref

severity(di::DiagnosticInfo) = API.LLVMGetDiagInfoSeverity(di)
message(di::DiagnosticInfo) = unsafe_message(API.LLVMGetDiagInfoDescription(di))

function handle_diagnostic(diag_ref::API.LLVMDiagnosticInfoRef, args::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(args)::DiagnosticState
    try
        di = DiagnosticInfo(diag_ref)
        sev = severity(di)
        msg = message(di)

        if sev == API.LLVMDSError
            state.error === nothing && (state.error = msg)
        elseif sev == API.LLVMDSWarning
            @warn msg
        elseif sev == API.LLVMDSRemark || sev == API.LLVMDSNote
            @debug msg
        else
            state.error === nothing &&
                (state.error = "unknown diagnostic severity level $sev")
        end
    catch
        # Diagnostic callbacks have no error return and must never unwind
        # through their C++ caller, including when logging itself fails.
    end

    return nothing
end

mutable struct DiagnosticState
    error::Union{Nothing,String}
    DiagnosticState() = new(nothing)
end

const DIAGNOSTIC_STATES = Dict{API.LLVMContextRef,DiagnosticState}()

function prepare_diagnostic(ctx::Context)
    state = get(DIAGNOSTIC_STATES, ctx.ref, nothing)
    state === nothing || (state.error = nothing)
    return nothing
end

function check_diagnostic(ctx::Context, failed::Bool=false,
                          fallback::String="LLVM operation failed")
    state = get(DIAGNOSTIC_STATES, ctx.ref, nothing)
    if state !== nothing && state.error !== nothing
        msg = state.error
        state.error = nothing
        throw(LLVMException(msg))
    elseif failed
        throw(LLVMException(fallback))
    end
    return nothing
end

function yield_callback(ctx_ref::API.LLVMContextRef, args::Ptr{Cvoid})
    ctx = Context(ctx_ref)
    @assert args == C_NULL

    # TODO: is this allowed? can we yield out of an active `ccall`?
    yield()
end

function _install_handlers(ctx::Context)
    # set yield callback
    callback = @cfunction(yield_callback, Cvoid, (Context, Ptr{Cvoid}))
    # NOTE: disabled until proven safe
    #API.LLVMContextSetYieldCallback(ctx, callback, C_NULL)

    # set diagnostic callback
    state = DiagnosticState()
    DIAGNOSTIC_STATES[ctx.ref] = state
    handler = @cfunction(handle_diagnostic, Cvoid, (API.LLVMDiagnosticInfoRef, Ptr{Cvoid}))
    API.LLVMContextSetDiagnosticHandler(ctx, handler, Base.pointer_from_objref(state))

    return nothing
end

function _remove_handlers(ctx::Context)
    API.LLVMContextSetDiagnosticHandler(ctx, C_NULL, C_NULL)
    delete!(DIAGNOSTIC_STATES, ctx.ref)
    return nothing
end

function _install_handlers()
    # LLVM fatal handlers are notifications before unconditional exit/abort,
    # not recovery hooks. Keep LLVM's default reporting and termination path.
    return nothing
end
