# Global state

# to simplify the API, we maintain a stack of contexts in task local storage
# and pass them implicitly to LLVM API's that require them.


## plain contexts

@vocabulary IR context, activate, deactivate, context!

# the stack of contexts of the current task, if any. constructors that use the active
# context call this all the time, so it's a single lookup, and type-asserted so that the
# C API wrappers it's passed to resolve statically.
context_stack() =
    get(task_local_storage(), :LLVMContext, nothing)::Union{Nothing,Vector{Context}}

"""
    context(; throw_error::Bool=true)

Get the active LLVM context for the current tasks. Throws an exception if no context is
active, unless `throw_error=false`. The context needs to belong to the current library (see
[`LLVM.library`](@ref)).
"""
function context(; throw_error::Bool=true)
    # inlined, so that the result is a concrete `Context` when an error is thrown
    # otherwise; returning `Union{Nothing,Context}` would allocate, as contexts remember
    # their library
    @inline
    stack = context_stack()
    if stack === nothing || isempty(stack)
        throw_error && error("No LLVM context is active")
        return nothing
    end
    ctx = last(stack)
    ctx.library === library() || throw_inactive_library(ctx)
    return ctx
end

@noinline throw_inactive_library(@nospecialize(ctx)) =
    error("""The active context belongs to LLVM library :$(ctx.library.name), but the current library is :$(library().name).
             Use `context!(ctx) do ... end` or `LLVM.library!` to make the context's library current.""")

"""
    activate(obj)

Make `obj` the active object of its kind for the current task, by pushing it onto a
task-local stack, e.g., the stack of contexts whose top [`context()`](@ref) returns. Undo
this using [`deactivate`](@ref), in reverse order.

Packages can add methods for their own types, e.g., to maintain a task-local stack of the
contexts of a related C API. Such a stack is separate from LLVM.jl's: activating one of
these objects doesn't activate an LLVM context.
"""
function activate end

"""
    deactivate(obj)

Undo [`activate`](@ref)`(obj)`, popping `obj` from the task-local stack that it was pushed
onto, which throws an error if `obj` isn't the active object.

Packages can add methods for their own types, along with methods for `activate`.
"""
function deactivate end

"""
    activate(ctx::LLVM.Context)

Pushes a new context onto the context stack.
"""
function activate(ctx::Context)
    stack = get!(task_local_storage(), :LLVMContext) do
        Context[]
    end
    push!(stack, ctx)
    return
end

"""
    deactivate(ctx::LLVM.Context)

Pops the current context from the context stack.
"""
function deactivate(ctx::Context)
    stack = context_stack()
    stack !== nothing && !isempty(stack) && last(stack) == ctx ||
        error("Deactivating wrong context")
    pop!(stack)
end

"""
    context!(ctx::LLVM.Context) do
        ...
    end

Temporarily activates the given context for the duration of the block, and makes its
library the current one (see [`LLVM.library!`](@ref)).
"""
function context!(f, ctx::Context)
    library!(ctx.library) do
        activate(ctx)
        try
            f()
        finally
            deactivate(ctx)
        end
    end
end


## thread-safe contexts

@vocabulary ORC ts_context, activate, deactivate, ts_context!

ts_context_stack() = get(task_local_storage(), :LLVMTSContext,
                         nothing)::Union{Nothing,Vector{ThreadSafeContext}}

"""
    ts_context(; throw_error::Bool=true)

Get the active LLVM thread-safe context for the current tasks. Throws an exception if no
context is active, unless `throw_error=false`.
"""
function ts_context(; throw_error::Bool=true)
    @inline
    stack = ts_context_stack()
    if stack === nothing || isempty(stack)
        throw_error && error("No LLVM thread-safe context is active")
        return nothing
    end
    ts_ctx = last(stack)
    ts_ctx.library === library() || throw_inactive_library(ts_ctx)
    return ts_ctx
end

"""
    activate(ts_ctx::LLVM.ThreadSafeContext)

Pushes a new thread-safe context onto the context stack.
"""
function activate(ts_ctx::ThreadSafeContext)
    stack = get!(task_local_storage(), :LLVMTSContext) do
        ThreadSafeContext[]
    end
    push!(stack, ts_ctx)
    return
end

"""
    deactivate(ts_ctx::LLVM.ThreadSafeContext)

Pops the current thread-safe context from the context stack.
"""
function deactivate(ts_ctx::ThreadSafeContext)
    stack = ts_context_stack()
    stack !== nothing && !isempty(stack) && last(stack) == ts_ctx ||
        error("Deactivating wrong thread-safe context")
    pop!(stack)
end

"""
    ts_context!(ts_ctx::LLVM.ThreadSafeContext) do
        ...
    end

Temporarily activates the given thread-safe context for the duration of the block, and
makes its library the current one (see [`LLVM.library!`](@ref)).
"""
function ts_context!(f, ts_ctx::ThreadSafeContext)
    library!(ts_ctx.library) do
        activate(ts_ctx)
        try
            f()
        finally
            deactivate(ts_ctx)
        end
    end
end
