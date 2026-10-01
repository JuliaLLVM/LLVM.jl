# Global state

# to simplify the API, we maintain a stack of contexts in task local storage
# and pass them implicitly to LLVM API's that require them.


## plain contexts

@vocabulary IR context, activate, deactivate, context!

_has_context() = haskey(task_local_storage(), :LLVMContext) &&
                 !isempty(task_local_storage(:LLVMContext))

"""
    context(; throw_error::Bool=true)

Get the active LLVM context for the current tasks. Throws an exception if no context is
active, unless `throw_error=false`.
"""
function context(; throw_error::Bool=true)
    if !_has_context()
        throw_error && error("No LLVM context is active")
        return nothing
    end
    last(task_local_storage(:LLVMContext))
end

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
    context() == ctx || error("Deactivating wrong context")
    pop!(task_local_storage(:LLVMContext))
end

"""
    context!(ctx::LLVM.Context) do
        ...
    end

Temporarily activates the given context for the duration of the block.
"""
function context!(f, ctx::Context)
    activate(ctx)
    try
        f()
    finally
        deactivate(ctx)
    end
end


## thread-safe contexts

@vocabulary ORC ts_context, activate, deactivate, ts_context!

_has_ts_context() = haskey(task_local_storage(), :LLVMTSContext) &&
                    !isempty(task_local_storage(:LLVMTSContext))

"""
    ts_context(; throw_error::Bool=true)

Get the active LLVM thread-safe context for the current tasks. Throws an exception if no
context is active, unless `throw_error=false`.
"""
function ts_context(; throw_error::Bool=true)
    if !_has_ts_context()
        throw_error && error("No LLVM thread-safe context is active")
        return nothing
    end
    last(task_local_storage(:LLVMTSContext))
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
    ts_context() == ts_ctx || error("Deactivating wrong thread-safe context")
    pop!(task_local_storage(:LLVMTSContext))
end

"""
    ts_context!(ts_ctx::LLVM.ThreadSafeContext) do
        ...
    end

Temporarily activates the given thread-safe context for the duration of the block.
"""
function ts_context!(f, ts_ctx::ThreadSafeContext)
    activate(ts_ctx)
    try
        f()
    finally
        deactivate(ts_ctx)
    end
end
