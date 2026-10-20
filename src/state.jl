# Global state

# to simplify the API, we maintain a stack of contexts in task local storage
# and pass them implicitly to LLVM API's that require them.


## plain contexts

@vocabulary IR context, activate, deactivate, context!

# the active contexts of the current task are kept in a single stack, in the order they
# were activated, which can mix contexts of different libraries. each library sees its own
# stack: lookups only consider the contexts of one library, and contexts of one library
# need to be deactivated in reverse order, independently of those of other libraries.

# constructors that use the active context look it up all the time, so this is a single
# lookup, and type-asserted so that the C API wrappers it's passed to resolve statically.
function active_stack(key::Symbol, ::Type{T}) where {T}
    tls = task_local_storage()
    stack = get(tls, key, nothing)
    stack === nothing && return nothing
    return stack::Vector{T}
end

# the index of the most recently activated context of `lib` in `stack`, or `nothing`
function active_index(stack, lib::Library)
    stack === nothing && return nothing
    for i in lastindex(stack):-1:firstindex(stack)
        @inbounds(stack[i]).library === lib && return i
    end
    return nothing
end

@inline function active_context(key::Symbol, ::Type{T}, lib::Library,
                                throw_error::Bool) where {T}
    stack = active_stack(key, T)
    i = active_index(stack, lib)
    i === nothing || return @inbounds stack[i]
    throw_error && throw_no_context(T, lib, stack)
    return nothing
end

@noinline function throw_no_context(T, lib::Library, stack)
    kind = T === Context ? "LLVM context" : "LLVM thread-safe context"
    others = stack === nothing ? Symbol[] : unique!([ctx.library.name for ctx in stack])
    if isempty(others)
        error("No $kind is active")
    else
        error("""No $kind of LLVM library :$(lib.name) is active, but one of :$(join(others, ", :")) is.
                 Use `context!(ctx) do ... end` or `LLVM.library!` to make the context's library current.""")
    end
end

function push_active!(key::Symbol, ctx::T) where {T}
    stack = get!(() -> T[], task_local_storage(), key)::Vector{T}
    push!(stack, ctx)
    return
end

function pop_active!(key::Symbol, ctx::T) where {T}
    stack = active_stack(key, T)
    i = active_index(stack, ctx.library)
    if i === nothing || stack[i] != ctx
        error(T === Context ? "Deactivating wrong context" :
                              "Deactivating wrong thread-safe context")
    end
    deleteat!(stack, i)
    return
end

"""
    context(; throw_error::Bool=true)

Get the active LLVM context of the current library (see [`LLVM.library`](@ref)) for the
current task: the one that was activated most recently. Throws an exception if no context
of that library is active, unless `throw_error=false`.
"""
function context(; throw_error::Bool=true)
    # inlined, so that the result is a concrete `Context` when an error is thrown
    # otherwise; returning `Union{Nothing,Context}` would allocate, as contexts remember
    # their library
    @inline
    active_context(:LLVMContext, Context, library(), throw_error)
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

Make `ctx` the active context of its library for the current task (see
[`context`](@ref)), until it is deactivated.
"""
activate(ctx::Context) = push_active!(:LLVMContext, ctx)

"""
    deactivate(ctx::LLVM.Context)

Undo [`activate`](@ref)`(ctx)`. Contexts of a library need to be deactivated in the reverse
order of their activation, which throws an error otherwise.
"""
deactivate(ctx::Context) = pop_active!(:LLVMContext, ctx)

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

"""
    ts_context(; throw_error::Bool=true)

Get the active LLVM thread-safe context of the current library for the current task.
Throws an exception if no thread-safe context of that library is active, unless
`throw_error=false`.
"""
function ts_context(; throw_error::Bool=true)
    @inline
    ts_context(library(); throw_error)
end

# the active thread-safe context of another library, e.g., that of a module
@inline ts_context(lib::Library; throw_error::Bool=true) =
    active_context(:LLVMTSContext, ThreadSafeContext, lib, throw_error)

"""
    activate(ts_ctx::LLVM.ThreadSafeContext)

Make `ts_ctx` the active thread-safe context of its library for the current task (see
[`ts_context`](@ref)), until it is deactivated.
"""
activate(ts_ctx::ThreadSafeContext) = push_active!(:LLVMTSContext, ts_ctx)

"""
    deactivate(ts_ctx::LLVM.ThreadSafeContext)

Undo [`activate`](@ref)`(ts_ctx)`, in the reverse order of activation like for contexts.
"""
deactivate(ts_ctx::ThreadSafeContext) = pop_active!(:LLVMTSContext, ts_ctx)

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
