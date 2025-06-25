export ThreadSafeModule, ThreadSafeContext

"""
    ThreadSafeContext

A thread-safe version of [`Context`](@ref).
"""
@checked struct ThreadSafeContext
    ref::API.LLVMOrcThreadSafeContextRef
end
Base.unsafe_convert(::Type{API.LLVMOrcThreadSafeContextRef}, ctx::ThreadSafeContext) = mark_use(ctx).ref

"""
    ThreadSafeContext(; opaque_pointers=nothing)

Create a new thread-safe context. The behavior of `opaque_pointers` is the same as in
[`Context`](@ref).

This object needs to be disposed of using [`dispose(::ThreadSafeContext)`](@ref).
"""
function ThreadSafeContext(; opaque_pointers=nothing)
    ts_ctx = mark_alloc(ThreadSafeContext(API.LLVMOrcCreateNewThreadSafeContext()))
    if opaque_pointers !== nothing
        opaque_pointers!(context(ts_ctx), opaque_pointers)
    end
    activate(ts_ctx)
    ts_ctx
end

function ThreadSafeContext(f::Core.Function; kwargs...)
    ctx = ThreadSafeContext(; kwargs...)
    try
        f(ctx)
    finally
        dispose(ctx)
    end
end

"""
    context(ts_ctx::ThreadSafeContext)

Obtain the context associated with a thread-safe context.

!!! warning

    This is an usafe operation, as the return context can be accessed in a thread-unsafe
    manner.
"""
function context(ctx::ThreadSafeContext)
    ref = API.LLVMOrcThreadSafeContextGetContext(ctx)
    Context(ref)
end

"""
    dispose(ctx::ThreadSafeContext)

Dispose of the thread-safe context, releasing all resources associated with it.
"""
function dispose(ctx::ThreadSafeContext)
    deactivate(ctx)
    mark_dispose(API.LLVMOrcDisposeThreadSafeContext, ctx)
end

"""
    ThreadSafeModule

A thread-safe version of [`LLVM.Module`](@ref).
"""
@checked struct ThreadSafeModule
    ref::API.LLVMOrcThreadSafeModuleRef
end
Base.unsafe_convert(::Type{API.LLVMOrcThreadSafeModuleRef}, mod::ThreadSafeModule) = mod.ref

"""
    ThreadSafeModule(mod::Module)

Create a thread-safe module from a regular module. This transfers ownership of the module to
the thread-safe module.

!!! warning

    The context of the module must be the same as the current thread-safe context.

This object needs to be disposed of using [`dispose(::ThreadSafeModule)`](@ref).
"""
function ThreadSafeModule(mod::Module)
    if context(mod) != context(ts_context())
        # XXX: the C API doesn't expose the convenience method to create a TSModule from a
        #      Module and a regular Context, only a method to create one from a Module
        #      and a pre-existing TSContext, which isn't useful...
        # TODO: expose the other convenience method?
        # XXX: work around this by serializing/deserializing in the correct context
        bitcode = convert(MemoryBuffer, mod)
        dispose(mod)
        mod = context!(context(ts_context())) do
            parse(Module, bitcode)
        end
        dispose(bitcode)
    end
    @assert context(mod) == context(ts_context())

    ref = API.LLVMOrcCreateNewThreadSafeModule(mod, ts_context())
    tsm = mark_alloc(ThreadSafeModule(ref))
    mark_dispose(mod)
    return tsm
end

"""
    ThreadSafeModule(name::String)

Create a thread-safe module with the given name.

This object needs to be disposed of using [`dispose(::ThreadSafeModule)`](@ref).
"""
function ThreadSafeModule(name::String)
    ts_ctx = ts_context()
    # XXX: we should lock the context here
    ctx = context(ts_ctx)
    mod = context!(ctx) do
        Module(name)
    end
    ThreadSafeModule(mod)
end

"""
    dispose(mod::ThreadSafeModule)

Dispose of the thread-safe module, releasing all resources associated with it.
"""
function dispose(mod::ThreadSafeModule)
    mark_dispose(API.LLVMOrcDisposeThreadSafeModule, mod)
end

mutable struct ThreadSafeModuleCallback
    ret::Ref{Any}
    callback

    ThreadSafeModuleCallback(callback) = new(Ref{Any}(), callback)
end

function tsm_callback(data::Ptr{Cvoid}, ref::API.LLVMModuleRef)
    cb = Base.unsafe_pointer_to_objref(data)::ThreadSafeModuleCallback
    mod = mark_alloc(Module(ref); allow_overwrite=true)
    ctx = context(mod)
    activate(ctx)
    try
        cb.ret = cb.callback(Module(ref))
    catch err
        msg = sprint(Base.display_error, err, Base.catch_backtrace())
        return API.LLVMCreateStringError(msg)
    finally
        mark_dispose(mod)
        deactivate(ctx)
    end
    return convert(API.LLVMErrorRef, C_NULL)
end

"""
    (mod::ThreadSafeModule)(f)

Apply `f` to the LLVM module contained within `mod`, after locking the module and activating
its context.
"""
function (mod::ThreadSafeModule)(f)
    cb = ThreadSafeModuleCallback(f)
    GC.@preserve cb begin
        @check API.LLVMOrcThreadSafeModuleWithModuleDo(
            mod,
            @cfunction(tsm_callback, API.LLVMErrorRef, (Ptr{Cvoid}, API.LLVMModuleRef)),
            Base.pointer_from_objref(cb))
    end
    cb.ret[]
end
