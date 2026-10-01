@vocabulary ORC ThreadSafeModule, ThreadSafeContext

"""
    ThreadSafeContext

A thread-safe version of [`Context`](@ref).

The underlying context is reference counted: the thread-safe context and every
[`ThreadSafeModule`](@ref LLVM.ThreadSafeModule) of it share it, so that it remains alive
while one of them is. That doesn't extend to the regular modules in the context, e.g., ones
created after activating `context(ts_ctx)`, copied from the module of a thread-safe module,
or taken out of one using [`LLVM.unsafe_take_module!`](@ref): these can only be used while
the thread-safe context they were created with is alive, as disposing of it may free them.
To keep such a module, wrap it in a thread-safe module, or serialize it, before disposing
of the thread-safe context.
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
    ctx = mark_untracked(context(ts_ctx))
    memcheck_register_context(ctx, ts_ctx)
    if opaque_pointers !== nothing
        opaque_pointers!(ctx, opaque_pointers)
    end
    _install_handlers(ctx)
    activate(ts_ctx)
    ts_ctx
end

ThreadSafeContext(f::Core.Function; kwargs...) =
    with_disposal(f, ThreadSafeContext(; kwargs...))

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

Dispose of the thread-safe context, releasing all resources associated with it. This frees
the underlying context, and the modules in it, unless a thread-safe module (or foreign
code) keeps it alive. Regular modules in the context can't be used afterwards in either
case (see [`ThreadSafeContext`](@ref LLVM.ThreadSafeContext)).

If an exception is in flight, the context is leaked instead of freed, so that values
captured by the exception remain valid; see [`dispose(::Context)`](@ref).
"""
function dispose(ctx::ThreadSafeContext)
    deactivate(ctx)
    @static if memcheck_enabled
        memcheck_unregister_context(context(ctx), ctx)
    end
    leak = leak_context()
    leak || _remove_handlers(context(ctx))
    mark_dispose(leak ? Returns(nothing) : API.LLVMOrcDisposeThreadSafeContext, ctx)
end

"""
    ThreadSafeModule

A thread-safe version of [`LLVM.Module`](@ref).

A thread-safe module is consumed by adding it to a JIT, e.g., with `add!` or [`emit!`](@ref),
after which it can't be used anymore, and disposing of it does nothing, so it can be
disposed of unconditionally, e.g., using the do-block form of its constructors
(`ThreadSafeModule(name) do tsm ... end`). That's different from calling the module
(`tsm() do mod ... end`), which gives access to the module it contains. The modules that an
IR transformation receives are borrowed: they can be used during the transformation, but
not be consumed or disposed of.

To use a thread-safe module that foreign code created, e.g., a `ccall` that returns a
`LLVMOrcThreadSafeModuleRef`, wrap its handle with `ThreadSafeModule(ref)`, which takes
over the responsibility to dispose of or consume it, or `ThreadSafeModule(ref;
borrowed=true)` to only use it, in which case it can't be disposed of or consumed. Neither
keeps the object alive, or affects its lifetime otherwise: a borrowed thread-safe module
can only be used for as long as its owner keeps it alive. To hand an owned thread-safe
module over to foreign code, use [`LLVM.consume!`](@ref).
"""
mutable struct ThreadSafeModule
    ref::API.LLVMOrcThreadSafeModuleRef
    owned::Bool     # whether we own the module, i.e., it wasn't consumed or borrowed
    borrowed::Bool  # whether the module is borrowed from LLVM (e.g., in an IR transform)
    # whether `unsafe_module` gave access to the module, which memcheck then doesn't track
    unsafe_access::Bool

    function ThreadSafeModule(ref::API.LLVMOrcThreadSafeModuleRef; borrowed::Bool=false)
        ref == C_NULL && throw(UndefRefError())
        tsm = new(ref, !borrowed, borrowed, false)
        borrowed ? tsm : mark_alloc(tsm)
    end
end

function check_usable(tsm::ThreadSafeModule)
    tsm.owned || tsm.borrowed ||
        throw(ArgumentError("This ThreadSafeModule has been consumed or disposed of"))
    return mark_use(tsm)
end

Base.unsafe_convert(::Type{API.LLVMOrcThreadSafeModuleRef}, tsm::ThreadSafeModule) =
    check_usable(tsm).ref

# the module can be taken out of a thread-safe module (see `unsafe_take_module!`), also
# using another wrapper of it, which leaves it empty. LLVM asserts on (or crashes with) an
# empty thread-safe module, so check for that natively.
function check_has_module(tsm::ThreadSafeModule)
    API.LLVMExtraThreadSafeModuleGetModuleUnlocked(tsm) == C_NULL &&
        throw(ArgumentError("The module of this ThreadSafeModule has been taken out of it"))
    return tsm
end

function check_consumable(tsm::ThreadSafeModule)
    tsm.borrowed && throw(ArgumentError("A borrowed ThreadSafeModule can't be consumed"))
    check_usable(tsm)
    return check_has_module(tsm)
end

function consume!(tsm::ThreadSafeModule)
    check_consumable(tsm)
    tsm.owned = false
    mark_dispose(tsm)
    return tsm.ref
end

"""
    ThreadSafeModule(mod::Module)

Create a thread-safe module from a regular module. This transfers ownership of the module to
the thread-safe module, so the module must not be disposed of afterwards.

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
    tsm = ThreadSafeModule(ref)
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

Dispose of the thread-safe module, releasing all resources associated with it, unless it
has been consumed. Borrowed modules can't be disposed of.
"""
function dispose(mod::ThreadSafeModule)
    mod.borrowed && throw(ArgumentError("A borrowed ThreadSafeModule can't be disposed of"))
    dispose_owned(API.LLVMOrcDisposeThreadSafeModule, mod)
end

mutable struct ThreadSafeModuleCallback
    ret::Ref{Any}
    callback
    tsm::ThreadSafeModule

    ThreadSafeModuleCallback(callback, tsm) = new(Ref{Any}(), callback, tsm)
end

function tsm_callback(data::Ptr{Cvoid}, ref::API.LLVMModuleRef)
    cb = Base.unsafe_pointer_to_objref(data)::ThreadSafeModuleCallback
    # the module is only valid during the callback, unless `unsafe_module` is used
    mod = Module(ref)
    tracked = !cb.tsm.unsafe_access
    # (it's borrowed for the duration of the callback, not owned by the thread-safe context)
    tracked && mark_alloc(mod; allow_overwrite=true, owner=nothing)
    ctx = context(mod)
    activate(ctx)
    try
        cb.ret = cb.callback(Module(ref))
    catch err
        msg = sprint(Base.display_error, err, Base.catch_backtrace())
        return API.LLVMCreateStringError(msg)
    finally
        # also check whether `unsafe_module` was called during the callback
        tracked && !cb.tsm.unsafe_access && mark_dispose(mod)
        deactivate(ctx)
    end
    return convert(API.LLVMErrorRef, C_NULL)
end

ThreadSafeModule(f::Core.Function, args...) = with_disposal(f, ThreadSafeModule(args...))

"""
    (mod::ThreadSafeModule)(f)

Apply `f` to the LLVM module contained within `mod`, after locking the module and activating
its context. Exceptions from `f` are reported after LLVM releases the module lock.

The module is only valid during the call, and should not be used after `f` returns: its
context isn't locked anymore then, and the thread-safe module can be consumed, e.g., by
compiling it. To use information from the module afterwards, extract it within `f`, e.g.,
by serializing the module to bitcode that can be parsed in another context. See
[`LLVM.unsafe_module`](@ref) for accessing the module when the lifetime of the thread-safe
module and synchronization of its context are ensured otherwise.
"""
function (mod::ThreadSafeModule)(f)
    check_has_module(mod)
    cb = ThreadSafeModuleCallback(f, mod)
    GC.@preserve cb begin
        @check API.LLVMOrcThreadSafeModuleWithModuleDo(
            mod,
            @cfunction(tsm_callback, API.LLVMErrorRef, (Ptr{Cvoid}, API.LLVMModuleRef)),
            Base.pointer_from_objref(cb))
    end
    cb.ret[]
end

@public unsafe_module

"""
    LLVM.unsafe_module(tsm::ThreadSafeModule)

Get the module contained in `tsm`, without locking or activating its context. This is an
escape hatch for when calling the thread-safe module (`tsm() do mod ... end`), which
locks its context while giving access to the module, isn't possible, e.g., when the module
needs to be used after the thread-safe module was obtained from and returned to foreign
code.

The module is borrowed: it can only be used for as long as the thread-safe module and its
context are alive, and the thread-safe module isn't consumed. The caller is responsible
for synchronizing all accesses to the context (which other threads may be using to compile
code), for activating it if needed, and should never dispose of the module. The `memcheck`
debugging mode doesn't track the module afterwards, also not when calling `tsm`. To take
ownership of the module instead, see [`LLVM.unsafe_take_module!`](@ref).
"""
function unsafe_module(tsm::ThreadSafeModule)
    check_has_module(tsm)
    mod = Module(API.LLVMExtraThreadSafeModuleGetModuleUnlocked(tsm))
    tsm.unsafe_access = true
    return mark_untracked(mod)
end

@public unsafe_take_module!

"""
    LLVM.unsafe_take_module!(tsm::ThreadSafeModule) -> LLVM.Module

Move the module out of `tsm`, leaving the thread-safe module empty, and return it. The
caller owns the module afterwards: it has to be disposed of, or handed over to an operation
that consumes it (like `link!`). This requires LLVM 16 or later.

This is for taking ownership of the module of a thread-safe module that foreign code owns,
e.g., the one that Julia's code generator returns. That's destructive access, also through
a borrowed thread-safe module: the caller must know that nothing else uses the module of
`tsm` afterwards, as its owner keeps the empty thread-safe module. Using an empty
thread-safe module (calling it, adding it to a JIT, or taking its module again) throws an
`ArgumentError`; disposing of it is fine.

The module still belongs to the context of the thread-safe module, which the caller has to
synchronize accesses to. It can only be used while the thread-safe context of `tsm` is
alive (see [`ThreadSafeContext`](@ref LLVM.ThreadSafeContext)), e.g., inside the do-block
that created it.
"""
function unsafe_take_module!(tsm::ThreadSafeModule)
    @static if version() >= v"16"
        check_has_module(tsm)
        return mark_alloc(Module(API.LLVMExtraThreadSafeModuleTakeModule(tsm)))
    else
        error("Taking the module out of a ThreadSafeModule requires LLVM 16 or later")
    end
end
