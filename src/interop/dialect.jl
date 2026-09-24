# The Julia LLVM dialect (julia/src/JuliaDialect.td): attaching the dialect to a
# context, building its ops, and verifying modules that use them.

import ..LLVM: @checked, refcheck, mark_alloc, mark_dispose, unsafe_message,
               dispose, LLVMException

export JuliaDialectContext, verify_dialect, gc_alloc_bytes_size_type,
       get_pgcstack!, get_pgcstack_or_new!, gc_loaded!,
       new_gc_frame!, push_gc_frame!, pop_gc_frame!, get_gc_frame_slot!,
       gc_alloc_bytes!, queue_gc_root!, safepoint!

"""
    JuliaDialectContext(ctx::Context)

Attach the Julia dialect to `ctx`, making it possible to build Julia dialect ops
into modules of that context. The attachment must be disposed with [`dispose`](@ref)
before the context is destroyed. Attachments are refcounted per context, so this
composes with Julia's own code emission and passes attaching to the same context.
"""
@checked mutable struct JuliaDialectContext
    ref::API.JLDialectContextRef
end

Base.unsafe_convert(::Type{API.JLDialectContextRef}, dc::JuliaDialectContext) =
    dc.ref

function JuliaDialectContext(ctx::Context)
    mark_alloc(JuliaDialectContext(API.JLDialectsAttachContext(ctx)))
end

function JuliaDialectContext(f::Core.Function, ctx::Context)
    dc = JuliaDialectContext(ctx)
    try
        f(dc)
    finally
        dispose(dc)
    end
end

function dispose(dc::JuliaDialectContext)
    mark_dispose(API.JLDialectsDisposeContext, dc)
end

"""
    verify_dialect(mod::LLVM.Module)

Verify that `mod` only uses ops of the Julia dialect in a well-formed way.
If verification fails, an exception carrying the verifier diagnostics is thrown.
"""
function verify_dialect(mod::LLVM.Module)
    out_error = Ref{Cstring}()
    status = API.JLDialectsVerifyModule(mod, out_error) |> Bool
    error = unsafe_message(out_error[])

    if status
        throw(LLVMException(error))
    end
end

"""
    gc_alloc_bytes_size_type(mod::LLVM.Module)

The integer type of `julia.gc_alloc_bytes`' size and type-tag arguments, which is
target dependent (the module datalayout's pointer-sized integer). The op verifies
with any integer width, but this is the type the GC lowering itself produces and
the runtime expects.
"""
function gc_alloc_bytes_size_type(mod::LLVM.Module)
    LLVMType(API.JLDialectsGCAllocBytesSizeType(mod))
end

# Op builders. Each of these creates the op at the insertion point of `builder`,
# with the correct types and attributes as specified in JuliaDialect.td. The
# dialect must have been attached to the module's context, either by emitting
# into a context set up by Julia's own codegen or through `JuliaDialectContext`.

get_pgcstack!(builder::IRBuilder) =
    Instruction(API.JLBuildGetPGCStack(builder))

get_pgcstack_or_new!(builder::IRBuilder) =
    Instruction(API.JLBuildGetPGCStackOrNew(builder))

gc_loaded!(builder::IRBuilder, base::Value, tracked::Value) =
    Instruction(API.JLBuildGCLoaded(builder, base, tracked))

new_gc_frame!(builder::IRBuilder, size::Value) =
    Instruction(API.JLBuildNewGCFrame(builder, size))

push_gc_frame!(builder::IRBuilder, frame::Value, size::Value) =
    Instruction(API.JLBuildPushGCFrame(builder, frame, size))

pop_gc_frame!(builder::IRBuilder, frame::Value) =
    Instruction(API.JLBuildPopGCFrame(builder, frame))

get_gc_frame_slot!(builder::IRBuilder, frame::Value, index::Value) =
    Instruction(API.JLBuildGetGCFrameSlot(builder, frame, index))

gc_alloc_bytes!(builder::IRBuilder, ptls::Value, size::Value, type::Value) =
    Instruction(API.JLBuildGCAllocBytes(builder, ptls, size, type))

queue_gc_root!(builder::IRBuilder, root::Value) =
    Instruction(API.JLBuildQueueGCRoot(builder, root))

safepoint!(builder::IRBuilder, signal_page::Value) =
    Instruction(API.JLBuildSafepoint(builder, signal_page))
