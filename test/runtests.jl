using LLVM

using InteractiveUtils
@info "System information:\n" * sprint(io->versioninfo(io))

ctx_typed_pointers = Context() do ctx
    supports_typed_pointers(ctx)
end
julia_typed_pointers = let
    ir = sprint(io->code_llvm(io, unsafe_load, Tuple{Ptr{Int}}))
    if occursin(r"load i64, i64\* .+, align 1", ir)
        true
    elseif occursin(r"load i64, ptr .+, align 1", ir)
        false
    else
        error("could not determine whether Julia uses typed pointers")
    end
end
@info "Pointer settings: Julia uses $(julia_typed_pointers ? "typed" : "opaque") pointers, default contexts use $(ctx_typed_pointers ? "typed" : "opaque") pointers"

@info "Debug settings: typecheck = $(LLVM.typecheck_enabled), memcheck = $(LLVM.memcheck_enabled)"

worker_init_expr = quote
    using LLVM

    # HACK: if a test throws within a Context() do block, displaying the LLVM value may
    #       crash because the context has been disposed already. avoid that by disabling
    #       `dispose`, and only have it pop the context off the stack (but not destroy it).
    LLVM.dispose(ctx::Context) = LLVM.mark_dispose(LLVM.deactivate, ctx)
end

using ReTestItems
runtests(LLVM; worker_init_expr, nworkers=min(Sys.CPU_THREADS,4), nworker_threads=1,
               testitem_timeout=120) do ti
    if ti.name == "jljit"
        # XXX: hangs on Windows
        Sys.iswindows() && return false
    end

    if ti.name == "transform"
        LLVM.has_oldpm() || return false
    end

    true
end
