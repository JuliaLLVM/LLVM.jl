using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes

raw_throwing_module_pass(::LLVM.API.LLVMModuleRef, ::Ptr{Cvoid})::Bool =
    error("exception thrown out of a raw pass callback")

function main()
    Context() do ctx
        mod = LLVM.Module("raw callback longjmp")
        try
            callback = @cfunction(raw_throwing_module_pass, Bool,
                                  (LLVM.API.LLVMModuleRef, Ptr{Cvoid}))
            pb = NewPMPassBuilder()
            try
                LLVM.API.LLVMPassBuilderExtensionsRegisterModulePass(
                    pb.exts, "raw-throwing-pass", callback, C_NULL)
                add!(pb, "raw-throwing-pass")
                run!(pb, mod)
            finally
                dispose(pb)
            end
            error("raw pass callback did not throw")
        catch err
            err isa ErrorException || rethrow()
            err.msg == "exception thrown out of a raw pass callback" || rethrow()
        end

        # StandardInstrumentations must retain valid storage after the skipped
        # C++ cleanup so a later pass run does not use dangling global state.
        run!("no-op-module", mod)
        dispose(mod)
    end
end

main()
