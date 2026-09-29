using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes

raw_throwing_module_pass(::LLVM.API.LLVMModuleRef, ::Ptr{Cvoid})::Bool =
    error("exception thrown out of a raw pass callback")

function main()
    Context() do ctx
        mod = LLVM.Module("raw callback longjmp")
        try
            callback = @cfunction(raw_throwing_module_pass, Bool,
                                  (LLVM.API.LLVMModuleRef, Ptr{Cvoid}))
            opts = LLVM.API.LLVMCreatePassBuilderOptions()
            exts = LLVM.API.LLVMCreatePassBuilderExtensions()
            try
                LLVM.API.LLVMPassBuilderExtensionsRegisterModulePass(
                    exts, "raw-throwing-pass", callback, C_NULL)
                LLVM.API.LLVMRunJuliaPasses(mod, "raw-throwing-pass", C_NULL, opts, exts)
            finally
                LLVM.API.LLVMDisposePassBuilderExtensions(exts)
                LLVM.API.LLVMDisposePassBuilderOptions(opts)
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
