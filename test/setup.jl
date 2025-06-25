using LLVM
using Test

# HACK: if a test throws within a Context() do block, displaying the LLVM value may crash
#       because the context has been disposed already. avoid that by disabling `dispose`,
#       and only have it pop the context off the stack (but not destroy it).
LLVM.dispose(ctx::Context) = LLVM.mark_dispose(LLVM.deactivate, ctx)

macro check_ir(inst, str)
    quote
        inst = string($(esc(inst)))
        @test occursin($(str), inst)
    end
end
