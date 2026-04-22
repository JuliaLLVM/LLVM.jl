using LLVM
using Test

# if a test throws within a Context() do block, displaying the LLVM value may crash
# because the context has been disposed already. avoid that by leaking contexts.
LLVM.leak_contexts[] = true

macro check_ir(inst, str)
    quote
        inst = string($(esc(inst)))
        @test occursin($(str), inst)
    end
end
