# same as `sum.jl`, but using the OrcJIT to compile the code and executing it via ccall.

using Test

using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

if length(ARGS) == 2
    x, y = parse.([Int32], ARGS[1:2])
else
    x = Int32(1)
    y = Int32(2)
end

function codegen!(mod::LLVM.Module, name, tm)
    param_types = [LLVM.Int32Type(), LLVM.Int32Type()]
    ret_type = LLVM.Int32Type()

    mod.triple = tm.triple

    ft = LLVM.FunctionType(ret_type, param_types)
    sum = LLVM.Function(mod, name, ft)

    # generate IR
    @dispose builder=IRBuilder() begin
        entry = BasicBlock(sum, "entry")
        position!(builder, entry)

        tmp = add!(builder, sum.parameters[1], sum.parameters[2], "tmp")
        ret!(builder, tmp)
    end

    verify(mod)

end

tm = LLVM.JITTargetMachine()
# XXX: LLJIT calls TargetMachineBuilder which disposes the TargetMachine
jit = LLJIT(; tm=LLVM.JITTargetMachine())

@dispose ts_ctx=ThreadSafeContext() begin
    ts_mod = ThreadSafeModule("jit")
    name = "sum_orc.jl"
    ts_mod() do mod
        codegen!(mod, name, tm)
    end

    jd = jit.main_dylib
    add!(jit, jd, ts_mod)
    addr = lookup(jit, name)

    @eval call_sum(x, y) = ccall($(pointer(addr)), Int32, (Int32, Int32), x, y)
end

@test call_sum(x, y) == x + y
LLVM.dispose(jit)
LLVM.dispose(tm)
