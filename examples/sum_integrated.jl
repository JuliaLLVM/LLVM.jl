# same as `sum.jl`, but reusing the Julia compiler to compile and execute the IR

using Test

using LLVM, LLVM.IR, LLVM.Build
using LLVM.Interop

if length(ARGS) == 2
    x, y = parse.([Int32], ARGS[1:2])
else
    x = Int32(1)
    y = Int32(2)
end

# generate IR, and make Julia compile and execute it
@eval call_sum(x, y) = $(generate_llvmcall(Int32, Tuple{Int32, Int32}, :x, :y) do builder, x, y
    add!(builder, x, y, "tmp")
end)

@test call_sum(x, y) == x + y
