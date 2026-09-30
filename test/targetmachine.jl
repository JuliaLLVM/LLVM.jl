@testset "targetmachine" begin

host_triple = LLVM.default_triple()
host_t = LLVM.Target(triple=host_triple)

let
    tm = LLVM.TargetMachine(host_t, host_triple)
    dispose(tm)
end

LLVM.TargetMachine(host_t, host_triple) do tm
end

# the host CPU
@test LLVM.host_cpu_name() isa String
@test !isempty(LLVM.host_cpu_name())
@test LLVM.host_cpu_features() isa String
@dispose tm=LLVM.TargetMachine(host_t, host_triple; cpu=LLVM.host_cpu_name(),
                               features=LLVM.host_cpu_features(),
                               opt_level=LLVM.CodeGenOptLevel.Aggressive) begin
    @test tm.cpu == LLVM.host_cpu_name()
    @test tm.features == LLVM.host_cpu_features()
end
# the CPU and features are keyword arguments
@test_throws MethodError LLVM.TargetMachine(host_t, host_triple, LLVM.host_cpu_name())

LLVM.JITTargetMachine(; cpu=LLVM.host_cpu_name(), opt_level=LLVM.CodeGenOptLevel.None) do tm
    @test tm.cpu == LLVM.host_cpu_name()
end
@test_throws MethodError LLVM.JITTargetMachine(LLVM.default_triple())

@dispose tm=LLVM.TargetMachine(host_t, host_triple) begin
    @test tm.target == host_t
    @test tm.triple == host_triple
    @test tm.cpu == ""
    @test tm.features == ""
    LLVM.asm_verbosity!(tm, true)

    # emission
    @dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, LLVM.at_end(entry))

        ret!(builder)

        asm = String(convert(Vector{UInt8}, LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile)))

        mktemp() do path, io
            LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile, path)
            @test asm == read(path, String)
        end

        @test_throws LLVMException LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile, "/")
    end

    dispose(LLVM.DataLayout(tm))
end

end
