@testset "targetmachine" begin

host_triple = LLVM.default_triple()
host_t = LLVM.Target(triple=host_triple)

let
    tm = LLVM.TargetMachine(host_t, host_triple)
    dispose(tm)
end

LLVM.TargetMachine(host_t, host_triple) do tm
end

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
        position!(builder, entry)

        ret!(builder)

        asm = String(convert(Vector{UInt8}, emit(tm, mod, LLVM.API.LLVMAssemblyFile)))

        mktemp() do path, io
            emit(tm, mod, LLVM.API.LLVMAssemblyFile, path)
            @test asm == read(path, String)
        end

        @test_throws LLVMException emit(tm, mod, LLVM.API.LLVMAssemblyFile, "/")
    end

    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        @dispose fpm=FunctionPassManager(mod) begin
            add_transform_info!(fpm)
            add_transform_info!(fpm, tm)
            add_library_info!(fpm, tm.triple)
        end
        @dispose mpm=ModulePassManager() begin
            add_transform_info!(mpm)
            add_transform_info!(mpm, tm)
            add_library_info!(mpm, tm.triple)
        end
    end

    dispose(LLVM.DataLayout(tm))
end

end
