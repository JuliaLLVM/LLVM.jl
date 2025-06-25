@testset "passmanager" begin

let
    mpm = ModulePassManager()
    dispose(mpm)
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") mpm=ModulePassManager() begin
    @test !run!(mpm, mod)
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") fpm=FunctionPassManager(mod) begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test !initialize!(fpm)
    @test !run!(fpm, fn)
    @test !finalize!(fpm)
end

end
