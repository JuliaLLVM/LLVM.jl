@testset "linker" begin

@dispose ctx=Context() builder=IRBuilder() begin
    mod1 = let
        mod = LLVM.Module("SomeModule")
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)

        mod
    end

    mod2 = let
        mod = LLVM.Module("SomeOtherModule")
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeOtherFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)

        mod
    end

    link!(mod1, mod2)
    @test haskey(functions(mod1), "SomeFunction")
    @test haskey(functions(mod1), "SomeOtherFunction")
    dispose(mod1)
end

end
