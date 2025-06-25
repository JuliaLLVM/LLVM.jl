@testset "pass" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    bb = BasicBlock(fn, "SomeBasicBlock")
    position!(builder, bb)

    ret!(builder)

    verify(mod)


    # module pass

    let mpm = ModulePassManager()
        dispose(mpm)
    end

    ModulePassManager() do mpm
    end

    function runOnModule(cur_mod::LLVM.Module)
        @test cur_mod == mod
        return false
    end

    let pass = ModulePass("SomeModulePass", runOnModule)
        @dispose mpm=ModulePassManager() begin
            add!(mpm, pass)
            run!(mpm, mod)
        end
    end

    # function pass

    let fpm = FunctionPassManager(mod)
        dispose(fpm)
    end

    FunctionPassManager(mod) do fpm
    end

    function runOnFunction(cur_fn::LLVM.Function)
        @test cur_fn == fn
        return false
    end

    let pass = FunctionPass("SomeFunctionPass", runOnFunction)
        @dispose fpm=FunctionPassManager(mod) begin
            add!(fpm, pass)
            run!(fpm, fn)
        end
    end
end

end
