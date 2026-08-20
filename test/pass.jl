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

    module_attempts = 0
    function throwing_module_pass(cur_mod::LLVM.Module)
        @test cur_mod == mod
        module_attempts += 1
        module_attempts == 1 && throw(ArgumentError("legacy module pass error"))
        return false
    end

    let pass = ModulePass("ThrowingModulePass", throwing_module_pass)
        @dispose mpm=ModulePassManager() begin
            add!(mpm, pass)
            try
                run!(mpm, mod)
                @test false
            catch err
                @test err isa PassException
                @test err.ex isa ArgumentError
                @test occursin("legacy module pass error", string(err.ex))
                @test !isempty(err.processed_bt)
            end
            @test !run!(mpm, mod)
            @test module_attempts == 2
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


    function throwing_function_pass(cur_fn::LLVM.Function)
        @test cur_fn == fn
        error("legacy function pass error")
    end

    let pass = FunctionPass("ThrowingFunctionPass", throwing_function_pass)
        @dispose fpm=FunctionPassManager(mod) begin
            add!(fpm, pass)
            @test !initialize!(fpm)
            @test_throws PassException run!(fpm, fn)
            @test !finalize!(fpm)
        end
    end
end

end
