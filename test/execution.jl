@testset "execution" begin

@testset "generic values" begin

@dispose ctx=Context() begin
    val = LLVM.GenericValue(LLVM.Int32Type(), -1)
    @test val.intwidth == 32
    @test convert(Int, val) == -1
    dispose(val)
end

@dispose ctx=Context() begin
    val = LLVM.GenericValue(LLVM.Int32Type(), UInt(1))
    @test convert(Int, val) == 1
    @test convert(UInt, val) == 1
    dispose(val)
end

@dispose ctx=Context() begin
    val = LLVM.GenericValue(LLVM.DoubleType(), Float32(1.1))
    @test convert(Float32, LLVM.to_float(val, LLVM.DoubleType())) == Float32(1.1)
    @test LLVM.to_float(val, LLVM.DoubleType()) == Float64(Float32(1.1))
    dispose(val)

    val = LLVM.GenericValue(LLVM.FloatType(), 1.5)
    @test LLVM.to_float(val, LLVM.FloatType()) === 1.5
    # other floating-point types aren't supported by the C API
    @test_throws MethodError LLVM.GenericValue(LLVM.HalfType(), 1.5)
    @test_throws MethodError LLVM.to_float(val, LLVM.FP128Type())
    dispose(val)
end

@dispose ctx=Context() begin
    val = LLVM.GenericValue(LLVM.DoubleType(), 1.1)
    @test convert(Float32, LLVM.to_float(val, LLVM.DoubleType())) == Float32(1.1)
    @test LLVM.to_float(val, LLVM.DoubleType()) == 1.1
    dispose(val)
end

let
    obj = "whatever"
    val = LLVM.GenericValue(pointer(obj))
    @test convert(Ptr{Cvoid}, val) == pointer(obj)
    dispose(val)
end

end


@testset "execution engine" begin

function emit_inc(val)
    mod = LLVM.Module("SomeModule")

    param_types = [LLVM.Int32Type()]
    ret_type = LLVM.FunctionType(LLVM.Int32Type(), param_types)

    sum = LLVM.Function(mod, "add_$(val)", ret_type)

    entry = BasicBlock(sum, "entry")

    @dispose builder=IRBuilder() begin
        position!(builder, LLVM.at_end(entry))

        tmp = add!(builder, sum.parameters[1], ConstantInt(LLVM.Int32Type(), val))
        ret!(builder, tmp)

        verify(mod)
    end

    return mod
end


function emit_phi()
    # if %1 > %2 then %1+2 else %2-5
    mod = LLVM.Module("sommod")
    params = [LLVM.Int32Type(), LLVM.Int32Type()]

    ft = LLVM.FunctionType(LLVM.Int32Type(), params)
    fn = LLVM.Function(mod, "gt", ft)

    entry = BasicBlock(fn, "entry")
    then = BasicBlock(fn, "then")
    elsee = BasicBlock(fn, "else")
    merge = BasicBlock(fn, "ifcont")

    @dispose builder=IRBuilder() begin
        position!(builder, LLVM.at_end(entry))

        cond = LLVM.icmp!(builder, LLVM.API.LLVMIntSGT, fn.parameters[1], fn.parameters[2], "ifcond")
        br!(builder, cond, then, elsee)

        position!(builder, LLVM.at_end(then))
        thencg = add!(builder, fn.parameters[1], ConstantInt(LLVM.Int32Type(), 2))
        br!(builder, merge)

        position!(builder, LLVM.at_end(elsee))
        elsecg = sub!(builder, fn.parameters[2], LLVM.ConstantInt(LLVM.Int32Type(), 5))
        br!(builder, merge)

        position!(builder, LLVM.at_end(merge))
        phi = phi!(builder, LLVM.Int32Type(), "iftmp")

        append!(phi.incoming, [(thencg, then), (elsecg, elsee)])

        @test length(phi.incoming) == 2
        @test_throws BoundsError phi.incoming[3]

        ret!(builder, phi)
    end
    verify(mod)
    return mod
end

@dispose ctx=Context() begin
    mod = emit_inc(1)

    args = [LLVM.GenericValue(LLVM.Int32Type(), 41)]

    let mod = copy(mod)
        engine = LLVM.Interpreter(mod)
        dispose(engine)
    end

    let mod = copy(mod)
        LLVM.Interpreter(mod) do engine
        end
    end

    let mod = copy(mod)
        fn = mod.functions["add_1"]
        @dispose engine=LLVM.Interpreter(mod) begin
            res = LLVM.execute(engine, fn, args)
            @test convert(Int, res) == 42
            dispose(res)
        end
    end

    dispose(mod)
    dispose.(args)
end

@dispose ctx=Context() begin
    let mod = emit_inc(1)
        engine = LLVM.JIT(mod)
        dispose(engine)
    end

    let mod = emit_inc(1)
        LLVM.JIT(mod) do engine
        end
    end

    let mod = emit_inc(1)
        @dispose engine=LLVM.JIT(mod) begin
            addr = lookup(engine, "add_1")
            res = ccall(addr, Int32, (Int32,), 41)
            @test res == 42
        end
    end

    let mod = emit_inc(1)
        @dispose engine=LLVM.JIT(mod; opt_level=LLVM.CodeGenOptLevel.None) begin
            @test ccall(lookup(engine, "add_1"), Int32, (Int32,), 41) == 42
        end
    end

    # the module is consumed, even if creating the engine fails
    let mod = emit_inc(1)
        mod.triple = "unknown-unknown-unknown"
        @test_throws LLVMException LLVM.JIT(mod)
    end
end

@dispose ctx=Context() begin
    args1 = [LLVM.GenericValue(LLVM.Int32Type(), 1),
             LLVM.GenericValue(LLVM.Int32Type(), 2)]

    args2 = [LLVM.GenericValue(LLVM.Int32Type(), 2),
             LLVM.GenericValue(LLVM.Int32Type(), 1)]

    for (args, true_res) in ((args1, -3), (args2, 4))
        let mod = emit_phi()
            fn = mod.functions["gt"]
            @dispose engine=LLVM.Interpreter(mod) begin
                res = LLVM.execute(engine, fn, view(args, :))
                @test convert(Int, res) == true_res
                dispose(res)
            end
        end
        dispose.(args)
    end

    let mod1 = emit_inc(1), mod2 = emit_inc(2)
        @dispose engine=LLVM.JIT(mod1) begin
            @test_throws MethodError collect(engine.functions)
            @test haskey(engine.functions, "add_1")
            @test engine.functions["add_1"] isa LLVM.Function

            @test delete!(engine, mod1) === engine
            @test_throws KeyError engine.functions["add_1"]
            @test !haskey(engine.functions, "add_1")
            # modules that aren't part of the engine are ignored
            @test delete!(engine, mod1) === engine
            dispose(mod1)

            push!(engine, mod2)
            @test haskey(engine.functions, "add_2")
            @test engine.functions["add_2"] isa LLVM.Function

            addr = lookup(engine, "add_2")
            res = ccall(addr, Int32, (Int32,), 40)
            @test res == 42
        end
    end
end

end

@testset "process-wide symbols" begin
    # symbols persist for the remainder of the process, so use names that are unique
    name = "llvmjl_test_symbol_$(getpid())_$(time_ns())"
    @test LLVM.find_symbol(name) == C_NULL
    @test LLVM.find_symbol("malloc") != C_NULL

    # the legacy execution engines resolve external symbols using them
    fptr = @cfunction(abs, Int32, (Int32,))
    @test LLVM.add_symbol(name, fptr) === nothing
    @test LLVM.find_symbol(name) == fptr

    # adding a symbol again replaces its address
    other = "llvmjl_test_symbol_other_$(getpid())_$(time_ns())"
    LLVM.add_symbol(other, Ptr{Cvoid}(1))
    LLVM.add_symbol(other, Ptr{Cvoid}(2))
    @test LLVM.find_symbol(other) == Ptr{Cvoid}(2)
    @dispose ctx=Context() begin
        mod = parse(LLVM.Module, """
            declare i32 @$name(i32)
            define i32 @call(i32 %x) {
              %y = call i32 @$name(i32 %x)
              ret i32 %y
            }""")
        @dispose engine=LLVM.JIT(mod) begin
            @test ccall(lookup(engine, "call"), Int32, (Int32,), -42) == 42
        end
    end

    # loading libraries
    # (which can be done multiple times)
    for _ in 1:2
        @test LLVM.load_library_permanently(String(Base.libllvm_path())) === nothing
    end
    @test_throws LLVMException LLVM.load_library_permanently("/nonexistent/libfoo.so")
end

end
