@testset "execution" begin

@testset "generic values" begin

@dispose ctx=Context() begin
    val = GenericValue(LLVM.Int32Type(), -1)
    @test intwidth(val) == 32
    @test convert(Int, val) == -1
    dispose(val)
end

@dispose ctx=Context() begin
    val = GenericValue(LLVM.Int32Type(), UInt(1))
    @test convert(Int, val) == 1
    @test convert(UInt, val) == 1
    dispose(val)
end

@dispose ctx=Context() begin
    val = GenericValue(LLVM.DoubleType(), Float32(1.1))
    @test convert(Float32, val, LLVM.DoubleType()) == Float32(1.1)
    @test convert(Float64, val, LLVM.DoubleType()) == Float64(Float32(1.1))
    dispose(val)
end

@dispose ctx=Context() begin
    val = GenericValue(LLVM.DoubleType(), 1.1)
    @test convert(Float32, val, LLVM.DoubleType()) == Float32(1.1)
    @test convert(Float64, val, LLVM.DoubleType()) == 1.1
    dispose(val)
end

let
    obj = "whatever"
    val = GenericValue(pointer(obj))
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
        position!(builder, entry)

        tmp = add!(builder, parameters(sum)[1], ConstantInt(LLVM.Int32Type(), val))
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
        position!(builder, entry)

        cond = LLVM.icmp!(builder, LLVM.API.LLVMIntSGT, parameters(fn)[1], parameters(fn)[2], "ifcond")
        br!(builder, cond, then, elsee)

        position!(builder, then)
        thencg = add!(builder, parameters(fn)[1], ConstantInt(LLVM.Int32Type(), 2))
        br!(builder, merge)

        position!(builder, elsee)
        elsecg = sub!(builder, parameters(fn)[2], LLVM.ConstantInt(LLVM.Int32Type(), 5))
        br!(builder, merge)

        position!(builder, merge)
        phi = phi!(builder, LLVM.Int32Type(), "iftmp")

        append!(incoming(phi), [(thencg, then), (elsecg, elsee)])

        @test length(incoming(phi)) == 2
        @test_throws BoundsError incoming(phi)[3]

        ret!(builder, phi)
    end
    verify(mod)
    return mod
end

@dispose ctx=Context() begin
    mod = emit_inc(1)

    args = [GenericValue(LLVM.Int32Type(), 41)]

    let mod = copy(mod)
        engine = Interpreter(mod)
        dispose(engine)
    end

    let mod = copy(mod)
        Interpreter(mod) do engine
        end
    end

    let mod = copy(mod)
        fn = functions(mod)["add_1"]
        @dispose engine=Interpreter(mod) begin
            res = run(engine, fn, args)
            @test convert(Int, res) == 42
            dispose(res)
        end
    end

    dispose(mod)
    dispose.(args)
end

@dispose ctx=Context() begin
    let mod = emit_inc(1)
        engine = JIT(mod)
        dispose(engine)
    end

    let mod = emit_inc(1)
        JIT(mod) do engine
        end
    end

    let mod = emit_inc(1)
        @dispose engine=JIT(mod) begin
            addr = lookup(engine, "add_1")
            res = ccall(addr, Int32, (Int32,), 41)
            @test res == 42
        end
    end
end

@dispose ctx=Context() begin
    args1 = [GenericValue(LLVM.Int32Type(), 1),
             GenericValue(LLVM.Int32Type(), 2)]

    args2 = [GenericValue(LLVM.Int32Type(), 2),
             GenericValue(LLVM.Int32Type(), 1)]

    for (args, true_res) in ((args1, -3), (args2, 4))
        let mod = emit_phi()
            fn = functions(mod)["gt"]
            @dispose engine=Interpreter(mod) begin
                res = run(engine, fn, args)
                @test convert(Int, res) == true_res
                dispose(res)
            end
        end
        dispose.(args)
    end

    let mod1 = emit_inc(1), mod2 = emit_inc(2)
        @dispose engine=JIT(mod1) begin
            @test_throws ErrorException collect(functions(engine))
            @test haskey(functions(engine), "add_1")
            @test functions(engine)["add_1"] isa LLVM.Function

            delete!(engine, mod1)
            @test_throws KeyError functions(engine)["add_1"]
            @test !haskey(functions(engine), "add_1")
            dispose(mod1)

            push!(engine, mod2)
            @test haskey(functions(engine), "add_2")
            @test functions(engine)["add_2"] isa LLVM.Function

            addr = lookup(engine, "add_2")
            res = ccall(addr, Int32, (Int32,), 40)
            @test res == 42
        end
    end
end

end

end
