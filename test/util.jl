@testset "utils" begin

@testset "function cloning" begin
    @dispose ctx=Context() mod=LLVM.Module("my_module") begin
        # set-up
        param_types = [LLVM.Int32Type(), LLVM.Int32Type()]
        ret_type = LLVM.Int32Type()
        fun_type = LLVM.FunctionType(ret_type, param_types)
        f = LLVM.Function(mod, "f", fun_type)

        # generate IR
        @dispose builder=IRBuilder() begin
            entry = BasicBlock(f, "entry")
            position!(builder, entry)
            ptr = const_inttoptr(
                ConstantInt(0xdeadbeef%UInt),
                LLVM.PointerType(LLVM.Int32Type()))
            tmp = add!(builder, parameters(f)[1], parameters(f)[2], "tmp")
            tmp2 = load!(builder, LLVM.Int32Type(), ptr)
            tmp3 = add!(builder, tmp, tmp2)
            ret!(builder, tmp3)

            verify(mod)
        end

        # basic clone
        let new_f = clone(f)
            @test new_f != f
            @test value_type(new_f) == value_type(f)
            for (bb1, bb2) in zip(blocks(f), blocks(new_f))
                for (inst1, inst2) in zip(instructions(bb2), instructions(bb2))
                    @test inst1 == inst2
                end
            end
        end

        # clone into, testing the value mapper (adding an argument)
        let
            new_param_types = [LLVM.Int32Type(), LLVM.Int32Type(), LLVM.Int32Type()]
            new_fun_type = LLVM.FunctionType(ret_type, new_param_types)
            new_f = LLVM.Function(mod, "new", new_fun_type)

            value_map = Dict{LLVM.Value, LLVM.Value}(
                parameters(f)[1] => parameters(new_f)[2],
                parameters(f)[2] => parameters(new_f)[3]
            )
            clone_into!(new_f, f; value_map)

            # operands of the add instruction should have been remapped
            add = first(instructions(first(blocks(new_f))))
            @test operands(add)[1] == parameters(new_f)[2]
            @test operands(add)[2] == parameters(new_f)[3]
        end

        # clone into, testing the type remapper (changing precision)
        let
            new_param_types = [LLVM.Int64Type(), LLVM.Int64Type()]
            new_ret_type = LLVM.Int64Type()
            new_fun_type = LLVM.FunctionType(new_ret_type, new_param_types)
            new_f = LLVM.Function(mod, "new", new_fun_type)

            # we always need to map all arguments
            value_map = Dict{LLVM.Value, LLVM.Value}(
                old_param => new_param for (old_param, new_param) in
                                            zip(parameters(f), parameters(new_f)))

            function type_mapper(typ)
                if typ == LLVM.Int32Type()
                    LLVM.Int64Type()
                else
                    typ
                end
            end
            function materializer(val)
                if val isa Union{LLVM.ConstantExpr, ConstantInt}
                    # test that we can return nothing
                    return nothing
                end
                # not needed here
                error("")
            end
            clone_into!(new_f, f; value_map, type_mapper, materializer)

            # the add should now be a 64-bit addition
            add = first(instructions(first(blocks(new_f))))
            @test value_type(operands(add)[1]) == LLVM.Int64Type()
            @test value_type(operands(add)[2]) == LLVM.Int64Type()
            @test value_type(add) == LLVM.Int64Type()
        end
    end

    # bug in basic clone: mapped parameters were incorrect
    let
        ir = """
            define i64 @"add"(i64 %0, i64 %1) {
            top:
                %2 = add i64 %1, %0
                ret i64 %2
            }""";
        @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
            src = functions(mod)["add"]
            value_map = Dict(
                parameters(src)[1] => ConstantInt(42)
            );
            dst = clone(src; value_map)
            @test length(parameters(dst)) == 1
        end
    end
end

@testset "basic block cloning" begin
    @dispose ctx=Context() begin
        # set-up
        ir = """
            declare void @bar(i8);

            define void @foo(i1 %cond, i8 %arg1, i8 %arg2) {
            entry:
                br i1 %cond, label %doit, label %cont

            doit:
                %val = add i8 %arg1, 1
                call void @bar(i8 %val)
                ret void

            cont:
                ret void
            }

            declare void @baz(i8 %val)
            """
        mod = parse(LLVM.Module, ir)
        f = functions(mod)["foo"]
        bb = blocks(f)[2]
        add = first(instructions(bb))

        # clone a basic block, providing a suffix
        let bb_clone = clone(bb; suffix="_clone")
            @test LLVM.parent(bb_clone) == f
            @test LLVM.name(bb_clone) == "doit_clone"

            # we should have remapped instructions in the basic block
            inst_clone = collect(instructions(bb_clone))
            add_clone = inst_clone[1]
            call_clone = inst_clone[2]
            @test first(operands(call_clone)) == add_clone
        end

        # clone, mapping values
        arg1 = parameters(f)[2]
        arg2 = parameters(f)[3]
        value_map = Dict{Value,Value}(arg1 => arg2)
        let bb_clone = clone(bb; value_map)
            # test that we've remapped the argument
            add_clone = first(instructions(bb_clone))
            @test operands(add_clone)[1] == arg2
        end

        # clone into a different function
        f2 = functions(mod)["baz"]
        value_map = Dict{Value,Value}(arg1 => only(parameters(f2)))
        let bb_clone = clone(bb; dest=f2, value_map)
            # make sure we don't refer anything from the original function
            verify(f2)
        end

        dispose(mod)
    end

end

end
