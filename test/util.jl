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
            tmp = add!(builder, f.parameters[1], f.parameters[2], "tmp")
            tmp2 = load!(builder, LLVM.Int32Type(), ptr)
            tmp3 = add!(builder, tmp, tmp2)
            ret!(builder, tmp3)

            verify(mod)
        end

        # basic clone
        let new_f = clone(f)
            @test new_f != f
            @test new_f.value_type == f.value_type
            for (bb1, bb2) in zip(f.blocks, new_f.blocks)
                for (inst1, inst2) in zip(bb2.instructions, bb2.instructions)
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
                f.parameters[1] => new_f.parameters[2],
                f.parameters[2] => new_f.parameters[3]
            )
            clone_into!(new_f, f; value_map)

            # operands of the add instruction should have been remapped
            add = first(first(new_f.blocks).instructions)
            @test add.operands[1] == new_f.parameters[2]
            @test add.operands[2] == new_f.parameters[3]
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
                                            zip(f.parameters, new_f.parameters))

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
            add = first(first(new_f.blocks).instructions)
            @test add.operands[1].value_type == LLVM.Int64Type()
            @test add.operands[2].value_type == LLVM.Int64Type()
            @test add.value_type == LLVM.Int64Type()
        end

        let new_f = LLVM.Function(mod, "type_mapper_error", fun_type)
            value_map = Dict{LLVM.Value, LLVM.Value}(
                old_param => new_param for (old_param, new_param) in
                                            zip(f.parameters, new_f.parameters))

            err = try
                clone_into!(new_f, f; value_map,
                            type_mapper=_ -> throw(ArgumentError("type mapper error")))
                nothing
            catch err
                err
            end
            @test err isa LLVM.CallbackException
            @test err.ex isa ArgumentError
            @test occursin("type mapper error", string(err.ex))
            @test !isempty(err.processed_bt)
            verify(mod)
        end

        let new_f = LLVM.Function(mod, "materializer_error", fun_type)
            value_map = Dict{LLVM.Value, LLVM.Value}(
                old_param => new_param for (old_param, new_param) in
                                            zip(f.parameters, new_f.parameters))

            err = try
                clone_into!(new_f, f; value_map,
                            materializer=_ -> throw(ArgumentError("materializer error")))
                nothing
            catch err
                err
            end
            @test err isa LLVM.CallbackException
            @test err.ex isa ArgumentError
            @test occursin("materializer error", string(err.ex))
            @test !isempty(err.processed_bt)
            verify(mod)
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
            src = mod.functions["add"]
            value_map = Dict(
                src.parameters[1] => ConstantInt(42)
            );
            dst = clone(src; value_map)
            @test length(dst.parameters) == 1
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
        f = mod.functions["foo"]
        bb = f.blocks[2]
        add = first(bb.instructions)

        # clone a basic block, providing a suffix
        let bb_clone = clone(bb; suffix="_clone")
            @test bb_clone.parent == f
            @test bb_clone.name == "doit_clone"

            # we should have remapped instructions in the basic block
            inst_clone = collect(bb_clone.instructions)
            add_clone = inst_clone[1]
            call_clone = inst_clone[2]
            @test first(call_clone.operands) == add_clone
        end

        # clone, mapping values
        arg1 = f.parameters[2]
        arg2 = f.parameters[3]
        value_map = Dict{Value,Value}(arg1 => arg2)
        let bb_clone = clone(bb; value_map)
            # test that we've remapped the argument
            add_clone = first(bb_clone.instructions)
            @test add_clone.operands[1] == arg2
        end

        # clone into a different function
        f2 = mod.functions["baz"]
        value_map = Dict{Value,Value}(arg1 => only(f2.parameters))
        let bb_clone = clone(bb; dest=f2, value_map)
            # make sure we don't refer anything from the original function
            verify(f2)
        end

        # clone without inserting the block into a function
        let bb_clone = clone(bb; dest=nothing)
            @test bb_clone.parent === nothing
            @test first(collect(bb_clone.instructions)[2].operands) ==
                  first(bb_clone.instructions)
            erase!(bb_clone)
        end

        dispose(mod)

        # debug records in cloned blocks refer to the cloned instructions
        if LLVM.version() >= v"19"
            mod = parse(LLVM.Module, """
                define void @f(i32 %x) !dbg !5 {
                entry:
                  br label %body
                body:
                  %a = add i32 %x, 1
                    #dbg_value(i32 %a, !9, !DIExpression(), !10)
                  ret void, !dbg !10
                }

                !llvm.dbg.cu = !{!0}
                !llvm.module.flags = !{!3}

                !0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, emissionKind: FullDebug)
                !1 = !DIFile(filename: "test.c", directory: "/tmp")
                !3 = !{i32 2, !"Debug Info Version", i32 3}
                !5 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !6, unit: !0, spFlags: DISPFlagDefinition)
                !6 = !DISubroutineType(types: !7)
                !7 = !{null}
                !8 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
                !9 = !DILocalVariable(name: "a", scope: !5, file: !1, line: 2, type: !8)
                !10 = !DILocation(line: 2, column: 1, scope: !5)
                """)
            body = mod.functions["f"].blocks[2]
            detached = clone(body; dest=nothing)
            # also when cloning a block that isn't part of a function itself
            detached2 = clone(detached; dest=nothing)
            for bb in (detached, detached2)
                a, ret = bb.instructions
                @test only(collect(ret.debug_records)).value == a
            end
            erase!(detached2)
            erase!(detached)
            dispose(mod)
        end
    end

end

end
