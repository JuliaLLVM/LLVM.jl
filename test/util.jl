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
            position!(builder, LLVM.at_end(entry))
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
            # the type mapper failed before anything was cloned
            @test isempty(new_f.blocks)
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

@testset "value maps" begin
    @dispose ctx=Context() mod=LLVM.Module("my_module") vmap=ValueMap() begin
        a = GlobalVariable(mod, LLVM.Int32Type(), "a")
        b = GlobalVariable(mod, LLVM.Int32Type(), "b")
        c = GlobalVariable(mod, LLVM.Int32Type(), "c")

        @test isempty(vmap)
        vmap[a] = b
        @test length(vmap) == 1
        @test vmap[a] == b
        @test haskey(vmap, a)
        @test !haskey(vmap, b)
        @test get(vmap, b, nothing) === nothing
        @test_throws KeyError vmap[b]
        @test collect(vmap) == [a => b]

        # entries follow their key when it is replaced, and are removed when it is deleted
        replace_uses!(a, c)
        @test !haskey(vmap, a)
        @test vmap[c] == b
        erase!(c)
        @test isempty(vmap)

        vmap[a] = b
        delete!(vmap, a)
        @test isempty(vmap)
        vmap[a] = b
        empty!(vmap)
        @test isempty(vmap)

        # metadata
        md1 = MDNode([MDString("1")])
        md2 = MDNode([MDString("2")])
        @test isempty(vmap.metadata)
        vmap.metadata[md1] = md2
        @test vmap.metadata[md1] == md2
        @test haskey(vmap.metadata, md1)
        @test !haskey(vmap.metadata, md2)
        @test collect(vmap.metadata) == [md1 => md2]
        @test isempty(vmap)
        delete!(vmap.metadata, md1)
        @test isempty(vmap.metadata)
    end
end

@testset "value mapping" begin
    @dispose ctx=Context(; opaque_pointers=true) mod=LLVM.Module("my_module") begin
        T_ptr1 = LLVM.PointerType(1)
        a = GlobalVariable(mod, LLVM.Int64Type(), "a", 1)
        b = GlobalVariable(mod, LLVM.Int64Type(), "b")
        ce = const_ptrtoint(a, LLVM.Int64Type())

        # values in the map, and constants that use them
        ValueMap() do vmap
            vmap[a] = b
            @test map_value(a, vmap) == b
            new = map_value(ce, vmap)
            @test new isa ConstantExpr
            @test new.operands[1] == b
            # the result is added to the map
            @test vmap[ce] == new

            # other global values are mapped to themselves, unless requested otherwise
            @test map_value(b, vmap) == b
            c = GlobalVariable(mod, LLVM.Int64Type(), "c")
            @test map_value(c, vmap; null_map_missing_globals=true) === nothing
        end

        # other values can't be mapped
        fn = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int64Type()]))
        ValueMap() do vmap
            @test map_value(fn.parameters[1], vmap) === nothing
        end

        # the type mapper and materializer
        ValueMap() do vmap
            materialized = Value[]
            function materializer(val)
                push!(materialized, val)
                return val == a ? b : nothing
            end
            arr = ConstantArray(LLVM.Int64Type(), [ce, ce])
            function type_mapper(typ)
                typ == LLVM.Int64Type() && return LLVM.Int32Type()
                typ isa LLVM.ArrayType && return LLVM.ArrayType(type_mapper(typ.element_type),
                                                                typ.length)
                return typ
            end
            new = map_value(arr, vmap; materializer, type_mapper)
            @test new.value_type == LLVM.ArrayType(LLVM.Int32Type(), 2)
            @test new.operands[1].opcode == LLVM.Opcode.PtrToInt
            @test new.operands[1].operands[1] == b
            @test arr in materialized && a in materialized
        end

        # a materializer that maps values itself, using the same map
        ValueMap() do vmap
            vmap[a] = b
            function materializer(val)
                val isa ConstantExpr || return nothing
                ops = Constant[map_value(op, vmap; materializer) for op in val.operands]
                return with_operands(val, ops; type=LLVM.Int32Type())
            end
            new = map_value(ce, vmap; materializer)
            @test new.value_type == LLVM.Int32Type()
            @test new.operands[1] == b
        end

        # exceptions thrown by the callbacks
        ValueMap() do vmap
            err = try
                map_value(ce, vmap; materializer=_ -> error("materializer error"))
                nothing
            catch err
                err
            end
            @test err isa LLVM.CallbackException
            @test occursin("materializer error", sprint(showerror, err))
        end
        ValueMap() do vmap
            err = try
                map_value(ce, vmap; type_mapper=_ -> error("type mapper error"))
                nothing
            catch err
                err
            end
            @test err isa LLVM.CallbackException
        end

        # a failing callback doesn't make LLVM rebuild constants from a mix of mapped and
        # unmapped operands (which an assertions build of LLVM would abort on)
        function wellformed(val)
            val isa Constant && !(val isa GlobalValue) || return true
            typ = val.value_type
            ops = collect(val.operands)
            if val isa ConstantArray
                all(op -> op.value_type == typ.element_type, ops) || return false
            elseif val isa ConstantStruct
                ops_types = [op.value_type for op in ops]
                ops_types == collect(typ.elements) || return false
            end
            return all(wellformed, ops)
        end
        b1 = const_addrspacecast(b, T_ptr1)
        function type_mapper(typ)
            typ isa LLVM.PointerType && return LLVM.PointerType()
            typ isa LLVM.ArrayType && return LLVM.ArrayType(type_mapper(typ.element_type),
                                                            typ.length)
            return typ
        end
        ## the type mapper fails for an operand of a constant whose type it maps
        ValueMap() do vmap
            arr = ConstantArray(T_ptr1, [b1])
            failing(typ) = typ == T_ptr1 ? error("type mapper error") : type_mapper(typ)
            @test_throws LLVM.CallbackException map_value(arr, vmap; type_mapper=failing)
            @test all(((_, val),) -> wellformed(val), vmap)
            @test isempty(vmap)
        end
        ValueMap() do vmap
            inner = ConstantArray(T_ptr1, [b1, b1])
            arr = ConstantArray(inner.value_type, [inner])
            function failing(typ)
                typ == arr.value_type && return type_mapper(typ)
                typ isa LLVM.ArrayType && error("type mapper error")
                return type_mapper(typ)
            end
            @test_throws LLVM.CallbackException map_value(arr, vmap; type_mapper=failing)
            @test all(((_, val),) -> wellformed(val), vmap)
            @test isempty(vmap)
        end
        ## the materializer fails for a nested operand
        ValueMap() do vmap
            vmap[a] = b
            gep = const_inbounds_gep(LLVM.Int8Type(), a, [ConstantInt(Int64(1))])
            inner = ConstantArray(T_ptr1, [b1, gep, b1])
            arr = ConstantArray(inner.value_type, [inner, inner])
            # like RemoveAddrspaces, fold `b1`: LLVM can't rebuild it as a cast to `ptr`
            function materializer(val)
                val == gep && error("materializer error")
                val == b1 && return b
                return nothing
            end
            @test_throws LLVM.CallbackException map_value(arr, vmap; type_mapper,
                                                         materializer)
            @test all(((_, val),) -> wellformed(val), vmap)
            @test vmap[arr].value_type == type_mapper(arr.value_type)
            @test vmap[gep] isa PoisonValue
        end

        # constants that refer to a global value that LLVM requires to map to one: when
        # mapping fails, they are mapped to poison as a whole, instead of the global value
        let ir = """
                @b = global i64 0
                define void @f1() addrspace(1) {
                entry:
                  br label %bb
                bb:
                  ret void
                }
                @arr = global [2 x ptr addrspace(1)] [ptr addrspace(1) addrspacecast (ptr @b to ptr addrspace(1)), ptr addrspace(1) blockaddress(@f1, %bb)]"""
            other = parse(LLVM.Module, ir)
            arr = other.globals["arr"].initializer
            e1 = arr.operands[1]
            f1 = other.functions["f1"]
            # after another value failed, and when the global value fails
            for failing in (e1, f1)
                ValueMap() do vmap
                    function materializer(val)
                        val == failing && error("materializer error")
                        # fold the cast, which LLVM can't rebuild as a cast to `ptr`
                        val == e1 && return other.globals["b"]
                        return nothing
                    end
                    @test_throws LLVM.CallbackException map_value(arr, vmap; type_mapper,
                                                                 materializer)
                    new = vmap[arr]
                    @test wellformed(new)
                    @test new.value_type == LLVM.ArrayType(LLVM.PointerType(), 2)
                    # (an array of poison values is folded into poison)
                    @test new isa PoisonValue || new.operands[2] isa PoisonValue
                end
            end

            # their global value and block are walked, e.g., to drop entries for deleted
            # values (the block address is not an operand in every version of LLVM)
            ba = arr.operands[2]
            ValueMap() do vmap
                deleted = LLVM.Function(other, "deleted", f1.function_type)
                vmap[f1] = deleted
                erase!(deleted)
                @test map_value(ba, vmap) == ba
            end
            dispose(other)
        end

        # references to global values, which LLVM.jl has to wrap for the materializer
        let ir = """
                declare void @h()
                define ptr @k() {
                  call void dso_local_equivalent @h()
                  ret ptr no_cfi @h
                }
                @dso = global ptr dso_local_equivalent @h
                @nocfi = global ptr no_cfi @h"""
            other = parse(LLVM.Module, ir)
            h = other.functions["h"]
            h2 = LLVM.Function(other, "h2", h.function_type)
            for name in ("dso", "nocfi")
                init = other.globals[name].initializer
                ValueMap() do vmap
                    vmap[h] = h2
                    new = map_value(init, vmap; type_mapper=identity,
                                    materializer=_ -> nothing)
                    @test typeof(new) == typeof(init)
                    @test new.global_value == h2
                end
            end
            k = other.functions["k"]
            k2 = LLVM.Function(other, "k2", k.function_type)
            ValueMap() do vmap
                vmap[h] = h2
                clone_into!(k2, k; value_map=vmap, type_mapper=identity,
                            materializer=_ -> nothing)
            end
            insts = collect(k2.entry.instructions)
            @test insts[1].called_operand isa DSOLocalEquivalent
            @test insts[1].called_operand.global_value == h2
            @test insts[2].operands[1] isa NoCFIValue
            @test insts[2].operands[1].global_value == h2
            @test verify(other) === nothing
            dispose(other)
        end

        # inline assembly is mapped with its function type
        let ir = """
                define void @f(ptr %p) {
                  call void asm sideeffect "", "r"(ptr %p)
                  ret void
                }"""
            other = parse(LLVM.Module, ir)
            call = first(other.functions["f"].entry.instructions)
            inline_asm = call.called_operand
            ValueMap() do vmap
                failing(typ) = typ isa LLVM.FunctionType ? error("type mapper error") : typ
                @test_throws LLVM.CallbackException map_value(inline_asm, vmap;
                                                             type_mapper=failing)
                @test isempty(vmap)
            end
            dispose(other)
        end

        # entries whose value was deleted are not used, also for values that refer to them
        for val in (ce, a)
            ValueMap() do vmap
                c = GlobalVariable(mod, LLVM.Int64Type(), "deleted", 1)
                vmap[a] = c
                erase!(c)
                @test !haskey(vmap, a)
                @test map_value(val, vmap) == val
            end
        end

        ## the type mapper fails for the type of a global value (which LLVM maps to itself,
        ## but which is mapped to poison when the materializer fails)
        ValueMap() do vmap
            g3 = GlobalVariable(mod, LLVM.Int64Type(), "g3", 3)
            failing(typ) = typ == LLVM.PointerType(3) ? error("type mapper error") : typ
            @test_throws LLVM.CallbackException map_value(const_ptrtoint(g3, LLVM.Int64Type()),
                                                         vmap; type_mapper=failing)
            @test isempty(vmap)
        end
        ValueMap() do vmap
            g3 = GlobalVariable(mod, LLVM.Int64Type(), "g3b", 3)
            e1 = const_ptrtoint(b, LLVM.Int64Type())
            st = ConstantStruct([e1, g3])
            new_st = LLVM.StructType([LLVM.Int32Type(), LLVM.PointerType()])
            function mapper(typ)
                typ == st.value_type && return new_st
                typ == LLVM.Int64Type() && return LLVM.Int32Type()
                typ isa LLVM.PointerType && return LLVM.PointerType()
                return typ
            end
            materializer(val) = val == e1 ? error("materializer error") : nothing
            @test_throws LLVM.CallbackException map_value(st, vmap; type_mapper=mapper,
                                                         materializer)
            new = vmap[st]
            @test wellformed(new)
            @test new.value_type == new_st
            @test all(op -> op isa PoisonValue, new.operands)
        end

        # metadata
        ValueMap() do vmap
            vmap[a] = b
            md = MDNode([Metadata(a), MDString("x")])
            new = map_metadata(md, vmap)
            @test new != md
            @test Value(new.operands[1]) == b
            @test new.operands[2] == md.operands[2]
            @test vmap.metadata[md] == new

            # distinct nodes are duplicated, unless they are mapped to themselves
            other = parse(LLVM.Module, """
                !n = !{!0}
                !0 = distinct !{!"y"}""")
            dmd = only(other.metadata["n"].operands)
            dup = map_metadata(dmd, vmap)
            @test dup != dmd
            empty!(vmap.metadata)
            vmap.metadata[dmd] = dmd
            @test map_metadata(dmd, vmap) == dmd
            empty!(vmap.metadata)
            @test map_metadata(dmd, vmap; reuse_and_mutate_distinct_mds=true) == dmd
            dispose(other)
        end
    end
end

@testset "what function cloning maps" begin
    # the type mapper is called before anything is cloned for all the types that LLVM maps,
    # and entries for deleted values are not used, also in parts of the function other
    # than instructions (see `prepare_function!`)
    ir = """
        @g = global i64 0
        @g1 = addrspace(1) global i64 0

        declare void @h(i16)
        declare void @llvm.dbg.value(metadata, metadata, metadata)

        define void @gep(ptr %p) {
          %q = getelementptr {i32, [2 x i16]}, ptr %p, i32 0, i32 1
          ret void
        }

        define void @call(i16 %a) {
          call void @h(i16 %a)
          ret void
        }

        define void @asm(ptr %p) {
          call void asm sideeffect "", "r"(ptr %p)
          ret void
        }

        define void @prefix() prefix [1 x ptr addrspace(2)] [ptr addrspace(2) addrspacecast (ptr @g to ptr addrspace(2))] {
          ret void
        }

        define void @dbg() !dbg !4 {
          call void @llvm.dbg.value(metadata ptr @g, metadata !7, metadata !DIExpression()), !dbg !9
          call void @llvm.dbg.value(metadata !DIArgList(ptr @g, ptr @g), metadata !7, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_plus)), !dbg !9
          ret void
        }

        define void @dbg_aggregate() !dbg !10 {
          call void @llvm.dbg.value(metadata [1 x ptr addrspace(1)] [ptr addrspace(1) @g1], metadata !11, metadata !DIExpression()), !dbg !12
          ret void
        }

        !llvm.dbg.cu = !{!1}
        !llvm.module.flags = !{!3}

        !1 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !2, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
        !2 = !DIFile(filename: "f.jl", directory: ".")
        !3 = !{i32 2, !"Debug Info Version", i32 3}
        !4 = distinct !DISubprogram(name: "dbg", scope: !2, file: !2, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !1)
        !5 = !DISubroutineType(types: !6)
        !6 = !{}
        !7 = !DILocalVariable(name: "x", scope: !4, file: !2, line: 1, type: !8)
        !8 = !DIBasicType(name: "Int64", size: 64, encoding: DW_ATE_signed)
        !9 = !DILocation(line: 1, scope: !4)
        !10 = distinct !DISubprogram(name: "dbg_aggregate", scope: !2, file: !2, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !1)
        !11 = !DILocalVariable(name: "y", scope: !10, file: !2, line: 1, type: !8)
        !12 = !DILocation(line: 1, scope: !10)"""
    @dispose ctx=Context(; opaque_pointers=true) mod=parse(LLVM.Module, ir) begin
        function clone_with(name, type_mapper; seed=Returns(nothing))
            f = mod.functions[name]
            new = LLVM.Function(mod, name * ".new", f.function_type)
            ValueMap() do vmap
                seed(vmap)
                for (arg, new_arg) in zip(f.parameters, new.parameters)
                    vmap[arg] = new_arg
                end
                clone_into!(new, f; value_map=vmap, type_mapper,
                            changes=LLVM.CloneFunctionChangeType.GlobalChanges)
            end
            return new
        end
        function fails_early(name, failing_type)
            failing(typ) = typ == failing_type ? error("type mapper error") : typ
            new = nothing
            err = try
                clone_with(name, failing)
                nothing
            catch err
                err
            end
            new = mod.functions[name * ".new"]
            ok = err isa LLVM.CallbackException && isempty(new.blocks) &&
                 !occursin(") prefix ", string(new))
            erase!(new)
            return ok
        end

        # the result element type of a GEP
        @test fails_early("gep", LLVM.ArrayType(LLVM.Int16Type(), 2))
        # the parameter types of a call
        @test fails_early("call", LLVM.Int16Type())
        # the function type of inline assembly
        @test fails_early("asm", LLVM.FunctionType(LLVM.VoidType(), [LLVM.PointerType()]))
        # prefix data
        @test fails_early("prefix", LLVM.PointerType(2))
        # constants in debug values (records since LLVM 19, intrinsics before)
        @test fails_early("dbg_aggregate", LLVM.ArrayType(LLVM.PointerType(1), 1))

        # entries for deleted values in prefix data and debug values are not used
        g = mod.globals["g"]
        function seed(vmap)
            deleted = GlobalVariable(mod, LLVM.Int64Type(), "deleted")
            vmap[g] = deleted
            erase!(deleted)
        end
        for name in ("prefix", "dbg")
            new = clone_with(name, identity; seed)
            str = string(new)
            @test occursin("@g", str)
            @test !occursin("poison", str)
            erase!(new)
        end
        @test verify(mod) === nothing
    end
end

@testset "function cloning with a value map" begin
    # rebuild two functions that call each other with another signature, like when
    # changing address spaces, using a single map
    ir = """
        define i64 @f(ptr addrspace(1) %p) {
          %v = load i64, ptr addrspace(1) %p
          %r = call i64 @g(ptr addrspace(1) %p, i64 %v)
          ret i64 %r
        }
        define i64 @g(ptr addrspace(1) %p, i64 %v) {
          %r = call i64 @f(ptr addrspace(1) %p)
          ret i64 %r
        }"""
    @dispose ctx=Context(; opaque_pointers=true) mod=parse(LLVM.Module, ir) vmap=ValueMap() begin
        type_mapper(typ) = typ isa LLVM.PointerType ? LLVM.PointerType() : typ
        old = collect(mod.functions)
        for f in old
            ft = f.function_type
            new_ft = LLVM.FunctionType(ft.return_type, map(type_mapper, ft.parameters))
            new_f = LLVM.Function(mod, f.name * ".new", new_ft)
            vmap[f] = new_f
            for (arg, new_arg) in zip(f.parameters, new_f.parameters)
                vmap[arg] = new_arg
            end
        end
        for f in old
            clone_into!(vmap[f], f; value_map=vmap, type_mapper,
                        changes=LLVM.CloneFunctionChangeType.GlobalChanges)
        end
        new_f, new_g = vmap[old[1]], vmap[old[2]]

        # the clones call each other
        f_call = collect(new_f.entry.instructions)[2]
        g_call = collect(new_g.entry.instructions)[1]
        @test f_call.called_operand == new_g
        @test g_call.called_operand == new_f
        @test f_call.operands[1] == new_f.parameters[1]
        # the map was extended with the instructions of the clones
        @test vmap[collect(old[1].entry.instructions)[2]] == f_call

        foreach(empty!, old)
        foreach(erase!, old)
        @test verify(mod) === nothing
        @test !occursin("addrspace", string(mod))
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
