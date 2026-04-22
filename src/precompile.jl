@setup_workload begin
    @compile_workload begin
        @dispose ctx=Context() begin
            # Type conversions for common Julia primitive types
            for T in (Base.BitInteger_types..., Bool, Float16, Float32, Float64)
                convert(LLVMType, T)
            end

            # IR building: function creation, arithmetic, memory, control flow
            T_int = LLVM.IntType(sizeof(Int) * 8)
            T_ptr = LLVM.PointerType(T_int)

            f, _ = Interop.create_function(T_int, [T_ptr, T_int])
            @dispose builder=IRBuilder() begin
                bb = BasicBlock(f, "entry")
                position!(builder, bb)
                ptr = gep!(builder, T_int, parameters(f)[1], [parameters(f)[2]])
                val = load!(builder, T_int, ptr)
                ret!(builder, val)
            end
            Interop.call_function(f, Int, Tuple{Ptr{Int}, Int}, :x, :y)

            # Intrinsics and metadata
            T_f32 = LLVM.FloatType()
            cf, _ = Interop.create_function(T_f32, [T_f32, T_f32])
            intr = Intrinsic("llvm.experimental.constrained.fadd")
            intr_fn = LLVM.Function(LLVM.parent(cf), intr, [T_f32])
            intr_ft = LLVM.FunctionType(intr, [T_f32])
            @dispose builder=IRBuilder() begin
                bb = BasicBlock(cf, "entry")
                position!(builder, bb)
                val = call!(builder, intr_ft, intr_fn,
                            [parameters(cf)...,
                             Value(MDString("round.upward")),
                             Value(MDString("fpexcept.strict"))])
                ret!(builder, val)
            end
            Interop.call_function(cf, Float32, Tuple{Float32, Float32}, :x, :y)

            # MCJIT execution
            mod = LLVM.Module("jit")
            T_i32 = LLVM.Int32Type()
            ft = LLVM.FunctionType(T_i32, [T_i32, T_i32])
            jf = LLVM.Function(mod, "sum", ft)
            @dispose builder=IRBuilder() begin
                bb = BasicBlock(jf, "entry")
                position!(builder, bb)
                ret!(builder, add!(builder, parameters(jf)[1], parameters(jf)[2]))
            end
            verify(mod)
            string(mod)
            bitcode = convert(MemoryBuffer, mod)
            @dispose parsed=parse(LLVM.Module, bitcode) begin
                verify(parsed)
            end
            dispose(bitcode)
            @dispose engine=JIT(mod) begin
                lookup(engine, "sum")
            end
        end

        # ORC JIT
        tm = JITTargetMachine()
        jit = LLJIT(; tm=JITTargetMachine())
        @dispose ts_ctx=ThreadSafeContext() begin
            ts_mod = ThreadSafeModule("jit")
            ts_mod() do mod
                triple!(mod, triple(tm))
                T_i32 = LLVM.Int32Type()
                ft = LLVM.FunctionType(T_i32, [T_i32, T_i32])
                f = LLVM.Function(mod, "sum", ft)
                @dispose builder=IRBuilder() begin
                    bb = BasicBlock(f, "entry")
                    position!(builder, bb)
                    ret!(builder, add!(builder, parameters(f)[1], parameters(f)[2]))
                end
                verify(mod)
            end
            jd = JITDylib(jit)
            add!(jit, jd, ts_mod)
            lookup(jit, "sum")
        end
        dispose(jit)
        dispose(tm)
    end
end
