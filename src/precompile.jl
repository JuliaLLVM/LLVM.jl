@setup_workload begin
    # __init__ has not yet run during precompilation, so ensure the EH frame
    # registration stubs are published before exercising the LLJIT (see support.jl).
    register_eh_frame_stubs()
    @compile_workload begin
        @dispose ctx=Context() begin
            # Type conversions for common Julia primitive types
            for T in (Base.BitInteger_types..., Bool, Float16, Float32, Float64)
                convert(LLVMType, T)
            end

            # Intrinsics and metadata
            Interop.generate_llvmcall(Float32, Tuple{Float32, Float32}, :x, :y) do builder, x, y
                T_f32 = LLVM.FloatType()
                intr = Intrinsic("llvm.experimental.constrained.fadd")
                intr_fn = LLVM.Function(Interop.current_module(builder), intr, [T_f32])
                intr_ft = LLVM.FunctionType(intr, [T_f32])
                call!(builder, intr_ft, intr_fn,
                      [x, y, Value(MDString("round.upward")), Value(MDString("fpexcept.strict"))])
            end

            # staged IR generation, as done by `@llvmgenerated`
            Interop.generate_llvmcall(Int, Tuple{Ptr{Int}, Int, Val{1}}, :x, :y, :z) do builder, x, y, z
                T_int = convert(LLVMType, Int)
                if !(value_type(x) isa LLVM.PointerType)
                    x = inttoptr!(builder, x, LLVM.PointerType(T_int))
                end
                load!(builder, T_int, gep!(builder, T_int, x, [y]))
            end

            # MCJIT execution
            mod = LLVM.Module("jit")
            T_i32 = LLVM.Int32Type()
            ft = LLVM.FunctionType(T_i32, [T_i32, T_i32])
            jf = LLVM.Function(mod, "sum", ft)
            @dispose builder=IRBuilder() begin
                bb = BasicBlock(jf, "entry")
                position!(builder, at_end(bb))
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
                    position!(builder, at_end(bb))
                    ret!(builder, add!(builder, parameters(f)[1], parameters(f)[2]))
                end
                verify(mod)
            end
            jd = jit.main_dylib
            add!(jit, jd, ts_mod)
            lookup(jit, "sum")
        end
        dispose(jit)
        dispose(tm)
    end
end
