@testset "jljit" begin

let jljit=JuliaOJIT()
    dispose(jljit)
end

JuliaOJIT() do jljit
end

let ctx = ThreadSafeContext()
    dispose(ctx)
end

ThreadSafeContext() do ctx
end

@testset "ThreadSafeModule" begin
    @dispose ts_ctx=ThreadSafeContext() ts_mod=ThreadSafeModule("jit") begin
        @test_throws LLVMException ts_mod() do mod
            error("Error")
        end
        run = Ref{Bool}(false)
        ts_mod() do mod
            run[] = true
        end
        @test run[]
    end
end

@testset "JITDylib" begin
    @dispose ts_ctx=ThreadSafeContext() jljit=JuliaOJIT() begin
        es = ExecutionSession(jljit)

        @test LLVM.lookup_dylib(es, "my.so") === nothing

        jd = JITDylib(es, "my.so")
        jd_bare = JITDylib(es, "mybare.so", bare=true)

        @test LLVM.lookup_dylib(es, "my.so") === jd

        jd_main = JITDylib(jljit)

        prefix = LLVM.get_prefix(jljit)
        dg = LLVM.CreateDynamicLibrarySearchGeneratorForProcess(prefix)
        add!(jd_main, dg)

        addr = lookup(jljit, "jl_apply_generic")
        @test pointer(addr) != C_NULL
    end
end

@testset "Undefined Symbol" begin
    @dispose jljit=JuliaOJIT() begin
        @test_throws LLVMException lookup(jljit, string(gensym()))
    end

    @dispose ts_ctx=ThreadSafeContext() jljit=JuliaOJIT() begin
        jd = JITDylib(jljit)

        ts_mod = ThreadSafeModule("jit")

        # build the module
        fname = "wrapper"
        ts_mod() do mod
            T_Int32 = LLVM.Int32Type()
            ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
            fn = LLVM.Function(mod, "mysum", ft)
            linkage!(fn, LLVM.API.LLVMExternalLinkage)

            wrapper = LLVM.Function(mod, fname, ft)
            # generate IR
            @dispose builder=IRBuilder() begin
                entry = BasicBlock(wrapper, "entry")
                position!(builder, entry)

                tmp = call!(builder, ft, fn, [parameters(wrapper)...])
                ret!(builder, tmp)
            end

            triple!(mod, triple(jljit))
            @dispose pm=ModulePassManager() tm=JITTargetMachine() begin
                # TODO: Get TM from jljit?
                add_library_info!(pm, triple(mod))
                add_transform_info!(pm, tm)
                run!(pm, mod)
            end
            verify(mod)
        end

        add!(jljit, jd, ts_mod)
        # @test_throws LLVMException redirect_stderr(devnull) do
        #     # XXX: this reports an unhandled JIT session error;
        #     #      can we handle it instead?
        #     lookup(jljit, fname)
        # end
        # This test triggers an assertion in the juliaJIT memory manager
        # because it allocates a code section but doesn't finalize it
    end
end

@testset "Loading ObjectFile" begin
    @dispose jljit=JuliaOJIT() begin
        jd = JITDylib(jljit)

        sym = "SomeFunction"
        obj = @dispose ctx=Context() mod=LLVM.Module("jit") begin
            ft = LLVM.FunctionType(LLVM.VoidType())
            fn = LLVM.Function(mod, sym, ft)

            @dispose builder=IRBuilder() begin
                entry = BasicBlock(fn, "entry")
                position!(builder, entry)
                ret!(builder)
            end
            verify(mod)

            @dispose tm=JITTargetMachine() begin
                emit(tm, mod, LLVM.API.LLVMObjectFile)
            end
        end
        add!(jljit, jd, MemoryBuffer(obj))

        addr = lookup(jljit, sym)

        @test pointer(addr) != C_NULL

        empty!(jd)
        @test_throws LLVMException lookup(jljit, sym)
    end

    @dispose jljit=JuliaOJIT() begin
        jd = JITDylib(jljit)

        sym = "SomeFunction"
        obj = @dispose ctx=Context() mod=LLVM.Module("jit") begin
            ft = LLVM.FunctionType(LLVM.Int32Type())
            fn = LLVM.Function(mod, sym, ft)

            gv = LLVM.GlobalVariable(mod, LLVM.Int32Type(), "gv")
            LLVM.extinit!(gv, true)

            @dispose builder=IRBuilder() begin
                entry = BasicBlock(fn, "entry")
                position!(builder, entry)
                val = load!(builder, LLVM.Int32Type(), gv)
                ret!(builder, val)
            end
            verify(mod)

            @dispose tm=JITTargetMachine() begin
                emit(tm, mod, LLVM.API.LLVMObjectFile)
            end
        end

        data = Ref{Int32}(42)
        GC.@preserve data begin
            address = LLVM.API.LLVMOrcJITTargetAddress(
                reinterpret(UInt, Base.unsafe_convert(Ptr{Int32}, data)))
            flags = LLVM.API.LLVMJITSymbolFlags(
                LLVM.API.LLVMJITSymbolGenericFlagsExported, 0)
            name = mangle(jljit, "gv")
            symbol = LLVM.API.LLVMJITEvaluatedSymbol(address, flags)
            gv = LLVM.API.LLVMOrcCSymbolMapPair(name, symbol)

            mu = LLVM.absolute_symbols(Ref(gv))
            LLVM.define(jd, mu)

            add!(jljit, jd, MemoryBuffer(obj))

            addr = lookup(jljit, sym)
            @test pointer(addr) != C_NULL

            @test ccall(pointer(addr), Int32, ()) == 42
            data[] = -1
            @test ccall(pointer(addr), Int32, ()) == -1
        end
        empty!(jd)
        @test_throws LLVMException lookup(jljit, sym)
    end
end


@testset "Lazy" begin
    @dispose ts_ctx=ThreadSafeContext() jljit=JuliaOJIT() begin
        jd = JITDylib(jljit)
        es = ExecutionSession(jljit)

        lctm = LLVM.LocalLazyCallThroughManager(triple(jljit), es)
        ism = LLVM.LocalIndirectStubsManager(triple(jljit))
        try
            # 1. define entry symbol
            entry_sym = "foo_entry"
            flags = LLVM.API.LLVMJITSymbolFlags(
                LLVM.API.LLVMJITSymbolGenericFlagsCallable |
                LLVM.API.LLVMJITSymbolGenericFlagsExported, 0)
            entry = LLVM.API.LLVMOrcCSymbolAliasMapPair(
                mangle(jljit, entry_sym),
                LLVM.API.LLVMOrcCSymbolAliasMapEntry(
                    mangle(jljit, "foo"), flags))

            mu = LLVM.reexports(lctm, ism, jd, Ref(entry))
            LLVM.define(jd, mu)

            # 2. Lookup address of entry symbol
            addr = lookup(jljit, entry_sym)
            @test pointer(addr) != C_NULL

            # 3. add MU that will call back into the compiler
            sym = LLVM.API.LLVMOrcCSymbolFlagsMapPair(mangle(jljit, "foo"), flags)

            function materialize(mr)
                syms = LLVM.get_requested_symbols(mr)
                @assert length(syms) == 1

                # syms contains mangled symbols
                # we need to emit an unmangled one

                ts_mod = ThreadSafeModule("jit")
                ts_mod() do mod
                    dl = datalayout(jljit)
                    if LLVM.version() >= v"20"
                        # XXX: LLVM 20 removed the ability to replace a data layout,
                        #      resulting in Julia's JIT having a different DL from the TM's.
                        #      https://github.com/llvm/llvm-project/pull/102993#issuecomment-2886101618
                        dl = replace(dl, r"-ni.*" => "")
                    end
                    datalayout!(mod, dl)

                    T_Int32 = LLVM.Int32Type()
                    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])

                    fn = LLVM.Function(mod, "foo", ft)

                    # generate IR
                    @dispose builder=IRBuilder() begin
                        entry = BasicBlock(fn, "entry")
                        position!(builder, entry)

                        tmp = add!(builder, parameters(fn)...)
                        ret!(builder, tmp)
                    end
                end

                il = LLVM.IRCompileLayer(jljit)
                LLVM.emit(il, mr, ts_mod)

                return nothing
            end

            function discard(jd, sym)
            end

            mu = LLVM.CustomMaterializationUnit("fooMU", Ref(sym), materialize, discard)
            LLVM.define(jd, mu)

            @test ccall(pointer(addr), Int32, (Int32, Int32), 1, 2) == 3
        finally
            dispose(lctm)
            dispose(ism)
        end
    end
end

end
