@testset "jljit" begin

let jljit=JuliaOJIT()
    dispose(jljit)
end

JuliaOJIT() do jljit
    @dispose dl=LLVM.DataLayout(jljit) begin
        @test string(dl) == jljit.datalayout_string
    end
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
        es = jljit.execution_session

        @test lookup_dylib(es, "my.so") === nothing

        jd = JITDylib(es, "my.so")
        jd_bare = JITDylib(es, "mybare.so", bare=true)

        @test lookup_dylib(es, "my.so") === jd

        jd_main = JITDylib(jljit, "main")

        dg = DynamicLibrarySearchGenerator(jljit)
        add!(jd_main, dg)

        addr = lookup(jljit, jd_main, "jl_apply_generic")
        @test pointer(addr) != C_NULL
    end
end

@testset "Undefined Symbol" begin
    @dispose jljit=JuliaOJIT() begin
        jd = JITDylib(jljit, "test")
        @test_throws LLVMException lookup(jljit, jd, string(gensym()))
    end

    @dispose ts_ctx=ThreadSafeContext() jljit=JuliaOJIT() begin
        jd = JITDylib(jljit, "test")

        ts_mod = ThreadSafeModule("jit")

        # build the module
        fname = "wrapper"
        ts_mod() do mod
            T_Int32 = LLVM.Int32Type()
            ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
            fn = LLVM.Function(mod, "mysum", ft)
            fn.linkage = LLVM.API.LLVMExternalLinkage

            wrapper = LLVM.Function(mod, fname, ft)
            # generate IR
            @dispose builder=IRBuilder() begin
                entry = BasicBlock(wrapper, "entry")
                position!(builder, LLVM.at_end(entry))

                tmp = call!(builder, ft, fn, [wrapper.parameters...])
                ret!(builder, tmp)
            end

            mod.triple = jljit.triple
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

@testset "CustomDefinitionGenerator" begin
    @dispose jljit=JuliaOJIT() begin
        # on older Julia versions, this is a JITDylib that is shared by all users of the
        # Julia JIT, so only generate the symbol we are looking for.
        jd = JITDylib(jljit, "generated")
        name = string(gensym("generated"))
        mangled = mangle(jljit, name)
        data = Ref{Int32}(42)
        dg = CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
            for (sym, flags) in lookup_set
                sym == mangled || continue
                retain(sym)
                define!(jd, absolute_symbols(sym => pointer_from_objref(data)))
            end
        end
        add!(jd, dg)

        @test pointer(lookup(jljit, jd, name)) == pointer_from_objref(data)
        release(mangled)
    end
end

# XXX: on Windows, Julia 1.10 and 1.11 deadlock when looking up a symbol from an object
#      file that was added to their JIT (on the JIT's emission lock)
if !Sys.iswindows() || VERSION >= v"1.12"
    @testset "Loading ObjectFile" begin
        @dispose jljit=JuliaOJIT() begin
            jd = JITDylib(jljit, "objfile1")

            sym = "SomeFunction"
            obj = @dispose ctx=Context() mod=LLVM.Module("jit") begin
                ft = LLVM.FunctionType(LLVM.VoidType())
                fn = LLVM.Function(mod, sym, ft)

                @dispose builder=IRBuilder() begin
                    entry = BasicBlock(fn, "entry")
                    position!(builder, LLVM.at_end(entry))
                    ret!(builder)
                end
                verify(mod)

                @dispose tm=LLVM.JITTargetMachine() begin
                    LLVM.emit(tm, mod, LLVM.API.LLVMObjectFile)
                end
            end
            add!(jljit, jd, MemoryBuffer(obj))

            addr = lookup(jljit, jd, sym)
            @test pointer(addr) != C_NULL
            empty!(jd)
            @test_throws LLVMException lookup(jljit, jd, sym)
        end

        @dispose jljit=JuliaOJIT() begin
            jd = JITDylib(jljit, "objfile2")

            sym = "SomeFunction"
            obj = @dispose ctx=Context() mod=LLVM.Module("jit") begin
                ft = LLVM.FunctionType(LLVM.Int32Type())
                fn = LLVM.Function(mod, sym, ft)

                gv = LLVM.GlobalVariable(mod, LLVM.Int32Type(), "gv")
                gv.externally_initialized = true

                @dispose builder=IRBuilder() begin
                    entry = BasicBlock(fn, "entry")
                    position!(builder, LLVM.at_end(entry))
                    val = load!(builder, LLVM.Int32Type(), gv)
                    ret!(builder, val)
                end
                verify(mod)

                @dispose tm=LLVM.JITTargetMachine() begin
                    LLVM.emit(tm, mod, LLVM.API.LLVMObjectFile)
                end
            end

            data = Ref{Int32}(42)
            GC.@preserve data begin
                ptr = Base.unsafe_convert(Ptr{Int32}, data)
                define!(jd, absolute_symbols(mangle(jljit, "gv") => ptr))

                add!(jljit, jd, MemoryBuffer(obj))

                addr = lookup(jljit, jd, sym)
                @test pointer(addr) != C_NULL
                @test ccall(pointer(addr), Int32, ()) == 42
                data[] = -1
                @test ccall(pointer(addr), Int32, ()) == -1
                empty!(jd)
                @test_throws LLVMException lookup(jljit, jd, sym)
            end
        end
    end
end


@testset "Lazy" begin
    @dispose ts_ctx=ThreadSafeContext() jljit=JuliaOJIT() begin
        jd = JITDylib(jljit, "lazy")
        es = jljit.execution_session

        lctm = LocalLazyCallThroughManager(jljit.triple, es)
        ism = LocalIndirectStubsManager(jljit.triple)
        try
            # 1. define entry symbol
            entry_sym = "foo_entry"
            mu = lazy_reexports(lctm, ism, jd,
                                [mangle(jljit, entry_sym) => mangle(jljit, "foo")])
            define!(jd, mu)

            # 2. Lookup address of entry symbol
            addr = lookup(jljit, jd, entry_sym)
            @test pointer(addr) != C_NULL

            # 3. add MU that will call back into the compiler
            function materialize(mr)
                syms = mr.requested_symbols
                @assert length(syms) == 1

                # syms contains mangled symbols
                # we need to emit an unmangled one

                ts_mod = ThreadSafeModule("jit")
                ts_mod() do mod
                    dl = jljit.datalayout_string
                    if LLVM.version() >= v"20"
                        # XXX: LLVM 20 removed the ability to replace a data layout,
                        #      resulting in Julia's JIT having a different DL from the TM's.
                        #      https://github.com/llvm/llvm-project/pull/102993#issuecomment-2886101618
                        dl = replace(dl, r"-ni.*" => "")
                    end
                    mod.datalayout = dl

                    T_Int32 = LLVM.Int32Type()
                    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])

                    fn = LLVM.Function(mod, "foo", ft)

                    # generate IR
                    @dispose builder=IRBuilder() begin
                        entry = BasicBlock(fn, "entry")
                        position!(builder, LLVM.at_end(entry))

                        tmp = add!(builder, fn.parameters...)
                        ret!(builder, tmp)
                    end
                end

                il = jljit.ir_compile_layer
                emit!(il, mr, ts_mod)

                return nothing
            end

            function discard(jd, sym)
            end

            symbols = [mangle(jljit, "foo") => SymbolFlags(callable=true)]
            mu = CustomMaterializationUnit("fooMU", symbols, materialize, discard)
            define!(jd, mu)

            @test ccall(pointer(addr), Int32, (Int32, Int32), 1, 2) == 3
        finally
            dispose(lctm)
            dispose(ism)
        end
    end
end

end
