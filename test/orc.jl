@testset "orc" begin

let lljit=LLJIT()
    dispose(lljit)
end

LLJIT() do lljit
end

let ctx = ThreadSafeContext()
    dispose(ctx)
end

ThreadSafeContext() do ctx
end

@testset "diagnostics" begin
    # diagnostics emitted in the inner context should be thrown as LLVMExceptions, just
    # like with a regular Context; without a handler installed, LLVM's default behavior
    # is to print the error and exit the process.
    ThreadSafeContext() do ts_ctx
        ctx = context(ts_ctx)
        activate(ctx)
        try
            invalid_bitcode = unsafe_wrap(Vector{UInt8}, "invalid")
            @test_throws LLVMException parse(LLVM.Module, invalid_bitcode)
            @test_throws LLVMException parse(LLVM.Module, invalid_bitcode; lazy=true)
        finally
            deactivate(ctx)
        end
    end
end

@testset "ThreadSafeModule" begin
    @dispose ts_ctx=ThreadSafeContext() ts_mod=ThreadSafeModule("jit") begin
        @test_throws LLVMException ts_mod() do mod
            error("Error")
        end
        @test ts_mod() do mod
            true
        end
    end

    @dispose ctx=Context() ts_ctx=ThreadSafeContext() begin
        src_mod = LLVM.Module("SomeModule")
        ts_mod = ThreadSafeModule(src_mod)
        ts_mod() do copied_mod
            # XXX: this is a very specific test to check the current implementation of the
            #      ThreadSafeModule constructor, which currently copies the source module
            #      from its context into the thread safe one. This is questionable; maybe
            #      it should create a ThreadSafeModule in a ThreadSafeContext matching the
            #      source context. However, that would result in a TSMod that doesn't match
            #      the currently-active ts_context()...
            @test context(copied_mod) != ctx
            @test context(copied_mod) == context(ts_context())
        end
        dispose(ts_mod)
    end
end

@testset "JITDylib" begin
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        es = ExecutionSession(lljit)

        @test LLVM.lookup_dylib(es, "my.so") === nothing

        jd = JITDylib(es, "my.so")
        jd_bare = JITDylib(es, "mybare.so", bare=true)

        @test LLVM.lookup_dylib(es, "my.so") === jd

        jd_main = JITDylib(lljit)

        dg = LLVM.DynamicLibrarySearchGenerator(lljit)
        add!(jd_main, dg)

        addr = lookup(lljit, "jl_apply_generic")
        @test pointer(addr) != C_NULL
    end
end

@testset "DynamicLibrarySearchGenerator" begin
    # a specific library
    @dispose lljit=LLJIT() begin
        path = String(Base.libllvm_path())
        expected = Libc.Libdl.dlopen(path) do handle
            Libc.Libdl.dlsym(handle, :LLVMContextCreate)
        end

        @test_throws LLVMException lookup(lljit, "LLVMContextCreate")
        dg = LLVM.DynamicLibrarySearchGenerator(lljit, path)
        add!(JITDylib(lljit), dg)
        @test pointer(lookup(lljit, "LLVMContextCreate")) == expected

        # the generator only searches the library it was created for
        @test_throws LLVMException lookup(lljit, "jl_apply_generic")
    end

    # generators that are not added to a JITDylib need to be disposed of
    @dispose lljit=LLJIT() begin
        dg = LLVM.DynamicLibrarySearchGenerator(lljit, String(Base.libllvm_path()))
        dispose(dg)

        @test_throws LLVMException LLVM.DynamicLibrarySearchGenerator(lljit, "/nonexistent/libfoo.so")
    end
end

@testset "CustomDefinitionGenerator" begin
    local dg
    data = Ref{Int32}(42)
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = JITDylib(lljit)
        gv_name = mangle(lljit, "gv")
        weak_name = mangle(lljit, "weak")

        requests = []
        dg = LLVM.CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
            push!(requests, (; kind, jd_flags, names=[string(name) => flags for (name, flags) in lookup_set]))
            for (name, flags) in lookup_set
                name == gv_name || continue
                LLVM.retain(name)   # borrowed, but absolute_symbols takes ownership
                LLVM.define(jd, LLVM.absolute_symbols(name => pointer_from_objref(data)))
            end
        end
        @test dg in LLVM.CUSTOM_DG_ROOTS
        add!(jd, dg)

        # lookups performed when linking code
        ts_mod = ThreadSafeModule("jit")
        ts_mod() do mod
            gv = GlobalVariable(mod, LLVM.Int32Type(), "gv")
            load_gv = LLVM.Function(mod, "load_gv", LLVM.FunctionType(LLVM.Int32Type()))
            @dispose builder=IRBuilder() begin
                position!(builder, BasicBlock(load_gv, "entry"))
                ret!(builder, load!(builder, LLVM.Int32Type(), gv))
            end
        end
        add!(lljit, jd, ts_mod)
        GC.@preserve data begin
            @test ccall(pointer(lookup(lljit, "load_gv")), Int32, ()) == 42
        end
        @test only(requests).kind == LLVM.API.LLVMOrcLookupKindStatic
        @test only(requests).names == [string(gv_name) => LLVM.API.LLVMOrcSymbolLookupFlagsRequiredSymbol]

        # direct lookups, of symbols that are already defined
        GC.@preserve data begin
            @test pointer(lookup(lljit, "gv")) == pointer_from_objref(data)
        end
        @test length(requests) == 1

        # symbols that the generator does not define remain undefined
        @test_throws LLVMException lookup(lljit, "undefined")
        @test length(requests) == 2
        @test requests[2].jd_flags == LLVM.API.LLVMOrcJITDylibLookupFlagsMatchAllSymbols

        # weak references are allowed to remain undefined
        # (older versions of LLVM request them as if they were required, and RuntimeDyld,
        #  which LLJIT uses to link COFF objects, aborts on unresolved weak references)
        if LLVM.version() >= v"18" && !Sys.iswindows()
            ts_mod = ThreadSafeModule("jit")
            ts_mod() do mod
                weak = GlobalVariable(mod, LLVM.Int32Type(), "weak")
                linkage!(weak, LLVM.API.LLVMExternalWeakLinkage)
                get_weak = LLVM.Function(mod, "get_weak", LLVM.FunctionType(value_type(weak)))
                @dispose builder=IRBuilder() begin
                    position!(builder, BasicBlock(get_weak, "entry"))
                    ret!(builder, weak)
                end
            end
            add!(lljit, jd, ts_mod)
            @test ccall(pointer(lookup(lljit, "get_weak")), Ptr{Int32}, ()) == C_NULL
            @test requests[end].names == [string(weak_name) =>
                                          LLVM.API.LLVMOrcSymbolLookupFlagsWeaklyReferencedSymbol]
        end

        LLVM.release(gv_name)
        LLVM.release(weak_name)
    end
    # destroying the JITDylib disposes of the generator
    @test !(dg in LLVM.CUSTOM_DG_ROOTS)

    # generators that are not added to a JITDylib need to be disposed of
    dg = LLVM.CustomDefinitionGenerator((args...) -> nothing)
    @test dg in LLVM.CUSTOM_DG_ROOTS
    dispose(dg)
    @test !(dg in LLVM.CUSTOM_DG_ROOTS)

    # exceptions are reported to ORC, and can be rethrown afterwards
    @dispose lljit=LLJIT() begin
        dg = LLVM.CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
            throw(ArgumentError("definition generator error"))
        end
        add!(JITDylib(lljit), dg)

        err = try
            lookup(lljit, "foo")
        catch err
            err
        end
        @test err isa LLVMException
        @test occursin("definition generator error", err.info)

        try
            LLVM.check_callback_error(dg)
            @test false
        catch err
            @test err isa CallbackException
            @test err.ex isa ArgumentError
            @test !isempty(err.processed_bt)
        end
        @test LLVM.check_callback_error(dg) === nothing
    end
end

@testset "Undefined Symbol" begin
    @dispose lljit=LLJIT() begin
        @test_throws LLVMException lookup(lljit, string(gensym()))
    end

    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT(;tm=JITTargetMachine()) begin
        jd = JITDylib(lljit)

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

            triple!(mod, triple(lljit))
            @dispose pm=ModulePassManager() tm=JITTargetMachine() begin
                # TODO: Get TM from lljit?
                add_library_info!(pm, triple(mod))
                add_transform_info!(pm, tm)
                run!(pm, mod)
            end
            verify(mod)
        end

        add!(lljit, jd, ts_mod)
        @test_throws LLVMException redirect_stderr(devnull) do
            # XXX: this reports an unhandled JIT session error;
            #      can we handle it instead?
            lookup(lljit, fname)
        end
    end
end

@testset "Materialization callback errors" begin
    @dispose lljit=LLJIT() begin
        jd = JITDylib(lljit)
        flags = LLVM.API.LLVMJITSymbolFlags(
            LLVM.API.LLVMJITSymbolGenericFlagsCallable |
            LLVM.API.LLVMJITSymbolGenericFlagsExported, 0)
        sym = LLVM.API.LLVMOrcCSymbolFlagsMapPair(mangle(lljit, "throws"), flags)

        mu = LLVM.CustomMaterializationUnit(
            "throwingMU", Ref(sym),
            mr -> throw(ArgumentError("materialization callback error")),
            (jd, sym) -> nothing)
        LLVM.define(jd, mu)
        @test mu in LLVM.CUSTOM_MU_ROOTS

        @test_throws LLVMException lookup(lljit, "throws")
        @test !(mu in LLVM.CUSTOM_MU_ROOTS)
        try
            LLVM.check_callback_error(mu)
            @test false
        catch err
            @test err isa CallbackException
            @test err.ex isa ArgumentError
            @test occursin("materialization callback error", string(err.ex))
            @test !isempty(err.processed_bt)
        end
        @test LLVM.check_callback_error(mu) === nothing
    end
end

@testset "Unmaterialized units" begin
    local mu
    @dispose lljit=LLJIT() begin
        symbols = [mangle(lljit, "unused") => LLVM.symbol_flags(callable=true)]
        mu = LLVM.CustomMaterializationUnit("unusedMU", symbols, mr -> nothing,
                                            (jd, sym) -> nothing)
        LLVM.define(JITDylib(lljit), mu)
        @test mu in LLVM.CUSTOM_MU_ROOTS
    end
    # destroying the JITDylib destroys the unit
    @test !(mu in LLVM.CUSTOM_MU_ROOTS)
end

@testset "Absolute symbols" begin
    @dispose lljit=LLJIT() begin
        jd = JITDylib(lljit)
        data = Ref{Int32}(42)
        ptr = pointer_from_objref(data)

        LLVM.define(jd, LLVM.absolute_symbols(mangle(lljit, "gv") => ptr))
        @test pointer(lookup(lljit, "gv")) == ptr

        # multiple symbols, flags, and collections
        LLVM.define(jd, LLVM.absolute_symbols([
            mangle(lljit, "gv1") => ptr + 1,
            mangle(lljit, "gv2") => (UInt(ptr) + 2, LLVM.symbol_flags(callable=true)),
        ]))
        LLVM.define(jd, LLVM.absolute_symbols(
            Dict(mangle(lljit, "gv3") => OrcTargetAddress(ptr + 3))))
        @test pointer(lookup(lljit, "gv1")) == ptr + 1
        @test pointer(lookup(lljit, "gv2")) == ptr + 2
        @test pointer(lookup(lljit, "gv3")) == ptr + 3

        # duplicate definitions are rejected
        @test_throws LLVMException LLVM.define(jd,
            LLVM.absolute_symbols(mangle(lljit, "gv") => ptr + 4))
        @test pointer(lookup(lljit, "gv")) == ptr
        sym = mangle(lljit, "dup")
        @test_throws ArgumentError LLVM.absolute_symbols([sym => ptr, sym => ptr])
        LLVM.release(sym)
    end
end

@testset "Symbols" begin
    @dispose lljit=LLJIT() begin
        sym = mangle(lljit, "foo")
        @test String(sym) == string(sym) == (LLVM.global_prefix(lljit) == 0 ? "foo" : "_foo")
        @test occursin(repr(String(sym)), repr(sym))

        # symbols are interned
        other = intern(ExecutionSession(lljit), String(sym))
        @test other == sym
        LLVM.release(other)
        LLVM.release(sym)
    end

    flags = LLVM.symbol_flags()
    @test flags.GenericFlags == UInt8(LLVM.API.LLVMJITSymbolGenericFlagsExported)
    @test flags.TargetFlags == 0
    flags = LLVM.symbol_flags(exported=false, callable=true, weak=true, target_flags=1)
    @test flags.GenericFlags == UInt8(LLVM.API.LLVMJITSymbolGenericFlagsCallable) |
                                UInt8(LLVM.API.LLVMJITSymbolGenericFlagsWeak)
    @test flags.TargetFlags == 1
end

@testset "Loading ObjectFile" begin
    @dispose lljit=LLJIT(;tm=JITTargetMachine()) begin
        jd = JITDylib(lljit)

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
        add!(lljit, jd, MemoryBuffer(obj))

        addr = lookup(lljit, sym)

        @test pointer(addr) != C_NULL

        empty!(jd)
        @test_throws LLVMException lookup(lljit, sym)

        # invalid objects are rejected (and consumed)
        @test_throws LLVMException add!(lljit, jd, MemoryBuffer(rand(UInt8, 64)))
    end

    @dispose lljit=LLJIT(; tm=JITTargetMachine()) begin
        jd = JITDylib(lljit)

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
            name = mangle(lljit, "gv")
            symbol = LLVM.API.LLVMJITEvaluatedSymbol(address, flags)
            gv = LLVM.API.LLVMOrcCSymbolMapPair(name, symbol)

            mu = LLVM.absolute_symbols(Ref(gv))
            LLVM.define(jd, mu)

            add!(lljit, jd, MemoryBuffer(obj))

            addr = lookup(lljit, sym)
            @test pointer(addr) != C_NULL

            @test ccall(pointer(addr), Int32, ()) == 42
            data[] = -1
            @test ccall(pointer(addr), Int32, ()) == -1
        end
        empty!(jd)
        @test_throws LLVMException lookup(lljit, sym)
    end
end

@testset "ObjectLinkingLayer" begin
    # JIT a simple function and return the symbol flags ORC recorded for it.
    function jit_symbol_flags(creator=nothing; tm=nothing)
        builder = LLJITBuilder()
        tm === nothing || targetmachinebuilder!(builder, TargetMachineBuilder(tm()))
        creator === nothing || linkinglayercreator!(creator, builder)
        @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT(builder) begin
            jd = JITDylib(lljit)

            ts_mod = ThreadSafeModule("jit")
            sym = "SomeFunctionOLL"

            ts_mod() do mod
                T = LLVM.DoubleType()
                ft = LLVM.FunctionType(T, [T])
                fn = LLVM.Function(mod, sym, ft)

                @dispose builder=IRBuilder() begin
                    entry = BasicBlock(fn, "entry")
                    position!(builder, entry)
                    ret!(builder, fadd!(builder, parameters(fn)[1], ConstantFP(T, 1.25)))
                end
                verify(mod)
            end

            add!(lljit, jd, ts_mod)
            addr = lookup(lljit, sym)
            @test pointer(addr) != C_NULL
            @test ccall(pointer(addr), Float64, (Float64,), 1.0) == 2.25

            # the JITDylib is keyed by linker-mangled names (e.g. prefixed with _ on macOS)
            mangled = mangle(lljit, sym)
            name = string(mangled)
            LLVM.release(mangled)
            m = match(Regex("\"$name\": \\S+ (\\S+)"), string(jd))
            @test m !== nothing
            return m[1]
        end
    end

    called_oll = Ref{Int}(0)
    flags = jit_symbol_flags() do es, triple
        oll = ObjectLinkingLayer(es, triple)
        register!(oll, GDBRegistrationListener())
        called_oll[] += 1
        return oll
    end
    @test called_oll[] >= 1

    # a custom layer should behave like LLJIT's default one
    @test flags == jit_symbol_flags()
    @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es)) == flags
    let tm = () -> JITTargetMachine()
        tm_flags = jit_symbol_flags(; tm)
        @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es, triple); tm) ==
              tm_flags
        @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es); tm) == tm_flags
    end

    # COFF objects need additional configuration (JuliaLLVM/LLVM.jl#395).
    # RuntimeDyld can link them on any host, so test that everywhere.
    if Sys.ARCH == :x86_64 && :X86 in LLVM.backends()
        coff_triple = "x86_64-w64-windows-gnu"
        tm = () -> TargetMachine(LLVM.Target(; triple=coff_triple), coff_triple;
                                 reloc=LLVM.API.LLVMRelocStatic,
                                 code=LLVM.API.LLVMCodeModelJITDefault)
        coff_flags = jit_symbol_flags(; tm)
        @test coff_flags == "[Callable]"
        # the callback receives the executor's triple on LLVM 21+, so pass the target's
        @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es, coff_triple); tm) ==
              coff_flags
        @test jit_symbol_flags(; tm) do es, triple
            ObjectLinkingLayer(es, coff_triple; override_object_flags=true,
                               auto_claim_object_symbols=true)
        end == coff_flags
    end

    builder = LLJITBuilder()
    linkinglayercreator!(builder) do es, triple
        throw(ArgumentError("object layer creator error"))
    end
    GC.gc()
    try
        LLJIT(builder)
        @test false
    catch err
        @test err isa CallbackException
        @test err.ex isa ArgumentError
        @test occursin("object layer creator error", string(err.ex))
        @test !isempty(err.processed_bt)
    end
end

@testset "Lazy" begin
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = JITDylib(lljit)
        es = ExecutionSession(lljit)

        lctm = LLVM.LocalLazyCallThroughManager(triple(lljit), es)
        ism = LLVM.LocalIndirectStubsManager(triple(lljit))
        try
            # 1. define entry symbol
            entry_sym = "foo_entry"
            mu = LLVM.lazy_reexports(lctm, ism, jd,
                                     [mangle(lljit, entry_sym) => mangle(lljit, "foo")])
            LLVM.define(jd, mu)

            # 2. Lookup address of entry symbol
            addr = lookup(lljit, entry_sym)
            @test pointer(addr) != C_NULL

            # 3. add MU that will call back into the compiler
            function materialize(mr)
                syms = LLVM.requested_symbols(mr)
                @assert length(syms) == 1

                # syms contains mangled symbols
                # we need to emit an unmangled one

                ts_mod = ThreadSafeModule("jit")
                ts_mod() do mod
                    dl = datalayout(lljit)
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

                il = LLVM.IRTransformLayer(lljit)
                LLVM.emit(il, mr, ts_mod)

                return nothing
            end

            function discard(jd, sym)
            end

            symbols = [mangle(lljit, "foo") => LLVM.symbol_flags(callable=true)]
            mu = LLVM.CustomMaterializationUnit("fooMU", symbols, materialize, discard)
            LLVM.define(jd, mu)

            @test ccall(pointer(addr), Int32, (Int32, Int32), 1, 2) == 3
            @test !(mu in LLVM.CUSTOM_MU_ROOTS)
        finally
            dispose(lctm)
            dispose(ism)
        end
    end
end

end
