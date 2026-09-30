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
        es = lljit.execution_session

        @test lookup_dylib(es, "my.so") === nothing

        jd = JITDylib(es, "my.so")
        jd_bare = JITDylib(es, "mybare.so", bare=true)

        @test lookup_dylib(es, "my.so") === jd

        jd_main = lljit.main_dylib

        dg = DynamicLibrarySearchGenerator(lljit)
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

        jd = JITDylib(lljit.execution_session, "libllvm"; bare=true)
        @test_throws LLVMException lookup(lljit, jd, "LLVMContextCreate")
        dg = DynamicLibrarySearchGenerator(lljit, path)
        add!(jd, dg)
        @test pointer(lookup(lljit, jd, "LLVMContextCreate")) == expected

        # the generator only searches the library it was created for
        @test_throws LLVMException lookup(lljit, jd, "jl_apply_generic")
    end

    # generators that are not added to a JITDylib need to be disposed of
    @dispose lljit=LLJIT() begin
        dg = DynamicLibrarySearchGenerator(lljit, String(Base.libllvm_path()))
        dispose(dg)

        @test_throws LLVMException DynamicLibrarySearchGenerator(lljit,
                                                                 "/nonexistent/libfoo.so")
    end
end

@testset "CustomDefinitionGenerator" begin
    local dg
    data = Ref{Int32}(42)
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib
        gv_name = mangle(lljit, "gv")
        weak_name = mangle(lljit, "weak")

        requests = []
        dg = CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
            push!(requests, (; kind, jd_flags, names=[string(name) => flags for (name, flags) in lookup_set]))
            for (name, flags) in lookup_set
                name == gv_name || continue
                retain(name)   # borrowed, but absolute_symbols takes ownership
                define!(jd, absolute_symbols(name => pointer_from_objref(data)))
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
                position!(builder, LLVM.at_end(BasicBlock(load_gv, "entry")))
                ret!(builder, load!(builder, LLVM.Int32Type(), gv))
            end
        end
        add!(lljit, jd, ts_mod)
        GC.@preserve data begin
            @test ccall(pointer(lookup(lljit, "load_gv")), Int32, ()) == 42
        end
        @test only(requests).kind == LLVM.LookupKind.Static
        @test only(requests).names == [string(gv_name) => LLVM.SymbolLookupFlags.RequiredSymbol]

        # direct lookups, of symbols that are already defined
        GC.@preserve data begin
            @test pointer(lookup(lljit, "gv")) == pointer_from_objref(data)
        end
        @test length(requests) == 1

        # symbols that the generator does not define remain undefined
        @test_throws LLVMException lookup(lljit, "undefined")
        @test length(requests) == 2
        @test requests[2].jd_flags == LLVM.JITDylibLookupFlags.MatchAllSymbols

        # weak references are allowed to remain undefined
        # (older versions of LLVM request them as if they were required, and RuntimeDyld,
        #  which LLJIT uses to link COFF objects, aborts on unresolved weak references)
        if LLVM.version() >= v"18" && !Sys.iswindows()
            ts_mod = ThreadSafeModule("jit")
            ts_mod() do mod
                weak = GlobalVariable(mod, LLVM.Int32Type(), "weak")
                weak.linkage = LLVM.API.LLVMExternalWeakLinkage
                get_weak = LLVM.Function(mod, "get_weak",
                                         LLVM.FunctionType(weak.value_type))
                @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.at_end(BasicBlock(get_weak, "entry")))
                    ret!(builder, weak)
                end
            end
            add!(lljit, jd, ts_mod)
            @test ccall(pointer(lookup(lljit, "get_weak")), Ptr{Int32}, ()) == C_NULL
            @test requests[end].names == [string(weak_name) =>
                                          LLVM.SymbolLookupFlags.WeaklyReferencedSymbol]
        end

        release(gv_name)
        release(weak_name)
    end
    # destroying the JITDylib disposes of the generator
    @test !(dg in LLVM.CUSTOM_DG_ROOTS)

    # generators that are not added to a JITDylib need to be disposed of
    dg = CustomDefinitionGenerator((args...) -> nothing)
    @test dg in LLVM.CUSTOM_DG_ROOTS
    dispose(dg)
    @test !(dg in LLVM.CUSTOM_DG_ROOTS)

    # exceptions are reported to ORC, and can be rethrown afterwards
    @dispose lljit=LLJIT() begin
        dg = CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
            throw(ArgumentError("definition generator error"))
        end
        add!(lljit.main_dylib, dg)

        err = try
            lookup(lljit, "foo")
        catch err
            err
        end
        @test err isa LLVMException
        @test occursin("definition generator error", err.info)

        try
            check_callback_error!(dg)
            @test false
        catch err
            @test err isa LLVM.CallbackException
            @test err.ex isa ArgumentError
            @test !isempty(err.processed_bt)
        end
        @test check_callback_error!(dg) === nothing
    end
end

@testset "Undefined Symbol" begin
    @dispose lljit=LLJIT() begin
        @test_throws LLVMException lookup(lljit, string(gensym()))
    end

    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT(;tm=LLVM.JITTargetMachine()) begin
        jd = lljit.main_dylib

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

            mod.triple = lljit.triple
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
        jd = lljit.main_dylib
        symbols = [mangle(lljit, "throws") => SymbolFlags(callable=true)]
        mu = CustomMaterializationUnit(
            "throwingMU", symbols,
            mr -> throw(ArgumentError("materialization callback error")),
            (jd, sym) -> nothing)
        define!(jd, mu)
        @test mu in LLVM.CUSTOM_MU_ROOTS

        @test_throws LLVMException lookup(lljit, "throws")
        @test !(mu in LLVM.CUSTOM_MU_ROOTS)
        try
            check_callback_error!(mu)
            @test false
        catch err
            @test err isa LLVM.CallbackException
            @test err.ex isa ArgumentError
            @test occursin("materialization callback error", string(err.ex))
            @test !isempty(err.processed_bt)
        end
        @test check_callback_error!(mu) === nothing
    end
end

@testset "Materializer errors after emitting" begin
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib
        function materialize(mr)
            ts_mod = ThreadSafeModule("jit")
            ts_mod() do mod
                # emitting directly to a layer bypasses LLJIT's module set-up
                mod.triple = lljit.triple
                mod.datalayout = lljit.datalayout_string
                fn = LLVM.Function(mod, "emitted", LLVM.FunctionType(LLVM.Int32Type()))
                @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                    ret!(builder, ConstantInt(Int32(42)))
                end
            end
            emit!(lljit.ir_transform_layer, mr, ts_mod)
            # the responsibility has been consumed
            @dispose unused=ThreadSafeModule("jit") begin
                @test_throws ArgumentError emit!(lljit.ir_transform_layer, mr, unused)
            end
            error("materializer error after emitting")
        end
        symbols = [mangle(lljit, "emitted") => SymbolFlags(callable=true)]
        mu = CustomMaterializationUnit("emittingMU", symbols, materialize,
                                            (jd, sym) -> nothing)
        define!(jd, mu)

        # the code was emitted, so the lookup succeeds
        @test ccall(pointer(lookup(lljit, "emitted")), Int32, ()) == 42
        @test_throws LLVM.CallbackException check_callback_error!(mu)
    end
end

@testset "Unmaterialized units" begin
    local mu
    @dispose lljit=LLJIT() begin
        symbols = [mangle(lljit, "unused") => SymbolFlags(callable=true)]
        mu = CustomMaterializationUnit("unusedMU", symbols, mr -> nothing,
                                            (jd, sym) -> nothing)
        define!(lljit.main_dylib, mu)
        @test mu in LLVM.CUSTOM_MU_ROOTS
    end
    # destroying the JITDylib destroys the unit
    @test !(mu in LLVM.CUSTOM_MU_ROOTS)
end

@testset "Ownership" begin
    data = Ref{Int32}(42)
    ptr = pointer_from_objref(data)
    @dispose lljit=LLJIT() begin
        jd = lljit.main_dylib

        # defining a unit consumes it
        mu = absolute_symbols(mangle(lljit, "owned1") => ptr)
        @test mu isa MaterializationUnit
        define!(jd, mu)
        @test_throws ArgumentError define!(jd, mu)
        dispose(mu)     # does nothing
        @test pointer(lookup(lljit, "owned1")) == ptr

        # also when defining it fails
        mu = absolute_symbols(mangle(lljit, "owned1") => ptr)
        @test_throws LLVMException define!(jd, mu)
        @test_throws ArgumentError define!(jd, mu)
        dispose(mu)

        # units that aren't defined need to be disposed of, so @dispose works either way
        @dispose mu=absolute_symbols(mangle(lljit, "unused") => ptr) begin end
        @dispose mu=absolute_symbols(mangle(lljit, "owned2") => ptr) begin
            define!(jd, mu)
        end
        @test pointer(lookup(lljit, "owned2")) == ptr

        # disposing of an unused custom unit unroots its callbacks
        mu = CustomMaterializationUnit("unusedMU",
                                       [mangle(lljit, "custom") => SymbolFlags(callable=true)],
                                       mr -> nothing, (jd, sym) -> nothing)
        @test mu in LLVM.CUSTOM_MU_ROOTS
        dispose(mu)
        @test !(mu in LLVM.CUSTOM_MU_ROOTS)
        dispose(mu)
        @test_throws ArgumentError define!(jd, mu)

        # the initializer symbol takes an additional reference
        init = mangle(lljit, "with_init")
        retain(init)
        init_flags = SymbolFlags(materialization_side_effects_only=true)
        mu = CustomMaterializationUnit("initMU", [init => init_flags], mr -> nothing,
                                       (jd, sym) -> nothing; init)
        dispose(mu)     # releases both references
        # which needs to be one of the symbols, and only have side effects
        init = mangle(lljit, "bad_init")
        other = mangle(lljit, "other")
        @test_throws ArgumentError CustomMaterializationUnit("initMU", [other => init_flags],
                                                             mr -> nothing, (jd, sym) -> nothing;
                                                             init)
        @test_throws ArgumentError CustomMaterializationUnit("initMU", [init => SymbolFlags()],
                                                             mr -> nothing, (jd, sym) -> nothing;
                                                             init)
        release(other)
        release(init)

        # a unit that can't be created isn't rooted
        nroots = length(LLVM.CUSTOM_MU_ROOTS)
        sym = mangle(lljit, "bad_name")
        @test_throws ArgumentError CustomMaterializationUnit("bad\0name",
                                                             [sym => SymbolFlags()],
                                                             mr -> nothing, (jd, sym) -> nothing)
        @test length(LLVM.CUSTOM_MU_ROOTS) == nroots
        release(sym)

        # a defined one stays rooted until it's materialized
        materialized = Ref(false)
        mu = CustomMaterializationUnit("rootedMU",
                                       [mangle(lljit, "rooted") => SymbolFlags(callable=true)],
                                       mr -> (materialized[] = true; error("not materializing")),
                                       (jd, sym) -> nothing)
        define!(jd, mu)
        dispose(mu)
        @test mu in LLVM.CUSTOM_MU_ROOTS
        mu = nothing
        GC.gc(true)
        @test_throws LLVMException lookup(lljit, "rooted")
        @test materialized[]

        # adding a generator consumes it
        dg = DynamicLibrarySearchGenerator(lljit)
        add!(jd, dg)
        @test_throws ArgumentError add!(jd, dg)
        dispose(dg)
        dg = CustomDefinitionGenerator((args...) -> nothing)
        add!(jd, dg)
        @test_throws ArgumentError add!(jd, dg)
        dispose(dg)
        @test dg in LLVM.CUSTOM_DG_ROOTS
    end

    # target machine builders are consumed by JIT builders, which are consumed by the JIT
    builder = LLJITBuilder()
    tmb = TargetMachineBuilder()
    target_machine_builder!(builder, tmb)
    @test_throws ArgumentError target_machine_builder!(builder, tmb)
    dispose(tmb)
    lljit = LLJIT(builder)
    @test_throws ArgumentError LLJIT(builder)
    @dispose tmb=TargetMachineBuilder() begin
        @test_throws ArgumentError target_machine_builder!(builder, tmb)
    end
    dispose(builder)
    dispose(lljit)

    # unused builders need to be disposed of
    @dispose builder=LLJITBuilder() tmb=TargetMachineBuilder() begin end

    # target machines are consumed by target machine builders, and thus by JITs
    @dispose tm=LLVM.JITTargetMachine() begin
        @dispose tmb=TargetMachineBuilder(tm) begin
            @test_throws ArgumentError tm.triple
            @test_throws ArgumentError TargetMachineBuilder(tm)
        end
    end
    @dispose tm=LLVM.JITTargetMachine() lljit=LLJIT(; tm) begin
        @test_throws ArgumentError tm.triple
    end

    # thread-safe modules are consumed by adding them to a JIT
    function constant_module(name)
        tsm = ThreadSafeModule("jit")
        tsm() do mod
            fn = LLVM.Function(mod, name, LLVM.FunctionType(LLVM.Int32Type()))
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                ret!(builder, ConstantInt(Int32(42)))
            end
        end
        tsm
    end
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib
        tsm = constant_module("consumed")
        add!(lljit, jd, tsm)
        @test_throws ArgumentError add!(lljit, jd, tsm)
        @test_throws ArgumentError tsm(mod -> nothing)
        dispose(tsm)    # does nothing
        @dispose tsm=constant_module("scoped") begin
            add!(lljit, jd, tsm)
        end
        @test ccall(pointer(lookup(lljit, "scoped")), Int32, ()) == 42

        # the modules that IR transformations receive are borrowed
        borrowed = Ref{Any}(nothing)
        transform!(lljit.ir_transform_layer) do tsm, mr
            borrowed[] = tsm
            @test tsm(mod -> mod isa LLVM.Module)
            @test_throws ArgumentError dispose(tsm)
            @test_throws ArgumentError add!(lljit, jd, tsm)
        end
        add!(lljit, jd, constant_module("transformed"))
        @test ccall(pointer(lookup(lljit, "transformed")), Int32, ()) == 42
        # and only during the transformation
        @test_throws ArgumentError borrowed[](mod -> nothing)
    end

    # object linking layers are consumed by returning them from a creator
    layer = Ref{Any}(nothing)
    builder = LLJITBuilder()
    linking_layer_creator!(builder) do es, triple
        layer[] = ObjectLinkingLayer(es, triple)
    end
    @dispose lljit=LLJIT(builder) begin
        @test layer[] isa ObjectLinkingLayer
        @test_throws ArgumentError register!(layer[], GDBRegistrationListener())
        dispose(layer[])    # does nothing
    end
    @dispose lljit=LLJIT() begin
        @dispose oll=ObjectLinkingLayer(lljit.execution_session) begin end
    end
end

@testset "do-block constructors" begin
    # every resource can be disposed of using a do-block, also once it has been consumed
    @test LLJITBuilder(builder -> 42) == 42
    TargetMachineBuilder() do tmb
        LLJITBuilder() do builder
            target_machine_builder!(builder, tmb)
            LLJIT(builder) do lljit
                es = lljit.execution_session
                ObjectLinkingLayer(oll -> nothing, es)
                DynamicLibrarySearchGenerator(dg -> nothing, lljit)
                DynamicLibrarySearchGenerator(lljit) do dg
                    add!(lljit.main_dylib, dg)
                end
                @test pointer(lookup(lljit, "jl_apply_generic")) != C_NULL
                LocalLazyCallThroughManager(lljit.triple, es) do lctm
                    LocalIndirectStubsManager(lljit.triple) do ism
                        @test lctm isa LLVM.LazyCallThroughManager
                        @test ism isa LLVM.IndirectStubsManager
                    end
                end
                ThreadSafeContext() do ts_ctx
                    ThreadSafeModule("unused") do tsm
                        @test tsm(mod -> mod.name) == "unused"
                    end
                    ThreadSafeModule("added") do tsm
                        add!(lljit, lljit.main_dylib, tsm)
                    end
                end
            end
        end
    end
end

@testset "Absolute symbols" begin
    @dispose lljit=LLJIT() begin
        jd = lljit.main_dylib
        data = Ref{Int32}(42)
        ptr = pointer_from_objref(data)

        define!(jd, absolute_symbols(mangle(lljit, "gv") => ptr))
        @test pointer(lookup(lljit, "gv")) == ptr

        # multiple symbols, flags, and collections
        define!(jd, absolute_symbols([
            mangle(lljit, "gv1") => ptr + 1,
            mangle(lljit, "gv2") => (UInt(ptr) + 2, SymbolFlags(callable=true)),
        ]))
        define!(jd, absolute_symbols(
            Dict(mangle(lljit, "gv3") => OrcTargetAddress(ptr + 3))))
        @test pointer(lookup(lljit, "gv1")) == ptr + 1
        @test pointer(lookup(lljit, "gv2")) == ptr + 2
        @test pointer(lookup(lljit, "gv3")) == ptr + 3

        # duplicate definitions are rejected
        @test_throws LLVMException define!(jd,
            absolute_symbols(mangle(lljit, "gv") => ptr + 4))
        @test pointer(lookup(lljit, "gv")) == ptr
        sym = mangle(lljit, "dup")
        @test_throws ArgumentError absolute_symbols([sym => ptr, sym => ptr])
        release(sym)
    end
end

@testset "Data layout" begin
    @dispose lljit=LLJIT() dl=LLVM.DataLayout(lljit) begin
        @test lljit.datalayout_string isa String
        @test string(dl) == lljit.datalayout_string
        @test LLVM.pointersize(dl) == sizeof(Ptr{Cvoid})
    end
end

@testset "Symbols" begin
    @dispose lljit=LLJIT() begin
        sym = mangle(lljit, "foo")
        @test String(sym) == string(sym) == (lljit.global_prefix == 0 ? "foo" : "_foo")
        @test occursin(repr(String(sym)), repr(sym))

        # symbols are interned
        other = intern(lljit.execution_session, String(sym))
        @test other == sym
        release(other)
        release(sym)
    end

    flags = SymbolFlags()
    @test flags.exported && !flags.callable && !flags.weak
    @test flags.target_flags == 0
    @test sprint(show, flags) == "SymbolFlags()"
    raw = convert(LLVM.API.LLVMJITSymbolFlags, flags)
    @test raw.GenericFlags == UInt8(LLVM.API.LLVMJITSymbolGenericFlagsExported)
    @test raw.TargetFlags == 0

    flags = SymbolFlags(exported=false, callable=true, weak=true, target_flags=1)
    @test sprint(show, flags) == "SymbolFlags(exported=false, callable=true, weak=true, target_flags=1)"
    raw = convert(LLVM.API.LLVMJITSymbolFlags, flags)
    @test raw.GenericFlags == UInt8(LLVM.API.LLVMJITSymbolGenericFlagsCallable) |
                              UInt8(LLVM.API.LLVMJITSymbolGenericFlagsWeak)
    @test raw.TargetFlags == 1
    @test_throws ArgumentError SymbolFlags(target_flags=256)

    # raw C API structures are rejected before the names are handed over to LLVM
    @dispose lljit=LLJIT() begin
        sym = mangle(lljit, "raw")
        raw_flags = convert(LLVM.API.LLVMJITSymbolFlags, SymbolFlags())
        @test_throws MethodError absolute_symbols(Ref(LLVM.API.LLVMOrcCSymbolMapPair(
            sym, LLVM.API.LLVMJITEvaluatedSymbol(0, raw_flags))))
        @test_throws ArgumentError absolute_symbols([sym => (0, raw_flags)])
        @test_throws ArgumentError absolute_symbols([sym => "0x0"])
        @test_throws ArgumentError absolute_symbols(["raw" => 0])
        @test_throws ArgumentError absolute_symbols(
            [sym => (0, SymbolFlags(materialization_side_effects_only=true))])
        nroots = length(LLVM.CUSTOM_MU_ROOTS)
        @test_throws ArgumentError CustomMaterializationUnit("rawMU", [sym => raw_flags],
                                                             mr -> nothing, (jd, s) -> nothing)
        @test_throws MethodError CustomMaterializationUnit("rawMU", Ref(sym => raw_flags),
                                                           mr -> nothing, (jd, s) -> nothing)
        @test length(LLVM.CUSTOM_MU_ROOTS) == nroots
        lctm = LocalLazyCallThroughManager(lljit.triple, lljit.execution_session)
        ism = LocalIndirectStubsManager(lljit.triple)
        try
            # lazy reexports need to be callable
            @test_throws ArgumentError lazy_reexports(lctm, ism, lljit.main_dylib,
                                                      [sym => (sym, SymbolFlags())])
        finally
            dispose(lctm)
            dispose(ism)
        end
        release(sym)
    end
end

@testset "Lookup in JITDylib" begin
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        es = lljit.execution_session
        jd = JITDylib(es, "other")

        ts_mod = ThreadSafeModule("jit")
        ts_mod() do mod
            fn = LLVM.Function(mod, "other_fn", LLVM.FunctionType(LLVM.Int32Type()))
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                ret!(builder, ConstantInt(Int32(42)))
            end
        end
        add!(lljit, jd, ts_mod)

        # the main JITDylib doesn't link against the other one
        @test_throws LLVMException lookup(lljit, "other_fn")
        addr = lookup(lljit, jd, "other_fn")
        @test ccall(pointer(addr), Int32, ()) == 42

        @test_throws LLVMException lookup(lljit, jd, "undefined")
        @test isempty(LLVM.SESSION_LOOKUP_ROOTS)
    end
end

@testset "ResourceTracker" begin
    function constant_module(name, val)
        ts_mod = ThreadSafeModule("jit")
        ts_mod() do mod
            fn = LLVM.Function(mod, name, LLVM.FunctionType(LLVM.Int32Type()))
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                ret!(builder, ConstantInt(Int32(val)))
            end
        end
        ts_mod
    end

    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib

        add!(lljit, jd, constant_module("untracked", 1))
        ResourceTracker(jd) do rt
            add!(lljit, rt, constant_module("tracked", 2))
            @test ccall(pointer(lookup(lljit, "tracked")), Int32, ()) == 2

            # removing a tracker only removes its code
            remove!(rt)
            @test_throws LLVMException lookup(lljit, "tracked")
            @test ccall(pointer(lookup(lljit, "untracked")), Int32, ()) == 1
        end

        # code of released trackers stays around
        ResourceTracker(jd) do rt
            add!(lljit, rt, constant_module("released", 3))
        end
        @test ccall(pointer(lookup(lljit, "released")), Int32, ()) == 3

        # transferring resources
        rt1 = ResourceTracker(jd)
        rt2 = ResourceTracker(jd)
        add!(lljit, rt1, constant_module("transferred", 4))
        transfer!(rt2, rt1)
        remove!(rt1)
        @test ccall(pointer(lookup(lljit, "transferred")), Int32, ()) == 4
        remove!(rt2)
        @test_throws LLVMException lookup(lljit, "transferred")
        dispose(rt1)
        dispose(rt2)

        # the default tracker tracks code added without an explicit tracker
        rt = jd.default_resource_tracker
        remove!(rt)
        dispose(rt)
        @test_throws LLVMException lookup(lljit, "untracked")
        @test_throws LLVMException lookup(lljit, "released")

        # LLVM doesn't retain the default tracker for us, so disposing of it shouldn't
        # release it (which used to corrupt the heap)
        for _ in 1:10
            dispose(jd.default_resource_tracker)
        end
        add!(lljit, jd, constant_module("after_default", 5))
        @test ccall(pointer(lookup(lljit, "after_default")), Int32, ()) == 5
    end
end

@testset "IRTransformLayer" begin
    function constant_module(name, val)
        ts_mod = ThreadSafeModule("jit")
        ts_mod() do mod
            fn = LLVM.Function(mod, name, LLVM.FunctionType(LLVM.Int32Type()))
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                ret!(builder, ConstantInt(Int32(val)))
            end
        end
        ts_mod
    end

    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib
        il = lljit.ir_transform_layer

        # transformations can modify modules in place
        transformed = String[]
        transform!(il) do tsm, mr
            tsm() do mod
                for fn in mod.functions
                    push!(transformed, fn.name)
                    fn.entry.terminator.operands[1] = ConstantInt(Int32(2))
                end
            end
        end
        add!(lljit, jd, constant_module("transformed", 1))
        @test ccall(pointer(lookup(lljit, "transformed")), Int32, ()) == 2
        @test transformed == ["transformed"]

        # exceptions fail materialization
        transform!(il) do tsm, mr
            throw(ArgumentError("transform error"))
        end
        add!(lljit, jd, constant_module("failing", 1))
        @test_throws LLVMException redirect_stderr(devnull) do
            lookup(lljit, "failing")
        end
        try
            check_callback_error!(il)
            @test false
        catch err
            @test err isa LLVM.CallbackException
            @test err.ex isa ArgumentError
        end
        @test check_callback_error!(il) === nothing
        @test length(lljit.roots) == 2
    end
end

@testset "Loading ObjectFile" begin
    @dispose lljit=LLJIT(;tm=LLVM.JITTargetMachine()) begin
        jd = lljit.main_dylib

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
        # the buffer is consumed, after which disposing of it does nothing
        @dispose buf=MemoryBuffer(obj) begin
            add!(lljit, jd, buf)
            @test_throws ArgumentError add!(lljit, jd, buf)
        end

        addr = lookup(lljit, sym)

        @test pointer(addr) != C_NULL

        empty!(jd)
        @test_throws LLVMException lookup(lljit, sym)

        # invalid objects are rejected (and consumed)
        @dispose buf=MemoryBuffer(rand(UInt8, 64)) begin
            @test_throws LLVMException add!(lljit, jd, buf)
            @test_throws ArgumentError length(buf)
        end
    end

    @dispose lljit=LLJIT(; tm=LLVM.JITTargetMachine()) begin
        jd = lljit.main_dylib

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
            define!(jd, absolute_symbols(mangle(lljit, "gv") => ptr))

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
        tm === nothing || target_machine_builder!(builder, TargetMachineBuilder(tm()))
        creator === nothing || linking_layer_creator!(creator, builder)
        @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT(builder) begin
            jd = lljit.main_dylib

            ts_mod = ThreadSafeModule("jit")
            sym = "SomeFunctionOLL"

            ts_mod() do mod
                T = LLVM.DoubleType()
                ft = LLVM.FunctionType(T, [T])
                fn = LLVM.Function(mod, sym, ft)

                @dispose builder=IRBuilder() begin
                    entry = BasicBlock(fn, "entry")
                    position!(builder, LLVM.at_end(entry))
                    ret!(builder, fadd!(builder, fn.parameters[1], ConstantFP(T, 1.25)))
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
            release(mangled)
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
    let tm = () -> LLVM.JITTargetMachine()
        tm_flags = jit_symbol_flags(; tm)
        @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es, triple); tm) ==
              tm_flags
        @test jit_symbol_flags((es, triple) -> ObjectLinkingLayer(es); tm) == tm_flags
    end

    # COFF objects need additional configuration (JuliaLLVM/LLVM.jl#395).
    # RuntimeDyld can link them on any host, so test that everywhere.
    if Sys.ARCH == :x86_64 && :X86 in LLVM.backends()
        coff_triple = "x86_64-w64-windows-gnu"
        tm = () -> LLVM.TargetMachine(LLVM.Target(; triple=coff_triple), coff_triple;
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
    linking_layer_creator!(builder) do es, triple
        throw(ArgumentError("object layer creator error"))
    end
    GC.gc()
    try
        LLJIT(builder)
        @test false
    catch err
        @test err isa LLVM.CallbackException
        @test err.ex isa ArgumentError
        @test occursin("object layer creator error", string(err.ex))
        @test !isempty(err.processed_bt)
    end
end

@testset "Lazy" begin
    @dispose ts_ctx=ThreadSafeContext() lljit=LLJIT() begin
        jd = lljit.main_dylib
        es = lljit.execution_session

        lctm = LocalLazyCallThroughManager(lljit.triple, es)
        ism = LocalIndirectStubsManager(lljit.triple)
        try
            # 1. define entry symbol
            entry_sym = "foo_entry"
            mu = lazy_reexports(lctm, ism, jd,
                                     [mangle(lljit, entry_sym) => mangle(lljit, "foo")])
            define!(jd, mu)

            # 2. Lookup address of entry symbol
            addr = lookup(lljit, entry_sym)
            @test pointer(addr) != C_NULL

            # 3. add MU that will call back into the compiler
            requested = String[]
            function materialize(mr)
                syms = mr.requested_symbols
                @assert length(syms) == 1
                append!(requested, String.(collect(syms)))

                # syms contains mangled symbols
                # we need to emit an unmangled one

                ts_mod = ThreadSafeModule("jit")
                ts_mod() do mod
                    dl = lljit.datalayout_string
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

                il = lljit.ir_transform_layer
                emit!(il, mr, ts_mod)

                return nothing
            end

            function discard(jd, sym)
            end

            symbols = [mangle(lljit, "foo") => SymbolFlags(callable=true)]
            mu = CustomMaterializationUnit("fooMU", symbols, materialize, discard)
            define!(jd, mu)

            @test ccall(pointer(addr), Int32, (Int32, Int32), 1, 2) == 3
            foo = mangle(lljit, "foo")
            @test requested == [String(foo)]
            release(foo)
            @test !(mu in LLVM.CUSTOM_MU_ROOTS)
        finally
            dispose(lctm)
            dispose(ism)
        end
    end
end

end
