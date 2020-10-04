@testset "orc" begin

@testset "Undefined Symbol" begin
    ctx = Context()
    tm  = JITTargetMachine()
    orc = OrcJIT(tm)

    mod = LLVM.Module("jit", ctx)
    T_Int32 = LLVM.Int32Type(ctx)
    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
    fn = LLVM.Function(mod, "mysum", ft)
    linkage!(fn, LLVM.API.LLVMExternalLinkage)

    fname = mangle(orc, "wrapper")
    wrapper = LLVM.Function(mod, fname, ft)
    # generate IR
    Builder(ctx) do builder
        entry = BasicBlock(wrapper, "entry", ctx)
        position!(builder, entry)

        tmp = call!(builder, fn, [parameters(wrapper)...])
        ret!(builder, tmp)
    end

    triple!(mod, triple(tm))
    ModulePassManager() do pm
        add_library_info!(pm, triple(mod))
        add_transform_info!(pm, tm)
        run!(pm, mod)
    end
    verify(mod)

    orc_mod = compile!(orc, mod)
    @test_throws ErrorException address(orc, fname)

    delete!(orc, orc_mod)
    dispose(orc)
end

@testset "Custom Resolver" begin
    ctx = Context()
    tm  = JITTargetMachine()
    orc = OrcJIT(tm)

    known_functions = Dict{String, OrcTargetAddress}()
    fnames = Dict{String, Int}()
    function lookup(name, ctx)
        name = unsafe_string(name)
        try 
            if !haskey(fnames, name)
                fnames[name] = 0
            end
            fnames[name] += 1

            return known_functions[name].ptr
        catch ex
            @error "Exception during lookup" name exception=(ex, catch_backtrace())
            error("OrcJIT: Could not find symbol")
        end
    end

    mod = LLVM.Module("jit", ctx)
    T_Int32 = LLVM.Int32Type(ctx)
    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
    fn = LLVM.Function(mod, "mysum", ft)
    linkage!(fn, LLVM.API.LLVMExternalLinkage)

    fname = mangle(orc, "wrapper")
    wrapper = LLVM.Function(mod, fname, ft)
    # generate IR
    Builder(ctx) do builder
        entry = BasicBlock(wrapper, "entry", ctx)
        position!(builder, entry)

        tmp = call!(builder, fn, [parameters(wrapper)...])
        ret!(builder, tmp)
    end

    triple!(mod, triple(tm))
    ModulePassManager() do pm
        add_library_info!(pm, triple(mod))
        add_transform_info!(pm, tm)
        run!(pm, mod)
    end
    verify(mod)

    mysum_name = mangle(orc, "mysum")
    known_functions[mysum_name] = OrcTargetAddress(@cfunction(+, Int32, (Int32, Int32)))

    f_lookup = @cfunction($lookup, UInt64, (Cstring, Ptr{Cvoid}))
    GC.@preserve f_lookup begin
        orc_mod = compile!(orc, mod, f_lookup, C_NULL, lazy=true) # will capture f_lookup

        addr = address(orc, fname)
        @test errormsg(orc) == ""
        addr2 = addressin(orc, orc_mod, fname)

        @test addr == addr2
        @test addr.ptr != 0
        @test !haskey(fnames, mysum_name)

        r = ccall(pointer(addr), Int32, (Int32, Int32), 1, 2) # uses f_lookup
        @test r == 3
    end
    
    @test haskey(fnames, mysum_name)
    @test fnames[mysum_name] == 1

    empty!(fnames)
    delete!(orc, orc_mod)
    dispose(orc)
end

@testset "Default Resolver + Stub" begin
    ctx = Context()
    tm  = JITTargetMachine()
    orc = OrcJIT(tm)

    mod = LLVM.Module("jit", ctx)
    T_Int32 = LLVM.Int32Type(ctx)
    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
    fn = LLVM.Function(mod, "mysum", ft)
    linkage!(fn, LLVM.API.LLVMExternalLinkage)

    fname = mangle(orc, "wrapper")
    wrapper = LLVM.Function(mod, fname, ft)
    # generate IR
    Builder(ctx) do builder
        entry = BasicBlock(wrapper, "entry", ctx)
        position!(builder, entry)

        tmp = call!(builder, fn, [parameters(wrapper)...])
        ret!(builder, tmp)
    end

    triple!(mod, triple(tm))
    ModulePassManager() do pm
        add_library_info!(pm, triple(mod))
        add_transform_info!(pm, tm)
        run!(pm, mod)
    end
    verify(mod)

    create_stub!(orc, mangle(orc, "mysum"), OrcTargetAddress(@cfunction(+, Int32, (Int32, Int32))))

    orc_mod = compile!(orc, mod)

    addr = address(orc, fname)
    @test errormsg(orc) == ""

    r = ccall(pointer(addr), Int32, (Int32, Int32), 1, 2)
    @test r == 3

    delete!(orc, orc_mod)
    dispose(orc)
end

@testset "Default Resolver + Global Symbol" begin
    ctx = Context()
    tm  = JITTargetMachine()
    orc = OrcJIT(tm)

    mod = LLVM.Module("jit", ctx)
    T_Int32 = LLVM.Int32Type(ctx)
    ft = LLVM.FunctionType(T_Int32, [T_Int32, T_Int32])
    mysum = mangle(orc, "mysum")
    fn = LLVM.Function(mod, mysum, ft)
    linkage!(fn, LLVM.API.LLVMExternalLinkage)

    fname = mangle(orc, "wrapper")
    wrapper = LLVM.Function(mod, fname, ft)
    # generate IR
    Builder(ctx) do builder
        entry = BasicBlock(wrapper, "entry", ctx)
        position!(builder, entry)

        tmp = call!(builder, fn, [parameters(wrapper)...])
        ret!(builder, tmp)
    end

    triple!(mod, triple(tm))
    ModulePassManager() do pm
        add_library_info!(pm, triple(mod))
        add_transform_info!(pm, tm)
        run!(pm, mod)
    end
    verify(mod)

    # Should do pretty much the same as `@ccallable`
    LLVM.add_symbol(mysum, @cfunction(+, Int32, (Int32, Int32)))
    ptr = LLVM.find_symbol(mysum)
    @test ptr !== C_NULL
    @test ccall(ptr, Int32, (Int32, Int32), 1, 2) == 3

    orc_mod = compile!(orc, mod, lazy=true)

    addr = address(orc, fname)
    @test errormsg(orc) == ""

    r = ccall(pointer(addr), Int32, (Int32, Int32), 1, 2)
    @test r == 3

    delete!(orc, orc_mod)
    dispose(orc)
end

@testset "Loading ObjectFile" begin
    ctx = Context()
    tm = JITTargetMachine()
    orc = OrcJIT(tm) 
    sym = mangle(orc, "SomeFunction")

    mod = LLVM.Module("jit", ctx)
    ft = LLVM.FunctionType(LLVM.VoidType(ctx))
    fn = LLVM.Function(mod, sym, ft)

    Builder(ctx) do builder
        entry = BasicBlock(fn, "entry")
        position!(builder, entry)
        ret!(builder)
    end
    verify(mod)

    obj = emit(tm, mod, LLVM.API.LLVMObjectFile)

    orc_m = add!(orc, MemoryBuffer(obj))
    addr = address(orc, sym)

    @test addr.ptr != 0
    delete!(orc, orc_m)
end

@testset "Stubs" begin
    ctx = Context()
    tm = JITTargetMachine()
    orc = OrcJIT(tm)

    toggle = Ref{Bool}(false)
    on()  = (toggle[] = true; nothing)
    off() = (toggle[] = false; nothing)

    # Note that `CFunction` objects can be GC'd (???)
    # and we capture them below.
    func_on = @cfunction($on, Cvoid, ())
    GC.@preserve func_on begin
        ptr = Base.unsafe_convert(Ptr{Cvoid}, func_on)

        create_stub!(orc, "mystub", OrcTargetAddress(ptr))
        addr = address(orc, "mystub")
    
        @test addr.ptr != 0
        @test toggle[] == false
    
        ccall(pointer(addr), Cvoid, ())
        @test toggle[] == true
    end

    func_off = @cfunction($off, Cvoid, ())
    GC.@preserve func_off begin
        ptr = Base.unsafe_convert(Ptr{Cvoid}, func_off)

        set_stub!(orc, "mystub", OrcTargetAddress(ptr))

        @test addr == address(orc, "mystub")
        @test toggle[] == true
    
        ccall(pointer(addr), Cvoid, ())
        @test toggle[] == false
    end

    dispose(orc)
end

# TODO:
# Test for `callback!`, currently unsure how to trigger that.

end