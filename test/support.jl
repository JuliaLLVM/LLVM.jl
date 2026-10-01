@testset "support" begin

@testset "command-line options" begin

code = """
    using LLVM
    LLVM.clopts("-version")
"""

(; out, err) = execute_code("LLVM.clopts(\"-version\")")
@test occursin("LLVM (http://llvm.org/)", out)

end

@testset "adopt" begin
    # without memcheck, adopting only checks that the object can be adopted
    @dispose ctx=Context() begin
        ref = LLVM.API.LLVMModuleCreateWithNameInContext("foreign", ctx)
        mod = LLVM.adopt(LLVM.Module(ref))
        @test mod isa LLVM.Module
        dispose(mod)
    end

    data = Vector{UInt8}("hello")
    ref = LLVM.API.LLVMCreateMemoryBufferWithMemoryRangeCopy(pointer(data), length(data), "")
    buf = LLVM.MemoryBuffer(ref)
    @test LLVM.adopt(buf) === buf
    LLVM.consume!(buf)
    LLVM.API.LLVMDisposeMemoryBuffer(buf.ref)
    @test_throws ArgumentError LLVM.adopt(buf)
end

if LLVM.memcheck_enabled
@testset "memcheck" begin
    # use after dispose (of an object that doesn't track its ownership, unlike, e.g., a
    # memory buffer, which rejects such uses)
    let (; out, err) =
        execute_code("""ctx = Context()
                        builder = IRBuilder()
                        dispose(builder)
                        LLVM.API.LLVMGetInsertBlock(builder)""")
        @test occursin("An instance of IRBuilder is being used after it was disposed of.", out)
    end

    # double dispose (which is only reported, as freeing the object again would crash, or
    # hang the process when the C library aborts while holding a lock)
    let (; out, err, success) =
        execute_code("""ctx = Context()
                        builder = IRBuilder()
                        dispose(builder)
                        dispose(builder)
                        dispose(ctx)""")
        @test occursin("An instance of IRBuilder is being disposed of twice.", out)
        @test !occursin("being used after it was disposed of", out)
        @test success
    end

    # unrelated dispose
    let (; out, err) =
        execute_code("""buf = LLVM.MemoryBuffer(LLVM.API.LLVMMemoryBufferRef(1))
                        dispose(buf)""")
        @test occursin("An unknown instance of MemoryBuffer is being disposed of.", out)
    end

    # missing dispose
    let (; out, err) =
        execute_code("""buf = LLVM.MemoryBuffer(UInt8[])""")
        @test occursin("An instance of MemoryBuffer was not properly disposed of.", out)
    end

    # problems that occur repeatedly are reported once, and summarized at exit
    let (; out, err) =
        execute_code("""ctx = Context()
                        builder = IRBuilder()
                        dispose(builder)
                        for i in 1:10
                            LLVM.API.LLVMGetInsertBlock(builder)
                        end""")
        @test count("is being used after it was disposed of.", out) == 1
        @test occursin("This is memcheck problem #1.", out)
        @test occursin(r"memcheck problem #1 \(\S*IRBuilder used after being disposed of\) has occurred 10 times\.", out)
        @test occursin(r"#1: \S*IRBuilder used after being disposed of, 10 times: allocated at", out)
        @test occursin(r"  - 10 times used at \S*none:6", out)
    end

    # also when the object is used at different locations
    let (; out, err) =
        execute_code("""ctx = Context()
                        builder = IRBuilder()
                        dispose(builder)
                        use1(b) = LLVM.API.LLVMGetInsertBlock(b)
                        use2(b) = LLVM.API.LLVMGetInsertBlock(b)
                        for i in 1:10
                            use1(builder)
                            use2(builder)
                        end""")
        @test count("is being used after it was disposed of.", out) == 1
        @test occursin(r"has occurred 10 times, at 2 locations\.", out)
        @test occursin(r"20 times: allocated at", out)
        @test occursin(r"  - 10 times used at \S*none:5", out)
        @test occursin(r"  - 10 times used at \S*none:6", out)
    end

    # leaks are grouped by where the objects were allocated
    let (; out, err) =
        execute_code("""bufs = [LLVM.MemoryBuffer(UInt8[]) for i in 1:3]
                        buf = LLVM.MemoryBuffer(UInt8[])""")
        @test occursin("3 instances of MemoryBuffer were not properly disposed of.", out)
        @test occursin("An instance of MemoryBuffer was not properly disposed of.", out)
    end

    # reports are not interleaved when stdout is buffered, e.g., when it is a file
    mktemp() do path, io
        close(io)
        script = """using LLVM
                    ctx = LLVM.Context()
                    builder = LLVM.IRBuilder()
                    LLVM.dispose(builder)
                    LLVM.API.LLVMGetInsertBlock(builder)"""
        cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) -e $script`
        run(pipeline(ignorestatus(cmd), stdout=path, stderr=devnull))
        @test occursin("being used after it was disposed of.\nThe object was allocated at:\nStacktrace:",
                       read(path, String))
    end

    # objects can be allocated and disposed of concurrently, e.g., by ORC callbacks, also
    # when they reuse the address of an object that was just disposed of on another thread
    let (; out, err, success) =
        execute_code("""Threads.@threads for i in 1:Threads.nthreads()
                            for j in 1:1000
                                dl = LLVM.DataLayout("e")
                                dispose(dl)
                            end
                        end""";
                     env=("JULIA_NUM_THREADS" => "4",))
        @test success
        @test !occursin("WARNING", out)
    end

    # deterministically: another thread allocates an object at the address of one that
    # is being disposed of, before its disposal has been recorded
    let (; out, err, success) =
        execute_code("""dl = LLVM.DataLayout("e")
                        LLVM.mark_dispose(dl) do dl
                            LLVM.API.LLVMDisposeTargetData(dl)
                            LLVM.mark_alloc(dl)
                        end
                        LLVM.mark_use(dl)
                        LLVM.mark_dispose(dl)""")
        @test success
        @test !occursin("WARNING", out)
    end

    # an object whose disposal failed is still alive
    let (; out, err, success) =
        execute_code("""dl = LLVM.DataLayout("e")
                        try
                            LLVM.mark_dispose(dl) do dl
                                error("failed")
                            end
                        catch
                        end
                        LLVM.mark_use(dl)
                        LLVM.mark_alloc(dl)
                        dispose(dl)""")
        @test success
        @test !occursin("is being used after", out)
        @test occursin("was not properly disposed of, and a new allocation will overwrite it", out)
    end

    # disposing of a context ends the lifetime of its modules. (these tests use
    # `mark_use`, as actually using the module would access freed memory)
    let (; out, err, success) =
        execute_code("""mod = Context() do ctx
                            LLVM.Module("escapee")
                        end
                        LLVM.mark_use(mod)
                        dispose(mod)""")
        @test occursin("An instance of LLVM.Module is being used after the Context that owns it was disposed of.", out)
        @test occursin("The owner was disposed of at:", out)
        # disposing of the module would free it again, so that is only reported
        @test occursin("An instance of LLVM.Module is being disposed of after the Context that owns it was disposed of", out)
        @test success
        # it's not reported as a leak
        @test !occursin("not properly disposed of", out)
    end

    # modules don't need to be disposed of before their context, and contexts and modules
    # can be allocated at the address of ones that were disposed of earlier
    let (; out, err, success) =
        execute_code("""for i in 1:100
                            Context() do ctx
                                mod = LLVM.Module("unused")
                                copy(mod)
                                LLVM.Module("disposed") do mod
                                    mod.name
                                end
                            end
                        end""")
        @test success
        @test !occursin("WARNING", out)
    end

    # also when the context is leaked, as it's disposed of during exception handling
    let (; out, err, success) =
        execute_code("""mod = try
                            Context() do ctx
                                global mod = LLVM.Module("escapee")
                                error("oops")
                            end
                        catch
                            mod
                        end
                        LLVM.mark_use(mod)""")
        @test occursin("An instance of LLVM.Module is being used after the Context that owns it was disposed of.", out)
        @test !occursin("not properly disposed of", out)
    end

    # also when another thread allocates an object at the address of the context before
    # its disposal is recorded (simulated here by doing so while disposing of it)
    let (; out, err, success) =
        execute_code("""ctx = Context()
                        mod = LLVM.Module("escapee")
                        deactivate(ctx)
                        LLVM.mark_dispose(ctx) do ctx
                            LLVM.API.LLVMContextDispose(ctx)
                            LLVM.mark_alloc(ctx)
                        end
                        LLVM.mark_use(mod)
                        LLVM.mark_dispose(ctx)""")
        @test occursin("An instance of LLVM.Module is being used after the Context that owns it was disposed of.", out)
        @test !occursin("not properly disposed of", out)
    end

    # or at the address of one of its modules, which is freed together with the context
    let (; out, err, success) =
        execute_code("""for reuse_ctx in (false, true)
                            ctx = Context()
                            mod = LLVM.Module("freed")
                            deactivate(ctx)
                            LLVM.mark_dispose(ctx) do ctx
                                LLVM.API.LLVMContextDispose(ctx)
                                reuse_ctx && LLVM.mark_alloc(ctx)
                                LLVM.mark_alloc(mod; owner=nothing)
                            end
                            LLVM.mark_use(mod)
                            LLVM.mark_dispose(mod)
                            reuse_ctx && LLVM.mark_dispose(ctx)
                        end""")
        @test success
        @test !occursin("WARNING", out)
    end

    # the same applies to the modules in the context of a thread-safe context, which
    # can't be used after it has been disposed of, even if other objects (e.g., a
    # thread-safe module) keep the context alive
    let (; out, err, success) =
        execute_code("""mod = ThreadSafeContext() do ts_ctx
                            context!(context(ts_ctx)) do
                                LLVM.Module("escapee")
                            end
                        end
                        LLVM.mark_use(mod)
                        dispose(mod)
                        ts_ctx = ThreadSafeContext()
                        tsm = ThreadSafeModule("tsm")
                        clone = tsm() do mod
                            copy(mod)
                        end
                        dispose(ts_ctx)
                        LLVM.mark_use(clone)""")
        @test count("An instance of LLVM.Module is being used after the ThreadSafeContext that owns it was disposed of.", out) == 2
        @test occursin("An instance of LLVM.Module is being disposed of after the ThreadSafeContext that owns it was disposed of", out)
        # the thread-safe module itself remains valid
        @test occursin(r"An instance of \S*ThreadSafeModule was not properly disposed of.", out)
        @test !occursin("An instance of LLVM.Module was not properly disposed of", out)
        @test success
    end

    # modules borrowed from a thread-safe module are not owned by the thread-safe context,
    # which may be disposed of while they're being used
    let (; out, err, success) =
        execute_code("""ts_ctx = ThreadSafeContext()
                        tsm = ThreadSafeModule("tsm")
                        tsm() do mod
                            dispose(ts_ctx)
                            mod.name
                        end
                        tsm() do mod
                            mod.name
                        end
                        dispose(tsm)""")
        @test success
        @test !occursin("WARNING", out)
    end

    # objects that foreign code hands over can be adopted
    let (; out, err, success) =
        execute_code("""ref = LLVM.API.LLVMCreateGenericValueOfInt(LLVM.API.LLVMInt32Type(), 5, 0)
                        dispose(LLVM.GenericValue(ref))
                        ref = LLVM.API.LLVMCreateGenericValueOfInt(LLVM.API.LLVMInt32Type(), 5, 0)
                        val = LLVM.adopt(LLVM.GenericValue(ref))
                        dispose(val)
                        LLVM.mark_use(val)
                        data = Vector{UInt8}("hello")
                        ref = LLVM.API.LLVMCreateMemoryBufferWithMemoryRangeCopy(pointer(data), length(data), "buf")
                        LLVM.adopt(LLVM.MemoryBuffer(ref))""")
        @test count("An unknown instance of", out) == 1
        @test occursin("An instance of LLVM.GenericValue is being used after it was disposed of.", out)
        @test occursin("An instance of MemoryBuffer was not properly disposed of.", out)
        @test success
    end

    # also when foreign code hands over an object at the address of one that is being
    # disposed of, before its disposal has been recorded
    let (; out, err, success) =
        execute_code("""ref = LLVM.API.LLVMCreateGenericValueOfInt(LLVM.API.LLVMInt32Type(), 5, 0)
                        val = LLVM.adopt(LLVM.GenericValue(ref))
                        LLVM.mark_dispose(val) do val
                            LLVM.API.LLVMDisposeGenericValue(val)
                            LLVM.adopt(val)
                        end
                        LLVM.mark_use(val)
                        LLVM.mark_dispose(val)""")
        @test success
        @test !occursin("WARNING", out)
    end

    # adopting an object that's tracked already is reported, and keeps what memcheck knows
    # about it, while an adopted context owns its (adopted) modules
    let (; out, err, success) =
        execute_code("""ctx = LLVM.adopt(Context(LLVM.API.LLVMContextCreate()))
                        mod = LLVM.adopt(LLVM.Module(LLVM.API.LLVMModuleCreateWithNameInContext("foreign", ctx)))
                        LLVM.adopt(ctx)
                        activate(ctx)
                        dispose(ctx)
                        LLVM.mark_use(mod)""")
        @test occursin("An instance of Context is being adopted, but it is owned already", out)
        @test occursin("An instance of LLVM.Module is being used after the Context that owns it was disposed of.", out)
        @test !occursin("not properly disposed of", out)
        @test success
    end

    # modules accessed with `unsafe_module` remain valid after a callback of their
    # thread-safe module, also when accessed during one
    let (; out, err) =
        execute_code("""@dispose ts_ctx=ThreadSafeContext() tsm=ThreadSafeModule("m") begin
                            m = LLVM.unsafe_module(tsm)
                            tsm() do mod
                                mod.name
                            end
                            m.name
                        end
                        @dispose ts_ctx=ThreadSafeContext() tsm=ThreadSafeModule("m") begin
                            m = tsm() do mod
                                LLVM.unsafe_module(tsm)
                            end
                            m.name
                        end""")
        @test !occursin("WARNING", out)
    end

    # objects are identified by `===`, so the checker doesn't call `==` or `hash`, which
    # wrapper types may implement by calling into foreign code
    let (; out, err, success) =
        execute_code("""struct Thing
                            ref::Ptr{Cvoid}
                        end
                        Base.:(==)(::Thing, ::Thing) = error("==")
                        Base.isequal(::Thing, ::Thing) = error("isequal")
                        Base.hash(::Thing, ::UInt) = error("hash")
                        a = LLVM.mark_alloc(Thing(Ptr{Cvoid}(1)))
                        b = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)))
                        LLVM.mark_dispose(Returns(nothing), Thing(Ptr{Cvoid}(1)))
                        LLVM.mark_use(a)
                        LLVM.mark_use(b)
                        LLVM.mark_dispose(Returns(nothing), b)""")
        @test success
        @test occursin("An instance of Thing is being used after it was disposed of.", out)
        @test count("WARNING", out) == 1
    end
end
end

end
