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

# the wrapper type of another package, around a resource of a foreign library, which uses
# memcheck's instrumentation
struct ExternalThing
    ref::Ptr{Cvoid}
end
# memcheck identifies objects by `===`, and doesn't use these
Base.:(==)(::ExternalThing, ::ExternalThing) = error("ExternalThing ==")
Base.isequal(::ExternalThing, ::ExternalThing) = error("ExternalThing isequal")
Base.hash(::ExternalThing, ::UInt) = error("ExternalThing hash")

@testset "instrumenting other wrapper types" begin
    destroyed = Ptr{Cvoid}[]
    destroy(t) = (push!(destroyed, t.ref); 42)

    session = ExternalThing(Ptr{Cvoid}(1))
    thing = ExternalThing(Ptr{Cvoid}(2))
    @test LLVM.mark_alloc(session) === session
    @test LLVM.mark_alloc(thing; owner=session) === thing
    @test LLVM.mark_use(thing) === thing
    if LLVM.memcheck_enabled
        @test LLVM.tracked_objects[ExternalThing(Ptr{Cvoid}(2))].owner === session
    end

    # an exception in the callback isn't recorded as a disposal, so it can be retried
    @test_throws ErrorException LLVM.mark_dispose(t -> error("failed"), thing)
    @test LLVM.mark_dispose(destroy, ExternalThing(Ptr{Cvoid}(2))) === nothing
    @test destroyed == [thing.ref]
    if LLVM.memcheck_enabled
        @test LLVM.tracked_objects[thing].dispose_bt !== nothing
    end

    # untracked objects aren't checked, and their disposal is only reported
    other = ExternalThing(Ptr{Cvoid}(3))
    @test LLVM.mark_alloc(other) === other
    @test LLVM.mark_untracked(other) === other
    if LLVM.memcheck_enabled
        @test !haskey(LLVM.tracked_objects, other)
    else
        # without memcheck, the callback is always called, also when disposing twice
        LLVM.mark_dispose(destroy, other)
        LLVM.mark_dispose(destroy, other)
        @test destroyed == [thing.ref, other.ref, other.ref]
        empty!(destroyed)
    end

    LLVM.mark_dispose(destroy, session)
    @test last(destroyed) == session.ref
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
                        LLVM.mark_disposed(dl)""")
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
                        LLVM.mark_disposed(ctx)""")
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
                            LLVM.mark_disposed(mod)
                            reuse_ctx && LLVM.mark_disposed(ctx)
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
                        LLVM.mark_disposed(val)""")
        @test success
        @test !occursin("WARNING", out)
    end

    # the disposal is recorded when the entry of the object changes while disposing of it,
    # e.g., because its owner is untracked
    let (; out, err, success) =
        execute_code("""struct Thing
                            ref::Ptr{Cvoid}
                        end
                        s = LLVM.mark_alloc(Thing(Ptr{Cvoid}(1)))
                        t = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)); owner=s)
                        LLVM.mark_dispose(t) do t
                            LLVM.mark_untracked(s)
                        end
                        LLVM.mark_use(t)""")
        @test success
        @test occursin("An instance of Thing is being used after it was disposed of.", out)
        @test count("WARNING", out) == 1
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

    @testset "other wrapper types" begin
        # wrapper types of another package, and a destructor that only records what it
        # destroyed (the handles aren't real)
        prelude = """
            struct Session
                ref::Ptr{Cvoid}
            end
            struct Thing
                ref::Ptr{Cvoid}
            end
            mutable struct Handle
                ref::Ptr{Cvoid}
            end
            for T in (Session, Thing)
                @eval Base.:(==)(::\$T, ::\$T) = error("==")
                @eval Base.isequal(::\$T, ::\$T) = error("isequal")
                @eval Base.hash(::\$T, ::UInt) = error("hash")
            end
            destroyed = Int[]
            destroy(x) = push!(destroyed, Int(x.ref))
            """

        # a clean lifecycle, using wrappers that are reconstructed from their handle, and
        # an owner whose disposal ends the lifetime of the objects it owns
        let (; out, err, success) =
            execute_code(prelude * """
                s = LLVM.mark_alloc(Session(Ptr{Cvoid}(1)))
                t = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)); owner=s)
                u = LLVM.mark_alloc(Thing(Ptr{Cvoid}(3)); owner=s)
                LLVM.mark_use(Thing(Ptr{Cvoid}(2)))
                LLVM.mark_dispose(destroy, Thing(Ptr{Cvoid}(2)))
                LLVM.mark_dispose(destroy, Session(Ptr{Cvoid}(1)))
                println("destroyed: ", destroyed)""")
            @test success
            @test occursin("destroyed: [2, 1]", out)
            @test !occursin("WARNING", out)
        end

        # problems with the lifetime of objects, and of the objects they own
        let (; out, err, success) =
            execute_code(prelude * """
                s = LLVM.mark_alloc(Session(Ptr{Cvoid}(1)))
                t = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)); owner=s)
                v = LLVM.mark_alloc(Thing(Ptr{Cvoid}(4)))
                LLVM.mark_dispose(destroy, v)
                LLVM.mark_use(v)
                LLVM.mark_dispose(destroy, v)
                LLVM.mark_dispose(destroy, Thing(Ptr{Cvoid}(5)))
                LLVM.mark_dispose(destroy, s)
                LLVM.mark_use(t)
                LLVM.mark_dispose(destroy, t)
                println("destroyed: ", destroyed)""")
            @test success
            @test occursin("An instance of Thing is being used after it was disposed of.", out)
            @test occursin("An instance of Thing is being disposed of twice.", out)
            @test occursin("An unknown instance of Thing is being disposed of.", out)
            @test occursin("An instance of Thing is being used after the Session that owns it was disposed of.", out)
            @test occursin("An instance of Thing is being disposed of after the Session that owns it was disposed of", out)
            # objects are not destroyed again, but unknown ones are destroyed
            @test occursin("destroyed: [4, 5, 1]", out)
            @test count("WARNING", out) == 5
        end

        # owners that can't own an object are reported, and the object is tracked without
        # an owner
        let (; out, err, success) =
            execute_code(prelude * """
                s = LLVM.mark_alloc(Session(Ptr{Cvoid}(1)))
                LLVM.mark_dispose(destroy, s)
                a = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)); owner=Session(Ptr{Cvoid}(9)))
                b = LLVM.mark_alloc(Thing(Ptr{Cvoid}(3)); owner=s)
                c = LLVM.mark_alloc(Thing(Ptr{Cvoid}(4)); owner=Thing(Ptr{Cvoid}(4)))
                d = Thing(Ptr{Cvoid}(6))
                LLVM.mark_dispose(LLVM.mark_alloc(Session(Ptr{Cvoid}(5)))) do s
                    LLVM.mark_alloc(d; owner=s)
                end
                # (an earlier object at the same address owns the owner indirectly)
                p = LLVM.mark_alloc(Thing(Ptr{Cvoid}(7)))
                q = LLVM.mark_alloc(Thing(Ptr{Cvoid}(8)); owner=p)
                r = LLVM.mark_alloc(Thing(Ptr{Cvoid}(10)); owner=q)
                p = LLVM.mark_alloc(Thing(Ptr{Cvoid}(7)); owner=r)
                owners = [LLVM.tracked_objects[x].owner for x in (a, b, c, d, p, q, r)]
                println("owners: ", map(o -> o === nothing ? 0 : Int(o.ref), owners))
                println("owning p: ", haskey(LLVM.owned_objects, p))
                for x in (a, b, c, d, r, q, p)
                    LLVM.mark_dispose(destroy, x)
                end""")
            @test success
            @test occursin("An instance of Thing is being allocated with an owning Session that isn't tracked", out)
            @test occursin("An instance of Thing is being allocated with an owning Session that was disposed of, so it is tracked without an owner.", out)
            @test occursin("An instance of Thing is being allocated with an owning Session that is being disposed of", out)
            @test count("An instance of Thing is being allocated with an owning Thing that it owns already", out) == 2
            @test occursin("An instance of Thing was not properly disposed of, and a new allocation will overwrite it.", out)
            @test count("WARNING", out) == 6
            # the objects with an invalid owner have no owner, and the earlier object at
            # the address of `p` doesn't own `q` anymore
            @test occursin("owners: [0, 0, 0, 0, 0, 0, 8]", out)
            @test occursin("owning p: false", out)
        end

        # untracking an object stops checking it, while the objects it owns stay tracked
        # (without an owner), and keep owning their objects
        let (; out, err, success) =
            execute_code(prelude * """
                s = LLVM.mark_alloc(Session(Ptr{Cvoid}(1)))
                t = LLVM.mark_alloc(Thing(Ptr{Cvoid}(2)); owner=s)
                w = LLVM.mark_alloc(Thing(Ptr{Cvoid}(3)); owner=t)
                handed = LLVM.mark_untracked(LLVM.mark_alloc(Thing(Ptr{Cvoid}(4))))
                LLVM.mark_use(handed)
                LLVM.mark_untracked(s)
                LLVM.mark_dispose(destroy, s)
                LLVM.mark_use(t)
                LLVM.mark_dispose(destroy, t)
                LLVM.mark_use(w)
                s2 = LLVM.mark_alloc(Session(Ptr{Cvoid}(5)))
                t2 = LLVM.mark_alloc(Thing(Ptr{Cvoid}(6)); owner=s2)
                LLVM.mark_untracked(s2)""")
            @test success
            @test occursin("An unknown instance of Session is being disposed of.", out)
            @test occursin("An instance of Thing is being used after the Thing that owns it was disposed of.", out)
            @test occursin("An instance of Thing was not properly disposed of.", out)
            @test count("WARNING", out) == 3
        end

        # a mutable wrapper is identified by the Julia object, and wrappers of different
        # types are different objects
        let (; out, err, success) =
            execute_code(prelude * """
                h = LLVM.mark_alloc(Handle(Ptr{Cvoid}(1)))
                LLVM.mark_dispose(destroy, Handle(Ptr{Cvoid}(1)))
                h.ref = Ptr{Cvoid}(2)
                LLVM.mark_use(h)
                LLVM.mark_dispose(destroy, h)
                t = LLVM.mark_alloc(Thing(Ptr{Cvoid}(3)))
                LLVM.mark_dispose(destroy, Session(Ptr{Cvoid}(3)))
                LLVM.mark_use(t)
                LLVM.mark_dispose(destroy, t)""")
            @test success
            @test occursin("An unknown instance of Handle is being disposed of.", out)
            @test occursin("An unknown instance of Session is being disposed of.", out)
            @test count("WARNING", out) == 2
        end
    end
end
end

end
