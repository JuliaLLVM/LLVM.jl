@testset "support" begin

@testset "command-line options" begin

code = """
    using LLVM
    LLVM.clopts("-version")
"""

(; out, err) = execute_code("LLVM.clopts(\"-version\")")
@test occursin("LLVM (http://llvm.org/)", out)

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

    # double dispose
    let (; out, err) =
        execute_code("""ctx = Context()
                        builder = IRBuilder()
                        dispose(builder)
                        dispose(builder)""")
        @test occursin("An instance of IRBuilder is being disposed of twice.", out)
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
end
end

end
