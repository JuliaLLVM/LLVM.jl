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
end
end

end
