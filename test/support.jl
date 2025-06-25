@testset "support" begin

function execute_code(code)
    script = """
        using LLVM
        $code"""
    cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) -e $script`

    out = Pipe()
    err = Pipe()
    proc = run(pipeline(cmd, stdout=out, stderr=err), wait=false)
    close(out.in)
    close(err.in)
    wait(proc)
    out = read(out, String)
    err = read(err, String)
    return (; out, err)
end

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
    # use after dispose
    let (; out, err) =
        execute_code("""buf = LLVM.MemoryBuffer(UInt8[])
                        dispose(buf)
                        length(buf)""")
        @test occursin("An instance of MemoryBuffer is being used after it was disposed of.", out)
    end

    # double dispose
    let (; out, err) =
        execute_code("""buf = LLVM.MemoryBuffer(UInt8[])
                        dispose(buf)
                        dispose(buf)""")
        @test occursin("An instance of MemoryBuffer is being disposed of twice.", out)
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
end
end

end
