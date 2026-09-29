using LLVM
using Test

# run code in a fresh Julia process with LLVM loaded, e.g., to test global state
function execute_code(code; env=())
    script = """
        using LLVM
        $code"""
    cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) -e $script`
    cmd = addenv(cmd, env...)

    out = IOBuffer()
    err = IOBuffer()
    proc = run(pipeline(ignorestatus(cmd), stdout=out, stderr=err))
    return (; out=String(take!(out)), err=String(take!(err)), success=success(proc))
end

macro check_ir(inst, str)
    quote
        inst = string($(esc(inst)))
        @test occursin($(str), inst)
    end
end
