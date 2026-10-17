using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.Analysis, LLVM.ORC
using Test

# whether the tests use the LLVM library that Julia uses, which functionality that integrates
# with Julia's code generator requires
julia_llvm_in_use() = LLVM.library() === LLVM.julia_library

# a script that runs code in a fresh Julia process with LLVM loaded, using the same library
# as the tests (see `LLVM_TEST_LIBRARY` in runtests.jl)
function julia_script(code)
    path = get(ENV, "LLVM_TEST_LIBRARY", "")
    isempty(path) && return """
        using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC
        $code"""
    # LLVM only reads JULIA_LLVM_ARGS for Julia's library, so apply it to this one too
    return """
        using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC
        lib = LLVM.register_library!(:test, $(repr(path)))
        LLVM.API.LLVMExtraParseEnvironmentOptions(lib, "julia", "JULIA_LLVM_ARGS")
        LLVM.library!(() -> include_string(Main, $(repr(code))), lib)"""
end

# run code in a fresh Julia process with LLVM loaded, e.g., to test global state
function execute_code(code; env=())
    script = julia_script(code)
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
