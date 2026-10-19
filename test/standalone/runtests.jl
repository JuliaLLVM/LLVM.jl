# test standalone LLVM libraries next to Julia's LLVM, loading each in a fresh process, with
# and without making its symbols globally available.
#
# usage: julia --project=test test/standalone/runtests.jl LIBRARY...

using Test

# the symbols that a library exports, and the libraries it depends on
function exported_symbols(lib)
    if Sys.isapple()
        [chopprefix(split(line)[end], "_") for line in readlines(`nm -gU $lib`)]
    else
        [split(line)[end] for line in readlines(`nm -D --defined-only $lib`)]
    end
end
function dependencies(lib)
    if Sys.isapple()
        # the first line is the library itself
        [first(split(strip(line))) for line in readlines(`otool -L $lib`)[3:end]]
    else
        [match(r"\[(.+)\]", line)[1] for line in readlines(`readelf -d $lib`)
         if occursin("(NEEDED)", line)]
    end
end

@testset "standalone LLVM" begin
    for lib in ARGS
        @testset "$(basename(lib))" begin
            # only LLVM's C API, LLVMExtra and the functions that support JIT-compiled code
            # are exported, LLVM is linked in statically, and no symbols are unique, which
            # would bind across libraries even when hidden
            @test all(sym -> startswith(sym, "LLVM") || startswith(sym, "llvm_orc_"),
                      exported_symbols(lib))
            @test !any(dep -> occursin("LLVM", dep), dependencies(lib))
            if !Sys.isapple()
                @test !any(contains("UNIQUE"), readlines(`readelf -Ws $lib`))
            end

            for mode in ("local", "global")
                cmd = `$(Base.julia_cmd()) --project=$(Base.active_project())
                       $(joinpath(@__DIR__, "coexist.jl")) $lib $mode`
                @test success(pipeline(cmd; stdout, stderr))
            end
        end
    end
end
