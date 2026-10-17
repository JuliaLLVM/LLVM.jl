@testset "examples" begin

function find_sources(path::String, sources=String[])
    if isdir(path)
        for entry in readdir(path)
            find_sources(joinpath(path, entry), sources)
        end
    elseif endswith(path, ".jl")
        push!(sources, path)
    end
    sources
end

examples_dir = joinpath(@__DIR__, "..", "examples")
examples = find_sources(examples_dir)
filter!(file -> readline(file) != "# EXCLUDE FROM TESTING", examples)
filter!(file -> !occursin("Kaleidoscope", file), examples)
# examples that integrate with Julia's compiler need Julia's LLVM
julia_llvm_in_use() || filter!(file -> !occursin("LLVM.Interop", read(file, String)), examples)

cd(examples_dir) do
    @testset for example in examples
        mod = @eval module $(gensym()) end
        @eval mod begin
            redirect_stdout(devnull) do
                include($example)
            end
        end
    end
end

end
