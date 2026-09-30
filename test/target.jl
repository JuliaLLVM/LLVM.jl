@testset "target" begin
    @test_throws ArgumentError LLVM.Target(triple="invalid")
    @test_throws ArgumentError LLVM.Target(name="invalid")

    host_triple = LLVM.default_triple()
    host_t = LLVM.Target(triple=host_triple)

    host_name = host_t.name
    host_t.description

    @test LLVM.hasjit(host_t)
    @test LLVM.hastargetmachine(host_t)
    @test LLVM.hasasmbackend(host_t)

    # target iteration
    let ts = LLVM.targets()
        @test !isempty(ts)
        @test host_t in ts

        @test eltype(ts) == LLVM.Target

        first(ts)

        for t in ts
            # ...
        end

        @test any(t -> t == host_t, collect(ts))
    end
end
