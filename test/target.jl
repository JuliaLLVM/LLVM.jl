@testset "target" begin
    @test_throws ArgumentError LLVM.Target(triple="invalid")
    @test_throws ArgumentError LLVM.Target(name="invalid")

    # target iteration
    let ts = LLVM.targets()
        @test !isempty(ts)

        @test eltype(ts) == LLVM.Target

        first(ts)

        for t in ts
            # ...
        end
    end

    if has_native_target()
        host_triple = LLVM.default_triple()
        host_t = LLVM.Target(triple=host_triple)

        host_name = host_t.name
        host_t.description

        @test LLVM.hasjit(host_t)
        @test LLVM.hastargetmachine(host_t)
        @test LLVM.hasasmbackend(host_t)

        @test host_t in LLVM.targets()
        @test any(t -> t == host_t, collect(LLVM.targets()))
    end
end
