@testset "target" begin
    @test_throws ArgumentError Target(triple="invalid")
    @test_throws ArgumentError Target(name="invalid")

    host_triple = triple()
    host_t = Target(triple=host_triple)

    host_name = name(host_t)
    description(host_t)

    @test hasjit(host_t)
    @test hastargetmachine(host_t)
    @test hasasmparser(host_t)

    # target iteration
    let ts = targets()
        @test !isempty(ts)
        @test host_t in ts

        @test eltype(ts) == Target

        first(ts)

        for t in ts
            # ...
        end

        @test any(t -> t == host_t, collect(ts))
    end
end
