@testset "Kaleidoscope" begin

include(joinpath(@__DIR__, "..", "examples", "Kaleidoscope", "Kaleidoscope.jl"))

@testset "recursion" begin
    program = """
        def fib(x) {
            if x < 3 then
                1
            else
                fib(x-1) + fib(x-2)
        }

        def entry() {
            fib(10)
        }
    """

    @dispose ctx=Context() begin
        m = Kaleidoscope.generate_IR(program)
        Kaleidoscope.optimize!(m)
        v = Kaleidoscope.run(m, "entry")
        @test v == 55.0
    end
end

@testset "loops" begin
    program = """
    def fib(x) {
        var a = 1, b = 1
        for i = 3, i < x, 1.0 {
            var c = a + b
            a = b
            b = c
        }
        b
    }

    def entry() {
        fib(10)
    }
    """

    @dispose ctx=Context() begin
        m = Kaleidoscope.generate_IR(program)
        Kaleidoscope.optimize!(m)
        mktemp() do path, io
            Kaleidoscope.write_objectfile(m, path)
        end
        v = Kaleidoscope.run(m, "entry")
        @test v == 55.0
    end
end

@testset "global vars" begin
    program = """
    var x = 5
    var y = 3
    def foo(a b) a + b + x + y

    def entry() foo(2, 3)
    """

    @dispose ctx=Context() begin
        m = Kaleidoscope.generate_IR(program)
        Kaleidoscope.optimize!(m)
        v = Kaleidoscope.run(m, "entry")
        @test v == 13
    end
end

end
