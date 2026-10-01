@testset "string arguments" begin

# a string type with UTF-16 code units, so that `ncodeunits` doesn't match the number of
# bytes of its `String` conversion (as passed to LLVM) for non-ASCII text. only covers the
# Basic Multilingual Plane, which suffices for these tests.
struct UTF16TestString <: AbstractString
    units::Vector{UInt16}
end
UTF16TestString(s::String) = UTF16TestString(transcode(UInt16, s))
Base.ncodeunits(s::UTF16TestString) = length(s.units)
Base.codeunit(::UTF16TestString) = UInt16
Base.codeunit(s::UTF16TestString, i::Integer) = s.units[i]
Base.isvalid(s::UTF16TestString, i::Int) = checkbounds(Bool, s.units, i)
Base.iterate(s::UTF16TestString, i::Int=1) =
    i > ncodeunits(s) ? nothing : (Char(s.units[i]), i + 1)

let s = UTF16TestString("∇f")
    @test ncodeunits(s) == 2
    @test String(s) == "∇f"
    @test ncodeunits(String(s)) == 4
end

@testset "encoding" begin
    # functions that pass the length of a string to LLVM measure its `String` conversion
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        attr = StringAttribute(UTF16TestString("kïnd"), UTF16TestString("välue"))
        @test attr.kind == "kïnd"
        @test attr.value == "välue"

        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "f", ft)
        attrs = fn.function_attributes
        push!(attrs, attr)
        @test haskey(attrs, UTF16TestString("kïnd"))
        @test attrs[UTF16TestString("kïnd")] == attr
        delete!(attrs, UTF16TestString("kïnd"))
        @test isempty(attrs)

        @dispose builder=IRBuilder() begin
            position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
            call = call!(builder, ft, fn)
            ret!(builder)
            attrs = call.function_attributes
            push!(attrs, attr)
            @test haskey(attrs, UTF16TestString("kïnd"))
            @test attrs[UTF16TestString("kïnd")] == attr
            delete!(attrs, UTF16TestString("kïnd"))
            @test isempty(attrs)
        end

        @test Intrinsic(UTF16TestString("llvm.trap")) == Intrinsic("llvm.trap")

        DIBuilder(mod) do dib
            file = LLVM.file!(dib, UTF16TestString("tëst.jl"), UTF16TestString("/tmp/∂ir"))
            @test file.filename == "tëst.jl"
            @test file.directory == "/tmp/∂ir"
            ty = LLVM.basic_type!(dib, UTF16TestString("∇f"), 64, 0x05)
            @test ty.name == "∇f"
        end
    end
end

end
