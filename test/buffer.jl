@testset "buffer" begin

data = rand(UInt8, 8)

let
    membuf = MemoryBuffer(data)
    dispose(membuf)
end

MemoryBuffer(data) do buf
end

@test_throws LLVMException MemoryBufferFile("nonexisting")

@dispose membuf=MemoryBuffer(data) begin
    @test pointer(data) != pointer(membuf)
    @test length(membuf) == length(data)
    @test convert(Vector{UInt8}, membuf) == data
end

@dispose membuf=MemoryBuffer(data, "SomeBuffer", false) begin
    @test pointer(data) == pointer(membuf)
end

mktemp() do path, _
    let
        membuf = MemoryBufferFile(path)
        dispose(membuf)
    end

    MemoryBufferFile(path) do buf
    end

    @dispose membuf=MemoryBufferFile(path) begin
    end
end

end
