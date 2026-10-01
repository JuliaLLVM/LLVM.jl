@testset "buffer" begin

data = rand(UInt8, 8)

let
    membuf = MemoryBuffer(data)
    dispose(membuf)

    # a disposed buffer can't be used, and disposing of it again does nothing
    @test_throws ArgumentError length(membuf)
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

# handing a buffer over to foreign code
let membuf = MemoryBuffer(data)
    ref = LLVM.consume!(membuf)
    @test ref isa LLVM.API.LLVMMemoryBufferRef
    @test_throws ArgumentError length(membuf)
    @test_throws ArgumentError LLVM.consume!(membuf)
    @test_throws ArgumentError LLVM.consume!(membuf; borrow=true)
    dispose(membuf)     # does nothing
    LLVM.API.LLVMDisposeMemoryBuffer(ref)
end

# ... while still borrowing it
let ref = nothing
    @dispose membuf=MemoryBuffer(data) begin
        LLVM.memcheck_enabled && @test haskey(LLVM.tracked_objects, membuf)
        ref = LLVM.consume!(membuf; borrow=true)
        # the new owner frees the buffer, so memcheck stops tracking it, which would
        # otherwise report it as leaked (or as used after being disposed of)
        LLVM.memcheck_enabled && @test !haskey(LLVM.tracked_objects, membuf)
        @test ref == membuf.ref
        @test length(membuf) == length(data)
        @test convert(Vector{UInt8}, membuf) == data

        # it can't be handed over again, also not to LLVM.jl's consuming operations
        @test_throws ArgumentError LLVM.consume!(membuf)
        @test_throws ArgumentError LLVM.consume!(membuf; borrow=true)
        @dispose ctx=Context() begin
            @test_throws ArgumentError parse(LLVM.Module, membuf; lazy=true)
        end

        # disposing of it does nothing, and keeps it usable
        dispose(membuf)
        @test convert(Vector{UInt8}, membuf) == data
    end
    # the new owner frees the buffer
    LLVM.API.LLVMDisposeMemoryBuffer(ref)
end

# only owned buffers can be handed over
let membuf = MemoryBuffer(data)
    dispose(membuf)
    @test_throws ArgumentError LLVM.consume!(membuf; borrow=true)
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
