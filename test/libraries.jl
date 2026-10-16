@testset "libraries" begin

# see test/standalone/libraries.jl for using several libraries
lib = LLVM.library()
@test lib.name === (LLVM.julia_llvm ? :julia : Symbol(:llvm, lib.version.major))
@test LLVM.Library(lib.name) === lib
@test LLVM.version() == lib.version
@test sprint(show, lib) == "LLVM.Library(:$(lib.name))"
@test occursin("LLVM $(lib.version) library :$(lib.name)", sprint(show, MIME"text/plain"(), lib))
@test_throws "No LLVM library is registered as :nonexistent" LLVM.Library(:nonexistent)
@test_throws "reserved" LLVM.register_library!(:julia, lib.path)

@test LLVM.library!(LLVM.library, lib) === lib
@test LLVM.library!(LLVM.library, lib.name) === lib

# objects that own LLVM resources remember their library
@dispose ctx=Context() begin
    @test ctx.library === lib
    @dispose mod=LLVM.Module("foo") builder=IRBuilder() membuf=MemoryBuffer(UInt8[]) begin
        @test mod.library === lib
        @test mod.context.library === lib
        @test builder.library === lib
        @test membuf.library === lib
    end
end

end
