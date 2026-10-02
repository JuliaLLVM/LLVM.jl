@testset "datalayout" begin

dlstr = "E-p:32:32-f128:128:128"

let
    dl = LLVM.DataLayout(dlstr)
    dispose(dl)
end

LLVM.DataLayout(dlstr) do dl
end

@dispose ctx=Context() dl=LLVM.DataLayout(dlstr) begin
    @test string(dl) == dlstr

    @test occursin(dlstr, sprint(io->show(io,dl)))

    @test dl.byteorder == LLVM.API.LLVMBigEndian
    @test LLVM.pointersize(dl) == LLVM.pointersize(dl, 0) == 4
    @test LLVM.pointersize(dl) isa Int
    LLVM.DataLayout("e-p:32:32-p1:64:64") do dl
        @test LLVM.pointersize(dl, 1) == 8
    end

    @test LLVM.intptr(dl) == LLVM.intptr(dl, 0) == LLVM.Int32Type()

    @test LLVM.bit_size(dl, LLVM.Int32Type()) == 32
    @test LLVM.storage_size(dl, LLVM.Int32Type()) == LLVM.abi_size(dl, LLVM.Int32Type()) == 4
    # types whose size isn't a multiple of 8 bits
    @test LLVM.bit_size(dl, LLVM.Int1Type()) == 1
    @test LLVM.storage_size(dl, LLVM.IntType(9)) == 2
    @test LLVM.abi_size(dl, LLVM.Int32Type()) isa Int

    @test LLVM.abi_alignment(dl, LLVM.Int32Type()) == LLVM.frame_alignment(dl, LLVM.Int32Type()) == LLVM.preferred_alignment(dl, LLVM.Int32Type()) == 4

    @dispose mod=LLVM.Module("SomeModule") begin
        gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
        @test LLVM.preferred_alignment(dl, gv) == 4

        mod.datalayout = dl
        @test string(mod.datalayout) == string(dl)
    end

    elem = [LLVM.Int32Type(), LLVM.FloatType()]
    let st = LLVM.StructType(elem)
        # elements are numbered from 1, like the elements of the struct type
        @test LLVM.element_at(dl, st, 0) == 1
        @test LLVM.element_at(dl, st, 4) == 2
        @test LLVM.offsetof(dl, st, 1) == 0
        @test LLVM.offsetof(dl, st, 2) == 4
        @test_throws BoundsError LLVM.offsetof(dl, st, 3)
        @test_throws ArgumentError LLVM.element_at(dl, st, 8)
    end
    opaque = LLVM.StructType("OpaqueLayout")
    for query in (LLVM.bit_size, LLVM.storage_size, LLVM.abi_size,
                  LLVM.abi_alignment, LLVM.frame_alignment, LLVM.preferred_alignment)
        @test_throws ArgumentError query(dl, opaque)
    end
    @test_throws ArgumentError LLVM.element_at(dl, opaque, 0)
    @test_throws ArgumentError LLVM.offsetof(dl, opaque, 1)

    @test dl.globals_addrspace == 0
    @dispose dl2=LLVM.DataLayout(dlstr*"-G1") begin
        @test dl2.globals_addrspace == 1
    end
end

end
