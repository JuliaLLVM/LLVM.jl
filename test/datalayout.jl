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

    @test LLVM.intptr(dl) == LLVM.intptr(dl, 0) == LLVM.Int32Type()

    @test sizeof(dl, LLVM.Int32Type()) == LLVM.storage_size(dl, LLVM.Int32Type()) == LLVM.abi_size(dl, LLVM.Int32Type()) == 4

    @test LLVM.abi_alignment(dl, LLVM.Int32Type()) == LLVM.frame_alignment(dl, LLVM.Int32Type()) == LLVM.preferred_alignment(dl, LLVM.Int32Type()) == 4

    @dispose mod=LLVM.Module("SomeModule") begin
        gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
        @test LLVM.preferred_alignment(dl, gv) == 4

        mod.datalayout = dl
        @test string(mod.datalayout) == string(dl)
    end

    elem = [LLVM.Int32Type(), LLVM.FloatType()]
    let st = LLVM.StructType(elem)
        @test LLVM.element_at(dl, st, 4) == 1
        @test LLVM.offsetof(dl, st, 1) == 4
    end

    @test dl.globals_addrspace == 0
    @dispose dl2=LLVM.DataLayout(dlstr*"-G1") begin
        @test dl2.globals_addrspace == 1
    end
end

end
