@testset "datalayout" begin

dlstr = "E-p:32:32-f128:128:128"

let
    dl = DataLayout(dlstr)
    dispose(dl)
end

DataLayout(dlstr) do dl
end

@dispose ctx=Context() dl=DataLayout(dlstr) begin
    @test string(dl) == dlstr

    @test occursin(dlstr, sprint(io->show(io,dl)))

    @test byteorder(dl) == LLVM.API.LLVMBigEndian
    @test pointersize(dl) == pointersize(dl, 0) == 4

    @test intptr(dl) == intptr(dl, 0) == LLVM.Int32Type()

    @test sizeof(dl, LLVM.Int32Type()) == storage_size(dl, LLVM.Int32Type()) == abi_size(dl, LLVM.Int32Type()) == 4

    @test abi_alignment(dl, LLVM.Int32Type()) == frame_alignment(dl, LLVM.Int32Type()) == preferred_alignment(dl, LLVM.Int32Type()) == 4

    @dispose mod=LLVM.Module("SomeModule") begin
        gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
        @test preferred_alignment(dl, gv) == 4

        datalayout!(mod, dl)
        @test string(datalayout(mod)) == string(dl)
    end

    elem = [LLVM.Int32Type(), LLVM.FloatType()]
    let st = LLVM.StructType(elem)
        @test element_at(dl, st, 4) == 1
        @test offsetof(dl, st, 1) == 4
    end

    @test globals_addrspace(dl) == 0
    @dispose dl2=DataLayout(dlstr*"-G1") begin
        @test globals_addrspace(dl2) == 1
    end
end

end
