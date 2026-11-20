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

    # types whose size depends on vscale only have an alignment
    sv = LLVM.ScalableVectorType(LLVM.Int32Type(), 4)
    scalable = LLVMType[sv]
    if LLVM.version() >= v"17"
        svcount = LLVM.TargetExtType("aarch64.svcount")
        push!(scalable, LLVM.StructType([sv, sv]), svcount,
              LLVM.ArrayType(LLVM.StructType([sv, sv]), 2), LLVM.ArrayType(svcount, 2))
    end
    if LLVM.version() >= v"18"
        push!(scalable, LLVM.ArrayType(sv, 2))
    end
    for typ in scalable
        @test isscalable(typ)
        @test issized(typ)
        for query in (LLVM.bit_size, LLVM.storage_size, LLVM.abi_size)
            @test_throws ArgumentError query(dl, typ)
        end
        @test LLVM.abi_alignment(dl, typ) > 0
        @test LLVM.preferred_alignment(dl, typ) > 0
        @test LLVM.frame_alignment(dl, typ) > 0
        if typ isa LLVM.StructType
            @test_throws ArgumentError LLVM.offsetof(dl, typ, 1)
            @test_throws ArgumentError LLVM.element_at(dl, typ, 0)
        end
    end
    # other target extension types can have a fixed size
    if LLVM.version() >= v"16"
        let typ = LLVM.TargetExtType("spirv.Event")
            @test !isscalable(typ)
            @test LLVM.abi_size(dl, typ) == LLVM.pointersize(dl)
        end
    end

    @test dl.globals_addrspace == 0
    @dispose dl2=LLVM.DataLayout(dlstr*"-G1") begin
        @test dl2.globals_addrspace == 1
    end

    @test dl.alloca_addrspace == 0
    @dispose dl2=LLVM.DataLayout(dlstr*"-A5-n8:16:32") begin
        @test dl2.alloca_addrspace == 5
        @test LLVM.is_legal_integer(dl2, 16)
        @test !LLVM.is_legal_integer(dl2, 64)
    end
    @test !LLVM.is_legal_integer(dl, 32)
end

# the alignment of pointers that is known from the IR
@dispose ctx=Context(; opaque_pointers=true) dl=LLVM.DataLayout("e-i64:64") mod=parse(LLVM.Module, """
        @g = global i64 0, align 32
        @h = external global i64

        define void @f(ptr align 8 %p, ptr %q, ptr align 4294967296 %r) {
          %a = alloca i64, align 16
          %c = call align 4 ptr @get()
          ret void
        }

        declare ptr @get()""") begin
    f = mod.functions["f"]
    p, q, r = f.parameters
    a, c = f.entry.instructions
    @test LLVM.pointer_alignment(mod.globals["g"], dl) == 32
    @test LLVM.pointer_alignment(mod.globals["h"], dl) == 8    # the ABI alignment of i64
    @test LLVM.pointer_alignment(p, dl) == 8
    @test LLVM.pointer_alignment(q, dl) == 1
    @test LLVM.pointer_alignment(r, dl) === Int64(2)^32
    @test LLVM.pointer_alignment(a, dl) == 16
    @test LLVM.pointer_alignment(c, dl) == 4
    @test LLVM.pointer_alignment(const_inttoptr(ConstantInt(Int64(48)),
                                                LLVM.PointerType()), dl) == 16
    @test_throws ArgumentError LLVM.pointer_alignment(ConstantInt(Int64(48)), dl)
end

end
