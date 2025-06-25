@testset "analysis" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.Int32Type())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    ret!(builder)

    @test_throws LLVMException verify(mod)
    @test_throws LLVMException verify(fn)
end

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    ret!(builder)

    verify(mod)
    verify(fn)
end

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    @test isempty(parameters(fn))

    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int1Type()])
    fn = LLVM.Function(mod, "SomeOtherFunction", ft)
    @test !isempty(parameters(fn))

    bb1 = BasicBlock(fn, "entry")
    bb2 = BasicBlock(fn, "then")
    bb3 = BasicBlock(fn, "else")

    position!(builder, bb1)
    allocinst1 = alloca!(builder, LLVM.Int8Type())
    brinst = br!(builder, parameters(fn)[1], bb2, bb3)
    @test opcode(brinst) == LLVM.API.LLVMBr

    position!(builder, bb2)
    retinst2 = ret!(builder)

    position!(builder, bb3)
    allocinst3 = alloca!(builder, LLVM.Int8Type())
    retinst3 = ret!(builder)

    @dispose domtree = DomTree(fn) begin
        @test  dominates(domtree, allocinst1, brinst)
        @test  dominates(domtree, allocinst1, retinst2)
        @test  dominates(domtree, allocinst1, allocinst3)
        @test  dominates(domtree, allocinst1, retinst3)
        @test !dominates(domtree, brinst, allocinst1)
        @test  dominates(domtree, brinst, retinst2)
        @test  dominates(domtree, brinst, allocinst3)
        @test  dominates(domtree, brinst, retinst3)
        @test !dominates(domtree, retinst2, allocinst1)
        @test !dominates(domtree, retinst2, retinst3)
        @test !dominates(domtree, retinst2, brinst)
        @test !dominates(domtree, retinst2, allocinst3)
        @test !dominates(domtree, retinst3, allocinst1)
        @test !dominates(domtree, retinst3, retinst2)
        @test !dominates(domtree, retinst3, brinst)
        @test !dominates(domtree, retinst3, allocinst3)
        @test !dominates(domtree, allocinst3, allocinst1)
        @test !dominates(domtree, allocinst3, brinst)
        @test !dominates(domtree, allocinst3, retinst2)
        @test  dominates(domtree, allocinst3, retinst3)
    end

    @dispose postdomtree = PostDomTree(fn) begin
        @test !dominates(postdomtree, allocinst1, brinst)
        @test !dominates(postdomtree, allocinst1, retinst2)
        @test !dominates(postdomtree, allocinst1, retinst3)
        @test !dominates(postdomtree, allocinst1, allocinst3)
        @test  dominates(postdomtree, brinst, allocinst1)
        @test !dominates(postdomtree, brinst, retinst2)
        @test !dominates(postdomtree, brinst, retinst3)
        @test !dominates(postdomtree, brinst, allocinst3)
        @test !dominates(postdomtree, retinst2, allocinst1)
        @test !dominates(postdomtree, retinst2, retinst3)
        @test !dominates(postdomtree, retinst2, brinst)
        @test !dominates(postdomtree, retinst2, allocinst3)
        @test !dominates(postdomtree, retinst3, allocinst1)
        @test !dominates(postdomtree, retinst3, retinst2)
        @test !dominates(postdomtree, retinst3, brinst)
        @test  dominates(postdomtree, retinst3, allocinst3)
        @test !dominates(postdomtree, allocinst3, allocinst1)
        @test !dominates(postdomtree, allocinst3, brinst)
        @test !dominates(postdomtree, allocinst3, retinst2)
        @test !dominates(postdomtree, allocinst3, retinst3)
    end
end

end
