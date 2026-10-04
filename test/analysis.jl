@testset "analysis" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.Int32Type())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, LLVM.at_end(entry))

    ret!(builder)

    @test_throws LLVMException verify(mod)
    @test_throws LLVMException verify(fn)

    # the error contains the verifier's message
    @test_throws "Function return type does not match operand type of return inst!" verify(mod)
    @test_throws "Function return type does not match operand type of return inst!" verify(fn)
    @test occursin("does not match", verification_error(mod))
    @test occursin("does not match", verification_error(fn))
end

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, LLVM.at_end(entry))

    ret!(builder)

    @test verify(mod) === nothing
    @test verify(fn) === nothing
    @test verification_error(mod) === nothing
    @test verification_error(fn) === nothing
end

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    @test isempty(fn.parameters)

    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int1Type()])
    fn = LLVM.Function(mod, "SomeOtherFunction", ft)
    @test !isempty(fn.parameters)

    bb1 = BasicBlock(fn, "entry")
    bb2 = BasicBlock(fn, "then")
    bb3 = BasicBlock(fn, "else")

    position!(builder, LLVM.at_end(bb1))
    allocinst1 = alloca!(builder, LLVM.Int8Type())
    brinst = br!(builder, fn.parameters[1], bb2, bb3)
    @test brinst.opcode == LLVM.API.LLVMBr

    position!(builder, LLVM.at_end(bb2))
    retinst2 = ret!(builder)

    position!(builder, LLVM.at_end(bb3))
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

    @test DomTree(dt -> dominates(dt, allocinst1, brinst), fn)

    @test PostDomTree(pdt -> dominates(pdt, brinst, allocinst1), fn)
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


# run `f(fn, am)` with the analysis manager of a pass that runs on function `name` of `mod`
function with_analyses(f, mod::LLVM.Module, name::String)
    result = Ref{Any}()
    run!(FunctionPass("with-analyses", (fn, am) -> begin
        fn.name == name && (result[] = f(fn, am))
        return false
    end; analyses=true), mod.functions[name])
    return result[]
end

@testset "assumption cache" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @llvm.assume(i1)
        define i64 @f(i64 %n, i64 %i) {
        entry:
          %c = icmp ult i64 %n, 1024
          call void @llvm.assume(i1 %c)
          call void @llvm.assume(i1 true) [ "align"(i64 %i, i64 8) ]
          %s = add i64 %i, %n
          ret i64 %s
        }""") begin
        with_analyses(mod, "f") do fn, am
            ac = am[AssumptionCache]
            n, i = fn.parameters
            entry = only(fn.blocks)
            insts = collect(entry.instructions)
            cond_assume, bundle_assume = insts[2], insts[3]

            @test collect(ac) == [cond_assume, bundle_assume]
            @test eltype(ac) == CallInst
            @test ac[n] == [AssumptionEntry(cond_assume, nothing)]
            @test ac[i] == [AssumptionEntry(bundle_assume, 1)]
            @test isempty(ac[insts[4]])

            # new assumptions need to be registered
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.before(entry.terminator))
                c = icmp!(builder, LLVM.API.LLVMIntSGE, i, ConstantInt(Int64(0)))
                assume = call!(builder, mod.functions["llvm.assume"].function_type,
                               mod.functions["llvm.assume"], [c])
                @test length(ac[i]) == 1
                @test push!(ac, assume) === ac
                @test length(collect(ac)) == 3
                @test any(e -> e.assume == assume && e.bundle_index === nothing, ac[i])
                @test_throws ArgumentError push!(ac, c)
            end

            # deleted assumptions are dropped automatically
            erase!(bundle_assume)
            @test length(collect(ac)) == 2

            # clearing the cache rescans the function
            @test empty!(ac) === ac
            @test length(collect(ac)) == 2
        end
    end
end

@testset "value tracking" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @llvm.assume(i1)
        define i64 @f(i64 noundef %n, i64 %i, i1 %b) {
        entry:
          %m = and i64 %i, 255
          %z = zext i32 0 to i64
          br i1 %b, label %next, label %exit
        next:
          %c = icmp ult i64 %n, 1024
          call void @llvm.assume(i1 %c)
          %s = add nuw i64 %m, 1
          %k = or i64 %i, 1
          %d = udiv i64 %n, %k
          ret i64 %s
        exit:
          ret i64 %n
        }""") begin
        with_analyses(mod, "f") do fn, am
            ac = am[AssumptionCache]
            dt = am[DomTree]
            n, i, b = fn.parameters
            entry, next, exit = fn.blocks
            m = first(entry.instructions)
            assume = collect(next.instructions)[2]
            s = collect(next.instructions)[3]
            k = collect(next.instructions)[4]
            d = collect(next.instructions)[5]

            # ranges, from instructions and from assumptions that hold at a context
            @test ConstantRange(m) == ConstantRange(64, 0, 256)
            @test ConstantRange(n) == ConstantRange(64)
            @test ConstantRange(n; at=d, assumptions=ac, domtree=dt) ==
                  ConstantRange(64, 0, 1024)
            @test ConstantRange(n; at=exit.terminator, assumptions=ac, domtree=dt) ==
                  ConstantRange(64)
            @test ConstantRange(ConstantInt(Int32(-1)); signed=true) == ConstantRange(32, -1)
            @test_throws ArgumentError ConstantRange(fn)

            # known bits, which need a data layout
            @test KnownBits(m) == KnownBits(64, ~UInt64(0xff), 0)
            @test KnownBits(ConstantInt(Int8(5)); datalayout=LLVM.DataLayout("")) ==
                  KnownBits(8, 0xfa, 0x05)
            @test_throws ArgumentError KnownBits(ConstantInt(Int8(5)))
            @test ConstantRange(KnownBits(n; at=d, assumptions=ac, domtree=dt)) ==
                  ConstantRange(64, 0, 1024)

            # assumptions and poison
            @test is_valid_assume_for_context(assume, d; domtree=dt)
            @test !is_valid_assume_for_context(assume, exit.terminator; domtree=dt)
            @test is_guaranteed_not_to_be_poison(n)    # noundef
            @test !is_guaranteed_not_to_be_poison(i)
            # a poison divisor is undefined behavior, while an unused poison value isn't
            @test program_undefined_if_poison(k)
            @test !program_undefined_if_poison(s)
        end
    end
end

@testset "lazy value info" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i64 @f(i64 %i, i64 %n) {
        entry:
          %c = icmp ult i64 %i, 100
          br i1 %c, label %then, label %exit
        then:
          %x = add i64 %i, 1
          %y = icmp ult i64 %i, %n
          br label %exit
        exit:
          %p = phi i64 [ %x, %then ], [ 0, %entry ]
          %q = mul i64 %p, 2
          ret i64 %q
        }""") begin
        with_analyses(mod, "f") do fn, am
            lvi = am[LazyValueInfo]
            i, n = fn.parameters
            entry, then, exit = fn.blocks
            x, y = collect(then.instructions)[1:2]
            p, q = collect(exit.instructions)[1:2]

            # ranges at a context instruction, using dominating branch conditions and PHIs
            @test ConstantRange(lvi, i; at=x) == ConstantRange(64, 0, 100)
            @test ConstantRange(lvi, i; at=entry.terminator) == ConstantRange(64)
            @test ConstantRange(lvi, p; at=q) == ConstantRange(64, 0, 101)

            # ranges on an edge
            @test ConstantRange(lvi, i; from=entry, to=then) == ConstantRange(64, 0, 100)
            @test ConstantRange(lvi, i; from=entry, to=exit) == ConstantRange(64, 100, 0)

            # ranges at a use
            if LLVM.version() >= v"16"
                use = only(filter(u -> u.user == y, collect(n.uses)))
                @test ConstantRange(lvi, use) == ConstantRange(64)
                use = only(filter(u -> u.user == x, collect(i.uses)))
                @test ConstantRange(lvi, use) == ConstantRange(64, 0, 100)
            end

            @test_throws ArgumentError ConstantRange(lvi, i)
            @test_throws ArgumentError ConstantRange(lvi, i; from=entry)
            @test_throws ArgumentError ConstantRange(lvi, i; from=entry, to=then,
                                                     undef_allowed=true)
            @test_throws ArgumentError ConstantRange(lvi, fn; at=x)
        end
    end
end

end
