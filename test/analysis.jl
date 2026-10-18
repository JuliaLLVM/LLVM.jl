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
    @test brinst.opcode == (LLVM.version() >= v"23" ? LLVM.Opcode.CondBr : LLVM.Opcode.Br)

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
          %fr = freeze i64 %i
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

            # undef, which is a different value at each use, unlike a frozen value
            fr = collect(entry.instructions)[3]
            @test is_guaranteed_not_to_be_undef_or_poison(n)
            @test is_guaranteed_not_to_be_undef_or_poison(fr)
            @test !is_guaranteed_not_to_be_undef_or_poison(i)
            @test is_guaranteed_not_to_be_undef_or_poison(ConstantInt(Int64(1)))
            @test !is_guaranteed_not_to_be_undef_or_poison(UndefValue(LLVM.Int64Type()))
            @test is_guaranteed_not_to_be_poison(UndefValue(LLVM.Int64Type()))

            # known facts
            @test is_known_non_negative(m)
            @test !is_known_positive(m)
            @test is_known_positive(s)
            @test is_known_non_zero(s)
            @test is_known_non_zero(k)
            @test !is_known_non_negative(i)
            @test !is_known_non_zero(i)
            @test !is_known_non_negative(n; use_instruction_info=false)
            @test is_known_non_negative(n; at=d, assumptions=ac, domtree=dt)
            @test !is_known_non_negative(n; at=exit.terminator, assumptions=ac, domtree=dt)
            @test_throws ArgumentError is_known_non_negative(fn)
            @test_throws ArgumentError is_known_positive(ConstantInt(Int8(5)))
            @test is_known_positive(ConstantInt(Int8(5)); datalayout=LLVM.DataLayout(""))

            # sign bits
            @test num_sign_bits(m) == 56
            @test num_sign_bits(i) == 1
            @test num_sign_bits(n; at=d, assumptions=ac, domtree=dt) == 54
            @test num_sign_bits(ConstantInt(Int8(5)); datalayout=LLVM.DataLayout("")) == 5
            @test num_sign_bits(ConstantInt(Int8(-1)); datalayout=LLVM.DataLayout("")) == 8
            @test_throws ArgumentError num_sign_bits(ConstantInt(Int8(5)))
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

@testset "loops and scalar evolution" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i64 @f(i64 %n, i64 %a) {
        entry:
          %b = add i64 %a, 7
          %nz = zext i32 7 to i64
          br label %outer
        outer:
          %j = phi i64 [ 0, %entry ], [ %j.next, %outer.latch ]
          br label %inner
        inner:
          %i = phi i64 [ 0, %outer ], [ %i.next, %inner ]
          %s = phi i64 [ 0, %outer ], [ %s.next, %inner ]
          %i.next = add nuw nsw i64 %i, 1
          %s.next = add i64 %s, %i
          %ci = icmp ult i64 %i.next, 10
          br i1 %ci, label %inner, label %outer.latch
        outer.latch:
          %j.next = add nuw nsw i64 %j, 1
          %cj = icmp ult i64 %j.next, %n
          br i1 %cj, label %outer, label %exit
        exit:
          %r = add i64 %b, %j
          ret i64 %r
        }""") begin
        with_analyses(mod, "f") do fn, am
            li = am[LoopInfo]
            se = am[ScalarEvolution]
            n, a = fn.parameters
            entry, outer, inner, latch, exit = fn.blocks
            b = first(entry.instructions)
            j = first(outer.instructions)
            i = first(inner.instructions)
            r = first(exit.instructions)

            # loops
            @test li[entry] === nothing
            @test li[exit] === nothing
            outer_loop = li[outer]
            inner_loop = li[inner]
            @test outer_loop isa Loop
            @test li[latch] == outer_loop
            @test outer_loop.header == outer
            @test inner_loop.header == inner
            @test outer_loop.depth == 1
            @test inner_loop.depth == 2
            @test outer_loop.parent === nothing
            @test inner_loop.parent == outer_loop
            @test inner in outer_loop
            @test !(outer in inner_loop)
            @test !(exit in outer_loop)
            @test sprint(show, inner_loop) == "Loop(header=\"inner\", depth=2)"

            # the structure of loops
            @test collect(li) == [outer_loop]
            @test outer_loop.subloops == [inner_loop]
            @test isempty(inner_loop.subloops)
            @test outer_loop.blocks == [outer, inner, latch]
            @test inner_loop.blocks == [inner]
            @test_throws BoundsError outer_loop.blocks[4]
            @test_throws Exception outer_loop.blocks[1] = entry
            @test collect(outer_loop.exit_blocks) == [exit]
            @test collect(inner_loop.exit_blocks) == [latch]
            @test outer_loop.latch == latch
            @test inner_loop.latch == inner
            @test outer_loop.preheader == entry
            @test inner_loop.preheader == outer
            @test i in inner_loop
            @test j in outer_loop
            @test !(j in inner_loop)
            @test !(b in outer_loop)
            @test is_loop_invariant(inner_loop, j)
            @test is_loop_invariant(inner_loop, a)
            @test !is_loop_invariant(inner_loop, i)
            @test !is_loop_invariant(outer_loop, i)
            @test is_loop_invariant(outer_loop, ConstantInt(Int64(1)))

            # expressions
            sa = se[a]
            @test sa isa SCEVUnknown
            @test sa.value == a
            @test sa.type == LLVM.Int64Type()
            sb = se[b]
            @test sb isa SCEVAddExpr
            @test length(sb.operands) == 2
            seven = only(filter(x -> x isa SCEVConstant, sb.operands))
            @test convert(Int, seven.value) == 7
            @test se[b] == sb                       # expressions are uniqued
            @test sprint(show, sb) == "SCEVAddExpr((7 + %a))"

            # add recurrences
            si = se[i]
            @test si isa SCEVAddRecExpr
            @test si.loop == inner_loop
            @test ConstantRange(se, si) == ConstantRange(64, 0, 10)
            @test ConstantRange(se, si; signed=true) == ConstantRange(64, 0, 10)
            @test contains_scev(si, SCEVAddRecExpr)
            @test !contains_scev(sb, SCEVAddRecExpr)
            @test contains_scev(sb, SCEVUnknown)
            @test ConstantRange(se, se[ConstantInt(Int64(-1))]) == ConstantRange(64, -1)

            # building expressions
            @test scev_minus(se, sb, sa) == seven
            @test scev_add(se, sa, seven) == sb
            @test scev_add(se, sa) == sa
            @test scev_minus(se, sa, sa) isa SCEVConstant
            @test_throws ArgumentError scev_add(se)
            @test_throws ArgumentError se[fn.blocks[1].terminator]

            # incompatible operands
            s32 = se[ConstantInt(Int32(1))]
            @test_throws ArgumentError scev_add(se, sa, s32)
            @test_throws ArgumentError scev_minus(se, sa, s32)

            @test is_scevable(se, a)
            @test is_scevable(se, LLVM.Int32Type())
            @test !is_scevable(se, LLVM.FloatType())

            # multiplication and casts
            @test scev_mul(se, sa, seven) isa SCEVMulExpr
            @test scev_mul(se, seven, seven) == se[ConstantInt(Int64(49))]
            @test_throws ArgumentError scev_mul(se)
            @test_throws ArgumentError scev_mul(se, sa, s32)
            t = scev_cast(se, LLVM.Opcode.Trunc, sa, LLVM.Int32Type())
            @test t isa SCEVTruncateExpr
            @test t.type == LLVM.Int32Type()
            @test scev_cast(se, LLVM.Opcode.ZExt, t, LLVM.Int64Type()) isa SCEVZeroExtendExpr
            @test scev_cast(se, LLVM.Opcode.SExt, s32, LLVM.Int64Type()) ==
                  se[ConstantInt(Int64(1))]
            @test_throws ArgumentError scev_cast(se, LLVM.Opcode.ZExt, sa, LLVM.Int32Type())
            @test_throws ArgumentError scev_cast(se, LLVM.Opcode.Trunc, sa, LLVM.Int64Type())
            @test_throws ArgumentError scev_cast(se, LLVM.Opcode.Add, sa, LLVM.Int32Type())

            # add recurrences: {0,+,1} for i, and {0,+,0,+,1} for s
            @test si.start == se[ConstantInt(Int64(0))]
            @test si.affine
            @test step_recurrence(se, si) == se[ConstantInt(Int64(1))]
            @test si.nuw && si.nsw && si.nw
            @test !sb.nuw && !sb.nsw
            ss = se[collect(inner.instructions)[2]]
            @test ss isa SCEVAddRecExpr
            @test !ss.affine
            step = step_recurrence(se, ss)
            @test step isa SCEVAddRecExpr && step.affine && step.loop == inner_loop

            # loop invariance and trip counts
            sj = se[j]
            @test is_loop_invariant(se, inner_loop, sa)
            @test is_loop_invariant(se, inner_loop, sj)
            @test !is_loop_invariant(se, inner_loop, si)
            @test !is_loop_invariant(se, outer_loop, sj)
            @test backedge_taken_count(se, inner_loop) == se[ConstantInt(Int64(9))]
            @test backedge_taken_count(se, inner_loop; kind=:constant_max) ==
                  se[ConstantInt(Int64(9))]
            outer_count = backedge_taken_count(se, outer_loop)
            @test !(outer_count isa SCEVCouldNotCompute)
            @test contains_scev(outer_count, SCEVUnknown)       # depends on %n
            @test backedge_taken_count(se, outer_loop; kind=:symbolic_max) isa SCEV
            @test_throws ArgumentError backedge_taken_count(se, outer_loop; kind=:bogus)
        end
    end
end

@testset "pointer expressions" begin
    @dispose ctx=Context() begin
    ptr = supports_typed_pointers(ctx) ? "i8*" : "ptr"
    @dispose mod=parse(LLVM.Module, """
        define void @f($ptr %p, $ptr %q, i64 %i) {
          %a = getelementptr i8, $ptr %p, i64 %i
          %b = getelementptr i8, $ptr %p, i64 8
          ret void
        }""") begin
        with_analyses(mod, "f") do fn, am
            se = am[ScalarEvolution]
            p, q, i = fn.parameters
            a, b = collect(only(fn.blocks).instructions)[1:2]
            # pointers with the same base can be subtracted
            d = scev_minus(se, se[a], se[b])
            @test d isa SCEVAddExpr
            @test d.type == LLVM.Int64Type()
            # with different bases, the difference is could-not-compute
            cnc = scev_minus(se, se[p], se[q])
            @test cnc isa SCEVCouldNotCompute
            @test_throws ArgumentError cnc.type
            @test_throws ArgumentError ConstantRange(se, cnc)
            @test_throws ArgumentError scev_add(se, cnc, se[i])
            # a sum can only have one pointer operand
            @test scev_add(se, se[p], se[i]) == se[a]
            @test_throws ArgumentError scev_add(se, se[p], se[q])
        end
    end
    end
end

@testset "dominance of uses and blocks" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i64 @f(i1 %c, i64 %x) {
        entry:
          %a = add i64 %x, 1
          br i1 %c, label %then, label %join
        then:
          %b = add i64 %a, 1
          br label %join
        join:
          %p = phi i64 [ %b, %then ], [ %a, %entry ]
          %q = add i64 %p, %a
          ret i64 %q
        }""") begin
        fn = mod.functions["f"]
        entry, then, join = fn.blocks
        a = first(entry.instructions)
        b = first(then.instructions)
        p, q = collect(join.instructions)[1:2]
        @dispose dt=DomTree(fn) begin
            @test dominates(dt, entry, join)
            @test !dominates(dt, then, join)
            @test dominates(dt, then, then)
            for use in b.uses
                # the use of %b in the phi is at the end of %then
                @test dominates(dt, b, use)
            end
            @test all(use -> dominates(dt, a, use), a.uses)
            @test !dominates(dt, b, first(a.uses))
        end
    end
end

@testset "control flow changes" begin
    @dispose ctx=Context() builder=IRBuilder() mod=parse(LLVM.Module, """
        define i32 @f(i32 %a, i32 %b) {
        entry:
          %c = icmp ne i32 %b, 0
          br i1 %c, label %body, label %exit
        body:
          %d = udiv i32 %a, %b
          br label %exit
        exit:
          %r = phi i32 [ %d, %body ], [ 0, %entry ]
          ret i32 %r
        }""") begin
        f = mod.functions["f"]
        entry, body, exit = f.blocks
        d = first(body.instructions)
        @dispose domtree=DomTree(f) begin
            # guard the division by a check that the dividend is at least the divisor
            position!(builder, LLVM.before(d))
            cond = icmp!(builder, LLVM.IntPredicate.UGE, f.parameters[1], f.parameters[2])
            term = split_block_and_insert_if_then!(cond, d; domtree)
            then = term.parent
            tail = d.parent
            @test term isa LLVM.BrInst
            @test only(term.successors) == tail
            @test collect(body.terminator.successors) == [then, tail]
            @test length(collect(f.blocks)) == 5

            # the domtree was updated
            @test dominates(domtree, body, then)
            @test dominates(domtree, body, tail)
            @test !dominates(domtree, then, tail)
            @dispose fresh=DomTree(f) begin
                for x in f.blocks, y in f.blocks
                    @test dominates(domtree, x, y) == dominates(fresh, x, y)
                end
            end

            # finish the bypass, and verify
            move!(d, LLVM.before(term))
            position!(builder, LLVM.at_begin(tail))
            q = phi!(builder, LLVM.Int32Type())
            replace_uses!(d, q)
            push!(q.incoming, (d, then))
            push!(q.incoming, (ConstantInt(Int32(0)), body))
            verify(f)

            # without a domtree, the tree needs to be recalculated
            e = collect(tail.instructions)[2]
            term2 = split_block_and_insert_if_then!(cond, e; unreachable=true)
            @test term2 isa LLVM.UnreachableInst
            @test recalculate!(domtree, f) === domtree
            @test dominates(domtree, tail, e.parent)
            @dispose fresh=DomTree(f) begin
                for x in f.blocks, y in f.blocks
                    @test dominates(domtree, x, y) == dominates(fresh, x, y)
                end
            end
        end

        @test_throws ArgumentError split_block_and_insert_if_then!(f.parameters[1], d)
        @test_throws ArgumentError split_block_and_insert_if_then!(
            ConstantInt(true), first(exit.instructions))
    end

    # the dominator tree must be one of the function that is changed
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define void @f(i1 %c) {
          ret void
        }
        define void @g() {
          ret void
        }""") begin
        f, g = mod.functions["f"], mod.functions["g"]
        @dispose domtree=DomTree(g) begin
            @test_throws ArgumentError split_block_and_insert_if_then!(
                f.parameters[1], f.entry.terminator; domtree)
            @test length(collect(f.blocks)) == 1
        end
    end

    # recalculating a tree after the entry block it was computed for was replaced
    @dispose ctx=Context() builder=IRBuilder() mod=parse(LLVM.Module, """
        define void @f() {
        entry:
          br label %exit
        exit:
          ret void
        }""") begin
        f = mod.functions["f"]
        old_entry, exit = f.blocks
        @dispose domtree=DomTree(f) begin
            new_entry = BasicBlock(LLVM.before(old_entry), "new_entry")
            position!(builder, LLVM.at_end(new_entry))
            br!(builder, exit)
            erase!(old_entry)
            recalculate!(domtree, f)
            @test dominates(domtree, new_entry, exit)
            @test !dominates(domtree, exit, new_entry)
        end
    end
end

@testset "preserving the dominator tree across control flow changes" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i32 @f(i32 %a, i32 %b) {
        entry:
          %d = udiv i32 %a, %b
          ret i32 %d
        }""") begin
        consistent = Ref(false)
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("split", (f, am) -> begin
                domtree = am[DomTree]
                d = first(f.entry.instructions)
                @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.before(d))
                    cond = icmp!(builder, LLVM.IntPredicate.NE, f.parameters[2],
                                 ConstantInt(Int32(0)))
                    split_block_and_insert_if_then!(cond, d; unreachable=true, domtree)
                end
                return PreservedAnalyses(DomTree)
            end; analyses=true))
            register!(pb, FunctionPass("check", (f, am) -> begin
                domtree = get(am, DomTree, nothing)
                domtree === nothing && return false
                @dispose fresh=DomTree(f) begin
                    consistent[] = all(dominates(domtree, x, y) == dominates(fresh, x, y)
                                       for x in f.blocks, y in f.blocks)
                end
                return false
            end; analyses=true))
            add!(pb, "function(split,check)")
            run!(pb, mod)
        end
        @test length(collect(mod.functions["f"].blocks)) == 3
        @test consistent[]
    end
end

@testset "dead code" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @g(i64)
        define void @f(i64 %x) {
          %a = add i64 %x, 1
          %b = mul i64 %a, 2
          %c = add i64 %a, 3
          call void @g(i64 %c)
          %d = add i64 %x, 4
          ret void
        }""") begin
        fn = mod.functions["f"]
        a, b, c, call, d = collect(only(fn.blocks).instructions)
        @test is_trivially_dead(b)
        @test is_trivially_dead(d)
        @test !is_trivially_dead(a)
        @test !is_trivially_dead(call)
        @test !erase_trivially_dead!(a)
        # erasing %b leaves %a, which is still used by %c
        @test erase_trivially_dead!(b)
        @test length(collect(only(fn.blocks).instructions)) == 5
        @test erase_trivially_dead!(d)
        @test length(collect(only(fn.blocks).instructions)) == 4
        @test verify(fn) === nothing
    end

    # operands that become dead are erased recursively
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define void @f(i64 %x) {
          %a = add i64 %x, 1
          %b = mul i64 %a, 2
          ret void
        }""") begin
        fn = mod.functions["f"]
        b = collect(only(fn.blocks).instructions)[2]
        @test erase_trivially_dead!(b)
        @test length(collect(only(fn.blocks).instructions)) == 1
    end

    # erasing several instructions, some of which are erased as an operand of another one
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @g(i64)
        define void @f(i64 %x) {
          %a = add i64 %x, 1
          %b = mul i64 %a, 2
          %c = add i64 %x, 3
          call void @g(i64 %c)
          ret void
        }""") begin
        fn = mod.functions["f"]
        a, b, c, call = collect(only(fn.blocks).instructions)
        # %c is used, and %a is erased when erasing %b
        @test erase_trivially_dead!([b, c, a])
        @test collect(only(fn.blocks).instructions)[1:2] == [c, call]
        @test !erase_trivially_dead!([c])
        @test !erase_trivially_dead!(Instruction[])
        @test verify(fn) === nothing
    end
end

@testset "promotion to registers" begin
    # in the pointer representation of the current context (typed by default before LLVM 17)
    function diamond()
        ptr = supports_typed_pointers(context()) ? "i32*" : "ptr"
        return """
        declare void @g($ptr)
        define i32 @f(i1 %c, i32 %a, i32 %b) {
        entry:
          %p = alloca i32
          %q = alloca i32
          %e = alloca i32
          call void @g($ptr %e)
          br i1 %c, label %then, label %else
        then:
          store i32 %a, $ptr %p
          br label %exit
        else:
          store i32 %b, $ptr %p
          br label %exit
        exit:
          %r = load i32, $ptr %p
          ret i32 %r
        }
        define void @h() {
          %x = alloca i32
          ret void
        }"""
    end

    @dispose ctx=Context() mod=parse(LLVM.Module, diamond()) begin
        f = mod.functions["f"]
        p, q, e = collect(f.entry.instructions)[1:3]
        x = first(mod.functions["h"].entry.instructions)
        @test is_promotable(p)
        @test is_promotable(q)
        @test !is_promotable(e)  # escapes into a call

        @dispose domtree=DomTree(f) begin
            # nothing changes when the requirements are not met
            ninsts() = sum(length(collect(bb.instructions)) for bb in f.blocks)
            n = ninsts()
            @test_throws ArgumentError promote_mem_to_reg!([p, e], domtree)
            @test_throws ArgumentError promote_mem_to_reg!([p, p], domtree)
            @test_throws ArgumentError promote_mem_to_reg!([p, x], domtree)
            @dispose other=DomTree(mod.functions["h"]) begin
                @test_throws ArgumentError promote_mem_to_reg!(p, other)
            end
            @test promote_mem_to_reg!(AllocaInst[], domtree) === nothing
            @test ninsts() == n

            # allocas that are not part of a function
            detached = x.parent
            remove!(detached)
            @test_throws ArgumentError promote_mem_to_reg!(x, domtree)
            remove!(x)
            @test_throws ArgumentError promote_mem_to_reg!(x, domtree)
            erase!(x)
            erase!(detached)

            @test promote_mem_to_reg!([p, q], domtree) === nothing
            @test verify(f) === nothing
            exit = last(collect(f.blocks))
            r = first(exit.instructions)
            @test r isa LLVM.PHIInst
            @test sort([(v.name, bb.name) for (v, bb) in r.incoming]) ==
                  [("a", "then"), ("b", "else")]
            @test !any(i -> i isa LLVM.AllocaInst && i != e, f.entry.instructions)
            @test !any(i -> i isa Union{LLVM.LoadInst,LLVM.StoreInst},
                       Iterators.flatten(bb.instructions for bb in f.blocks))

            # the CFG is unchanged, so the tree remains valid
            @dispose fresh=DomTree(f) begin
                for a in f.blocks, b in f.blocks
                    @test dominates(domtree, a, b) == dominates(fresh, a, b)
                end
            end
        end
    end

    # in a custom pass, with the analyses of the pass manager
    @dispose ctx=Context() mod=parse(LLVM.Module, diamond()) begin
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("promote", (f, am) -> begin
                allocas = filter(i -> i isa LLVM.AllocaInst && is_promotable(i),
                                 collect(f.entry.instructions))
                isempty(allocas) && return PreservedAnalyses(AllAnalyses)
                foreach(allocas) do alloca
                    promote_mem_to_reg!(alloca, am[DomTree];
                                        assumptions=am[AssumptionCache])
                end
                return PreservedAnalyses(CFGAnalyses)
            end; analyses=true))
            add!(pb, "function(promote)")
            run!(pb, mod)
        end
        f = mod.functions["f"]
        @test verify(mod) === nothing
        @test count(i -> i isa LLVM.AllocaInst, f.entry.instructions) == 1
        @test first(last(collect(f.blocks)).instructions) isa LLVM.PHIInst
        @test isempty(collect(mod.functions["h"].entry.instructions)[1:end-1])
    end
end

@testset "constant folding" begin
    @dispose ctx=Context() dl=LLVM.DataLayout("e-p:64:64") begin
        c = ConstantInt(Int64(0x1_0000_0005))
        @test constant_fold_cast(LLVM.Opcode.Trunc, c, LLVM.Int32Type(), dl) ==
              ConstantInt(Int32(5))
        @test constant_fold_cast(LLVM.Opcode.SExt, ConstantInt(Int8(-1)), LLVM.Int32Type(),
                                 dl) == ConstantInt(Int32(-1))
        @test constant_fold_cast(LLVM.Opcode.ZExt, ConstantInt(Int8(-1)), LLVM.Int32Type(),
                                 dl) == ConstantInt(Int32(255))
        @test constant_fold_cast(LLVM.Opcode.SIToFP, ConstantInt(Int32(2)), LLVM.DoubleType(),
                                 dl) == ConstantFP(2.0)
        @test_throws ArgumentError constant_fold_cast(LLVM.Opcode.Trunc, c,
                                                      LLVM.Int128Type(), dl)
        @test_throws ArgumentError constant_fold_cast(LLVM.Opcode.Add, c,
                                                      LLVM.Int32Type(), dl)

        # extensions of constant expressions can't be folded since LLVM 18
        @dispose mod=LLVM.Module("constants") begin
            gv = GlobalVariable(mod, LLVM.Int8Type(), "gv")
            p2i = const_ptrtoint(gv, LLVM.Int32Type())
            folded = constant_fold_cast(LLVM.Opcode.ZExt, p2i, LLVM.Int64Type(), dl)
            if LLVM.version() >= v"18"
                @test folded === nothing
            else
                @test folded isa ConstantExpr
            end
        end
    end
end


@testset "split_block!" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i32 @f(i32 %x, i1 %c) {
        entry:
          br i1 %c, label %body, label %exit
        body:
          %a = add i32 %x, 1
          %b = mul i32 %a, 2
          br label %exit
        exit:
          %r = phi i32 [ 0, %entry ], [ %b, %body ]
          ret i32 %r
        }""") begin
        f = mod.functions["f"]
        entry, body, exit = f.blocks
        a, b, br = body.instructions
        r = first(exit.instructions)

        tail = split_block!(b, "tail")
        @test tail.name == "tail"
        @test collect(f.blocks) == [entry, body, tail, exit]
        @test collect(body.instructions)[1] == a
        @test only(body.terminator.successors) == tail
        @test collect(tail.instructions) == [b, br]
        # PHI nodes of the successors refer to the new block
        @test collect(r.incoming) == [(ConstantInt(Int32(0)), entry), (b, tail)]
        verify(f)

        # at an insertion point, without a name
        tail2 = split_block!(LLVM.after(b))
        @test collect(tail.instructions)[1] == b
        @test only(tail2.instructions) == br
        verify(f)

        @test_throws ArgumentError split_block!(LLVM.at_end(tail2))
        @test_throws ArgumentError split_block!(r)
        detached = BasicBlock("detached")
        @test_throws ArgumentError split_block!(LLVM.at_begin(detached))
        erase!(detached)
    end

    # debug records at the split point
    if LLVM.version() >= v"19"
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define void @f(i32 %x) !dbg !5 {
          %a = add i32 %x, %x
            #dbg_value(i32 %x, !9, !DIExpression(), !10)
          %b = add i32 %x, %x
            #dbg_value(i32 %x, !9, !DIExpression(), !10)
          ret void, !dbg !10
        }

        !llvm.dbg.cu = !{!0}
        !llvm.module.flags = !{!3}

        !0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, emissionKind: FullDebug)
        !1 = !DIFile(filename: "test.c", directory: "/tmp")
        !3 = !{i32 2, !"Debug Info Version", i32 3}
        !5 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !6, unit: !0, spFlags: DISPFlagDefinition)
        !6 = !DISubroutineType(types: !7)
        !7 = !{null}
        !8 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
        !9 = !DILocalVariable(name: "v", scope: !5, file: !1, line: 2, type: !8)
        !10 = !DILocation(line: 2, column: 1, scope: !5)
        """) begin
        f = mod.functions["f"]
        a, b, ret = f.entry.instructions
        records(inst) = collect(inst.debug_records)
        (rb,) = records(b)
        (rret,) = records(ret)

        # at the head of an instruction's records, they move to the new block
        bb2 = split_block!(LLVM.after(a))
        @test first(bb2.instructions) == b && records(b) == [rb]

        # before the instruction, they stay in the original block
        bb3 = split_block!(LLVM.before(ret))
        @test isempty(records(ret)) && records(bb2.terminator) == [rret]
        @test only(bb3.instructions) == ret
        verify(mod)
    end
    end
end

@testset "simplify_instruction" begin
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i32 @f(i32 %x, i32 %y, i1 %c) {
          %a = add i32 %x, 0
          %b = and i1 %c, false
          %d = add i32 %x, %y
          %e = sub i32 %d, %y
          ret i32 %e
        }""") begin
        f = mod.functions["f"]
        a, b, d, e, ret = f.entry.instructions
        x, y, c = f.parameters
        @test simplify_instruction(a) == x
        @test simplify_instruction(b) == ConstantInt(false)
        @test simplify_instruction(d) === nothing
        @test simplify_instruction(e) == x
        # the IR is unchanged
        @test collect(f.entry.instructions) == [a, b, d, e, ret]

        a2 = copy(a)
        @test_throws ArgumentError simplify_instruction(a2)
        erase!(a2)
    end
end

@testset "inline!" begin
    @dispose ctx=Context() begin
    ptr = supports_typed_pointers(ctx) ? "i32*" : "ptr"
    mod = parse(LLVM.Module, """
        declare i32 @external(i32)

        define i32 @callee(i32 %x) sspstrong {
          %p = alloca i32
          store i32 %x, $ptr %p
          %v = load i32, $ptr %p
          %r = add i32 %v, 1
          ret i32 %r
        }

        define i32 @caller(i32 %x) ssp {
          %a = call i32 @callee(i32 %x)
          %b = call i32 @callee(i32 %a)
          %c = call i32 @external(i32 %b)
          %d = call i32 @callee(i32 %c)
          ret i32 %d
        }""")
    caller, callee = mod.functions["caller"], mod.functions["callee"]
    a, b, c, d, ret = caller.entry.instructions
    calls(f, prefix) = count(inst -> inst isa LLVM.CallInst &&
                                     startswith(inst.called_operand.name, prefix),
                             [inst for bb in f.blocks for inst in bb.instructions])

    # calls of declarations can't be inlined, and are left alone
    reason = inline!(c)
    @test reason isa String && !isempty(reason)
    @test c.parent == caller.entry

    # by default, allocas are marked using lifetime intrinsics
    @test inline!(a) === nothing
    @test calls(caller, "callee") == 2
    @test calls(caller, "llvm.lifetime.start") == 1
    @test !haskey(caller.function_attributes, :sspstrong)
    verify(caller)

    # without lifetime intrinsics, and merging the attributes of the functions
    @test inline!(b; insert_lifetime=false, merge_attributes=true) === nothing
    @test calls(caller, "callee") == 1
    @test calls(caller, "llvm.lifetime.start") == 1
    @test haskey(caller.function_attributes, :sspstrong)
    verify(caller)

    # the result of the call is replaced
    @test inline!(d) === nothing
    @test calls(caller, "callee") == 0
    @test ret.operands[1] isa LLVM.AddInst
    verify(mod)

    c2 = copy(c)
    @test_throws ArgumentError inline!(c2)
    erase!(c2)
    dispose(mod)
    end

    # InlineFunction doesn't check whether the body of the callee can be inlined
    @dispose ctx=Context() begin
    ptr = supports_typed_pointers(ctx) ? "i8*" : "ptr"
    va_start = LLVM.version() >= v"19" ? "llvm.va_start.p0" : "llvm.va_start"
    mod = parse(LLVM.Module, """
        declare void @$va_start($ptr)

        define i32 @variadic(i32 %n, ...) {
          %ap = alloca i8, i32 32
          call void @$va_start($ptr %ap)
          ret i32 %n
        }

        define i32 @fixed(i32 %n) {
          ret i32 %n
        }

        define i32 @caller(i32 %x) {
          %a = call i32 (i32, ...) @variadic(i32 %x)
          %b = call i32 @fixed(i32 %a)
          ret i32 %b
        }""")
    variadic, fixed = mod.functions["variadic"], mod.functions["fixed"]
    @test occursin("va_start", inline_viability(variadic))
    @test inline_viability(fixed) === nothing
    @test_throws ArgumentError inline_viability(mod.functions[va_start])
    verify(mod)
    dispose(mod)
    end

    # a failure leaves the IR unchanged, also where InlineFunction changes it before failing
    @dispose ctx=Context() begin
    pers(f) = supports_typed_pointers(ctx) ? "i8* bitcast (i32 (...)* @$f to i8*)" : "ptr @$f"
    mod = parse(LLVM.Module, """
        declare i32 @pers1(...)
        declare i32 @pers2(...)

        define void @callee() gc "statepoint-example" personality $(pers("pers1")) {
          ret void
        }

        define void @caller() personality $(pers("pers2")) {
          call void @callee()
          ret void
        }""")
    ir = string(mod)
    call = first(mod.functions["caller"].entry.instructions)
    @test occursin("personality", inline!(call))
    @test string(mod) == ir
    dispose(mod)
    end
end

end
