@testset "constant ranges" begin

@testset "construction and properties" begin
    r = ConstantRange(64, 0, 100)
    @test r.nbits == 64
    @test r.lower === UInt64(0)
    @test r.upper === UInt64(100)
    @test r.unsigned_min === UInt64(0)
    @test r.unsigned_max === UInt64(99)
    @test r.signed_min === Int64(0)
    @test r.signed_max === Int64(99)
    @test !isempty(r) && !isfullset(r) && !iswrappedset(r) && !issignwrappedset(r)
    @test :unsigned_max in propertynames(r)

    # singleton ranges, and negative values as two's complement
    @test ConstantRange(32, 7) == ConstantRange(32, 7, 8)
    @test ConstantRange(32, -1) == ConstantRange(32, typemax(UInt32), 0)
    @test ConstantRange(32, -1).signed_min == -1
    @test ConstantRange(8, -128, 0).signed_max == -1

    # the full and empty ranges
    full = ConstantRange(16)
    empty = ConstantRange(16; empty=true)
    @test isfullset(full) && !isempty(full)
    @test isempty(empty) && !isfullset(empty)
    @test full.unsigned_max == typemax(UInt16)
    @test full.signed_min == typemin(Int16)
    @test full.signed_max == typemax(Int16)
    @test_throws ArgumentError empty.unsigned_min
    @test_throws ArgumentError empty.signed_max
    @test ConstantRange(16, 0, 0) == empty
    @test ConstantRange(16, 0xffff, 0xffff) == full

    # wrapped ranges
    w = ConstantRange(8, 250, 5)
    @test iswrappedset(w) && !issignwrappedset(w)
    @test (w.unsigned_min, w.unsigned_max) == (0, 255)
    @test (w.signed_min, w.signed_max) == (-6, 4)
    s = ConstantRange(8, 100, -100)
    @test issignwrappedset(s) && !iswrappedset(s)
    @test (s.signed_min, s.signed_max) == (-128, 127)
    @test (s.unsigned_min, s.unsigned_max) == (100, 155)

    # the integer types depend on the bit width
    @test ConstantRange(1, 0, 1).unsigned_max isa UInt64
    @test ConstantRange(65, 0, 1).unsigned_max isa UInt128
    @test ConstantRange(65, -1).signed_min === Int128(-1)
    big = ConstantRange(200, -5, 10)
    @test big.signed_min isa BigInt && big.signed_min == -5
    @test big.unsigned_max == BigInt(2)^200 - 1
    @test ConstantRange(128, 0, UInt128(1) << 100).upper === UInt128(1) << 100

    # invalid ranges
    @test_throws ArgumentError ConstantRange(8, 5, 5)
    @test_throws ArgumentError ConstantRange(8, 0, 256)
    @test_throws ArgumentError ConstantRange(8, -129, 0)
    @test_throws ArgumentError ConstantRange(0, 0, 0)

    # display as the call that creates the range
    @test sprint(show, r) == "ConstantRange(64, 0, 100)"
    @test sprint(show, w) == "ConstantRange(8, -6, 5)"
    @test sprint(show, full) == "ConstantRange(16)"
    @test sprint(show, empty) == "ConstantRange(16; empty=true)"
end

@testset "operations" begin
    r = ConstantRange(64, 0, 100)
    one = ConstantRange(64, 1)
    @test r + one == ConstantRange(64, 1, 101)
    @test r - one == ConstantRange(64, -1, 99)
    @test r * ConstantRange(64, 2) == ConstantRange(64, 0, 199)
    @test r << ConstantRange(64, 2) == ConstantRange(64, 0, 397)
    @test r >>> one == ConstantRange(64, 0, 50)
    @test ConstantRange(64, -8, 0) >> one == ConstantRange(64, -4, 0)
    @test r & ConstantRange(64, 15) == ConstantRange(64, 0, 16)
    @test isfullset(ConstantRange(64) + one)

    # binary operators, with and without no-wrap flags
    @test binary_op(LLVM.Opcode.UDiv, r, ConstantRange(64, 10)) == ConstantRange(64, 0, 10)
    nonneg = ConstantRange(64, 0, typemax(Int64))
    small = ConstantRange(64, 1, 3)
    @test issignwrappedset(nonneg + small)
    @test binary_op(LLVM.Opcode.Add, nonneg, small; nsw=true) ==
          ConstantRange(64, 1, typemin(Int64))
    @test binary_op(LLVM.Opcode.Sub, ConstantRange(64, 0, 10), ConstantRange(64, 5, 6);
                    nuw=true) == ConstantRange(64, 0, 5)
    @test_throws ArgumentError binary_op(LLVM.Opcode.ZExt, r, r)

    # intersections and unions are approximated by a single range
    @test intersect_with(r, ConstantRange(64, 50, 200)) == ConstantRange(64, 50, 100)
    @test union_with(r, ConstantRange(64, 150, 200)) == ConstantRange(64, 0, 200)
    a = ConstantRange(8, 250, 10)     # wraps in the unsigned domain
    b = ConstantRange(8, 5, 255)
    @test intersect_with(a, b; prefer=:unsigned) == ConstantRange(8, 5, 10) ||
          !iswrappedset(intersect_with(a, b; prefer=:unsigned))
    @test !issignwrappedset(intersect_with(a, b; prefer=:signed))
    @test_throws ArgumentError intersect_with(a, b; prefer=:other)

    # casts
    @test cast_op(LLVM.Opcode.ZExt, ConstantRange(32, -1), 64) ==
          ConstantRange(64, typemax(UInt32))
    @test cast_op(LLVM.Opcode.SExt, ConstantRange(32, -1), 64) == ConstantRange(64, -1)
    @test cast_op(LLVM.Opcode.Trunc, ConstantRange(64, 0, 1 << 40), 32) == ConstantRange(32)
    @test cast_op(LLVM.Opcode.ZExt, r, 200).nbits == 200
    @test_throws ArgumentError cast_op(LLVM.Opcode.ZExt, r, 32)
    @test_throws ArgumentError cast_op(LLVM.Opcode.Add, r, 128)

    # comparison regions
    @test allowed_icmp_region(LLVM.IntPredicate.ULT, ConstantRange(64, 10, 20)) ==
          ConstantRange(64, 0, 19)
    @test satisfying_icmp_region(LLVM.IntPredicate.ULT, ConstantRange(64, 10, 20)) ==
          ConstantRange(64, 0, 10)
    @test allowed_icmp_region(LLVM.IntPredicate.SGE, ConstantRange(64, 0)) ==
          ConstantRange(64, 0, typemin(Int64))

    # ranges of different widths cannot be combined
    @test_throws ArgumentError r + ConstantRange(32, 1)
    @test_throws ArgumentError intersect_with(r, ConstantRange(32, 1))

    # wide ranges
    big = ConstantRange(200, -5, 10)
    @test big + big == ConstantRange(200, -10, 19)

    # operations on ranges of up to 64 bits don't allocate (Julia 1.10 and 1.11 cannot
    # elide the buffer that is passed to LLVM)
    f(a, b) = intersect_with(a + b, b; prefer=:unsigned)
    f(r, one)
    @test (@allocated f(r, one)) <= (VERSION >= v"1.12" ? 0 : 64)
end

@testset "containment" begin
    r = ConstantRange(8, 10, 20)
    @test 10 in r
    @test 19 in r
    @test !(20 in r)
    @test !(9 in r)
    # negative integers are two's complement, and integers that don't fit are never in it
    wrapped = ConstantRange(8, -2, 2)
    @test -1 in wrapped
    @test 0xff in wrapped
    @test 1 in wrapped
    @test !(2 in wrapped)
    @test !(256 in ConstantRange(8))
    @test !(-129 in ConstantRange(8))
    @test 255 in ConstantRange(8)
    @test !(0 in ConstantRange(8; empty=true))
    @test 2^100 in ConstantRange(128, 0, big(2)^101)

    @test issubset(ConstantRange(8, 12, 15), r)
    @test !issubset(ConstantRange(8, 5, 15), r)
    @test issubset(ConstantRange(8; empty=true), r)
    @test issubset(r, ConstantRange(8))
    @test issubset(ConstantRange(8, -1, 1), wrapped)
    @test_throws ArgumentError issubset(r, ConstantRange(16, 10, 20))

    @test r.all_nonnegative
    @test ConstantRange(8, 0, 128).all_nonnegative
    @test !ConstantRange(8, 0, 129).all_nonnegative
    @test !wrapped.all_nonnegative
    @test ConstantRange(8; empty=true).all_nonnegative
    @test !ConstantRange(8).all_nonnegative
end

@testset "range metadata" begin
@dispose ctx=Context() begin
    r = ConstantRange(32, -5, 100)
    md = MDNode(r)
    @test md isa MDTuple
    @test ConstantRange(md) == r
    @test_throws ArgumentError MDNode(ConstantRange(32))
    @test_throws ArgumentError MDNode(ConstantRange(32; empty=true))
    @test ConstantRange(MDNode(ConstantRange(128, 1, big(2)^100))) ==
          ConstantRange(128, 1, big(2)^100)

    # multiple ranges are merged, and attached to instructions
    @dispose mod=parse(LLVM.Module, """
        define i64 @f(i64 %x) {
          %a = add i64 %x, 1, !range !0
          %b = add i64 %x, 2, !range !1
          ret i64 %a
        }
        !0 = !{i64 0, i64 10, i64 20, i64 30}
        !1 = !{i64 0, i64 10, i32 20, i32 30}""") begin
        a, b, ret = mod.functions["f"].entry.instructions
        @test ConstantRange(a.metadata[LLVM.MD_range]) == ConstantRange(64, 0, 30)
        # invalid metadata isn't accepted
        @test_throws ArgumentError ConstantRange(b.metadata[LLVM.MD_range])
        @test_throws ArgumentError ConstantRange(MDNode([MDString("x")]))
        @test_throws ArgumentError ConstantRange(MDNode(Metadata[]))

        ret.operands[1].metadata[LLVM.MD_range] = MDNode(ConstantRange(64, 1, 11))
        @test ConstantRange(a.metadata[LLVM.MD_range]) == ConstantRange(64, 1, 11)
        a.metadata[LLVM.MD_noundef] = MDNode(Metadata[])
        @test haskey(a.metadata, LLVM.MD_noundef)
        @test occursin("!noundef", string(a))
    end
end
end

@testset "known bits" begin
    kb = KnownBits(64, ~UInt64(0xff), 0x1)
    @test kb.nbits == 64
    @test kb.zero === ~UInt64(0xff)
    @test kb.one === UInt64(1)
    @test ConstantRange(kb) == ConstantRange(64, 1, 256)
    @test ConstantRange(KnownBits(8, 0, 0x80); signed=true) == ConstantRange(8, -128, 0)
    @test KnownBits(ConstantRange(64, 16, 32)) == KnownBits(64, ~UInt64(0x1f), 0x10)
    @test KnownBits(200, 0, 1).one == 1
    @test_throws ArgumentError KnownBits(8, 1, 1)
    @test_throws ArgumentError KnownBits(8, 0x100, 0)
    @test sprint(show, KnownBits(8, 0xf0, 0x01)) == "KnownBits(8, 0xf0, 0x1)"
end

end
