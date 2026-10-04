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
