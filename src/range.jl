# constant ranges and known bits

@vocabulary Analysis ConstantRange, isfullset, iswrappedset, issignwrappedset,
                     intersect_with, union_with, binary_op, cast_op, allowed_icmp_region,
                     satisfying_icmp_region, KnownBits

# integers of arbitrary bit widths are stored as 64-bit words (least significant first),
# as APInt and the C API do. they are exposed as Julia integers of a type that depends on
# the number of words: (U)Int64 for up to 64 bits, (U)Int128 for up to 128 bits, and BigInt
# for wider integers.
const Words{N} = NTuple{N,UInt64}

nwords(nbits::Integer) = cld(nbits, 64)

unsigned_type(N::Int) = N == 1 ? UInt64 : N == 2 ? UInt128 : BigInt
signed_type(N::Int) = N == 1 ? Int64 : N == 2 ? Int128 : BigInt

function from_words(::Type{T}, words::Words{N}) where {T,N}
    x = zero(T)
    for i in N:-1:1
        x = (x << 64) | words[i]
    end
    return x
end

function to_words(::Val{N}, x::Integer) where {N}
    x = big(x)
    return ntuple(i -> UInt64((x >> (64 * (i - 1))) & typemax(UInt64)), Val(N))
end
to_words(::Val{1}, x::Base.BitInteger) = (x % UInt64,)
to_words(::Val{2}, x::Base.BitInteger) = (x % UInt64, ((x % UInt128) >> 64) % UInt64)

# the value of the low `nbits` bits of the words, interpreted as a signed integer
function signed_value(::Type{T}, nbits::Integer, words::Words) where {T}
    x = from_words(T === BigInt ? BigInt : unsigned(T), words)
    shift = (T === BigInt ? nbits : 8 * sizeof(T)) - nbits
    if T === BigInt
        return x >= big(1) << (nbits - 1) ? x - big(1) << nbits : x
    else
        return (reinterpret(T, x) << shift) >> shift
    end
end

# convert a Julia integer to the words of an `nbits`-bit integer, interpreting negative
# values as two's complement, and checking that the value fits
function checked_words(::Val{N}, nbits::Integer, x::Integer, what) where {N}
    -(big(1) << (nbits - 1)) <= x < big(1) << nbits ||
        throw(ArgumentError("$what $x does not fit in $nbits bits"))
    return to_words(Val(N), x < 0 ? big(1) << nbits + x : x)
end

function check_nbits(nbits::Integer)
    0 < nbits <= typemax(Cuint) ||
        throw(ArgumentError("Invalid number of bits for an integer: $nbits"))
    return Int(nbits)
end


## constant range

"""
    ConstantRange

A range of integers of a certain bit width, like LLVM's `ConstantRange`: the values from
`lower` (inclusive) up to `upper` (exclusive), wrapping around at the end of the integer
domain if `upper` is smaller than `lower` (a wrapped range). Equal bounds denote the empty
range when they are zero, and the full range when they are the maximum value.

    ConstantRange(nbits, lower, upper)
    ConstantRange(nbits, value)
    ConstantRange(nbits; empty=false)

Create the range of `nbits`-bit integers from `lower` up to `upper`, containing only
`value`, or containing all (or, with `empty=true`, none) of the `nbits`-bit integers.
Negative bounds and values are interpreted as two's complement integers.

Constant ranges are immutable values, and LLVM's own implementation of ranges is used to
compute with them, e.g., using `+`, `-`, `*`, `<<` or [`binary_op`](@ref).
Ranges are over-approximations: an operation can return a range that contains more values
than the exact result (e.g., [`intersect_with`](@ref) returns a range containing the
intersection of two ranges, as the exact intersection may not be a single range).
Operations on ranges of different bit widths are not supported.

# Properties

The bounds and extrema of a range are integers of a type that depends on the bit width:
`UInt64` or `Int64` for up to 64 bits, `UInt128` or `Int128` for up to 128 bits, and
`BigInt` for wider ranges.

- `r.nbits`: the bit width of the integers in the range.
- `r.lower`, `r.upper`: the bounds of the range, as unsigned integers.
- `r.unsigned_min`, `r.unsigned_max`: the smallest and largest value in the range when
  interpreted as unsigned integers.
- `r.signed_min`, `r.signed_max`: the smallest and largest value in the range when
  interpreted as signed integers.

The extrema are not defined for the empty range.

See also: [`isfullset`](@ref), [`iswrappedset`](@ref)
"""
struct ConstantRange{N}
    nbits::UInt32
    lower::Words{N}
    upper::Words{N}

    # inner constructor that doesn't check the representation
    global unsafe_range(nbits::Integer, lower::Words{N}, upper::Words{N}) where {N} =
        new{N}(nbits, lower, upper)
end

# properties of value types are accessed through specialized methods, unlike those of IR
# objects (see `@properties`), so that they are type stable
macro value_properties(T)
    quote
        @inline Base.getproperty(x::$T, s::Symbol) = getprop(x, Val(s))
        Base.setproperty!(x::$T, s::Symbol, v) = setprop!(x, Val(s), v)
        Base.propertynames(x::$T, private::Bool=false) = property_names(x, private)
    end |> esc
end

@value_properties ConstantRange

# the integer types used for the bounds and extrema of a range
unsigned_type(::ConstantRange{N}) where {N} = unsigned_type(N)
signed_type(::ConstantRange{N}) where {N} = signed_type(N)

function ConstantRange(nbits::Integer, lower::Integer, upper::Integer)
    nbits = check_nbits(nbits)
    N = nwords(nbits)
    lo = checked_words(Val(N), nbits, lower, "Lower bound")
    hi = checked_words(Val(N), nbits, upper, "Upper bound")
    if lo == hi
        max = to_words(Val(N), big(1) << nbits - 1)
        lo == to_words(Val(N), 0) || lo == max ||
            throw(ArgumentError("The bounds of a range can only be equal to denote the empty range (0, 0) or the full range (the maximum value)"))
    end
    return unsafe_range(nbits, lo, hi)
end

function ConstantRange(nbits::Integer, value::Integer)
    nbits = check_nbits(nbits)
    N = nwords(nbits)
    lo = checked_words(Val(N), nbits, value, "Value")
    hi = to_words(Val(N), (big(from_words(unsigned_type(N), lo)) + 1) % (big(1) << nbits))
    return unsafe_range(nbits, lo, hi)
end

function ConstantRange(nbits::Integer; empty::Bool=false)
    nbits = check_nbits(nbits)
    bound = to_words(Val(nwords(nbits)), empty ? 0 : big(1) << nbits - 1)
    return unsafe_range(nbits, bound, bound)
end

bitwidth(r::ConstantRange) = Int(getfield(r, :nbits))
lower(r::ConstantRange) = from_words(unsigned_type(r), getfield(r, :lower))
upper(r::ConstantRange) = from_words(unsigned_type(r), getfield(r, :upper))

@property ConstantRange nbits => bitwidth
@property ConstantRange lower
@property ConstantRange upper

"""
    isempty(r::ConstantRange)
    isfullset(r::ConstantRange)

Check whether the range contains no, or all integers of its bit width.
"""
isfullset(r::ConstantRange) = lower(r) == upper(r) && lower(r) != 0

Base.isempty(r::ConstantRange) = lower(r) == upper(r) == 0

"""
    iswrappedset(r::ConstantRange)
    issignwrappedset(r::ConstantRange)

Check whether the range wraps around the end of the unsigned (or signed) integer domain,
i.e., whether it contains both the maximum and the minimum unsigned (or signed) value, but
is not the full range.
"""
iswrappedset(r::ConstantRange) = lower(r) > upper(r) && upper(r) != 0

@doc (@doc iswrappedset)
function issignwrappedset(r::ConstantRange)
    T = signed_type(r)
    lo = signed_value(T, bitwidth(r), getfield(r, :lower))
    hi = signed_value(T, bitwidth(r), getfield(r, :upper))
    return lo > hi && hi != typemin_signed(T, bitwidth(r))
end

typemin_signed(::Type{T}, nbits) where {T} =
    T === BigInt ? -(big(1) << (nbits - 1)) : (-one(T)) << (nbits - 1)
typemax_signed(::Type{T}, nbits) where {T} =
    T === BigInt ? big(1) << (nbits - 1) - 1 : ~typemin_signed(T, nbits)

function check_nonempty(r::ConstantRange)
    isempty(r) && throw(ArgumentError("The empty range has no extrema"))
    return r
end

function unsigned_min(r::ConstantRange)
    check_nonempty(r)
    return isfullset(r) || iswrappedset(r) ? zero(unsigned_type(r)) : lower(r)
end

function unsigned_max(r::ConstantRange)
    check_nonempty(r)
    T = unsigned_type(r)
    max = T === BigInt ? big(1) << bitwidth(r) - 1 :
                         typemax(T) >> (8 * sizeof(T) - bitwidth(r))
    # the upper bound is exclusive, and only zero when the range wraps to the end
    return isfullset(r) || upper(r) == 0 || iswrappedset(r) ? max : upper(r) - one(T)
end

function signed_min(r::ConstantRange)
    check_nonempty(r)
    T = signed_type(r)
    (isfullset(r) || issignwrappedset(r)) && return typemin_signed(T, bitwidth(r))
    return signed_value(T, bitwidth(r), getfield(r, :lower))
end

function signed_max(r::ConstantRange)
    check_nonempty(r)
    T = signed_type(r)
    (isfullset(r) || issignwrappedset(r)) && return typemax_signed(T, bitwidth(r))
    hi = signed_value(T, bitwidth(r), getfield(r, :upper))
    # an upper bound of the minimum signed value means the range ends at the maximum
    hi == typemin_signed(T, bitwidth(r)) && return typemax_signed(T, bitwidth(r))
    return hi - one(T)
end

@property ConstantRange unsigned_min
@property ConstantRange unsigned_max
@property ConstantRange signed_min
@property ConstantRange signed_max

function Base.show(io::IO, r::ConstantRange)
    if isfullset(r)
        print(io, "ConstantRange(", bitwidth(r), ")")
    elseif isempty(r)
        print(io, "ConstantRange(", bitwidth(r), "; empty=true)")
    else
        T = signed_type(r)
        # print the bounds as signed integers, like LLVM does
        print(io, "ConstantRange(", bitwidth(r), ", ",
              signed_value(T, bitwidth(r), getfield(r, :lower)), ", ",
              signed_value(T, bitwidth(r), getfield(r, :upper)), ")")
    end
end

# compute a range using a C function: `f(ptrs...)` is called with pointers to the words of
# the bounds of the input ranges (lower, upper), followed by pointers to the words of the
# bounds of the resulting `nbits`-bit range (of `M` words). `f` returns `false` if it
# failed, in which case `nothing` is returned. all words are kept in a single buffer, which
# can be allocated on the stack.
function compute_range(f::F, ::Val{M}, nbits::Integer) where {F,M}
    out = ntuple(_ -> UInt64(0), Val(M))
    buf = Ref((out, out))
    success = GC.@preserve buf begin
        p = Ptr{UInt64}(Base.unsafe_convert(Ptr{typeof(buf[])}, buf))
        f(p, p + 8M)
    end
    success === false && return nothing
    return unsafe_range(nbits, buf[][1], buf[][2])
end
function compute_range(f::F, ::Val{M}, nbits::Integer, a::ConstantRange{N}) where {F,M,N}
    out = ntuple(_ -> UInt64(0), Val(M))
    buf = Ref((getfield(a, :lower), getfield(a, :upper), out, out))
    success = GC.@preserve buf begin
        p = Ptr{UInt64}(Base.unsafe_convert(Ptr{typeof(buf[])}, buf))
        f(p, p + 8N, p + 16N, p + 16N + 8M)
    end
    success === false && return nothing
    return unsafe_range(nbits, buf[][3], buf[][4])
end
function compute_range(f::F, ::Val{M}, nbits::Integer, a::ConstantRange{N},
                       b::ConstantRange{N}) where {F,M,N}
    check_same_width(a, b)
    out = ntuple(_ -> UInt64(0), Val(M))
    buf = Ref((getfield(a, :lower), getfield(a, :upper), getfield(b, :lower),
               getfield(b, :upper), out, out))
    success = GC.@preserve buf begin
        p = Ptr{UInt64}(Base.unsafe_convert(Ptr{typeof(buf[])}, buf))
        f(p, p + 8N, p + 16N, p + 24N, p + 32N, p + 32N + 8M)
    end
    success === false && return nothing
    return unsafe_range(nbits, buf[][5], buf[][6])
end
compute_range(f, nbits::Integer, a::ConstantRange{N}) where {N} =
    compute_range(f, Val(N), nbits, a)
compute_range(f, nbits::Integer, a::ConstantRange{N}, b::ConstantRange{N}) where {N} =
    compute_range(f, Val(N), nbits, a, b)
compute_range(f, nbits::Integer, a::ConstantRange, b::ConstantRange) =
    check_same_width(a, b)

function check_same_width(a::ConstantRange, b::ConstantRange)
    bitwidth(a) == bitwidth(b) ||
        throw(ArgumentError("Cannot combine ranges of $(bitwidth(a)) and $(bitwidth(b)) bits"))
    return
end

words_pointer(ref::Ref{<:Words}) =
    Ptr{UInt64}(Base.unsafe_convert(Ptr{eltype(ref)}, ref))

"""
    intersect_with(a::ConstantRange, b::ConstantRange; prefer=:smallest)
    union_with(a::ConstantRange, b::ConstantRange; prefer=:smallest)

Compute a range that contains the intersection, or the union, of two ranges. As the exact
result may not be a single range, there can be multiple candidates, of which the smallest
one is returned by default. With `prefer=:unsigned` or `prefer=:signed`, a candidate that
does not wrap in the unsigned or signed domain is preferred.
"""
function intersect_with(a::ConstantRange, b::ConstantRange; prefer::Symbol=:smallest)
    type = range_type(prefer)
    compute_range(bitwidth(a), a, b) do alo, ahi, blo, bhi, lo, hi
        API.LLVMExtraConstantRangeIntersectWith(bitwidth(a), alo, ahi, blo, bhi, type, lo,
                                                hi)
    end
end

@doc (@doc intersect_with)
function union_with(a::ConstantRange, b::ConstantRange; prefer::Symbol=:smallest)
    type = range_type(prefer)
    compute_range(bitwidth(a), a, b) do alo, ahi, blo, bhi, lo, hi
        API.LLVMExtraConstantRangeUnionWith(bitwidth(a), alo, ahi, blo, bhi, type, lo, hi)
    end
end

function range_type(prefer::Symbol)
    prefer === :smallest && return API.LLVMExtraSmallestRange
    prefer === :unsigned && return API.LLVMExtraUnsignedRange
    prefer === :signed && return API.LLVMExtraSignedRange
    throw(ArgumentError("Invalid preferred range type :$prefer; expected :smallest, :unsigned or :signed"))
end

"""
    binary_op(opcode, a::ConstantRange, b::ConstantRange; nuw=false, nsw=false)

Compute the range of the result of the binary operator `opcode` (e.g.,
`LLVM.Opcode.Add`) applied to values in the ranges `a` and `b`. With `nuw` or `nsw`, only
the results of operations that do not wrap in the unsigned or signed sense are included,
as for instructions with those flags (which produce poison otherwise). This does not prove
that an operation does not wrap. Operations for which LLVM does not know how to use these
flags ignore them, which is less precise, but still correct (on LLVM before 19, this is the
case for multiplications, and before 20 for left shifts).

The common operators are also available using Julia's operators: `a + b`, `a - b`,
`a * b`, `a & b`, `a | b`, `xor(a, b)`, `a << b`, `a >> b` (arithmetic shift), and
`a >>> b` (logical shift).
"""
function binary_op(opcode::API.LLVMOpcode, a::ConstantRange, b::ConstantRange;
                   nuw::Bool=false, nsw::Bool=false)
    flags = (nuw ? UInt32(API.LLVMExtraNoUnsignedWrap) : UInt32(0)) |
            (nsw ? UInt32(API.LLVMExtraNoSignedWrap) : UInt32(0))
    r = compute_range(bitwidth(a), a, b) do alo, ahi, blo, bhi, lo, hi
        API.LLVMExtraConstantRangeBinaryOp(opcode, flags, bitwidth(a), alo, ahi, blo, bhi,
                                           lo, hi) |> Bool
    end
    r === nothing && throw(ArgumentError("$opcode is not a binary operator"))
    return r
end

for (op, opcode) in [(:+, :LLVMAdd), (:-, :LLVMSub), (:*, :LLVMMul), (:&, :LLVMAnd),
                     (:|, :LLVMOr), (:xor, :LLVMXor), (:<<, :LLVMShl), (:>>, :LLVMAShr),
                     (:>>>, :LLVMLShr)]
    @eval Base.$op(a::ConstantRange, b::ConstantRange) = binary_op(API.$opcode, a, b)
end

"""
    cast_op(opcode, r::ConstantRange, nbits)

Compute the range of the values in `r` after the cast `opcode` to `nbits`-bit integers:
`LLVM.Opcode.ZExt` or `LLVM.Opcode.SExt` to a larger bit width, or `LLVM.Opcode.Trunc` to
a smaller one.
"""
function cast_op(opcode::API.LLVMOpcode, r::ConstantRange, nbits::Integer)
    nbits = check_nbits(nbits)
    result = compute_range(Val(nwords(nbits)), nbits, r) do rlo, rhi, lo, hi
        API.LLVMExtraConstantRangeCastOp(opcode, bitwidth(r), rlo, rhi, nbits, lo,
                                         hi) |> Bool
    end
    result === nothing &&
        throw(ArgumentError("Invalid bit width $nbits for a $opcode of a $(bitwidth(r))-bit range"))
    return result
end

"""
    allowed_icmp_region(predicate, r::ConstantRange)
    satisfying_icmp_region(predicate, r::ConstantRange)

Compute the smallest range containing every value `x` for which `icmp predicate x, y`
holds for some `y` in `r` (the allowed region), or for all `y` in `r` (the satisfying
region). The `predicate` is an integer predicate, like `LLVM.IntPredicate.ULT`.

For example, if `x < y` is known to hold for some `y` in `r`, then `x` is in
`allowed_icmp_region(LLVM.IntPredicate.ULT, r)`.
"""
allowed_icmp_region(predicate::API.LLVMIntPredicate, r::ConstantRange) =
    icmp_region(predicate, false, r)

@doc (@doc allowed_icmp_region)
satisfying_icmp_region(predicate::API.LLVMIntPredicate, r::ConstantRange) =
    icmp_region(predicate, true, r)

function icmp_region(predicate, satisfying, r::ConstantRange)
    compute_range(bitwidth(r), r) do rlo, rhi, lo, hi
        API.LLVMExtraConstantRangeMakeICmpRegion(predicate, satisfying, bitwidth(r), rlo,
                                                 rhi,
                                                 lo, hi)
    end
end


## known bits

"""
    KnownBits(nbits, zero, one)

The bits of an `nbits`-bit integer that are known, as LLVM's `KnownBits`: those set in
`zero` are known to be zero, and those set in `one` are known to be one.

# Properties

- `kb.nbits`: the bit width.
- `kb.zero`, `kb.one`: the masks of the bits known to be zero or one, as unsigned integers
  (`UInt64` for up to 64 bits, `UInt128` for up to 128 bits, and `BigInt` otherwise).

Known bits can be converted to a range using `ConstantRange(kb; signed=false)`, and a range
to the bits that are known for all of its values using `KnownBits(r)`.
"""
struct KnownBits{N}
    nbits::UInt32
    zero::Words{N}
    one::Words{N}

    global unsafe_known_bits(nbits::Integer, zero::Words{N}, one::Words{N}) where {N} =
        new{N}(nbits, zero, one)
end

@value_properties KnownBits

function KnownBits(nbits::Integer, zero::Integer, one::Integer)
    nbits = check_nbits(nbits)
    0 <= zero < big(1) << nbits && 0 <= one < big(1) << nbits ||
        throw(ArgumentError("The masks of known bits must be unsigned $nbits-bit integers"))
    zero & one == 0 ||
        throw(ArgumentError("Bits cannot be known to be both zero and one"))
    N = nwords(nbits)
    return unsafe_known_bits(nbits, to_words(Val(N), zero), to_words(Val(N), one))
end

bitwidth(kb::KnownBits) = Int(getfield(kb, :nbits))
known_zero(kb::KnownBits{N}) where {N} = from_words(unsigned_type(N), getfield(kb, :zero))
known_one(kb::KnownBits{N}) where {N} = from_words(unsigned_type(N), getfield(kb, :one))

@property KnownBits nbits => bitwidth
@property KnownBits zero => known_zero
@property KnownBits one => known_one

function Base.show(io::IO, kb::KnownBits)
    print(io, "KnownBits(", bitwidth(kb), ", 0x", string(known_zero(kb); base=16), ", 0x",
          string(known_one(kb); base=16), ")")
end

"""
    ConstantRange(kb::KnownBits; signed=false)

The range of integers with the known bits `kb`. With `signed=true`, the range is
optimized for signed comparisons (it does not wrap in the signed domain).
"""
function ConstantRange(kb::KnownBits{N}; signed::Bool=false) where {N}
    zero = Ref(getfield(kb, :zero))
    one = Ref(getfield(kb, :one))
    lo = Ref{Words{N}}()
    hi = Ref{Words{N}}()
    GC.@preserve zero one lo hi begin
        API.LLVMExtraConstantRangeFromKnownBits(bitwidth(kb), words_pointer(zero),
                                                words_pointer(one), signed,
                                                words_pointer(lo), words_pointer(hi))
    end
    return unsafe_range(bitwidth(kb), lo[], hi[])
end

function KnownBits(r::ConstantRange{N}) where {N}
    lo = Ref(getfield(r, :lower))
    hi = Ref(getfield(r, :upper))
    zero = Ref{Words{N}}()
    one = Ref{Words{N}}()
    GC.@preserve zero one lo hi begin
        API.LLVMExtraConstantRangeToKnownBits(bitwidth(r), words_pointer(lo),
                                              words_pointer(hi),
                                              words_pointer(zero), words_pointer(one))
    end
    return unsafe_known_bits(bitwidth(r), zero[], one[])
end
