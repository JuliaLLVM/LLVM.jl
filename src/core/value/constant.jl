@vocabulary IR null, isnull, all_ones

"""
    LLVM.Constant <: LLVM.User

Abstract supertype for all constant values.
"""
abstract type Constant <: User end
@vocabulary IR Constant

unsafe_destroy!(constant::Constant) = API.LLVMDestroyConstant(constant)

@vocabulary IR remove_dead_constant_users!

"""
    remove_dead_constant_users!(c::Constant)

Remove the constants that use `c`, directly or transitively, but are not used themselves,
like C++'s `Constant::removeDeadConstantUsers`. These are, e.g., constant expressions that
remain after replacing or erasing the instructions that used them, and that keep `c` from
being unused. `c` itself is not removed. Returns `c`.
"""
function remove_dead_constant_users!(c::Constant)
    API.LLVMExtraRemoveDeadConstantUsers(c)
    return c
end

# forward declarations
# not part of a vocabulary, as it would clash with `Base.Module`
@public Module
@checked struct Module
    ref::API.LLVMModuleRef
end
abstract type Instruction <: User end

@vocabulary IR convert_users_to_instructions!

"""
    convert_users_to_instructions!(consts::AbstractVector{<:Constant};
                                   func::Union{Nothing,LLVM.Function}=nothing,
                                   remove_dead_constants::Bool=true,
                                   include_self::Bool=false) -> Bool

Rewrite every constant expression or constant aggregate that (transitively) uses one of
`consts` into equivalent instructions at each point of use; phi operands are materialized in
their incoming block. Returns whether anything changed.

Optionally restrict the rewrite to `func`, keep dead constants around
(`remove_dead_constants=false`), or also convert the passed constants themselves
(`include_self=true`). These three options require LLVM 19 or later; the function itself
requires LLVM 17 or later.
"""
function convert_users_to_instructions!(consts::AbstractVector{<:Constant};
                                        func=nothing,
                                        remove_dead_constants::Bool=true,
                                        include_self::Bool=false)
    if version() < v"17"
        throw(ArgumentError("convert_users_to_instructions! requires LLVM 17 or later"))
    end
    if version() < v"19" && (func !== nothing || !remove_dead_constants || include_self)
        throw(ArgumentError("the `func`, `remove_dead_constants` and `include_self` " *
                            "options require LLVM 19 or later"))
    end
    func === nothing || func isa Function ||
        throw(ArgumentError("`func` must be an LLVM.Function or `nothing`"))
    API.LLVMConvertUsersOfConstantsToInstructions(
        as_vector(consts), length(consts), something(func, C_NULL),
        remove_dead_constants, include_self) |> Bool
end


## convenience constructors

"""
    null(typ::LLVMType)

Create a null constant of the given type.
"""
null(typ::LLVMType) = Value(API.LLVMConstNull(typ))

"""
    all_ones(typ::LLVMType)

Create a constant with all bits set to one of the given type.
"""
all_ones(typ::LLVMType) = Value(API.LLVMConstAllOnes(typ))

"""
    isnull(val::LLVM.Value)

Check if the given value is a null constant.
"""
isnull(val::Value) = API.LLVMIsNull(val) |> Bool


## data

@vocabulary IR PointerNull, UndefValue, PoisonValue, ConstantTokenNone, ConstantTargetNone,
              ConstantInt, ConstantFP

# Abstract supertype for all constant value without operands.
abstract type ConstantData <: Constant end


"""
    PointerNull <: LLVM.ConstantData

A null pointer constant.
"""
@checked struct PointerNull <: ConstantData
    ref::API.LLVMValueRef
end
register(PointerNull, API.LLVMConstantPointerNullValueKind)

"""
    PointerNull(typ::LLVMType)

Create a null pointer constant of the given type.
"""
PointerNull(typ::PointerType) = PointerNull(API.LLVMConstPointerNull(typ))


"""
    UndefValue <: LLVM.ConstantData

An undefined constant value.
"""
@checked struct UndefValue <: ConstantData
    ref::API.LLVMValueRef
end
register(UndefValue, API.LLVMUndefValueValueKind)

"""
    UndefValue(typ::LLVMType)

Create an constant undefined value of the given type.
"""
UndefValue(typ::LLVMType) = UndefValue(API.LLVMGetUndef(typ))


"""
    PoisonValue <: LLVM.ConstantData

A poison constant value.
"""
@checked struct PoisonValue <: ConstantData # XXX: actually <: UndefValue
    ref::API.LLVMValueRef
end
register(PoisonValue, API.LLVMPoisonValueValueKind)

"""
    PoisonValue(typ::LLVMType)

Create a poison constant value of the given type.
"""
PoisonValue(typ::LLVMType) = PoisonValue(API.LLVMGetPoison(typ))


"""
    ConstantTokenNone <: LLVM.ConstantData

The `none` token, e.g., the parent pad of a `cleanuppad` or `catchswitch` instruction that
is not nested in another pad. It is the null value of the token type, so it is created
using `null(LLVM.TokenType())`.
"""
@checked struct ConstantTokenNone <: ConstantData
    ref::API.LLVMValueRef
end
register(ConstantTokenNone, API.LLVMConstantTokenNoneValueKind)


"""
    ConstantTargetNone <: LLVM.ConstantData

The `zeroinitializer` of a target extension type (e.g., `target("spirv.Event")`), which
only exists on LLVM 16 and later.
"""
@checked struct ConstantTargetNone <: ConstantData
    ref::API.LLVMValueRef
end
if version() >= v"16"
    register(ConstantTargetNone, API.LLVMConstantTargetNoneValueKind)
end


"""
    ConstantInt <: LLVM.ConstantData

A constant integer value.
"""
@checked struct ConstantInt <: ConstantData
    ref::API.LLVMValueRef
end
register(ConstantInt, API.LLVMConstantIntValueKind)

# NOTE: fixed set for dispatch, also because we can't rely on sizeof(T)==width(T)
const WideInteger = Union{Int64, UInt64}
ConstantInt(typ::IntegerType, val::WideInteger, signed=false) =
    ConstantInt(API.LLVMConstInt(typ, reinterpret(Culonglong, val), signed))
const SmallInteger = Union{Bool, Int8, Int16, Int32, UInt8, UInt16, UInt32}
ConstantInt(typ::IntegerType, val::SmallInteger, signed=false) =
    ConstantInt(typ, convert(Int64, val), signed)

"""
    ConstantInt(typ::LLVM.IntegerType, val, [signed=false])

Create a constant integer value of the given type and value. If `signed` is `true`, the
value is treated as a signed integer.
"""
function ConstantInt(typ::IntegerType, val::Integer, signed=false)
    # the two's complement words of the value, truncated to the width of the type
    numwords = cld(width(typ), 64)
    words = Vector{UInt64}(undef, numwords)
    for i in 1:numwords
        words[i] = (val >> (64*(i-1))) % UInt64
    end
    return ConstantInt(API.LLVMConstIntOfArbitraryPrecision(typ, numwords, words))
end

"""
    ConstantInt(val::Integer)

Create a constant integer value of the appropriate type for the given value.
"""
ConstantInt(val::Integer)

# NOTE: fixed set where sizeof(T) does match the numerical width
const SizeableInteger = Union{Int8, Int16, Int32, Int64, Int128,
                              UInt8, UInt16, UInt32, UInt64, UInt128}
function ConstantInt(val::T) where T<:SizeableInteger
    typ = IntType(sizeof(T)*8)
    return ConstantInt(typ, val, T<:Signed)
end

# Booleans are encoded with a single bit, so we can't use sizeof
ConstantInt(val::Bool) = ConstantInt(Int1Type(), val ? 1 : 0)

"""
    convert(::Type{<:Integer}, val::ConstantInt)

Convert a constant integer value back to a Julia integer.
"""
Base.convert(::Type, val::ConstantInt)

function Base.convert(::Type{T}, val::ConstantInt) where {T<:Union{Signed,Unsigned}}
    bits = width(value_type(val))
    if bits <= 64
        return T <: Signed ? convert(T, API.LLVMConstIntGetSExtValue(val)) :
                             convert(T, API.LLVMConstIntGetZExtValue(val))
    end

    # wider constants are read word by word, as LLVM only returns 64-bit values
    words = Vector{UInt64}(undef, cld(bits, 64))
    API.LLVMExtraConstIntGetWords(val, words)
    x = big(0)
    for (i, word) in enumerate(words)
        x |= big(word) << (64*(i-1))
    end
    if T <: Signed && isodd(x >> (bits-1))
        x -= big(1) << bits
    end
    return convert(T, x)
end

# Booleans aren't Signed or Unsigned
Base.convert(::Type{Bool}, val::ConstantInt) = convert(Int, val) != 0


"""
    ConstantFP <: LLVM.ConstantData

A constant floating point value.

# Properties

    val.bitpattern

The bit pattern of a constant floating point value, as the smallest unsigned integer that
can hold it (e.g., `UInt32` for `float`, or `UInt128` for `x86_fp80`).

See also [`ConstantFP`](@ref), which can create a constant from its bit pattern.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct ConstantFP <: ConstantData
    ref::API.LLVMValueRef
end
register(ConstantFP, API.LLVMConstantFPValueKind)

"""
    ConstantFP(typ::LLVMType, val::Real)

Create a constant floating point value of the given type and value.
"""
ConstantFP(typ::FloatingPointType, val::Real) =
    ConstantFP(API.LLVMConstReal(typ, Cdouble(val)))

"""
    ConstantFP(val::Real)

Create a constant floating point value of the appropriate type for the given value.
"""
ConstantFP(val::Real)

ConstantFP(val::Float64) = ConstantFP(DoubleType(), val)
ConstantFP(val::Float32) = ConstantFP(FloatType(), val)
ConstantFP(val::Float16) = ConstantFP(HalfType(), val)

"""
    convert(::Type{<:AbstractFloat}, val::ConstantFP)

Convert a constant floating point value back to a Julia floating point number.
"""
Base.convert(::Type{T}, val::ConstantFP) where {T<:AbstractFloat} =
    convert(T, API.LLVMConstRealGetDouble(val, Ref{API.LLVMBool}()))

# bit patterns

fp_width(::HalfType) = 16
fp_width(::BFloatType) = 16
fp_width(::FloatType) = 32
fp_width(::DoubleType) = 64
fp_width(::X86FP80Type) = 80
fp_width(::FP128Type) = 128
fp_width(::PPCFP128Type) = 128

# the smallest unsigned integer that can hold a floating-point value of the given width
fp_container(width::Int) =
    width <= 16 ? UInt16 : width <= 32 ? UInt32 : width <= 64 ? UInt64 : UInt128

"""
    ConstantFP(typ::FloatingPointType; bits::Unsigned)

Create a constant floating point value of the given type from its bit pattern. As opposed
to passing a `Real` value, which is converted to `Float64` first, this can represent every
value of wider types like `fp128` or `x86_fp80`, as well as the payload of NaN values.

Use the [`bitpattern`](@ref LLVM.ConstantFP) property to get the bit pattern of an existing
constant.

# Examples

```julia
julia> ConstantFP(LLVM.FP128Type(); bits=0x3fff0000000000000000000000000000)
fp128 0xL00000000000000003FFF000000000000
```
"""
function ConstantFP(typ::FloatingPointType; bits::Unsigned)
    width = fp_width(typ)
    if 8*sizeof(bits) > width && bits >> width != 0
        throw(ArgumentError("Bit pattern $(repr(bits)) does not fit in a $width-bit floating-point type"))
    end
    bits = UInt128(bits)
    words = UInt64[(bits >> (64*(i-1))) % UInt64 for i in 1:cld(width, 64)]
    ConstantFP(API.LLVMConstFPFromBits(typ, words))
end

function bitpattern(val::ConstantFP)
    typ = value_type(val)
    width = fp_width(typ isa VectorType ? element_type(typ) : typ)
    words = Vector{UInt64}(undef, cld(width, 64))
    API.LLVMExtraConstFPGetBits(val, words)
    bits = zero(UInt128)
    for (i, word) in enumerate(words)
        bits |= UInt128(word) << (64*(i-1))
    end
    return bits % fp_container(width)
end

@property ConstantFP bitpattern


# sequential data

@vocabulary IR ConstantDataSequential, ConstantDataArray, ConstantDataVector

"""
    LLVM.ConstantDataSequential <: LLVM.Constant

Abstract supertype of constant arrays and vectors of simple data values:
[`ConstantDataArray`](@ref) and [`ConstantDataVector`](@ref).
"""
abstract type ConstantDataSequential <: Constant end

# ConstantData can only contain primitive types (1/2/4/8 byte integers, float/half), as
# opposed to ConstantAggregate which can contain arbitrary LLVM values. LLVM uses them
# interchangeably, e.g., LLVMConstArray returns a ConstantDataArray when the elements are
# simple data, so both are accessed using the `elements` property.

"""
    ConstantDataArray <: LLVM.ConstantDataSequential

A constant array of simple data values, i.e., whose element type is a simple 1/2/4/8-byte
integer or half/bfloat/float/double, and whose elements are just simple data values. Its
elements are available as the `elements` property, see [`LLVM.ConstantAggregate`](@ref).

See also: [`ConstantArray`](@ref)
"""
@checked struct ConstantDataArray <: ConstantDataSequential
    ref::API.LLVMValueRef
end
register(ConstantDataArray, API.LLVMConstantDataArrayValueKind)

"""
    ConstantDataArray(typ::LLVMType, data::AbstractVector)

Create a constant array of simple data values of the given type and data.

The element type needs to be a 1/2/4/8-byte integer or a half/bfloat/float/double type, of
the same size as the elements of `data`, whose bits are used as-is.
"""
function ConstantDataArray(typ::LLVMType, data::AbstractVector{T}) where {T <: Union{Integer, AbstractFloat}}
    # the element types supported by ConstantDataSequential
    bits = if typ isa IntegerType && width(typ) in (8, 16, 32, 64)
        width(typ)
    elseif typ isa Union{HalfType, BFloatType}
        16
    elseif typ isa FloatType
        32
    elseif typ isa DoubleType
        64
    else
        throw(ArgumentError("ConstantDataArray does not support elements of type $typ; use ConstantArray instead"))
    end
    isbitstype(T) ||
        throw(ArgumentError("ConstantDataArray requires elements of a concrete bits type, got $T"))
    8*sizeof(T) == bits ||
        throw(ArgumentError("Elements of type $T do not match the size of LLVM type $typ"))

    # the data is passed as a pointer, so make sure it is stored contiguously
    data isa Array || (data = collect(data))
    return ConstantDataArray(API.LLVMConstDataArray(typ, data, sizeof(data)))
end

"""
    ConstantDataArray(data::AbstractVector)

Create a constant array of simple data values from a Julia vector.
"""
ConstantDataArray(::AbstractVector)

# shorthands with arrays of plain Julia data
# FIXME: duplicates the ConstantInt/ConstantFP conversion rules
# XXX: X[X(...)] instead of X.(...) because of empty-container inference
ConstantDataArray(data::AbstractVector{T}) where {T<:Integer} =
    ConstantDataArray(IntType(sizeof(T)*8), data)
ConstantDataArray(data::AbstractVector{Bool}) =
    throw(ArgumentError("ConstantDataArray does not support elements of type i1; use ConstantArray instead"))
ConstantDataArray(data::AbstractVector{Float64}) =
    ConstantDataArray(DoubleType(), data)
ConstantDataArray(data::AbstractVector{Float32}) =
    ConstantDataArray(FloatType(), data)
ConstantDataArray(data::AbstractVector{Float16}) =
    ConstantDataArray(HalfType(), data)

@vocabulary IR isstring

"""
    isstring(val::Value)

Check whether the given value is a constant string, i.e., a constant array of `i8`
values, like C++'s `ConstantDataSequential::isString`. Its contents can be retrieved using
[`String`](@ref String(::ConstantDataArray)).
"""
isstring(val::Value) = val isa ConstantDataArray && Bool(API.LLVMIsConstantString(val))

"""
    String(str::ConstantDataArray)

Get the contents of a constant string, like C++'s `ConstantDataSequential::getAsString`.
This includes all NUL characters, e.g., the one that terminates a C string. Throws an
`ArgumentError` if the array is not a string; see [`isstring`](@ref).
"""
function Base.String(str::ConstantDataArray)
    isstring(str) || throw(ArgumentError("Constant array of type $(value_type(str)) is not a string"))
    len = Ref{Csize_t}()
    data = API.LLVMGetAsString(str, len)
    return unsafe_string(convert(Ptr{UInt8}, data), len[])
end

"""
    ConstantDataVector <: LLVM.ConstantDataSequential

A constant vector of simple data values, i.e., whose element type is a simple 1/2/4/8-byte
integer or half/bfloat/float/double, and whose elements are just simple data values. Its
elements are available as the `elements` property, see [`LLVM.ConstantAggregate`](@ref).
"""
@checked struct ConstantDataVector <: ConstantDataSequential
    ref::API.LLVMValueRef
end
register(ConstantDataVector, API.LLVMConstantDataVectorValueKind)


# aggregate zero

@vocabulary IR ConstantAggregateZero

"""
    ConstantAggregateZero <: LLVM.ConstantData

The `zeroinitializer` of an array, structure or vector type, as created by
[`null`](@ref) or by LLVM for aggregates whose elements are all zero. Its elements are
available as the `elements` property, see [`LLVM.ConstantAggregate`](@ref).
"""
@checked struct ConstantAggregateZero <: ConstantData
    ref::API.LLVMValueRef
end
register(ConstantAggregateZero, API.LLVMConstantAggregateZeroValueKind)


## regular aggregate

"""
    LLVM.ConstantAggregate <: LLVM.Constant

Abstract supertype of constant arrays, structs and vectors whose elements are other
constants: [`ConstantArray`](@ref), [`ConstantStruct`](@ref) and `ConstantVector`.

# Properties

    c.elements

The elements of an aggregate constant, as a read-only vector of constants, e.g., `i32 2`
for the second element of `[3 x i32] [i32 1, i32 2, i32 3]`. This property is also
available for arrays and vectors of simple data (`ConstantDataArray` and
`ConstantDataVector`) and for `zeroinitializer` (`ConstantAggregateZero`), which LLVM uses
to represent aggregate constants whose elements are simple data or zero. Nested aggregates
are elements themselves, i.e., the elements of a constant of type `[2 x [2 x i32]]` are two
constants of type `[2 x i32]`.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
abstract type ConstantAggregate <: Constant end
@vocabulary IR ConstantAggregate

# arrays

@vocabulary IR ConstantArray

"""
    ConstantArray <: LLVM.ConstantAggregate

A constant array of values. Its elements are available as the `elements` property, see
[`LLVM.ConstantAggregate`](@ref).
"""
@checked struct ConstantArray <: ConstantAggregate
    ref::API.LLVMValueRef
end
register(ConstantArray, API.LLVMConstantArrayValueKind)

# generic constructor taking an array of constants
"""
    ConstantArray(typ::LLVMType, data::AbstractArray)

Create a constant array of values of the given type and data.

!!! note

    When using simple data types, this constructor can also return a
    [`ConstantDataArray`](@ref).
"""
function ConstantArray(typ::LLVMType, data::AbstractArray{<:Constant,N}) where {N}
    @assert all(x->x==typ, value_type.(data))

    if N == 1
        # XXX: this can return a ConstDataArray (presumably as an optimization?)
        return Value(API.LLVMConstArray(typ, Array(data), length(data)))
    end

    ca_vec = map(x->ConstantArray(typ, x), eachslice(data, dims=1))
    ca_typ = value_type(first(ca_vec))

    return ConstantArray(API.LLVMConstArray(ca_typ, ca_vec, length(ca_vec)))
end

# shorthands with arrays of plain Julia data
# FIXME: duplicates the ConstantInt/ConstantFP conversion rules
# XXX: X[X(...)] instead of X.(...) because of empty-container inference
ConstantArray(data::AbstractArray{T}) where {T<:Integer} =
    ConstantArray(IntType(sizeof(T)*8), ConstantInt[ConstantInt(x) for x in data])
ConstantArray(data::AbstractArray{Bool}) =
    ConstantArray(Int1Type(), ConstantInt[ConstantInt(x) for x in data])
ConstantArray(data::AbstractArray{Float16}) =
    ConstantArray(HalfType(), ConstantFP[ConstantFP(x) for x in data])
ConstantArray(data::AbstractArray{Float32}) =
    ConstantArray(FloatType(), ConstantFP[ConstantFP(x) for x in data])
ConstantArray(data::AbstractArray{Float64}) =
    ConstantArray(DoubleType(), ConstantFP[ConstantFP(x) for x in data])

"""
    ConstantArray(data::AbstractArray)

Create a constant array of values from a Julia array, using the appropriate constant type.
"""
ConstantArray(::AbstractArray)

# structs

@vocabulary IR ConstantStruct

"""
    ConstantStruct <: LLVM.ConstantAggregate

A constant struct of values.
"""
@checked struct ConstantStruct <: ConstantAggregate
    ref::API.LLVMValueRef
end
register(ConstantStruct, API.LLVMConstantStructValueKind)

ConstantStructOrAggregateZero(value) = Value(value)::Union{ConstantStruct,ConstantAggregateZero}

"""
    ConstantStruct(values::AbstractVector{<:Constant}; packed=false)

Create an anonymous constant struct of the given values.
"""
ConstantStruct(values::AbstractVector{<:Constant}; packed::Bool=false) =
    ConstantStructOrAggregateZero(API.LLVMConstStructInContext(context(), as_vector(values),
                                                               length(values), packed))

"""
    ConstantStruct(typ::LLVM.StructType, values::AbstractVector{<:Constant})

Create a constant struct of the given type and values.
"""
ConstantStruct(typ::StructType, values::AbstractVector{<:Constant}) =
    ConstantStructOrAggregateZero(API.LLVMConstNamedStruct(typ, as_vector(values),
                                                           length(values)))

"""
    ConstantStruct(value::T, [name=String(nameof(T)), anonymous=false, packed=false])

Create a constant struct from an (isbits) Julia struct instance.
"""
function ConstantStruct(value::T, name::AbstractString=String(nameof(T));
                        anonymous::Bool=false, packed::Bool=false) where {T}
    isbitstype(T) || throw(ArgumentError("Can only create a ConstantStruct from an isbits struct"))
    isprimitivetype(T) && throw(ArgumentError("Cannot create a ConstantStruct from a primitive value"))

    constants = Vector{Constant}()
    for fieldname in fieldnames(T)
        field = getfield(value, fieldname)

        if isa(field, Integer)
            push!(constants, ConstantInt(field))
        elseif isa(field, AbstractFloat)
            push!(constants, ConstantFP(field))
        else # TODO: nested structs?
            throw(ArgumentError("only structs with boolean, integer and floating point fields are allowed"))
        end
    end

    if anonymous
        ConstantStruct(constants; packed)
    elseif haskey(types(context()), name)
        typ = types(context())[name]
        if collect(elements(typ)) != value_type.(constants)
            throw(ArgumentError("Cannot create struct $name {$(join(value_type.(constants), ", "))} as it is already defined in this context as {$(join(elements(typ), ", "))}."))
        end
        ConstantStruct(typ, constants)
    else
        typ = StructType(name)
        elements!(typ, value_type.(constants))
        ConstantStruct(typ, constants)
    end
end

# vectors

@vocabulary IR ConstantVector

"""
    ConstantVector <: LLVM.ConstantAggregate

A constant vector of other constants, which LLVM creates for vectors whose elements
aren't simple data values (see [`ConstantDataVector`](@ref)). Its elements are available as
the `elements` property, see [`LLVM.ConstantAggregate`](@ref).
"""
@checked struct ConstantVector <: ConstantAggregate
    ref::API.LLVMValueRef
end
register(ConstantVector, API.LLVMConstantVectorValueKind)


## aggregate elements

struct ConstantAggregateElementSet <: AbstractVector{Constant}
    c::Constant
end

const AnyConstantAggregate =
    Union{ConstantAggregate, ConstantDataSequential, ConstantAggregateZero}

elements(c::AnyConstantAggregate) = ConstantAggregateElementSet(c)

@property AnyConstantAggregate elements

function Base.size(iter::ConstantAggregateElementSet)
    typ = value_type(iter.c)
    n = typ isa StructType ? length(elements(typ)) :
        typ isa ArrayType ? array_length(typ) : vector_length(typ)
    return (n,)
end

Base.IndexStyle(::Type{ConstantAggregateElementSet}) = IndexLinear()

function Base.getindex(iter::ConstantAggregateElementSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return Value(API.LLVMGetAggregateElement(iter.c, i-1))::Constant
end


## constant expressions

@vocabulary IR ConstantExpr
@vocabulary Build const_neg, const_nswneg, const_not, const_add, const_nswadd,
                  const_nuwadd, const_sub, const_nswsub, const_nuwsub, const_xor, const_gep,
                  const_inbounds_gep, const_trunc, const_ptrtoint, const_inttoptr,
                  const_bitcast, const_addrspacecast, const_truncorbitcast,
                  const_pointercast, const_shufflevector

"""
    LLVM.ConstantExpr <: LLVM.Constant

A constant value that is initialized with an expression using other constant values.

Constant expressions are created using `const_`-prefixed functions, which correspond to
the LLVM IR instructions: `const_neg`, `const_not`, etc.

# Properties

    ce.opcode

The opcode of the constant expression, e.g., `LLVM.Opcode.Add`.

    ce.source_element_type

The type that a `getelementptr` constant expression indexes into. Throws an
`ArgumentError` for other constant expressions.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
@checked struct ConstantExpr <: Constant
    ref::API.LLVMValueRef
end
register(ConstantExpr, API.LLVMConstantExprValueKind)

opcode(ce::ConstantExpr) = API.LLVMGetConstOpcode(ce)

@property ConstantExpr opcode

const_neg(val::Constant) =
    Value(API.LLVMConstNeg(val))

const_nswneg(val::Constant) =
    Value(API.LLVMConstNSWNeg(val))

const_not(val::Constant) =
    Value(API.LLVMConstNot(val))

const_add(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstAdd(lhs, rhs))

const_nswadd(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNSWAdd(lhs, rhs))

const_nuwadd(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNUWAdd(lhs, rhs))

const_sub(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstSub(lhs, rhs))

const_nswsub(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNSWSub(lhs, rhs))

const_nuwsub(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNUWSub(lhs, rhs))

const_xor(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstXor(lhs, rhs))

function const_gep(Ty::LLVMType, val::Constant, Indices::AbstractVector{<:Constant})
    Value(API.LLVMConstGEP2(Ty, val, as_vector(Indices), length(Indices)))
end

function const_inbounds_gep(Ty::LLVMType, val::Constant,
                            Indices::AbstractVector{<:Constant})
    Value(API.LLVMConstInBoundsGEP2(Ty, val, as_vector(Indices), length(Indices)))
end

const_trunc(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstTrunc(val, ToType))

const_ptrtoint(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstPtrToInt(val, ToType))

const_inttoptr(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstIntToPtr(val, ToType))

const_bitcast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstBitCast(val, ToType))

const_addrspacecast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstAddrSpaceCast(val, ToType))

const_truncorbitcast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstTruncOrBitCast(val, ToType))

const_pointercast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstPointerCast(val, ToType))

const_extractelement(vector::Constant, index::Constant) =
    Value(API.LLVMConstExtractElement(vector ,index))

const_insertelement(vector::Constant, element::Value, index::Constant) =
    Value(API.LLVMConstInsertElement(vector ,element, index))

const_shufflevector(vector1::Constant, vector2::Constant, mask::Constant) =
    Value(API.LLVMConstShuffleVector(vector1, vector2, mask))

@vocabulary Build const_splat

"""
    const_splat(typ::LLVM.VectorType, value::Constant)
    const_splat(typ::LLVM.VectorType, value::Real)

Create a constant vector of type `typ` whose elements are all `value`, which must be a
constant of the element type of `typ`, or a Julia number that is converted to one: using
[`ConstantFP`](@ref) for a vector of floating-point values, or [`ConstantInt`](@ref) for a
vector of integers, which requires an `Integer`. For example, to create a vector of
floating-point ones:

```julia
const_splat(LLVM.VectorType(LLVM.FloatType(), 4), 1)
```

The result is the constant that LLVM uses to represent the splat, e.g., a
`ConstantDataVector`, or a `ConstantAggregateZero` for zeros, so it is only guaranteed to
be a `Constant`.
"""
function const_splat(typ::VectorType, value::Constant)
    context(typ) == context(value) ||
        throw(ArgumentError("The vector type and the value belong to different contexts"))
    eltyp = element_type(typ)
    value_type(value) == eltyp ||
        throw(ArgumentError("Cannot splat a value of type $(string(value_type(value))) into a vector of $(string(eltyp))"))
    Value(API.LLVMExtraConstVectorSplat(typ, value))::Constant
end

function const_splat(typ::VectorType, value::Real)
    eltyp = element_type(typ)
    element = if eltyp isa FloatingPointType
        ConstantFP(eltyp, value)
    elseif eltyp isa IntegerType && value isa Integer
        # sign-extend signed values to wider element types
        ConstantInt(eltyp, value, value isa Signed)
    else
        throw(ArgumentError("Cannot splat a $(typeof(value)) into a vector of $(string(eltyp))"))
    end
    const_splat(typ, element)
end

if version() < v"17"

@vocabulary Build const_select

const_select(cond::Constant, if_true::Value, if_false::Value) =
    Value(API.LLVMConstSelect(cond, if_true, if_false))

end

if version() < v"18"

@vocabulary Build const_and, const_or, const_lshr, const_ashr, const_sext, const_zext,
                  const_fptrunc, const_fpext, const_fptoui, const_fptosi, const_uitofp,
                  const_sitofp, const_intcast, const_fpcast, const_zextorbitcast,
                  const_sextorbitcast

const_and(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstAnd(lhs, rhs))

const_or(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstOr(lhs, rhs))

const_lshr(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstLShr(lhs, rhs))

const_ashr(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstAShr(lhs, rhs))

const_sext(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstSExt(val, ToType))

const_zext(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstZExt(val, ToType))

const_fptrunc(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstFPTrunc(val, ToType))

const_fpext(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstFPExt(val, ToType))

const_fptoui(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstFPToUI(val, ToType))

const_fptosi(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstFPToSI(val, ToType))

const_uitofp(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstUIToFP(val, ToType))

const_sitofp(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstSIToFP(val, ToType))

const_intcast(val::Constant, ToType::LLVMType, isSigned::Bool) =
    Value(API.LLVMConstIntCast(val, ToType, isSigned))

const_fpcast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstFPCast(val, ToType))

const_zextorbitcast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstZExtOrBitCast(val, ToType))

const_sextorbitcast(val::Constant, ToType::LLVMType) =
    Value(API.LLVMConstSExtOrBitCast(val, ToType))

end

if version() < v"19"

@vocabulary Build const_icmp, const_fcmp, const_shl

const_icmp(Predicate::API.LLVMIntPredicate, lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstICmp(Predicate, lhs, rhs))

const_fcmp(Predicate::API.LLVMRealPredicate, lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstFCmp(Predicate, lhs, rhs))

const_shl(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstShl(lhs, rhs))

end

if version() < v"21"

@vocabulary Build const_mul, const_nswmul, const_nuwmul

const_mul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstMul(lhs, rhs))

const_nswmul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNSWMul(lhs, rhs))

const_nuwmul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNUWMul(lhs, rhs))

end

# the documentation of the constant expressions that this version of LLVM supports
let unary = [(:const_neg, "`sub 0, val`"), (:const_nswneg, "`sub nsw 0, val`"),
             (:const_not, "`xor val, -1`")],
    binary = [(:const_add, "`add`"), (:const_nswadd, "`add nsw`"), (:const_nuwadd, "`add nuw`"),
              (:const_sub, "`sub`"), (:const_nswsub, "`sub nsw`"), (:const_nuwsub, "`sub nuw`"),
              (:const_mul, "`mul`"), (:const_nswmul, "`mul nsw`"), (:const_nuwmul, "`mul nuw`"),
              (:const_xor, "`xor`"), (:const_and, "`and`"), (:const_or, "`or`"),
              (:const_shl, "`shl`"), (:const_lshr, "`lshr`"), (:const_ashr, "`ashr`")],
    casts = [(:const_trunc, "`trunc`"), (:const_sext, "`sext`"), (:const_zext, "`zext`"),
             (:const_fptrunc, "`fptrunc`"), (:const_fpext, "`fpext`"),
             (:const_fptoui, "`fptoui`"), (:const_fptosi, "`fptosi`"),
             (:const_uitofp, "`uitofp`"), (:const_sitofp, "`sitofp`"),
             (:const_ptrtoint, "`ptrtoint`"), (:const_inttoptr, "`inttoptr`"),
             (:const_bitcast, "`bitcast`"), (:const_addrspacecast, "`addrspacecast`"),
             (:const_zextorbitcast, "`zext` (or `bitcast`, if the types have the same size)"),
             (:const_sextorbitcast, "`sext` (or `bitcast`, if the types have the same size)"),
             (:const_truncorbitcast, "`trunc` (or `bitcast`, if the types have the same size)"),
             (:const_pointercast, "pointer cast (`bitcast`, `addrspacecast` or `ptrtoint`)"),
             (:const_fpcast, "floating-point cast (`fptrunc` or `fpext`)")],
    note = "LLVM folds the expression if it can, so the result is a `Constant`, not " *
           "necessarily a `ConstantExpr`."
    docs = Pair{Symbol,String}[]
    for (f, expr) in unary
        push!(docs, f => """
                  $f(val::Constant) -> Constant

              Create the constant expression $expr. $note
              """)
    end
    for (f, op) in binary
        push!(docs, f => """
                  $f(lhs::Constant, rhs::Constant) -> Constant

              Create the constant expression $op of `lhs` and `rhs`. $note
              """)
    end
    for (f, op) in casts
        push!(docs, f => """
                  $f(val::Constant, dest_type::LLVMType) -> Constant

              Create the constant expression that converts `val` to `dest_type` using a $op.
              $note
              """)
    end
    append!(docs, [
        :const_intcast => """
                const_intcast(val::Constant, dest_type::LLVMType, signed::Bool) -> Constant

            Create the constant expression that converts the integer `val` to the integer type
            `dest_type`, using a `trunc`, or a `sext` or `zext` depending on `signed`. $note
            """,
        :const_gep => """
                const_gep(type::LLVMType, ptr::Constant, indices::AbstractVector{<:Constant})
                    -> Constant
                const_inbounds_gep(type::LLVMType, ptr::Constant,
                                   indices::AbstractVector{<:Constant}) -> Constant

            Create the constant expression `getelementptr` (or `getelementptr inbounds`) that
            computes the address of an element of the value of `type` at `ptr`, using the
            0-based `indices`. $note
            """,
        :const_shufflevector => """
                const_shufflevector(v1::Constant, v2::Constant, mask::Constant) -> Constant

            Create the constant expression `shufflevector` of `v1` and `v2`, using the constant
            vector `mask`. $note
            """,
        :const_extractelement => """
                const_extractelement(vec::Constant, index::Constant) -> Constant

            Create the constant expression `extractelement` of the element at the 0-based `index`
            of `vec`. $note
            """,
        :const_insertelement => """
                const_insertelement(vec::Constant, elt::Value, index::Constant) -> Constant

            Create the constant expression `insertelement` that replaces the element at the
            0-based `index` of `vec` by `elt`. $note
            """,
        :const_select => """
                const_select(cond::Constant, then::Value, else::Value) -> Constant

            Create the constant expression `select`, which is `then` if `cond` is true and
            `else` otherwise. $note
            """,
        :const_icmp => """
                const_icmp(predicate::LLVM.IntPredicate.T, lhs::Constant, rhs::Constant)
                    -> Constant
                const_fcmp(predicate::LLVM.RealPredicate.T, lhs::Constant, rhs::Constant)
                    -> Constant

            Create the constant expression `icmp` or `fcmp` that compares `lhs` and `rhs` using
            `predicate`. $note
            """])
    for (f, doc) in docs
        isdefined(@__MODULE__, f) || continue
        @eval @doc $doc $f
    end
    for (f, other) in (:const_inbounds_gep => :const_gep, :const_fcmp => :const_icmp)
        isdefined(@__MODULE__, f) || continue
        @eval @doc (@doc $other) $f
    end
end

# TODO: alignof, sizeof


## pointer authentication

@vocabulary IR ConstantPtrAuth

"""
    ConstantPtrAuth <: LLVM.Constant

A signed pointer, `ptrauth (ptr @f, i32 0)` in LLVM IR, as used for pointer authentication
(e.g., on arm64e). This constant only exists on LLVM 19 and later.
"""
@checked struct ConstantPtrAuth <: Constant
    ref::API.LLVMValueRef
end
if version() >= v"19"
    register(ConstantPtrAuth, API.LLVMConstantPtrAuthValueKind)
end


## inline assembly

@vocabulary IR InlineAsm

"""
    InlineAsm <: LLVM.Constant

A constant inline assembly block.
"""
@checked struct InlineAsm <: Constant
    ref::API.LLVMValueRef
end
register(InlineAsm, API.LLVMInlineAsmValueKind)

"""
    InlineAsm(typ::LLVM.FunctionType, asm::String, constraints::String, side_effects::Bool,
              [align_stack::Bool=false])

Create a constant inline assembly block with the given type, assembly code, constraints,
and a boolean indicating whether the assembly has side effects. The optional boolean
`align_stack` specifies whether the stack should be aligned, forcing the compiler to
generate its usual stack alignment code in the prologue.
"""
InlineAsm(typ::FunctionType, asm::String, constraints::String,
          side_effects::Bool, align_stack::Bool=false) =
    InlineAsm(API.LLVMConstInlineAsm(typ, asm, constraints, side_effects, align_stack))


## global values

"""
    LLVM.GlobalValue <: LLVM.Constant

Abstract supertype for all global values.

# Properties

    gv.parent

The module that contains the global value.

    gv.global_value_type

The type of the global value.

This differs from the `value_type` property in that it is the type of the contained value,
not the type of the global value itself, which is always a pointer type.

    gv.linkage
    gv.linkage = linkage::LLVM.Linkage.T

The linkage of the global value.

    gv.section
    gv.section = section::String

The section of the global value, or an empty string if it isn't placed in a specific
section. Only global objects (functions, global variables and ifuncs) can be assigned a
section: the section of an alias is that of its aliasee, and cannot be changed.

    gv.visibility
    gv.visibility = visibility::LLVM.Visibility.T

The visibility of the global value.

    gv.dllstorage
    gv.dllstorage = storage::LLVM.DLLStorageClass.T

The DLL storage class of the global value.

    gv.unnamed_addr
    gv.unnamed_addr = kind::LLVM.UnnamedAddr.T

Whether the address of the global value is significant: `LLVM.UnnamedAddr.No` if it
is, `LLVM.UnnamedAddr.Local` if it is insignificant within the module
(`local_unnamed_addr`), and `LLVM.UnnamedAddr.Global` if it is insignificant
altogether (`unnamed_addr`), which allows merging it with other constants that have the
same initializer.

The properties of [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available too.
"""
abstract type GlobalValue <: Constant end

"""
    LLVM.GlobalObject <: LLVM.GlobalValue

Abstract supertype for global values that are backed by an actual object in memory, i.e.,
functions, global variables and ifuncs, but not aliases.

# Properties

    inst.metadata
    gv.metadata

The metadata attached to an instruction or a global object (a function or global variable),
as a dictionary-like view that maps the kind of metadata to a metadata node. The kind can be
an `MDKind`, like `LLVM.MD_dbg`, or the name of the kind, like `"tbaa"`. The view can be
iterated (in the case of an instruction, this includes its debug location), and is mutable:
assign to a kind to attach metadata, e.g., `inst.metadata["tbaa"] = node`, and use `delete!`
to remove it.

The properties of [`GlobalValue`](@ref LLVM.GlobalValue), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
abstract type GlobalObject <: GlobalValue end
@vocabulary IR GlobalObject

@vocabulary IR GlobalValue, isdeclaration

parent(val::GlobalValue) = Module(API.LLVMGetGlobalParent(val))

@property GlobalValue parent

global_value_type(val::GlobalValue) = LLVMType(API.LLVMGetGlobalValueType(val))

@property GlobalValue global_value_type

"""
    isdeclaration(val::LLVM.GlobalValue)

Check if the global value is a declaration, i.e. it does not have a definition.
"""
isdeclaration(val::GlobalValue) = API.LLVMIsDeclaration(val) |> Bool

linkage(val::GlobalValue) = API.LLVMGetLinkage(val)

linkage!(val::GlobalValue, linkage::API.LLVMLinkage) =
    API.LLVMSetLinkage(val, linkage)

@property GlobalValue linkage linkage!

function section(val::GlobalValue)
  #=
  The following started to fail on LLVM 4.0:
    @dispose ctx=Context() begin
      @dispose mod=LLVM.Module("SomeModule") begin
        st = LLVM.StructType("SomeType")
        ft = LLVM.FunctionType(st, [st])
        fn = LLVM.Function(mod, "SomeFunction", ft)
        section(fn) == ""
      end
      end
  =#
  section_ptr = API.LLVMGetSection(val)
  return section_ptr != C_NULL ? unsafe_string(section_ptr) : ""
end

section!(val::GlobalObject, sec::String) = API.LLVMSetSection(val, sec)

@property GlobalObject section section!

visibility(val::GlobalValue) = API.LLVMGetVisibility(val)

visibility!(val::GlobalValue, viz::API.LLVMVisibility) =
    API.LLVMSetVisibility(val, viz)

@property GlobalValue visibility visibility!

dllstorage(val::GlobalValue) = API.LLVMGetDLLStorageClass(val)

dllstorage!(val::GlobalValue, storage::API.LLVMDLLStorageClass) =
    API.LLVMSetDLLStorageClass(val, storage)

@property GlobalValue dllstorage dllstorage!

unnamed_addr(val::GlobalValue) = API.LLVMGetUnnamedAddress(val)

unnamed_addr!(val::GlobalValue, kind::API.LLVMUnnamedAddr) =
    API.LLVMSetUnnamedAddress(val, kind)

@property GlobalValue unnamed_addr unnamed_addr!


## global variables

@vocabulary IR GlobalVariable, erase!

"""
    GlobalVariable <: LLVM.GlobalObject

A global variable.

# Properties

    gv.initializer
    gv.initializer = val::Union{LLVM.Constant,Nothing}

The initializer of the global variable, or `nothing` if it has none (i.e., if it is a
declaration). Assigning `nothing` removes the current initializer.

    gv.threadlocal
    gv.threadlocal = flag::Bool

Whether the global variable is thread-local. This is a view of the `threadlocal_mode`
property: assigning `true` to a variable that is not thread-local selects the general
dynamic model, while assigning `false` makes the variable not thread-local. Assigning the
current value does not change the thread-local mode.

    gv.constant
    gv.constant = flag::Bool

Whether the global variable is a global constant, i.e., whether its value is immutable
throughout the runtime execution of the program.

This differs from `isconstant(gv)`, which checks whether a value is an LLVM constant, and
is true for every global variable (which represents a constant address).

    gv.threadlocal_mode
    gv.threadlocal_mode = mode::LLVM.ThreadLocalMode.T

The thread-local storage model of the global variable, e.g.,
`LLVM.ThreadLocalMode.GeneralDynamic`, or `LLVM.ThreadLocalMode.NotThreadLocal` if it is not
thread-local. See also the `threadlocal` property.

    gv.externally_initialized
    gv.externally_initialized = flag::Bool

Whether the global variable is externally initialized, i.e., whether its value may be
changed before the program starts running, so that optimizations cannot rely on its
initializer.

    gv.alignment
    gv.alignment = bytes::Integer

The alignment of the global variable in bytes, or 0 if it has no explicit alignment. The
assigned alignment must be a power of 2, or 0 to remove the explicit alignment.

    gv.next
    gv.prev

The next or previous global variable in the module, or `nothing` if there is none.

The properties of [`GlobalObject`](@ref LLVM.GlobalObject), [`GlobalValue`](@ref
LLVM.GlobalValue), [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available
too.
"""
@checked struct GlobalVariable <: GlobalObject
    ref::API.LLVMValueRef
end
register(GlobalVariable, API.LLVMGlobalVariableValueKind)

"""
    GlobalVariable(mod::LLVM.Module, typ::LLVM.Type, name::String, [addrspace=0])

Create a global variable in the given module with the given type, name, and optional
address space.
"""
GlobalVariable(mod::Module, typ::LLVMType, name::String, addrspace::Integer=0) =
    GlobalVariable(API.LLVMAddGlobalInAddressSpace(mod, typ,
                                                   name, addrspace))

"""
    erase!(gv::GlobalVariable)

Remove the global variable from its parent module and delete it.

!!! warning

    This function is unsafe as it does not check if the global variable is still used
    elsewhere.
"""
erase!(gv::GlobalVariable) = API.LLVMDeleteGlobal(gv)

function initializer(gv::GlobalVariable)
    init = API.LLVMGetInitializer(gv)
    init == C_NULL ? nothing : Value(init)
end

function initializer!(gv::GlobalVariable, val::Union{Constant,Nothing})
    api = version() >= v"20" ? API.LLVMSetInitializer : API.LLVMSetInitializer2
    api(gv, something(val, C_NULL))
end

@property GlobalVariable initializer initializer!

threadlocal(gv::GlobalVariable) = API.LLVMIsThreadLocal(gv) |> Bool

# only change the mode when needed, so that marking a thread-local variable as such does
# not replace a more specific model (unlike LLVM's `setThreadLocal`)
function threadlocal!(gv::GlobalVariable, flag::Bool)
    flag == threadlocal(gv) || API.LLVMSetThreadLocal(gv, flag)
    return
end

@property GlobalVariable threadlocal threadlocal!

constant(gv::GlobalVariable) = API.LLVMIsGlobalConstant(gv) |> Bool

constant!(gv::GlobalVariable, flag::Bool) = API.LLVMSetGlobalConstant(gv, flag)

@property GlobalVariable constant constant!

threadlocal_mode(gv::GlobalVariable) = API.LLVMGetThreadLocalMode(gv)

threadlocal_mode!(gv::GlobalVariable, mode::API.LLVMThreadLocalMode) =
    API.LLVMSetThreadLocalMode(gv, mode)

@property GlobalVariable threadlocal_mode threadlocal_mode!

externally_initialized(gv::GlobalVariable) = API.LLVMIsExternallyInitialized(gv) |> Bool

externally_initialized!(gv::GlobalVariable, flag::Bool) =
    API.LLVMSetExternallyInitialized(gv, flag)

@property GlobalVariable externally_initialized externally_initialized!

# alignments are powers of 2 passed to LLVM as a 32-bit integer. global objects can also have
# no explicit alignment, which is represented by 0.
function check_alignment(align; allow_zero::Bool=false)
    align === nothing || (allow_zero && align == 0) ||
        (0 < align <= typemax(Cuint) && ispow2(align)) ||
        throw(ArgumentError("Alignment must be a positive power of 2 up to 2^31" *
                            (allow_zero ? ", or 0 to remove it" : "") * ", got $align"))
end

alignment(gv::GlobalVariable) = API.LLVMGetAlignment(gv)

function alignment!(gv::GlobalVariable, bytes::Integer)
    check_alignment(bytes; allow_zero=true)
    API.LLVMSetAlignment(gv, bytes)
end

@property GlobalVariable alignment alignment!


## global aliases

@vocabulary IR GlobalAlias

"""
    GlobalAlias <: LLVM.GlobalValue

A global alias, i.e., a new symbol for an existing global value or constant expression.

# Properties

    alias.aliasee
    alias.aliasee = val::LLVM.Constant

The value that the global alias refers to. The type of an assigned value must match that of
the alias.

    alias.next
    alias.prev

The next or previous global alias in the module, or `nothing` if there is none.

The properties of [`GlobalValue`](@ref LLVM.GlobalValue), [`User`](@ref LLVM.User) and
[`Value`](@ref LLVM.Value) are available too.
"""
@checked struct GlobalAlias <: GlobalValue
    ref::API.LLVMValueRef
end
register(GlobalAlias, API.LLVMGlobalAliasValueKind)

"""
    GlobalAlias(mod::LLVM.Module, typ::LLVM.Type, aliasee::LLVM.Constant, name::String)

Create a global alias in the given module, with the given value type and name, referring to
the pointer constant `aliasee`. The address space of the alias is that of `aliasee`.

See also the `aliasee` property.
"""
function GlobalAlias(mod::Module, typ::LLVMType, aliasee::Constant, name::String)
    ptrtyp = value_type(aliasee)
    if !(ptrtyp isa PointerType)
        throw(ArgumentError("Aliasee must be a pointer, got a value of type $ptrtyp"))
    end
    # with typed pointers, the value type also needs to match that of the aliasee
    if PointerType(typ, addrspace(ptrtyp)) != ptrtyp
        throw(ArgumentError("Aliasee of type $ptrtyp does not match alias value type $typ"))
    end
    GlobalAlias(API.LLVMAddAlias2(mod, typ, addrspace(ptrtyp), aliasee, name))
end

"""
    GlobalAlias(mod::LLVM.Module, aliasee::LLVM.GlobalValue, name::String)

Create a global alias in the given module, with the given name, referring to the global
value `aliasee`. The value type and address space of the alias are taken from `aliasee`.
"""
GlobalAlias(mod::Module, aliasee::GlobalValue, name::String) =
    GlobalAlias(mod, global_value_type(aliasee), aliasee, name)

aliasee(alias::GlobalAlias) = Value(API.LLVMAliasGetAliasee(alias))

function aliasee!(alias::GlobalAlias, val::Constant)
    if value_type(val) != value_type(alias)
        throw(ArgumentError("Aliasee of type $(value_type(val)) does not match alias type $(value_type(alias))"))
    end
    API.LLVMAliasSetAliasee(alias, val)
end

@property GlobalAlias aliasee aliasee!

# LLVM does not support setting the section of an alias
@property GlobalAlias section


## global ifuncs

@vocabulary IR GlobalIFunc

"""
    GlobalIFunc <: LLVM.GlobalObject

An indirect function, whose address is determined at load time by calling a resolver
function.

# Properties

    ifunc.resolver
    ifunc.resolver = val::LLVM.Constant

The resolver of the ifunc. The type of an assigned value must be a pointer in the address
space of the ifunc.

    ifunc.next
    ifunc.prev

The next or previous ifunc in the module, or `nothing` if there is none.

The properties of [`GlobalObject`](@ref LLVM.GlobalObject), [`GlobalValue`](@ref
LLVM.GlobalValue), [`User`](@ref LLVM.User) and [`Value`](@ref LLVM.Value) are available
too.
"""
@checked struct GlobalIFunc <: GlobalObject
    ref::API.LLVMValueRef
end
register(GlobalIFunc, API.LLVMGlobalIFuncValueKind)

"""
    GlobalIFunc(mod::LLVM.Module, typ::LLVM.FunctionType, resolver::LLVM.Constant,
                name::String)

Create an indirect function in the given module, with the given name and function type,
whose address is computed by calling `resolver`. Note that `typ` is the type of the
resolved function, not that of the resolver. The address space of the ifunc is that of
`resolver`.

The resolver should be (or refer to) a function definition that returns a pointer; this is
not checked here, but by the IR verifier.

See also the `resolver` property.
"""
function GlobalIFunc(mod::Module, typ::FunctionType, resolver::Constant, name::String)
    ptrtyp = value_type(resolver)
    if !(ptrtyp isa PointerType)
        throw(ArgumentError("Resolver must be a pointer, got a value of type $ptrtyp"))
    end
    GlobalIFunc(API.LLVMAddGlobalIFunc(mod, name, ncodeunits(name), typ,
                                       addrspace(ptrtyp), resolver))
end

"""
    erase!(ifunc::GlobalIFunc)

Remove the ifunc from its parent module and delete it.

!!! warning

    This function is unsafe as it does not check if the ifunc is still used elsewhere.
"""
erase!(ifunc::GlobalIFunc) = API.LLVMEraseGlobalIFunc(ifunc)

resolver(ifunc::GlobalIFunc) = Value(API.LLVMGetGlobalIFuncResolver(ifunc))

function resolver!(ifunc::GlobalIFunc, val::Constant)
    ptrtyp = value_type(val)
    if !(ptrtyp isa PointerType) || addrspace(ptrtyp) != addrspace(value_type(ifunc))
        throw(ArgumentError("Resolver of type $ptrtyp is not a pointer in the address space of the ifunc"))
    end
    API.LLVMSetGlobalIFuncResolver(ifunc, val)
end

@property GlobalIFunc resolver resolver!
