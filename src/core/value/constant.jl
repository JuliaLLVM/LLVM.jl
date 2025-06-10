export null, isnull, all_ones

"""
    LLVM.Constant <: LLVM.User

Abstract supertype for all constant values.
"""
abstract type Constant <: User end

unsafe_destroy!(constant::Constant) = API.LLVMDestroyConstant(constant)

# forward declarations
@checked struct Module
    ref::API.LLVMModuleRef
end
abstract type Instruction <: User end


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

export PointerNull, UndefValue, PoisonValue, ConstantInt, ConstantFP

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
    valbits = ceil(Int, log2(abs(val))) + 1 # FIXME: doesn't work for val=0
    numwords = ceil(Int, valbits / 64)
    words = Vector{Culonglong}(undef, numwords)
    for i in 1:numwords
        words[i] = (val >> 64(i-1)) % Culonglong
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

Base.convert(::Type{T}, val::ConstantInt) where {T<:Unsigned} =
    convert(T, API.LLVMConstIntGetZExtValue(val))

Base.convert(::Type{T}, val::ConstantInt) where {T<:Signed} =
    convert(T, API.LLVMConstIntGetSExtValue(val))

# Booleans aren't Signed or Unsigned
Base.convert(::Type{Bool}, val::ConstantInt) = convert(Int, val) != 0


"""
    ConstantFP <: LLVM.ConstantData

A constant floating point value.
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


# sequential data

export ConstantDataSequential, ConstantDataArray, ConstantDataVector

abstract type ConstantDataSequential <: Constant end

# ConstantData can only contain primitive types (1/2/4/8 byte integers, float/half),
# as opposed to ConstantAggregate which can contain arbitrary LLVM values.
#
# however, LLVM seems to use both array types interchangeably, e.g., constructing
# a ConstArray through LLVMConstArray may return a ConstantDataArray (presumably as an
# optimization, when the data can be represented as densely packed primitive values).
# because of that, ConstantDataArray and ConstantArray need to behave the same way,
# concretely, indexing a ConstantDataArray has to return LLVM constant values...
#
# XXX: maybe we should just not expose ConstantDataArray then?
#      one advantage of keeping them separate is that creating a ConstantDataArray
#      is much cheaper (we should also be able to iterate much more efficiently,
#      but cannot support that as explained above).

# array interface
Base.eltype(cda::ConstantDataSequential) = eltype(value_type(cda))
Base.length(cda::ConstantDataSequential) = length(value_type(cda))
Base.size(cda::ConstantDataSequential) = (length(cda),)
function Base.getindex(cda::ConstantDataSequential, idx::Integer)
    @boundscheck 1 <= idx <= length(cda) || throw(BoundsError(cda, idx))
    Value(API.LLVMGetElementAsConstant(cda, idx-1))
end
function Base.collect(cda::ConstantDataSequential)
    constants = Array{Value}(undef, length(cda))
    for i in 1:length(cda)
        @inbounds constants[i] = cda[i]
    end
    return constants
end

"""
    ConstantDataArray <: LLVM.ConstantDataSequential

A constant array of simple data values, i.e., whose element type is a simple 1/2/4/8-byte
integer or half/bfloat/float/double, and whose elements are just simple data values

See also: [`ConstantArray`](@ref)
"""
@checked struct ConstantDataArray <: ConstantDataSequential
    ref::API.LLVMValueRef
end
register(ConstantDataArray, API.LLVMConstantDataArrayValueKind)

"""
    ConstantDataArray(typ::LLVMType, data::AbstractVector)

Create a constant array of simple data values of the given type and data.

!!! warning

    The memory layout of the data array must match the expected layout of the LLVM type.
"""
function ConstantDataArray(typ::LLVMType, data::AbstractVector{T}) where {T <: Union{Integer, AbstractFloat}}
    # TODO: can we look up the primitive size of the LLVM type?
    #       use that to assert it matches the Julia element type.
    return ConstantDataArray(API.LLVMConstDataArray(typ, data, length(data)))
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
    ConstantDataArray(Int1Type(), data)
ConstantDataArray(data::AbstractVector{Float64}) =
    ConstantDataArray(DoubleType(), data)
ConstantDataArray(data::AbstractVector{Float32}) =
    ConstantDataArray(FloatType(), data)
ConstantDataArray(data::AbstractVector{Float16}) =
    ConstantDataArray(HalfType(), data)

"""
    ConstantDataVector <: LLVM.ConstantDataSequential

A constant vector of simple data values, i.e., whose element type is a simple 1/2/4/8-byte
integer or half/bfloat/float/double, and whose elements are just simple data values
"""
@checked struct ConstantDataVector <: ConstantDataSequential
    ref::API.LLVMValueRef
end
register(ConstantDataVector, API.LLVMConstantDataVectorValueKind)


# aggregate zero

export ConstantAggregateZero

@checked struct ConstantAggregateZero <: ConstantData
    ref::API.LLVMValueRef
end
register(ConstantAggregateZero, API.LLVMConstantAggregateZeroValueKind)

# array interface
# FIXME: can we reuse the ::ConstantArray functionality with ConstantAggregateZero values?
#        probably works fine if we just get rid of the refcheck
Base.eltype(caz::ConstantAggregateZero) = eltype(value_type(caz))
Base.size(caz::ConstantAggregateZero) = (0,)
Base.length(caz::ConstantAggregateZero) = 0
Base.axes(caz::ConstantAggregateZero) = (Base.OneTo(0),)
Base.collect(caz::ConstantAggregateZero) = Value[]


## regular aggregate

# Abstract supertype for all constant aggregate values, which are aggregates of other
# constants, stored as operands.
abstract type ConstantAggregate <: Constant end

# arrays

export ConstantArray

"""
    ConstantArray <: LLVM.ConstantAggregate

A constant array of values.

This type implements the Julia array interface, so (to some extent) it can be used as a
regular Julia array.
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

"""
    collect(ca::ConstantArray)

Convert a constant array back to a Julia array.
"""
function Base.collect(ca::ConstantArray)
    constants = Array{Value}(undef, size(ca))
    for I in CartesianIndices(size(ca))
        @inbounds constants[I] = ca[Tuple(I)...]
    end
    return constants
end

# array interface
Base.eltype(ca::ConstantArray) = eltype(value_type(ca))
function Base.size(ca::ConstantArray)
    dims = Int[]
    typ = value_type(ca)
    while typ isa ArrayType
        push!(dims, length(typ))
        typ = eltype(typ)
    end
    return Tuple(dims)
end
Base.length(ca::ConstantArray) = prod(size(ca))
Base.axes(ca::ConstantArray) = Base.OneTo.(size(ca))

function Base.getindex(ca::ConstantArray, idx::Integer...)
    # multidimensional arrays are represented by arrays of arrays,
    # which we need to 'peel back' by looking at the operand sets.
    # for the final dimension, we use LLVMGetElementAsConstant
    @boundscheck Base.checkbounds_indices(Bool, axes(ca), idx) ||
        throw(BoundsError(ca, idx))
    I = CartesianIndices(size(ca))[idx...]
    for i in Tuple(I)
        if isempty(operands(ca))
            # XXX: is this valid? LLVMGetElementAsConstant is meant to be used with
            #      Constant*Data*Arrays, not ConstantArrays
            ca = Value(API.LLVMGetElementAsConstant(ca, i-1))
        else
            ca = (Base.@_propagate_inbounds_meta; operands(ca)[i])
        end
    end
    return ca
end

# structs

export ConstantStruct

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
    ConstantStruct(values::Vector{<:Constant}, [packed=false])

Create an anonymous constant struct of the given values.
"""
ConstantStruct(values::Vector{<:Constant}; packed::Bool=false) =
    ConstantStructOrAggregateZero(API.LLVMConstStructInContext(context(), values,
                                                               length(values), packed))

"""
    ConstantStruct(typ::LLVM.StructType, values::Vector{<:Constant})

Create a constant struct of the given type and values.
"""
ConstantStruct(typ::StructType, values::Vector{<:Constant}) =
    ConstantStructOrAggregateZero(API.LLVMConstNamedStruct(typ, values, length(values)))

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

export ConstantVector

@checked struct ConstantVector <: ConstantAggregate
    ref::API.LLVMValueRef
end
register(ConstantVector, API.LLVMConstantVectorValueKind)


## constant expressions

export ConstantExpr,

       const_neg, const_nswneg, const_nuwneg, const_not, const_add,
       const_nswadd, const_nuwadd, const_sub, const_nswsub, const_nuwsub, const_mul,
       const_nswmul, const_nuwmul, const_xor,
       const_gep, const_inbounds_gep, const_trunc,
       const_ptrtoint, const_inttoptr, const_bitcast,
       const_addrspacecast, const_truncorbitcast,
       const_pointercast, const_shufflevector

"""
    LLVM.ConstantExpr <: LLVM.Constant

A constant value that is initialized with an expression using other constant values.

Constant expressions are created using `const_`-prefixed functions, which correspond to
the LLVM IR instructions: `const_neg`, `const_not`, etc.
"""
@checked struct ConstantExpr <: Constant
    ref::API.LLVMValueRef
end
register(ConstantExpr, API.LLVMConstantExprValueKind)

opcode(ce::ConstantExpr) = API.LLVMGetConstOpcode(ce)

const_neg(val::Constant) =
    Value(API.LLVMConstNeg(val))

const_nswneg(val::Constant) =
    Value(API.LLVMConstNSWNeg(val))

const_nuwneg(val::Constant) =
    Value(API.LLVMConstNUWNeg(val))

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

const_mul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstMul(lhs, rhs))

const_nswmul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNSWMul(lhs, rhs))

const_nuwmul(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstNUWMul(lhs, rhs))

const_xor(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstXor(lhs, rhs))

function const_gep(Ty::LLVMType, val::Constant, Indices::Vector{<:Constant})
    Value(API.LLVMConstGEP2(Ty, val, Indices, length(Indices)))
end

function const_inbounds_gep(Ty::LLVMType, val::Constant, Indices::Vector{<:Constant})
    Value(API.LLVMConstInBoundsGEP2(Ty, val, Indices, length(Indices)))
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

if version() < v"17"

export const_select

const_select(cond::Constant, if_true::Value, if_false::Value) =
    Value(API.LLVMConstSelect(cond, if_true, if_false))

end

if version() < v"18"

export const_and, const_or, const_lshr, const_ashr, const_sext, const_zext,
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

export const_icmp, const_fcmp, const_shl

const_icmp(Predicate::API.LLVMIntPredicate, lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstICmp(Predicate, lhs, rhs))

const_fcmp(Predicate::API.LLVMRealPredicate, lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstFCmp(Predicate, lhs, rhs))

const_shl(lhs::Constant, rhs::Constant) =
    Value(API.LLVMConstShl(lhs, rhs))

end

# TODO: alignof, sizeof, block_address


## inline assembly

export InlineAsm

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
"""
abstract type GlobalValue <: Constant end

export GlobalValue, global_value_type,
       isdeclaration,
       linkage, linkage!,
       section, section!,
       visibility, visibility!,
       dllstorage, dllstorage!,
       unnamed_addr, unnamed_addr!,
       local_unnamed_addr, local_unnamed_addr!

"""
    parent(val::LLVM.GlobalValue)

Get the parent module of the global value.
"""
parent(val::GlobalValue) = Module(API.LLVMGetGlobalParent(val))

"""
    global_value_type(val::LLVM.GlobalValue)

Get the type of the global value.

This differs from [`value_type`](@ref) in that it returns the type of the contained value,
not the type of the global value itself which is always a pointer type.
"""
global_value_type(val::GlobalValue) = LLVMType(API.LLVMGetGlobalValueType(val))

"""
    isdeclaration(val::LLVM.GlobalValue)

Check if the global value is a declaration, i.e. it does not have a definition.
"""
isdeclaration(val::GlobalValue) = API.LLVMIsDeclaration(val) |> Bool

"""
    linkage(val::LLVM.GlobalValue)

Get the linkage of the global value.
"""
linkage(val::GlobalValue) = API.LLVMGetLinkage(val)

"""
    linkage!(val::LLVM.GlobalValue, linkage::LLVM.LLVMLinkage)

Set the linkage of the global value.
"""
linkage!(val::GlobalValue, linkage::API.LLVMLinkage) =
    API.LLVMSetLinkage(val, linkage)

"""
    section(val::LLVM.GlobalValue)

Get the section of the global value.
"""
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

"""
    section!(val::LLVM.GlobalValue, sec::String)

Set the section of the global value.
"""
section!(val::GlobalValue, sec::String) = API.LLVMSetSection(val, sec)

"""
    visibility(val::LLVM.GlobalValue)

Get the visibility of the global value.
"""
visibility(val::GlobalValue) = API.LLVMGetVisibility(val)

"""
    visibility!(val::LLVM.GlobalValue, viz::LLVM.LLVMVisibility)

Set the visibility of the global value.
"""
visibility!(val::GlobalValue, viz::API.LLVMVisibility) =
    API.LLVMSetVisibility(val, viz)

"""
    dllstorage(val::LLVM.GlobalValue)

Get the DLL storage class of the global value.
"""
dllstorage(val::GlobalValue) = API.LLVMGetDLLStorageClass(val)

"""
    dllstorage!(val::LLVM.GlobalValue, storage::LLVM.LLVMDLLStorageClass)

Set the DLL storage class of the global value.
"""
dllstorage!(val::GlobalValue, storage::API.LLVMDLLStorageClass) =
    API.LLVMSetDLLStorageClass(val, storage)

"""
    unnamed_addr(val::LLVM.GlobalValue)

Check if the global value has the unnamed address flag set.
"""
unnamed_addr(val::GlobalValue) = API.LLVMGetUnnamedAddress(val) === API.LLVMGlobalUnnamedAddr

"""
    unnamed_addr!(val::LLVM.GlobalValue, flag::Bool)

Set the unnamed address flag of the global value.
"""
unnamed_addr!(val::GlobalValue, flag::Bool) = API.LLVMSetUnnamedAddress(val, flag ? API.LLVMGlobalUnnamedAddr : API.LLVMNoUnnamedAddr)

"""
    local_unnamed_addr(val::LLVM.GlobalValue)

Check if the global value has the local unnamed address flag set.
"""
local_unnamed_addr(val::GlobalValue) = API.LLVMGetUnnamedAddress(val) === API.LLVMLocalUnnamedAddr

"""
    local_unnamed_addr!(val::LLVM.GlobalValue, flag::Bool)

Set the local unnamed address flag of the global value.
"""
local_unnamed_addr!(val::GlobalValue, flag::Bool) = API.LLVMSetUnnamedAddress(val, flag ? API.LLVMLocalUnnamedAddr : API.LLVMNoUnnamedAddr)


## global variables

abstract type GlobalObject <: GlobalValue end

export GlobalVariable, erase!,
       initializer, initializer!,
       isthreadlocal, threadlocal!,
       threadlocalmode, threadlocalmode!,
       isconstant, constant!,
       isextinit, extinit!,
       alignment, alignment!

"""
    GlobalVariable <: LLVM.GlobalObject

A global variable.
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

"""
    initializer(gv::GlobalVariable)

Get the initializer of the global variable.
"""
function initializer(gv::GlobalVariable)
    init = API.LLVMGetInitializer(gv)
    init == C_NULL ? nothing : Value(init)
end

"""
    initializer!(gv::GlobalVariable, val::Constant)

Set the initializer of the global variable. Setting the value to `nothing` removes the
current initializer.
"""
function initializer!(gv::GlobalVariable, val::Union{Constant,Nothing})
    api = version() >= v"20" ? API.LLVMSetInitializer : API.LLVMSetInitializer2
    api(gv, something(val, C_NULL))
end

"""
    isthreadlocal(gv::GlobalVariable)

Check if the global variable is thread-local.
"""
isthreadlocal(gv::GlobalVariable) = API.LLVMIsThreadLocal(gv) |> Bool

"""
    threadlocal!(gv::GlobalVariable, flag::Bool)

Set the thread-local flag of the global variable.
"""
threadlocal!(gv::GlobalVariable, bool) =
  API.LLVMSetThreadLocal(gv, bool)

"""
    isconstant(gv::GlobalVariable)

Check if the global variable is a global constant, i.e., its value is immutable throughout
the runtime execution of the program.
"""
isconstant(gv::GlobalVariable) = API.LLVMIsGlobalConstant(gv) |> Bool

"""
    constant!(gv::GlobalVariable, flag::Bool)

Set the constant flag of the global variable.
"""
constant!(gv::GlobalVariable, bool) = API.LLVMSetGlobalConstant(gv, bool)

"""
    threadlocalmode(gv::GlobalVariable)

Get the thread-local mode of the global variable.
"""
threadlocalmode(gv::GlobalVariable) = API.LLVMGetThreadLocalMode(gv)

"""
    threadlocalmode!(gv::GlobalVariable, mode::LLVM.LLVMThreadLocalMode)

Set the thread-local mode of the global variable.
"""
threadlocalmode!(gv::GlobalVariable, mode) = API.LLVMSetThreadLocalMode(gv, mode)

"""
    isextinit(gv::GlobalVariable)

Check if the global variable is externally initialized.
"""
isextinit(gv::GlobalVariable) = API.LLVMIsExternallyInitialized(gv) |> Bool

"""
    extinit!(gv::GlobalVariable, flag::Bool)

Set the externally initialized flag of the global variable.
"""
extinit!(gv::GlobalVariable, bool) = API.LLVMSetExternallyInitialized(gv, bool)

"""
    alignment(gv::GlobalVariable)

Get the alignment of the global variable.
"""
alignment(gv::GlobalVariable) = API.LLVMGetAlignment(gv)

"""
    alignment!(gv::GlobalVariable, bytes::Integer)

Set the alignment of the global variable.
"""
alignment!(gv::GlobalVariable, bytes::Integer) = API.LLVMSetAlignment(gv, bytes)
