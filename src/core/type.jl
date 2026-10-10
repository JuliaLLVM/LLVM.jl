@vocabulary IR LLVMType, issized, context

"""
    LLVMType

Abstract supertype for all LLVM types.

# Properties

    typ.context

The context in which the type was created.
"""
abstract type LLVMType end
@properties LLVMType

# subtypes must be immutable structs with a single `ref::API.LLVMTypeRef` field
# (see `check_layout`)
@inline function Base.unsafe_convert(::Type{API.LLVMTypeRef},
                                      @nospecialize(typ::LLVMType))
    typecheck_enabled && check_layout(typeof(typ), API.LLVMTypeRef)
    unsafe_load_ref(API.LLVMTypeRef, typ)
end

@inline propref(@nospecialize(x::LLVMType)) = Base.unsafe_convert(API.LLVMTypeRef, x)

# avoid specializing the conversions performed by `ccall` on the concrete wrapper type.
# wrappers consist of nothing but their reference, so there's nothing else to keep alive.
Base.cconvert(::Type{API.LLVMTypeRef}, @nospecialize(obj::LLVMType)) = obj
function Base.cconvert(::Type{Ptr{API.LLVMTypeRef}},
                       @nospecialize(objs::Vector{<:LLVMType}))
    R = API.LLVMTypeRef
    R[Base.unsafe_convert(R, obj) for obj in objs]
end


Base.sizeof(typ::LLVMType) = error("LLVM types are not sized")

const type_kinds = Vector{Type}(fill(Nothing, typemax(API.LLVMTypeKind)+1))
function identify(::Type{LLVMType}, ref::API.LLVMTypeRef)
    kind = API.LLVMGetTypeKind(ref)
    typ = @inbounds type_kinds[kind+1]
    typ === Nothing && error("Unknown type kind $kind")
    return typ
end
Base.@nospecializeinfer function register(@nospecialize(T::Type{<:LLVMType}),
                                          kind::API.LLVMTypeKind)
    check_layout(T, API.LLVMTypeRef)
    type_kinds[kind+1] = T
end

function refcheck(::Type{T}, ref::API.LLVMTypeRef) where T<:LLVMType
    ref==C_NULL && throw(UndefRefError())
    if typecheck_enabled
        T′ = identify(LLVMType, ref)
        if T != T′
            error("invalid conversion of $T′ type reference to $T")
        end
    end
end

# Construct a concretely typed type object from an abstract type ref
function LLVMType(ref::API.LLVMTypeRef)
    ref == C_NULL && throw(UndefRefError())
    T = identify(LLVMType, ref)
    return unsafe_wrap_ref(T, ref)::LLVMType
end

"""
    issized(typ::LLVMType)

Return true if it makes sense to take the size of this type.

Note that this does not mean that it's possible to call `sizeof` on this type, as LLVM types
sizes can only queried given a target data layout.

See also: [`LLVM.bit_size`](@ref).
"""
issized(typ::LLVMType) = API.LLVMTypeIsSized(typ) |> Bool

context(typ::LLVMType) = Context(API.LLVMGetTypeContext(typ))

@property LLVMType context

Base.string(typ::LLVMType) = unsafe_message(API.LLVMPrintTypeToString(typ))

function Base.show(io::IO, ::MIME"text/plain", typ::LLVMType)
    print(io, strip(string(typ)))
end

function Base.show(io::IO, typ::LLVMType)
    print(io, typeof(typ), "(", strip(string(typ)), ")")
end

@vocabulary IR isemptytype

"""
    isemptytype(typ::LLVMType)

Check whether the given type is empty, i.e., has no elements, or only empty elements, like
C++'s `Type::isEmptyTy`. For example, `{}`, `[0 x i32]` and `[4 x {}]` are empty types.
"""
isemptytype(@nospecialize(T::LLVMType)) = false


## integer

"""
    LLVM.IntegerType <: LLVMType

Type representing arbitrary bit width integers.

# Properties

    inttyp.width

The bit width of the integer type.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct IntegerType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR IntegerType, IntType, Int1Type, Int8Type, Int16Type, Int32Type, Int64Type,
               Int128Type
register(IntegerType, API.LLVMIntegerTypeKind)

"""
    LLVM.IntType(bits::Integer)

Create an integer type with the given `bits` width.

Short-hand constructors are available for common widths: `LLVM.Int1Type`, `LLVM.Int8Type`,
`LLVM.Int16Type`, `LLVM.Int32Type`, `LLVM.Int64Type`, and `LLVM.Int128Type`.
"""
IntType(bits::Integer) = IntegerType(API.LLVMIntTypeInContext(context(), bits))

for T in [:Int1, :Int8, :Int16, :Int32, :Int64, :Int128]
    jl_fname = Symbol(T, :Type)
    api_fname = Symbol(:LLVM, jl_fname)
    @eval begin
        $jl_fname() = IntegerType(API.$(Symbol(api_fname, :InContext))(context()))
        @doc (@doc LLVM.IntType) $jl_fname
    end
end

width(inttyp::IntegerType) = Int(API.LLVMGetIntTypeWidth(inttyp))

@property IntegerType width

if version() >= v"23"
    @checked struct ByteType <: LLVMType
        ref::API.LLVMTypeRef
    end
    @doc """
        LLVM.ByteType(bits::Integer)

    An arbitrary-width byte type, which holds uninterpreted bits. Requires LLVM 23+.

    Its `width` property gives the bit width.
    """ ByteType
    @vocabulary IR ByteType
    register(ByteType, API.LLVMByteTypeKind)
    ByteType(bits::Integer) = ByteType(API.LLVMByteTypeInContext(context(), bits))
    width(typ::ByteType) = Int(API.LLVMGetByteTypeWidth(typ))
    @property ByteType width
end


## floating-point

# NOTE: this type doesn't exist in the LLVM API,
#       we add it for convenience of typechecking generic values (see execution.jl)
"""
    LLVM.FloatingPointType <: LLVMType

Abstract supertype of the floating-point types, like [`LLVM.FloatType`](@ref) and
[`LLVM.DoubleType`](@ref).
"""
abstract type FloatingPointType <: LLVMType end

@vocabulary IR FloatingPointType, HalfType, FloatType, DoubleType, BFloatType, FP128Type,
               X86FP80Type, PPCFP128Type

# each floating-point type has its own type kind, so unlike integer types, which share
# `IntegerType`, they are represented by distinct Julia types
for (T, description) in [
        (:Half, "16-bit floating-point type"),
        (:BFloat, "16-bit “brain” floating-point type"),
        (:Float, "32-bit floating-point type"),
        (:Double, "64-bit floating-point type"),
        (:FP128, "128-bit floating-point type, with a 113-bit significand"),
        (:X86_FP80, "80-bit, X87 floating-point type"),
        (:PPC_FP128, "128-bit floating-point type, consisting of two 64-bit values")]
    CleanT = Symbol(replace(String(T), "_"=>""))    # only the type kind retains the underscore
    typename = Symbol(CleanT, :Type)
    api_fname = Symbol(:LLVM, typename, :InContext)
    enumkind = Symbol(:LLVM, T, :TypeKind)
    doc = """
        LLVM.$typename <: LLVM.FloatingPointType

    The $description.

        LLVM.$typename()

    Create the $description in the active context.
    """
    @eval begin
        @checked struct $typename <: FloatingPointType
            ref::API.LLVMTypeRef
        end
        register($typename, API.$enumkind)

        $typename() = $typename(API.$api_fname(context()))

        @doc $doc $typename
    end
end


## function types

@vocabulary IR isvararg

"""
    LLVM.FunctionType <: LLVMType

A function type, representing a function signature.

# Properties

    ft.return_type

The return type of the function type.

    ft.parameters

The parameter types of the function type, as a read-only view. Types are uniqued and cannot
be changed, so create a new function type instead.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct FunctionType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR FunctionType
register(FunctionType, API.LLVMFunctionTypeKind)

"""
    LLVM.FunctionType(rettyp::LLVMType, params::LLVMType[]; vararg=false)

Create a function type with the given `rettyp` return type and `params` parameter types.
The `vararg` argument indicates whether the function is variadic.

See also: [`isvararg`](@ref), and the [`return_type`](@ref LLVM.FunctionType) and
[`parameters`](@ref LLVM.FunctionType) properties.
"""
FunctionType(rettyp::LLVMType, params::AbstractVector{<:LLVMType}=LLVMType[];
             vararg::Bool=false) =
    FunctionType(API.LLVMFunctionType(rettyp, as_vector(params),
                                      length(params), vararg))

"""
    isvararg(ft::LLVM.FunctionType)

Check whether the given function type is variadic.
"""
isvararg(ft::FunctionType) = API.LLVMIsFunctionVarArg(ft) |> Bool

return_type(ft::FunctionType) = LLVMType(API.LLVMGetReturnType(ft))

@property FunctionType return_type

struct FunctionTypeParameterSet <: AbstractVector{LLVMType}
    typ::FunctionType
end

parameters(ft::FunctionType) = FunctionTypeParameterSet(ft)

@property FunctionType parameters

Base.size(iter::FunctionTypeParameterSet) = (Int(API.LLVMCountParamTypes(iter.typ)),)

Base.IndexStyle(::Type{FunctionTypeParameterSet}) = IndexLinear()

# LLVM only supports fetching all parameter types at once. since types are immutable,
# fetching them once when iterating does not change the semantics of the view.
function param_type_refs(ft::FunctionType)
    refs = Vector{API.LLVMTypeRef}(undef, API.LLVMCountParamTypes(ft))
    isempty(refs) || API.LLVMGetParamTypes(ft, refs)
    return refs
end

function Base.getindex(iter::FunctionTypeParameterSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return LLVMType(param_type_refs(iter.typ)[i])
end

function Base.iterate(iter::FunctionTypeParameterSet,
                      (refs, i)=(param_type_refs(iter.typ), 1))
    i > length(refs) ? nothing : (LLVMType(refs[i]), (refs, i+1))
end

# NOTE: optimized `collect`
Base.collect(iter::FunctionTypeParameterSet) =
    LLVMType[LLVMType(ref) for ref in param_type_refs(iter.typ)]


## pointer types

"""
    LLVM.PointerType <: LLVMType

A pointer type.

# Properties

    ptrtyp.addrspace

The address space of the pointer type.

    ptrtyp.element_type

The type that a typed pointer points to, or `nothing` for an opaque pointer. Only available
before LLVM 17, which removed typed pointers.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct PointerType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR PointerType
register(PointerType, API.LLVMPointerTypeKind)

"""
    LLVM.PointerType(eltyp::LLVMType, addrspace=0)

Create a typed pointer type with the given `eltyp` and `addrspace`. This is only supported
when the context still supports typed pointers.

See also: the [`addrspace`](@ref LLVM.PointerType) property,
[`supports_typed_pointers`](@ref).
"""
function PointerType(eltyp::LLVMType, addrspace=0)
    return PointerType(API.LLVMPointerType(eltyp, addrspace))
end

"""
    LLVM.PointerType(addrspace=0)

Create an opaque pointer type in the given `addrspace`.

See also: the [`addrspace`](@ref LLVM.PointerType) property, [`isopaque`](@ref).
"""
function PointerType(addrspace=0)
    return PointerType(API.LLVMPointerTypeInContext(context(), addrspace))
end

isopaque(ptrtyp::PointerType) = API.LLVMPointerTypeIsOpaque(ptrtyp) |> Bool

@static if version() < v"17"
    # typed pointers
    function element_type(typ::PointerType)
        isopaque(typ) && return nothing
        LLVMType(API.LLVMGetElementType(typ))
    end
    @property PointerType element_type
end

"""
    isopaque(ptrtyp::LLVM.PointerType)

Check whether the given pointer type is opaque.
"""
isopaque(::PointerType)

addrspace(ptrtyp::PointerType) = Int(API.LLVMGetPointerAddressSpace(ptrtyp))

@property PointerType addrspace


## array types

"""
    LLVM.ArrayType <: LLVMType

An array type, representing a fixed-size array of identically-typed elements.

# Properties

    arrtyp.element_type

The type of the elements of the array type.

    arrtyp.length

The number of elements of the array type.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct ArrayType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR ArrayType
register(ArrayType, API.LLVMArrayTypeKind)

"""
    LLVM.ArrayType(eltyp::LLVMType, count)

Create an array type with `count` elements of type `eltyp`.
"""
function ArrayType(eltyp::LLVMType, count)
    @static if version() >= v"17"
        return ArrayType(API.LLVMArrayType2(eltyp, count))
    else
        return ArrayType(API.LLVMArrayType(eltyp, count))
    end
end

function array_length(arrtyp::ArrayType)
    @static if version() >= v"17"
        Int(API.LLVMGetArrayLength2(arrtyp))
    else
        Int(API.LLVMGetArrayLength(arrtyp))
    end
end

@property ArrayType length => array_length

isemptytype(@nospecialize(T::ArrayType)) =
    array_length(T) == 0 || isemptytype(element_type(T))


## vector types

"""
    LLVM.VectorType <: LLVMType

A vector type, representing a fixed-size vector of identically-typed elements. Typically
used for SIMD operations.

# Properties

    vectyp.element_type

The type of the elements of the vector type.

    vectyp.length

The number of elements of the vector type.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct VectorType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR VectorType
register(VectorType, API.LLVMVectorTypeKind)

"""
    VectorType(eltyp::LLVMType, count)

Create a vector type with `count` elements of type `eltyp`.
"""
function VectorType(eltyp::LLVMType, count)
    return VectorType(API.LLVMVectorType(eltyp, count))
end

vector_length(vectyp::VectorType) = Int(API.LLVMGetVectorSize(vectyp))

@property VectorType length => vector_length

element_type(typ::Union{ArrayType,VectorType}) = LLVMType(API.LLVMGetElementType(typ))

@property Union{ArrayType,VectorType} element_type


## structure types

@vocabulary IR ispacked, isopaque, elements!

"""
    LLVM.StructType <: LLVMType

A structure type, representing a collection of named fields of potentially different types.

# Properties

    structtyp.name

The name of the structure type, or `nothing` if it is a literal (unnamed) structure.

    structtyp.elements

The element types of the structure type, as a read-only view. Use
[`elements!`](@ref) to set the body of an opaque structure type.

The properties of [`LLVMType`](@ref LLVM.LLVMType) are available too.
"""
@checked struct StructType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR StructType
register(StructType, API.LLVMStructTypeKind)

"""
    LLVM.StructType(name::AbstractString)

Create an opaque structure type with the given `name`. The structure can be later defined
with [`elements!`](@ref).

See also the [`name`](@ref LLVM.StructType) property.
"""
function StructType(name::AbstractString)
    return StructType(API.LLVMStructCreateNamed(context(), name))
end

"""
    LLVM.StructType(elements::LLVMType[]; packed=false)

Create a structure type with the given `elements`. The `packed` argument indicates whether
the structure should be packed, i.e., without padding between fields.

See also: [`ispacked`](@ref), and the [`elements`](@ref LLVM.StructType) property.
"""
StructType(elems::AbstractVector{<:LLVMType}; packed::Bool=false) =
    StructType(API.LLVMStructTypeInContext(context(), as_vector(elems), length(elems),
                                           packed))

function name(structtyp::StructType)
    cstr = API.LLVMGetStructName(structtyp)
    cstr == C_NULL ? nothing : unsafe_string(cstr)
end

@property StructType name

"""
    ispacked(structtyp::LLVM.StructType)

Check whether the given structure type is packed.
"""
ispacked(structtyp::StructType) = API.LLVMIsPackedStruct(structtyp) |> Bool

"""
    isopaque(structtyp::LLVM.StructType)

Check whether the given structure type is opaque.
"""
isopaque(structtyp::StructType) = API.LLVMIsOpaqueStruct(structtyp) |> Bool

"""
    elements!(structtyp::LLVM.StructType, elems::AbstractVector{<:LLVMType}; packed=false)

Set the body of the given structure type, i.e., its elements `elems` and whether it is
`packed` (without padding between fields). This is typically used to define an opaque,
named structure type.

See also the [`elements`](@ref LLVM.StructType) property.
"""
elements!(structtyp::StructType, elems::AbstractVector{<:LLVMType}; packed::Bool=false) =
    API.LLVMStructSetBody(structtyp, as_vector(elems), length(elems), packed)

isemptytype(@nospecialize(T::StructType)) = all(isemptytype, elements(T))

# element iteration

struct StructTypeElementSet <: AbstractVector{LLVMType}
    typ::StructType
end

elements(typ::StructType) = StructTypeElementSet(typ)

@property StructType elements

Base.size(iter::StructTypeElementSet) = (Int(API.LLVMCountStructElementTypes(iter.typ)),)

Base.IndexStyle(::Type{StructTypeElementSet}) = IndexLinear()

function Base.getindex(iter::StructTypeElementSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return LLVMType(API.LLVMStructGetTypeAtIndex(iter.typ, i-1))
end

# NOTE: optimized `collect`
function Base.collect(iter::StructTypeElementSet)
    elems = Vector{API.LLVMTypeRef}(undef, length(iter))
    isempty(elems) || API.LLVMGetStructElementTypes(iter.typ, elems)
    return LLVMType[LLVMType(elem) for elem in elems]
end


## other

"""
    LLVM.VoidType <: LLVMType

A void type, representing the absence of a value.
"""
@checked struct VoidType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR VoidType
register(VoidType, API.LLVMVoidTypeKind)

"""
    LLVM.VoidType()

Create a void type.
"""
VoidType() = VoidType(API.LLVMVoidTypeInContext(context()))

"""
    LLVM.LabelType <: LLVMType

A label type, representing a code label.
"""
@checked struct LabelType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR LabelType
register(LabelType, API.LLVMLabelTypeKind)

"""
    LLVM.LabelType()

Create a label type.
"""
LabelType() = LabelType(API.LLVMLabelTypeInContext(context()))

"""
    LLVM.MetadataType <: LLVMType

A metadata type, representing a metadata value.
"""
@checked struct MetadataType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR MetadataType
register(MetadataType, API.LLVMMetadataTypeKind)

MetadataType() = MetadataType(API.LLVMMetadataTypeInContext(context()))

"""
    LLVM.TokenType <: LLVMType

A token type, representing a token value.
"""
@checked struct TokenType <: LLVMType
    ref::API.LLVMTypeRef
end
@vocabulary IR TokenType
register(TokenType, API.LLVMTokenTypeKind)

"""
    LLVM.TokenType()

Create a token type.
"""
TokenType() = TokenType(API.LLVMTokenTypeInContext(context()))


## type iteration

# a lookup of the named types of a context, which LLVM can't enumerate (so not a collection)
struct ContextTypeDict
    ctx::Context
end

types(ctx::Context) = ContextTypeDict(ctx)

@property Context types

Base.show(io::IO, iter::ContextTypeDict) = print(io, "ContextTypeDict(", iter.ctx, ")")
Base.show(io::IO, ::MIME"text/plain", iter::ContextTypeDict) = show(io, iter)

function Base.haskey(iter::ContextTypeDict, name::AbstractString)
    API.LLVMGetTypeByName2(iter.ctx, name) != C_NULL
end

function Base.getindex(iter::ContextTypeDict, name::AbstractString)
    objref = API.LLVMGetTypeByName2(iter.ctx, name)
    objref == C_NULL && throw(KeyError(name))
    return LLVMType(objref)
end

function Base.get(iter::ContextTypeDict, name::AbstractString, default)
    objref = API.LLVMGetTypeByName2(iter.ctx, name)
    objref == C_NULL ? default : LLVMType(objref)
end
