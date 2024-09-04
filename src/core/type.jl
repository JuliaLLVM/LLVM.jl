export LLVMType, issized, context

"""
    LLVMType

Abstract supertype for all LLVM types.
"""
abstract type LLVMType end

# subtypes are expected to have a 'ref::API.LLVMTypeRef' field
Base.unsafe_convert(::Type{API.LLVMTypeRef}, typ::LLVMType) = typ.ref

"""
    eltype(typ::LLVMType)

Get the element type of the given type, if supported.
"""
Base.eltype(typ::LLVMType) = LLVMType(API.LLVMGetElementType(typ))

Base.sizeof(typ::LLVMType) = error("LLVM types are not sized")
# TODO: expose LLVMSizeOf/LLVMAlignOf, yielding run-time values?
# XXX: can we query type sizes from the data layout or target?

const type_kinds = Vector{Type}(fill(Nothing, typemax(API.LLVMTypeKind)+1))
function identify(::Type{LLVMType}, ref::API.LLVMTypeRef)
    kind = API.LLVMGetTypeKind(ref)
    typ = @inbounds type_kinds[kind+1]
    typ === Nothing && error("Unknown type kind $kind")
    return typ
end
function register(T::Type{<:LLVMType}, kind::API.LLVMTypeKind)
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
    return T(ref)::LLVMType
end

"""
    issized(typ::LLVMType)

Return true if it makes sense to take the size of this type.

Note that this does not mean that it's possible to call `sizeof` on this type, as LLVM types
sizes can only queried given a target data layout.

See also: [`sizeof(::DataLayout, ::LLVMType)`](@ref).
"""
issized(typ::LLVMType) = API.LLVMTypeIsSized(typ) |> Bool

"""
    context(typ::LLVMType)

Returns the context in which the given type was created.
"""
context(typ::LLVMType) = Context(API.LLVMGetTypeContext(typ))

Base.string(typ::LLVMType) = unsafe_message(API.LLVMPrintTypeToString(typ))

function Base.show(io::IO, ::MIME"text/plain", typ::LLVMType)
    print(io, strip(string(typ)))
end

function Base.show(io::IO, typ::LLVMType)
    print(io, typeof(typ), "(", strip(string(typ)), ")")
end

Base.isempty(@nospecialize(T::LLVMType)) = false


## integer

export width

"""
    LLVM.IntegerType <: LLVMType

Type representing arbitrary bit width integers.
"""
@checked struct IntegerType <: LLVMType
    ref::API.LLVMTypeRef
end
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

"""
    width(inttyp::LLVM.IntegerType)

Get the bit width of the given integer type.
"""
width(inttyp::IntegerType) = Int(API.LLVMGetIntTypeWidth(inttyp))


## floating-point

# NOTE: this type doesn't exist in the LLVM API,
#       we add it for convenience of typechecking generic values (see execution.jl)
abstract type FloatingPointType <: LLVMType end

for T in [:Half, :Float, :Double, :BFloat, :FP128, :X86_FP80, :PPC_FP128]
    CleanT = Symbol(replace(String(T), "_"=>""))    # only the type kind retains the underscore
    jl_fname = Symbol(CleanT, :Type)
    api_typename = Symbol(:LLVM, CleanT)
    api_fname = Symbol(:LLVM, jl_fname)
    enumkind = Symbol(:LLVM, T, :TypeKind)
    @eval begin
        @checked struct $api_typename <: FloatingPointType
            ref::API.LLVMTypeRef
        end
        register($api_typename, API.$enumkind)

        $jl_fname() =
            $api_typename(API.$(Symbol(api_fname, :InContext))(context()))
    end
end

"""
    LLVM.HalfType()

Create a 16-bit floating-point type.
"""
HalfType

"""
    LLVM.BFloatType()

Create a 16-bit “brain” floating-point type.
"""
BFloatType

"""
    LLVM.FloatType()

Create a 32-bit floating-point type.
"""
FloatType

"""
    LLVM.DoubleType()

Create a 64-bit floating-point type.
"""
DoubleType

"""
    LLVM.FP128Type()

Create a 128-bit floating-point type, with a 113-bit significand.
"""
FP128Type

"""
    LLVM.X86FP80Type()

Create a 80-bit, X87 floating-point type.
"""
X86FP80Type

"""
    LLVM.PPCFP128Type()

Create a 128-bit floating-point type, consisting of two 64-bits.
"""
PPCFP128Type


## function types

export isvararg, return_type, parameters

"""
    LLVM.FunctionType <: LLVMType

A function type, representing a function signature.
"""
@checked struct FunctionType <: LLVMType
    ref::API.LLVMTypeRef
end
register(FunctionType, API.LLVMFunctionTypeKind)

"""
    LLVM.FunctionType(rettyp::LLVMType, params::LLVMType[]; vararg=false)

Create a function type with the given `rettyp` return type and `params` parameter types.
The `vararg` argument indicates whether the function is variadic.

See also: [`isvararg`](@ref), [`return_type`](@ref), [`parameters`](@ref).
"""
FunctionType(rettyp::LLVMType, params::Vector{<:LLVMType}=LLVMType[];
             vararg::Bool=false) =
    FunctionType(API.LLVMFunctionType(rettyp, params,
                                      length(params), vararg))

"""
    isvararg(ft::LLVM.FunctionType)

Check whether the given function type is variadic.
"""
isvararg(ft::FunctionType) = API.LLVMIsFunctionVarArg(ft) |> Bool

"""
    return_type(ft::LLVM.FunctionType)

Get the return type of the given function type.
"""
return_type(ft::FunctionType) = LLVMType(API.LLVMGetReturnType(ft))

"""
    parameters(ft::LLVM.FunctionType)

Get the parameter types of the given function type.
"""
function parameters(ft::FunctionType)
    nparams = API.LLVMCountParamTypes(ft)
    params = Vector{API.LLVMTypeRef}(undef, nparams)
    API.LLVMGetParamTypes(ft, params)
    return LLVMType[LLVMType(param) for param in params]
end


## pointer types

export addrspace, is_opaque

"""
    LLVM.PointerType <: LLVMType

A pointer type.
"""
@checked struct PointerType <: LLVMType
    ref::API.LLVMTypeRef
end
register(PointerType, API.LLVMPointerTypeKind)

"""
    LLVM.PointerType(eltyp::LLVMType, addrspace=0)

Create a typed pointer type with the given `eltyp` and `addrspace`. This is only supported
when the context still supports typed pointers.

See also: [`addrspace`](@ref), [`supports_typed_pointers`](@ref).
"""
function PointerType(eltyp::LLVMType, addrspace=0)
    return PointerType(API.LLVMPointerType(eltyp, addrspace))
end

"""
    LLVM.PointerType(addrspace=0)

Create an opaque pointer type in the given `addrspace`.

See also: [`addrspace`](@ref), [`is_opaque`](@ref).
"""
function PointerType(addrspace=0)
    return PointerType(API.LLVMPointerTypeInContext(context(), addrspace))
end

if version() >= v"13"
    is_opaque(ptrtyp::PointerType) = API.LLVMPointerTypeIsOpaque(ptrtyp) |> Bool

    function Base.eltype(typ::PointerType)
        is_opaque(typ) && throw(error("Taking the type of an opaque pointer is illegal"))
        invoke(eltype, Tuple{LLVMType}, typ)
    end
else
    is_opaque(ptrtyp::PointerType) = false
end

"""
    is_opaque(ptrtyp::LLVM.PointerType)

Check whether the given pointer type is opaque.
"""
is_opaque

"""
    addrspace(ptrtyp::LLVM.PointerType)

Get the address space of the given pointer type.
"""
addrspace(ptrtyp::PointerType) = Int(API.LLVMGetPointerAddressSpace(ptrtyp))


## array types

"""
    LLVM.ArrayType <: LLVMType

An array type, representing a fixed-size array of identically-typed elements.
"""
@checked struct ArrayType <: LLVMType
    ref::API.LLVMTypeRef
end
register(ArrayType, API.LLVMArrayTypeKind)

"""
    LLVM.ArrayType(eltyp::LLVMType, count)

Create an array type with `count` elements of type `eltyp`.

See also: [`length`](@ref), [`isempty`](@ref).
"""
function ArrayType(eltyp::LLVMType, count)
    return ArrayType(API.LLVMArrayType(eltyp, count))
end

"""
    length(arrtyp::LLVM.ArrayType)

Get the length of the given array type.
"""
Base.length(arrtyp::ArrayType) = Int(API.LLVMGetArrayLength(arrtyp))

"""
    isempty(arrtyp::LLVM.ArrayType)

Check whether the given array type is empty.
"""
Base.isempty(@nospecialize(T::ArrayType)) = length(T) == 0 || isempty(eltype(T))


## vector types

"""
    LLVM.VectorType <: LLVMType

A vector type, representing a fixed-size vector of identically-typed elements. Typically
used for SIMD operations.
"""
@checked struct VectorType <: LLVMType
    ref::API.LLVMTypeRef
end
register(VectorType, API.LLVMVectorTypeKind)

"""
    VectorType(eltyp::LLVMType, count)

Create a vector type with `count` elements of type `eltyp`.

See also: [`length`](@ref).
"""
function VectorType(eltyp::LLVMType, count)
    return VectorType(API.LLVMVectorType(eltyp, count))
end

"""
    length(vectyp::LLVM.VectorType)

Get the length of the given vector type.
"""
Base.length(vectyp::VectorType) = Int(API.LLVMGetVectorSize(vectyp))


## structure types

export name, ispacked, isopaque, elements!

"""
    LLVM.StructType <: LLVMType

A structure type, representing a collection of named fields of potentially different types.
"""
@checked struct StructType <: LLVMType
    ref::API.LLVMTypeRef
end
register(StructType, API.LLVMStructTypeKind)

"""
    LLVM.StructType(name::String)

Create an opaque structure type with the given `name`. The structure can be later defined
with [`elements!`](@ref).

See also: [`name`](@ref).
"""
function StructType(name::String)
    return StructType(API.LLVMStructCreateNamed(context(), name))
end

"""
    LLVM.StructType(elements::LLVMType[]; packed=false)

Create a structure type with the given `elements`. The `packed` argument indicates whether
the structure should be packed, i.e., without padding between fields.

See also: [`ispacked`](@ref), [`elements`](@ref).
"""
StructType(elems::Vector{<:LLVMType}; packed::Bool=false) =
    StructType(API.LLVMStructTypeInContext(context(), elems, length(elems), packed))

"""
    name(structtyp::StructType)

Get the name of the given structure type.
"""
function name(structtyp::StructType)
    cstr = API.LLVMGetStructName(structtyp)
    cstr == C_NULL ? nothing : unsafe_string(cstr)
end

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
    elements!(structtyp::LLVM.StructType, elems::LLVMType[]; packed=false)

Set the elements of the given structure type to `elems`. The `packed` argument
indicates whether the structure should be packed, i.e., without padding between fields.

See also: [`elements`](@ref).
"""
elements!(structtyp::StructType, elems::Vector{<:LLVMType}, packed::Bool=false) =
    API.LLVMStructSetBody(structtyp, elems, length(elems), packed)

Base.isempty(@nospecialize(T::StructType)) =
    isempty(elements(T)) || all(isempty, elements(T))

# element iteration

export elements

struct StructTypeElementSet
    typ::StructType
end

"""
    elements(structtyp::LLVM.StructType)

Get the elements of the given structure type.

See also: [`elements!`](@ref).
"""
elements(typ::StructType) = StructTypeElementSet(typ)

Base.eltype(::StructTypeElementSet) = LLVMType

function Base.getindex(iter::StructTypeElementSet, i)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    return LLVMType(API.LLVMStructGetTypeAtIndex(iter.typ, i-1))
end

function Base.iterate(iter::StructTypeElementSet, i=1)
    i >= length(iter) + 1 ? nothing : (iter[i], i+1)
end

Base.length(iter::StructTypeElementSet) = API.LLVMCountStructElementTypes(iter.typ)

Base.lastindex(iter::StructTypeElementSet) = length(iter)

# NOTE: optimized `collect`
function Base.collect(iter::StructTypeElementSet)
    elems = Vector{API.LLVMTypeRef}(undef, length(iter))
    API.LLVMGetStructElementTypes(iter.typ, elems)
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
register(MetadataType, API.LLVMMetadataTypeKind)

MetadataType() = MetadataType(API.LLVMMetadataTypeInContext(context()))

"""
    LLVM.TokenType <: LLVMType

A token type, representing a token value.
"""
@checked struct TokenType <: LLVMType
    ref::API.LLVMTypeRef
end
register(TokenType, API.LLVMTokenTypeKind)

"""
    LLVM.TokenType()

Create a token type.
"""
TokenType() = TokenType(API.LLVMTokenTypeInContext(context()))


## type iteration

export types

struct ContextTypeDict <: AbstractDict{String,LLVMType}
    ctx::Context
end

"""
    types(ctx::LLVM.Context)

Get a dictionary of all types in the given context.
"""
types(ctx::Context) = ContextTypeDict(ctx)

function Base.haskey(iter::ContextTypeDict, name::String)
    @static if version() >= v"12"
        API.LLVMGetTypeByName2(iter.ctx, name) != C_NULL
    else
        context!(iter.ctx) do
            @dispose mod=Module("dummy") begin
                API.LLVMGetTypeByName(mod, name) != C_NULL
            end
        end
    end
end

function Base.getindex(iter::ContextTypeDict, name::String)
    objref = @static if version() >= v"12"
        API.LLVMGetTypeByName2(iter.ctx, name)
    else
        context!(iter.ctx) do
            @dispose mod=Module("dummy") begin
                API.LLVMGetTypeByName(mod, name)
            end
        end
    end
    objref == C_NULL && throw(KeyError(name))
    return LLVMType(objref)
end
