export LLVMType, issized, context, show

import Base: show

@compat abstract type LLVMType end
reftype{T<:LLVMType}(::Type{T}) = API.LLVMTypeRef
kindtype{T<:LLVMType}(::Type{T}) = API.LLVMTypeKind

identify(::Type{LLVMType}, ref::API.LLVMTypeRef) =
    identify(LLVMType, Val{API.LLVMGetTypeKind(ref)}())
identify{T}(::Type{LLVMType}, ::Val{T}) = error("Unknown type $T")

@inline function check{T<:LLVMType}(::Type{T}, ref::API.LLVMTypeRef)
    ref==C_NULL && throw(NullException())
    @static if DEBUG
        T′ = identify(LLVMType, ref)
        if T != T′
            error("invalid conversion of $T′ type reference to $T")
        end
    end
end

# Pseudo-constructor, creating a concretely typed object from an abstract type ref
function LLVMType(ref::API.LLVMTypeRef)
    ref == C_NULL && throw(NullException())
    T = identify(LLVMType, ref)
    return T(ref)
end

issized(typ::LLVMType) =
    convert(Core.Bool, API.LLVMTypeIsSized(ref(typ)))
context(typ::LLVMType) = Context(API.LLVMGetTypeContext(ref(typ)))

function show(io::IO, typ::LLVMType)
    output = unsafe_string(API.LLVMPrintTypeToString(ref(typ)))
    print(io, output)
end


## integer

export width

@checked immutable IntegerType <: LLVMType
    ref::reftype(LLVMType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMIntegerTypeKind}) = IntegerType

for T in [:Int1, :Int8, :Int16, :Int32, :Int64, :Int128]
    jl_fname = Symbol(T, :Type)
    api_fname = Symbol(:LLVM, jl_fname)
    @eval begin
        $jl_fname() = IntegerType(API.$api_fname())
        $jl_fname(ctx::Context) =
            IntegerType(
                      API.$(Symbol(api_fname, :InContext))(ref(ctx)))
    end
end

IntType(bits::Integer) = IntegerType(API.LLVMIntType(Cuint(bits)))
IntType(bits::Integer, ctx::Context) =
    IntegerType(API.LLVMIntTypeInContext(ref(ctx), Cuint(bits)))

width(inttyp::IntegerType) = API.LLVMGetIntTypeWidth(ref(inttyp))


## floating-point

# NOTE: this type doesn't exist in the LLVM API,
#       we add it for convenience of typechecking generic values (see execution.jl)
 @compat abstract type FloatingPointType <: LLVMType end

# NOTE: we don't handle the obscure types here (:X86FP80, :FP128, :PPCFP128),
#       they would also need special casing as LLVMPPCFP128Type != LLVMPPC_FP128TypeKind
for T in [:Half, :Float, :Double]
    jl_fname = Symbol(T, :Type)
    api_typename = Symbol(:LLVM, T)
    api_fname = Symbol(:LLVM, jl_fname)
    enumkind = Symbol(:LLVM, T, :TypeKind)
    @eval begin
        @checked immutable $api_typename <: FloatingPointType
            ref::reftype(FloatingPointType)
        end
        identify(::Type{LLVMType}, ::Val{API.$enumkind}) = $api_typename

        $jl_fname() = $api_typename(API.$api_fname())
        $jl_fname(ctx::Context) =
            $api_typename(API.$(Symbol(api_fname, :InContext))(ref(ctx)))
    end
end


## function types

export isvararg, return_type, parameters

@checked immutable FunctionType <: LLVMType
    ref::reftype(LLVMType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMFunctionTypeKind}) = FunctionType

FunctionType{T<:LLVMType}(rettyp::LLVMType, params::Vector{T}=LLVMType[], vararg::Core.Bool=false) =
    FunctionType(API.LLVMFunctionType(ref(rettyp), ref.(params),
                                      Cuint(length(params)), convert(Bool, vararg)))

isvararg(ft::FunctionType) =
    convert(Core.Bool, API.LLVMIsFunctionVarArg(ref(ft)))

return_type(ft::FunctionType) =
    LLVMType(API.LLVMGetReturnType(ref(ft)))

function parameters(ft::FunctionType)
    nparams = API.LLVMCountParamTypes(ref(ft))
    params = Vector{API.LLVMTypeRef}(nparams)
    API.LLVMGetParamTypes(ref(ft), params)
    return LLVMType.(params)
end



## composite types

@compat abstract type CompositeType <: LLVMType end


## sequential types

export addrspace

@compat abstract type SequentialType <: CompositeType end

import Base: length, size, eltype

eltype(typ::SequentialType) =
    LLVMType(API.LLVMGetElementType(ref(typ)))

@checked immutable PointerType <: SequentialType
    ref::reftype(SequentialType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMPointerTypeKind}) = PointerType

function PointerType(eltyp::LLVMType, addrspace=0)
    return PointerType(API.LLVMPointerType(ref(eltyp),
                                           Cuint(addrspace)))
end

addrspace(ptrtyp::PointerType) =
    API.LLVMGetPointerAddressSpace(ref(ptrtyp))

@checked immutable ArrayType <: SequentialType
    ref::reftype(SequentialType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMArrayTypeKind}) = ArrayType

function ArrayType(eltyp::LLVMType, count)
    return ArrayType(API.LLVMArrayType(ref(eltyp), Cuint(count)))
end

length(arrtyp::ArrayType) = API.LLVMGetArrayLength(ref(arrtyp))

@checked immutable VectorType <: SequentialType
    ref::reftype(SequentialType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMVectorTypeKind}) = VectorType

function VectorType(eltyp::LLVMType, count)
    return VectorType(API.LLVMVectorType(ref(eltyp), Cuint(count)))
end

size(vectyp::VectorType) = API.LLVMGetVectorSize(ref(vectyp))


## structure types

export name, ispacked, isopaque, elements!

@checked immutable StructType <: SequentialType
    ref::reftype(SequentialType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMStructTypeKind}) = StructType

function StructType(name::String, ctx::Context=GlobalContext())
    return StructType(API.LLVMStructCreateNamed(ref(ctx), name))
end

StructType{T<:LLVMType}(elems::Vector{T}, packed::Core.Bool=false) =
    StructType(API.LLVMStructType(ref.(elems), Cuint(length(elems)),
                                  convert(Bool, packed)))

StructType{T<:LLVMType}(elems::Vector{T}, ctx::Context, packed::Core.Bool=false) =
    StructType(API.LLVMStructTypeInContext(ref(ctx), ref.(elems),
                                           Cuint(length(elems)), convert(Bool, packed)))

name(structtyp::StructType) =
    unsafe_string(API.LLVMGetStructName(ref(structtyp)))
ispacked(structtyp::StructType) =
    convert(Core.Bool, API.LLVMIsPackedStruct(ref(structtyp)))
isopaque(structtyp::StructType) =
    convert(Core.Bool, API.LLVMIsOpaqueStruct(ref(structtyp)))

elements!{T<:LLVMType}(structtyp::StructType, elems::Vector{T}, packed::Core.Bool=false) =
    API.LLVMStructSetBody(ref(structtyp), ref.(elems),
                          Cuint(length(elems)), convert(Bool, packed))

# element iteration

export elements

import Base: eltype, getindex, setindex!, start, next, done, length, endof, collect

immutable StructTypeElementSet
    typ::StructType
end

elements(typ::StructType) = StructTypeElementSet(typ)

eltype(::StructTypeElementSet) = LLVMType

getindex(iter::StructTypeElementSet, i) =
    LLVMType(API.LLVMStructGetTypeAtIndex(ref(iter.typ), Cuint(i-1)))

start(iter::StructTypeElementSet) = (1,length(iter))

next(iter::StructTypeElementSet, state) =
    (iter[state[1]], (state[1]+1,state[2]))

done(::StructTypeElementSet, state) = (state[1] > state[2])

length(iter::StructTypeElementSet) = API.LLVMCountStructElementTypes(ref(iter.typ))
endof(iter::StructTypeElementSet) = length(iter)

# NOTE: optimized `collect`
function collect(iter::StructTypeElementSet)
    elems = Vector{API.LLVMTypeRef}(length(iter))
    API.LLVMGetStructElementTypes(ref(iter.typ), elems)
    return LLVMType.(elems)
end


## other

@checked immutable VoidType <: LLVMType
    ref::reftype(LLVMType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMVoidTypeKind}) = VoidType

VoidType() = VoidType(API.LLVMVoidType())
VoidType(ctx::Context) =
    VoidType(API.LLVMVoidTypeInContext(ref(ctx)))

@checked immutable LabelType <: LLVMType
    ref::reftype(LLVMType)
end
identify(::Type{LLVMType}, ::Val{API.LLVMLabelTypeKind}) = LabelType

LabelType() = LabelType(API.LLVMLabelType())
LabelType(ctx::Context) =
    LabelType(API.LLVMLabelTypeInContext(ref(ctx)))
