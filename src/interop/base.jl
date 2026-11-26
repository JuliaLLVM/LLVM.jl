export isboxed, isghosttype

# Julia's code generator lowers types with Julia's LLVM, into a context of that library
function julia_type_to_llvm(@nospecialize(typ::Type), ctx::Union{Context,API.LLVMContextRef})
    isboxed_ref = Ref{Bool}()
    ref = ccall(:jl_type_to_llvm, API.LLVMTypeRef,
                (Any, API.LLVMContextRef, Ptr{Bool}), typ, ctx, isboxed_ref)
    return ref, isboxed_ref[]
end

# call `f` with a temporary context of Julia's LLVM. it is not an LLVM.jl `Context`, which
# would be kept alive when disposed while an exception is being handled, and it is neither
# activated nor does it change the current library.
function with_julia_context(f; opaque_pointers::Bool=true)
    lib = LLVM.julia_library
    LLVM.check_loaded(lib)
    ref = API.LLVMContextCreate(lib)
    try
        LLVM.atleast(17, lib) || API.LLVMContextSetOpaquePointers(lib, ref, opaque_pointers)
        return f(ref)
    finally
        API.LLVMContextDispose(lib, ref)
    end
end

function julia_type_to_llvm(@nospecialize(typ::Type), ctx::Union{Context,API.LLVMContextRef},
                            allow_boxed::Bool)
    ref, isboxed = julia_type_to_llvm(typ, ctx)
    !allow_boxed && isboxed && error("Conversion of boxed type $typ is not allowed")
    return ref
end

# lower `typ` in the active context: directly in a context of Julia's LLVM, or else in a
# temporary one, rebuilding the result in the active context
function lower_type(@nospecialize(typ::Type), allow_boxed::Bool)
    ctx = context()
    if ctx.library === LLVM.julia_library
        return LLVMType(julia_type_to_llvm(typ, ctx, allow_boxed))
    end
    # typed pointers are only used if Julia's LLVM supports them too (before LLVM 17)
    with_julia_context(; opaque_pointers=!supports_typed_pointers(ctx)) do jlctx
        import_type(LLVM.julia_library, julia_type_to_llvm(typ, jlctx, allow_boxed))
    end
end

# rebuild a type of another library in the active context. this only covers the types that
# Julia's code generator lowers Julia types to, which are literal types.
function import_type(lib::LLVM.Library, ref::API.LLVMTypeRef)
    kind = API.LLVMGetTypeKind(lib, ref)
    if kind == API.LLVMVoidTypeKind
        LLVM.VoidType()
    elseif kind == API.LLVMIntegerTypeKind
        LLVM.IntType(API.LLVMGetIntTypeWidth(lib, ref))
    elseif kind == API.LLVMHalfTypeKind
        LLVM.HalfType()
    elseif kind == API.LLVMBFloatTypeKind
        LLVM.BFloatType()
    elseif kind == API.LLVMFloatTypeKind
        LLVM.FloatType()
    elseif kind == API.LLVMDoubleTypeKind
        LLVM.DoubleType()
    elseif kind == API.LLVMPointerTypeKind
        addrspace = API.LLVMGetPointerAddressSpace(lib, ref)
        if !supports_typed_pointers()
            LLVM.PointerType(addrspace)
        elseif !Bool(API.LLVMPointerTypeIsOpaque(lib, ref))
            LLVM.PointerType(import_type(lib, API.LLVMGetElementType(lib, ref)), addrspace)
        else
            error("Cannot convert a Julia type with pointers to LLVM in a context with typed pointers: Julia's LLVM $(lib.version) only lowers to opaque pointers")
        end
    elseif kind == API.LLVMArrayTypeKind
        n = LLVM.atleast(17, lib) ? API.LLVMGetArrayLength2(lib, ref) :
                                    API.LLVMGetArrayLength(lib, ref)
        LLVM.ArrayType(import_type(lib, API.LLVMGetElementType(lib, ref)), n)
    elseif kind == API.LLVMVectorTypeKind
        LLVM.VectorType(import_type(lib, API.LLVMGetElementType(lib, ref)),
                        API.LLVMGetVectorSize(lib, ref))
    elseif kind == API.LLVMStructTypeKind && Bool(API.LLVMIsLiteralStruct(lib, ref))
        elems = LLVMType[import_type(lib, API.LLVMStructGetTypeAtIndex(lib, ref, i))
                         for i in 0:Int(API.LLVMCountStructElementTypes(lib, ref))-1]
        LLVM.StructType(elems; packed=Bool(API.LLVMIsPackedStruct(lib, ref)))
    else
        str = LLVM.unsafe_message(lib, API.LLVMPrintTypeToString(lib, ref))
        error("Cannot rebuild LLVM type $str of LLVM library :$(lib.name) in another context")
    end
end

"""
    isboxed(typ::Type)

Return if a type would be boxed when instantiated in the code generator.
"""
function isboxed(@nospecialize(typ::Type))
    ctx = context(; throw_error=false)
    if ctx !== nothing && ctx.library === LLVM.julia_library
        last(julia_type_to_llvm(typ, ctx))
    else
        # this only depends on Julia's code generator, which uses Julia's LLVM
        with_julia_context(ctx -> last(julia_type_to_llvm(typ, ctx)))
    end
end

"""
    convert(LLVMType, typ::Type; allow_boxed=false)

Convert a Julia type `typ` to the LLVM type that Julia's code generator uses for its
values, in the active context. Boxed types, whose values are pointers to objects, are an
error unless `allow_boxed` is set.

The active context can belong to any LLVM library. Julia's code generator lowers the type
with Julia's LLVM, so for a context of another library the result is rebuilt there. In a
context with typed pointers, this requires Julia's LLVM to support typed pointers as well
(LLVM 16 or older) for types that contain pointers.
"""
Base.convert(::Type{LLVMType}, @nospecialize(typ::Type); allow_boxed::Bool=false) =
    lower_type(typ, allow_boxed)

"""
    isghosttype(t::Type)
    isghosttype(T::LLVMType)

Check if a type is a ghost type, implying it would not be emitted by the Julia compiler,
e.g., because its values don't contain any data (like `Nothing`). For a Julia type, this
is cheap and can be constant-folded. For an LLVM type, as converted from a Julia type,
this checks whether it is the void type of the current context, or an empty type.
"""
isghosttype

isghosttype(@nospecialize(T::LLVMType)) = T == LLVM.VoidType() || isemptytype(T)
function isghosttype(@nospecialize(t::Type))
    # mirrors `_julia_type_to_llvm` in Julia's src/cgutils.cpp, which emits `Union{}` and
    # its aliases, and concrete immutable types without any data, as the void type
    t === Union{} && return true
    @static if VERSION >= v"1.12.0-DEV.1072"
        # JuliaLang/julia#55508 made `Type{Union{}}` an alias of `typeof(Union{})`
        t === Type{Union{}} && return true
    end
    return Base.issingletontype(t)
end
