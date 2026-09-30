export isboxed, isghosttype

"""
    isboxed(typ::Type)

Return if a type would be boxed when instantiated in the code generator.
"""
function isboxed(typ::Type)
    if context(; throw_error=false) === nothing
        LLVM.Context() do _
            _isboxed(typ)
        end
    else
        _isboxed(typ)
    end
end
function _isboxed(typ::Type)
    isboxed_ref = Ref{Bool}()
    ccall(:jl_type_to_llvm, LLVM.API.LLVMTypeRef,
           (Any, LLVM.API.LLVMContextRef, Ptr{Bool}), typ, context(), isboxed_ref)
    return isboxed_ref[]
end

"""
    convert(LLVMType, typ::Type; allow_boxed=true)

Convert a Julia type `typ` to its LLVM representation in the current context.
The `allow_boxed` argument determines whether boxed types are allowed.
"""
function Base.convert(::Type{LLVMType}, typ::Type; allow_boxed::Bool=false)
    isboxed_ref = Ref{Bool}()
    llvmtyp =
        LLVMType(ccall(:jl_type_to_llvm, LLVM.API.LLVMTypeRef,
                        (Any, Context, Ptr{Bool}), typ, context(), isboxed_ref))
    if !allow_boxed && isboxed_ref[]
        error("Conversion of boxed type $typ is not allowed")
    end

    return llvmtyp
end

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
