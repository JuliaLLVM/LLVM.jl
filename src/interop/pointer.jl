# pointer intrinsics

export @typed_ccall

# TODO: can we use constant propagation instead of passing the alignment as a Val?

using Core: LLVMPtr

@inline function pointerref(ptr::LLVMPtr{T}, i::Int, ::Val{align}) where {T,align}
    sizeof(T) == 0 && return T.instance
    ispow2(align) || error("pointerref: alignment must be a power of 2, got ", align)
    return _pointerref(ptr, i - 1, Val(align))
end

@llvmgenerated builder function _pointerref(ptr::LLVMPtr{T,A}, i::Int,
                                            ::Val{align})::T where {T,A,align}
    eltyp = convert(LLVMType, T)
    if supports_typed_pointers(LLVM.context())
        ptr = bitcast!(builder, ptr, LLVM.PointerType(eltyp, A))
    end
    ld = load!(builder, eltyp, inbounds_gep!(builder, eltyp, ptr, [i]))
    if A != 0
        ld.metadata[LLVM.MD_tbaa] = tbaa_addrspace(A)
    end
    ld.alignment = align
    ld
end

@inline function pointerset(ptr::LLVMPtr{T}, x::T, i::Int, ::Val{align}) where {T,align}
    sizeof(T) == 0 && return
    ispow2(align) || error("pointerset: alignment must be a power of 2, got ", align)
    _pointerset(ptr, x, i - 1, Val(align))
    return
end

@llvmgenerated builder function _pointerset(ptr::LLVMPtr{T,A}, x::T, i::Int,
                                            ::Val{align})::Nothing where {T,A,align}
    eltyp = convert(LLVMType, T)
    if supports_typed_pointers(LLVM.context())
        ptr = bitcast!(builder, ptr, LLVM.PointerType(eltyp, A))
    end
    st = store!(builder, x, inbounds_gep!(builder, eltyp, ptr, [i]))
    if A != 0
        st.metadata[LLVM.MD_tbaa] = tbaa_addrspace(A)
    end
    st.alignment = align
    nothing
end

# Like Base's `unsafe_load`/`unsafe_store!` for `Ptr`, the index is widened to `Int`
# before reaching the intrinsic. This ensures the conversion is done in Julia, where
# the signedness of the index is known, rather than by `getelementptr`, which would
# sign-extend an unsigned index. `Int` must be at least as wide as the index width of
# every address space this is compiled for.
@inline Base.unsafe_load(ptr::Core.LLVMPtr, i::Integer=1, align::Val=Val(1)) =
    pointerref(ptr, Int(i), align)

@inline function Base.unsafe_store!(ptr::Core.LLVMPtr{T}, x, i::Integer=1,
                                    align::Val=Val(1)) where {T}
    pointerset(ptr, convert(T, x), Int(i), align)
    return ptr
end

# pointer operations

# NOTE: this is type-pirating; move functionality upstream

LLVMPtr{T,A}(x::Union{Int,UInt,Ptr}) where {T,A} = reinterpret(LLVMPtr{T,A}, x)
LLVMPtr{T,A}() where {T,A} = LLVMPtr{T,A}(0)

# conversions from and to integers
Base.UInt(x::LLVMPtr) = reinterpret(UInt, x)
Base.Int(x::LLVMPtr) = reinterpret(Int, x)
Base.convert(::Type{LLVMPtr{T,A}}, x::Union{Int,UInt}) where {T,A} =
    reinterpret(LLVMPtr{T,A}, x)

Base.isequal(x::LLVMPtr, y::LLVMPtr) = (x === y)
Base.isless(x::LLVMPtr{T,A}, y::LLVMPtr{T,A}) where {T,A} = x < y

Base.:(==)(x::LLVMPtr{<:Any,A}, y::LLVMPtr{<:Any,A}) where {A} = UInt(x) == UInt(y)
Base.:(<)(x::LLVMPtr{<:Any,A},  y::LLVMPtr{<:Any,A}) where {A} = UInt(x) < UInt(y)
Base.:(==)(x::LLVMPtr, y::LLVMPtr) = false

Base.:(-)(x::LLVMPtr{<:Any,A},  y::LLVMPtr{<:Any,A}) where {A} = UInt(x) - UInt(y)

@llvmgenerated builder function add_ptr(x::LLVMPtr{T,A}, y::I)::LLVMPtr{T,A} where {T,A,I}
    T_ptr = x.value_type
    T_byteptr = convert(LLVMType, Core.LLVMPtr{Int8,A})
    if T_ptr == T_byteptr
        # when LLVMPtr is i8* (the default), or when using opaque pointers
        gep!(builder, LLVM.Int8Type(), x, [y])
    else
        # future proofing, for when LLVMPtr isn't always an i8*
        byteptr = bitcast!(builder, x, T_byteptr)
        byteptr = gep!(builder, LLVM.Int8Type(), byteptr, [y])
        bitcast!(builder, byteptr, T_ptr)
    end
end

Base.:(+)(x::LLVMPtr, y::Integer) = add_ptr(x, Int(y))
Base.:(-)(x::LLVMPtr, y::Integer) = add_ptr(x, -Int(y))
Base.:(+)(x::Integer, y::LLVMPtr) = y + x

Base.unsigned(x::LLVMPtr) = UInt(x)
Base.signed(x::LLVMPtr) = Int(x)

export addrspacecast

"""
    addrspacecast(::Type{Core.LLVMPtr{T,AS}}, ptr::Core.LLVMPtr) -> Core.LLVMPtr{T,AS}

Convert `ptr` to a pointer to `T` in address space `AS`, using an `addrspacecast`
instruction if the address spaces differ. Whether that is valid, and what it does, depends
on the target.
"""
@llvmgenerated builder function addrspacecast(::Type{LLVMPtr{TDest,ASDest}},
                                              src::LLVMPtr{TSrc,ASSrc}
                                             )::LLVMPtr{TDest,ASDest} where {TDest,ASDest,TSrc,ASSrc}
    T_dest = convert(LLVMType, LLVMPtr{TDest,ASDest})
    dest_ptr = ASDest != ASSrc ? addrspacecast!(builder, src, T_dest) : src
    bitcast!(builder, dest_ptr, T_dest)
end


# type-preserving ccall

@generated function _typed_llvmcall(::Val{intr}, rettyp, argtt, args...) where {intr}
    # make types available for direct use in this generator
    rettyp = rettyp.parameters[1]
    argtt = argtt.parameters[1]
    argtyps = DataType[argtt.parameters...]
    argexprs = Any[:(args[$i]) for i in 1:length(args)]

    # arguments passed as a `Val` are emitted as constants. we still pass their value, so
    # that the signature of the function doesn't depend on which arguments are constant.
    const_args = Any[argval <: Val ? argval.parameters[1] : nothing for argval in args]
    for (i, argval) in enumerate(args)
        argval <: Val && (argexprs[i] = const_args[i])
    end

    # build IR that calls the intrinsic, casting types if necessary
    generate_llvmcall(rettyp, argtt, argexprs...) do builder, params...
        T_ret = convert(LLVMType, rettyp)

        # Julia's compiler strips pointers of their element type.
        # reconstruct those so that we can accurately look up intrinsics.
        T_actual_args = LLVMType[]
        actual_args = LLVM.Value[]
        for (arg, argtyp, const_arg) in zip(params, argtyps, const_args)
            if argtyp <: LLVMPtr
                # passed as i8*
                T,AS = argtyp.parameters
                actual_typ = LLVM.PointerType(convert(LLVMType, T), AS)
                actual_arg = if const_arg == C_NULL
                    LLVM.PointerNull(actual_typ)
                elseif const_arg !== nothing
                    intptr = LLVM.ConstantInt(LLVM.Int64Type(), Int(const_arg))
                    const_inttoptr(intptr, actual_typ)
                else
                    bitcast!(builder, arg, actual_typ)
                end
            elseif argtyp <: Ptr
                T = eltype(argtyp)
                actual_typ = LLVM.PointerType(convert(LLVMType, T))
                actual_arg = if const_arg == C_NULL
                    LLVM.PointerNull(actual_typ)
                elseif const_arg !== nothing
                    intptr = LLVM.ConstantInt(LLVM.Int64Type(), Int(const_arg))
                    const_inttoptr(intptr, actual_typ)
                elseif arg.value_type isa LLVM.PointerType
                    # passed as i8* or ptr
                    bitcast!(builder, arg, actual_typ)
                else
                    # passed as i64
                    inttoptr!(builder, arg, actual_typ)
                end
            elseif argtyp <: Bool
                # passed as i8
                actual_typ = LLVM.Int1Type()
                actual_arg = if const_arg !== nothing
                    LLVM.ConstantInt(actual_typ, const_arg)
                else
                    trunc!(builder, arg, actual_typ)
                end
            else
                actual_typ = convert(LLVMType, argtyp)
                actual_arg = if const_arg isa Integer
                    LLVM.ConstantInt(actual_typ, const_arg)
                elseif const_arg isa AbstractFloat
                    LLVM.ConstantFP(actual_typ, const_arg)
                else
                    arg
                end
            end
            push!(T_actual_args, actual_typ)
            push!(actual_args, actual_arg)
        end

        # same for the return type
        T_ret_actual = if rettyp <: LLVMPtr
            T,AS = rettyp.parameters
            LLVM.PointerType(convert(LLVMType, T), AS)
        elseif rettyp <: Ptr
            T = eltype(rettyp)
            LLVM.PointerType(convert(LLVMType, T))
        elseif rettyp <: Bool
            LLVM.Int1Type()
        else
            T_ret
        end

        intr_ft = LLVM.FunctionType(T_ret_actual, T_actual_args)
        intr_f = LLVM.Function(current_module(builder), String(intr), intr_ft)
        rv = call!(builder, intr_ft, intr_f, actual_args)

        # also convert the return value
        if T_ret_actual == LLVM.VoidType()
            nothing
        elseif rettyp <: LLVMPtr
            bitcast!(builder, rv, T_ret)
        elseif rettyp <: Ptr
            if T_ret isa LLVM.PointerType
                bitcast!(builder, rv, T_ret)
            else
                ptrtoint!(builder, rv, T_ret)
            end
        elseif rettyp <: Bool
            zext!(builder, rv, T_ret)
        else
            rv
        end
    end
end

"""
    @typed_ccall(intrinsic, llvmcall, rettyp, (argtyps...), args...)

Perform a `ccall` while more accurately preserving argument types like LLVM expects them:

- `Bool`s are passed as `i1`, not `i8`;
- Pointers (both `Ptr` and `Core.LLVMPtr`) are passed as typed pointers (instead of resp.
  `i8*` and `i64`);
- `Val`-typed arguments will be passed as constants, if supported.

These features can be useful to call LLVM intrinsics, which may expect a specific set of
argument types.

!!! note

    This macro is not needed anymore on Julia 1.12, where the `llvmcall` ABI has been
    extended to preserve argument types more accurately.
"""
macro typed_ccall(intrinsic, cc, rettyp, argtyps, args...)
    # destructure and validate the arguments
    cc == :llvmcall || error("Can only use @typed_ccall with the llvmcall calling convention")
    Meta.isexpr(argtyps, :tuple) || error("@typed_ccall expects a tuple of argument types")

    # assign arguments to variables and unsafe_convert/cconvert them as per ccall behavior
    vars = Tuple(gensym() for arg in args)
    var_exprs = map(zip(vars, args)) do (var,arg)
        :($var = $arg)
    end
    arg_exprs = map(zip(vars,argtyps.args)) do (var,typ)
        quote
            if $var isa Val
                Val(Base.unsafe_convert($typ, Base.cconvert($typ, typeof($var).parameters[1])))
            else
                Base.unsafe_convert($typ, Base.cconvert($typ, $var))
            end
        end
    end

    esc(quote
        $(var_exprs...)
        GC.@preserve $(vars...) begin
            $_typed_llvmcall($(Val(Symbol(intrinsic))), $rettyp, Tuple{$(argtyps.args...)}, $(arg_exprs...))
        end
    end)
end
