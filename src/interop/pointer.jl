# pointer intrinsics

export @typed_ccall

# TODO: can we use constant propagation instead of passing the alignment as a Val?

using Core: LLVMPtr

@inline @generated function pointerref(ptr::LLVMPtr{T,A}, i::I, ::Val{align}) where {T,A,I,align}
    sizeof(T) == 0 && return T.instance
    ispow2(align) || return :(error("pointerref: alignment must be a power of 2, got $($align)"))
    @dispose ctx=Context() begin
        eltyp = convert(LLVMType, T)

        T_idx = convert(LLVMType, I)
        T_ptr = convert(LLVMType, ptr)

        T_typed_ptr = LLVM.PointerType(eltyp, A)

        # create a function
        param_types = [T_ptr, T_idx]
        llvm_f, _ = create_function(eltyp, param_types)

        # generate IR
        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)
            ptr = if supports_typed_pointers(ctx)
                typed_ptr = bitcast!(builder, parameters(llvm_f)[1], T_typed_ptr)
                inbounds_gep!(builder, eltyp, typed_ptr, [parameters(llvm_f)[2]])
            else
                inbounds_gep!(builder, eltyp, parameters(llvm_f)[1], [parameters(llvm_f)[2]])
            end
            ld = load!(builder, eltyp, ptr)
            if A != 0
                metadata(ld)[LLVM.MD_tbaa] = tbaa_addrspace(A)
            end
            alignment!(ld, align)

            ret!(builder, ld)
        end

        call_function(llvm_f, T, Tuple{LLVMPtr{T,A}, I}, :ptr, :(i-one(I)))
    end
end

@inline @generated function pointerset(ptr::LLVMPtr{T,A}, x::T, i::I, ::Val{align}) where {T,A,I,align}
    sizeof(T) == 0 && return
    ispow2(align) || return :(error("pointerset: alignment must be a power of 2, got $($align)"))
    @dispose ctx=Context() begin
        eltyp = convert(LLVMType, T)

        T_idx = convert(LLVMType, I)
        T_ptr = convert(LLVMType, ptr)

        T_typed_ptr = LLVM.PointerType(eltyp, A)

        # create a function
        param_types = [T_ptr, eltyp, T_idx]
        llvm_f, _ = create_function(LLVM.VoidType(), param_types)

        # generate IR
        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)
            ptr = if supports_typed_pointers(ctx)
                typed_ptr = bitcast!(builder, parameters(llvm_f)[1], T_typed_ptr)
                inbounds_gep!(builder, eltyp, typed_ptr, [parameters(llvm_f)[3]])
            else
                inbounds_gep!(builder, eltyp, parameters(llvm_f)[1], [parameters(llvm_f)[3]])
            end
            val = parameters(llvm_f)[2]
            st = store!(builder, val, ptr)
            if A != 0
                metadata(st)[LLVM.MD_tbaa] = tbaa_addrspace(A)
            end
            alignment!(st, align)

            ret!(builder)
        end

        call_function(llvm_f, Cvoid, Tuple{LLVMPtr{T,A}, T, I},
                      :ptr, :(convert(T,x)), :(i-one(I)))
    end
end

@inline Base.unsafe_load(ptr::Core.LLVMPtr, i::Integer=1, align::Val=Val(1)) =
    pointerref(ptr, i, align)

@inline Base.unsafe_store!(ptr::Core.LLVMPtr{T}, x, i::Integer=1, align::Val=Val(1)) where {T} =
    pointerset(ptr, convert(T, x), i, align)


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

@generated function add_ptr(x::LLVMPtr{T,A}, y::I) where {T,A,I}
    Context() do ctx
        T_uint = convert(LLVMType, I)
        T_ptr = convert(LLVMType, Core.LLVMPtr{T,A})
        T_byteptr = convert(LLVMType, Core.LLVMPtr{Int8,A})

        # create a function
        llvm_f, _ = create_function(T_ptr, [T_ptr, T_uint])

        # generate IR
        IRBuilder() do builder
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)

            if T_ptr == T_byteptr
                # when LLVMPtr is i8* (the default), or when using opaque pointers
                ptr = gep!(builder, LLVM.Int8Type(), parameters(llvm_f)[1], [parameters(llvm_f)[2]])
            else
                # future proofing, for when LLVMPtr isn't always an i8*
                byteptr = bitcast!(builder, parameters(llvm_f)[1], T_byteptr)
                byteptr = gep!(builder, LLVM.Int8Type(), byteptr, [parameters(llvm_f)[2]])
                ptr = bitcast!(builder, byteptr, T_ptr)
            end

            ret!(builder, ptr)
        end

        call_function(llvm_f, Core.LLVMPtr{T,A},
                      Tuple{Core.LLVMPtr{T,A}, I},
                      :x, :y)
    end
end

Base.:(+)(x::LLVMPtr, y::Integer) = add_ptr(x, Int(y))
Base.:(-)(x::LLVMPtr, y::Integer) = add_ptr(x, -Int(y))
Base.:(+)(x::Integer, y::LLVMPtr) = y + x

Base.unsigned(x::LLVMPtr) = UInt(x)
Base.signed(x::LLVMPtr) = Int(x)

@generated function addrspacecast(::Type{LLVMPtr{TDest, ASDest}}, src::LLVMPtr{TSrc, ASSrc}) where {TDest, ASDest, TSrc, ASSrc}
    @dispose ctx=Context() begin
        T_dest = convert(LLVMType, LLVMPtr{TDest, ASDest})
        T_src = convert(LLVMType, LLVMPtr{TSrc, ASSrc})

        llvm_f, _ = create_function(T_dest, [T_src])
        mod = LLVM.parent(llvm_f)

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)

            dest_ptr = if ASDest != ASSrc
                addrspacecast!(builder, parameters(llvm_f)[1], T_dest)
            else
                parameters(llvm_f)[1]
            end
            ret!(builder, bitcast!(builder, dest_ptr, T_dest))
        end

        call_function(llvm_f, LLVMPtr{TDest, ASDest},
                      Tuple{LLVMPtr{TSrc, ASSrc}}, :src)
    end
end


# type-preserving ccall

@generated function _typed_llvmcall(::Val{intr}, rettyp, argtt, args...) where {intr}
    # make types available for direct use in this generator
    rettyp = rettyp.parameters[1]
    argtt = argtt.parameters[1]
    argtyps = DataType[argtt.parameters...]
    argexprs = Any[:(args[$i]) for i in 1:length(args)]

    # build IR that calls the intrinsic, casting types if necessary
    @dispose ctx=Context() begin
        T_ret = convert(LLVMType, rettyp)
        T_args = LLVMType[convert(LLVMType, typ) for typ in argtyps]

        llvm_f, _ = create_function(T_ret, T_args)
        mod = LLVM.parent(llvm_f)

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)

            # Julia's compiler strips pointers of their element type.
            # reconstruct those so that we can accurately look up intrinsics.
            T_actual_args = LLVMType[]
            actual_args = LLVM.Value[]
            for (i, (arg, argtyp, argval)) in enumerate(zip(parameters(llvm_f), argtyps, args))
                # if the value is a Val, we'll try to emit it as a constant
                const_arg = if argval <: Val
                    # also pass the actual value for the fallback path (and to simplify
                    # construction of the LLVM function, where we can ignore constants)
                    argexprs[i] = argval.parameters[1]

                    argval.parameters[1]
                else
                    nothing
                end

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
                    elseif value_type(arg) isa LLVM.PointerType
                        # passed as i8* or ptr
                        bitcast!(builder, arg, actual_typ)
                    else
                        # passed as i64
                        inttoptr!(builder, arg, actual_typ)
                    end
                elseif argtyp <: Bool
                    # passed as i8
                    T = eltype(argtyp)
                    actual_typ = LLVM.Int1Type()
                    actual_arg = if const_arg !== nothing
                        LLVM.ConstantInt(actual_typ, const_arg)
                    else
                        trunc!(builder, arg, actual_typ)
                    end
                else
                    actual_typ = convert(LLVMType, argtyp)
                    actual_arg = if const_arg isa Integer
                        LLVM.ConstantInt(actual_typ, argval.parameters[1])
                    elseif const_arg isa AbstractFloat
                        LLVM.ConstantFP(actual_typ, argval.parameters[1])
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
            intr_f = LLVM.Function(mod, String(intr), intr_ft)

            rv = call!(builder, intr_ft, intr_f, actual_args)

            if T_ret_actual == LLVM.VoidType()
                ret!(builder)
            else
                # also convert the return value
                rv = if rettyp <: LLVMPtr
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

                ret!(builder, rv)
            end
        end

        call_function(llvm_f, rettyp, argtt, argexprs...)
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
