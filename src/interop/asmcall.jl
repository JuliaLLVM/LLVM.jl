export @asmcall

function check_asm_operand(T::Type, llvm_T::LLVMType, what::String)
    llvm_T isa Union{LLVM.IntegerType,LLVM.FloatingPointType,
                     LLVM.PointerType,LLVM.VectorType} && return

    throw(ArgumentError("@asmcall $what must lower to an LLVM scalar or vector type; " *
                        "$T lowers to `$(string(llvm_T))`"))
end

@generated function _asmcall(::Val{asm}, ::Val{constraints}, ::Val{side_effects},
                             ::Val{rettyp}, ::Val{argtyp}, args...) where
                            {asm, constraints, side_effects, rettyp, argtyp}
    # the declared argument types determine the ABI, not the actual ones
    argexprs = Any[:(args[$i]) for i in 1:length(args)]
    llvmcall_expr(rettyp, argtyp, argexprs...) do builder, params...
        llvm_rettyp = convert(LLVMType, rettyp)
        llvm_argtyp = LLVMType[convert(LLVMType, T) for T in argtyp.parameters]

        # LLVM accepts aggregate inputs in IR, but can crash while selecting them.
        for (i, T) in enumerate(argtyp.parameters)
            check_asm_operand(T, llvm_argtyp[i], "argument $i")
        end

        # Tuples describe multiple outputs, except when they lower to an LLVM vector.
        multiple_outputs = rettyp <: Tuple && !(llvm_rettyp isa LLVM.VectorType)
        if multiple_outputs
            for (i, T) in enumerate(rettyp.parameters)
                check_asm_operand(T, convert(LLVMType, T), "return value element $i")
            end
        elseif rettyp !== Nothing
            check_asm_operand(rettyp, llvm_rettyp, "return value")
        end

        # LLVM dictates the inline asm's return shape from the number of direct
        # outputs in the constraint string: 0 -> void, 1 -> T, N>=2 -> a struct
        # { T0, ..., T_{N-1} }. Julia, however, lowers homogeneous Tuples (incl.
        # NTuple) to [N x T]. Drive the asm callee's return type from `rettyp`
        # (Tuple ⇒ struct, scalar ⇒ T) so we always match LLVM's rule, then
        # bridge to llvm_rettyp via insertvalue when Julia's lowering disagrees.
        asm_rettyp = if multiple_outputs && length(rettyp.parameters) > 0
            elem_types = LLVMType[convert(LLVMType, T) for T in rettyp.parameters]
            length(elem_types) == 1 ? elem_types[1] : LLVM.StructType(elem_types)
        else
            llvm_rettyp
        end
        asm_ft = LLVM.FunctionType(asm_rettyp, llvm_argtyp)
        inline_asm = InlineAsm(asm_ft, String(asm), String(constraints), side_effects)

        val = call!(builder, asm_ft, inline_asm, collect(Value, params))
        if rettyp === Nothing
            nothing
        elseif asm_rettyp == llvm_rettyp
            val
        else
            # asm returned T or { T0, ... }; outer fn must return llvm_rettyp
            # (typically [N x T] for homogeneous tuples). Reshape via
            # insertvalue; optimization folds it away or reduces to a small
            # struct→array shuffle.
            ret_val = LLVM.UndefValue(llvm_rettyp)
            n = length(rettyp.parameters)
            for i in 0:n-1
                elem = n == 1 ? val : extract_value!(builder, val, i)
                ret_val = insert_value!(builder, ret_val, elem, i)
            end
            ret_val
        end
    end
end

"""
    @asmcall asm::String [constraints::String] [side_effects::Bool=false]
             rettyp=Nothing argtyp=Tuple{} args...

Call some inline assembly `asm`, optionally constrained by `constraints` and denoting other
side effects in `side_effects`, specifying the return type in `rettyp` and types of
arguments as a tuple-type in `argtyp`.

Inputs and individual direct outputs must lower to LLVM scalar or vector types. Pass
aggregate inputs as separate values or through a pointer.

For inline asm with multiple direct outputs (e.g. constraints `"=r,=r"`), pass `rettyp` as a
`Tuple` whose element count matches the number of `=` outputs in `constraints`; the result
is returned as a Julia tuple. Use a scalar `rettyp` for a single output, and `Nothing` when
the asm has no direct outputs (indirect `=*` outputs that write through pointer arguments
do not contribute to the return).

```julia
# single output
@asmcall("bswap \$0", "=r,r", UInt32, Tuple{UInt32}, x)

# two outputs (heterogeneous and homogeneous both work)
@asmcall("...", "=r,=r", Tuple{Int16,Int32})
@asmcall("...", "=r,=r", Tuple{UInt32,UInt32})
```
"""
:(@asmcall)

macro asmcall(asm::String, constraints::String, side_effects::Bool,
              rettyp::Union{Expr,Symbol,Type}=:(Nothing),
              argtyp::Union{Expr,Type}=:(Tuple{}), args...)
    asm_val = Val{Symbol(asm)}()
    constraints_val = Val{Symbol(constraints)}()
    return esc(:($Interop._asmcall($asm_val, $constraints_val,
                                   Val{$side_effects}(), Val{$rettyp}(), Val{$argtyp}(),
                                   $(args...))))
end

# shorthand: no side_effects
macro asmcall(asm::String, constraints::String,
              rettyp::Union{Expr,Symbol,Type}=:(Nothing),
              argtyp::Union{Expr,Type}=:(Tuple{}), args...)
    esc(:($Interop.@asmcall $asm $constraints false $rettyp $argtyp $(args...)))
end

# shorthand: no side_effects or constraints
macro asmcall(asm::String,
              rettyp::Union{Expr,Symbol,Type}=:(Nothing),
              argtyp::Union{Expr,Type}=:(Tuple{}), args...)
    esc(:($Interop.@asmcall $asm "" $rettyp $argtyp $(args...)))
end
