export @asmcall

@generated function _asmcall(::Val{asm}, ::Val{constraints}, ::Val{side_effects},
                             ::Val{rettyp}, ::Val{argtyp}, args...) where
                            {asm, constraints, side_effects, rettyp, argtyp}
    @dispose ctx=Context() begin
        llvm_rettyp = convert(LLVMType, rettyp)
        llvm_argtyp = LLVMType[convert.(LLVMType, [argtyp.parameters...])...]
        llvm_f, llvm_ft = create_function(llvm_rettyp, llvm_argtyp)

        # LLVM dictates the inline asm's return shape from the number of direct
        # outputs in the constraint string: 0 -> void, 1 -> T, N>=2 -> a struct
        # { T0, ..., T_{N-1} }. Julia, however, lowers homogeneous Tuples (incl.
        # NTuple) to [N x T]. Drive the asm callee's return type from `rettyp`
        # (Tuple ⇒ struct, scalar ⇒ T) so we always match LLVM's rule, then
        # bridge to llvm_rettyp via insertvalue when Julia's lowering disagrees.
        asm_rettyp = if rettyp <: Tuple && length(rettyp.parameters) > 0
            elem_types = LLVMType[convert(LLVMType, T) for T in rettyp.parameters]
            length(elem_types) == 1 ? elem_types[1] : LLVM.StructType(elem_types)
        else
            llvm_rettyp
        end
        asm_ft = LLVM.FunctionType(asm_rettyp, llvm_argtyp)
        inline_asm = InlineAsm(asm_ft, String(asm), String(constraints), side_effects)

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)

            val = call!(builder, asm_ft, inline_asm, collect(parameters(llvm_f)))
            if rettyp === Nothing
                ret!(builder)
            elseif asm_rettyp == llvm_rettyp
                ret!(builder, val)
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
                ret!(builder, ret_val)
            end
        end

        call_function(llvm_f, rettyp, argtyp, (:(args[$i]) for i in 1:length(args))...)
    end
end

"""
    @asmcall asm::String [constraints::String] [side_effects::Bool=false]
             rettyp=Nothing argtyp=Tuple{} args...

Call some inline assembly `asm`, optionally constrained by `constraints` and denoting other
side effects in `side_effects`, specifying the return type in `rettyp` and types of
arguments as a tuple-type in `argtyp`.
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
