export @asmcall

# Count direct (non-indirect) outputs in an LLVM inline-asm constraint string.
# LLVM's rule: with N>=2 direct outputs, the asm call's return type is a struct
# {T0,...,T_{N-1}}. Indirect outputs (`=*`) write through pointer args and do
# not contribute to the return.
function _count_direct_outputs(constraints::AbstractString)
    n = 0
    for tok in eachsplit(constraints, ',')
        s = lstrip(strip(tok), ('&', '%'))
        if startswith(s, "=") && !startswith(s, "=*")
            n += 1
        end
    end
    return n
end

@generated function _asmcall(::Val{asm}, ::Val{constraints}, ::Val{side_effects},
                             ::Val{rettyp}, ::Val{argtyp}, args...) where
                            {asm, constraints, side_effects, rettyp, argtyp}
    @dispose ctx=Context() begin
        llvm_rettyp = convert(LLVMType, rettyp)
        llvm_argtyp = LLVMType[convert.(LLVMType, [argtyp.parameters...])...]
        llvm_f, llvm_ft = create_function(llvm_rettyp, llvm_argtyp)

        # Multi-output inline asm returns a struct in LLVM, but Julia lowers
        # homogeneous tuples to [N x T] arrays. Bridge the mismatch with
        # extractvalue/insertvalue.
        n_outputs = _count_direct_outputs(String(constraints))
        asm_ft = if n_outputs >= 2
            if !(rettyp <: Tuple) || length(rettyp.parameters) != n_outputs
                error("multi-output @asmcall with $n_outputs outputs requires a Tuple rettyp with $n_outputs elements, got $rettyp")
            end
            elem_types = LLVMType[convert(LLVMType, T) for T in rettyp.parameters]
            LLVM.FunctionType(LLVM.StructType(elem_types), llvm_argtyp)
        else
            llvm_ft
        end
        inline_asm = InlineAsm(asm_ft, String(asm), String(constraints), side_effects)

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(llvm_f, "entry")
            position!(builder, entry)

            val = call!(builder, asm_ft, inline_asm, collect(parameters(llvm_f)))
            if rettyp === Nothing
                ret!(builder)
            elseif n_outputs >= 2
                ret_val = LLVM.UndefValue(llvm_rettyp)
                for i in 0:n_outputs-1
                    ret_val = insert_value!(builder, ret_val,
                                            extract_value!(builder, val, i), i)
                end
                ret!(builder, ret_val)
            else
                ret!(builder, val)
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
