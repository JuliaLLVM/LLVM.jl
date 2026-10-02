# staged functions that generate LLVM IR

export @llvmgenerated, generate_llvmcall, current_function, current_module

"""
    current_function(builder::IRBuilder) -> LLVM.Function

Return the function containing the builder's insertion point.
"""
current_function(builder::IRBuilder) = builder.insert_block.parent

"""
    current_module(builder::IRBuilder) -> LLVM.Module

Return the module containing the builder's insertion point, e.g., to declare intrinsics or
add globals from the body of an [`@llvmgenerated`](@ref) function.
"""
current_module(builder::IRBuilder) = current_function(builder).parent

# the `T` of `Type{T}` (which isn't a `DataType` anymore on Julia 1.14)
@static if isdefined(Base, :type_parameter)
    type_parameter(@nospecialize(T)) = Base.type_parameter(T)
else
    type_parameter(@nospecialize(T)) = T.parameters[1]
end

# Arguments whose value is known at generation time are not passed to `llvmcall`, but
# bound to that value in the generator body. This is not the same as being a ghost type
# in LLVM: `Type{T}` lowers to a boxed pointer, but its value `T` is known statically.
function static_argument(@nospecialize(T))
    if Base.isType(T)
        return Some{Any}(type_parameter(T))
    elseif Base.issingletontype(T)
        return Some{Any}(T.instance)
    else
        return nothing
    end
end

# whether a name can only be assigned to, like `_`
function iswriteonly(name::Symbol)
    for c in String(name)
        c == '_' || return false
    end
    return true
end

vararg_exprs(fixed::Vector{Any}, name::Symbol, @nospecialize(types::Tuple)) =
    append!(fixed, Any[:($name[$i]) for i in 1:length(types)])

vararg_values(values::Vector{Any}, nfixed::Int) = (values[nfixed+1:end]...,)

# the value of a `Val`, without specializing on it
valueof(@nospecialize(val::Val)) = typeof(val).parameters[1]

# Emit the return from the entry function, unless the body already did so.
function emit_return!(builder::IRBuilder, f::LLVM.Function, @nospecialize(rv),
                      @nospecialize(rettyp), @nospecialize(T_ret::LLVMType), what::String)
    ref = API.LLVMGetInsertBlock(builder)
    bb = ref == C_NULL ? nothing : BasicBlock(ref)
    if bb === nothing || bb.parent != f
        if rv isa Value && !(rv isa Instruction && isterminator(rv))
            error("$what: the body returned an LLVM value, but the builder is not positioned in the entry function anymore")
        end
        return
    end
    bb.terminator === nothing || return

    if rettyp === Union{}
        rv === nothing ||
            error("$what: the return type is Union{}, so the body should not return a value (got $(typeof(rv)))")
        unreachable!(builder)
    elseif T_ret isa LLVM.VoidType
        rv === nothing ||
            error("$what: the return type $rettyp has no LLVM representation, so the body should return `nothing` (got $(typeof(rv)))")
        ret!(builder)
    else
        rv isa Value ||
            error("$what: the body should return an LLVM value of type $(string(T_ret)), for return type $rettyp (got $(typeof(rv)))")
        rv.value_type == T_ret ||
            error("$what: the body returned a value of type $(string(rv.value_type)), but return type $rettyp lowers to $(string(T_ret))")
        ret!(builder, rv)
    end
    return
end

function _generate_llvmcall(@nospecialize(gen), @nospecialize(rettyp),
                            @nospecialize(argtypes), argexprs::Vector{Any}, what::String,
                            sparams::Union{Nothing,Vector{Any}}=nothing)
    rettyp isa Type || throw(ArgumentError("$what: return type $rettyp is not a type"))
    (argtypes isa DataType && argtypes <: Tuple && !Base.isvatuple(argtypes)) ||
        throw(ArgumentError("$what: argument types $argtypes are not a tuple type of fixed length"))
    params = argtypes.parameters::Core.SimpleVector
    nargs = length(params)
    length(argexprs) == nargs ||
        throw(ArgumentError("$what: got $(length(argexprs)) argument expressions for $nargs argument types"))

    values = Vector{Any}(undef, nargs)
    abi_args = Int[]
    ir, fn = @dispose ctx=Context() begin
        # derive the LLVM signature the same way `llvmcall` lowers the Julia one
        T_ret = convert(LLVMType, rettyp; allow_boxed=true)
        T_args = LLVMType[]
        for i in 1:nargs
            T = params[i]
            val = static_argument(T)
            if val !== nothing
                values[i] = something(val)
                continue
            end
            T_arg = convert(LLVMType, T; allow_boxed=true)
            isghosttype(T_arg) &&
                throw(ArgumentError("$what: argument $i of type $T has no LLVM representation"))
            push!(T_args, T_arg)
            push!(abi_args, i)
        end

        @dispose mod=LLVM.Module("llvmcall") builder=IRBuilder() begin
            f = LLVM.Function(mod, "entry", LLVM.FunctionType(T_ret, T_args))
            push!(f.function_attributes, EnumAttribute("alwaysinline", 0))
            for (param, i) in zip(f.parameters, abi_args)
                values[i] = param
            end

            position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
            rv = if sparams === nothing
                gen(builder, values...)
            else
                # the calling convention of `@llvmgenerated` bodies
                gen(builder, sparams, values)
            end
            emit_return!(builder, f, rv, rettyp, T_ret, what)

            # verify the IR as `llvmcall` will see it, after parsing, which upgrades
            # outdated constructs (e.g. `readnone` on a function declaration)
            ir = string(mod)
            parsed = try
                parse(LLVM.Module, ir)
            catch err
                err isa LLVMException || rethrow()
                error("$what generated invalid LLVM IR: $(err.info)\n$ir")
            end
            try
                verify(parsed)
            catch err
                err isa LLVMException || rethrow()
                error("$what generated invalid LLVM IR: $(err.info)\n$ir")
            finally
                dispose(parsed)
            end

            ir, f.name
        end
    end

    # evaluate every argument expression once and in order, even those we don't pass on
    stmts = Any[Expr(:meta, :inline)]
    call_args = Any[]
    for i in 1:nargs
        ex = argexprs[i]
        if ex isa Symbol || ex isa Expr || ex isa GlobalRef
            tmp = gensym("arg")
            push!(stmts, :($tmp = $ex))
            ex = tmp
        end
        i in abi_args && push!(call_args, ex)
    end
    abi_types = Any[]
    for i in abi_args
        push!(abi_types, params[i])
    end
    push!(stmts, Expr(:call, GlobalRef(Base, :llvmcall), Expr(:tuple, ir, fn),
                      rettyp, Tuple{abi_types...}, call_args...))
    return Expr(:block, stmts...)
end

"""
    generate_llvmcall(rettyp::Type, argtypes::Type{<:Tuple}, argexprs...) do builder, args...
        ...
    end

Generate LLVM IR for a function with Julia return type `rettyp` and argument types
`argtypes`, and return an expression that calls it using `Base.llvmcall` on the arguments
`argexprs`. This is the functional counterpart of [`@llvmgenerated`](@ref), for use in
hand-written generators or when building code with `@eval`; refer to the documentation of
that macro for details on how the body is executed.

The callback receives an `IRBuilder` positioned at the entry of the function, followed by
one value per argument type: the LLVM parameter for arguments that are passed to
`llvmcall`, or the argument's value if it is statically known (singletons, and `T` for
`Type{T}`). Every argument expression is evaluated once, in order, even when the argument
is not passed to `llvmcall`.

The callback is compiled for the types of itself and of its arguments, so a closure that
captures types (e.g., static parameters of the generator) or that takes `Val` arguments is
compiled again for every specialization of the generated function, which can take longer
than the rest of compiling it. To compile the callback once, pass a function that does not
capture anything and does not specialize on its arguments, and pass compile-time
information as statically-known arguments, which are bound to their value:

```julia
function emit_scale(builder, T, x)
    @nospecialize
    mul!(builder, x, ConstantInt(convert(LLVMType, T), sizeof(T)))
end

@generated scale(x::T) where {T<:Integer} =
    generate_llvmcall(emit_scale, T, Tuple{Type{T}, T}, T, :x)
```

[`@llvmgenerated`](@ref) does this automatically.
"""
generate_llvmcall(gen, @nospecialize(rettyp::Type), @nospecialize(argtypes::Type{<:Tuple}),
                  argexprs...) =
    _generate_llvmcall(gen, rettyp, argtypes, Any[argexprs...], "generate_llvmcall")

"""
    @llvmgenerated builder function f(args...)::RT [where {...}]
        ...
    end

Define a staged function `f` whose body is executed once per specialization, at compile
time, to generate the LLVM IR that implements it. The LLVM signature of the generated
function is derived from the Julia one:

```julia
@llvmgenerated builder function add(x::T, y::T)::T where {T<:Integer}
    add!(builder, x, y)
end
```

The body is executed in a fresh LLVM context, with an `IRBuilder` (bound to the name given
as first argument to the macro) positioned in the entry block of a new function. Static
parameters are available as in a regular `@generated` function, but function arguments
are bound to LLVM values instead of their types:

- arguments that are passed to `llvmcall` are bound to their LLVM parameter, whose type is
  what Julia lowers the argument type to (e.g., `Bool` becomes `i8`). On Julia 1.10 and
  1.11, `Ptr` becomes an integer and `Core.LLVMPtr` an `i8` pointer, as the body generates
  IR in a fresh context that uses typed pointers (on 1.11, Julia's code generator uses
  opaque pointers, but its context is not the one of the body). The body should check
  `supports_typed_pointers(LLVM.context())` before assuming opaque pointers, or cast
  unconditionally (`bitcast!` does nothing on opaque pointers). Arguments that lower to a
  boxed pointer are passed as such, and must be handled with care to respect GC invariants;
- arguments whose value is known statically are not passed, but bound to that value
  instead: singletons like `Val{x}()` are bound to the instance, and `Type{T}` to `T`;
- varargs are bound to a tuple of the above.

Varargs may use `args...`, `args::Vararg`, `args::Vararg{T,N}`, or the corresponding
`Core.Vararg` spelling. Other aliases for `Vararg` are not recognized by the macro.

The value returned by the body is returned by the function. It should be an LLVM value of
the type the return type `RT` lowers to, or `nothing` if `RT` has no LLVM representation
(like `Nothing`). If the body terminates the current block itself, e.g., using `ret!`
after emitting control flow, the returned value is ignored. For a return type of
`Union{}`, an `unreachable` terminator is emitted.

The return type annotation is required; it is part of the ABI, and does not result in a
conversion like it would with regular functions.

Julia code that should run before generating IR, like checking or converting arguments,
belongs in a regular function that calls the `@llvmgenerated` one:

```julia
@inline function pointerref(ptr::LLVMPtr{T}, i::Int, align::Val) where {T}
    sizeof(T) == 0 && return T.instance
    return _pointerref(ptr, i - 1, align)
end

@llvmgenerated builder function _pointerref(ptr::LLVMPtr{T,A}, i::Int, ::Val{align})::T where {T,A,align}
    ...
end
```

As arguments are bound to their LLVM value, their Julia type is only available through
static parameters (e.g. `x::T`). When the body needs the Julia types of varargs, write a
`@generated` function that uses [`generate_llvmcall`](@ref) instead.

The generated function is marked for inlining, and verified before being embedded in the
Julia IR. Since it is generated once and cached with the compiled code (including in
package images), the body should only depend on its arguments' types and static
parameters, and should not embed pointers or other session-specific values. The IR may
also be used for different compilation targets, so it should not make assumptions about
the target. As with `@generated` functions, the body can only call functions defined before
the `@llvmgenerated` function.

The body is compiled once for all specializations of the function: it does not specialize
on the types of the arguments or on the values of the static parameters, which it only
sees when it runs. This makes generating a new specialization cheap, and a precompilation
workload that calls one specialization also precompiles the body for the others. Functions
called from the body are compiled as usual, for the types they are called with, so prefer
taking `Val` arguments apart in the signature (`::Val{order}`, making `order` a static
parameter) over passing them to helper functions that would be compiled for every value.
The arguments and static parameters are local variables of the body, so they cannot be
declared `global` in it.

If the body throws an error, Julia's compiler gives up on inferring calls to the function,
which then remain dynamic invocations (e.g., reported as an unsupported dynamic function
invocation when compiling for a GPU), and the error is only thrown when the function is
called. To see the error without calling the function, expand the generator directly, e.g.,
`code_lowered(f, Tuple{Val{1}}; generated=true)` for a call `f(Val(1))`.

To print from the body while debugging, use `Core.println` rather than `println`, which can
fail with "task switch not allowed from inside staged nor pure functions" (e.g., on Julia
1.11). Note that the body runs when the function is compiled, possibly more than once, and
not every time it is called.

!!! warning

    LLVM objects created in the body are only valid until the body returns, and should
    not be stored elsewhere. Similarly, no LLVM objects should be captured from outside
    the body, as they will belong to a different context.
"""
macro llvmgenerated(def)
    throw(ArgumentError("@llvmgenerated expects the name of the builder as first argument, e.g., `@llvmgenerated builder function ...`"))
end

is_vararg_head(head) = head === :Vararg ||
    (Meta.isexpr(head, :., 2) && head.args[1] === :Core &&
     head.args[2] isa QuoteNode && head.args[2].value === :Vararg)

macro llvmgenerated(builder, def)
    builder isa Symbol ||
        throw(ArgumentError("@llvmgenerated expects the name of the builder as first argument, e.g., `@llvmgenerated builder function ...`"))
    if !(Meta.isexpr(def, :function, 2) || Meta.isexpr(def, :(=), 2))
        throw(ArgumentError("@llvmgenerated expects a function definition"))
    end
    sig, body = def.args

    # peel off the where clauses, retaining them for the final definition
    wheres = Any[]
    while Meta.isexpr(sig, :where)
        push!(wheres, sig.args[2:end])
        sig = sig.args[1]
    end
    Meta.isexpr(sig, :(::), 2) ||
        throw(ArgumentError("@llvmgenerated requires a return type annotation, e.g., `f(x)::Nothing`"))
    call, rettyp = sig.args
    Meta.isexpr(call, :call) ||
        throw(ArgumentError("@llvmgenerated expects a function definition"))
    fname = call.args[1]
    what = "@llvmgenerated function $fname"
    Meta.isexpr(fname, :(::)) &&
        throw(ArgumentError("$what: typed callable receivers are not supported"))

    params = Any[]      # arguments of the method
    argtypes = Any[]    # argument types, as available in the generator
    fixed_args = Any[]  # argument expressions, excluding varargs
    vararg = nothing
    for (i, arg) in enumerate(call.args[2:end])
        Meta.isexpr(arg, :parameters) &&
            throw(ArgumentError("$what: keyword arguments are not supported"))
        isva = Meta.isexpr(arg, :...)
        param = isva ? arg.args[1] : arg
        default = nothing
        if Meta.isexpr(param, :kw, 2)
            param, default = param.args
        end
        typ = Meta.isexpr(param, :(::)) ? param.args[end] : nothing
        explicit_va = is_vararg_head(typ) ||
                      (Meta.isexpr(typ, :curly) && is_vararg_head(typ.args[1]))
        isva |= explicit_va
        isva && i != length(call.args) - 1 &&
            throw(ArgumentError("$what: only the last argument can be a vararg"))
        if param isa Symbol
            name = param
        elseif Meta.isexpr(param, :(::), 2) && param.args[1] isa Symbol
            name = param.args[1]
        elseif Meta.isexpr(param, :(::), 1)
            name = gensym("arg")
            param = Expr(:(::), name, param.args[1])
        else
            throw(ArgumentError("$what: unsupported argument `$arg`"))
        end
        if iswriteonly(name)
            # write-only names can't be passed on to `llvmcall`
            name = gensym("arg")
            param = param isa Symbol ? name : Expr(:(::), name, param.args[2])
        end
        name === builder &&
            throw(ArgumentError("$what: argument `$name` conflicts with the name of the builder"))

        if isva
            push!(params, explicit_va ? param : Expr(:..., param))
            push!(argtypes, Expr(:..., name))
            vararg = name
        else
            push!(params, default === nothing ? param : Expr(:kw, param, default))
            push!(argtypes, name)
            push!(fixed_args, QuoteNode(name))
        end
    end

    # static parameters, except for write-only ones like `_`
    sparams = Symbol[]
    for w in wheres, tv in w
        name = tv
        if Meta.isexpr(tv, :<:, 2) || Meta.isexpr(tv, :>:, 2)
            name = tv.args[1]
        elseif Meta.isexpr(tv, :comparison, 5)
            name = tv.args[3]
        end
        name isa Symbol ||
            throw(ArgumentError("$what: unsupported static parameter `$tv`"))
        name === builder &&
            throw(ArgumentError("$what: static parameter `$name` conflicts with the name of the builder"))
        iswriteonly(name) || push!(sparams, name)
    end

    # in the generator, the arguments are bound to their types
    argexprs = Expr(:ref, GlobalRef(Core, :Any), fixed_args...)
    if vararg !== nothing
        argexprs = :($vararg_exprs($argexprs, $(QuoteNode(vararg)), $vararg))
    end

    # the body is compiled once, and not for every specialization of the function: it does
    # not capture the static parameters, and it takes them and the values of the arguments
    # packed in vectors, so its signature doesn't depend on their types or number.
    sparams_var = gensym("sparams")
    values_var = gensym("values")
    bindings = Any[]
    for i in 1:length(sparams)
        push!(bindings, :(local $(sparams[i]) = $sparams_var[$i]))
    end
    for i in 1:length(fixed_args)
        push!(bindings, :(local $(fixed_args[i].value) = $values_var[$i]))
    end
    if vararg !== nothing
        push!(bindings, :(local $vararg = $vararg_values($values_var, $(length(fixed_args)))))
    end
    gen = Expr(:->, Expr(:tuple, builder, sparams_var, values_var),
               Expr(:block, bindings..., body))
    generator = :($_generate_llvmcall($gen, $rettyp,
                                      $(GlobalRef(Core, :Tuple)){$(argtypes...)},
                                      $argexprs, $what,
                                      $(GlobalRef(Core, :Any))[$(sparams...)]))

    sig = Expr(:call, fname, params...)
    for w in reverse(wheres)
        sig = Expr(:where, sig, w...)
    end
    return esc(Expr(:macrocall, GlobalRef(Base, Symbol("@generated")), __source__,
                    Expr(:function, sig, Expr(:block, __source__, generator))))
end
