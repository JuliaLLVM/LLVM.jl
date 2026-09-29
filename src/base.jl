# helpers for wrapping the library

function unsafe_message(ptr, args...)
    str = unsafe_string(ptr, args...)
    API.LLVMDisposeMessage(ptr)
    str
end

export CallbackException

"""
    CallbackException

Exception captured inside a Julia callback and rethrown after the surrounding
foreign operation has returned normally. The original exception is available
in the `ex` field.
"""
struct CallbackException <: Exception
    context::String
    ex::Any
    processed_bt::Vector{Base.StackTraces.StackFrame}

    function CallbackException(context, ex, bt)
        processed_bt = stacktrace(bt)
        new(context, ex, processed_bt[1:min(100, end)])
    end
end

function Base.showerror(io::IO, err::CallbackException)
    print(io, "exception in ", err.context, " callback\n\n    nested exception: ")
    showerror(io, err.ex, err.processed_bt, backtrace=true)
end

# callbacks may run concurrently, e.g., when LLVM compiles on multiple threads
const CALLBACK_EXCEPTION_LOCK = ReentrantLock()

# record the first exception thrown by a callback (to be called from a `catch` block)
function _capture_callback_exception!(state, err)
    bt = Base.catch_backtrace()
    @lock CALLBACK_EXCEPTION_LOCK begin
        state.exception === nothing && (state.exception = (err, bt))
    end
    return nothing
end

# take and clear the recorded exception, if any
function _take_callback_exception!(state)
    @lock CALLBACK_EXCEPTION_LOCK begin
        exception = state.exception
        state.exception = nothing
        exception
    end
end

# `@public foo, bar` → `public foo, bar` on Julia ≥ 1.11, nothing on older.
# `public` is only parseable at module top-level on all Julia versions, so a
# bare `@static if ...; public foo; end` would fail at parse time. Taking the
# names through a macro sidesteps that: `foo, bar` parses as a plain tuple,
# and we splice its members into an `Expr(:public, ...)` the lowerer accepts.
macro public(names)
    @static if VERSION >= v"1.11"
        syms = names isa Symbol ? (names,) :
               Meta.isexpr(names, :tuple) ? names.args :
               error("@public expects a symbol or a comma-separated list of symbols")
        return esc(Expr(:public, syms...))
    else
        return nothing
    end
end


## defining types in the LLVM type hierarchy

# llvm.org/docs/doxygen/html/group__LLVMCSupportTypes.html

# macro that adds an inner constructor to a type definition,
# calling `refcheck` on the ref field argument
macro checked(typedef)
    # decode structure definition
    if Meta.isexpr(typedef, :struct)
        structure = typedef.args[2]
        body = typedef.args[3]
    else
        error("argument is not a structure definition")
    end
    if isa(structure, Symbol)
        # basic type definition
        typename = structure
    elseif Meta.isexpr(structure, :<:)
        # typename <: parentname
        all(e->isa(e,Symbol), structure.args) ||
            error("typedef should consist of plain types, ie. not parametric ones")
        typename = structure.args[1]
    else
        error("malformed type definition: cannot decode type name")
    end

    # decode fields
    field_names = Symbol[]
    field_defs = Union{Symbol,Expr}[]
    for arg in body.args
        if isa(arg, LineNumberNode)
            continue
        elseif isa(arg, Symbol)
            push!(field_names, arg)
            push!(field_defs, arg)
        elseif Meta.isexpr(arg, :(::))
            push!(field_names, arg.args[1])
            push!(field_defs, arg)
        end
    end
    :ref in field_names || error("structure definition should contain 'ref' field")

    # insert checked constructor
    push!(body.args, :(
        $typename($(field_defs...)) = (refcheck($typename, ref); new($(field_names...)))
    ))

    return esc(typedef)
end

## dispatch-free access to wrapper objects

# Objects like values, types and metadata are wrapped in a Julia type that mirrors LLVM's
# class hierarchy, and that's only known at run time (e.g., the operands of an instruction
# can be of any value type). To avoid dynamic dispatch when working with such objects, all
# of these wrappers have the same layout: a single `ref` field. That makes it possible to
# construct them, and to access their reference, without knowing the concrete type.

# check that a wrapper type has the expected layout
function check_layout(T::Type, R::Type{<:Ptr})
    valid = isbitstype(T) && fieldcount(T) == 1 &&
            fieldname(T, 1) === :ref && fieldtype(T, 1) === R
    valid || error("$T should be an immutable struct with a single `ref::$R` field")
end

# construct an object of a type only known at run time, without calling its constructor.
# this does not perform any checks, so the type needs to be the result of `identify`.
@inline unsafe_wrap_ref(@nospecialize(T::Type), ref::R) where {R<:Ptr} =
    ccall(:jl_new_bits, Any, (Any, Ref{R}), T, ref)

# load the reference of an object, without dispatching on its type. when the concrete type
# is known, this compiles to a plain field access.
@inline function unsafe_load_ref(::Type{R}, @nospecialize(obj)) where {R<:Ptr}
    GC.@preserve obj unsafe_load(Ptr{R}(ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)))
end

# the most basic check is asserting that we don't use a null pointer
@inline function refcheck(::Type, ref::Ptr)
    ref==C_NULL && throw(UndefRefError())
end


## properties

# Attributes of LLVM objects are exposed as properties, e.g., `gv.linkage` or
# `mod.triple = "..."`. Each property is backed by an accessor function of the same name
# (`linkage(gv)`, and `linkage!(gv, val)` for writable properties). These accessors are
# internal: the property is the only public spelling, so don't mark them `@public` or add
# them to a vocabulary, and don't give other public functionality the same name (e.g.,
# `overloaded_name` instead of a `name(intrinsic, types)` method). LLVM.jl itself can keep
# calling the accessors. Document a property in the docstring of the type it is declared on,
# in a "Properties" section with signature lines like `gv.linkage` and
# `gv.linkage = linkage::LLVM.API.LLVMLinkage` and a description of what assignment does,
# rather than on the accessor, which users do not call. Properties that are declared on a
# group of instructions are documented on the union type of that group, like `CallBase`, and
# those of individual instruction types on the group they belong to, or on `Instruction`.
#
# The one exception is `context`, which is public because of `context()`, the task-local
# context, and `context(::ThreadSafeContext)`. The `context` property is documented on the
# types that have it, like other properties.
#
# The root of a type hierarchy opts in using `@properties`, after which `@property`
# declares individual properties for that type or any of its subtypes.

# the reference of wrapper objects (overridden for hierarchies whose concrete type is only
# known at run time, to access it without dispatch)
@inline propref(x) = getfield(x, :ref)

# (type, name) pairs, to implement `propertynames`
const property_registry = Tuple{Type,Symbol}[]

# fall back to the object's fields, or error with a list of the available properties
@inline function getprop(x, ::Val{S}) where {S}
    hasfield(typeof(x), S) || property_error(x, S)
    getfield(x, S)
end
@inline function setprop!(x, ::Val{S}, v) where {S}
    hasfield(typeof(x), S) || property_error(x, S, v)
    setfield!(x, S, v)
end
@noinline function property_error(x, s::Symbol, v...)
    names = property_names(x, false)
    if s in names
        # the property exists, but its setter does not support this value
        throw(ArgumentError("cannot set property `$s` of $(typeof(x)) to a value of type $(typeof(only(v)))"))
    end
    error(typeof(x), " has no property `", s, "`; available properties are: ",
          join(names, ", "))
end

function property_names(x, private::Bool)
    names = Symbol[name for (T, name) in property_registry if x isa T]
    private && append!(names, fieldnames(typeof(x)))
    return Tuple(unique!(names))
end

macro properties(T)
    quote
        # `ref` is accessed all over the place, so give it a direct path
        @inline Base.getproperty(@nospecialize(x::$T), s::Symbol) =
            s === :ref ? propref(x) : getprop(x, Val(s))
        @inline Base.setproperty!(@nospecialize(x::$T), s::Symbol, v) =
            setprop!(x, Val(s), v)
        Base.propertynames(x::$T, private::Bool=false) = property_names(x, private)
    end |> esc
end

# `@property T name` declares a read-only property backed by `name(x)`, while
# `@property T name setter` makes it writable by calling `setter(x, v)`. For setters that
# need to adapt the value, `setter` can be an anonymous function `(x, v) -> ...`, which
# becomes the body of the setter method (so that its arguments can be typed).
macro property(T, name::Symbol, setter=nothing)
    sym = QuoteNode(name)
    setter_method = if setter === nothing
        :(setprop!(x::$T, ::Val{$sym}, v) =
            error("property `", $sym, "` of ", typeof(x), " is read-only"))
    elseif Meta.isexpr(setter, :->)
        x, v = setter.args[1].args
        vname = Meta.isexpr(v, :(::)) ? v.args[1] : v
        :(setprop!($x::$T, ::Val{$sym}, $v) = ($(setter.args[2]); $vname))
    else
        :(setprop!(x::$T, ::Val{$sym}, v) = ($setter(x, v); v))
    end
    quote
        @inline getprop(x::$T, ::Val{$sym}) = $name(x)
        $setter_method
        push!(property_registry, ($T, $sym))
    end |> esc
end


## helper macro for disposing resources without do-block syntax

export @dispose

"""
    @dispose foo=Foo() bar=Bar() begin
        ...
    end

Helper macro for disposing resources (by calling the `dispose` function for every resource
in reverse order) after executing a block of code. This is often equivalent to calling the
resource constructor with do-block syntax, but without using (potentially costly) closures.

Resources are constructed in order, and each one is disposed of even if constructing a later
resource fails. For example, if `Bar()` throws, `foo` is still disposed of.
"""
macro dispose(ex...)
    resources = ex[1:end-1]
    code = ex[end]

    Meta.isexpr(code, :block) ||
        error("Expected a code block as final argument to LLVM.@dispose")

    # nest a try/finally block per resource, so that resources that have already been
    # constructed are disposed of when constructing a later one throws. this matters for,
    # e.g., contexts, which would otherwise remain active on the context stack.
    ex = code
    for res in reverse(resources)
        Meta.isexpr(res, :(=)) ||
            error("Resource arguments to LLVM.@dispose should be assignments")
        ex = quote
            let $res
                try
                    $ex
                finally
                    $dispose($(res.args[1]))
                end
            end
        end
    end
    esc(ex)
end
