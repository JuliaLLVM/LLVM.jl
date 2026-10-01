## public API

# `@public foo, bar` → `public foo, bar` on Julia ≥ 1.11, nothing on older.
# `public` is only parseable at module top-level on all Julia versions, so a
# bare `@static if ...; public foo; end` would fail at parse time. Taking the
# names through a macro sidesteps that: `foo, bar` parses as a plain tuple,
# and we splice its members into an `Expr(:public, ...)` the lowerer accepts.
#
# The declarations are also recorded in `public_names`, which Julia 1.10 doesn't keep track
# of, e.g., to check that every public name is documented. They are recorded when the
# declaring code runs, so that names declared in version-dependent code are only recorded
# when they exist.
const public_names = Dict{Core.Module,Vector{Symbol}}()
macro public(names)
    syms = names isa Symbol ? (names,) :
           Meta.isexpr(names, :tuple) ? names.args :
           error("@public expects a symbol or a comma-separated list of symbols")
    decl = @static VERSION >= v"1.11" ? Expr(:public, syms...) : nothing
    quote
        $decl
        append!(get!(Vector{Symbol}, $public_names, $__module__),
                $(Expr(:tuple, QuoteNode.(syms)...)))
    end |> esc
end

# To avoid clashes, `using LLVM` only brings `@dispose` into scope. The rest of the API is
# public, and grouped into vocabularies that code can opt into, e.g., `using LLVM.IR` (see
# src/vocabularies.jl). `@vocabulary IR foo, bar` marks `foo` and `bar` public, and adds
# them to the `IR` vocabulary.
#
# When adding API, use `@vocabulary` instead of `export`, picking the subsystem it belongs
# to: `IR` for the object model and its traversal and modification, `Build` for
# constructing IR (including debug info, using the `DIBuilder`), `Passes` for passes and
# pipelines, `ORC` for the JIT. A type and the functions that operate on it belong to the
# same vocabulary, and a name can be part of several vocabularies when it is used by
# several subsystems (e.g., `add!`, `dispose` or `finalize!`). Use `@public` instead for
# functionality that should always be used qualified, like specialized subsystems
# (targets, target machines, data layouts, the legacy execution engines). The accessors
# that back a property are not public at all (see `@property` below).
#
# Only add a method to a Base function when the meaning clearly matches its documented
# contract; e.g., LLVM's `parent` property (the containing object) is not `Base.parent`
# (which unwraps a view), and the size of a debug info type is a property in bits, not
# `Base.sizeof`. Mutating Base methods return the collection they modify, like Base does
# (`push!`, `append!`, `delete!`, `empty!`, `setindex!`).
#
# Naming: predicates are named `isfoo` or `hasfoo`, with the words concatenated when that
# reads well (`isdeclaration`, `isopaque`, `hasjit`), and separated by underscores when it
# doesn't (`is_acquire_or_stronger`). Other names use underscores to separate words
# (`linking_layer_creator!`, `target_machine_builder!`, `debug_location`), except for established
# LLVM terms and abbreviations that are written as one word (`callconv`, `datalayout`,
# `syncscope`, `threadlocal`, `inbounds_gep!`). Functions that mirror a family of LLVM
# names, like the instruction builders or the methods of `AbstractTargetTransformInfo`,
# follow that family.
const vocabularies = Dict{Symbol,Vector{Symbol}}()

macro vocabulary(vocabulary::Symbol, names)
    syms = names isa Symbol ? (names,) :
           Meta.isexpr(names, :tuple) ? names.args :
           error("@vocabulary expects a symbol or a comma-separated list of symbols")
    quote
        @public $names
        append!(get!(vocabularies, $(QuoteNode(vocabulary)), Symbol[]),
                $(Expr(:tuple, QuoteNode.(syms)...)))
    end |> esc
end

# the vocabulary modules re-export bindings that are defined in LLVM: those declared using
# `@vocabulary`, and any additional ones that are passed explicitly
macro reexport(vocabulary::Symbol, extra::Symbol...)
    names = unique([vocabularies[vocabulary]; extra...])
    path = Expr(:., :., :., :LLVM)
    imports = Expr(:import, Expr(:(:), path, (Expr(:., n) for n in names)...))
    esc(Expr(:toplevel, imports, Expr(:export, names...)))
end


# helpers for wrapping the library

function unsafe_message(ptr, args...)
    str = unsafe_string(ptr, args...)
    API.LLVMDisposeMessage(ptr)
    str
end

@vocabulary IR CallbackException
@vocabulary ORC CallbackException

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

# the C API wrappers convert `Vector`s of wrapper objects, so collect other vectors (like
# the views that represent collections of IR objects) before passing them
@inline as_vector(x::Vector) = x
as_vector(x::AbstractVector) = collect(x)

# the most basic check is asserting that we don't use a null pointer
@inline function refcheck(::Type, ref::Ptr)
    ref==C_NULL && throw(UndefRefError())
end


## properties

# Attributes of LLVM objects are exposed as properties, e.g., `gv.linkage`,
# `mod.triple = "..."` or `f.blocks`. Each property is backed by an accessor function of the
# same name (`linkage(gv)`, and `linkage!(gv, val)` for writable properties). These
# accessors are internal: the property is the only public spelling, so don't mark them
# `@public` or add them to a vocabulary, and don't give other public functionality the same
# name (e.g., `overloaded_name` instead of a `name(intrinsic, types)` method). LLVM.jl
# itself can keep calling the accessors. Document a property in the docstring of the type it
# is declared on, in a "Properties" section with signature lines like `gv.linkage` and
# `gv.linkage = linkage::LLVM.Linkage.T` and a description of what assignment does,
# rather than on the accessor, which users do not call. Properties that are declared on a
# group of instructions are documented on the union type of that group, like `CallBase`, and
# those of individual instruction types on the group they belong to, or on `Instruction`.
#
# The one exception is `context`, which is public (and part of `LLVM.IR`) because of
# `context()`, the task-local context, and `context(::ThreadSafeContext)`. The `context`
# property is documented on the types that have it, like other properties.
#
# The root of a type hierarchy opts in using `@properties`, after which `@property`
# declares individual properties for that type or any of its subtypes.
#
# When to use a property, as documented for users in the "Properties" section of the manual
# (docs/src/man/essentials.md):
# - Properties expose what an object has: its characteristics (`name`, `linkage`), its
#   relationships (`parent`, `terminator`, `initializer`, `next`), and its contents, as
#   views (`functions`, `blocks`, `operands`, `uses`, `inline_asm`). Functions ask questions
#   that cannot be assigned (predicates like `isdeclaration` or `isvararg`), compute from
#   additional arguments (`overloaded_name(intr, types)`, `dominates`), or act (operations
#   like `erase!` or `elements!`, builders, and constructors).
# - A collection is a property that returns a view: a live window onto the IR, which
#   queries the IR object when used, never a copy of its contents (don't cache contents
#   either, as they go stale when the IR changes). Make the view mutable where LLVM
#   supports modifying the collection in place (`inst.operands[i] = val`,
#   `push!(f.function_attributes, attr)`), and read-only otherwise, so that mutation throws
#   instead of silently doing nothing (e.g., by subtyping `AbstractVector` without defining
#   `setindex!`). Keyed lookups index the view (`inst.metadata[kind]`,
#   `mod.functions[name]`), and collections that are indexed by position are vectors of
#   views (`f.parameter_attributes[i]`). Assigning to a collection property is not
#   supported. Functions that take a vector of IR objects should accept an `AbstractVector`,
#   so that views can be passed to them.
# - Navigating to a sibling in a list is a relationship, exposed as the read-only `next` and
#   `prev` properties (like C++'s `getNextNode` and `getPrevNode`), which are only declared
#   on objects that are part of a list.
# - A flag that can be assigned is a `Bool` property named without an `is` or `has` prefix
#   (`gv.constant`, `inst.volatile`), not a predicate with an `x!` setter.
# - Richer state, like a bundle of flags or the memory effects of a function, is exposed as
#   a property that returns a view object bound to the IR object (`FastMathFlags`,
#   `FunctionMemoryEffects`). Reading from the view queries the IR object, modifying it
#   (`inst.fast_math.nnan = true`, `f.memory_effects[:argmem] = :read`) writes through, and
#   assigning to the property replaces the state wholesale. Make it easy to convert a view
#   to a value (`NamedTuple(flags)`, `MemoryEffects(effects)`).
# - Enum-valued state uses the enums of the C API, which are documented using their scoped
#   names (`LLVM.Linkage.Internal`, see src/enums.jl), not their `LLVM.API` names.
# - For enum-valued state with a common yes/no question, provide both as properties that
#   are views of the same state, like LLVM's C++ API does (`threadlocal_mode` and
#   `threadlocal`, `tailcall_kind` and `tailcall`). Assigning the current value to the
#   `Bool` view should not change the underlying state.
# - Reading a property may perform a lookup, convert data, or create a (lazy) view, but must
#   not run an analysis, traverse the IR, or construct a collection of IR objects. Views
#   that are derived from other IR (like the `predecessors` of a block, from its uses) only
#   do that work when they are used.
# - Declare the property on the types that support it, not on a supertype where the
#   accessor would fail (e.g., `alignment` is only available on memory instructions). When
#   support depends on the LLVM version, only declare the property on versions that support
#   it (e.g., `disjoint`), but keep defining its accessor so that it can be documented.
# - When a relationship can be absent, the accessor returns `nothing` instead of throwing.
# - A relationship is a property even if it refers to a different kind of object, like the
#   execution session of a JIT (`jit.execution_session`, not `ExecutionSession(jit)`).
#   Constructors are for creating objects (`JITDylib(es, name)`), or for converting and
#   interpreting values (`MemoryEffects(f.memory_effects)`, `Intrinsic(f)`).
# - State that can be set but not read back, or that is a callback, is set with a function
#   named after it (`asm_verbosity!(tm, true)`, `transform!(f, layer)`), as there are no
#   write-only properties.
# - When assignment should not simply call `name!(x, v)`, pass an adapter as the setter,
#   e.g., to accept `nothing` (`debug_location`), or when `name!` is taken by an unrelated
#   function (`subprogram!` creates a subprogram using a `DIBuilder`). If the underlying API
#   cannot implement assignment semantics, add the missing functionality to LLVMExtra (as
#   done to replace fast-math flags), or keep the property read-only.

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
# the error paths don't need to be fast, so don't compile them for every type
@noinline Base.@nospecializeinfer function property_error(@nospecialize(x), s::Symbol,
                                                         @nospecialize(v...))
    names = property_names(x, false)
    if s in names
        # the property exists, but its setter does not support this value
        throw(ArgumentError("cannot set property `$s` of $(typeof(x)) to a value of type " *
                            string(typeof(only(v)))))
    end
    error(typeof(x), " has no property `", s, "`; available properties are: ",
          join(names, ", "))
end

Base.@nospecializeinfer function property_names(@nospecialize(x), private::Bool)
    names = Symbol[name for (T, name) in property_registry if x isa T]
    private && append!(names, fieldnames(typeof(x)))
    return Tuple(unique!(names))
end

# the roots of the type hierarchies that have properties
const property_roots = Type[]
macro properties(T)
    :(push!(property_roots, $T)) |> esc
end

# implement the properties of all hierarchies at once (called after they have been
# declared), as every method that's added to a Base function makes loading slower
function define_properties()
    R = Union{property_roots...}
    @eval begin
        # `ref` is accessed all over the place, so give it a direct path
        @inline Base.getproperty(@nospecialize(x::$R), s::Symbol) =
            s === :ref ? propref(x) : getprop(x, Val(s))
        @inline Base.setproperty!(@nospecialize(x::$R), s::Symbol, v) =
            setprop!(x, Val(s), v)
        Base.propertynames(x::$R, private::Bool=false) = property_names(x, private)
    end
end

# `@property T name` declares a read-only property backed by `name(x)`, while
# `@property T name setter` makes it writable by calling `setter(x, v)`. For setters that
# need to adapt the value, `setter` can be an anonymous function `(x, v) -> ...`, which
# becomes the body of the setter method (so that its arguments can be typed).
macro property(T, name, setter=nothing)
    # `@property T name => getter` uses a getter that isn't named after the property, e.g.,
    # when that name is a Base function with another meaning (like `length`)
    name, getter = Meta.isexpr(name, :call) && name.args[1] === :(=>) ?
                   (name.args[2], name.args[3]) : (name, name)
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
        @inline getprop(x::$T, ::Val{$sym}) = $getter(x)
        $setter_method
        push!(property_registry, ($T, $sym))
    end |> esc
end


## disposing of resources

# the do-block form of resource constructors, e.g., `Foo(f, args...) = with_disposal(f,
# Foo(args...))`: call `f` with the resource, and dispose of it afterwards
function with_disposal(f::F, x) where {F}
    try
        f(x)
    finally
        dispose(x)
    end
end

# Objects that an operation hands over to LLVM (e.g., a memory buffer that is added to a
# JIT, or a materialization unit that is added to a JITDylib) have an `owned` field that
# tracks whether their handle still owns them, like a C++ `unique_ptr` that has been moved
# from. A consumed handle can't be used anymore, and disposing of it does nothing, so that
# it's safe to dispose of it unconditionally (e.g., with `@dispose`). This only covers the
# handle that was handed over, not other wrappers of the same object.
function check_owned(obj)
    obj.owned ||
        throw(ArgumentError("This $(nameof(typeof(obj))) has been consumed or disposed of"))
    return mark_use(obj)
end

# hand the object over to LLVM, returning its reference
function consume_owned!(obj)
    check_owned(obj)
    obj.owned = false
    mark_disposed(obj)
    return obj.ref
end

@public consume!

"""
    LLVM.consume!(obj)
    LLVM.consume!(buf::MemoryBuffer; borrow=false)

Hand `obj` over to foreign code that takes ownership of it, returning its raw handle. This
is for calling a C API that takes ownership of an object, e.g., using `ccall`, which
LLVM.jl's own consuming operations (like adding a buffer or module to a JIT) do
automatically. Afterwards, the wrapper can't be used anymore, and disposing of it does
nothing, like after those operations:

```julia
@dispose tsm=ThreadSafeModule("jit") begin
    ...
    ccall(:jl_consume_module, Cvoid, (LLVM.API.LLVMOrcThreadSafeModuleRef,),
          LLVM.consume!(tsm))
end
```

This is an irreversible handoff, not a conversion: the object isn't disposed of if the
foreign call fails, so validate the other arguments first, and call `consume!` right
before the call. For a C API that only takes ownership when it succeeds, pass the object
itself to the call (which converts it to its handle without consuming it), and call
`consume!` after it succeeded.

Some foreign code takes ownership of an object, but keeps it alive and lets the caller keep
using it, e.g., clang's `SourceManager` with a memory buffer that is then lexed. For a
[`MemoryBuffer`](@ref), `LLVM.consume!(buf; borrow=true)` expresses such a handover: the
wrapper can still be used, but not consumed again, and disposing of it does nothing. This
doesn't extend the lifetime of the buffer, so it can only be used for as long as its new
owner keeps it alive:

```julia
@dispose buf=MemoryBuffer(data) begin
    fid = ccall(:create_file_id, Cint, (Ptr{Cvoid}, LLVM.API.LLVMMemoryBufferRef),
                source_manager, LLVM.consume!(buf; borrow=true))
    lex(source_manager, fid, buf)   # `buf` is still usable
end                                 # and isn't freed here
```

Only this wrapper changes state, not other wrappers of the same object, and the raw handle
doesn't keep Julia objects alive that the wrapper references (e.g., the callbacks of an
[`LLJITBuilder`](@ref LLVM.LLJITBuilder)), so use `GC.@preserve obj` around the foreign
call.

This is supported by the objects that track their ownership: [`MemoryBuffer`](@ref),
[`TargetMachine`](@ref), [`ThreadSafeModule`](@ref LLVM.ThreadSafeModule),
[`MaterializationUnit`](@ref LLVM.MaterializationUnit),
[`MaterializationResponsibility`](@ref LLVM.MaterializationResponsibility),
[`DefinitionGenerator`](@ref LLVM.DefinitionGenerator),
[`ObjectLinkingLayer`](@ref LLVM.ObjectLinkingLayer),
[`TargetMachineBuilder`](@ref LLVM.TargetMachineBuilder) and
[`LLJITBuilder`](@ref LLVM.LLJITBuilder). Objects that are borrowed from LLVM (e.g., the
thread-safe module and materialization responsibility of an IR transformation) can't be
consumed, and neither can consumed or disposed objects.
"""
function consume! end

@public adopt

"""
    LLVM.adopt(obj) -> obj

Register `obj` as an object that foreign code handed over to the caller, e.g., one that a
C API returned with ownership, which the caller is then responsible for disposing of (or
for handing over again, e.g., to an operation that consumes it), like an object that
LLVM.jl created. This is the opposite of [`LLVM.consume!`](@ref):

```julia
ref = ccall(:create_module, LLVM.API.LLVMModuleRef, ())
mod = LLVM.adopt(LLVM.Module(ref))
...
dispose(mod)
```

This is bookkeeping for the `memcheck` debugging mode, which otherwise reports disposing
of the object as disposing of an unknown instance, and afterwards checks the object like
one that LLVM.jl created. It does nothing else: it doesn't take ownership from foreign code
that still owns the object, or make a borrowed, consumed or disposed object usable.

This is supported for [`LLVM.Module`](@ref), [`Context`](@ref), [`MemoryBuffer`](@ref)
and [`LLVM.GenericValue`](@ref). An adopted context owns the modules in it, like one that
LLVM.jl created (see [`dispose(::Context)`](@ref)), so adopt it before adopting its
modules. Unlike creating a context, adopting one doesn't activate it, while disposing of
it pops it from the context stack, so activate it before disposing of it.
"""
function adopt end

# dispose of the object using `f(ref)`, unless it was consumed already
function dispose_owned(f, obj)
    obj.owned || return
    obj.owned = false
    mark_dispose(obj -> f(obj.ref), obj)
    return
end


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
