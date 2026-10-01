## generic value

# TODO: this is a _very_ ugly wrapper, but hard to improve since we can't deduce the type
#       of a GenericValue, and need to pass concrete LLVM type objects to the API

@public GenericValue, dispose, to_float

"""
    GenericValue

A generic value that can be passed to or returned from a function in the execution engine.

Note that only simple types are supported, and for most use cases it is recommended
to look up the address of the compiled function and `ccall` it directly.

This object needs to be disposed of using [`dispose`](@ref).

# Properties

    val.intwidth

The bit width of the integer value stored in the generic value.
"""
@checked struct GenericValue
    ref::API.LLVMGenericValueRef
end

Base.unsafe_convert(::Type{API.LLVMGenericValueRef}, val::GenericValue) = mark_use(val).ref

@properties GenericValue

"""
    dispose(val::GenericValue)

Dispose of the given generic value.
"""
dispose(val::GenericValue) = mark_dispose(API.LLVMDisposeGenericValue, val)

"""
    GenericValue(typ::LLVM.IntegerType, N::Integer)

Create a generic value from an integer of the given type.
"""
GenericValue(typ::LLVMType, val)

GenericValue(typ::IntegerType, N::Signed) =
    mark_alloc(GenericValue(
        API.LLVMCreateGenericValueOfInt(typ,
                                        reinterpret(Culonglong, convert(Int64, N)), true)))

GenericValue(typ::IntegerType, N::Unsigned) =
    mark_alloc(GenericValue(
        API.LLVMCreateGenericValueOfInt(typ,
                                        reinterpret(Culonglong, convert(UInt64, N)), false)))

intwidth(val::GenericValue) = Int(API.LLVMGenericValueIntWidth(val))

@property GenericValue intwidth

"""
    convert(::Type{<:Integer}, val::GenericValue)

Convert a generic value to an integer of the given type.
"""
Base.convert(::Type{T}, val::GenericValue) where {T <: Integer}

Base.convert(::Type{T}, val::GenericValue) where {T<:Signed} =
    convert(T, reinterpret(Clonglong, API.LLVMGenericValueToInt(val, true)))

Base.convert(::Type{T}, val::GenericValue) where {T<:Unsigned} =
    convert(T, API.LLVMGenericValueToInt(val, false))

"""
    GenericValue(typ::LLVM.FloatingPointType, N::AbstractFloat)

Create a generic value from a floating point number of the given type, which needs to be
`LLVM.FloatType()` or `LLVM.DoubleType()`: generic values only support single and double
precision floating point numbers.
"""
GenericValue(typ::Union{FloatType,DoubleType}, N::AbstractFloat) =
    mark_alloc(GenericValue(API.LLVMCreateGenericValueOfFloat(typ, convert(Cdouble, N))))

"""
    LLVM.to_float(val::GenericValue, typ::LLVM.FloatingPointType) -> Float64

Get the floating point number stored in a generic value. Unlike integers, generic values
don't know the type of the floating point number they store, so it needs to be passed
explicitly: `LLVM.FloatType()` or `LLVM.DoubleType()`. Use
`convert(T, LLVM.to_float(val, typ))` to get another Julia type.
"""
to_float(val::GenericValue, typ::Union{FloatType,DoubleType}) =
    API.LLVMGenericValueToFloat(typ, val)

"""
    GenericValue(ptr::Ptr)

Create a generic value from a pointer.
"""
GenericValue(ptr::Ptr) =
    mark_alloc(GenericValue(API.LLVMCreateGenericValueOfPointer(convert(Ptr{Cvoid}, ptr))))

"""
    convert(::Type{Ptr{T}}, val::GenericValue)

Convert a generic value to a pointer.
"""
Base.convert(::Type{Ptr{T}}, val::GenericValue) where {T} =
    convert(Ptr{T}, API.LLVMGenericValueToPointer(val))


## execution engine

@public Interpreter, JIT, lookup

"""
    LLVM.ExecutionEngine

An execution engine that can run functions in a module.

# Properties

    engine.functions

The functions in the modules of the execution engine, as a view that supports looking up
a function by name (`get`, `haskey` and indexing). The functions cannot be iterated, so
the view is not a collection.

# Ownership

An execution engine takes ownership of the modules it is created with or that are
added to it using `push!`, and disposes of them together with itself, so these modules
must not be disposed of separately. The constructors take ownership of the module even if
they fail. Use `delete!(engine, mod)` to take back ownership of a module.
"""
@checked struct ExecutionEngine
    ref::API.LLVMExecutionEngineRef
    mods::Set{Module}
end
@public ExecutionEngine, execute
@properties ExecutionEngine

Base.unsafe_convert(::Type{API.LLVMExecutionEngineRef}, engine::ExecutionEngine) =
    mark_use(engine).ref

"""
    LLVM.ExecutionEngine(mod::Module)

Create an execution engine for the given module, taking ownership of it: a JIT compiler
if possible, or an interpreter otherwise.

This object needs to be disposed of using [`dispose`](@ref).
"""
function ExecutionEngine(mod::Module)
    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateExecutionEngineForModule(out_ref, mod, out_error) |> Bool

    if status
        # the module is consumed, even on failure
        mark_dispose(mod)
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return mark_alloc(ExecutionEngine(out_ref[], Set([mod])))
end

"""
    Interpreter(mod::Module)

Create an interpreter for the given module, taking ownership of it.

This object needs to be disposed of using [`dispose`](@ref).
"""
function Interpreter(mod::Module)
    API.LLVMLinkInInterpreter()

    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateInterpreterForModule(out_ref, mod, out_error) |> Bool

    if status
        # the module is consumed, even on failure
        mark_dispose(mod)
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return mark_alloc(ExecutionEngine(out_ref[], Set([mod])))
end

"""
    JIT(mod::Module; opt_level=LLVM.CodeGenOptLevel.Default)

Create a JIT compiler for the given module, taking ownership of it.

This object needs to be disposed of using [`dispose`](@ref).
"""
function JIT(mod::Module; opt_level::API.LLVMCodeGenOptLevel=API.LLVMCodeGenLevelDefault)
    API.LLVMLinkInMCJIT()

    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateJITCompilerForModule(out_ref, mod, opt_level, out_error) |> Bool

    if status
        # the module is consumed, even on failure
        mark_dispose(mod)
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return mark_alloc(ExecutionEngine(out_ref[], Set([mod])))
end

"""
    dispose(engine::ExecutionEngine)

Dispose of the given execution engine.
"""
function dispose(engine::ExecutionEngine)
    for mod in engine.mods
        mark_dispose(mod)
    end
    mark_dispose(API.LLVMDisposeExecutionEngine, engine)
end

for x in [:ExecutionEngine, :Interpreter, :JIT]
    @eval $x(f::Core.Function, args...; kwargs...) =
        with_disposal(f, $x(args...; kwargs...))
end

"""
    push!(engine::LLVM.ExecutionEngine, mod::Module)

Add another module to the execution engine.

This takes ownership of the module.
"""
function Base.push!(engine::ExecutionEngine, mod::Module)
    push!(engine.mods, mod)
    API.LLVMAddModule(engine.ref, mod.ref)
    return engine
end

"""
    delete!(engine::ExecutionEngine, mod::Module)

Remove a module from the execution engine.

Ownership of the module is transferred back to the user. Does nothing if the module isn't
part of the engine.
"""
function Base.delete!(engine::ExecutionEngine, mod::Module)
    mod in engine.mods || return engine
    out_ref = Ref{API.LLVMModuleRef}()
    out_error = Ref{Cstring}(C_NULL)
    # the C API can report a failure, although LLVM's implementation doesn't
    if API.LLVMRemoveModule(engine, mod, out_ref, out_error) |> Bool
        throw(LLVMException(unsafe_message(out_error[])))
    end
    delete!(engine.mods, mod)
    return engine
end

"""
    LLVM.execute(engine::ExecutionEngine, f::LLVM.Function,
                 [args::AbstractVector{GenericValue}]) -> GenericValue

Run the function `f` with the given arguments in the execution engine, and return its
result. The arguments are only borrowed, while the result needs to be disposed of using
[`dispose`](@ref dispose(::GenericValue)).
"""
function execute(engine::ExecutionEngine, f::Function,
                 args::AbstractVector{GenericValue}=GenericValue[])
    vals = convert(Vector{GenericValue}, args)
    mark_alloc(GenericValue(API.LLVMRunFunction(engine, f, length(vals), vals)))
end

"""
    lookup(engine::ExecutionEngine, fn::String)

Look up the address of the given function in the execution engine.
"""
function lookup(engine::ExecutionEngine, fn::String)
    # LLVMGetFunctionAddress returns UInt64 (even on 32-bit platforms)
    # so we need to convert it to UInt first.
    addr = Ptr{Nothing}(API.LLVMGetFunctionAddress(engine, fn) % UInt)
    if addr == C_NULL
        throw(KeyError(fn))
    end
    return addr
end

# function lookup

# a lookup of the functions of an execution engine, which can't be enumerated
struct ExecutionEngineFunctionSet
    engine::ExecutionEngine
end

functions(engine::ExecutionEngine) = ExecutionEngineFunctionSet(engine)

@property ExecutionEngine functions

function Base.get(functionset::ExecutionEngineFunctionSet, name::String, default)
    out_ref = Ref{API.LLVMValueRef}()
    # returns 0 on success
    failed = API.LLVMFindFunction(functionset.engine.ref, name, out_ref) |> Bool
    return failed ? default : Function(out_ref[])
end

function Base.haskey(functionset::ExecutionEngineFunctionSet, name::String)
    f = get(functionset, name, nothing)
    return f != nothing
end

function Base.getindex(functionset::ExecutionEngineFunctionSet, name::String)
    f = get(functionset, name, nothing)
    return f == nothing ? throw(KeyError(name)) : f
end

# event listeners

@vocabulary ORC GDBRegistrationListener, IntelJITEventListener,
                OProfileJITEventListener, PerfJITEventListener

@checked struct JITEventListener
    ref::API.LLVMJITEventListenerRef
end
Base.unsafe_convert(::Type{API.LLVMJITEventListenerRef}, listener::JITEventListener) = listener.ref

"""
    GDBRegistrationListener()
    IntelJITEventListener()
    OProfileJITEventListener()
    PerfJITEventListener()

Create a listener for the events of a JIT, to register the code that it emits with GDB,
Intel VTune, OProfile or Linux' `perf`, e.g., using `register!` on an
[`ObjectLinkingLayer`](@ref). Creating a listener for a profiler that LLVM wasn't built
with support for throws an `UndefRefError`.
"""
GDBRegistrationListener()  = JITEventListener(API.LLVMCreateGDBRegistrationListener())
IntelJITEventListener()    = JITEventListener(API.LLVMCreateIntelJITEventListener())
OProfileJITEventListener() = JITEventListener(API.LLVMCreateOProfileJITEventListener())
PerfJITEventListener()     = JITEventListener(API.LLVMCreatePerfJITEventListener())

for listener in (:IntelJITEventListener, :OProfileJITEventListener, :PerfJITEventListener)
    @eval @doc (@doc GDBRegistrationListener) $listener
end
