## generic value

# TODO: this is a _very_ ugly wrapper, but hard to improve since we can't deduce the type
#       of a GenericValue, and need to pass concrete LLVM type objects to the API

export GenericValue, dispose,
       intwidth

"""
    GenericValue

A generic value that can be passed to or returned from a function in the execution engine.

Note that only simple types are supported, and for most use cases it is recommended
to look up the address of the compiled function and `ccall` it directly.

This object needs to be disposed of using [`dispose`](@ref).
"""
@checked struct GenericValue
    ref::API.LLVMGenericValueRef
end

Base.unsafe_convert(::Type{API.LLVMGenericValueRef}, val::GenericValue) = mark_use(val).ref

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

"""
    intwidth(val::GenericValue)

Get the bit width of the integer value stored in the generic value.
"""
intwidth(val::GenericValue) = API.LLVMGenericValueIntWidth(val)

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

Create a generic value from a floating point number of the given type.
"""
GenericValue(typ::FloatingPointType, N::AbstractFloat) =
    mark_alloc(GenericValue(API.LLVMCreateGenericValueOfFloat(typ, convert(Cdouble, N))))

# NOTE: this ugly three-arg convert is needed to match the C API,
#       which uses the type to call the correct C++ function.

"""
    convert(::Type{<:AbstractFloat}, val::GenericValue, typ::LLVM.FloatingPointType)

Convert a generic value to a floating point number of the given type.

Contrary to the integer conversion, the LLVM type is also required to be passed explicitly.
"""
Base.convert(::Type{T}, val::GenericValue, typ::LLVMType) where {T<:AbstractFloat} =
    convert(T, API.LLVMGenericValueToFloat(typ, val))

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

export Interpreter, JIT,
       run, lookup

"""
    LLVM.ExecutionEngine

An execution engine that can run functions in a module.
"""
@checked struct ExecutionEngine
    ref::API.LLVMExecutionEngineRef
    mods::Set{Module}
end

Base.unsafe_convert(::Type{API.LLVMExecutionEngineRef}, engine::ExecutionEngine) =
    mark_use(engine).ref

# NOTE: these takes ownership of the module
function ExecutionEngine(mod::Module)
    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateExecutionEngineForModule(out_ref, mod, out_error) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return mark_alloc(ExecutionEngine(out_ref[], Set([mod])))
end

"""
    Interpreter(mod::Module)

Create an interpreter for the given module.

This object needs to be disposed of using [`dispose`](@ref).
"""
function Interpreter(mod::Module)
    API.LLVMLinkInInterpreter()

    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateInterpreterForModule(out_ref, mod, out_error) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    return mark_alloc(ExecutionEngine(out_ref[], Set([mod])))
end

"""
    JIT(mod::Module)

Create a JIT compiler for the given module.

This object needs to be disposed of using [`dispose`](@ref).
"""
function JIT(mod::Module, optlevel::API.LLVMCodeGenOptLevel=API.LLVMCodeGenLevelDefault)
    API.LLVMLinkInMCJIT()

    out_ref = Ref{API.LLVMExecutionEngineRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMCreateJITCompilerForModule(out_ref, mod, optlevel, out_error) |> Bool

    if status
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
    mark_dispose.(engine.mods)
    mark_dispose(API.LLVMDisposeExecutionEngine, engine)
end

for x in [:ExecutionEngine, :Interpreter, :JIT]
    @eval function $x(f::Core.Function, args...; kwargs...)
        engine = $x(args...; kwargs...)
        try
            f(engine)
        finally
            dispose(engine)
        end
    end
end

"""
    push!(engine::LLVM.ExecutionEngine, mod::Module)

Add another module to the execution engine.

This takes ownership of the module.
"""
function Base.push!(engine::ExecutionEngine, mod::Module)
    push!(engine.mods, mod)
    API.LLVMAddModule(engine.ref, mod.ref)
end

"""
    delete!(engine::ExecutionEngine, mod::Module)

Remove a module from the execution engine.

Ownership of the module is transferred back to the user.
"""
function Base.delete!(engine::ExecutionEngine, mod::Module)
    out_ref = Ref{API.LLVMModuleRef}()
    API.LLVMRemoveModule(engine.ref, mod.ref, out_ref, Ref{Cstring}()) # out string is not used
    @assert mod == Module(out_ref[])
    delete!(engine.mods, mod)
    return
end

"""
    run(engine::ExecutionEngine, f::Function, [args::Vector{GenericValue}])

Run the given function with the given arguments in the execution engine.
"""
Base.run(engine::ExecutionEngine, f::Function, args::Vector{GenericValue}=GenericValue[]) =
    mark_alloc(GenericValue(API.LLVMRunFunction(engine, f,
                                                length(args), args)))

"""
    lookup(engine::ExecutionEngine, fn::String)

Look up the address of the given function in the execution engine.
"""
function lookup(engine::ExecutionEngine, fn::String)
    addr = Ptr{Nothing}(API.LLVMGetFunctionAddress(engine, fn))
    if addr == C_NULL
        throw(KeyError(fn))
    end
    return addr
end

# function lookup

export functions

struct ExecutionEngineFunctionSet
    engine::ExecutionEngine
end

"""
    functions(engine::ExecutionEngine)

Get an iterator over the functions in the execution engine.

The iterator object is not actually iterable, but supports `get` and `haskey` queries with
function names, and `getindex` to get the function object.
"""
functions(engine::ExecutionEngine) = ExecutionEngineFunctionSet(engine)

Base.IteratorSize(::ExecutionEngineFunctionSet) = Base.SizeUnknown()
Base.iterate(::ExecutionEngineFunctionSet) =
    error("Iteration of functions in the execution engine is not supported")

function Base.get(functionset::ExecutionEngineFunctionSet, name::String, default)
    out_ref = Ref{API.LLVMValueRef}()
    API.LLVMFindFunction(functionset.engine.ref, name, out_ref)
    status = API.LLVMFindFunction(functionset.engine.ref, name, out_ref) |> Bool
    return status == 0 ? Function(out_ref[]) : default
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

export GDBRegistrationListener, IntelJITEventListener,
       OProfileJITEventListener, PerfJITEventListener

@checked struct JITEventListener
    ref::API.LLVMJITEventListenerRef
end
Base.unsafe_convert(::Type{API.LLVMJITEventListenerRef}, listener::JITEventListener) = listener.ref

GDBRegistrationListener()  = JITEventListener(API.LLVMCreateGDBRegistrationListener())
IntelJITEventListener()    = JITEventListener(API.LLVMCreateIntelJITEventListener())
OProfileJITEventListener() = JITEventListener(API.LLVMCreateOProfileJITEventListener())
PerfJITEventListener()     = JITEventListener(API.LLVMCreatePerfJITEventListener())
