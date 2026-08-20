# Execution

## Generic Value

```@docs
GenericValue
dispose(::GenericValue)
```

### Integer

```@docs
GenericValue(::LLVM.IntegerType, ::Integer)
intwidth
convert(::Type{T}, val::GenericValue) where {T <: Integer}
```

## Floating Point

```@docs
GenericValue(::LLVM.FloatingPointType, ::AbstractFloat)
convert(::Type{T}, val::GenericValue, typ::LLVMType) where {T<:AbstractFloat}
```

## Pointer

```@docs
GenericValue(::Ptr)
convert(::Type{Ptr{T}}, ::GenericValue) where T
```

## MCJIT

```@docs
LLVM.ExecutionEngine
Interpreter
JIT
dispose(::LLVM.ExecutionEngine)
Base.push!(::LLVM.ExecutionEngine, ::LLVM.Module)
Base.delete!(::LLVM.ExecutionEngine, ::LLVM.Module)
run(::LLVM.ExecutionEngine, ::LLVM.Function, ::Vector{GenericValue})
lookup(::LLVM.ExecutionEngine, ::String)
functions(::LLVM.ExecutionEngine)
```

## ORCJIT

```@docs
ThreadSafeContext
ThreadSafeContext()
context(::ThreadSafeContext)
dispose(::ThreadSafeContext)
ThreadSafeModule
ThreadSafeModule(::String)
ThreadSafeModule(::Module)
dispose(::ThreadSafeModule)
```

```@docs
LLJIT
JITDylib
lookup(::LLJIT, ::Any)
CallbackException
linkinglayercreator!
check_callback_error
```
