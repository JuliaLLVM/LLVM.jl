# Execution

## Generic Value

```@docs
LLVM.GenericValue
dispose(::LLVM.GenericValue)
```

### Integer

```@docs
LLVM.GenericValue(::LLVM.IntegerType, ::Integer)
convert(::Type{T}, val::LLVM.GenericValue) where {T <: Integer}
```

## Floating Point

```@docs
LLVM.GenericValue(::LLVM.FloatingPointType, ::AbstractFloat)
convert(::Type{T}, val::LLVM.GenericValue, typ::LLVMType) where {T<:AbstractFloat}
```

## Pointer

```@docs
LLVM.GenericValue(::Ptr)
convert(::Type{Ptr{T}}, ::LLVM.GenericValue) where T
```

## MCJIT

```@docs
LLVM.ExecutionEngine
LLVM.Interpreter
LLVM.JIT
dispose(::LLVM.ExecutionEngine)
Base.push!(::LLVM.ExecutionEngine, ::LLVM.Module)
Base.delete!(::LLVM.ExecutionEngine, ::LLVM.Module)
run(::LLVM.ExecutionEngine, ::LLVM.Function, ::Vector{LLVM.GenericValue})
lookup(::LLVM.ExecutionEngine, ::String)
functions(::LLVM.ExecutionEngine)
```

## ORC

### Thread-safe contexts and modules

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

### JITs

```@docs
LLJIT
LLJITBuilder
target_machine_builder!
linking_layer_creator!
TargetMachineBuilder
ObjectLinkingLayer
ObjectLinkingLayer(::ExecutionSession, ::String)
JuliaOJIT
ExecutionSession
LLVM.global_prefix
```

### JITDylibs

```@docs
JITDylib
LLVM.lookup_dylib
add!(::LLJIT, ::JITDylib, ::MemoryBuffer)
add!(::JuliaOJIT, ::JITDylib, ::MemoryBuffer)
empty!(::JITDylib)
lookup(::LLJIT, ::Any)
lookup(::JuliaOJIT, ::JITDylib, ::Any)
OrcTargetAddress
```

### Resource trackers

```@docs
LLVM.ResourceTracker
LLVM.default_resource_tracker
remove!(::LLVM.ResourceTracker)
LLVM.transfer!
dispose(::LLVM.ResourceTracker)
```

### Symbols

```@docs
LLVM.LLVMSymbol
mangle
intern
LLVM.retain
LLVM.release
LLVM.symbol_flags
LLVM.define
LLVM.absolute_symbols
```

### Definition generators

```@docs
LLVM.DefinitionGenerator
add!(::JITDylib, ::LLVM.DefinitionGenerator)
dispose(::LLVM.DefinitionGenerator)
LLVM.DynamicLibrarySearchGenerator
LLVM.CustomDefinitionGenerator
```

### Materialization

```@docs
LLVM.CustomMaterializationUnit
LLVM.MaterializationResponsibility
LLVM.requested_symbols
LLVM.emit(::LLVM.IRTransformLayer, ::LLVM.MaterializationResponsibility, ::ThreadSafeModule)
LLVM.IRTransformLayer
LLVM.transform!
LLVM.IRCompileLayer
LLVM.lazy_reexports
LLVM.LocalLazyCallThroughManager
LLVM.LocalIndirectStubsManager
```

### Callback errors

```@docs
LLVM.CallbackException
LLVM.check_callback_error
```
