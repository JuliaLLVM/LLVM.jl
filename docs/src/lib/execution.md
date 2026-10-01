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
LLVM.GenericValue(::Union{LLVM.FloatType,LLVM.DoubleType}, ::AbstractFloat)
LLVM.to_float
```

## Pointer

```@docs
LLVM.GenericValue(::Ptr)
convert(::Type{Ptr{T}}, ::LLVM.GenericValue) where T
```

## MCJIT

```@docs
LLVM.ExecutionEngine
LLVM.ExecutionEngine(::LLVM.Module)
LLVM.Interpreter
LLVM.JIT
dispose(::LLVM.ExecutionEngine)
Base.push!(::LLVM.ExecutionEngine, ::LLVM.Module)
Base.delete!(::LLVM.ExecutionEngine, ::LLVM.Module)
LLVM.execute
lookup(::LLVM.ExecutionEngine, ::String)
```

### Process-wide symbols

```@docs
LLVM.load_library_permanently
LLVM.add_symbol
LLVM.find_symbol
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
ThreadSafeModule(::LLVM.Module)
dispose(::ThreadSafeModule)
LLVM.unsafe_module
LLVM.unsafe_take_module!
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
GDBRegistrationListener
IntelJITEventListener
OProfileJITEventListener
PerfJITEventListener
JuliaOJIT
ExecutionSession
```

### JITDylibs

```@docs
JITDylib
JITDylib(::ExecutionSession, ::Any)
JITDylib(::JuliaOJIT, ::AbstractString)
LLVM.supports_jit_dylib_creation
lookup_dylib
add!(::LLJIT, ::JITDylib, ::MemoryBuffer)
add!(::JuliaOJIT, ::JITDylib, ::MemoryBuffer)
empty!(::JITDylib)
lookup(::LLJIT, ::Any)
lookup(::JuliaOJIT, ::JITDylib, ::Any)
OrcTargetAddress
```

### Resource trackers

```@docs
ResourceTracker
remove!(::ResourceTracker)
transfer!
dispose(::ResourceTracker)
```

### Symbols

```@docs
LLVMSymbol
mangle
intern
retain
release
SymbolFlags
define!
absolute_symbols
```

### Definition generators

```@docs
DefinitionGenerator
add!(::JITDylib, ::DefinitionGenerator)
dispose(::DefinitionGenerator)
DynamicLibrarySearchGenerator
CustomDefinitionGenerator
```

### Materialization

```@docs
MaterializationUnit
CustomMaterializationUnit
MaterializationResponsibility
emit!(::IRTransformLayer, ::MaterializationResponsibility, ::ThreadSafeModule)
IRTransformLayer
transform!
IRCompileLayer
lazy_reexports
LocalLazyCallThroughManager
LocalIndirectStubsManager
```

### Callback errors

```@docs
LLVM.CallbackException
check_callback_error!
```
