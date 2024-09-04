# Essentials

## Initialization

```@docs
LLVM.backends
LLVM.InitializeAllTargetInfos
LLVM.InitializeAllTargets
LLVM.InitializeAllTargetMCs
LLVM.InitializeAllAsmParsers
LLVM.InitializeAllAsmPrinters
LLVM.InitializeAllDisassemblers
```


## Contexts

```@docs
Context
Context()
dispose(::Context)
supports_typed_pointers
```

LLVM.jl also tracks the context in task-local scope:

```@docs
context()
activate(::Context)
deactivate(::Context)
context!
```

```@docs
ts_context
activate(::ThreadSafeContext)
deactivate(::ThreadSafeContext)
ts_context!
```


## Resources

```@docs
@dispose
```


## Exceptions

```@docs
LLVMException
```


## Memory buffers

```@docs
MemoryBuffer
MemoryBuffer(::Vector{T}, ::String, ::Bool) where {T<:Union{UInt8,Int8}}
MemoryBufferFile
dispose(::MemoryBuffer)
```


## Other

```@docs
LLVM.clopts
LLVM.ismultithreaded
```
