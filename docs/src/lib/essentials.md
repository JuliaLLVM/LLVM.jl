# Essentials

## Vocabularies

```@docs
LLVM.IR
LLVM.Build
LLVM.Passes
LLVM.ORC
```

## Version

```@docs
LLVM.version
```

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
LLVM.consume!
LLVM.adopt
```

## Exceptions

```@docs
LLVMException
```

## Memory buffers

```@docs
MemoryBuffer
MemoryBuffer(::Vector{T}, ::AbstractString, ::Bool) where {T<:Union{UInt8,Int8}}
MemoryBufferFile
dispose(::MemoryBuffer)
```

## Other

```@docs
LLVM.clopts
LLVM.ismultithreaded
```
