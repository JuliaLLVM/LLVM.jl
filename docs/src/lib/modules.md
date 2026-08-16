# Modules

```@docs
LLVM.Module
copy(::LLVM.Module)
dispose(::LLVM.Module)
```


## Properties and operations

```@docs
context(::LLVM.Module)
name(::LLVM.Module)
name!(::LLVM.Module, ::String)
triple(::LLVM.Module)
triple!(::LLVM.Module, ::String)
datalayout
datalayout!
inline_asm!
inline_asm
sdk_version
sdk_version!
set_used!
set_compiler_used!
```

## Textual representation

```@docs
parse(::Type{LLVM.Module}, ir::String)
string(mod::LLVM.Module)
```

## Binary representation ("bitcode")

```@docs
parse(::Type{LLVM.Module}, membuf::MemoryBuffer)
parse(::Type{LLVM.Module}, data::Vector)
convert(::Type{MemoryBuffer}, mod::LLVM.Module)
convert(::Type{Vector{T}}, mod::LLVM.Module) where {T<:Union{UInt8,Int8}}
write(io::IO, mod::LLVM.Module)
```

## Contents

```@docs
globals
sort!(::LLVM.ModuleGlobalSet)
prevglobal
nextglobal
functions(::LLVM.Module)
sort!(::LLVM.ModuleFunctionSet)
prevfun
nextfun
flags(::LLVM.Module)
```

## Linking

```@docs
link!(::LLVM.Module, ::LLVM.Module)
```
