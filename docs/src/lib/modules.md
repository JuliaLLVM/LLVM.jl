# Modules

```@docs
LLVM.Module
copy(::LLVM.Module)
dispose(::LLVM.Module)
```

## Operations

```@docs
append_inline_asm!
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
aliases
prevalias
nextalias
ifuncs
previfunc
nextifunc
module_flags
```

## Linking

```@docs
link!(::LLVM.Module, ::LLVM.Module)
```
