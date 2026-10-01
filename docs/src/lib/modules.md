# Modules

```@docs
LLVM.Module
copy(::LLVM.Module)
dispose(::LLVM.Module)
```

## Textual representation

```@docs
parse(::Type{LLVM.Module}, ir::AbstractString)
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
sort!(::LLVM.ModuleGlobalSet)
sort!(::LLVM.ModuleFunctionSet)
get!(::LLVM.ModuleMetadataIterator, ::AbstractString)
get!(::Base.Callable, ::LLVM.ModuleGlobalSet, ::AbstractString)
get!(::Base.Callable, ::LLVM.ModuleFunctionSet, ::AbstractString)
```

## Linking

```@docs
link!(::LLVM.Module, ::LLVM.Module)
```
