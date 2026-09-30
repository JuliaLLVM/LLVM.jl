# Julia integration

```@docs
LLVM.Interop
```

## Essentials

```@docs
LLVM.Interop.isboxed
LLVM.Interop.isghosttype
```

## Generating LLVM IR

```@docs
LLVM.Interop.@llvmgenerated
LLVM.Interop.generate_llvmcall
LLVM.Interop.current_function
LLVM.Interop.current_module
```

## Calling inline assembly

```@docs
LLVM.Interop.@asmcall
```

## LLVM pointer support

```@docs
LLVM.Interop.@typed_ccall
LLVM.Interop.addrspacecast
LLVM.Interop.volatile_load
LLVM.Interop.volatile_store!
```

## LLVM intrinsics

```@docs
LLVM.Interop.trap
LLVM.Interop.assume
```

## Type-based alias analysis

```@docs
LLVM.Interop.tbaa_make_child
LLVM.Interop.tbaa_addrspace
```

## Passes

```@docs
LLVM.Interop.JuliaPipeline
```

```@autodocs
Modules = [LLVM.Interop]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM.Interop && getfield(mod, name) === f,
                  LLVM.pass_functions)
```
