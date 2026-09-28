# Julia integration

## Essentials

```@docs
LLVM.Interop.isboxed
LLVM.Interop.isghosttype
```

## Generating LLVM IR

```@docs
LLVM.Interop.@llvmgenerated
LLVM.Interop.llvmcall_expr
LLVM.Interop.current_function
LLVM.Interop.current_module
LLVM.Interop.create_function
LLVM.Interop.call_function
```

## Calling inline assembly

```@docs
LLVM.Interop.@asmcall
```

## LLVM pointer support

```@docs
LLVM.Interop.@typed_ccall
```

## LLVM intrinsics

```@docs
LLVM.Interop.trap
LLVM.Interop.assume
```
