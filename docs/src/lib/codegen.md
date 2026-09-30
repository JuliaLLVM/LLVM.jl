# Code generation

## Targets

```@docs
LLVM.Target
LLVM.hasjit(::LLVM.Target)
LLVM.hastargetmachine(::LLVM.Target)
LLVM.hasasmparser(::LLVM.Target)
LLVM.targets
```

## Target machines

```@docs
LLVM.TargetMachine
dispose(::LLVM.TargetMachine)
LLVM.default_triple
LLVM.host_cpu_name
LLVM.host_cpu_features
LLVM.normalize(::String)
LLVM.asm_verbosity!
LLVM.emit
LLVM.JITTargetMachine
```

## Data layout

```@docs
LLVM.DataLayout
dispose(::LLVM.DataLayout)
LLVM.pointersize
LLVM.intptr
LLVM.bit_size
LLVM.storage_size
LLVM.abi_size
LLVM.abi_alignment
LLVM.frame_alignment
LLVM.preferred_alignment
LLVM.element_at
LLVM.offsetof
```

## Disassembly

```@docs
LLVM.Disassembler
dispose(::LLVM.Disassembler)
LLVM.disassemble
```
