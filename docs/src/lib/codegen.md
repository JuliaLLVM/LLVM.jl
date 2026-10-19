# Code generation

## Targets

```@docs
LLVM.Target
LLVM.hasjit(::LLVM.Target)
LLVM.hastargetmachine(::LLVM.Target)
LLVM.hasasmbackend(::LLVM.Target)
LLVM.targets
```

## Target triples

```@docs
LLVM.Triple
LLVM.normalize(::LLVM.Triple)
LLVM.isx86
LLVM.isaarch64
LLVM.isarm
LLVM.isthumb
LLVM.isriscv
LLVM.isppc
LLVM.isnvptx
LLVM.isamdgpu
LLVM.isspirv
LLVM.iswasm
LLVM.isdarwin
LLVM.iswindows
LLVM.islinux
```

## Target machines

```@docs
LLVM.TargetMachine
dispose(::LLVM.TargetMachine)
LLVM.default_triple
LLVM.host_cpu_name
LLVM.host_cpu_features
LLVM.normalize(::AbstractString)
LLVM.asm_verbosity!
LLVM.emit
LLVM.JITTargetMachine
```

## Subtargets

```@docs
LLVM.SubtargetInfo
dispose(::LLVM.SubtargetInfo)
LLVM.processor_features
LLVM.hasfeature
LLVM.hasfeatures
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
LLVM.constant_offset
LLVM.strip_and_accumulate_constant_offsets
```

## Disassembly

```@docs
LLVM.Disassembler
dispose(::LLVM.Disassembler)
LLVM.disassemble
```
