# Code generation

## Targets

```@docs
Target
name(::Target)
description(::Target)
hasjit(::Target)
hastargetmachine(::Target)
hasasmparser(::Target)
targets
```

## Target machines

```@docs
TargetMachine
dispose(::TargetMachine)
target(::TargetMachine)
triple(::TargetMachine)
triple()
normalize(::String)
cpu(::TargetMachine)
features(::TargetMachine)
asm_verbosity!
emit
add_transform_info!
add_library_info!
JITTargetMachine
```

## Data layout

```@docs
DataLayout
dispose(::DataLayout)
byteorder
pointersize
intptr
globals_addrspace
sizeof(::DataLayout, ::LLVMType)
storage_size
abi_size
abi_alignment
frame_alignment
preferred_alignment
element_at
offsetof
```
