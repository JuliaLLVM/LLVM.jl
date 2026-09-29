# Code generation

## Targets

```@docs
Target
hasjit(::Target)
hastargetmachine(::Target)
hasasmparser(::Target)
targets
```

## Target machines

```@docs
TargetMachine
dispose(::TargetMachine)
default_triple
normalize(::String)
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
pointersize
intptr
sizeof(::DataLayout, ::LLVMType)
storage_size
abi_size
abi_alignment
frame_alignment
preferred_alignment
element_at
offsetof
```

## Disassembly

```@docs
Disassembler
dispose(::Disassembler)
disassemble
```
