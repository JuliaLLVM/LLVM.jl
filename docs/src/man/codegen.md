# Code generation

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

To generate native code from an LLVM module, you need to create a target, a target machine,
and use those objects to call the `emit` function to generate machine code.

The functionality on this page is not part of any of the vocabularies, so it is used
qualified, e.g., `LLVM.TargetMachine`.


## Targets

In LLVM, targets represent a specific architecture, such as `x86_64`, or `aarch64`. You
can inspect the available targets using the `LLVM.targets` function:

```julia-repl
julia> collect(LLVM.targets())
5-element Vector{LLVM.Target}:
 LLVM.Target(aarch64_32): AArch64 (little endian ILP32)
 LLVM.Target(aarch64_be): AArch64 (big endian)
 LLVM.Target(aarch64): AArch64 (little endian)
 LLVM.Target(arm64_32): ARM64 (little endian ILP32)
 LLVM.Target(arm64): ARM64 (little endian)
```

The exact availability of targets depends on the LLVM build, and what target infos have been
activated. Additional targets can be activated using `Initialize*TargetInfo` functions:

```jldoctest
julia> LLVM.InitializeWebAssemblyTargetInfo()

julia> # or, to simply initialize all target infos
       LLVM.InitializeAllTargetInfos()
```

Alternatively, targets can also be constructed by name or by triple (again, assuming the
necessary bits in LLVM have been initialized):

```jldoctest target
julia> target = LLVM.Target(; name="wasm64")
LLVM.Target(wasm64): WebAssembly 64-bit

julia> triple = "wasm64-unknown-unknown";

julia> target = LLVM.Target(; triple)
LLVM.Target(wasm64): WebAssembly 64-bit
```

With these objects, a number of APIs are available:

- `target.name`: the target's name
- `target.description`: a textual description of the target
- `LLVM.hasjit`: whether the target has a JIT
- `LLVM.hastargetmachine`: whether the target has a target machine
- `LLVM.hasasmbackend`: whether the target has an assembly backend, to emit object files


## Target machines

Starting from a target and a triple, it's possible to create a target machine for native
code generation purposes. Note that this requires initializing both the target and its
machine code generation support:

```jldoctest target
julia> LLVM.InitializeWebAssemblyTarget();

julia> LLVM.InitializeWebAssemblyTargetMC();

julia> tm = LLVM.TargetMachine(target, triple);
```

The target machine constructor takes various additional options too, as keyword arguments:

- `cpu` and `features`: strings that describe the CPU and its features to target
- `opt_level`: the optimization level to use (e.g., `LLVM.CodeGenOptLevel.Aggressive`)
- `reloc`: the relocation model to use (e.g., `LLVM.RelocMode.PIC`)
- `code`: the code model to use (e.g., `LLVM.CodeModel.Small`)

Various APIs are available to manipulate `TargetMachine` objects:

- `tm.target` and `tm.triple`: the target and triple that was used to create the target
  machine
- `tm.cpu` and `tm.features`: the CPU and features string that were (optionally) set
- `asm_verbosity!`: enable or disable verbose assembly emission

The most important function however is the `emit` function, which converts an IR module to
native code:

```jldoctest target
julia> mod = LLVM.Module("SomeModule");

julia> LLVM.InitializeWebAssemblyAsmPrinter()

julia> String(LLVM.emit(tm, mod, LLVM.CodeGenFileType.Assembly)) |> println
	.text
	.file	"SomeModule"
	.section	.custom_section.target_features,"",@
	.int8	3
	.int8	43
	.int8	15
	.ascii	"mutable-globals"
	.int8	43
	.int8	8
	.ascii	"sign-ext"
	.int8	43
	.int8	8
	.ascii	"memory64"
	.text
```


## Data layout

Data layouts are used to describe the memory layout for a given target. It's the
responsibility of the frontend to generate IR that matches the target's data layout. This
involves both configuring the module with the correct data layout string, but also
generating operations that are valid for the target's memory model.

To create a data layout object, you call the `DataLayout` constructor, either specifying
the data layout string directly, or by inferring it from a target machine

```jldoctest target
julia> LLVM.DataLayout(tm)
DataLayout(e-m:e-p:64:64-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20)

julia> dl = LLVM.DataLayout("e-m:e-p:64:64-i64:64-n32:64-S128");
```

An IR module can now be configured with this data layout:

```jldoctest target
julia> mod.datalayout = dl;

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"

!llvm.module.flags = !{!0, !1}

!0 = !{i32 1, !"wasm-feature-mutable-globals", i32 43}
!1 = !{i32 1, !"wasm-feature-sign-ext", i32 43}
```

The data layout object can be used to query various properties that are relevant for
generating IR. The byte order and the address space of globals are available as the
`byteorder` and `globals_addrspace` properties, while other queries are functions, most of
which take a type or an address space:

- `pointersize`
- `intptr`
- `bit_size`
- `storage_size`
- `abi_size`
- `abi_alignment`
- `frame_alignment`
- `preferred_alignment`
- `element_at`
- `offsetof`


## Disassembly

To go the other way, from machine code to assembly, create a `Disassembler` for a target
triple. This requires initializing the target's info, machine code layer, and disassembler:

```jldoctest disasm
julia> LLVM.InitializeWebAssemblyTargetInfo()

julia> LLVM.InitializeWebAssemblyTargetMC()

julia> LLVM.InitializeWebAssemblyDisassembler()

julia> dis = LLVM.Disassembler("wasm32-unknown-unknown");
```

Like the target machine constructor, the disassembler constructor also takes `cpu` and
`features` keyword arguments, as well as a couple of options that affect the output:

- `hex_immediates`: print immediate operands in hexadecimal
- `alternate_syntax`: use the target's alternate assembly dialect (e.g. Intel syntax on X86)
- `comments`: annotate instructions with target-specific comments

The `disassemble` function then decodes machine code, lazily yielding instructions with
their address, their size in bytes, and their textual representation (or `nothing` if the
bytes could not be decoded):

```jldoctest disasm
julia> code = UInt8[0x41, 0x2a,  # i32.const 42
                    0x0b];       # end

julia> for (; address, size, text) in LLVM.disassemble(dis, code; address=0x100)
           println(string(address; base=16), " (", size, "):", text)
       end
100 (2):	i32.const	42
102 (1):	end
```

Alternatively, the instructions can be printed directly:

```jldoctest disasm
julia> LLVM.disassemble(stdout, dis, code)
	i32.const	42
	end

julia> dispose(dis)
```
