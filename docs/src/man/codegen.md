# Code generation

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

To generate native code from an LLVM module, you need to create a target, a target machine,
and use those objects to call the `emit` function to generate machine code.


## Targets

In LLVM, targets represent a specific architecture, such as `x86_64`, or `aarch64`. You
can inspect the available targets using the `targets` function:

```julia-repl
julia> collect(targets())
5-element Vector{Target}:
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
julia> target = Target(; name="wasm64")
LLVM.Target(wasm64): WebAssembly 64-bit

julia> triple = "wasm64-unknown-unknown";

julia> target = Target(; triple)
LLVM.Target(wasm64): WebAssembly 64-bit
```

With these objects, a number of APIs are available:

- `name`: the target's name
- `description`: a textual description of the target
- `hasjit`: whether the target has a JIT
- `hastargetmachine`: whether the target has a target machine
- `hasasmparser`: whether the target has an assembly parser


## Target machines

Starting from a target and a triple, it's possible to create a target machine for native
code generation purposes. Note that this requires initializing both the target and its
machine code generation support:

```jldoctest target
julia> LLVM.InitializeWebAssemblyTarget();

julia> LLVM.InitializeWebAssemblyTargetMC();

julia> tm = TargetMachine(target, triple);
```

The target machine constructor takes various additional options too:

- `cpu` and `features`: strings that describe the CPU and its features to target
- `optlevel`: the optimization level to use
- `reloc`: the relocation model to use
- `code`: the code model to use

Various APIs are available to manipulate `TargetMachine` objects:

- `target` and `triple`: the target and triple that was used to create the target machine
- `cpu` and `features`: the CPU and features string that were (optionally) set
- `asm_verbosity!`: enable or disable verbose assembly emission

The most important function however is the `emit` function, which converts an IR module to
native code:

```jldoctest target
julia> mod = LLVM.Module("SomeModule");

julia> LLVM.InitializeWebAssemblyAsmPrinter()

julia> String(emit(tm, mod, LLVM.API.LLVMAssemblyFile)) |> println
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
julia> DataLayout(tm)
DataLayout(e-m:e-p:64:64-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20)

julia> dl = DataLayout("e-m:e-p:64:64-i64:64-n32:64-S128");
```

An IR module can now be configured with this data layout:

```jldoctest target
julia> datalayout!(mod, dl);

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"

!llvm.module.flags = !{!0, !1}

!0 = !{i32 1, !"wasm-feature-mutable-globals", i32 43}
!1 = !{i32 1, !"wasm-feature-sign-ext", i32 43}
```

The data layout object can be used to query various properties that are relevant for
generating IR:

- `byteorder`
- `pointersize`
- `intptr`
- `globals_addrspace`
- `sizeof`
- `storage_size`
- `abi_alignment`
- `frame_alignment`
- `preferred_alignment`
- `element_at`
- `offsetof`
