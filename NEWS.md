# LLVM.jl release notes


## LLVM.jl v9.14

New features:

- `@llvmgenerated` defines staged functions that generate LLVM IR, deriving the LLVM
  signature from the Julia one and binding arguments to LLVM values. This replaces the
  boilerplate of `create_function` and `call_function`, which remain available.
  `generate_llvmcall` offers the same functionality for use in hand-written generators.
- Machine code can be disassembled using a `Disassembler` and the `disassemble` function,
  which lazily decodes instructions into their address, size and textual representation.
- Stack allocations can be aligned using the `align` keyword argument of `alloca!` and
  `array_alloca!`, and the alignment of functions can be inspected and changed using
  `alignment` and `alignment!`.
- The `memory` attribute that replaced `readnone`, `readonly`, `argmemonly` etc. in LLVM 16
  can be created and inspected using `MemoryEffects`, e.g.,
  `memory_effects!(f, MemoryEffects(argmem=:read))` or `access(memory_effects(f)) == :none`.
- Global aliases and ifuncs are supported by means of the `GlobalAlias` and `GlobalIFunc`
  types, and can be iterated using `aliases` and `ifuncs`. Previously, encountering such a
  value, e.g., as an instruction operand, resulted in an "Unknown value kind" error.
- The flags that make integer instructions return poison can be inspected and changed:
  `hasnuw`/`nuw!`, `hasnsw`/`nsw!`, `isexact`/`exact!`, `hasdisjoint`/`disjoint!`,
  `hasnneg`/`nneg!` and `hassamesign`/`samesign!`.
- `case_value` and `case_value!` work on every LLVM version, not just LLVM 22.
- Floating-point constants can be created from and converted to their exact bit pattern,
  using `ConstantFP(typ; bits)` and `LLVM.bitpattern`, e.g., for `fp128` values that a
  `Float64` cannot represent.
- The debug records attached to an instruction can be iterated using `debug_records`
  (LLVM 19+), and inspected using `kind`, `debuglocation`, `variable`, `expression`,
  `value` and `LLVM.location_operands`.

Performance:

- Working with values, types and metadata whose concrete type is only known at run time,
  e.g., when iterating over instructions or their operands, no longer requires dynamic
  dispatch. This makes walking the IR 3 to 6 times faster.
- Subtypes of `LLVM.Value`, `LLVM.LLVMType` and `LLVM.Metadata` need to be immutable
  structs with a single `ref` field, which is now checked when registering them.

Deprecations:

- `nuwneg!` and `const_nuwneg` are deprecated, following LLVM 19, which removed `nuw`
  negation. Use `neg!` followed by `nuw!`, or `const_neg`.

Bug fixes:

- `ConstantDataArray` now copies vectors that aren't stored contiguously (e.g. strided views
  or reinterpreted arrays) instead of producing wrong elements or reading out of bounds, and
  rejects element types that LLVM cannot store as packed data, like `Bool`.
- Indexing a multidimensional `ConstantArray` no longer crashes when one of its rows is a
  `zeroinitializer`, `undef` or `poison` value.
- `alignment` and `alignment!` are now only defined for values that have an alignment, and
  reject invalid alignments, instead of silently returning garbage or corrupting the IR.
- Contexts can be created and disposed of concurrently from multiple threads.
- Pass instrumentation options like `-print-after-all` and `-print-changed` no longer crash or
  silently print nothing when running a `NewPMPassBuilder` pipeline on LLVM 19 and older
  (except on Windows, where Julia itself requires LLVM 20 for these options).
- `section!` is now only defined for global objects, as LLVM does not support setting the
  section of a global alias.
- `last(functions(mod))` returns the last function of a module, instead of the first one.
- `personality` returns the actual personality value, which may be a `GlobalAlias` or a
  constant expression, instead of wrapping it as a `Function`, and `personality!` accepts
  any constant.


## LLVM.jl v9.13

New features:

- Functions and global variables can be reordered with `move_before` and `move_after`, or
  sorted by name with `sort!`, enabling deterministic module layouts.
- Named metadata operands can be removed with `empty!`.


## LLVM.jl v9.12

New features:

- Support for LLVM 22, including `PtrToAddrInst` and the new `case_value`/`case_value!`
  accessors required now that switch case values are no longer regular operands.
- LLVM.jl can precompile without LLVMExtra, improving support for custom LLVM builds before
  their extensions library has been built.


## LLVM.jl v9.11

- Added [`convert_users_to_instructions!`](https://github.com/JuliaLLVM/LLVM.jl/pull/580)
  to materialize constant expressions and aggregates as instructions at their points of use.


## LLVM.jl v9.10

- Added the [`ExpandAtomicModifyPass`](https://github.com/JuliaLLVM/LLVM.jl/pull/564) used
  by Julia 1.13 and later.
- Improved compatibility with Julia's evolving module-decoration API, including direct use
  of `jl_decorate_llvm_module` on Julia 1.14.
- Thread-safe contexts now report LLVM diagnostics as Julia exceptions, and contexts are
  kept alive during exception unwinding to prevent invalid captured IR values.


## LLVM.jl v9.9

- Substantially expanded the [debug-info API](https://github.com/JuliaLLVM/LLVM.jl/pull/549),
  including builders and accessors for variables, expressions, locations, and records.


## LLVM.jl v9.8

- Added support for LLVM's newer attribute representation.
- Fixed LLJIT lookup on Windows by providing the frame-registration stubs expected by LLVM
  21.


## LLVM.jl v9.7

- Fixed and documented multi-output [`@asmcall`](https://github.com/JuliaLLVM/LLVM.jl/pull/552)
  for homogeneous tuple return types.
- Improved memory-checking accuracy for emitted buffers and contexts.


## LLVM.jl v9.6

- Added support for [lazy module parsing and linking](https://github.com/JuliaLLVM/LLVM.jl/pull/547).


## LLVM.jl v9.5

- Added support for LLVM 21.
- New pass-manager pipelines can use a [custom target transform
  implementation](https://github.com/JuliaLLVM/LLVM.jl/pull/542).
- `pointerref` and `pointerset` now reject non-power-of-two alignments instead of producing
  invalid IR that may be miscompiled.


## LLVM.jl v9.4

- Added support for LLVM 20.
- Added call-site and invocation attribute iterators, `local_unnamed_addr` accessors, and
  richer global-value metadata accessors.
- External libraries can register custom C++ passes, with exceptions propagated back to
  Julia instead of escaping through LLVM.
- Fixed global strings being emitted into the wrong address space.


## LLVM.jl v9.3

- New-pass-manager options are parsed by LLVM itself, improving support for version-specific
  pass parameters while retaining deprecations for renamed options.


## LLVM.jl v9.2

- Added support for LLVM 19.
- Extended the atomic-instruction API with ordering, synchronization-scope, and operation
  accessors.
- Fixed ownership tracking when transferring modules into `ThreadSafeModule`.


## LLVM.jl v9.1

The most important feature of this release is the addition of documentation, both in the
form of function docstrings, and an extensive manual.

As part of the documentation writing effort, many minor issues or areas for improvement were
identified, resulting in a large amount of minor, but breaking changes. For all of those,
deprecations are in place. However, it is strongly recommended to update your code to the
new APIs as soon as possible, which can be done by testing your code with `--depwarn=error`.

Technically beaking changes (unlikely to affect any users):

- Metadata values attached using the `metadata` function [now need to
  be](https://github.com/JuliaLLVM/LLVM.jl/pull/476) a subtype of `MDNode`. This behavior
  was already expected by LLVM, but only triggered a crash using an assertions build.
- Creating a `ThreadSafeModule` from a `Module` [now
  will](https://github.com/JuliaLLVM/LLVM.jl/pull/474) copy the source module into the active
  thread-safe context. This is a behavioural change, but is unlikely to affect any users.
  The previous behavior resulted in the wrong context being used, which could lead to
  crashes.

Minor changes (breaking changes with deprecations):

- Branch instruction predicate getters [have been
  renamed](https://github.com/JuliaLLVM/LLVM.jl/pull/473) from `predicate_int` and
  `predicate_float` to simply `predicate`. The old names are deprecated.
- Conversion of a `MDString` to a Julia string [is now
  implemented](https://github.com/JuliaLLVM/LLVM.jl/pull/470) using the `convert` method,
  rather than the `string` method. The old method is deprecated.
- The `delete!` and `unsafe_delete!` methods [have been
  renamed](https://github.com/JuliaLLVM/LLVM.jl/pull/467) to `remove!` and `erase!` to more
  closely match LLVM's terminology. The old names are deprecated.
- Copy constructors [have been deprecated](https://github.com/JuliaLLVM/LLVM.jl/pull/466) in
  favor of explicit `copy` methods.
- Several publicly unused APIs that had been deprecated upstream, have been removed:
  [`GlobalContext`](https://github.com/JuliaLLVM/LLVM.jl/pull/463),
  [`ModuleProvider`](https://github.com/JuliaLLVM/LLVM.jl/pull/465),
  [`PassRegistry`](https://github.com/JuliaLLVM/LLVM.jl/pull/461).

New features:

- A `lookup` function [has been added](https://github.com/JuliaLLVM/LLVM.jl/pull/458) to
  enable extracting the address of a compiled function from an execution engine. This makes
  it possible to simply `ccall` a compiled function without having to deal with
  `GenericValue`s.
- `globalstring!` and `globalstring_ptr!` now support `addrspace` and `add_null` arguments,
  similar to their C++ counterparts.


## LLVM.jl v9.0

Major changes:

- The `OperandBundle` API [was changed](https://github.com/JuliaLLVM/LLVM.jl/pull/437) to the
  upstream version, replacing `OperandBundleDef` and `OperandBundleUse` with
  `OperandBundle`, renaming `tag_name` to `tag` and removing `tag_id`. No deprecations are
  in place for this change.
- The `SyncScope` API [was changed](https://github.com/JuliaLLVM/LLVM.jl/pull/443) to the
  upstream version, switching from string-based synchronization scope names to a
  `SyncScope` object, while adding `is_atomic` check and `syncscope`/`syncscope!` getters
  and setters for atomic instructions. Deprecations are in place for the old API.

New features:

- Support for LLVM 18
- An alias-analysis pipeline [can now be
  specified](https://github.com/JuliaLLVM/LLVM.jl/pull/439) using the `NewPMAAManager` API.
- API wrappers [now come with](https://github.com/JuliaLLVM/LLVM.jl/pull/448) docstrings.
- Functions [have been added](https://github.com/JuliaLLVM/LLVM.jl/pull/447) to move between
  blocks, instructions and functions without having to iterate using the parent.


## LLVM.jl v8.1

Minor changes:

- Support for Julia versions below v1.10 has been dropped.

New features:

- A [memory checker](https://github.com/JuliaLLVM/LLVM.jl/pull/420) has been added. Toggling
  the `memcheck` preference to `true` will enable LLVM.jl to detect missing disposes, use
  after frees, etc.
- Support for `atomic_rmw!` with synchronizatin scopes [has been
  added](https://github.com/JuliaLLVM/LLVM.jl/pull/431)


## LLVM.jl v8.0

Major changes:

- The NewPM wrappers [have been overhauled](https://github.com/JuliaLLVM/LLVM.jl/pull/416) to
  be based on the upstream string-based interface, rather than maintaining various API
  extensions to expose the pass manager internals. There are no deprecations in place for
  this change.


## LLVM.jl v7.2

Minor changes:

- Metadata APIs [have been extended](https://github.com/JuliaLLVM/LLVM.jl/pull/414) to all
  value subtypes, making it possible to attach metadata to functions.


## LLVM.jl v7.1

Minor changes:

- The NewPM internalize pass [has been
  extended](https://github.com/JuliaLLVM/LLVM.jl/pull/409) to support a list of exported
  symbols. This makes it possible to switch GPUCompiler.jl to the new pass manager.


## LLVM.jl v7.0

Major changes:

- `LowerSIMDLoopPass` [was switched](https://github.com/JuliaLLVM/LLVM.jl/pull/398) to being a
  loop pass on Julia v1.10. This may require having to use a different pass manager.
