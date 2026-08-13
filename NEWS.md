# LLVM.jl release notes


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
