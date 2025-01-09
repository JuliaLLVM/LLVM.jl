# LLVM.jl release notes


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
