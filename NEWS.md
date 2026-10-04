# LLVM.jl release notes


## LLVM.jl v10.1

### Analyses

Custom passes written in Julia can now use LLVM's analyses. A function pass created with
`FunctionPass(name, callback; analyses=true)` receives the pipeline's
`FunctionAnalysisManager`, which returns analysis results by type (`am[DomTree]`), and
can return a `PreservedAnalyses` value (e.g. `PreservedAnalyses(CFGAnalyses)`) instead of
a boolean.

The new `LLVM.Analysis` vocabulary groups the analyses and the values they compute.
`ConstantRange` and `KnownBits` are immutable values that represent the possible values of
an integer, and use LLVM's implementation to compute with them (`r + s`,
`intersect_with(r, s)`, `binary_op(LLVM.Opcode.Mul, r, s; nsw=true)`, ...). Range
attributes can be created from, and read back as, a `ConstantRange` (`attr.value`).

LLVM's value tracking computes the range or known bits of an integer value, optionally
using the assumptions that hold at an instruction: `ConstantRange(v; at, assumptions,
domtree)` and `KnownBits(v; ...)`. The `AssumptionCache` of a function lists its
assumptions (`ac[v]` for those that affect a value), and new ones are registered with
`push!`. `is_valid_assume_for_context`, `is_guaranteed_not_to_be_poison` and
`program_undefined_if_poison` mirror the corresponding LLVM queries.

`LazyValueInfo` computes ranges at a point of the function, also using the conditions of
the branches that lead there: `ConstantRange(lvi, v; at=inst)`, on an edge between two
blocks (`from`, `to`), or at a use (LLVM 16+).


## LLVM.jl v10.0

LLVM.jl 10 uses a smaller set of consistent names and explicit ownership rules. The
highlights below describe the main changes for packages upgrading to this release.

### Generated IR and Julia interop

`@llvmgenerated` defines staged functions that generate LLVM IR from a Julia signature;
`generate_llvmcall` supports hand-written generators. These replace repeated LLVM and
Julia signature declarations in `create_function` / `call_function` code. Static arguments
remain available in the generator but are omitted from the `llvmcall` ABI. Generators
support explicit `Vararg{T,N}` syntax and compile once across
specializations, reducing the cost of generating IR for new argument types.

### Namespaces, properties and IR views

`using LLVM` exports only `@dispose`. Opt into `LLVM.IR`, `LLVM.Build`, `LLVM.Passes`, and
`LLVM.ORC`, or qualify names. The old getter and setter functions were removed without
deprecations; see the *Vocabularies* and *Properties* sections of the Essentials manual
for the new spellings.
Object state and relationships use properties: `f.name`,
`f.blocks`, `inst.parent`, `mod.globals`, and writable forms such as `gv.linkage = ...`.
Collections are live IR views; use `collect` or `copy(mod.used)` for a snapshot.
Use `LLVM.before(inst)` and `LLVM.at_end(bb)`
for insertion and movement; the other factories are `LLVM.after`, `LLVM.at_begin`, and
`LLVM.after_phis`. An instruction moved with
`move!(inst, builder.position)` keeps its own debug location; assign
`inst.debug_location = builder.debug_location` to copy the builder's location. Debug
records inserted at a head position reverse their order on LLVM 19 and later.

### Types, constants and attributes

LLVM types and constants are IR objects with explicit properties and element views.
Constructors return the actual LLVM constant kind, including canonical zero, undef and
poison aggregates. Multidimensional constant arrays handle zero extents. Named Julia
struct constants honor `packed=true` and reject incompatible reuse. Data layout sizes
have explicit units: `bit_size`, `storage_size`, and `abi_size`. `element_at` and `offsetof`
use one-based struct fields; GEP indices in IR remain zero-based. Attribute kind keys are
Symbols and string attributes remain Strings. Old integer-kind comparisons can silently
become false. `MemoryEffects` works across supported LLVM versions when representable.

### Atomics

Builders accept orderings, scopes, alignment and volatility together. Positional forms
and property setters apply the same ordering checks. Partword expansion requires a fixed,
naturally aligned value and rejects pointer-valued subword operations. Pointer-to-integer
atomic lowering requires an integral pointer representation. The expansion helpers retain
atomic initial loads, pointer provenance, metadata, volatility and alignment. Code that
requires a scope should name it explicitly; the default is system scope.

### Debug information

Debug types follow LLVM's local-scope hierarchy. Temporary nodes have owned handles, and
DIBuilder inputs accept metadata operand views after checking their elements. Parameter
numbers are positive and one-based. Debug-record insertion uses explicit positions; the
direction of debug-location assignment matters when porting old `debuglocation!` calls.

### ORC and ownership

Thread-safe modules, target machines, buffers and JIT resources distinguish owning,
borrowed and consumed wrappers. Call `LLVM.consume!(x)` once foreign code has taken
ownership of `x`. A borrowed module from a thread-safe module is valid only during its
callback unless `unsafe_module` is used. Keep the exact Julia `LLJIT` wrapper that owns
foreign JIT callbacks rooted until the native JIT is destroyed. `jit.datalayout_string`
returns the text; `DataLayout(jit)` creates a queryable layout object.

### Passes and native execution

`PassBuilder` is the single pass-manager interface. Custom `ModulePass` and `FunctionPass`
callbacks accept `required=true` when correctness requires LLVM to run them even where it
skips optional passes. Audit legacy correctness passes during migration. Target
emission reports deferred diagnostics. Legacy execution
engines gain explicit execution and static constructor/destructor runners. Machine code
can be decoded lazily with `Disassembler` and `disassemble`.

### Additional features

Stack allocation builders accept `align`; functions expose alignment. Global aliases and
ifuncs have wrappers and collection views. Integer poison flags, exact floating-point
constant bit patterns, debug-record inspection and memory-effect attributes are supported.
Common IR traversal and builder calls are faster.


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
