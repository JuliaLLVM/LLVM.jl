# LLVM.jl release notes


## LLVM.jl v10.0

This release reorganizes LLVM.jl's API, so that it can be combined with other packages and
has one spelling for every concept. The "Vocabularies" and "Properties" sections of the
manual describe the new design.

Namespace:

- `using LLVM` now only brings `@dispose` into scope, so that it doesn't clash with other
  packages. The rest of the API is public, and can be used qualified
  (`LLVM.isdeclaration(f)`) or brought into scope by opting into one of the new
  vocabularies: `LLVM.IR` for the object model, its predicates and operations, `LLVM.Build`
  for the `IRBuilder`, the `DIBuilder` and constant expressions, `LLVM.Passes` for passes
  and pipelines, and `LLVM.ORC` for the ORC JIT. Code that did `using LLVM` typically needs
  `using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes`.
- Targets, target machines, data layouts, target initialization, the disassembler and the
  legacy execution engines are not part of any vocabulary, and need to be qualified
  (`LLVM.TargetMachine`, `LLVM.DataLayout`), as do `LLVM.Module` and `LLVM.Function`,
  which would clash with Base. Types that used to require qualification, like
  `LLVM.Int32Type()`, `LLVM.PointerType`, `LLVM.FunctionType` and the instruction types
  (`LLVM.CallInst`, `LLVM.LoadInst`, ...), are part of `LLVM.IR`, and so are new union
  types for groups of instructions that share properties: `CallBase` (`call`, `invoke` and
  `callbr`), `TerminatorInst`, `AtomicInst`, `MemAccessInst`, `AlignedInst`, `NoWrapInst`,
  `ExactInst`, `NonNegInst` and `FPMathInst`.
- The functions of the `DIBuilder` (`LLVM.file!`, `LLVM.subprogram!`, ...), which were
  public but not exported, are part of `LLVM.Build`.

Properties:

- What an LLVM object has is now a property: its attributes (`f.name`, `gv.linkage`,
  `mod.triple`, `loc.line`), its relationships to other objects (`inst.parent.parent`,
  `bb.terminator`, `f.entry`, `gv.initializer`), and its contents (`mod.functions`,
  `f.blocks`, `inst.operands`). Assigning to a property sets it, where that is supported
  (`gv.linkage = LLVM.Linkage.Internal`). This generally follows the getters and
  setters of LLVM's C++ API, e.g., `I->getParent()` becomes `inst.parent`.
- The functions that used to provide this information have been removed from the API:
  `name(f)` becomes `f.name`, `name!(f, "x")` becomes `f.name = "x"`, `blocks(f)` becomes
  `f.blocks`, `LLVM.parent(inst)` becomes `inst.parent`, and so on. The exception is
  `context`, because of `context()` (the task-local context): `context(mod)` becomes
  `mod.context`, but `context()` and `context!` remain. Functions remain for predicates
  (`isdeclaration(f)`), computations that take arguments, and operations.
- Relationships that can be absent are `nothing`: the `entry` of a function without a body,
  the `parent` of an instruction or block that has been removed, and the `next` or `prev`
  sibling at the end of a list.
- The contents of objects are live views of the IR, which reflect later changes. They are
  mutable where LLVM supports it (`inst.operands[i] = val`,
  `push!(f.function_attributes, attr)`, `mod.flags[name, behavior] = md`), and read-only
  otherwise, so that mutation throws an error instead of silently changing a copy. Keyed
  lookups index these views (`mod.functions["f"]`, `inst.metadata[kind]`), and collections
  indexed by position are vectors of views (`f.parameter_attributes[i]`,
  `call.argument_attributes[i]`, `switch.case_values[i]`). Collections that used to be
  copies are now views too: the operands of metadata nodes and named metadata nodes, the
  parameters of function types, the predecessors of a block, the arguments of a call and
  the location operands of a debug record. Use `collect` to get a copy. Functions that take
  a vector of IR objects accept these views.
- Siblings in a list are the `next` and `prev` properties, replacing `nextinst`/`previnst`,
  `nextblock`/`prevblock`, `nextfun`/`prevfun`, `nextglobal`/`prevglobal`,
  `nextalias`/`prevalias` and `nextifunc`/`previfunc`. They are also available on function
  parameters, named metadata nodes and debug records (`prev` requires LLVM 20).
- Flags that can be assigned are `Bool` properties named without an `is` or `has` prefix,
  replacing pairs of predicates and setters: `gv.constant` (`isconstant(gv)`/`constant!`),
  `gv.externally_initialized` (`isextinit`/`extinit!`), `gv.threadlocal`, `inst.volatile`,
  `cmpxchg.weak`, `call.tailcall`, and the poison-generating flags introduced in 9.14,
  `inst.nuw`, `inst.nsw`, `inst.exact`, `inst.disjoint`, `inst.nneg` and `inst.samesign`,
  which are only available on the instructions (and LLVM versions) that support them.
  `isconstant(val)` still exists, but only checks whether a value is a constant.
- `gv.unnamed_addr` holds an `LLVM.UnnamedAddr.T`, replacing `unnamed_addr` and
  `local_unnamed_addr`, which described the same state with two Bools. `gv.threadlocal` is
  a Bool view of `gv.threadlocal_mode`, and `call.tailcall` of the new `call.tailcall_kind`
  (available on every LLVM version).
- The fast-math flags of a floating-point instruction are a `FastMathFlags` view with a Bool
  property per flag: `inst.fast_math.nnan = true` sets a flag, `inst.fast_math.fast = true`
  sets all of them, and `inst.fast_math = (; nnan=true)` replaces all flags.
  `NamedTuple(inst.fast_math)` replaces `fast_math(inst)`.
- The memory effects of a function are a `FunctionMemoryEffects` view of its `memory`
  attribute, which can be modified in place (`f.memory_effects[:argmem] = :read`) or
  replaced (`f.memory_effects = MemoryEffects(...)`). Calls have the same property,
  `call.memory_effects`, for the `memory` attribute of the call site, replacing
  `memory_effects` and `memory_effects!` on call site attributes.
- Module-level inline assembly is a collection: `push!(mod.inline_asm, asm)` appends,
  `empty!` clears, and `String(mod.inline_asm)` returns its text, replacing `inline_asm`
  and `inline_asm!`. This anticipates LLVM 24, which represents it as a list of fragments.
- The global values in `llvm.used` and `llvm.compiler.used` are sets, available as
  `mod.used` and `mod.compiler_used`, which support `push!`, `delete!`, `union!`,
  `setdiff!`, `empty!`, iteration and `in`. This replaces `set_used!(mod, gvs...)` and
  `set_compiler_used!`, which could only append global variables, with
  `union!(mod.used, gvs)`.
- The documentation of properties is part of the docstring of the type that has them, in
  a "Properties" section, e.g., `?LLVM.GlobalVariable` or `?LLVM.CallBase`.
- The ORC API follows the same design: `jit.triple`, `jit.datalayout`, `jit.global_prefix`
  (replacing `get_prefix`), `jit.execution_session`, `lljit.main_dylib`,
  `lljit.ir_transform_layer`, `jljit.ir_compile_layer`, `jd.default_resource_tracker` and
  `mr.requested_symbols` (replacing `get_requested_symbols`, as a read-only view). The
  constructors that used to return these, like `JITDylib(lljit)` or
  `ExecutionSession(jit)`, have been removed; constructors that create objects, like
  `JITDylib(es, name)`, remain.
- The size, offset and alignment of debug info types are `ty.size_in_bits`,
  `ty.offset_in_bits` and `ty.align_in_bits`. This replaces `offset(ty)`, `align(ty)` and
  `sizeof(ty)`, which returned eight times the size in bits.

Renamed functionality, for consistency:

- `is_opaque(ptrtyp)` is now `isopaque`, like for structure types, `is_atomic` is
  `isatomic`, and `LLVM.available(op)` is `LLVM.isavailable`.
- `targetmachinebuilder!`, `linkinglayercreator!` and `set_transform!` are now
  `target_machine_builder!`, `linking_layer_creator!` and `transform!`.
- `debuglocation` is now the `debug_location` property, and `threadlocalmode` the
  `threadlocal_mode` property.
- `LLVM.triple()` (the host triple) is now `LLVM.default_triple()`, and
  `LLVM.name(intrinsic, types)` is `LLVM.overloaded_name`.
- `subprogram!(f, sp)` becomes `f.subprogram = sp`, while `subprogram!` remains the
  `DIBuilder` function that creates a subprogram. `debuglocation!(builder, inst)`, which
  copied the builder's debug location to an instruction, becomes
  `inst.debug_location = builder.debug_location`.
- `elements!(st, elems, packed)` takes `packed` as a keyword argument, as documented.

Removed functionality:

- `LLVM.Interop.create_function` and `call_function` have been removed in favor of
  `@llvmgenerated` and `generate_llvmcall`.
- Deprecated functionality has been removed: `called_value`, `predicate_int`,
  `predicate_real`, `unsafe_delete!`, `get_subprogram`/`set_subprogram!`, `has_orc_v1`,
  `has_orc_v2`, `has_newpm`, `has_julia_ojit`, `ValueMetadataDict`,
  `LLVM.Interop.JuliaPipelinePass`, `lookup(jljit, name)` without a `JITDylib`, string
  sync scopes for `fence!`/`atomic_rmw!`/`atomic_cmpxchg!`, `size(::VectorType)`,
  `Module(::Module)`, `Instruction(::Instruction)`, `delete!` on functions and blocks, the
  old spellings of pass keyword arguments (e.g., `allow_partial`, now `partial`), `nuwneg!`
  and `const_nuwneg`, `CreateDynamicLibrarySearchGeneratorForProcess(prefix)` (use
  `DynamicLibrarySearchGenerator(jit)`), `reexports` (use `lazy_reexports`), `get_prefix`
  and `get_requested_symbols`. `string(::MDString)` now returns the textual form of the
  metadata, like for other metadata; use `convert(String, md)` for the string's contents.

Pass managers:

- The legacy pass manager has been removed: `ModulePassManager()` and
  `FunctionPassManager(mod)` from the legacy API, legacy custom passes, `PassManagerBuilder`
  and the legacy transform functions (`instruction_combining!`, ..., which only existed
  before LLVM 17), the legacy Julia passes in `LLVM.Interop` (`alloc_opt!`, ...),
  `add_transform_info!`, `add_library_info!` and `LLVM.has_oldpm()`. LLVM deprecated the
  legacy pass manager, and LLVM.jl's interface to the new one works on every supported
  version of LLVM, including custom passes written in Julia.
- The new pass manager's types lost their `NewPM` prefix: `PassBuilder`, `PassManager`,
  `ModulePassManager()`, `CGSCCPassManager()`, `FunctionPassManager()`,
  `LoopPassManager()`, `AAManager()`, custom passes created with `ModulePass(name, f)` and
  `FunctionPass(name, f)` (of type `CustomPass`), and the `DebugifyPass` and
  `CheckDebugifyPass` constructors.
- `add!` and `register!` return the pass builder or pass manager, including when adding a
  nested pass manager using a do-block, instead of internal state.
- `ExpandReductionsPass()` (`expand-reductions`) is available on every supported version of
  LLVM; LLVM itself only registers it with the new pass manager since LLVM 21.

Insertion points:

- Where to insert or move IR objects is an `InsertionPoint`, created with
  `LLVM.before(x)`, `LLVM.after(x)`, `LLVM.at_begin(c)`, `LLVM.at_end(c)` and
  `LLVM.after_phis(bb)`. These factories are public, but not part of a vocabulary.
  Positions are resolved when they are created, and follow LLVM's rules for debug
  records: `before(inst)` inserts after the debug records attached to `inst`, while
  `after(prev)` and `at_begin(bb)` insert before them.
- `position!(builder, pos)` positions a builder at an insertion point, replacing
  `position!(builder, inst)` and `position!(builder, bb)`, which didn't make clear where
  instructions would go (`LLVM.before(inst)` and `LLVM.at_end(bb)`, respectively).
  `LLVM.after(inst)` also works for the last instruction of a block, `LLVM.at_begin(bb)`
  positions before any PHI nodes, and `LLVM.after_phis(bb)` at the first position where
  other instructions can go, like C++'s `getFirstInsertionPt`. `builder.position` is the
  insertion point of a builder, and `builder.insert_block` the block, replacing
  `position(builder)`. `position!(builder, pos) do ... end` positions a builder
  temporarily, and restores its position and debug location afterwards.
- `move!(x, pos)` moves an instruction, basic block, function or global variable to an
  insertion point, replacing `move_before` and `move_after`. Instructions and blocks that
  are not part of a block or function are inserted, which replaces `insert!(builder, inst)`
  (use `move!(inst, builder.position)`), and they can be moved to another block or
  function.
- `BasicBlock(pos, name)` creates a block at an insertion point, replacing
  `BasicBlock(bb, name)`, which inserted before `bb`.
- `dbg_declare!`, `dbg_value!` and `dbg_label!` insert debug records (or intrinsics, before
  LLVM 19) at an insertion point, replacing `declare_before!`, `declare_at_end!`,
  `value_before!`, `value_at_end!`, `label_before!` and `label_at_end!`. The end of a block
  is always its literal end, so records can't be inserted after a terminator; before,
  `declare_at_end!` inserted before the terminator and `value_at_end!` after it.

Types, constants and data layouts:

- LLVM types and constants no longer implement Base's collection functions, which
  returned LLVM objects where Julia expects Julia types, and whose results depended on
  LLVM's constant folding. The element type and length of array and vector types are
  `ty.element_type` and `ty.length`, and the element type of a typed pointer is
  `ptrtyp.element_type` (`nothing` for an opaque pointer), replacing `eltype` and
  `length`. `isemptytype(ty)` replaces `isempty(ty)`.
- `c.elements` is a read-only vector of the elements of an aggregate constant: arrays,
  structs and vectors, their simple data variants (`ConstantDataArray` and
  `ConstantDataVector`), and `zeroinitializer`. It replaces indexing, `length`, `size`,
  `eltype` and `collect` on constants, which for a `zeroinitializer` (e.g., what
  `ConstantArray([0, 0, 0])` folds to) had no elements. `LLVM.ConstantAggregate` is public.
- `LLVM.bit_size(dl, ty)` returns the size of a type in bits, replacing `sizeof(dl, ty)`,
  which divided by 8 as a float and threw for `i1`. The size and alignment queries of data
  layouts return `Int`s. `LLVM.element_at` returns, and `LLVM.offsetof` takes, a 1-based
  element index, like the `elements` of the struct type, and they check their arguments.
- `mod.metadata[name]` throws a `KeyError` for missing named metadata instead of creating
  it; use `get!(mod.metadata, name)`, or `get`. The view supports `length`, and `first`
  returns a `name => node` pair.
- `ctx.types` and `engine.functions` only support lookups (`[name]`, `haskey` and `get`),
  since LLVM can't enumerate them; `ctx.types` is no longer an `AbstractDict`.
- Functions that take vectors of IR objects accept any `AbstractVector`, like the views of
  the IR (e.g., `gep!`, `ret!`, `call!` with operand bundles, `ConstantStruct`,
  `const_gep`, `MDNode` and the `DIBuilder` functions), and `clone` accepts any
  `AbstractDict` as its value map.

Enumerations:

- The enums of the C API, which LLVM.jl uses for enum-valued state, are available using
  scoped names, without the common prefix and suffix of their names:
  `LLVM.Linkage.Internal === LLVM.API.LLVMInternalLinkage`, `LLVM.IntPredicate.EQ`,
  `LLVM.Opcode.BitCast`, and `LLVM.Linkage.T === LLVM.API.LLVMLinkage` for the type. These
  modules are public but not part of a vocabulary, and are generated from `LLVM.API`, so
  they contain the values that it defines for the current version of LLVM (including
  backfilled `atomicrmw` operations; see `LLVM.isavailable`). Values of these enums
  are displayed and converted to strings using these names, e.g., `LLVM.Linkage.Internal`
  instead of `LLVMInternalLinkage::LLVMLinkage = 0x00000008` (or `LLVMInternalLinkage`, for
  `string`). `LLVM.DebugEmissionKind` covers the debug info levels of Julia's code
  generator, as used with its `CodegenParams`.

Attributes:

- The kind of an enum, type or constant range attribute is a `Symbol` naming it
  (`attr.kind == :nounwind`) instead of an integer ID, and enum, type and string attributes
  are displayed as the call that creates them (`EnumAttribute(:align, 16)`). Code that
  compared `attr.kind` against an ID from the C API (e.g., from
  `LLVMGetEnumAttributeKindForName`) now silently compares a Symbol against an integer; use
  the keyed operations below instead, or `LLVM.API.LLVMGetEnumAttributeKind(attr)`.
- The constructors of attributes accept Symbols, and reject unknown kinds, kinds that
  belong to another kind of attribute (e.g., `EnumAttribute(:sret)`, which needs a type),
  and values for kinds that don't take one, which used to create invalid attributes.
- Attribute sets can be indexed by kind, using a `Symbol` for LLVM's attribute kinds and a
  string for string attributes: `haskey(f.function_attributes, :nounwind)`,
  `attrs["target-cpu"]`, `get(call.argument_attributes[1], :align, nothing)` and
  `delete!(attrs, :noinline)`.

New functionality:

- `get(mod.functions, name, default)`, and similarly for global variables, aliases and
  ifuncs, looks up a value without throwing. `get!(f, mod.functions, name)` looks up a
  function, or calls `f` to declare it (e.g., using a do-block that also adds attributes),
  like C++'s `Module::getOrInsertFunction`, and `get!(f, mod.globals, name)` does the same
  for global variables.
- Properties for the operands and types of common instructions: `inst.pointer_operand`
  (loads, stores, GEPs, `atomicrmw` and `cmpxchg`), `inst.value_operand` (stores and
  `atomicrmw`), `alloca.allocated_type`, `gep.source_element_type`, `gep.inbounds`,
  `inst.indices` of `extractvalue` and `insertvalue`, `call.called_function` (the function
  that is called directly, or `nothing`), `arg.index`, and `f.intrinsic` (the intrinsic,
  or `nothing`). `call.called_operand` can be assigned to replace the callee.
- `isintrinsic` accepts any value, and optionally the intrinsic to check for, e.g.,
  `isintrinsic(call.called_operand, Intrinsic("llvm.memcpy"))`. `Intrinsic(name)` throws
  for unknown intrinsics, and intrinsics are displayed by name (`Intrinsic("llvm.abs")`)
  instead of by their ID, which differs between versions of LLVM.
- `copy_attributes!(dest, src)` copies the attributes of a function or global variable
  that aren't needed to create it (calling convention, section, function attributes, ...),
  like C++'s `copyAttributesFrom`, e.g., to replace a function by one with a different
  signature.
- `extract_value!` and `insert_value!` accept a vector of indices to access nested
  elements, and check the indices. `exactudiv!` builds an exact unsigned division.
- `comes_before` orders instructions, and `may_read_from_memory`, `may_write_to_memory` and
  `may_have_side_effects` query what they may do. `take_name!(val, from)` transfers a name, and `strip_pointer_casts` and
  `strip_pointer_casts_and_aliases` look through casts and aliases.
- `val.users` is a view of the users of a value, and `remove_dead_constant_users!(c)`
  removes constant expressions that use a constant but are unused themselves.
- `supports_fast_math(inst)` checks whether an instruction can have fast-math flags, which
  for `phi`, `select` and `call` instructions depends on their type.
- `isstring(val)` checks whether a value is a constant string, and `String(str)` returns
  the contents of one.
- `switch.cases` is a mutable view of the cases of a switch instruction, which supports
  adding cases with `push!` and `append!`.
- `verify(f)` reports the verifier's message instead of "broken function", and
  `verification_error` returns the message (or `nothing`) instead of throwing.
- `register_callbacks!(pb, callback)` registers a native pass builder callback, like the
  ones of pass plugins, to use passes implemented in C++ with a `PassBuilder`.
- `LLVM.host_cpu_name()` and `LLVM.host_cpu_features()` return the name and features of
  the host CPU, e.g., to create a `TargetMachine` for it.
- It is documented that the element that was just returned by iterating the views of the
  instructions of a block, the blocks of a function, or the functions and global variables
  of a module can be erased, and that wrappers can be used as keys of a `Dict` directly.

Bug fixes:

- Running a `PassBuilder` with custom passes multiple times no longer uses the
  callbacks, and garbage-collected state, of the first run.
- Array types with 2^32 or more elements can be created, and their `length` is correct
  (on LLVM 17 and later), instead of being truncated to 32 bits.
- The operands of constants other than global values can no longer be changed using the
  `operands` view, which corrupted LLVM's uniquing of constants.
- Integer constants wider than 64 bits are created and converted correctly:
  `ConstantInt(LLVM.IntType(128), Int128(-1))` used to be `2^64-1`, zero threw an
  `InexactError`, and converting went through LLVM's 64-bit getters.
- `ConstantRangeAttribute` checks that its bounds have the right number of words and form a
  valid range, instead of reading out of bounds or failing an assertion in LLVM.
- Strings are passed to LLVM by their number of bytes, so non-ASCII metadata strings, module
  names and flags, named metadata, sync scopes and operand bundle tags aren't truncated.
- The traits of views are defined on their types, so that generic code sees, e.g., that
  `f.parameters` supports linear indexing and that `bb.instructions` contains instructions.
- Custom TTI overrides that are specialized on the argument types of the callbacks, like
  `is_noop_addr_space_cast(::MyTTI, ::UInt, ::UInt)`, are no longer silently ignored, and
  operand lists that don't fit the C API's buffer are reported instead of truncated.
- `replace_metadata_uses!` replaces by values of another type directly on LLVM 18+, and no
  longer loops forever on older versions when the new value isn't a global value.
- `PassBuilder` no longer leaks its options when given an invalid keyword argument.
- `unsafe_store!` on `Core.LLVMPtr` returns the pointer, like Base.
- `erase!` on an instruction or basic block that isn't part of a block or function, and
  `clone(bb; dest=nothing)` on LLVM 18 and later, no longer crash.
- Moving basic blocks (now using `move!`) works for detached blocks, which crashed, and
  before a block of another function, which corrupted the IR: the block was listed in the
  other function, but kept its old parent.

Other changes:

- Mutating methods on views, like `push!` on attribute sets or `setindex!` on metadata,
  return the view, like Base's collections do, instead of `nothing`.
- `f.blocks` no longer caches the blocks of the function, which made it return stale blocks
  after blocks were added or removed.
- Attribute sets support `append!` as documented, and they, the metadata of an instruction
  and the flags of a module can be iterated.
- Property access on values whose concrete type is only known at run time doesn't dispatch.


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
- The C API wrappers and instruction builders are no longer compiled for every combination
  of value types they are called with, which reduces the latency of generating IR.
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
