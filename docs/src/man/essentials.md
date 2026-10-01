# Essentials

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC
end
```

After importing LLVM.jl, the packages is ready to use. A simple test to check if the package
is working correctly is to query the version of the LLVM library:

```julia-repl
julia> LLVM.version()
v"15.0.7"
```

Some back-end functionality may require explicit initialization, for which there are
specific functions (replacing `*` with the back-end name):

- `LLVM.Initialize*AsmParser`: initialize the assembly parser;
- `LLVM.Initialize*AsmPrinter`: initialize the assembly printer;
- `LLVM.Initialize*Disassembler`: initialize the disassembler;
- `LLVM.Initialize*TargetInfo`: initialize the target, allowing inspection;
- `LLVM.Initialize*Target`: initialize the target, allowing use;
- `LLVM.Initialize*TargetMC`: initialize the target machine code generation.

These functions are only available for the back-ends that are enabled in the LLVM library:

```julia-repl
julia> LLVM.backends()
4-element Vector{Symbol}:
 :AArch64
 :AVR
 :BPF
 :WebAssembly
```

Special versions of these functions are available to initialize all available targets,
e.g., `LLVM.InitializeAllTargetInfos`, or to initialize the native target, e.g.,
`LLVM.InitializeNativeTarget`.


## Vocabularies

LLVM's API uses many common words, like `verify`, `add!`, `lookup` or `Context`, which
would clash with other packages if they were all exported. That's why `using LLVM` only
brings the `@dispose` macro into scope. The rest of the API is public, and can be used
qualified, e.g., `LLVM.isdeclaration(f)`, or brought into scope by opting into one or more
vocabularies:

| Vocabulary    | Contents                                                                  |
|:------------- |:------------------------------------------------------------------------- |
| `LLVM.IR`     | contexts, modules, values, types, metadata and debug info, and functions to inspect and modify them (`isdeclaration`, `erase!`, `replace_uses!`, `verify`, ...) |
| `LLVM.Build`  | the `IRBuilder` and its instruction-building functions (`add!`, `load!`, `call!`, `ret!`, ...), constant expressions, and the `DIBuilder` |
| `LLVM.Passes` | pass builders and managers, passes like `InstCombinePass`, and pipeline callbacks |
| `LLVM.ORC`    | the ORC just-in-time compiler: `LLJIT`, JIT dylibs, thread-safe modules, ... |

Code that mainly works with LLVM, like a compiler, typically opts into the vocabularies it
needs and uses their names unqualified:

```julia
using LLVM, LLVM.IR, LLVM.Build

for f in mod.functions
    isdeclaration(f) && continue
    for bb in f.blocks, inst in bb.instructions
        # ...
    end
end
```

Code that only occasionally uses LLVM.jl, or that combines it with other packages using
the same words (e.g., `mul!` from LinearAlgebra), can instead qualify the names,
`LLVM.mul!(builder, lhs, rhs)`, or import specific ones, `using LLVM: isdeclaration, mul!`.

Some functionality is not part of any vocabulary, and is always used qualified: target
initialization, targets, target machines and data layouts (`LLVM.TargetMachine`), and the
legacy execution engines (`LLVM.JIT`).

Finally, the `LLVM.Interop` submodule contains functionality to integrate with Julia's code
generator, like generating `llvmcall`s or inline assembly, and Julia's own LLVM passes.
It is imported the same way, `using LLVM.Interop`, but unlike the vocabularies, which bring
LLVM.jl's own functionality into scope, it builds on top of LLVM.jl. See [Julia
integration](@ref man-interop) for more details.

The examples in this documentation assume all vocabularies have been imported.


## Contexts

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    # XXX: clean-up previous contexts
    while context(; throw_error=false) !== nothing
        dispose(context())
    end
end
```

Most operations in LLVM require a context to be active. In LLVM.jl, LLVM contexts are
available as `Context` objects, and you are expected to create a context before creating any
other LLVM objects.

To create a new LLVM context, use the `Context()` constructor:

```jldoctest
julia> ctx = Context()
LLVM.Context(0x0000600001f95470)

julia> dispose(ctx) # see next section
```

Although many LLVM APIs expect a context as an argument, LLVM.jl automatically manages the
context in the task-local state. The current task-local context is accessible via the
`context()` function, and is automatically used by most LLVM.jl functions when an API
requires an LLVM context. To populate the task-local context, it is sufficient to create a
new context object:

```jldoctest
julia> context()
ERROR: No LLVM context is active

julia> ctx = Context();

julia> context()
LLVM.Context(0x0000600001fa07e0)
```

Although the context is automatically managed by LLVM.jl, it is still important to keep
track of the context object for proper disposal after use. This also wipes the task-local
context:

```jldoctest
julia> ctx = Context()
LLVM.Context(0x0000600001fb8fb0)

julia> dispose(ctx)

julia> context()
ERROR: No LLVM context is active
```


## Memory management

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

LLVM.jl does not use automatic memory management for LLVM objects[^1], and instead relies on
manual disposal of resources by calling the `dispose` method. For example, to create and
dispose of a module object:

[^1]: See [this issue](https://github.com/JuliaLLVM/LLVM.jl/pull/309) for more details.

```jldoctest
julia> mod = LLVM.Module("MyModule");

julia> dispose(mod)
```

After calling `dispose`, the object is no longer valid and should not be used. Doing so
will often result in hard crashes:

```julia-repl
julia> mod
[94707] signal (11.2): Segmentation fault: 11
```

Most LLVM.jl objects, like modules, values, types and metadata, are lightweight wrappers
around a pointer to the LLVM object. Wrappers of the same object are equal (both `==` and
`===`) and have the same hash, so they can be compared, and used as keys of a `Dict` or as
elements of a `Set`, without converting them to a pointer. This is object identity: two
instructions that compute the same thing are different objects, while changing an object
does not change its identity. Wrappers do not keep the object alive, so they become invalid
when the object is disposed of or erased.

### Scoped disposal

For convenience, many of these objects can be created and disposed using do-block variants
of their constructors. This makes it harder to use the object outside of its lifetime, and
also handles exceptions that might occur during the object's construction:

```jldoctest
julia> LLVM.Module("MyModule") do mod
         # use mod
       end
```

This pattern is useful, but can become cumbersome when working with multiple objects that
need to be disposed of. In addition, the function closures constructed here can have an
impact on performance. To address these issues, LLVM.jl provides a [`@dispose`](@ref) macro
that conveniently disposes of multiple objects at once:

```jldoctest
julia> @dispose ctx=Context() mod=LLVM.Module("jit") begin
         # mod and ctx are automatically disposed of after this block
       end
```

It is recommended to use the [`@dispose`](@ref) macro whenever possible.

### Disposal during exception handling

When a context is disposed of while an exception is being thrown -- for example, when an
error occurs inside a `Context() do` block, whose implicit `dispose` call then runs during
stack unwinding -- the context is popped from the context stack but intentionally leaked
instead of freed. This ensures that LLVM objects captured by the exception, or by test
machinery recording it, remain valid when they are displayed later, e.g., as part of a test
summary. Without this, reporting such errors would crash the process:

```jldoctest
julia> val = Ref{Any}();

julia> try
         @dispose ctx=Context() begin
           val[] = LLVM.ConstantInt(Int32(42))
           error("something went wrong")
           # the implicit `dispose(ctx)` does not free the context here
         end
       catch
       end

julia> string(val[]) # safe, even though the context was disposed of
"i32 42"
```

### Debugging missing disposals

To ensure that all resources are properly disposed of, LLVM.jl provides functionality to
track the creation and disposal of objects. This can be enabled by setting the `memcheck`
preference in `LocalPreferences` to `true`.

When enabled, LLVM.jl will warn when using an object after it has been disposed of:

```julia-repl
julia> ctx = Context();

julia> dispose(ctx)

julia> ctx
WARNING: An instance of Context is being used after it was disposed.
```

The package will also warn about erroneous disposals, whether it's disposing an unknown
object, or disposing an object that has already been disposed of:

```julia-repl
julia> builder = IRBuilder();

julia> dispose(builder)
julia> dispose(builder)
WARNING: An instance of IRBuilder is being disposed of twice.
```

An object that is disposed of twice is only reported, and not disposed of again.

```julia-repl
julia> dispose(MemoryBuffer(LLVM.API.LLVMMemoryBufferRef(1)))
WARNING: An unknown instance of MemoryBuffer is being disposed of.
```

Finally, when not properly disposing of an object, LLVM.jl will warn about the leaked
object when the process exits:

```julia-repl
julia> ctx = Context();

julia> exit()
WARNING: An instance of Context was not properly disposed of.
```

Each warning includes the backtraces of where the object was allocated, disposed of, and
used. To keep the output manageable, a problem is only reported in full the first time it
occurs for objects that were allocated and disposed of at the same locations in user code
(the first location outside of LLVM.jl and Julia's Base library in each backtrace). Later
occurrences are counted by where they happen, an update is printed when a problem occurred
10, 100, 1000, ... times, and all repeated problems are summarized when the
process exits, and objects that leaked from the same location are reported together:

```julia-repl
julia> for i in 1:10
           LLVM.MemoryBuffer(UInt8[])
       end

julia> exit()
WARNING: 10 instances of MemoryBuffer were not properly disposed of.
They were allocated at the same location, e.g.:
...
```


## Properties

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Attributes of LLVM objects, like the name of a value, the linkage of a global, or the line
number of a debug location, are available as properties, as are their relationships to other
objects and their contents:

```jldoctest properties
julia> mod = LLVM.Module("SomeModule");

julia> mod.triple = "x86_64-unknown-linux-gnu";

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "counter");

julia> gv.initializer = ConstantInt(Int32(0));

julia> gv.linkage = LLVM.Linkage.Internal;

julia> gv.name, gv.linkage
("counter", LLVM.Linkage.Internal)

julia> gv
@counter = internal global i32 0
```

Use `propertynames`, or tab completion in the REPL, to discover which properties an object
has. Properties that cannot be changed, like `value_type`, throw an error when assigned to.

Properties are the only public way to access these attributes. To pass a property to a
higher-order function, use an anonymous function:

```jldoctest properties
julia> map(gv -> gv.name, mod.globals)
1-element Vector{String}:
 "counter"
```

The docstring of each type lists its properties in a "Properties" section, with
signatures like `gv.linkage`, followed by `gv.linkage = linkage` for properties that can
be assigned to. The documentation is available in the REPL too, e.g., using
`?LLVM.GlobalVariable`.

Properties expose what an object has: its characteristics (its name, its linkage, whether it
is constant), its relationships to other objects (the block an instruction is part of, the
terminator of a block, the next instruction), and its contents (the functions of a module,
the operands of an instruction). Flags that can be set or cleared are `Bool` properties
named without an `is` or `has` prefix, like `gv.constant` or `inst.volatile`. Functions are
used to ask questions that cannot be assigned an answer (predicates), to compute something
from additional arguments, and to act: operations that modify the IR, builders and
constructors.

| Kind            | Examples                                                                 |
|:--------------- |:------------------------------------------------------------------------ |
| characteristics | `f.name`, `gv.linkage = ...`, `inst.opcode`, `loc.line`                  |
| flags           | `gv.constant = true`, `inst.volatile`, `inst.nuw = false`, `call.tailcall` |
| relationships   | `inst.parent.parent`, `bb.terminator`, `f.entry`, `inst.next`, `bb.prev` |
| contents        | `mod.functions`, `f.blocks`, `bb.instructions`, `inst.operands`, `val.uses` |
| views of state  | `inst.fast_math.nnan = true`, `f.memory_effects[:argmem] = :read`        |
| predicates      | `isdeclaration(f)`, `isvararg(ft)`, `isterminator(inst)`, `isconstant(val)` |
| computations    | `LLVM.overloaded_name(intrinsic, types)`, `dominates(tree, a, b)`        |
| operations      | `erase!(inst)`, `replace_uses!(old, new)`, `move!(f, LLVM.before(g))`, `elements!(st, elems)` |

Predicates are functions so that they can be passed to higher-order functions, e.g.,
`filter(isdeclaration, mod.functions)`. Relationships that can be absent are `nothing`,
e.g., the `next` instruction of the last instruction in a block, or the `entry` of a
function without a body.

### Collections

The contents of an object are properties that return a view: a live window onto the IR,
rather than a copy. Reading from a view queries the IR object, so it reflects changes that
are made after the view was created:

```jldoctest properties
julia> fn = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()));

julia> bbs = fn.blocks;

julia> isempty(bbs)
true

julia> BasicBlock(fn, "entry");

julia> map(bb -> bb.name, bbs)
1-element Vector{String}:
 "entry"
```

Views support the operations that LLVM supports on the underlying collection. They are
mutable where the collection can be modified in place, e.g., the operands of an instruction
or metadata node (`inst.operands[i] = val`), the successors of a terminator, the attributes
of a function (`push!`, `delete!`), or the module-level inline assembly (`push!`,
`empty!`). Collections that cannot be modified directly are read-only views, and mutating
them throws an error: e.g., the parameter types of a function type (types are immutable),
the predecessors of a block (which are derived from the uses of the block), or the blocks
of a function and the instructions of a block (which are added by creating them, and
removed with operations like `erase!`). Assigning to the property itself is not supported.

Keyed lookups are indexing operations on these views, e.g., `mod.functions["name"]`,
`inst.metadata["tbaa"]` or `mod.flags[key]`, while collections that are indexed by
position, like the attributes of each parameter, are vectors of views:
`f.parameter_attributes[i]`. Some views, like the blocks of a function, can be indexed even
though LLVM stores their elements in a linked list, which makes indexing linear in the
position of the element; iterate the view instead of indexing it in a loop. To get a copy
that doesn't change along with the IR, use `collect`.

Because views reflect changes to the IR, changing a collection while iterating over it
requires care. The views of linked lists, like the instructions of a block, the blocks of a
function, or the functions and global variables of a module, look up the next element
before returning the current one, so it is safe to remove or erase the element that was
just returned (and only that element):

```julia
for inst in bb.instructions
    if inst isa LLVM.CallInst && inst.called_function == f
        erase!(inst)
    end
end
```

Similarly, the use that was just returned when iterating over the `uses` of a value can be
replaced, or its user erased, as long as that doesn't also remove the next use (e.g., when
the user uses the value multiple times). In other cases, `collect` the view first, and
iterate over the copy.

### Views of richer state

Some state is richer than a single value, like the fast-math flags of an instruction or the
memory effects of a function. The corresponding properties return a view object that is
bound to the IR object: reading from the view queries the IR object, while assigning to one
of its properties or indices modifies it in place. Assigning to the property itself replaces
the state as a whole, e.g., `inst.fast_math = (; nnan=true)` clears all other flags. When
enum-valued state has a common yes/no question, both are available as properties that are
views of the same state, e.g., `gv.threadlocal_mode` and `gv.threadlocal`.

### Cost

Reading a property retrieves information that is attached to the object. It may perform a
lookup, convert LLVM's representation (e.g., copying a string), or create a view, but it
does not run an analysis, traverse the IR, or construct a collection of IR objects. Such
work only happens when using the view, e.g., when iterating the predecessors of a block.

This mostly corresponds to LLVM's C++ API, which makes it easy to port code: C++ getters and
setters like `F->getName()`/`F->setName(...)` and `I->getParent()` become properties, as
do flags like `GV->isThreadLocal()`/`GV->setThreadLocal(true)`, which become
`gv.threadlocal` and `gv.threadlocal = true`, collections like `M.functions()` and
`I->operands()` become `mod.functions` and `inst.operands`, navigation like
`I->getNextNode()` becomes `inst.next`, and iteration like `for (auto &I : BB)` becomes
`for inst in bb.instructions`.


## Enumerations

LLVM's C API defines enumerations for things like the linkage of a global value, the
predicate of a comparison, or the opcode of an instruction. LLVM.jl uses these values
directly, as returned by properties like `gv.linkage` or taken by functions like `icmp!`.
They are available as `LLVM.API.LLVMInternalLinkage`, but also with a shorter name, in a
module per enumeration:

```jldoctest
julia> LLVM.Linkage.Internal
LLVM.Linkage.Internal

julia> LLVM.Linkage.Internal === LLVM.API.LLVMInternalLinkage
true

julia> LLVM.IntPredicate.EQ, LLVM.Opcode.BitCast, LLVM.AtomicOrdering.Acquire
(LLVM.IntPredicate.EQ, LLVM.Opcode.BitCast, LLVM.AtomicOrdering.Acquire)
```

These names are those of the C API, without their common prefix and suffix. The modules
contain the values that `LLVM.API` defines for the current version of LLVM (including
values that LLVM.jl backfills, like newer `atomicrmw` operations, whose availability can be
checked with `LLVM.isavailable`), and `T`, their type (e.g., `LLVM.Linkage.T ===
LLVM.API.LLVMLinkage`), to use in type annotations or with functions like
`parse(LLVM.AtomicOrdering.T, "acquire")`. The modules are public, but not
part of any vocabulary, so they are always qualified by default. To use them unqualified,
import them explicitly: `using LLVM: Linkage, IntPredicate`.

