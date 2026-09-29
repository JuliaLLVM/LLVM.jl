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

LLVM's API uses many common words, like `functions`, `add!`, `lookup` or `Context`, which
would clash with other packages if they were all exported. That's why `using LLVM` only
brings the `@dispose` macro into scope. The rest of the API is public, and can be used
qualified, e.g., `LLVM.functions(mod)`, or brought into scope by opting into one or more
vocabularies:

| Vocabulary    | Contents                                                                  |
|:------------- |:------------------------------------------------------------------------- |
| `LLVM.IR`     | contexts, modules, values, types, metadata and debug info, and functions to traverse and modify them (`functions`, `blocks`, `instructions`, `operands`, `uses`, ...) |
| `LLVM.Build`  | the `IRBuilder` and its instruction-building functions (`add!`, `load!`, `call!`, `ret!`, ...), constant expressions, and the `DIBuilder` |
| `LLVM.Passes` | pass builders and managers, passes like `InstCombinePass`, and pipeline callbacks |
| `LLVM.ORC`    | the ORC just-in-time compiler: `LLJIT`, JIT dylibs, thread-safe modules, ... |

Code that mainly works with LLVM, like a compiler, typically opts into the vocabularies it
needs and uses their names unqualified:

```julia
using LLVM, LLVM.IR, LLVM.Build

for f in functions(mod), bb in blocks(f), inst in instructions(bb)
    # ...
end
```

Code that only occasionally uses LLVM.jl, or that combines it with other packages using
the same words (e.g., `mul!` from LinearAlgebra), can instead qualify the names,
`LLVM.mul!(builder, lhs, rhs)`, or import specific ones, `using LLVM: functions, mul!`.

Some functionality is not part of any vocabulary, and is always used qualified: target
initialization, targets, target machines and data layouts (`LLVM.TargetMachine`), and the
legacy execution engines (`LLVM.JIT`).

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
julia> buf = MemoryBuffer(UInt8[]);

julia> dispose(buf)
julia> dispose(buf)
WARNING: An instance of MemoryBuffer is being disposed twice.
```

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
number of a debug location, are available as properties:

```jldoctest properties
julia> mod = LLVM.Module("SomeModule");

julia> mod.triple = "x86_64-unknown-linux-gnu";

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "counter");

julia> gv.initializer = ConstantInt(Int32(0));

julia> gv.linkage = LLVM.API.LLVMInternalLinkage;

julia> gv.name, gv.linkage
("counter", LLVM.API.LLVMInternalLinkage)

julia> gv
@counter = internal global i32 0
```

Use `propertynames`, or tab completion in the REPL, to discover which properties an object
has. Properties that cannot be changed, like `value_type`, throw an error when assigned to.

Properties are the only public way to access these attributes. To pass a property to a
higher-order function, use an anonymous function:

```jldoctest properties
julia> map(gv -> gv.name, globals(mod))
1-element Vector{String}:
 "counter"
```

The docstring of each type lists its properties in a "Properties" section, with
signatures like `gv.linkage`, followed by `gv.linkage = linkage` for properties that can
be assigned to. The documentation is available in the REPL too, e.g., using
`?LLVM.GlobalVariable`.

Properties expose named characteristics and distinguished relationships of an object: its
name, its linkage, its initializer, the block it is part of, the terminator of a block,
etc. Functions are used to test conditions, to access and traverse collections, for lookups
that take a key or other arguments, and for operations that modify the IR:

| Kind                         | Examples                                                                 |
|:---------------------------- |:------------------------------------------------------------------------ |
| properties                   | `f.name`, `gv.linkage = ...`, `inst.parent.parent`, `bb.terminator`, `f.entry`, `loc.line` |
| predicates                   | `isdeclaration(f)`, `isvolatile(inst)`, with setters like `volatile!(inst, true)` |
| collections and traversal    | `functions(mod)`, `blocks(f)`, `operands(inst)`, `uses(val)`, `nextinst(inst)` |
| keyed and parameterized lookups | `metadata(inst)[kind]`, `module_flags(mod)[key]`, `LLVM.overloaded_name(intrinsic, types)` |

Predicates are functions so that they can be passed to higher-order functions, e.g.,
`filter(isdeclaration, functions(mod))`.

Reading a property retrieves information that is attached to the object. It may perform a
lookup or convert LLVM's representation (e.g., copying a string), but it does not run an
analysis, traverse the IR, or construct a collection of IR objects.

This mostly corresponds to LLVM's C++ API, which makes it easy to port code: C++ getters and
setters like `F->getName()`/`F->setName(...)` and `I->getParent()` become properties,
`GV->isThreadLocal()`/`GV->setThreadLocal(true)` become `isthreadlocal(gv)` and
`threadlocal!(gv, true)`, and iteration like `for (auto &I : BB)` becomes
`for inst in instructions(bb)`.
