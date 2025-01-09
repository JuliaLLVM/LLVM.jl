# Essentials

```@meta
DocTestSetup = quote
    using LLVM
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


## Contexts

```@meta
DocTestSetup = quote
    using LLVM

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
LLVM.Context(0x0000600003d18980, typed ptrs)

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
LLVM.Context(0x0000600000ae1470, typed ptrs)
```

Although the context is automatically managed by LLVM.jl, it is still important to keep
track of the context object for proper disposal after use. This also wipes the task-local
context:

```jldoctest
julia> ctx = Context()
LLVM.Context(0x000060000007c4b0, typed ptrs)

julia> dispose(ctx)

julia> context()
ERROR: No LLVM context is active
```


## Memory management

```@meta
DocTestSetup = quote
    using LLVM

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
