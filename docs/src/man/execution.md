# Execution

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
      define i64 @"add"(i64 %0, i64 %1) {
      top:
        %2 = add i64 %1, %0
        ret i64 %2
      }"""
    mod = parse(LLVM.Module, ir)
    add = only(functions(mod))
end
```

If instead of compiling the LLVM module to native code, you just want to execute it, LLVM
offers different mechanisms to do so. We'll be using the following LLVM IR code for the
examples in this section:

```jldoctest
julia> add
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}
```


## Interpreter

LLVM's interpreter is a simple way to execute LLVM IR code, and can be constructed from just
a module. Executing code is done using the `run` function, which takes a reference to the
function to execute, and an array of `GenericValue` arguments, returning a `GenericValue`
result:

```jldoctest
julia> engine = Interpreter(mod);

julia> res = run(engine, add, [GenericValue(LLVM.Int64Type(), 1),
                               GenericValue(LLVM.Int64Type(), 2)]);

julia> convert(Int, res)
3
```

After having constructed an engine, more modules can be added to it using `push!`, and
removed from it using `delete!`.


## MCJIT

Interpreting IR is obviously slow, so for all but the simplest programs you'll want to use
the JIT engine instead, which is based on LLVM's MCJIT. Usage of the JIT engine is almost
identical to the interpreter, using `JIT` objects instead.

One crucial difference is that MCJIT does not support the `run` function with arguments.
Instead, you need to look up the address of the compiled function, and call it directly:

```jldoctest
julia> engine = JIT(mod);

julia> addr = lookup(engine, "add");

julia> res = ccall(addr, Int64, (Int64, Int64), 1, 2)
3
```


## ORCJIT

The ORCJIT engine is a more modern JIT engine, which is more flexible and powerful than
MCJIT. LLVM.jl only supports the ORCv2 API, which is the latest version of ORCJIT.

!!! warning

    Documentation for ORCJIT is a work in progress.

### Thread-safe operation

```@meta
DocTestSetup = quote
    using LLVM

    # XXX: clean-up previous contexts
    while ts_context(; throw_error=false) !== nothing
        dispose(ts_context())
    end
end
```

Because of ORC often compiling code lazily, thread-safety is a concern. To ensure that
everything is thread-safe, the ORC APIs use thread-safe wrappers of LLVM objects like
contexts and modules.

Thread safe contexts are similar to regular contexts, but they require taking a lock
when using them. On the Julia side, they are created much like regular contexts:

```jldoctest
julia> ts_ctx = ThreadSafeContext()
ThreadSafeContext(Ptr{LLVM.API.LLVMOrcOpaqueThreadSafeContext}(0x0000600001fac180))

julia> dispose(ts_ctx)
```

LLVM.jl also maintains a task-bound thread-safe context, simplifying API usage much in the
same way as regular contexts:

```
julia> ts_context()
ERROR: No LLVM thread-safe context is active

julia> ts_ctx = ThreadSafeContext();

julia> ts_context()
ThreadSafeContext(Ptr{LLVM.API.LLVMOrcOpaqueThreadSafeContext} @0x00006000035844a0)

julia> dispose(ts_ctx)

julia> ts_context()
ERROR: No LLVM thread-safe context is active
```

```@meta
DocTestSetup = quote
    using LLVM

    if ts_context(; throw_error=false) === nothing
        ThreadSafeContext()
    end
end
```

Similarly, `ThreadSafeModule` is a thread-safe wrapper around a module, and can be used
much like a regular module:

```jldoctest
julia> ts_mod = ThreadSafeModule("SomeModule")
ThreadSafeModule(Ptr{LLVM.API.LLVMOrcOpaqueThreadSafeModule}(0x0000600001d893a0))

julia> dispose(ts_mod)
```

Whereas thread-safe contexts are just for use with LLVM C APIs, it is possible to access
the underlying module from a thread-safe module (taking a lock in the process):

```jldoctest
julia> ts_mod = ThreadSafeModule("SomeModule");

julia> ts_mod() do mod
            string(mod)
        end
"; ModuleID = 'SomeModule'\nsource_filename = \"SomeModule\"\n"
```
