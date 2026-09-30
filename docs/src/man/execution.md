# Execution

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

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
    add = only(mod.functions)
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
result. These legacy execution engines are not part of any vocabulary, so they are used
qualified:

```jldoctest
julia> engine = LLVM.Interpreter(mod);

julia> res = run(engine, add, [LLVM.GenericValue(LLVM.Int64Type(), 1),
                               LLVM.GenericValue(LLVM.Int64Type(), 2)]);

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
julia> engine = LLVM.JIT(mod);

julia> addr = LLVM.lookup(engine, "add");

julia> res = ccall(addr, Int64, (Int64, Int64), 1, 2)
3
```


## ORC

ORC is LLVM's modern JIT framework, and the recommended way to execute LLVM IR. LLVM.jl
supports LLJIT, a ready-to-use JIT built on ORC, as well as adding code to Julia's own JIT.
Its functionality is available in the `LLVM.ORC` vocabulary (`using LLVM.ORC`).

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    # XXX: clean-up previous contexts
    while ts_context(; throw_error=false) !== nothing
        dispose(ts_context())
    end
end
```

### Thread-safe contexts and modules

Because ORC can compile code lazily, possibly on other threads, it works with thread-safe
wrappers of LLVM contexts and modules. A `ThreadSafeContext` is created much like a
regular context, and similarly becomes the task's active thread-safe context:

```jldoctest
julia> ts_ctx = ThreadSafeContext();

julia> ts_context() == ts_ctx
true

julia> dispose(ts_ctx)

julia> ts_context(; throw_error=false) === nothing
true
```

A `ThreadSafeModule` wraps a module in the active thread-safe context. To access the module,
call the thread-safe module with a function, which locks the context while the function
runs:

```jldoctest
julia> @dispose ts_ctx=ThreadSafeContext() begin
           ts_mod = ThreadSafeModule("SomeModule")
           ts_mod() do mod
               string(mod)
           end
       end
"; ModuleID = 'SomeModule'\nsource_filename = \"SomeModule\"\n"
```

Only access modules and contexts in this way: using the underlying context directly, e.g.,
through `context(ts_ctx)`, bypasses the lock.

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if ts_context(; throw_error=false) === nothing
        ThreadSafeContext()
    end
end
```

### Compiling and running code

An `LLJIT` compiles code for the host by default. After adding a module to one of its
JITDylibs, look up a symbol to compile it and get its address:

```jldoctest orc
julia> lljit = LLJIT();

julia> ts_mod = ThreadSafeModule("jit");

julia> ts_mod() do mod
           mod.triple = lljit.triple
           ft = LLVM.FunctionType(LLVM.Int64Type(), [LLVM.Int64Type(), LLVM.Int64Type()])
           fn = LLVM.Function(mod, "add", ft)
           @dispose builder=IRBuilder() begin
               position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
               ret!(builder, add!(builder, fn.parameters...))
           end
           return
       end

julia> jd = lljit.main_dylib;

julia> add!(lljit, jd, ts_mod)

julia> addr = lookup(lljit, "add");

julia> ccall(pointer(addr), Int64, (Int64, Int64), 1, 2)
3
```

Adding a module consumes it. The JIT, and all code it compiled, stays alive until it is
disposed of, which should only happen once its code is not used anymore. Alternatively, use
the do-block form `LLJIT() do lljit ... end`, or `@dispose lljit=LLJIT() begin ... end`.

To customize the JIT, e.g., to use a different object linking layer, create it from an
`LLJITBuilder`.

### JITDylibs and symbols

Code is added to JITDylibs, which are the JIT's equivalent of dynamic libraries. Every
`LLJIT` has a main JITDylib, `lljit.main_dylib`, which `lookup(lljit, name)` searches. More
JITDylibs can be created in the JIT's execution session, and searched explicitly:

```jldoctest orc
julia> es = lljit.execution_session;

julia> other = JITDylib(es, "other");

julia> lookup_dylib(es, "other") == other
true

julia> lookup(lljit, other, "add")
ERROR: LLVM error: Symbols not found: [ add ]
```

Internally, ORC identifies symbols by their linker-mangled names, e.g., with an underscore
prepended on macOS. `lookup` takes care of this, but other APIs take symbols created by
`mangle`, which applies the target's mangling and interns the result in the execution
session:

```jldoctest orc
julia> sym = mangle(lljit, "add");

julia> String(sym) in ("add", "_add")
true
```

Symbols are reference counted. `mangle` returns a new reference, which most APIs that take
symbols take ownership of. Otherwise, release it:

```jldoctest orc
julia> release(sym)
```

### Making host symbols available

A JITDylib does not see any symbols from the host process by default. To call host
functions or access host data, define them as absolute symbols:

```jldoctest orc
julia> counter = Ref(41);

julia> define!(jd, absolute_symbols(
           mangle(lljit, "counter") => pointer_from_objref(counter)))

julia> pointer(lookup(lljit, "counter")) == pointer_from_objref(counter)
true
```

Alternatively, attach a definition generator to the JITDylib, which is consulted whenever a
symbol cannot be found. `DynamicLibrarySearchGenerator` makes all symbols of the current
process, or of a specific library, available:

```jldoctest orc
julia> add!(jd, DynamicLibrarySearchGenerator(lljit))

julia> lookup(lljit, "jl_apply_generic");
```

For other policies, `CustomDefinitionGenerator` calls a Julia function with the symbols
that could not be found, which can then define them:

```jldoctest orc
julia> answer = Ref(42);

julia> dg = CustomDefinitionGenerator() do kind, jd, jd_flags, lookup_set
           for (name, flags) in lookup_set
               if String(name) in ("answer", "_answer")
                   retain(name)   # the lookup set's names are borrowed
                   define!(jd, absolute_symbols(name => pointer_from_objref(answer)))
               end
           end
       end;

julia> add!(jd, dg)

julia> pointer(lookup(lljit, "answer")) == pointer_from_objref(answer)
true
```

### Removing code

Code can be removed from a JITDylib by clearing it with `empty!`, or selectively, by
adding it using a resource tracker:

```jldoctest orc
julia> rt = ResourceTracker(jd);

julia> ts_mod = ThreadSafeModule("jit");

julia> ts_mod() do mod
           fn = LLVM.Function(mod, "temporary", LLVM.FunctionType(LLVM.VoidType()))
           @dispose builder=IRBuilder() begin
               position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
               ret!(builder)
           end
           return
       end

julia> add!(lljit, rt, ts_mod)

julia> lookup(lljit, "temporary");

julia> remove!(rt)

julia> lookup(lljit, "temporary")
ERROR: LLVM error: Symbols not found: [ temporary ]
```

Code that is added without a tracker is tracked by the JITDylib's default tracker,
`jd.default_resource_tracker`. Resource trackers are reference counted too; `dispose`
releases the reference, without removing the tracked code:

```jldoctest orc
julia> dispose(rt)

julia> dispose(lljit)
```

### Lazy compilation

Instead of adding code upfront, a materialization unit can promise to define symbols, and
only generate code when one of them is looked up. `CustomMaterializationUnit` calls a
Julia function to do so, which typically generates a module and emits it through one of the
JIT's layers:

```julia
flags = SymbolFlags(callable=true)
mu = CustomMaterializationUnit("lazy", [mangle(lljit, "foo") => flags],
    function materialize(mr)
        ts_mod = ThreadSafeModule("foo")
        ts_mod() do mod
            # generate IR defining `foo`
        end
        emit!(lljit.ir_transform_layer, mr, ts_mod)
    end,
    function discard(jd, sym)
        # `sym` was overridden before being materialized
    end)
define!(jd, mu)
```

Looking up `foo` then materializes it. When a unit defines multiple symbols, the
`requested_symbols` property of the materialization responsibility tells which of them were
looked up.

To defer compilation even further, until a function is first *called*, create a lazy
reexport. Looking it up returns the address of a stub, which calls into the JIT to look up
(and thus materialize) the target the first time it is called:

```julia
es = lljit.execution_session
lctm = LocalLazyCallThroughManager(lljit.triple, es)
ism = LocalIndirectStubsManager(lljit.triple)
define!(jd, lazy_reexports(lctm, ism, jd,
                          [mangle(lljit, "foo_stub") => mangle(lljit, "foo")]))
addr = lookup(lljit, "foo_stub")    # doesn't materialize `foo` yet
```

Both managers need to stay alive for as long as the stubs can be called, and need to be
disposed of afterwards.

Finally, to process all modules before they are compiled, e.g., to optimize them, install a
transformation on the JIT's IR transform layer:

```julia
transform!(lljit.ir_transform_layer) do tsm, mr
    tsm() do mod
        run!("default<O2>", mod)
    end
end
```

### Errors in callbacks

Julia exceptions cannot propagate through LLVM. When a callback like a materializer,
definition generator or transformation throws, the operation that triggered it fails with a
generic `LLVMException`. The original exception is kept, and can be rethrown as a
`LLVM.CallbackException` by calling `check_callback_error!` on the object that owns the
callback.

### Julia's JIT

Code can also be added to Julia's own JIT, using `JuliaOJIT()`, e.g., to make it callable
from Julia code. Its API is similar to that of `LLJIT`, but lookups always need an explicit
JITDylib: `lookup(jljit, jd, name)`.

How JITDylibs work depends on the Julia version: on Julia 1.14 and later,
`JITDylib(jljit, name)` creates a new JITDylib, which can see Julia's symbols but is not
visible to other code. On older versions, it returns a single JITDylib that is shared by
all users of Julia's JIT, and whose symbols are visible to Julia code.
