# Transforms

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
end
```

LLVM contains a variety of passes that can be used to transform IR. With LLVM.jl, it is
also possible to write your own passes in Julia. In this section, we will demonstrate
the new pass manager interface; the legacy pass manager is being deprecated, and not
recommended for new code.


## Pass builders

The core abstraction for running passes is the `NewPMPassBuilder` object, which aggregates
passes via the `add!` function and runs them over a function or module using the `run!`
function:

```jldoctest
julia> @dispose pb=NewPMPassBuilder() begin
         add!(pb, "loop-unroll")
         run!(pb, mod)
       end
```

When only running a single pass or pipeline, it is possible to bypass the construction
of the pass builder and directly use the `run!` function:

```jldoctest
julia> run!("loop-unroll", mod)
```

Pass builders also support a number of keyword argument, mostly for debugging purposes.
Refer to the `NewPMPassBuilder` docstring for more details.


## Passes

In LLVM's new pass manager, passes are simply strings, as shown above. In LLVM.jl, we also
expose objects for each pass, which helps to avoid typos in pass names, and simplifies
passing arguments to the pass.

The `loop-unroll` pass from above, for example, can also be constructed using the
`LoopUnrollPass` object, which simplifies setting options for the pass:

```jldoctest
julia> run!(LoopUnrollPass(; allow_partial=true), mod)
```

### Pipelines

Pipelines, such as LLVM's default pipeline, are similarly represented by either strings
(`"default"`), or objects (`DefaultPipeline`), both of which supporting options (resp.
`"default<O3>"` and `DefaultPipeline(; opt_level=3)`).

LLVM's default pipeline doesn't support many options (as opposed to, e.g., Julia's
pipeline). Instead, the pipeline can be tuned through pipeline tuning keyword arguments that
have to be set on the `PassBuilder` object. Refer to the `NewPMPasBuilder` docstrings
for more details.


## Pass managers

When running or adding passes directly to a pass builder, LLVM will guess the appropriate
pass manager to use. When combining multiple types of passes, it is required to manually
construct the appropriate pass manager:

```jldoctest
julia> @dispose pb=NewPMPassBuilder() begin
         add!(pb, NewPMModulePassManager()) do mpm
           add!(mpm, NoOpModulePass())
           add!(mpm, NewPMFunctionPassManager()) do fpm
             add!(fpm, NoOpFunctionPass())
             add!(fpm, NewPMLoopPassManager()) do lpm
               add!(lpm, NoOpLoopPass())
             end
           end
         end
         run!(pb, mod)
       end
```


## Alias analyses

When not specified, LLVM will use the default alias analyses passes when optimizing code.
It is possible to customize this selection through the `AAManager` object. This object
behaves like other pass managers, and alias analysis passes can similarly to regular passes
be constructed by name or by object:

```jldoctest
julia> @dispose pb=NewPMPassBuilder() begin
         add!(pb, NewPMAAManager()) do aam
           add!(aam, "basic-aa")
           add!(aam, SCEVAA())
         end
         add!(pb, "aa-eval")
         run!(pb, mod)
       end
```


## Custom passes

Simple LLVM passes can be implemented in Julia by defining a function that takes a
single argument (a module or function), and returns a boolean indicating whether the
module or function was modified. This function is then to be wrapped in a `ModulePass`
or `FunctionPass` object, and registered with the pass builder:

```jldoctest
julia> function custom_module_pass!(mod::LLVM.Module)
         println("Hello, World!")
         return false
       end;

julia> CustomModulePass() = NewPMModulePass("custom_module_pass", custom_module_pass!);

julia> @dispose pb=NewPMPassBuilder() begin
         register!(pb, CustomModulePass())
         add!(pb, CustomModulePass())
         run!(pb, mod)
       end
Hello, World!
```


## Custom target info

Some analyses and passes consult LLVM's `TargetTransformInfo` (TTI) to decide
whether they can do anything useful — `InferAddressSpacesPass` wants to know
the flat address space, `UniformityAnalysis` wants to know which values are
divergent, and so on.

Most users don't need to think about this. In-tree back-ends (host CPU, NVPTX,
AMDGPU) carry their own TTI through a `TargetMachine`, and passing that
machine to `run!(pb, mod, tm)` is enough — the machine provides everything
these passes need.

The `AbstractTargetTransformInfo` interface is for the cases where that isn't
enough:

- **Out-of-tree back-ends** whose TTI isn't linked into the `libLLVM` that
  Julia loads (e.g. Metal). Pipelines targeting them have no `TargetMachine`
  to derive TTI from, so the baseline TTI takes over and TTI-sensitive passes
  silently no-op.
- **Tooling / analysis flows** that run passes without any target at all.
- **Per-query overrides on top of a real `TargetMachine`**: a custom TTI
  attached via `target_transform_info!` takes precedence over the
  machine-derived one, useful for forcing a particular knob while leaving
  everything else alone.

Subtype `AbstractTargetTransformInfo` and override only the queries you care
about; every other query falls back to a default matching LLVM's
`TargetTransformInfoImplBase`. Attach the instance to a `NewPMPassBuilder`
with `target_transform_info!`:

```julia
using LLVM

struct MyTTI <: AbstractTargetTransformInfo end
LLVM.flat_address_space(::MyTTI) = UInt(0)
LLVM.is_noop_addr_space_cast(::MyTTI, from::Unsigned, to::Unsigned) =
    from == 0 || to == 0

@dispose pb=NewPMPassBuilder() begin
    target_transform_info!(pb, MyTTI())
    add!(pb, NewPMFunctionPassManager()) do fpm
        add!(fpm, InferAddressSpacesPass())
    end
    run!(pb, mod)
end
```

Pass `nothing` to `target_transform_info!` to revert to LLVM's native TTI
(the `TargetMachine`-derived one if you supply a machine to `run!`, otherwise
the conservative baseline).

See the API reference for the full list of overridable queries.


## IR cloning

Somewhat distinct from IR passes, it is also possible to clone bits of the IR. This can be
useful when you want to keep the original version of the IR around, or when you have to
fundamentally change the IR in a way that requires recreating the IR (e.g., when changing a
function type).

The workhorse for this is the `clone_into!` function, which takes a source and a destination
function, cloning the source into the destination:

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
             }""";
    mod = parse(LLVM.Module, ir);
    src = functions(mod)["add"];
end
```

```jldoctest
julia> src
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}

julia> dst = LLVM.Function(mod, "new_add", function_type(src));

julia> value_map = Dict(
            parameters(src)[1] => parameters(dst)[1],
            parameters(src)[2] => parameters(dst)[2]
       );

julia> clone_into!(dst, src; value_map);

julia> dst
define i64 @new_add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}
```

Note how we had to provide a value map to map the arguments of the source function to the
arguments of the new destination function. This is a powerful tool, which makes it possible
to splice IR into functions that have different signatures:

```jldoctest
julia> dst = LLVM.Function(mod, "new_add", function_type(src));

julia> # let's swap the arguments around
       value_map = Dict(
            parameters(src)[1] => parameters(dst)[2],
            parameters(src)[2] => parameters(dst)[1]
       );

julia> clone_into!(dst, src; value_map);

julia> dst
define i64 @new_add(i64 %0, i64 %1) {
top:
  %2 = add i64 %0, %1
  ret i64 %2
}
```

It is also possible to map types using the `type_mapper` callback, or to materialize values
by passing a `materializer` callback. Refer to the upstream LLVM documentation for more
details.

For the simpler use case of just cloning a function and mapping values, the `clone` function
can be used:

```jldoctest
julia> # let's replace an argument by a constant
       value_map = Dict(
            parameters(src)[1] => ConstantInt(42)
       );

julia> clone(src; value_map)
define i64 @add.1(i64 %0) {
top:
  %1 = add i64 %0, 42
  ret i64 %1
}
```

Finally, it is also possible to clone just a basic block, inserting it at the end of
a function. This differs from a simple call to `copy` in that it also accepts a value map:

```jldoctest
julia> bb = entry(src);

julia> # let's again an argument by a constant
       value_map = Dict(
            parameters(src)[1] => ConstantInt(42)
       );

julia> clone(bb; value_map);

julia> src
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2

top1:                                             ; No predecessors!
  %3 = add i64 %1, 42
  ret i64 %3
}
```

This of course isn't very useful by itself, but can be a useful starting point for more
complex transformations.
