# Transforms

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.Analysis, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
end
```

LLVM contains a variety of passes that can be used to transform IR. With LLVM.jl, it is
also possible to write your own passes in Julia. LLVM.jl uses LLVM's new pass manager;
the legacy pass manager, which LLVM deprecated, is not supported.


## Pass builders

The core abstraction for running passes is the `PassBuilder` object, which aggregates
passes via the `add!` function and runs them over a function or module using the `run!`
function:

```jldoctest
julia> @dispose pb=PassBuilder() begin
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
Refer to the `PassBuilder` docstring for more details.


## Passes

In LLVM's new pass manager, passes are simply strings, as shown above. LLVM.jl also provides
a function for each pass that returns its string, which helps to avoid typos in pass names,
and turns keyword arguments into the pass's options.

The `loop-unroll` pass from above, for example, can also be written using the
`LoopUnrollPass` function:

```jldoctest
julia> LoopUnrollPass(; partial=true)
"loop-unroll<O0;partial>"

julia> run!(LoopUnrollPass(; partial=true), mod)
```

Custom passes implemented in Julia, on the other hand, are objects (see below).

### Pipelines

Pipelines, such as LLVM's default pipeline, are similarly strings (`"default<O3>"`), which
functions like `DefaultPipeline(; opt_level=3)` can construct.

LLVM's default pipeline doesn't support many options (as opposed to, e.g., Julia's
pipeline). Instead, the pipeline can be tuned through pipeline tuning keyword arguments that
have to be set on the `PassBuilder` object. Refer to the `PassBuilder` docstrings
for more details.

To see what a pipeline consists of, `print_pipeline` prints it as LLVM parses it, like
`opt -print-pipeline-passes` does: with the pass managers that LLVM inserts, every pass under
its name, and the options of passes that take them. Pipelines like `default<O2>` are
printed as the passes they consist of.

```jldoctest
julia> @dispose pb=PassBuilder() begin
         add!(pb, "globaldce,instsimplify,dce")
         print_pipeline(pb)
       end
globaldce,function(instsimplify),function(dce)
```


## Pass managers

When running or adding passes directly to a pass builder, LLVM will guess the appropriate
pass manager to use. When combining multiple types of passes, it is required to manually
construct the appropriate pass manager:

```jldoctest
julia> @dispose pb=PassBuilder() begin
         add!(pb, ModulePassManager()) do mpm
           add!(mpm, NoOpModulePass())
           add!(mpm, FunctionPassManager()) do fpm
             add!(fpm, NoOpFunctionPass())
             add!(fpm, LoopPassManager()) do lpm
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
behaves like other pass managers, and alias analysis passes can, like regular passes, be
given by name or by using their functions (e.g., `BasicAA()`):

```jldoctest
julia> @dispose pb=PassBuilder() begin
         add!(pb, AAManager()) do aam
           add!(aam, "basic-aa")
           add!(aam, SCEVAA())
         end
         add!(pb, "aa-eval")
         run!(pb, mod)
       end
```

An empty `AAManager` disables alias analysis. LLVM's default alias analyses include those
that the target machine provides, e.g., ones that know about the address spaces of a GPU.
To use them together with other alias analyses, list them using `TargetAA()`:

```julia
@dispose pb=PassBuilder() begin
    add!(pb, AAManager()) do aam
        add!(aam, BasicAA())
        add!(aam, TargetAA())
    end
    add!(pb, "gvn")
    run!(pb, mod, tm)
end
```


## Custom passes

Simple LLVM passes can be implemented in Julia by defining a function that takes a
single argument (a module or function), and returns a boolean indicating whether the
module or function was modified. This function is then to be wrapped in a `ModulePass`
or `FunctionPass` object, and registered with the pass builder:

```jldoctest custom_pass
julia> function custom_module_pass!(mod::LLVM.Module)
         println("Hello, World!")
         return false
       end;

julia> CustomModulePass() = ModulePass("custom_module_pass", custom_module_pass!);

julia> @dispose pb=PassBuilder() begin
         register!(pb, CustomModulePass())
         add!(pb, CustomModulePass())
         run!(pb, mod)
       end
Hello, World!
```

To run a single custom pass, `run!` can also be called with the pass itself, which
registers it with a temporary pass builder:

```jldoctest custom_pass
julia> run!(CustomModulePass(), mod)
Hello, World!
```

Function passes can use LLVM's analyses, which LLVM computes and caches for the pipeline.
To do so, pass `analyses=true` when creating the pass: the function is then also given the
pipeline's `FunctionAnalysisManager`, which provides analysis results by type. Instead of a
boolean, such a pass may return the set of analyses that remain valid after it ran, so that
LLVM does not need to recompute them:

```jldoctest custom_pass
julia> function check_dominance!(f::LLVM.Function, am)
         domtree = am[DomTree]
         entry, then, join = f.blocks
         println("entry dominates join: ", dominates(domtree, entry.terminator, join.terminator))
         println("then dominates join: ", dominates(domtree, then.terminator, join.terminator))
         return PreservedAnalyses(AllAnalyses)
       end;

julia> diamond = parse(LLVM.Module, """
         define void @diamond(i1 %c) {
         entry:
           br i1 %c, label %then, label %join
         then:
           br label %join
         join:
           ret void
         }""");

julia> run!(FunctionPass("check-dominance", check_dominance!; analyses=true), diamond)
entry dominates join: true
then dominates join: false
```

Module passes can use analyses too. With `analyses=true`, a module pass is given the
pipeline's `ModuleAnalysisManager`, which provides the analyses of each function of the
module by indexing it with that function. For example, a pass that looks at the
dominator trees and value ranges (see the `LLVM.Analysis` vocabulary) of several functions:

```jldoctest custom_pass
julia> function check_ranges!(mod::LLVM.Module, am)
         for f in mod.functions
           x = only(f.parameters)
           entry, then, exit = f.blocks
           @assert dominates(am[f][DomTree], entry.terminator, then.terminator)
           range = ConstantRange(am[f][LazyValueInfo], x; at=then.terminator)
           println(f.name, ": ", range)
         end
         return PreservedAnalyses(AllAnalyses)
       end;

julia> bounds = parse(LLVM.Module, """
         define void @small(i32 %x) {
         entry:
           %c = icmp ult i32 %x, 10
           br i1 %c, label %then, label %exit
         then:
           ret void
         exit:
           ret void
         }
         define void @large(i32 %x) {
         entry:
           %c = icmp ult i32 %x, 1000
           br i1 %c, label %then, label %exit
         then:
           ret void
         exit:
           ret void
         }""");

julia> run!(ModulePass("check-ranges", check_ranges!; analyses=true), bounds)
small: ConstantRange(32, 0, 10)
large: ConstantRange(32, 0, 1000)
```

Analysis results are owned by the analysis manager, and borrowed by the pass:

- they must not be disposed of, or used after the pass has returned;
- they must not be used after they were invalidated, e.g., using `invalidate!`;
- they are not updated when the pass changes the IR. A pass that queries analyses after
  changing the IR needs to keep them up to date itself, or invalidate them using
  `invalidate!` (e.g., `invalidate!(am)` in a function pass, or `invalidate!(am[f])` for
  a function `f` in a module pass).

What a pass returns only determines which analyses remain valid after it ran. A module pass
that returns `true` (or `PreservedAnalyses()`) invalidates the analyses of all functions.
To keep those of the functions it did not change, it can invalidate the analyses of the
functions it changed itself, and return `PreservedAnalyses(FunctionAnalyses)`. Before a
module pass deletes a function, it needs to remove all of its analyses using
`empty!(am[f])`, as some of them (like the assumption cache) survive invalidation.

A function pass with analyses can also run other passes on its function, e.g.,
`run!(InstCombinePass(), f, am)`. These run as part of the pipeline, with its analyses,
like a nested function pass manager would, and `run!` returns the analyses that they
preserved. Unlike a nested pass manager, the pass can decide whether to run them, and what
to do with their result:

```jldoctest custom_pass
julia> function simplify_twice!(f::LLVM.Function, am)
         preserved = run!(InstSimplifyPass(), f, am)
         println(f.name, ": ", preserved)
         if preserved != PreservedAnalyses(AllAnalyses)
           preserved = intersect(preserved, run!(InstSimplifyPass(), f, am))
         end
         return preserved
       end;

julia> run!(FunctionPass("simplify-twice", simplify_twice!; analyses=true), diamond)
diamond: PreservedAnalyses(AllAnalyses)
```

Passes with analyses can report what they did, or why they didn't, using optimization
remarks, like LLVM's passes do. These can be collected using the `remarks` keyword argument
of the pass builder (or of `run!`):

```jldoctest custom_pass
julia> function report!(f::LLVM.Function, am)
         remark!(am, :missed, "report", "NoChange", f.entry.terminator, "nothing to do")
         return false
       end;

julia> run!(FunctionPass("report", report!; analyses=true), diamond;
            remarks = r -> println(r.function_name, ": ", r.message))
diamond: nothing to do
```

Custom passes can also take options, which are written in a pipeline like those of LLVM's
passes, e.g., `my-pass<limit=10>`. To support them, pass a function that parses the text
between the angle brackets using the `parse_options` keyword argument; the pass callback
is then called with the parsed options as an extra argument:

```jldoctest custom_pass
julia> pass = FunctionPass("print-limit", (f, limit) -> (println(f.name, ": ", limit); false);
                           parse_options = str -> isempty(str) ? 10 : parse(Int, str));

julia> @dispose pb=PassBuilder() begin
         register!(pb, pass)
         add!(pb, "function(print-limit<42>,print-limit)")
         run!(pb, diamond)
       end
diamond: 42
diamond: 10
```

LLVM knows custom passes by their name, like it knows its own passes by theirs. This is the
name that its instrumentation reports (e.g., the passes that `debug_logging=true` lists),
and that its command-line options for debugging pipelines select passes with, e.g.,
`-print-before=print-limit` to print the IR before each occurrence of the pass above.

Loop passes run on each loop of a function, innermost loops first, in a loop pass manager:
a `loop(...)` pipeline, or a `loop-mssa(...)` pipeline that also maintains MemorySSA. A
custom loop pass is created using `LoopPass`, and its callback is given the loop and the
`LoopAnalysisResults` of the loop pass manager, which provide the analyses that loop
passes use. Like LLVM's loop passes, it can also be named on its own at the start of a
pipeline:

```jldoctest custom_pass
julia> function count_blocks!(loop::Loop, ar::LoopAnalysisResults)
         se = ar[ScalarEvolution]
         println(loop.header.name, ": ", length(loop.blocks), " block(s), ",
                 backedge_taken_count(se, loop), " backedges taken")
         return PreservedAnalyses(AllAnalyses)
       end;

julia> counted = parse(LLVM.Module, """
         define void @counted() {
         entry:
           br label %loop
         loop:
           %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
           %i.next = add i32 %i, 1
           %c = icmp ult i32 %i.next, 10
           br i1 %c, label %loop, label %exit
         exit:
           ret void
         }""");

julia> @dispose pb=PassBuilder() begin
         register!(pb, LoopPass("count-blocks", count_blocks!))
         add!(pb, "count-blocks")
         run!(pb, counted)
       end
loop: 1 block(s), SCEVConstant(9) backedges taken
```

The analyses that a loop pass is given are shared by the whole loop pipeline, which relies
on every pass to keep them valid: a pass that changes a loop needs to update the dominator
tree, loop info and scalar evolution (e.g., using `forget!` for values and loops whose
scalar evolution changed), and in a `loop-mssa` pipeline, MemorySSA. It then returns
`PreservedAnalyses(LoopAnalyses)`, together with `CFGAnalyses` if it did not change the
control-flow graph and `MemorySSA` if it kept MemorySSA valid, which LLVM requires in a
`loop-mssa` pipeline. The loop pass manager runs the `loop-simplify` and `lcssa` passes
before the loop passes, so loops normally have a preheader, but that can fail, so a pass
that needs one should check that `loop.preheader !== nothing`. See [`LoopPass`](@ref) for
the details.

If a custom pass throws an exception, the optional passes that remain in the pipeline and
all custom passes are skipped, as the IR and the analyses that the pipeline relies on may
be inconsistent, and `run!` throws a `PassException`. This is best-effort containment:
LLVM's required passes, like the pass managers, still run, so a pass should avoid leaving
the IR inconsistent.

Passes that are implemented in C++ can be used as well, by registering a native callback
that is called with LLVM's `PassBuilder`, like the ones pass plugins provide:

```julia
@dispose pb=PassBuilder() begin
    register_callbacks!(pb, cglobal((:registerCallbacks, libfoo)))
    add!(pb, "foo-pass")
    run!(pb, mod)
end
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
`TargetTransformInfoImplBase`. Attach the instance to a `PassBuilder`
with `target_transform_info!`:

```julia
using LLVM, LLVM.Passes

struct MyTTI <: LLVM.AbstractTargetTransformInfo end
LLVM.flat_address_space(::MyTTI) = UInt(0)
LLVM.is_noop_addr_space_cast(::MyTTI, from::Unsigned, to::Unsigned) =
    from == 0 || to == 0

@dispose pb=PassBuilder() begin
    target_transform_info!(pb, MyTTI())
    add!(pb, FunctionPassManager()) do fpm
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
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

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
    src = mod.functions["add"];
end
```

```jldoctest
julia> src
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}

julia> dst = LLVM.Function(mod, "new_add", src.function_type);

julia> value_map = Dict(
            src.parameters[1] => dst.parameters[1],
            src.parameters[2] => dst.parameters[2]
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
julia> dst = LLVM.Function(mod, "new_add", src.function_type);

julia> # let's swap the arguments around
       value_map = Dict(
            src.parameters[1] => dst.parameters[2],
            src.parameters[2] => dst.parameters[1]
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
            src.parameters[1] => ConstantInt(42)
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
julia> bb = src.entry;

julia> # let's again an argument by a constant
       value_map = Dict(
            src.parameters[1] => ConstantInt(42)
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
