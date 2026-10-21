# pass builders, pass managers and passes


## pass managers

@vocabulary Passes ModulePassManager, CGSCCPassManager, FunctionPassManager,
                   LoopPassManager, AAManager

abstract type AbstractPassManager end

"""
    add!(pm::AbstractPassManager, pass)

Adds a pass or pipeline to a pass builder or pass manager, and returns the pass builder or
pass manager.

The pass or pipeline should be a string or string-convertible object known by LLVM. These
can be constructed by using pass constructors, e.g., `InternalizePass()`, or by manually
specifying names like `default<O3>`.

When using custom passes, remember that they need to be registered with the pass builder
before they can be used.

See also: [`register!`](@ref)
"""
function add!(pm::AbstractPassManager, pass)
    push!(pm.passes, string(pass))
    return pm
end

"""
    ModulePassManager()
    CGSCCPassManager()
    FunctionPassManager(; eager_invalidate=false)
    LoopPassManager(; use_memory_ssa=false)
    AAManager()

Create a new pass manager of the specified type. These objects can be used to construct
pass pipelines, by `add!`ing passes to them, and finally `add!`ing them to a parent
pass manager or pass builder.

Creating a pass manager and adding it to a parent manager or builder can be shortened
using a single `add!`:

```julia
add!(parent, ModulePassManager()) do mpm
    add!(mpm, SomeModulePass())
end
```

This form does not add pass managers that are left empty.

With `eager_invalidate=true`, the analyses of each function are invalidated as soon as the
passes of a function pass manager have run on it, rather than kept for the passes that come
later (`function<eager-inv>`), which LLVM's pipelines do to save memory. With
`use_memory_ssa=true`, a loop pass manager maintains MemorySSA for passes that require it
(`loop-mssa`).

An `AAManager` selects the alias analyses that passes use (in order), instead of LLVM's
default ones, and can only be added to a pass builder. Unlike other pass managers, it is
meaningful when empty: the passes then use no alias analysis. LLVM's default alias
analyses include those of the target machine, which an `AAManager` only includes where it
lists [`TargetAA()`](@ref TargetAA).

See also: [`add!`](@ref), [`PassBuilder`](@ref)
"""
struct PassManager <: AbstractPassManager
    type::String
    passes::Vector{String}

    PassManager(type::AbstractString) = new(type, String[])
end
@vocabulary Passes PassManager

Base.string(pm::PassManager) = "$(pm.type)($(join(pm.passes, ",")))"

function add!(f::Base.Callable, parent::AbstractPassManager, nested::AbstractPassManager)
    f(nested)
    if !isempty(nested.passes)
        add!(parent, nested)
    end
    return parent
end

@doc (@doc PassManager)
ModulePassManager() = PassManager("module")

@doc (@doc PassManager)
CGSCCPassManager() = PassManager("cgscc")

@doc (@doc PassManager)
FunctionPassManager(; eager_invalidate::Bool=false) =
    PassManager(eager_invalidate ? "function<eager-inv>" : "function")

@doc (@doc PassManager)
LoopPassManager(; use_memory_ssa=false) =
    PassManager(use_memory_ssa ? "loop-mssa" : "loop")


## custom passes

@vocabulary Passes ModulePass, FunctionPass, LoopPass

"""
    ModulePass(name, callback; required=false, analyses=false, parse_options=nothing)
    FunctionPass(name, callback; required=false, analyses=false, parse_options=nothing)

Create a new custom pass. The `name` is a string that will be used to identify the pass
in the pass manager. The `callback` is a function that will be called when the pass is
run. The function should take a single argument, the module or function to be processed,
and return a boolean indicating whether the pass made any changes.
Set `required=true` for a pass needed for correctness; LLVM then does not skip it on
`optnone` functions or under `-opt-bisect-limit`.
LLVM's instrumentation also knows the pass by its name, e.g., when listing the passes that
run (with `debug_logging=true`), or when selecting passes to print the IR of (with LLVM's
`-print-before=name` option). That does not work for a name that is also the class name of
one of LLVM's passes, like `InstCombinePass`, which LLVM already knows by its own name.

With `analyses=true`, a pass can use LLVM's analyses: the callback is then called with two
arguments, the function and its [`FunctionAnalysisManager`](@ref) (for a function pass),
or the module and its [`ModuleAnalysisManager`](@ref) (for a module pass). Instead of a
boolean, it may return a [`PreservedAnalyses`](@ref) value describing the analyses that
remain valid after the pass:

```julia
function my_pass!(f::LLVM.Function, am::LLVM.FunctionAnalysisManager)
    domtree = am[DomTree]
    changed = ...
    # this pass does not modify the CFG
    return changed ? PreservedAnalyses(CFGAnalyses) : PreservedAnalyses(AllAnalyses)
end
FunctionPass("my-pass", my_pass!; analyses=true)
```

A module pass gets the analyses of each of its functions by indexing its manager with the
function:

```julia
function my_module_pass!(mod::LLVM.Module, am::LLVM.ModuleAnalysisManager)
    for f in mod.functions
        isdeclaration(f) && continue
        domtree = am[f][DomTree]
        ...
    end
    return false
end
ModulePass("my-module-pass", my_module_pass!; analyses=true)
```

A pass can take options, which are written in a pipeline like those of LLVM's passes:
`name<options>`. To support them, pass a function `parse_options` that takes the string
between the angle brackets (an empty string if the pass is written as `name` or
`name<>`), and returns the options. The callback is then called with those options as an
extra, last argument. For example, a pass that is configured using `my-pass<limit=N>`:

```julia
function parse_my_options(str)
    limit = 10
    for opt in split(str, ';'; keepempty=false)
        key, value = split(opt, '=')
        key == "limit" || throw(ArgumentError("unknown option \$key"))
        limit = parse(Int, value)
    end
    return (; limit)
end
my_pass!(f::LLVM.Function, am, options) = ...
FunctionPass("my-pass", my_pass!; analyses=true, parse_options=parse_my_options)
```

Each occurrence of the pass in a pipeline has its own options, which are used for every
function or module that occurrence runs on, so they should not be mutated by the pass.
The parser may be called more than once for an occurrence, so it should not have side
effects. Options cannot contain the characters `,`, `(` and `)`, which separate passes in
a pipeline. Exceptions thrown by the parser are rethrown as `PassException` by `run!`.

Before using a custom pass, it must be registered with a pass builder using `register!`.
LLVM.jl catches exceptions from these callbacks and rethrows them as `PassException`
after control has returned from LLVM. The optional passes that remain to run in the
pipeline are skipped then, as are custom passes even if they are required, since the IR may
be inconsistent. This is not a guarantee that nothing uses it: LLVM's required passes (like
the pass managers and adaptors, which skip the passes they contain) still run, and with
`verify_each`, LLVM verifies the IR after the failing pass and aborts if it is invalid.
Callbacks registered directly through the `LLVM.API.LLVMPassBuilderExtensionsRegister*Pass`
APIs must provide an equivalent exception
barrier: Julia exceptions must not escape a callback, because they bypass C++ destructors in
LLVM's pass runner.

See also: [`register!`](@ref)
"""
struct CustomPass
  type::Symbol
  name::String
  callback::Any
  required::Bool
  analyses::Bool
  parse_options::Any
end
@vocabulary Passes CustomPass

CustomPass(type, name, callback, required, analyses=false) =
    CustomPass(type, name, callback, required, analyses, nothing)

Base.string(pass::CustomPass) = pass.name

@doc (@doc CustomPass)
ModulePass(name, callback; required::Bool=false, analyses::Bool=false,
           parse_options=nothing) =
    CustomPass(:module, name, callback, required, analyses, parse_options)

@doc (@doc CustomPass)
FunctionPass(name, callback; required::Bool=false, analyses::Bool=false,
             parse_options=nothing) =
    CustomPass(:function, name, callback, required, analyses, parse_options)

"""
    LoopPass(name, callback; required=false, analyses=true, parse_options=nothing)

Create a custom loop pass, which runs on each loop of a function (innermost loops first) in
a loop pass manager, i.e., in a `loop(...)` or `loop-mssa(...)` pipeline. Like LLVM's loop
passes, it can also be used on its own (without MemorySSA) if its name is the first of a
pipeline, e.g., `add!(pb, "my-loop-pass")`.

The `callback` is called with the [`Loop`](@ref) and the [`LoopAnalysisResults`](@ref) of
the loop pass manager, which provide the analyses that loop passes use (e.g.,
`ar[ScalarEvolution]`). It returns the [`PreservedAnalyses`](@ref) describing the analyses
that remain valid, typically `PreservedAnalyses(AllAnalyses)` if the pass did not change
anything, and `PreservedAnalyses(LoopAnalyses)` (plus `CFGAnalyses` and `MemorySSA` as
appropriate) if it did. With `analyses=false`, the callback is only called with the loop,
and returns a boolean indicating whether it changed anything, like other custom passes.

```julia
function hoist!(loop::Loop, ar::LoopAnalysisResults)
    se = ar[ScalarEvolution]
    changed = ...
    changed || return PreservedAnalyses(AllAnalyses)
    return PreservedAnalyses(LoopAnalyses, CFGAnalyses, MemorySSA)
end
LoopPass("hoist", hoist!)
```

Loop passes need to respect the contract of LLVM's loop pass manager, which shares its
analyses between all passes of the pipeline:

- the dominator tree, loop info and scalar evolution of the function must be kept valid
  by the pass (for scalar evolution, e.g., using [`forget!`](@ref)), as the loop pass
  manager treats them as preserved, whatever the pass returns;
- in a `loop-mssa` pipeline, MemorySSA must be kept valid as well, and the pass must
  preserve it (`MemorySSA` or `AllAnalyses`); returning anything else is an error;
- the pass should only change the loop it is called with (and the loops nested in it),
  and not delete loops, as it cannot notify the loop pass manager of that;
- the loop pass manager runs LLVM's `loop-simplify` and `lcssa` passes before the loop
  passes, so loops are normally in their canonical form, which a pass should keep. That
  can fail (e.g., for loops entered through an `indirectbr`), so a pass that needs a
  preheader should check that `loop.preheader !== nothing`, like Julia's loop passes do.

`required` and `parse_options` work like for [`FunctionPass`](@ref). If the callback
throws an exception, the remaining passes of the pipeline are skipped as described for
[`FunctionPass`](@ref), since the analyses that the loop pass manager shares may be stale,
and `run!` throws a `PassException`. To keep the loop pass manager from aborting the
process, a failing pass in a `loop-mssa` pipeline is reported to preserve MemorySSA. This
is best-effort containment, not recovery from an arbitrary partial change of the IR:
LLVM's required passes, like the pass managers and adaptors, still run and use analyses.

See also: [`register!`](@ref), [`LoopAnalysisResults`](@ref)
"""
LoopPass(name, callback; required::Bool=false, analyses::Bool=true,
         parse_options=nothing) =
    CustomPass(:loop, name, callback, required, analyses, parse_options)

# State struct to store callback and any caught exception
mutable struct CustomPassState
    callback::Any
    exception::Union{Nothing, Tuple{Any, Vector}}  # (exception, backtrace)

    # for passes with options: the parser, and the occurrences of the pass in the pipeline
    pass::Union{Nothing,CustomPass}
    occurrences::Vector{Any}

    # the extensions of the run, to abort it when the callback fails
    exts::API.LLVMPassBuilderExtensionsRef

    CustomPassState(callback) = new(callback, nothing, nothing, [], C_NULL)
    CustomPassState(pass::CustomPass, exts=C_NULL) = new(pass.callback, nothing, pass, [], exts)
end

# record an exception thrown by a callback, and skip the rest of the pipeline, which could
# otherwise run on inconsistent IR or analyses
function fail_pass!(state::CustomPassState, err)
    _capture_callback_exception!(state, err)
    state.exts == C_NULL || API.LLVMExtraPassBuilderExtensionsAbort(state.exts)
    return
end

# an occurrence of a pass with options in a pipeline
mutable struct CustomPassOccurrence
    state::CustomPassState
    options::Any
end

# Exception type to preserve original error and backtrace
@vocabulary Passes PassException

"""
    PassException

The exception thrown by [`run!`](@ref) when a custom pass (or a custom target transform
info) threw an exception while LLVM ran it. The original exception is available as its
`ex` field, and the backtrace of where it was thrown as `processed_bt`.
"""
struct PassException <: Exception
    ex::Any
    processed_bt::Vector{Base.StackTraces.StackFrame}

    function PassException(ex, bt)
        # `stacktrace` accepts either a raw bt from `catch_backtrace()` or
        # an already-processed frame vector, and is stable across Julia
        # versions (unlike `Base.process_backtrace`, whose method for the
        # raw bt type was dropped in 1.14).
        bt_processed = stacktrace(bt)
        new(ex, bt_processed[1:min(100, end)])
    end
end

function Base.showerror(io::IO, e::PassException)
    print(io, "PassException: exception in custom pass callback\n\n    nested exception: ")
    showerror(io, e.ex, e.processed_bt, backtrace=true)
end

function module_callback(ref::API.LLVMModuleRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return true
    try
        mod = LLVM.Module(ref)
        return state.callback(mod)::Bool
    catch err
        fail_pass!(state, err)
        # The callback may have changed IR before throwing. Invalidate all
        # analyses before surfacing the exception after LLVM returns.
        return true
    end
end

function function_callback(ref::API.LLVMValueRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return true
    try
        fun = LLVM.Function(ref)
        return state.callback(fun)::Bool
    catch err
        fail_pass!(state, err)
        # The callback may have changed IR before throwing. Invalidate all
        # analyses before surfacing the exception after LLVM returns.
        return true
    end
end

function function_callback_with_analyses(ref::API.LLVMValueRef,
                                         am::API.LLVMFunctionAnalysisManagerRef,
                                         pa::API.LLVMPreservedAnalysesRef,
                                         thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    # the output object starts out as preserving nothing, which is what we want if this or
    # a previous invocation of the pass failed
    state.exception === nothing || return
    try
        fun = LLVM.Function(ref)
        preserved = state.callback(fun, FunctionAnalysisManager(am, fun))
        set_preserved!(pa, preserved)
    catch err
        fail_pass!(state, err)
    end
    return
end

function module_callback_with_analyses(ref::API.LLVMModuleRef,
                                       am::API.LLVMModuleAnalysisManagerRef,
                                       pa::API.LLVMPreservedAnalysesRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return
    try
        mod = LLVM.Module(ref)
        preserved = state.callback(mod, ModuleAnalysisManager(am, mod))
        set_module_preserved!(pa, preserved)
    catch err
        fail_pass!(state, err)
    end
    return
end

function parse_options_callback(params::Cstring, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    state.exception === nothing || return C_NULL
    try
        options = state.pass.parse_options(unsafe_string(params))
        occurrence = CustomPassOccurrence(state, options)
        # keep the occurrence alive for as long as the pipeline, which refers to it
        push!(state.occurrences, occurrence)
        return pointer_from_objref(occurrence)
    catch err
        fail_pass!(state, err)
        return C_NULL
    end
end

function module_callback_with_options(ref::API.LLVMModuleRef,
                                      am::API.LLVMModuleAnalysisManagerRef,
                                      pa::API.LLVMPreservedAnalysesRef, thunk::Ptr{Cvoid})
    occurrence = Base.unsafe_pointer_to_objref(thunk)::CustomPassOccurrence
    state = occurrence.state
    state.exception === nothing || return
    try
        mod = LLVM.Module(ref)
        preserved = if state.pass.analyses
            state.callback(mod, ModuleAnalysisManager(am, mod), occurrence.options)
        else
            state.callback(mod, occurrence.options)::Bool
        end
        set_module_preserved!(pa, preserved)
    catch err
        fail_pass!(state, err)
    end
    return
end

function function_callback_with_options(ref::API.LLVMValueRef,
                                        am::API.LLVMFunctionAnalysisManagerRef,
                                        pa::API.LLVMPreservedAnalysesRef, thunk::Ptr{Cvoid})
    occurrence = Base.unsafe_pointer_to_objref(thunk)::CustomPassOccurrence
    state = occurrence.state
    state.exception === nothing || return
    try
        fun = LLVM.Function(ref)
        preserved = if state.pass.analyses
            state.callback(fun, FunctionAnalysisManager(am, fun), occurrence.options)
        else
            state.callback(fun, occurrence.options)::Bool
        end
        set_preserved!(pa, preserved)
    catch err
        fail_pass!(state, err)
    end
    return
end


function loop_callback(ref::API.LLVMLoopRef, am::API.LLVMLoopAnalysisManagerRef,
                       ar::API.LLVMLoopStandardAnalysisResultsRef, u::API.LLVMLPMUpdaterRef,
                       pa::API.LLVMPreservedAnalysesRef, thunk::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(thunk)::CustomPassState
    run_loop_callback(state, (), ref, ar, u, pa)
    return
end

function loop_callback_with_options(ref::API.LLVMLoopRef,
                                    am::API.LLVMLoopAnalysisManagerRef,
                                    ar::API.LLVMLoopStandardAnalysisResultsRef,
                                    u::API.LLVMLPMUpdaterRef,
                                    pa::API.LLVMPreservedAnalysesRef, thunk::Ptr{Cvoid})
    occurrence = Base.unsafe_pointer_to_objref(thunk)::CustomPassOccurrence
    run_loop_callback(occurrence.state, (occurrence.options,), ref, ar, u, pa)
    return
end

function run_loop_callback(state::CustomPassState, options::Tuple, ref::API.LLVMLoopRef,
                           ar::API.LLVMLoopStandardAnalysisResultsRef,
                           u::API.LLVMLPMUpdaterRef, pa::API.LLVMPreservedAnalysesRef)
    # the output object starts out as what LLVM should see if the pass fails
    state.exception === nothing || return
    loop = Loop(ref)
    results = LoopAnalysisResults(ar, LPMUpdater(u), parent(header(loop)))
    try
        preserved = if state.pass.analyses
            state.callback(loop, results, options...)
        else
            state.callback(loop, options...)::Bool
        end
        set_loop_preserved!(pa, preserved, results)
    catch err
        fail_pass!(state, err)
    finally
        results.valid = false
    end
    return
end

## analysis managers

@vocabulary Passes FunctionAnalysisManager, ModuleAnalysisManager, LoopAnalysisResults,
                   LPMUpdater, PreservedAnalyses, AllAnalyses, CFGAnalyses,
                   FunctionAnalyses, LoopAnalyses, invalidate!

"""
    FunctionAnalysisManager

The analysis manager of a pass pipeline, for the function that a custom pass (created
using `FunctionPass(...; analyses=true)`) runs on. It provides the results of LLVM's
function analyses, which it owns and caches:

- `am[T]`: get the result of the analysis `T` (e.g., a `DomTree`), computing it if needed;
- `get(am, T, nothing)`: get the result of `T` only if it has been computed already;
- [`invalidate!(am, preserved)`](@ref invalidate!): invalidate the results that are not
  preserved;
- `empty!(am)`: remove all of the function's results, including the ones that survive
  invalidation. This is required before a module pass deletes the function.

The supported analyses are [`DomTree`](@ref), [`PostDomTree`](@ref),
[`AssumptionCache`](@ref), [`LazyValueInfo`](@ref), [`ScalarEvolution`](@ref),
[`LoopInfo`](@ref) and [`MemorySSA`](@ref).

Analysis results are borrowed from the manager, which owns them:

- they must not be disposed of, and must not be used after the pass returns;
- they must not be used after they were invalidated, using `invalidate!` or `empty!`;
- they are not updated when the pass changes the IR, so they become stale when it changes
  what they describe. A pass that changes the IR and then queries an analysis again needs
  to update that analysis itself (e.g., register new assumptions with the
  [`AssumptionCache`](@ref)), or invalidate it first.

What the pass returns only determines which analyses remain valid after the pass.

See also: [`PreservedAnalyses`](@ref), [`ModuleAnalysisManager`](@ref)
"""
struct FunctionAnalysisManager
    ref::API.LLVMFunctionAnalysisManagerRef
    fun::Function
end

Base.unsafe_convert(::Type{API.LLVMFunctionAnalysisManagerRef},
                    am::FunctionAnalysisManager) = am.ref

Base.show(io::IO, am::FunctionAnalysisManager) =
    print(io, "FunctionAnalysisManager(", repr(am.fun.name), ")")

# the analyses that can be queried, preserved and invalidated: the type of their result,
# and how to wrap a pointer to that result
analysis_id(T::Type) = throw(ArgumentError("$T is not a supported function analysis"))
analysis_id(::Type{DomTree}) = API.LLVMExtraDominatorTreeAnalysis
analysis_id(::Type{PostDomTree}) = API.LLVMExtraPostDominatorTreeAnalysis
analysis_id(::Type{AssumptionCache}) = API.LLVMExtraAssumptionAnalysis
analysis_id(::Type{LazyValueInfo}) = API.LLVMExtraLazyValueAnalysis
analysis_id(::Type{ScalarEvolution}) = API.LLVMExtraScalarEvolutionAnalysis
analysis_id(::Type{LoopInfo}) = API.LLVMExtraLoopAnalysis
analysis_id(::Type{MemorySSA}) = API.LLVMExtraMemorySSAAnalysis

# analysis results are owned by the analysis manager; stop tracking their wrappers, so that
# memcheck doesn't mistake them for objects that were disposed of at the same address
borrow_analysis(T::Type, ptr::Ptr{Cvoid}) =
    mark_untracked(T(convert(fieldtype(T, :ref), ptr)))

function Base.getindex(am::FunctionAnalysisManager, T::Type)
    # LLVM's analyses expect a function body
    isdeclaration(am.fun) &&
        throw(ArgumentError("Cannot compute analyses of declaration $(am.fun.name)"))
    ptr = API.LLVMExtraFunctionAnalysisManagerGetResult(am, am.fun, analysis_id(T))
    return borrow_analysis(T, ptr)
end

function Base.get(am::FunctionAnalysisManager, T::Type, default)
    ptr = API.LLVMExtraFunctionAnalysisManagerGetCachedResult(am, am.fun, analysis_id(T))
    return ptr == C_NULL ? default : borrow_analysis(T, ptr)
end

function Base.empty!(am::FunctionAnalysisManager)
    API.LLVMExtraFunctionAnalysisManagerClear(am, am.fun)
    return am
end

"""
    ModuleAnalysisManager

The analysis manager of a pass pipeline, for the module that a custom pass (created using
`ModulePass(...; analyses=true)`) runs on. Indexing it with a function of the module,
`am[f]`, returns the [`FunctionAnalysisManager`](@ref) for that function, which provides
LLVM's function analyses for it:

```julia
domtree = am[f][DomTree]
lvi = am[f][LazyValueInfo]
```

Analyses can only be computed for functions that have a body, not for declarations.
The function analyses of all functions are cached by the pipeline, and remain valid
until they are invalidated: when a module pass changes a function and then queries its
analyses again, it needs to invalidate them first using `invalidate!(am[f])`, and before
a module pass deletes a function, it needs to remove its results using `empty!(am[f])`.
The rules for borrowed results are the same as for function passes (see
[`FunctionAnalysisManager`](@ref)).

What the pass returns determines which analyses remain valid afterwards, on all functions
of the module (see [`PreservedAnalyses`](@ref)). A pass that only changed some functions
can invalidate their analyses itself, and return `PreservedAnalyses(FunctionAnalyses)` to
keep the cached results of the other functions.
"""
struct ModuleAnalysisManager
    ref::API.LLVMModuleAnalysisManagerRef
    mod::Module
end

Base.unsafe_convert(::Type{API.LLVMModuleAnalysisManagerRef}, am::ModuleAnalysisManager) =
    am.ref

Base.show(io::IO, am::ModuleAnalysisManager) =
    print(io, "ModuleAnalysisManager(", repr(am.mod.name), ")")

function Base.getindex(am::ModuleAnalysisManager, f::Function)
    parent(f) == am.mod ||
        throw(ArgumentError("Function $(f.name) is not part of module $(am.mod.name)"))
    fam = API.LLVMExtraModuleAnalysisManagerGetFunctionAnalysisManager(am, am.mod)
    return FunctionAnalysisManager(fam, f)
end

# module analyses are not supported yet; reserve the syntax for them
Base.getindex(am::ModuleAnalysisManager, T::Type) =
    throw(ArgumentError("$T is not a supported module analysis"))

"""
    LPMUpdater

The updater of a loop pass manager, through which a loop pass notifies the loop pass
manager of changes to the loop nest. It is available as `ar.updater` from the
[`LoopAnalysisResults`](@ref) of a custom loop pass, and like those, it must not be used
after the pass returns. No operations are supported yet.
"""
struct LPMUpdater
    ref::API.LLVMLPMUpdaterRef
end

Base.unsafe_convert(::Type{API.LLVMLPMUpdaterRef}, u::LPMUpdater) = u.ref

"""
    LoopAnalysisResults

The analyses that the loop pass manager of a pipeline provides to a custom loop pass
(created using [`LoopPass`](@ref)), for the function containing the loop. These are the
results of function analyses that the loop pass manager computes before running the loop
passes, and keeps valid while they run, with the help of the passes:

- `ar[T]`: the result of the analysis `T`, which is one of [`DomTree`](@ref),
  [`LoopInfo`](@ref), [`ScalarEvolution`](@ref), [`AssumptionCache`](@ref) and
  [`MemorySSA`](@ref). MemorySSA is only available in a `loop-mssa(...)` pipeline;
- `get(ar, T, nothing)`: the same, or `nothing` if the result is not available (i.e., for
  MemorySSA outside of a `loop-mssa(...)` pipeline);
- `ar.updater`: the [`LPMUpdater`](@ref) of the loop pass manager.

The results, and the object itself, are borrowed from the loop pass manager: they must not
be used after the pass returns. Unlike the results that a function pass obtains, they are
shared by all passes of the loop pipeline, so the pass must keep them up to date when it
changes the IR (see [`LoopPass`](@ref)).
"""
mutable struct LoopAnalysisResults
    const ref::API.LLVMLoopStandardAnalysisResultsRef
    const updater::LPMUpdater
    const fun::Function
    valid::Bool
end

LoopAnalysisResults(ref::API.LLVMLoopStandardAnalysisResultsRef, updater, fun) =
    LoopAnalysisResults(ref, updater, fun, true)

Base.unsafe_convert(::Type{API.LLVMLoopStandardAnalysisResultsRef},
                    ar::LoopAnalysisResults) = ar.ref

Base.show(io::IO, ar::LoopAnalysisResults) = print(io, "LoopAnalysisResults()")

loop_analysis_id(T::Type) =
    throw(ArgumentError("$T is not an analysis that the loop pass manager provides"))
loop_analysis_id(::Type{DomTree}) = API.LLVMExtraLoopDominatorTree
loop_analysis_id(::Type{LoopInfo}) = API.LLVMExtraLoopLoopInfo
loop_analysis_id(::Type{ScalarEvolution}) = API.LLVMExtraLoopScalarEvolution
loop_analysis_id(::Type{AssumptionCache}) = API.LLVMExtraLoopAssumptionCache
loop_analysis_id(::Type{MemorySSA}) = API.LLVMExtraLoopMemorySSA

function Base.get(ar::LoopAnalysisResults, T::Type, default)
    ar.valid || throw(ArgumentError("Loop analysis results cannot be used after the pass returned"))
    ptr = API.LLVMExtraLoopStandardAnalysisResultsGet(ar, loop_analysis_id(T))
    return ptr == C_NULL ? default : borrow_analysis(T, ptr)
end

function Base.getindex(ar::LoopAnalysisResults, T::Type)
    result = get(ar, T, nothing)
    result === nothing &&
        throw(ArgumentError("$T is not available; MemorySSA requires a loop-mssa pipeline"))
    return result
end

"""
    AllAnalyses
    CFGAnalyses
    FunctionAnalyses
    LoopAnalyses

Markers for sets of analyses, used with [`PreservedAnalyses`](@ref): `AllAnalyses` for every
analysis, `CFGAnalyses` for the analyses that only depend on the control-flow graph of
a function (like `DomTree` and `PostDomTree`), i.e., on its blocks and their terminators,
`FunctionAnalyses` for the function analyses of the function (for a function pass), or
of every function of the module (for a module pass), and `LoopAnalyses` (only for loop
passes) for the analyses that a loop pass keeps valid when it changes a loop: the dominator
tree, loop info and scalar evolution of the function.
"""
struct AllAnalyses end

@doc (@doc AllAnalyses)
struct CFGAnalyses end

@doc (@doc AllAnalyses)
struct FunctionAnalyses end

@doc (@doc AllAnalyses)
struct LoopAnalyses end

"""
    PreservedAnalyses(analyses...)

The set of analyses that remain valid after a custom pass, returned by a pass created using
`FunctionPass(...; analyses=true)`, `ModulePass(...; analyses=true)` or `LoopPass`. Each
argument is either an analysis (e.g. `DomTree`), or a marker for a set of analyses
([`AllAnalyses`](@ref), [`CFGAnalyses`](@ref), [`FunctionAnalyses`](@ref) or
[`LoopAnalyses`](@ref)):

- `PreservedAnalyses()`: no analysis is preserved (like returning `true`);
- `PreservedAnalyses(AllAnalyses)`: every analysis is preserved (like returning `false`);
- `PreservedAnalyses(CFGAnalyses)`: the pass did not change the control-flow graph;
- `PreservedAnalyses(DomTree)`: the pass kept the dominator tree up to date.

Some analyses are invalidated with the analyses they depend on, even when they are
preserved: e.g., [`MemorySSA`](@ref) depends on the dominator tree, so a pass that kept it
up to date returns `PreservedAnalyses(MemorySSA, CFGAnalyses)` (or `PreservedAnalyses(MemorySSA,
DomTree)` if it changed the control-flow graph and updated the dominator tree).

For a module pass, the function analyses that are listed are preserved for every function
of the module, while all other analyses of the module are invalidated. A module pass that
invalidated the analyses of the functions it changed itself (using
`invalidate!(am[f])`) can return `PreservedAnalyses(FunctionAnalyses)`, which keeps the
cached function analyses of all other functions.

A function pass that changed the function, but invalidated the analyses that it did not
preserve itself, can similarly return `PreservedAnalyses(FunctionAnalyses)`: the function's
analyses are then kept, while LLVM still knows that the function changed. This is what
running passes from a function pass with [`run!(pb, f, am)`](@ref run!(::PassBuilder,
::LLVM.Function, ::FunctionAnalysisManager)) returns when they changed the function.

The analyses that are preserved by two passes that run one after the other are
`intersect(a, b)`, like for LLVM's `PreservedAnalyses::intersect`: an analysis or set of
analyses is preserved when both preserve it, or when one of them preserves all analyses.

A loop pass can only preserve sets of analyses and MemorySSA: `PreservedAnalyses(AllAnalyses)`
if it did not change anything, or `PreservedAnalyses(LoopAnalyses)` if it did, together
with `CFGAnalyses` if it did not change the control-flow graph, and `MemorySSA` if it kept
MemorySSA up to date (which is required in a `loop-mssa(...)` pipeline). See
[`LoopPass`](@ref).

See also: [`FunctionAnalysisManager`](@ref), [`invalidate!`](@ref)
"""
struct PreservedAnalyses
    all::Bool
    functions::Bool
    loop::Bool
    cfg::Bool
    analyses::Vector{Type}

    function PreservedAnalyses(analyses::Type...)
        all = functions = loop = cfg = false
        individual = Type[]
        for T in analyses
            if T === AllAnalyses
                all = true
            elseif T === FunctionAnalyses
                functions = true
            elseif T === LoopAnalyses
                loop = true
            elseif T === CFGAnalyses
                cfg = true
            else
                analysis_id(T)  # validate
                T in individual || push!(individual, T)
            end
        end
        return new(all, functions, loop, cfg, individual)
    end
end

function Base.show(io::IO, pa::PreservedAnalyses)
    print(io, "PreservedAnalyses(")
    names = Any[pa.analyses...]
    pa.cfg && pushfirst!(names, CFGAnalyses)
    pa.loop && pushfirst!(names, LoopAnalyses)
    pa.functions && pushfirst!(names, FunctionAnalyses)
    pa.all && pushfirst!(names, AllAnalyses)
    join(io, names, ", ")
    print(io, ")")
end

Base.:(==)(a::PreservedAnalyses, b::PreservedAnalyses) =
    a.all == b.all && a.functions == b.functions && a.loop == b.loop && a.cfg == b.cfg &&
    issetequal(a.analyses, b.analyses)

Base.convert(::Type{PreservedAnalyses}, changed::Bool) =
    changed ? PreservedAnalyses() : PreservedAnalyses(AllAnalyses)

function Base.intersect(a::PreservedAnalyses, b::PreservedAnalyses)
    a.all && return b
    b.all && return a
    sets = Type[]
    a.functions && b.functions && push!(sets, FunctionAnalyses)
    a.loop && b.loop && push!(sets, LoopAnalyses)
    a.cfg && b.cfg && push!(sets, CFGAnalyses)
    return PreservedAnalyses(sets..., intersect(a.analyses, b.analyses)...)
end

# the analyses that the function analysis manager knows, in the terms of `PreservedAnalyses`
const function_analyses = (DomTree, PostDomTree, AssumptionCache, LazyValueInfo,
                           ScalarEvolution, LoopInfo, MemorySSA)

# the preserved analyses of an LLVM `PreservedAnalyses` object, as far as they can be
# expressed using the analyses that LLVM.jl knows
function PreservedAnalyses(ref::API.LLVMPreservedAnalysesRef)
    all, functions, cfg = Ref{API.LLVMBool}(), Ref{API.LLVMBool}(), Ref{API.LLVMBool}()
    API.LLVMExtraGetPreservedAnalyses(ref, all, functions, cfg)
    all[] == 1 && return PreservedAnalyses(AllAnalyses)
    preserved = Type[]
    functions[] == 1 && push!(preserved, FunctionAnalyses)
    cfg[] == 1 && push!(preserved, CFGAnalyses)
    for T in function_analyses
        API.LLVMExtraPreservedAnalysesIsPreserved(ref, analysis_id(T)) == 1 &&
            push!(preserved, T)
    end
    return PreservedAnalyses(preserved...)
end

# pass a set of preserved function analyses to a C function
# `f(args..., all, functions, cfg, ids, nids)`
function with_preserved(f, pa::PreservedAnalyses, args...)
    check_not_loop_preserved(pa)
    ids = API.LLVMExtraFunctionAnalysis[analysis_id(T) for T in pa.analyses]
    f(args..., pa.all, pa.functions, pa.cfg, ids, length(ids))
end

set_preserved!(ref::API.LLVMPreservedAnalysesRef, preserved) =
    with_preserved(API.LLVMExtraSetPreservedAnalyses,
                   convert(PreservedAnalyses, preserved)::PreservedAnalyses, ref)

function check_not_loop_preserved(pa::PreservedAnalyses)
    pa.loop && throw(ArgumentError("LoopAnalyses can only be preserved by a loop pass"))
end

function set_module_preserved!(ref::API.LLVMPreservedAnalysesRef, preserved)
    pa = convert(PreservedAnalyses, preserved)::PreservedAnalyses
    check_not_loop_preserved(pa)
    ids = API.LLVMExtraFunctionAnalysis[analysis_id(T) for T in pa.analyses]
    API.LLVMExtraSetModulePreservedAnalyses(ref, pa.all, pa.functions, pa.cfg, ids,
                                            length(ids))
end

function set_loop_preserved!(ref::API.LLVMPreservedAnalysesRef, preserved,
                             ar::LoopAnalysisResults)
    pa = convert(PreservedAnalyses, preserved)::PreservedAnalyses
    pa.functions &&
        throw(ArgumentError("FunctionAnalyses cannot be preserved by a loop pass"))
    for T in pa.analyses
        T === MemorySSA ||
            throw(ArgumentError("A loop pass cannot preserve $T on its own; use LoopAnalyses"))
    end
    mssa = MemorySSA in pa.analyses
    # the loop pass manager reports a fatal error otherwise
    if !pa.all && !mssa && get(ar, MemorySSA, nothing) !== nothing
        throw(ArgumentError("A loop pass in a loop-mssa pipeline needs to preserve MemorySSA"))
    end
    API.LLVMExtraSetLoopPreservedAnalyses(ref, pa.all, pa.loop, pa.cfg, mssa)
end

"""
    invalidate!(am::FunctionAnalysisManager, preserved=PreservedAnalyses())

Invalidate the analysis results of the function that are not in the set of `preserved`
analyses (by default, all of them), so that they are recomputed when they are queried
next. Any result that was obtained before must not be used anymore. This is useful for
a pass that changes the IR and then queries analyses that depend on it again.

Some analyses, like the assumption cache, are designed to survive invalidation, and need
to be updated by the pass instead.
"""
function invalidate!(am::FunctionAnalysisManager,
                     preserved::PreservedAnalyses=PreservedAnalyses())
    with_preserved(API.LLVMExtraFunctionAnalysisManagerInvalidate, preserved, am, am.fun)
    return am
end


## optimization remarks

@vocabulary Passes remark!, remark_enabled, Remark, RemarkFile

# the kind of a remark, by name
function remark_kind(kind::Symbol)
    if kind === :passed
        API.LLVMExtraRemarkPassed
    elseif kind === :missed
        API.LLVMExtraRemarkMissed
    elseif kind === :analysis
        API.LLVMExtraRemarkAnalysis
    else
        throw(ArgumentError("Invalid kind of remark :$kind; expected :passed, :missed or :analysis"))
    end
end

"""
    remark_enabled(ctx::Context, kind::Symbol, pass_name) -> Bool

Whether remarks of the given `kind` (`:passed`, `:missed` or `:analysis`) emitted by the
pass `pass_name` are wanted in the context `ctx`: because they are collected using the
`remarks` keyword argument of [`PassBuilder`](@ref) or `run!`, enabled for that pass
using LLVM's `-pass-remarks`, `-pass-remarks-missed` or `-pass-remarks-analysis` options
(see [`LLVM.clopts`](@ref)), written to a [`RemarkFile`](@ref) that accepts the pass, or
logged as debug messages because the current logger accepts those of LLVM.jl (e.g., with
`JULIA_DEBUG=LLVM`). A `remarks` callback or a debug logger wants all remarks of every
pass, while LLVM's own passes only build theirs when they are enabled using these options
or a remark file.

The do-block form of [`remark!`](@ref) uses this to only build remarks that are wanted.
A pass can also use it to skip work whose only purpose is to explain a decision in a
remark, like LLVM's passes do using `OptimizationRemarkEmitter::allowExtraAnalysis`.
"""
function remark_enabled(ctx::Context, kind::Symbol, pass_name::AbstractString)
    api_kind = remark_kind(kind)
    state = diagnostic_state(ctx)
    if state !== nothing
        state.remark_callback !== nothing && return true
        debug_remarks_enabled() && return true
    end
    return API.LLVMExtraIsRemarkEnabled(ctx, api_kind, pass_name) |> Bool
end

# without a callback, `handle_diagnostic` logs the remarks it receives as debug messages
# (from src/core/context.jl, hence the group), which a logger may want
debug_remarks_enabled() =
    Base.CoreLogging.current_logger_for_env(Base.CoreLogging.Debug, :context,
                                            @__MODULE__) !== nothing

# check the parts of a remark's message before creating it, so that it isn't leaked
check_remark_part(part::AbstractString) = nothing
check_remark_part(part::Pair{<:AbstractString}) = check_remark_value(part.second)
check_remark_part(part) =
    throw(ArgumentError("Invalid part of a remark message: $(repr(part)); expected a string or a `key => value` pair"))
check_remark_value(::Union{Value,LLVMType,AbstractString,Bool,Integer}) = nothing
check_remark_value(value) =
    throw(ArgumentError("Invalid value of a remark argument: $(repr(value)); expected a value, a type, a string or an integer"))

append_remark_part!(ref, part::AbstractString) =
    API.LLVMExtraOptimizationRemarkAppendString(ref, part)
append_remark_part!(ref, part::Pair) = append_remark_arg!(ref, part.first, part.second)
append_remark_arg!(ref, key, value::Value) =
    API.LLVMExtraOptimizationRemarkAppendValue(ref, key, value)
append_remark_arg!(ref, key, value::LLVMType) =
    API.LLVMExtraOptimizationRemarkAppendType(ref, key, value)
append_remark_arg!(ref, key, value::AbstractString) =
    API.LLVMExtraOptimizationRemarkAppendNamedString(ref, key, value)
append_remark_arg!(ref, key, value::Bool) =
    API.LLVMExtraOptimizationRemarkAppendBool(ref, key, value)
append_remark_arg!(ref, key, value::Signed) =
    API.LLVMExtraOptimizationRemarkAppendInt(ref, key, Int64(value))
append_remark_arg!(ref, key, value::Unsigned) =
    API.LLVMExtraOptimizationRemarkAppendUnsigned(ref, key, UInt64(value))
append_remark_arg!(ref, key, value::Integer) =
    value < 0 ? API.LLVMExtraOptimizationRemarkAppendInt(ref, key, Int64(value)) :
                API.LLVMExtraOptimizationRemarkAppendUnsigned(ref, key, UInt64(value))

function emit_remark(am, kind::Symbol, pass_name::AbstractString,
                     remark_name::AbstractString, inst::Instruction, parts)
    api_kind = remark_kind(kind)
    foreach(check_remark_part, parts)
    ref = API.LLVMExtraCreateOptimizationRemark(api_kind, pass_name, remark_name, inst)
    try
        foreach(part -> append_remark_part!(ref, part), parts)
    catch
        API.LLVMExtraDisposeOptimizationRemark(ref)
        rethrow()
    end
    # this disposes of the remark
    API.LLVMExtraEmitOptimizationRemark(ref, am)
    return
end

function check_remark_inst(fun::Function, inst::Instruction)
    bb = parent(inst)
    bb !== nothing && parent(bb) == fun ||
        throw(ArgumentError("Instruction is not part of function $(fun.name)"))
end
check_remark_target(am::FunctionAnalysisManager, inst::Instruction) =
    check_remark_inst(am.fun, inst)
function check_remark_target(ar::LoopAnalysisResults, inst::Instruction)
    ar.valid || throw(ArgumentError("Loop analysis results cannot be used after the pass returned"))
    check_remark_inst(ar.fun, inst)
end

# loop passes have no access to the function's cached remark emitter
remark_emitter(am::FunctionAnalysisManager) = am
remark_emitter(ar::LoopAnalysisResults) = C_NULL

"""
    remark!(f, am::FunctionAnalysisManager, kind::Symbol, pass_name, remark_name,
            inst::Instruction)
    remark!(f, ar::LoopAnalysisResults, kind::Symbol, pass_name, remark_name,
            inst::Instruction)
    remark!(am_or_ar, kind::Symbol, pass_name, remark_name, inst::Instruction, message...)

Emit an optimization remark about the instruction `inst` of the function of `am` (or of a
loop pass, given its [`LoopAnalysisResults`](@ref)), like LLVM's passes do using an
`OptimizationRemarkEmitter`: with `kind=:passed` for an optimization that the pass
performed, `kind=:missed` for one that it could not perform, or `kind=:analysis` for a
fact that explains a decision. The `pass_name` and the `remark_name` identify the remark.

The message of the remark is made of parts, each of which is either a string, or an
argument `key => value` whose value is a value, a type, a string or an integer, like the
`ore::NV(key, value)` arguments of LLVM's remarks. LLVM renders these arguments, so that
they read like those of its own passes: a value as its name if it is an argument or a
global, a constant as an operand, an intrinsic call as `call` and the name of the
intrinsic (on LLVM 21 and later), and any other instruction as its opcode. The remark
also carries the arguments separately, with the source location of a value that has one,
as can be seen in a [`RemarkFile`](@ref).

In the preferred do-block form, `f()` returns the message, as a single part or as a
collection of parts, and is only called when remarks of this kind are wanted for the pass
(see [`remark_enabled`](@ref)), so passes don't pay for building remarks that are
discarded:

```julia
remark!(am, :passed, "my-pass", "Hoisted", inst) do
    "hoisting ", "Inst" => inst, " out of the loop"
end
```

Otherwise, the remark is always emitted, with the parts of the message passed as the
final arguments.

Remarks are passed to the diagnostic handler of the context, where they can be collected
using the `remarks` keyword argument of [`PassBuilder`](@ref) (also accepted by `run!`),
or written to a file using a [`RemarkFile`](@ref). Otherwise, they are logged as debug
messages.
"""
function remark!(am::Union{FunctionAnalysisManager,LoopAnalysisResults}, kind::Symbol,
                 pass_name::AbstractString, remark_name::AbstractString,
                 inst::Instruction, message...)
    check_remark_target(am, inst)
    emit_remark(remark_emitter(am), kind, pass_name, remark_name, inst, message)
end

function remark!(f::Base.Callable, am::Union{FunctionAnalysisManager,LoopAnalysisResults},
                 kind::Symbol, pass_name::AbstractString, remark_name::AbstractString,
                 inst::Instruction)
    check_remark_target(am, inst)
    remark_enabled(context(inst), kind, pass_name) || return
    message = f()
    parts = message isa Union{AbstractString,Pair} ? (message,) : message
    emit_remark(remark_emitter(am), kind, pass_name, remark_name, inst, parts)
end

"""
    Remark

An optimization remark, as collected by a pass builder with a `remarks` callback (see
[`PassBuilder`](@ref)) and emitted by a custom pass using [`remark!`](@ref) or by one of
LLVM's passes. Its fields are:

- `kind`: `:passed`, `:missed` or `:analysis`;
- `pass_name`, `name`: the names of the pass and of the remark;
- `function_name`: the name of the function the remark is about;
- `message`: the message of the remark;
- `file`, `line`, `column`: the source location of the remark, from debug information,
  or an empty file name if it is unknown. LLVM joins the directory and the name of the file
  with the platform's path separator;
- `arguments`: the parts that make up the message, as `key => value` pairs of strings,
  with the key `"String"` for plain text, and LLVM's rendering of other values.
"""
struct Remark
    kind::Symbol
    pass_name::String
    name::String
    function_name::String
    message::String
    file::String
    line::Int
    column::Int
    arguments::Vector{Pair{String,String}}
end

# the remark of a diagnostic, or nothing if it isn't an optimization remark
function Remark(di::DiagnosticInfo)
    kind = Ref{API.LLVMExtraRemarkKind}()
    strs = [Ref{Cstring}() for _ in 1:5]
    line = Ref{Cuint}()
    column = Ref{Cuint}()
    Bool(API.LLVMExtraGetRemarkInfo(di, kind, strs..., line, column)) || return nothing
    pass_name, name, function_name, message, file = map(ref -> unsafe_message(ref[]), strs)
    kind_sym = kind[] == API.LLVMExtraRemarkPassed ? :passed :
               kind[] == API.LLVMExtraRemarkMissed ? :missed : :analysis
    arguments = map(0:Int(API.LLVMExtraGetRemarkNumArguments(di))-1) do i
        key = Ref{Cstring}()
        value = Ref{Cstring}()
        API.LLVMExtraGetRemarkArgument(di, i, key, value)
        unsafe_message(key[]) => unsafe_message(value[])
    end
    return Remark(kind_sym, pass_name, name, function_name, message, file, line[],
                  column[], arguments)
end

function Base.show(io::IO, remark::Remark)
    print(io, "Remark(:", remark.kind, ", ", repr(remark.pass_name), ", ",
          repr(remark.name), ", ", repr(remark.function_name), ", ", repr(remark.message),
          ")")
end

"""
    RemarkFile(path; passes="", format="yaml")
    RemarkFile(f, path; kwargs...)

Write the optimization remarks emitted in the current context to the file at `path`,
like the `-pass-remarks-output` option of LLVM's tools, until the remark file is disposed
of. The `format` is any that LLVM supports for remark files: `"yaml"`, or, on LLVM 22 and
later, `"bitstream"` (older versions of LLVM write bitstream remarks for a string table
that is stored elsewhere, in an object file). `passes` is a regular expression that limits
the remarks to those of the passes whose name it matches. Remarks are written regardless of whether they are
enabled using `-pass-remarks` options, also by LLVM's own passes, and still passed to the
diagnostic handler of the context (e.g., the `remarks` callback of a [`PassBuilder`](@ref)).

A context can only write remarks to one file at a time, and the remark file needs to be
disposed of before the context, e.g., using the do-block form.
"""
mutable struct RemarkFile
    ref::API.LLVMRemarkFileRef
    library::Library
end

argument_library(file::RemarkFile) = file.library

Base.unsafe_convert(::Type{API.LLVMRemarkFileRef}, file::RemarkFile) = mark_use(file).ref

function RemarkFile(path::AbstractString; passes::AbstractString="",
                    format::AbstractString="yaml")
    ctx = context()
    error_ref = Ref{Cstring}()
    ref = API.LLVMExtraCreateRemarkFile(ctx, path, passes, format, error_ref)
    ref == C_NULL && throw(LLVMException(unsafe_message(error_ref[])))
    # not owned by the context, which doesn't dispose of it
    mark_alloc(RemarkFile(ref, ctx.library))
end

RemarkFile(f::Core.Function, args...; kwargs...) =
    with_disposal(f, RemarkFile(args...; kwargs...))

dispose(file::RemarkFile) = mark_dispose(API.LLVMExtraDisposeRemarkFile, file)


## pass builder

@vocabulary Passes PassBuilder, register!, add!, run!

"""
    PassBuilder(; verify_each=false, debug_logging=false, pipeline_tuning_kwargs...)
    PassBuilder(f; kwargs...)

Create a new pass builder. The pass builder is the main object used to construct and run
pass pipelines. The `verify_each` keyword argument enables module verification after each
pass, while `debug_logging` can be used to enable more output. Pass builder objects need to
be disposed of after use, e.g., using `@dispose` or the do-block form.

Several other keyword arguments can override LLVM's version- and configuration-dependent
pipeline defaults. This only has an effect when using one of LLVM's default pipelines,
like `default<O3>`:

- `loop_interleaving::Bool`: Enable loop interleaving.
- `loop_vectorization::Bool`: Enable loop vectorization.
- `slp_vectorization::Bool`: Enable SLP vectorization.
- `loop_unrolling::Bool`: Enable loop unrolling.
- `forget_all_scev_in_loop_unroll::Bool`: Forget all SCEV information in loop
  unrolling.
- `licm_mssa_opt_cap::Int`: LICM MSSA optimization cap.
- `licm_mssa_no_acc_for_promotion_cap::Int`: LICM MSSA no access for promotion cap.
- `call_graph_profile::Bool`: Enable call graph profiling.
- `merge_functions::Bool`: Enable function merging.

To collect the optimization remarks that passes emit, e.g., using [`remark!`](@ref), pass a
function as the `remarks` keyword argument. It is called with each [`Remark`](@ref) while
the passes run, instead of the remark being logged as a debug message. LLVM's own passes
only emit remarks when they are enabled using LLVM's `-pass-remarks`, `-pass-remarks-missed`
or `-pass-remarks-analysis` command-line options. Exceptions thrown by the function are
rethrown as `PassException` by `run!`. This requires a context created by LLVM.jl.

After a pass builder is constructed, custom passes can be registered with `register!`,
passes or nested pass managers can be added with `add!`, and finally the passes can be run
with `run!`:

```julia
@dispose pb = PassBuilder(verify_each=true) begin
    register!(pb, SomeCustomPass())
    add!(pb, SomeModulePass())
    add!(pb, FunctionPassManager()) do fpm
        add!(fpm, SomeFunctionPass())
    end
    run!(pb, mod, tm)
end
```

For quickly running a simple pass or pipeline, a shorthand `run!` method is provided that
obviates the construction of a `PassBuilder`:

```julia
run!("some-pass", mod, tm; verify_each=true)
```

See also: [`register!`](@ref), [`add!`](@ref), [`run!`](@ref)
"""
mutable struct PassBuilder <: AbstractPassManager
    opts::API.LLVMPassBuilderOptionsRef
    const library::Library
    passes::Vector{String}
    aa_passes::Vector{String}
    custom_passes::Vector{CustomPass}
    custom_tti::Union{AbstractTargetTransformInfo,Nothing}
    registration_callbacks::Vector{Ptr{Cvoid}}
    remarks::Any
end

Base.string(pm::PassBuilder) = join(pm.passes, ",")

Base.unsafe_convert(::Type{API.LLVMPassBuilderOptionsRef}, pb::PassBuilder) =
    mark_use(pb).opts

argument_library(pb::PassBuilder) = pb.library

function PassBuilder(; kwargs...)
    opts = API.LLVMCreatePassBuilderOptions()
    obj = mark_alloc(PassBuilder(opts, library(), String[], String[], CustomPass[], nothing,
                                 Ptr{Cvoid}[], nothing))

    # dispose of the options if a keyword argument is invalid
    try
        for (name, value) in pairs(kwargs)
            if name == :verify_each
                API.LLVMPassBuilderOptionsSetVerifyEach(obj, value)
            elseif name == :debug_logging
                API.LLVMPassBuilderOptionsSetDebugLogging(obj, value)
            elseif name == :loop_interleaving
                API.LLVMPassBuilderOptionsSetLoopInterleaving(obj, value)
            elseif name == :loop_vectorization
                API.LLVMPassBuilderOptionsSetLoopVectorization(obj, value)
            elseif name == :slp_vectorization
                API.LLVMPassBuilderOptionsSetSLPVectorization(obj, value)
            elseif name == :loop_unrolling
                API.LLVMPassBuilderOptionsSetLoopUnrolling(obj, value)
            elseif name == :forget_all_scev_in_loop_unroll
                API.LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll(obj, value)
            elseif name == :licm_mssa_opt_cap
                API.LLVMPassBuilderOptionsSetLicmMSSAOptCap(obj, value)
            elseif name == :licm_mssa_no_acc_for_promotion_cap
                API.LLVMPassBuilderOptionsSetLicmMSSANoAccForPromotionCap(obj, value)
            elseif name == :call_graph_profile
                API.LLVMPassBuilderOptionsSetCallGraphProfile(obj, value)
            elseif name == :merge_functions
                API.LLVMPassBuilderOptionsSetMergeFunctions(obj, value)
            elseif name == :remarks
                obj.remarks = value
            else
                throw(ArgumentError("invalid keyword argument $name"))
            end
        end
    catch
        dispose(obj)
        rethrow()
    end

    return obj
end

PassBuilder(f::Core.Function; kwargs...) = with_disposal(f, PassBuilder(; kwargs...))

dispose(pb::PassBuilder) = mark_dispose(API.LLVMDisposePassBuilderOptions, pb)

"""
    register!(pb, custom_pass)

Register a custom pass with the pass builder. This is necessary before the pass can be
used in a pass pipeline.

See also: [`ModulePass`](@ref), [`FunctionPass`](@ref)
"""
function register!(pb::PassBuilder, pass::CustomPass)
    any(p -> p.type === pass.type && p.name == pass.name, pb.custom_passes) &&
        throw(ArgumentError("pass $(pass.name) is already registered for $(pass.type)"))
    push!(pb.custom_passes, pass)
    return pb
end

@vocabulary Passes register_callbacks!

"""
    register_callbacks!(pb::PassBuilder, callback::Ptr{Cvoid})

Register a native callback that is called with LLVM's C++ `PassBuilder` (as a `void *`)
when the pass builder is used to run passes. This makes it possible to use passes that are
implemented in C++, by calling the `PassBuilder`'s `register*Callback` methods, like pass
plugins do. For example, for a library that provides a `registerCallbacks` function:

```julia
register_callbacks!(pb, cglobal((:registerCallbacks, libfoo)))
```

The callback needs to have the signature `void (*)(void *)`, and the library that provides
it needs to be built against the same version of LLVM as the one that Julia uses, and remain
loaded while the pass builder is used. Callbacks are called in the order they were
registered, after LLVM.jl registers Julia's passes, and must not throw Julia exceptions. To
implement a pass in Julia instead, use [`register!`](@ref).
"""
function register_callbacks!(pb::PassBuilder, callback::Ptr{Cvoid})
    callback == C_NULL && throw(ArgumentError("Registration callback cannot be NULL"))
    push!(pb.registration_callbacks, callback)
    return pb
end

@vocabulary Passes target_transform_info!

function install_custom_tti!(exts::API.LLVMPassBuilderExtensionsRef,
                              tti::AbstractTargetTransformInfo)
    state, opts = build_custom_tti_options(tti)
    API.LLVMPassBuilderExtensionsSetTTI(exts, opts)
    # `SetTTI` copies the options into the extensions, so `opts` can be freed
    # right away. `state` backs the `UserData` pointer the C++ side kept and
    # must stay GC-rooted until `run!` completes.
    API.LLVMDisposeTTIOptions(opts)
    return state
end

"""
    target_transform_info!(pb::PassBuilder, tti::AbstractTargetTransformInfo)
    target_transform_info!(pb::PassBuilder, ::Nothing)

Attach an [`AbstractTargetTransformInfo`](@ref) subtype instance to the pass
builder, replacing any previously-attached custom TTI. Pass `nothing` to
revert to LLVM's native TTI (derived from the `TargetMachine`, if any;
otherwise `TargetTransformInfoImplBase` with full `DataLayout`/`Module`-aware
defaults).
"""
function target_transform_info!(pb::PassBuilder,
                                tti::AbstractTargetTransformInfo)
    pb.custom_tti = tti
    return pb
end

function target_transform_info!(pb::PassBuilder, ::Nothing)
    pb.custom_tti = nothing
    return pb
end

"""
    run!(pb::PassBuilder, mod::Module, [tm::TargetMachine])
    run!(pipeline::AbstractString, mod::Module, [tm::TargetMachine])

Run passes on a module. The passes are specified by a pass builder or a string that
represents a pass pipeline. The target machine is used to optimize the passes.

The target's context and library are active while the passes run, including in custom
pass callbacks. The caller's context and library are restored afterwards.
For a function target, select its library before calling `run!`, since function wrappers
do not remember their library.
"""
run!

function run!(pb::PassBuilder, target::Union{Module,Function}, tm::Union{Nothing,TargetMachine}=nothing)
    isempty(pb.passes) && return
    # passes run with the library of the module, which may not be the current one
    lib = target isa Module ? target.library : library()
    check_library(lib, pb, tm)
    context!(() -> _run!(pb, target, tm), context(target))
end

function _run!(pb::PassBuilder, target::Union{Module,Function},
               tm::Union{Nothing,TargetMachine})
    pipeline = join(pb.passes, ",")
    # without an AA manager, LLVM uses its default alias analyses
    aa_pipeline = isempty(pb.aa_passes) ? nothing : join(filter(!isempty, pb.aa_passes), ",")

    # XXX: The Base API is too restricted, not supporting custom passes
    #      or Julia's pass registration callback
    #@check API.LLVMRunPasses(mod, string(pb), tm, pb.opts)

    # the extensions only live for the duration of this run, as they hold references to
    # the state of the callbacks (which would be stale during a later run)
    exts = API.LLVMCreatePassBuilderExtensions()
    try
        run_passes!(pb, exts, target, tm, pipeline, aa_pipeline)
    finally
        API.LLVMDisposePassBuilderExtensions(exts)
    end
end

function run_passes!(pb::PassBuilder, exts::API.LLVMPassBuilderExtensionsRef,
                     target::Union{Module,Function}, tm::Union{Nothing,TargetMachine},
                     pipeline::String, aa_pipeline::Union{Nothing,String})
    # Create state objects to hold callbacks and any caught exceptions
    states = [CustomPassState(pass, exts) for pass in pb.custom_passes]
    tti_state = pb.custom_tti === nothing ? nothing :
                install_custom_tti!(exts, pb.custom_tti)
    ctx = context(target)
    prepare_diagnostic(ctx)
    diag_state = diagnostic_state(ctx)
    if pb.remarks !== nothing
        diag_state === nothing &&
            throw(ArgumentError("Remarks can only be collected in a context created by LLVM.jl"))
        # support nested runs, e.g., by a custom pass
        previous_remarks = (diag_state.remark_callback, diag_state.remark_exception)
        diag_state.remark_callback = pb.remarks
        diag_state.remark_exception = nothing
    end
    try
        run_pipeline!(pb, exts, target, tm, pipeline, aa_pipeline, states, tti_state, ctx)
        if pb.remarks !== nothing && diag_state.remark_exception !== nothing
            throw(PassException(diag_state.remark_exception...))
        end
    finally
        if pb.remarks !== nothing
            diag_state.remark_callback, diag_state.remark_exception = previous_remarks
        end
    end
end

function run_pipeline!(pb::PassBuilder, exts::API.LLVMPassBuilderExtensionsRef,
                       target::Union{Module,Function}, tm::Union{Nothing,TargetMachine},
                       pipeline::String, aa_pipeline::Union{Nothing,String}, states,
                       tti_state, ctx)
    GC.@preserve states tti_state aa_pipeline begin
        register_extensions!(exts, pb, states)

        # register AA pipeline
        if aa_pipeline !== nothing
            if atleast(20, pb.library)
                API.LLVMPassBuilderOptionsSetAAPipeline(pb, aa_pipeline)
            else
                API.LLVMPassBuilderExtensionsSetAAPipeline(exts, aa_pipeline)
            end
        end

        err = try
            if target isa Module
                API.LLVMRunJuliaPasses(target, pipeline, something(tm, C_NULL), pb, exts)
            elseif target isa Function
                API.LLVMRunJuliaPassesOnFunction(target, pipeline, something(tm, C_NULL),
                                                 pb, exts)
            end
        finally
            # the options keep a pointer to the AA pipeline, which is only valid during the run
            if aa_pipeline !== nothing && atleast(20, pb.library)
                API.LLVMPassBuilderOptionsSetAAPipeline(pb, C_NULL)
            end
        end

        check_custom_passes(err, states, pb.library, tti_state)
        check_diagnostic(ctx)
    end
end

# register the custom passes and the registration callbacks of a pass builder with the
# extensions that LLVM.jl passes to LLVM. the states of the custom passes need to be
# preserved for as long as the extensions are used.
function register_extensions!(exts::API.LLVMPassBuilderExtensionsRef, pb::PassBuilder,
                              states::Vector{CustomPassState})
    # register custom passes
    for (i,pass) in enumerate(pb.custom_passes)
        if pass.parse_options !== nothing
            parse_cb = @cfunction(parse_options_callback, Ptr{Cvoid},
                                  (Cstring, Ptr{Cvoid}))
            if pass.type === :module
                cb = @cfunction(module_callback_with_options, Cvoid,
                                (API.LLVMModuleRef, API.LLVMModuleAnalysisManagerRef,
                                 API.LLVMPreservedAnalysesRef, Ptr{Cvoid}))
                api = API.LLVMExtraPassBuilderExtensionsRegisterParametrizedModulePass
            elseif pass.type === :function
                cb = @cfunction(function_callback_with_options, Cvoid,
                                (API.LLVMValueRef, API.LLVMFunctionAnalysisManagerRef,
                                 API.LLVMPreservedAnalysesRef, Ptr{Cvoid}))
                api = API.LLVMExtraPassBuilderExtensionsRegisterParametrizedFunctionPass
            elseif pass.type === :loop
                cb = @cfunction(loop_callback_with_options, Cvoid,
                                (API.LLVMLoopRef, API.LLVMLoopAnalysisManagerRef,
                                 API.LLVMLoopStandardAnalysisResultsRef,
                                 API.LLVMLPMUpdaterRef, API.LLVMPreservedAnalysesRef,
                                 Ptr{Cvoid}))
                api = API.LLVMExtraPassBuilderExtensionsRegisterParametrizedLoopPass
            else
                throw(ArgumentError("invalid pass type $(pass.type)"))
            end
            api(exts, pass.name, cb, parse_cb, Ref(states, i), pass.required)
            continue
        elseif pass.type === :module && pass.analyses
            cb = @cfunction(module_callback_with_analyses, Cvoid,
                            (API.LLVMModuleRef, API.LLVMModuleAnalysisManagerRef,
                             API.LLVMPreservedAnalysesRef, Ptr{Cvoid}))
            api = API.LLVMExtraPassBuilderExtensionsRegisterModulePassWithAnalyses
        elseif pass.type === :module
            cb = @cfunction(module_callback, Bool, (API.LLVMModuleRef, Ptr{Cvoid}))
            api = API.LLVMPassBuilderExtensionsRegisterModulePassWithRequired
        elseif pass.type === :function && pass.analyses
            cb = @cfunction(function_callback_with_analyses, Cvoid,
                            (API.LLVMValueRef, API.LLVMFunctionAnalysisManagerRef,
                             API.LLVMPreservedAnalysesRef, Ptr{Cvoid}))
            api = API.LLVMExtraPassBuilderExtensionsRegisterFunctionPassWithAnalyses
        elseif pass.type === :function
            cb = @cfunction(function_callback, Bool, (API.LLVMValueRef, Ptr{Cvoid}))
            api = API.LLVMPassBuilderExtensionsRegisterFunctionPassWithRequired
        elseif pass.type === :loop
            cb = @cfunction(loop_callback, Cvoid,
                            (API.LLVMLoopRef, API.LLVMLoopAnalysisManagerRef,
                             API.LLVMLoopStandardAnalysisResultsRef,
                             API.LLVMLPMUpdaterRef, API.LLVMPreservedAnalysesRef,
                             Ptr{Cvoid}))
            api = API.LLVMExtraPassBuilderExtensionsRegisterLoopPass
        else
            throw(ArgumentError("invalid pass type $(pass.type)"))
        end
        api(exts, pass.name, cb, Ref(states, i), pass.required)
    end

    # register Julia passes, which only work with Julia's LLVM
    if pb.library === julia_library
        julia_callback = cglobal(:jl_register_passbuilder_callbacks)
        API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, julia_callback)
    end

    # register native callbacks
    for callback in pb.registration_callbacks
        API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, callback)
    end
end

# throw the error that LLVM returned, or an exception that was caught in a custom pass
# callback, which takes precedence over LLVM's errors (e.g., the failure to parse the
# pipeline when parsing the options of a pass threw)
function check_custom_passes(err::API.LLVMErrorRef, states, lib::Library,
                             tti_state=nothing)
    exception = nothing
    for state in states
        if state.exception !== nothing
            exception = state.exception
            break
        end
    end
    if exception === nothing && tti_state !== nothing
        exception = tti_state.exception
    end
    if exception !== nothing
        err == C_NULL || API.LLVMConsumeError(err)
        throw(PassException(exception...))
    end
    if err != C_NULL
        exception = convert(LLVMException, LLVMError(err, lib))
        check_julia_pass_names(exception, lib)
        throw(exception)
    end
end

@vocabulary Passes print_pipeline

"""
    print_pipeline([io::IO], pb::PassBuilder, [tm::TargetMachine])

Print the pipeline of a pass builder as LLVM parses it, like LLVM's `opt
-print-pipeline-passes` does: with the pass managers that LLVM inserts, every pass under the
name it is known by, and the options of passes that take them. Pipelines like
`"default<O2>"` are printed as the passes that they consist of. The pipeline is parsed at
the module level, with the custom passes that were registered with the pass builder, and
with the passes of the target machine `tm`, but it is not run.

The printed pipeline can be added to a pass builder again. LLVM prints passes in this way
on a best-effort basis, though: some passes are printed without some of their options, or
under a name that cannot be parsed.

```jldoctest; setup = :(using LLVM, LLVM.Passes)
julia> @dispose pb=PassBuilder() begin
         add!(pb, "globaldce,instsimplify")
         print_pipeline(pb)
       end
globaldce,function(instsimplify)
```
"""
print_pipeline(pb::PassBuilder, args...) = print_pipeline(stdout, pb, args...)

function print_pipeline(io::IO, pb::PassBuilder, tm::Union{Nothing,TargetMachine}=nothing)
    # the pipeline is parsed by the library of the pass builder
    check_library(pb.library, tm)
    library!(() -> _print_pipeline(io, pb, tm), pb.library)
end

function _print_pipeline(io::IO, pb::PassBuilder, tm::Union{Nothing,TargetMachine})
    states = [CustomPassState(pass) for pass in pb.custom_passes]
    exts = API.LLVMCreatePassBuilderExtensions()
    try
        GC.@preserve states begin
            register_extensions!(exts, pb, states)
            out = Ref{Cstring}()
            err = API.LLVMExtraPrintJuliaPasses(string(pb), something(tm, C_NULL), pb, exts,
                                                out)
            check_custom_passes(err, states, pb.library)
            print(io, unsafe_message(out[]))
        end
    finally
        API.LLVMDisposePassBuilderExtensions(exts)
    end
    return
end

"""
    run!(pb::PassBuilder, f::Function, am::FunctionAnalysisManager, [tm::TargetMachine])
    run!(pipeline::AbstractString, f, am, [tm])
    run!(pass::CustomPass, f, am, [tm])
        -> PreservedAnalyses

Run the passes of a pass builder on a function, from a custom function pass that runs on
it with the function analysis manager `am` (see [`FunctionPass`](@ref)). The passes run as
part of the enclosing pipeline: like the passes of a nested function pass manager, they use
and update the analyses of that pipeline, and its instrumentation reports them. The
pipeline is parsed as a function pipeline, with the passes that were registered with the
pass builder and those of the target machine `tm`.

Returns the analyses that the passes preserved, which LLVM computes like for a nested
function pass manager: the analyses that a pass did not preserve have been invalidated
after it ran, so when the passes changed the function, this preserves
[`FunctionAnalyses`](@ref AllAnalyses). This can be returned by the custom pass, or
intersected with what the rest of the pass preserves (see [`PreservedAnalyses`](@ref)).
`PreservedAnalyses` only describes the analyses that LLVM.jl knows, so analyses that the
passes preserve individually, but that are not among them, are not preserved by what is
returned. As a result, LLVM may recompute more analyses after the custom pass than after
the passes themselves.

The alias analyses, the target transform info and the instrumentation are those of the
enclosing pipeline: the pass builder's options for them (like `verify_each` or
`debug_logging`) have no effect, and it cannot have an [`AAManager`](@ref), a custom target
transform info or collect remarks. If one of the passes fails, the passes that remain are
skipped, and `run!` throws a `PassException`, which fails the calling pass unless it catches
it.

For example, a pass that unrolls loops, and cleans up after that if it changed anything:

```julia
function unroll_and_clean_up!(f::LLVM.Function, am::LLVM.FunctionAnalysisManager)
    preserved = run!(LoopUnrollPass(), f, am)
    preserved == PreservedAnalyses(AllAnalyses) && return preserved
    @dispose pb=PassBuilder() begin
        add!(pb, SimplifyCFGPass())
        add!(pb, InstCombinePass())
        return intersect(preserved, run!(pb, f, am))
    end
end
FunctionPass("unroll-and-clean-up", unroll_and_clean_up!; analyses=true)
```
"""
function run!(pb::PassBuilder, f::Function, am::FunctionAnalysisManager,
              tm::Union{Nothing,TargetMachine}=nothing)
    am.fun == f ||
        throw(ArgumentError("The analysis manager belongs to function $(am.fun.name), not $(f.name)"))
    isempty(pb.aa_passes) && pb.custom_tti === nothing && pb.remarks === nothing ||
        throw(ArgumentError("Passes that run from a pass use the alias analyses, target transform info and remarks of the enclosing pipeline"))
    isempty(pb.passes) && return PreservedAnalyses(AllAnalyses)
    check_library(library(), pb, tm)
    context!(() -> _run!(pb, f, am, tm), context(f))
end

function _run!(pb::PassBuilder, f::Function, am::FunctionAnalysisManager,
               tm::Union{Nothing,TargetMachine})
    exts = API.LLVMCreatePassBuilderExtensions()
    states = [CustomPassState(pass, exts) for pass in pb.custom_passes]
    try
        GC.@preserve states begin
            register_extensions!(exts, pb, states)
            ref = Ref{API.LLVMPreservedAnalysesRef}()
            err = API.LLVMExtraRunJuliaPassesWithAnalyses(f, string(pb), something(tm, C_NULL),
                                                          pb, exts, am, ref)
            try
                check_custom_passes(err, states, pb.library)
                return PreservedAnalyses(ref[])
            finally
                ref[] == C_NULL || API.LLVMExtraDisposePreservedAnalyses(ref[])
            end
        end
    finally
        API.LLVMDisposePassBuilderExtensions(exts)
    end
end

function run!(pass::CustomPass, f::Function, am::FunctionAnalysisManager,
              tm::Union{Nothing,TargetMachine}=nothing; kwargs...)
    pass.type === :function ||
        throw(ArgumentError("Cannot run $(pass.type) pass $(pass.name) on a function"))
    @dispose pb=PassBuilder(; kwargs...) begin
        register!(pb, pass)
        add!(pb, pass)
        run!(pb, f, am, tm)
    end
end

function run!(pass::AbstractString, args...; kwargs...)
    library!(route_library(args...)) do
        @dispose pb=PassBuilder(; kwargs...) begin
            add!(pb, pass)
            run!(pb, args...)
        end
    end
end

"""
    run!(pass::CustomPass, target::Union{Module,Function}, [tm::TargetMachine]; kwargs...)

Run a single custom pass on a module or a function, using a temporary pass builder that
is created with the given keyword arguments (see [`PassBuilder`](@ref)). A function pass
that runs on a module runs on each of its functions, while a module pass cannot run on a
function. A loop pass runs on each loop of the function(s) in a `loop(...)` pipeline,
i.e., without MemorySSA. Like for passes that run in a pipeline, exceptions thrown by the
pass are rethrown as a [`PassException`](@ref).
"""
function run!(pass::CustomPass, target::Union{Module,Function},
              tm::Union{Nothing,TargetMachine}=nothing; kwargs...)
    if pass.type === :module && target isa Function
        throw(ArgumentError("Cannot run module pass $(pass.name) on a function"))
    end
    library!(route_library(target, tm)) do
        @dispose pb=PassBuilder(; kwargs...) begin
            register!(pb, pass)
            if pass.type === :function && target isa Module
                add!(pb, FunctionPassManager()) do fpm
                    add!(fpm, pass)
                end
            elseif pass.type === :loop
                lpm = LoopPassManager()
                add!(lpm, pass)
                if target isa Module
                    add!(pb, FunctionPassManager()) do fpm
                        add!(fpm, lpm)
                    end
                else
                    add!(pb, lpm)
                end
            else
                add!(pb, pass)
            end
            run!(pb, target, tm)
        end
    end
end


## pass definitions

# convert Julia keyword arguments to a LLVM pass parameter string
function kwargs_to_params(kwargs)
    isempty(kwargs) && return ""

    params = String[]
    for (k, v) in kwargs
        # Julia uses `_` in kwargs, while LLVM always uses `-`
        k = replace(string(k), "_" => "-")

        if v isa Bool
            push!(params, v ? k : "no-$k")
        else
            push!(params, "$k=$v")
        end
    end
    "<" * join(params, ";") * ">"
end

# the functions that return the names of passes, for the reference documentation
const pass_functions = Tuple{Core.Module,Symbol,String}[]

# the passes that the functions defined by `@module_pass` etc. return, by function name.
# some passes were renamed, or changed kind, so a function can have several entries, of
# which the one for the version of the current library is used.
struct PassEntry
    name::String
    kind::String
    # the first and (exclusive) last major versions of LLVM that have the pass, if limited
    since::Union{Nothing,Int}
    until::Union{Nothing,Int}
    # whether it is one of Julia's passes, which only work with Julia's LLVM
    julia::Bool
end
const pass_entries = Dict{Symbol,Vector{PassEntry}}()
# all entries, in the order they were defined
const pass_table = PassEntry[]

isavailable(entry::PassEntry, lib::Library) =
    (entry.since === nothing || atleast(entry.since, lib)) &&
    (entry.until === nothing || !atleast(entry.until, lib)) &&
    (!entry.julia || lib === julia_library)

# the name of a pass, for the current library
function pass_name(class_name::Symbol)
    lib = library()
    entries = pass_entries[class_name]
    for entry in entries
        isavailable(entry, lib) && return entry.name
    end
    throw_unavailable_pass(class_name, entries, lib)
end

@noinline function throw_unavailable_pass(class_name, entries, lib)
    any(entry -> entry.julia, entries) &&
        check_julia_library(string(class_name), lib)
    error("$class_name is not available on LLVM $(lib.version) (only on $(pass_availability(entries)))")
end

function pass_availability(entries)
    join(map(entries) do entry
        if entry.since === nothing && entry.until === nothing
            "every version"
        elseif entry.since === nothing
            "LLVM $(entry.until-1) and earlier"
        elseif entry.until === nothing
            "LLVM $(entry.since) and later"
        else
            "LLVM $(entry.since) to $(entry.until-1)"
        end
    end, ", ")
end

function define_pass(mod, pass_name, class_name, kind, define_class, since, until, julia)
    entry = PassEntry(pass_name, kind, since, until, julia)
    # some passes work with multiple types of managers, or have been renamed, so only
    # define their function once
    defined = haskey(pass_entries, class_name)
    push!(get!(Vector{PassEntry}, pass_entries, class_name), entry)
    push!(pass_table, entry)
    if defined || isdefined(LLVM, class_name)
        return
    end
    push!(pass_functions, (mod, class_name, kind))

    # LLVM's passes are part of the Passes vocabulary, while passes defined elsewhere
    # (e.g., Julia's passes in LLVM.Interop) are exported by their module
    ex = if mod === LLVM
        quote
            $(esc(:(@vocabulary Passes $class_name)))
        end
    else
        quote
            export $(esc(class_name))
        end
    end
    if define_class
        # the documentation is attached once all passes are defined (see `document_passes`)
        push!(ex.args, :(
            function $(esc(class_name))(; kwargs...)
                return $(GlobalRef(LLVM, :pass_name))($(QuoteNode(class_name))) *
                       kwargs_to_params(kwargs)
            end
        ))
    end
    return ex
end

# document the functions that return the names of passes, mentioning all of their names
function document_passes(mod::Core.Module)
    for (mod′, class_name, _) in pass_functions
        mod′ === mod || continue
        entries = get(pass_entries, class_name, nothing)
        entries === nothing && continue
        f = getfield(mod, class_name)
        # passes defined manually are documented with their definition
        haskey(Base.Docs.meta(mod), Base.Docs.Binding(mod, class_name)) && continue
        main = first(entries)
        aa = main.kind == "alias analysis" ? " (in an [`AAManager`](@ref))" : ""
        options = occursin('<', main.name) ? "" : """

            Keyword arguments become options of the pass, e.g., `$class_name(; foo=true, bar=2)`
            is `"$(main.name)<foo;bar=2>"`, while `false` values become `no-` options."""
        # passes that work with several kinds of pass managers, on every version
        unlimited = filter(e -> e.since === nothing && e.until === nothing, entries)
        also = filter(!=(main.kind), unique(e.kind for e in unlimited if e.name == main.name))
        versions = isempty(also) ? "" :
                   "\n\nIt can also be used as a " * join(also, ", ", " or ") * "."
        # passes that were added, removed, renamed or changed kind
        limited = filter(e -> e.since !== nothing || e.until !== nothing, entries)
        if length(limited) > 1
            versions *= "\n\nDepending on the version of LLVM, this is " *
                        join(["the `$(e.name)` $(e.kind) on $(pass_availability([e]))"
                              for e in limited], ", or ") * "."
        elseif length(limited) == 1
            versions *= "\n\nAvailable on $(pass_availability(limited))."
        end
        julia = main.julia ? "\n\nRequires the LLVM library that Julia uses." : ""
        doc = """
            $class_name(; options...) -> String

        The `$(main.name)` $(main.kind), as a string for use with [`add!`](@ref) or [`run!`](@ref)$aa.$options$versions$julia
        """
        @eval mod @doc $doc $class_name
    end
end

# naming one of Julia's passes in a pipeline for another library fails to parse it, so
# explain why when that happens
function check_julia_pass_names(exception::LLVMException, lib::Library)
    lib === julia_library && return
    m = match(r"unknown pass name '([^']*)'", exception.info)
    m === nothing && return
    name = m.captures[1]
    for entry in pass_table
        if entry.julia && entry.name == name
            error("""Julia's $(entry.kind) `$name` requires the LLVM library that Julia uses, but the pipeline runs with LLVM library :$(lib.name) (LLVM $(lib.version)).
                     $(exception.info)""")
        end
    end
end

# the passes of a kind that are available on a library, e.g., for testing them all
passes_of_kind(kind::String, lib::Library) =
    unique!([entry.name for entry in pass_table if entry.kind == kind && isavailable(entry, lib)])
module_passes(lib::Library=library()) = passes_of_kind("module pass", lib)
cgscc_passes(lib::Library=library()) = passes_of_kind("CGSCC pass", lib)
function_passes(lib::Library=library()) = passes_of_kind("function pass", lib)
loop_passes(lib::Library=library()) = passes_of_kind("loop pass", lib)

# `@module_pass "name" Name [define_class] [since=major] [until=major] [julia=true]`
function pass_macro(mod, kind, pass_name, class_name, args)
    define_class = true
    since = until = nothing
    julia = false
    for arg in args
        if Meta.isexpr(arg, :(=)) && arg.args[1] === :since
            since = arg.args[2]::Int
        elseif Meta.isexpr(arg, :(=)) && arg.args[1] === :until
            until = arg.args[2]::Int
        elseif Meta.isexpr(arg, :(=)) && arg.args[1] === :julia
            julia = arg.args[2]::Bool
        elseif arg isa Bool
            define_class = arg
        else
            error("unsupported argument to a pass macro: $arg")
        end
    end
    define_pass(mod, pass_name, class_name, kind, define_class, since, until, julia)
end
macro module_pass(pass_name, class_name, args...)
    pass_macro(__module__, "module pass", pass_name, class_name, args)
end
macro cgscc_pass(pass_name, class_name, args...)
    pass_macro(__module__, "CGSCC pass", pass_name, class_name, args)
end
macro function_pass(pass_name, class_name, args...)
    pass_macro(__module__, "function pass", pass_name, class_name, args)
end
macro loop_pass(pass_name, class_name, args...)
    pass_macro(__module__, "loop pass", pass_name, class_name, args)
end

# module passes

@module_pass "always-inline" AlwaysInlinerPass
@module_pass "attributor" AttributorPass
@module_pass "annotation2metadata" Annotation2MetadataPass
@module_pass "openmp-opt" OpenMPOptPass
@module_pass "called-value-propagation" CalledValuePropagationPass
@module_pass "canonicalize-aliases" CanonicalizeAliasesPass
@module_pass "cg-profile" CGProfilePass
@module_pass "check-debugify" CheckDebugifyPass
@module_pass "constmerge" ConstantMergePass
@module_pass "coro-early" CoroEarlyPass
@module_pass "coro-cleanup" CoroCleanupPass
@module_pass "cross-dso-cfi" CrossDSOCFIPass
@module_pass "deadargelim" DeadArgumentEliminationPass
@module_pass "debugify" DebugifyPass
@module_pass "dot-callgraph" CallGraphDOTPrinterPass
@module_pass "elim-avail-extern" EliminateAvailableExternallyPass
@module_pass "extract-blocks" BlockExtractorPass
@module_pass "forceattrs" ForceFunctionAttrsPass
@module_pass "function-import" FunctionImportPass
@module_pass "ipsccp<func-spec>" FunctionSpecializationPass since=16
@module_pass "function-specialization" FunctionSpecializationPass until=16
@module_pass "globaldce" GlobalDCEPass
@module_pass "globalopt" GlobalOptPass
@module_pass "globalsplit" GlobalSplitPass
@module_pass "hotcoldsplit" HotColdSplittingPass
@module_pass "inferattrs" InferFunctionAttrsPass
@module_pass "inliner-wrapper" ModuleInlinerWrapperPass
@module_pass "inliner-ml-advisor-release" ModuleInlinerMLAdvisorReleasePass
@module_pass "print<inline-advisor>" InlineAdvisorAnalysisPrinterPass
@module_pass "inliner-wrapper-no-mandatory-first" ModuleInlinerWrapperNoMandatoryFirstPass
@module_pass "insert-gcov-profiling" GCOVProfilerPass
@module_pass "instrorderfile" InstrOrderFilePass until=21
@module_pass "instrprof" InstrProfiling
@module_pass "invalidate<all>" InvalidateAllAnalysesPass
@module_pass "ipsccp" IPSCCPPass
@module_pass "iroutliner" IROutlinerPass
@module_pass "print-ir-similarity" IRSimilarityAnalysisPrinterPass
@module_pass "lower-global-dtors" LowerGlobalDtorsPass
@module_pass "lowertypetests" LowerTypeTestsPass
@module_pass "metarenamer" MetaRenamerPass
@module_pass "mergefunc" MergeFunctionsPass
@module_pass "name-anon-globals" NameAnonGlobalPass
@module_pass "no-op-module" NoOpModulePass
@module_pass "objc-arc-apelim" ObjCARCAPElimPass until=22
@module_pass "partial-inliner" PartialInlinerPass
@module_pass "pgo-icall-prom" PGOIndirectCallPromotion
@module_pass "pgo-instr-gen" PGOInstrumentationGen
@module_pass "pgo-instr-use" PGOInstrumentationUse
@module_pass "print-profile-summary" ProfileSummaryPrinterPass
@module_pass "print-callgraph" CallGraphPrinterPass
@module_pass "print" PrintModulePass
@module_pass "print-lcg" LazyCallGraphPrinterPass
@module_pass "print-lcg-dot" LazyCallGraphDOTPrinterPass
@module_pass "print-must-be-executed-contexts" MustBeExecutedContextPrinterPass
@module_pass "print-stack-safety" StackSafetyGlobalPrinterPass
@module_pass "print<module-debuginfo>" ModuleDebugInfoPrinterPass
@module_pass "recompute-globalsaa" RecomputeGlobalsAAPass
@module_pass "rel-lookup-table-converter" RelLookupTableConverterPass
@module_pass "rewrite-statepoints-for-gc" RewriteStatepointsForGC
@module_pass "rewrite-symbols" RewriteSymbolPass
@module_pass "rpo-function-attrs" ReversePostOrderFunctionAttrsPass
@module_pass "sample-profile" SampleProfileLoaderPass
@module_pass "strip" StripSymbolsPass
@module_pass "strip-dead-debug-info" StripDeadDebugInfoPass
@module_pass "pseudo-probe" SampleProfileProbePass
@module_pass "strip-dead-prototypes" StripDeadPrototypesPass
@module_pass "strip-debug-declare" StripDebugDeclarePass
@module_pass "strip-nondebug" StripNonDebugSymbolsPass
@module_pass "strip-nonlinetable-debuginfo" StripNonLineTableDebugInfoPass
@module_pass "synthetic-counts-propagation" SyntheticCountsPropagation until=20
@module_pass "trigger-crash-module" TriggerCrashModulePass since=19
@module_pass "trigger-crash" TriggerCrashPass until=19
@module_pass "verify" VerifierPass
@module_pass "view-callgraph" CallGraphViewerPass
@module_pass "wholeprogramdevirt" WholeProgramDevirtPass
@module_pass "dfsan" DataFlowSanitizerPass
@module_pass "module-inline" ModuleInlinerPass
@module_pass "tsan-module" ModuleThreadSanitizerPass
@module_pass "sancov-module" SanitizerCoveragePass
@module_pass "memprof-module" ModuleMemProfilerPass
@module_pass "poison-checking" PoisonCheckingPass until=20
@module_pass "pseudo-probe-update" PseudoProbeUpdatePass
@module_pass "loop-extract" LoopExtractorPass
@module_pass "hwasan" HWAddressSanitizerPass
@module_pass "asan" AddressSanitizerPass since=16
@module_pass "asan-module" AddressSanitizerPass until=16
@module_pass "msan" MemorySanitizerPass since=16
@function_pass "msan" MemorySanitizerPass until=16
@module_pass "internalize" InternalizePass false
"""
    InternalizePass(; preserved_gvs=String[], options...) -> String

The `internalize` module pass, as a string for use with [`add!`](@ref) or [`run!`](@ref),
which gives internal linkage to the global values of a module, except for the ones named
in `preserved_gvs`. Other keyword arguments become options of the pass.
"""
function InternalizePass(; preserved_gvs::Vector=String[], kwargs...)
    kwargs = [kwargs...]

    # map a single `preserved_gvs` to many `preserve_gv` options
    for gv in preserved_gvs
        push!(kwargs, :preserve_gv => gv)
    end

    "internalize" * kwargs_to_params(kwargs)
end

# Helper for extension-point callback passes (not part of general pass sweep).
# Back-ported by LLVMExtra to LLVM 17..21; supported upstream from LLVM 22 on.
function ep_callbacks_pass(name, since; opt_level=0)
    lib = library()
    atleast(since, lib) ||
        error("The $name pass is not available on LLVM $(lib.version) (only on LLVM $since and later)")
    name * kwargs_to_params(Dict{Symbol,Any}(Symbol("O$opt_level") => true))
end

macro callbacks_pass(pass_name, name, since=17)
    doc = """
        $name(; opt_level=0) -> String

    The `$pass_name` pass, as a string for use with [`add!`](@ref) or [`run!`](@ref). It
    runs the passes that pass builder callbacks (e.g., registered with
    [`register_callbacks!`](@ref)) add to the corresponding extension point of LLVM's
    default pipelines, for the optimization level `opt_level`. Requires LLVM $since+.
    """
    quote
        @doc $doc $(esc(name))(; opt_level=0) = ep_callbacks_pass($pass_name, $since; opt_level)
        push!(pass_functions, ($__module__, $(QuoteNode(name)), "extension point callbacks"))
    end
end

# module callbacks
@vocabulary Passes PipelineStartCallbacks, PipelineEarlySimplificationCallbacks,
                   OptimizerEarlyCallbacks, OptimizerLastCallbacks
@callbacks_pass "pipeline-start-callbacks" PipelineStartCallbacks
@callbacks_pass "pipeline-early-simplification-callbacks" PipelineEarlySimplificationCallbacks
@callbacks_pass "optimizer-early-callbacks" OptimizerEarlyCallbacks
@callbacks_pass "optimizer-last-callbacks" OptimizerLastCallbacks

# CGSCC passes

@cgscc_pass "argpromotion" ArgumentPromotionPass
@cgscc_pass "invalidate<all>" InvalidateAllAnalysesPass
@cgscc_pass "function-attrs" PostOrderFunctionAttrsPass
@cgscc_pass "attributor-cgscc" AttributorCGSCCPass
@cgscc_pass "openmp-opt-cgscc" OpenMPOptCGSCCPass
@cgscc_pass "no-op-cgscc" NoOpCGSCCPass
@cgscc_pass "inline" InlinerPass
@cgscc_pass "coro-split" CoroSplitPass

# CGSCC callbacks
@vocabulary Passes CGSCCOptimizerLateCallbacks
@callbacks_pass "cgscc-optimizer-late-callbacks" CGSCCOptimizerLateCallbacks

# function passes

@function_pass "aa-eval" AAEvaluator
@function_pass "adce" ADCEPass
@function_pass "add-discriminators" AddDiscriminatorsPass
@function_pass "aggressive-instcombine" AggressiveInstCombinePass
@function_pass "assume-builder" AssumeBuilderPass
@function_pass "assume-simplify" AssumeSimplifyPass
@function_pass "alignment-from-assumptions" AlignmentFromAssumptionsPass
@function_pass "annotation-remarks" AnnotationRemarksPass
@function_pass "bdce" BDCEPass
@function_pass "bounds-checking" BoundsCheckingPass
@function_pass "break-crit-edges" BreakCriticalEdgesPass
@function_pass "callsite-splitting" CallSiteSplittingPass
@function_pass "consthoist" ConstantHoistingPass
@function_pass "constraint-elimination" ConstraintEliminationPass
@function_pass "chr" ControlHeightReductionPass
@function_pass "coro-elide" CoroElidePass
@function_pass "correlated-propagation" CorrelatedValuePropagationPass
@function_pass "dce" DCEPass
@function_pass "dfa-jump-threading" DFAJumpThreadingPass
@function_pass "div-rem-pairs" DivRemPairsPass
@function_pass "dse" DSEPass
@function_pass "dot-cfg" CFGPrinterPass
@function_pass "dot-cfg-only" CFGOnlyPrinterPass
@function_pass "dot-dom" DomPrinter
@function_pass "dot-dom-only" DomOnlyPrinter
@function_pass "dot-post-dom" PostDomPrinter
@function_pass "dot-post-dom-only" PostDomOnlyPrinter
@function_pass "view-dom" DomViewer
@function_pass "view-dom-only" DomOnlyViewer
@function_pass "view-post-dom" PostDomViewer
@function_pass "view-post-dom-only" PostDomOnlyViewer
# LLVM only registers this pass since LLVM 21, but LLVMExtra makes it available on all
# supported versions
@function_pass "expand-reductions" ExpandReductionsPass
@function_pass "fix-irreducible" FixIrreduciblePass
@function_pass "flatten-cfg" FlattenCFGPass since=19
@function_pass "flattencfg" FlattenCFGPass until=19
@function_pass "make-guards-explicit" MakeGuardsExplicitPass
@function_pass "gvn-hoist" GVNHoistPass
@function_pass "gvn-sink" GVNSinkPass
@function_pass "helloworld" HelloWorldPass
@function_pass "trigger-crash-function" TriggerCrashFunctionPass since=19
@function_pass "infer-address-spaces" InferAddressSpacesPass
@function_pass "instcombine" InstCombinePass false
"""
    InstCombinePass(; options...) -> String

The `instcombine` function pass, as a string for use with [`add!`](@ref) or [`run!`](@ref).
Keyword arguments become options of the pass. Unlike LLVM's C API, LLVM.jl doesn't enable
the `verify-fixpoint` option by default (on LLVM 18 and later).
"""
function InstCombinePass(; kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)
    if version() >= v"18"
        # XXX: LLVM "helpfully" enables fixpoint verification by default when using the C API
        #      https://github.com/llvm/llvm-project/blob/3c3fb357a0ed4dbf640bdb6c61db2a430f7eb298/llvm/lib/Passes/PassBuilder.cpp#L1034-L1036
        #      https://github.com/llvm/llvm-project/issues/92648
        kwargs[:verify_fixpoint] = get(kwargs, :verify_fixpoint, false)
    end
    "instcombine" * kwargs_to_params(kwargs)
end
@function_pass "instcount" InstCountPass
@function_pass "instsimplify" InstSimplifyPass
@function_pass "invalidate<all>" InvalidateAllAnalysesPass
@function_pass "irce" IRCEPass
@function_pass "float2int" Float2IntPass
@function_pass "no-op-function" NoOpFunctionPass
@function_pass "libcalls-shrinkwrap" LibCallsShrinkWrapPass
@function_pass "lint" LintPass
@function_pass "inject-tli-mappings" InjectTLIMappings
@function_pass "instnamer" InstructionNamerPass
@function_pass "lower-atomic" LowerAtomicPass since=19
@function_pass "loweratomic" LowerAtomicPass until=19
@function_pass "lower-expect" LowerExpectIntrinsicPass
@function_pass "lower-guard-intrinsic" LowerGuardIntrinsicPass
@function_pass "lower-constant-intrinsics" LowerConstantIntrinsicsPass
@function_pass "lower-widenable-condition" LowerWidenableConditionPass
@function_pass "guard-widening" GuardWideningPass
@function_pass "load-store-vectorizer" LoadStoreVectorizerPass
@function_pass "loop-simplify" LoopSimplifyPass
@function_pass "loop-sink" LoopSinkPass
@function_pass "lower-invoke" LowerInvokePass since=19
@function_pass "lower-switch" LowerSwitchPass since=19
@function_pass "lowerinvoke" LowerInvokePass until=19
@function_pass "lowerswitch" LowerSwitchPass until=19
@function_pass "mem2reg" PromotePass
@function_pass "memcpyopt" MemCpyOptPass
@function_pass "mergeicmps" MergeICmpsPass
@function_pass "mergereturn" UnifyFunctionExitNodesPass until=23
@function_pass "nary-reassociate" NaryReassociatePass
@function_pass "newgvn" NewGVNPass
@function_pass "jump-threading" JumpThreadingPass
@function_pass "partially-inline-libcalls" PartiallyInlineLibCallsPass
@function_pass "lcssa" LCSSAPass
@function_pass "loop-data-prefetch" LoopDataPrefetchPass
@function_pass "loop-load-elim" LoopLoadEliminationPass
@function_pass "loop-fusion" LoopFusePass
@function_pass "loop-distribute" LoopDistributePass
@function_pass "loop-versioning" LoopVersioningPass
@function_pass "objc-arc" ObjCARCOptPass
@function_pass "objc-arc-contract" ObjCARCContractPass
@function_pass "objc-arc-expand" ObjCARCExpandPass
@function_pass "pgo-memop-opt" PGOMemOPSizeOpt
@function_pass "print" PrintFunctionPass
@function_pass "print<assumptions>" AssumptionPrinterPass
@function_pass "print<block-freq>" BlockFrequencyPrinterPass
@function_pass "print<branch-prob>" BranchProbabilityPrinterPass
@function_pass "print<cost-model>" CostModelPrinterPass
@function_pass "print<cycles>" CycleInfoPrinterPass
@function_pass "print<da>" DependenceAnalysisPrinterPass
@function_pass "print<divergence>" DivergenceAnalysisPrinterPass until=17
@function_pass "print<domtree>" DominatorTreePrinterPass
@function_pass "print<postdomtree>" PostDominatorTreePrinterPass
@function_pass "print<delinearization>" DelinearizationPrinterPass
@function_pass "print<demanded-bits>" DemandedBitsPrinterPass
@function_pass "print<domfrontier>" DominanceFrontierPrinterPass
@function_pass "print<func-properties>" FunctionPropertiesPrinterPass
@function_pass "print<inline-cost>" InlineCostAnnotationPrinterPass
@function_pass "print<loops>" LoopPrinterPass
@function_pass "print<memoryssa>" MemorySSAPrinterPass
@function_pass "print<memoryssa-walker>" MemorySSAWalkerPrinterPass
@function_pass "print<phi-values>" PhiValuesPrinterPass
@function_pass "print<regions>" RegionInfoPrinterPass
@function_pass "print<scalar-evolution>" ScalarEvolutionPrinterPass
@function_pass "print<stack-safety-local>" StackSafetyPrinterPass
@function_pass "print-alias-sets" AliasSetsPrinterPass
@function_pass "print-predicateinfo" PredicateInfoPrinterPass
@function_pass "print-mustexecute" MustExecutePrinterPass
@function_pass "print-memderefs" MemDerefPrinterPass
@function_pass "print<access-info>" LoopAccessInfoPrinterPass since=16
@loop_pass "print-access-info" LoopAccessInfoPrinterPass until=16
@function_pass "reassociate" ReassociatePass
@function_pass "redundant-dbg-inst-elim" RedundantDbgInstEliminationPass
@function_pass "reg2mem" RegToMemPass
@function_pass "scalarize-masked-mem-intrin" ScalarizeMaskedMemIntrinPass
@function_pass "scalarizer" ScalarizerPass
@function_pass "separate-const-offset-from-gep" SeparateConstOffsetFromGEPPass
@function_pass "sccp" SCCPPass
@function_pass "sink" SinkingPass
@function_pass "slp-vectorizer" SLPVectorizerPass
@function_pass "slsr" StraightLineStrengthReducePass
@function_pass "speculative-execution" SpeculativeExecutionPass
@function_pass "sroa" SROAPass
@function_pass "strip-gc-relocates" StripGCRelocates
@function_pass "structurizecfg" StructurizeCFGPass
@function_pass "tailcallelim" TailCallElimPass
@function_pass "unify-loop-exits" UnifyLoopExitsPass
@function_pass "vector-combine" VectorCombinePass false
"""
    VectorCombinePass(; early=false) -> String

The `vector-combine` function pass, as a string for use with [`add!`](@ref) or
[`run!`](@ref). With `early=true`, it only performs the folds that LLVM's pipelines run
early on, before vectorization (`vector-combine<early>`). LLVM's own pipeline parser does
not support that option, which LLVM.jl adds on LLVM 16 and later.
"""
function VectorCombinePass(; early::Bool=false)
    if early && version() < v"16"
        throw(ArgumentError("vector-combine<early> requires LLVM 16 or later"))
    end
    return early ? "vector-combine<early>" : "vector-combine"
end
@function_pass "verify" VerifierPass
@function_pass "verify<domtree>" DominatorTreeVerifierPass
@function_pass "verify<loops>" LoopVerifierPass
@function_pass "verify<memoryssa>" MemorySSAVerifierPass
@function_pass "verify<regions>" RegionInfoVerifierPass
@function_pass "verify<safepoint-ir>" SafepointIRVerifierPass
@function_pass "verify<scalar-evolution>" ScalarEvolutionVerifierPass
@function_pass "view-cfg" CFGViewerPass
@function_pass "view-cfg-only" CFGOnlyViewerPass
@function_pass "tlshoist" TLSVariableHoistPass until=20
@function_pass "transform-warning" WarnMissedTransformationsPass
@function_pass "tsan" ThreadSanitizerPass
@function_pass "memprof" MemProfilerPass
@function_pass "early-cse" EarlyCSEPass
@function_pass "ee-instrument" EntryExitInstrumenterPass
@function_pass "lower-matrix-intrinsics" LowerMatrixIntrinsicsPass
@function_pass "loop-unroll" LoopUnrollPass false
"""
    LoopUnrollPass(; opt_level=0, options...) -> String

The `loop-unroll` function pass, as a string for use with [`add!`](@ref) or
[`run!`](@ref), for the optimization level `opt_level`. Other keyword arguments become
options of the pass, e.g., `LoopUnrollPass(; partial=true)` is `"loop-unroll<O0;partial>"`.
"""
function LoopUnrollPass(; opt_level=0, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)
    kwargs[Symbol("O$opt_level")] = true
    "loop-unroll" * kwargs_to_params(kwargs)
end
@function_pass "simplifycfg" SimplifyCFGPass
@function_pass "loop-vectorize" LoopVectorizePass
@function_pass "mldst-motion" MergedLoadStoreMotionPass
@function_pass "gvn" GVNPass
@function_pass "print<stack-lifetime>" StackLifetimePrinterPass

# Function pass callbacks
@vocabulary Passes PeepholeCallbacks, ScalarOptimizerLateCallbacks, VectorizerStartCallbacks
@callbacks_pass "peephole-callbacks" PeepholeCallbacks
@callbacks_pass "scalar-optimizer-late-callbacks" ScalarOptimizerLateCallbacks
@callbacks_pass "vectorizer-start-callbacks" VectorizerStartCallbacks
@vocabulary Passes VectorizerEndCallbacks
@callbacks_pass "vectorizer-end-callbacks" VectorizerEndCallbacks 21
# loop nest passes

@loop_pass "loop-flatten" LoopFlattenPass
@loop_pass "loop-interchange" LoopInterchangePass
@loop_pass "loop-unroll-and-jam" LoopUnrollAndJamPass
@loop_pass "no-op-loopnest" NoOpLoopNestPass

# loop passes

@loop_pass "canon-freeze" CanonicalizeFreezeInLoopsPass
@loop_pass "dot-ddg" DDGDotPrinterPass
@loop_pass "invalidate<all>" InvalidateAllAnalysesPass
@loop_pass "loop-idiom" LoopIdiomRecognizePass
@loop_pass "loop-instsimplify" LoopInstSimplifyPass
@loop_pass "loop-rotate" LoopRotatePass
@loop_pass "no-op-loop" NoOpLoopPass
@loop_pass "print" PrintLoopPass
@loop_pass "loop-deletion" LoopDeletionPass
@loop_pass "loop-simplifycfg" LoopSimplifyCFGPass
@loop_pass "loop-reduce" LoopStrengthReducePass
@loop_pass "indvars" IndVarSimplifyPass
@loop_pass "loop-unroll-full" LoopFullUnrollPass
@loop_pass "print<ddg>" DDGAnalysisPrinterPass
@loop_pass "print<iv-users>" IVUsersPrinterPass
@loop_pass "print<loopnest>" LoopNestPrinterPass
@loop_pass "print<loop-cache-cost>" LoopCachePrinterPass
@loop_pass "loop-predication" LoopPredicationPass
@loop_pass "guard-widening" GuardWideningPass
@loop_pass "loop-bound-split" LoopBoundSplitPass
@loop_pass "loop-reroll" LoopRerollPass until=19
@loop_pass "loop-versioning-licm" LoopVersioningLICMPass
@loop_pass "simple-loop-unswitch" SimpleLoopUnswitchPass
@loop_pass "licm" LICMPass
@loop_pass "lnicm" LNICMPass

# loop callbacks
@vocabulary Passes LateLoopOptimizationsCallbacks, LoopOptimizerEndCallbacks
@callbacks_pass "late-loop-optimizations-callbacks" LateLoopOptimizationsCallbacks
@callbacks_pass "loop-optimizer-end-callbacks" LoopOptimizerEndCallbacks


## alias analyses

@doc (@doc PassManager)
struct AAManager <: AbstractPassManager
    passes::Vector{String}

    AAManager() = new([])
end

Base.string(pb::AAManager) = join(pb.passes, ",")

function add!(pb::PassBuilder, aa::AAManager)
    push!(pb.aa_passes, string(aa))
    return pb
end
add!(pm::AAManager, aa::AAManager) =
    error("Alias analyses can only be added to the top-level pass builder")

# unlike other pass managers, an empty AA manager is meaningful: it disables alias analysis
function add!(f::Base.Callable, pb::PassBuilder, aa::AAManager)
    f(aa)
    return add!(pb, aa)
end

macro aa_pass(pass_name, class_name, args...)
    pass_macro(__module__, "alias analysis", pass_name, class_name, args)
end

@aa_pass "basic-aa" BasicAA
@aa_pass "objc-arc-aa" ObjCARCAA
@aa_pass "scev-aa" SCEVAA
@aa_pass "scoped-noalias-aa" ScopedNoAliasAA
@aa_pass "tbaa" TypeBasedAA

@vocabulary Passes TargetAA
push!(pass_functions, (LLVM, :TargetAA, "alias analysis"))

"""
    TargetAA() -> String

The alias analyses of the target machine that the passes run with, as a string for use in
an [`AAManager`](@ref). LLVM's default alias analyses include them, but other alias
analysis pipelines only do where they list `TargetAA()`. Without a target machine, or for
targets that have no alias analyses of their own, it adds nothing. This is not one of
LLVM's alias analyses: LLVM.jl adds it to the pipelines it runs.
"""
TargetAA() = "target-aa"


document_passes(LLVM)


## pipelines

@vocabulary Passes DefaultPipeline

"""
    DefaultPipeline(; opt_level=0, options...) -> String

LLVM's default optimization pipeline for the optimization level `opt_level` (0 to 3, or
`"s"` and `"z"` to optimize for size on LLVM versions before 23), as a string for use with
[`add!`](@ref) or [`run!`](@ref), e.g., `"default<O3>"`. Other keyword arguments become options of the
pipeline, but LLVM's default pipeline takes few: it is tuned using the keyword arguments of
[`PassBuilder`](@ref) instead.
"""
function DefaultPipeline(; opt_level=0, kwargs...)
    if version() >= v"23" && string(opt_level) in ("s", "z")
        throw(ArgumentError("LLVM 23 removed size optimization pipelines; use opt_level=2 " *
                            "and optsize or minsize function attributes"))
    end
    kwargs = Dict{Symbol, Any}(kwargs)

    # `opt_level` => `O` flag (which is mandatory)
    kwargs[Symbol("O$opt_level")] = true

    "default" * kwargs_to_params(kwargs)
end
