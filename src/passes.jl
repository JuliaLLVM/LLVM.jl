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
    FunctionPassManager()
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

See also: [`add!`](@ref), [`PassBuilder`](@ref)
"""
struct PassManager <: AbstractPassManager
    type::String
    passes::Vector{String}

    PassManager(type::AbstractString) = new(type, [])
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
FunctionPassManager() = PassManager("function")

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
[`AssumptionCache`](@ref), [`LazyValueInfo`](@ref), [`ScalarEvolution`](@ref) and
[`LoopInfo`](@ref).

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
`FunctionAnalyses` (only for module passes) for the function analyses of every
function of the module, and `LoopAnalyses` (only for loop passes) for the analyses that a
loop pass keeps valid when it changes a loop: the dominator tree, loop info and scalar
evolution of the function.
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

For a module pass, the function analyses that are listed are preserved for every function
of the module, while all other analyses of the module are invalidated. A module pass that
invalidated the analyses of the functions it changed itself (using
`invalidate!(am[f])`) can return `PreservedAnalyses(FunctionAnalyses)`, which keeps the
cached function analyses of all other functions.

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
                T === MemorySSA || analysis_id(T)  # validate
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

# pass a set of preserved function analyses to a C function `f(args..., all, cfg, ids, nids)`
function with_preserved(f, pa::PreservedAnalyses, args...)
    pa.functions &&
        throw(ArgumentError("FunctionAnalyses can only be preserved by a module pass"))
    check_not_loop_preserved(pa)
    ids = API.LLVMExtraFunctionAnalysis[analysis_id(T) for T in pa.analyses]
    f(args..., pa.all, pa.cfg, ids, length(ids))
end

set_preserved!(ref::API.LLVMPreservedAnalysesRef, preserved) =
    with_preserved(API.LLVMExtraSetPreservedAnalyses,
                   convert(PreservedAnalyses, preserved)::PreservedAnalyses, ref)

function check_not_loop_preserved(pa::PreservedAnalyses)
    (pa.loop || MemorySSA in pa.analyses) &&
        throw(ArgumentError("LoopAnalyses and MemorySSA can only be preserved by a loop pass"))
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
        throw(ArgumentError("FunctionAnalyses can only be preserved by a module pass"))
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

@vocabulary Passes remark!, Remark

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
    remark!(am::FunctionAnalysisManager, kind::Symbol, pass_name, remark_name,
            inst::Instruction, message)
    remark!(ar::LoopAnalysisResults, kind::Symbol, pass_name, remark_name,
            inst::Instruction, message)

Emit an optimization remark about the instruction `inst` of the function of `am` (or of a
loop pass, given its [`LoopAnalysisResults`](@ref)), like LLVM's passes do using an
`OptimizationRemarkEmitter`: with `kind=:passed` for an
optimization that the pass performed, `kind=:missed` for one that it could not perform,
or `kind=:analysis` for a fact that explains a decision. The `pass_name` and the
`remark_name` identify the remark, and `message` describes it.

Remarks are passed to the diagnostic handler of the context, where they can be collected
using the `remarks` keyword argument of [`PassBuilder`](@ref) (also accepted by `run!`), or
written to a file using LLVM's `-pass-remarks-output` option. Otherwise, they are logged
as debug messages.
"""
function remark!(am::FunctionAnalysisManager, kind::Symbol, pass_name::AbstractString,
                 remark_name::AbstractString, inst::Instruction, message::AbstractString)
    api_kind = remark_kind(kind)
    bb = parent(inst)
    bb !== nothing && parent(bb) == am.fun ||
        throw(ArgumentError("Instruction is not part of function $(am.fun.name)"))
    API.LLVMExtraFunctionAnalysisManagerEmitRemark(am, api_kind, pass_name, remark_name,
                                                   inst, message)
    return
end

function remark!(ar::LoopAnalysisResults, kind::Symbol, pass_name::AbstractString,
                 remark_name::AbstractString, inst::Instruction, message::AbstractString)
    ar.valid || throw(ArgumentError("Loop analysis results cannot be used after the pass returned"))
    bb = parent(inst)
    bb !== nothing && parent(bb) == ar.fun ||
        throw(ArgumentError("Instruction is not part of function $(ar.fun.name)"))
    API.LLVMExtraEmitRemark(remark_kind(kind), pass_name, remark_name, inst, message)
    return
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
  or an empty file name if it is unknown.
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
    return Remark(kind_sym, pass_name, name, function_name, message, file, line[],
                  column[])
end

function Base.show(io::IO, remark::Remark)
    print(io, "Remark(:", remark.kind, ", ", repr(remark.pass_name), ", ",
          repr(remark.name), ", ", repr(remark.function_name), ", ", repr(remark.message),
          ")")
end


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

function PassBuilder(; kwargs...)
    opts = API.LLVMCreatePassBuilderOptions()
    obj = mark_alloc(PassBuilder(opts, [], [], [], nothing, [], nothing))

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
"""
run!

function run!(pb::PassBuilder, target::Union{Module,Function}, tm::Union{Nothing,TargetMachine}=nothing)
    isempty(pb.passes) && return
    pipeline = join(pb.passes, ",")
    aa_pipeline = join(pb.aa_passes, ",")

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
                     pipeline::String, aa_pipeline::String)
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
                       pipeline::String, aa_pipeline::String, states, tti_state, ctx)
    GC.@preserve states tti_state aa_pipeline begin
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

        # register Julia passes
        julia_callback = cglobal(:jl_register_passbuilder_callbacks)
        API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, julia_callback)

        # register native callbacks
        for callback in pb.registration_callbacks
            API.LLVMPassBuilderExtensionsPushRegistrationCallbacks(exts, callback)
        end

        # register AA pipeline
        if !isempty(aa_pipeline)
            if version() >= v"20"
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
            if !isempty(aa_pipeline) && version() >= v"20"
                API.LLVMPassBuilderOptionsSetAAPipeline(pb, C_NULL)
            end
        end

        # exceptions caught in custom pass callbacks take precedence over LLVM's errors,
        # e.g., the failure to parse the pipeline when parsing the options of a pass threw
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
        @check err
        check_diagnostic(ctx)
    end
end

function run!(pass::AbstractString, args...; kwargs...)
    @dispose pb=PassBuilder(; kwargs...) begin
        add!(pb, pass)
        run!(pb, args...)
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

function define_pass(mod, pass_name, class_name, kind, define_class=true)
    # don't re-define passes (some work with multiple types of managers,
    # or could be manually-defined)
    if isdefined(LLVM, class_name)
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
        options = occursin('<', pass_name) ? "" : """

            Keyword arguments become options of the pass, e.g., `$class_name(; foo=true, bar=2)`
            is `"$pass_name<foo;bar=2>"`, while `false` values become `no-` options."""
        doc = """
            $class_name(; options...) -> String

        The `$pass_name` $kind, as a string for use with [`add!`](@ref) or [`run!`](@ref)$(kind == "alias analysis" ? " (in an [`AAManager`](@ref))" : "").$options
        """
        push!(ex.args, :(
            @doc $doc function $(esc(class_name))(; kwargs...)
                return $pass_name * kwargs_to_params(kwargs)
            end
        ))
    end
    return ex
end

# for testing purposes, keep track of all defined passes
const module_passes = String[]
const cgscc_passes = String[]
const function_passes = String[]
const loop_passes = String[]

macro module_pass(pass_name, class_name, define_class=true)
    push!(module_passes, pass_name)
    define_pass(__module__, pass_name, class_name, "module pass", define_class)
end
macro cgscc_pass(pass_name, class_name, define_class=true)
    push!(cgscc_passes, pass_name)
    define_pass(__module__, pass_name, class_name, "CGSCC pass", define_class)
end
macro function_pass(pass_name, class_name, define_class=true)
    push!(function_passes, pass_name)
    define_pass(__module__, pass_name, class_name, "function pass", define_class)
end
macro loop_pass(pass_name, class_name, define_class=true)
    push!(loop_passes, pass_name)
    define_pass(__module__, pass_name, class_name, "loop pass", define_class)
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
@static if version() < v"16"
    @module_pass "function-specialization" FunctionSpecializationPass
else
    @module_pass "ipsccp<func-spec>" FunctionSpecializationPass
end
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
@static if version() < v"21"
    @module_pass "instrorderfile" InstrOrderFilePass
end
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
@static if version() < v"22"
    @module_pass "objc-arc-apelim" ObjCARCAPElimPass
end
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
@static if version() < v"20"
    @module_pass "synthetic-counts-propagation" SyntheticCountsPropagation
end
@static if version() < v"19"
    @module_pass "trigger-crash" TriggerCrashPass
else
    @module_pass "trigger-crash-module" TriggerCrashModulePass
end
@module_pass "verify" VerifierPass
@module_pass "view-callgraph" CallGraphViewerPass
@module_pass "wholeprogramdevirt" WholeProgramDevirtPass
@module_pass "dfsan" DataFlowSanitizerPass
@module_pass "module-inline" ModuleInlinerPass
@module_pass "tsan-module" ModuleThreadSanitizerPass
@module_pass "sancov-module" SanitizerCoveragePass
@module_pass "memprof-module" ModuleMemProfilerPass
@static if version() < v"20"
    @module_pass "poison-checking" PoisonCheckingPass
end
@module_pass "pseudo-probe-update" PseudoProbeUpdatePass
@module_pass "loop-extract" LoopExtractorPass
@module_pass "hwasan" HWAddressSanitizerPass
@static if version() < v"16"
    @module_pass "asan-module" AddressSanitizerPass
else
    @module_pass "asan" AddressSanitizerPass
end
@static if version() < v"16"
    @function_pass "msan" MemorySanitizerPass
else
    @module_pass "msan" MemorySanitizerPass
end
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
function ep_callbacks_pass(name; opt_level=0)
    name * kwargs_to_params(Dict{Symbol,Any}(Symbol("O$opt_level") => true))
end

macro callbacks_pass(pass_name, name)
    doc = """
        $name(; opt_level=0) -> String

    The `$pass_name` pass, as a string for use with [`add!`](@ref) or [`run!`](@ref). It
    runs the passes that pass builder callbacks (e.g., registered with
    [`register_callbacks!`](@ref)) add to the corresponding extension point of LLVM's
    default pipelines, for the optimization level `opt_level`. Requires LLVM 17+.
    """
    quote
        @doc $doc $(esc(name))(; opt_level=0) = ep_callbacks_pass($pass_name; opt_level)
        push!(pass_functions, ($__module__, $(QuoteNode(name)), "extension point callbacks"))
    end
end

# module callbacks
@static if version() >= v"17"
@vocabulary Passes PipelineStartCallbacks, PipelineEarlySimplificationCallbacks,
                   OptimizerEarlyCallbacks, OptimizerLastCallbacks
@callbacks_pass "pipeline-start-callbacks" PipelineStartCallbacks
@callbacks_pass "pipeline-early-simplification-callbacks" PipelineEarlySimplificationCallbacks
@callbacks_pass "optimizer-early-callbacks" OptimizerEarlyCallbacks
@callbacks_pass "optimizer-last-callbacks" OptimizerLastCallbacks
end

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
@static if version() >= v"17"
@vocabulary Passes CGSCCOptimizerLateCallbacks
@callbacks_pass "cgscc-optimizer-late-callbacks" CGSCCOptimizerLateCallbacks
end

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
@static if version() < v"19"
    @function_pass "flattencfg" FlattenCFGPass
else
    @function_pass "flatten-cfg" FlattenCFGPass
end
@function_pass "make-guards-explicit" MakeGuardsExplicitPass
@function_pass "gvn-hoist" GVNHoistPass
@function_pass "gvn-sink" GVNSinkPass
@function_pass "helloworld" HelloWorldPass
@static if version() >= v"19"
    @function_pass "trigger-crash-function" TriggerCrashFunctionPass
end
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
@static if version() < v"19"
    @function_pass "loweratomic" LowerAtomicPass
else
    @function_pass "lower-atomic" LowerAtomicPass
end
@function_pass "lower-expect" LowerExpectIntrinsicPass
@function_pass "lower-guard-intrinsic" LowerGuardIntrinsicPass
@function_pass "lower-constant-intrinsics" LowerConstantIntrinsicsPass
@function_pass "lower-widenable-condition" LowerWidenableConditionPass
@function_pass "guard-widening" GuardWideningPass
@function_pass "load-store-vectorizer" LoadStoreVectorizerPass
@function_pass "loop-simplify" LoopSimplifyPass
@function_pass "loop-sink" LoopSinkPass
@static if version() < v"19"
    @function_pass "lowerinvoke" LowerInvokePass
    @function_pass "lowerswitch" LowerSwitchPass
else
    @function_pass "lower-invoke" LowerInvokePass
    @function_pass "lower-switch" LowerSwitchPass
end
@function_pass "mem2reg" PromotePass
@function_pass "memcpyopt" MemCpyOptPass
@function_pass "mergeicmps" MergeICmpsPass
@static if version() < v"23"
    @function_pass "mergereturn" UnifyFunctionExitNodesPass
end
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
@static if version() < v"17"
    @function_pass "print<divergence>" DivergenceAnalysisPrinterPass
end
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
@static if version() < v"16"
    @loop_pass "print-access-info" LoopAccessInfoPrinterPass
else
    @function_pass "print<access-info>" LoopAccessInfoPrinterPass
end
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
@function_pass "vector-combine" VectorCombinePass
@function_pass "verify" VerifierPass
@function_pass "verify<domtree>" DominatorTreeVerifierPass
@function_pass "verify<loops>" LoopVerifierPass
@function_pass "verify<memoryssa>" MemorySSAVerifierPass
@function_pass "verify<regions>" RegionInfoVerifierPass
@function_pass "verify<safepoint-ir>" SafepointIRVerifierPass
@function_pass "verify<scalar-evolution>" ScalarEvolutionVerifierPass
@function_pass "view-cfg" CFGViewerPass
@function_pass "view-cfg-only" CFGOnlyViewerPass
@static if version() < v"20"
    @function_pass "tlshoist" TLSVariableHoistPass
end
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
@static if version() >= v"17"
@vocabulary Passes PeepholeCallbacks, ScalarOptimizerLateCallbacks, VectorizerStartCallbacks
@callbacks_pass "peephole-callbacks" PeepholeCallbacks
@callbacks_pass "scalar-optimizer-late-callbacks" ScalarOptimizerLateCallbacks
@callbacks_pass "vectorizer-start-callbacks" VectorizerStartCallbacks
@static if version() >= v"21"
    @vocabulary Passes VectorizerEndCallbacks
    @callbacks_pass "vectorizer-end-callbacks" VectorizerEndCallbacks
end
end # version() >= v"17"
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
@static if version() < v"19"
    @loop_pass "loop-reroll" LoopRerollPass
end
@loop_pass "loop-versioning-licm" LoopVersioningLICMPass
@loop_pass "simple-loop-unswitch" SimpleLoopUnswitchPass
@loop_pass "licm" LICMPass
@loop_pass "lnicm" LNICMPass

# loop callbacks
@static if version() >= v"17"
@vocabulary Passes LateLoopOptimizationsCallbacks, LoopOptimizerEndCallbacks
@callbacks_pass "late-loop-optimizations-callbacks" LateLoopOptimizationsCallbacks
@callbacks_pass "loop-optimizer-end-callbacks" LoopOptimizerEndCallbacks
end


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

macro aa_pass(pass_name, class_name)
    define_pass(__module__, pass_name, class_name, "alias analysis")
end

@aa_pass "basic-aa" BasicAA
@aa_pass "objc-arc-aa" ObjCARCAA
@aa_pass "scev-aa" SCEVAA
@aa_pass "scoped-noalias-aa" ScopedNoAliasAA
@aa_pass "tbaa" TypeBasedAA


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
