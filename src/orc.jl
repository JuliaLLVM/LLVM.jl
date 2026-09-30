@vocabulary ORC LLJITBuilder, LLJIT, ExecutionSession, JITDylib, OrcTargetAddress
@vocabulary ORC TargetMachineBuilder, target_machine_builder!, linking_layer_creator!
@vocabulary ORC mangle, lookup, intern
@vocabulary ORC ObjectLinkingLayer, register!
@vocabulary ORC LLVMSymbol, retain, release, SymbolFlags, define!, absolute_symbols
@vocabulary ORC DefinitionGenerator, DynamicLibrarySearchGenerator
@vocabulary ORC CustomDefinitionGenerator, check_callback_error!
@vocabulary ORC lookup_dylib, ResourceTracker, transfer!
@vocabulary ORC IRTransformLayer, IRCompileLayer, transform!
@vocabulary ORC MaterializationResponsibility, MaterializationUnit, CustomMaterializationUnit, emit!
@vocabulary ORC LocalIndirectStubsManager, LocalLazyCallThroughManager, lazy_reexports

include("executionengine/utils.jl")


## ownership

# ORC objects that an operation hands over to LLVM (e.g., a materialization unit that is
# added to a JITDylib) keep track of whether their handle still owns them, like a C++
# `unique_ptr` that has been moved from. A consumed handle can't be used anymore, and
# disposing of it does nothing, so that it's safe to dispose of it unconditionally.
function check_owned(obj)
    obj.owned ||
        throw(ArgumentError("This $(nameof(typeof(obj))) has been consumed or disposed of"))
    return mark_use(obj)
end

# hand the object over to LLVM, returning its reference
function consume!(obj)
    check_owned(obj)
    obj.owned = false
    mark_dispose(obj)
    return obj.ref
end

# dispose of the object using `f(ref)`, unless it was consumed already
function dispose_owned(f, obj)
    obj.owned || return
    obj.owned = false
    mark_dispose(obj -> f(obj.ref), obj)
    return
end


## target machine builder

"""
    TargetMachineBuilder()
    TargetMachineBuilder(tm::TargetMachine)

Create a builder of target machines, as used by an [`LLJITBuilder`](@ref) to create the
target machines that compile code. The builder either targets the host, or is based on
`tm`, taking ownership of it.

The builder is consumed by [`target_machine_builder!`](@ref); otherwise, it needs to be
disposed of using `dispose` or the do-block form, which do nothing once it has been
consumed.
"""
mutable struct TargetMachineBuilder
    ref::API.LLVMOrcJITTargetMachineBuilderRef
    owned::Bool

    function TargetMachineBuilder(ref::API.LLVMOrcJITTargetMachineBuilderRef)
        ref == C_NULL && throw(UndefRefError())
        mark_alloc(new(ref, true))
    end
end
Base.unsafe_convert(::Type{API.LLVMOrcJITTargetMachineBuilderRef},
                    tmb::TargetMachineBuilder) = check_owned(tmb).ref


function TargetMachineBuilder()
    ref = Ref{API.LLVMOrcJITTargetMachineBuilderRef}()
    @check API.LLVMOrcJITTargetMachineBuilderDetectHost(ref)
    TargetMachineBuilder(ref[])
end

function TargetMachineBuilder(tm::TargetMachine)
    tmb = API.LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine(tm)
    mark_dispose(tm)
    TargetMachineBuilder(tmb)
end

TargetMachineBuilder(f::Core.Function, args...) =
    with_disposal(f, TargetMachineBuilder(args...))

dispose(tmb::TargetMachineBuilder) =
    dispose_owned(API.LLVMOrcDisposeJITTargetMachineBuilder, tmb)

include("executionengine/lljit.jl")

"""
    ExecutionSession

The execution session of a JIT, which manages the JIT's JITDylibs and symbol string pool.
It is available as the `execution_session` property of a JIT.
"""
@checked struct ExecutionSession
    ref::API.LLVMOrcExecutionSessionRef
end
Base.unsafe_convert(::Type{API.LLVMOrcExecutionSessionRef}, es::ExecutionSession) = es.ref

execution_session(lljit::LLJIT) =
    ExecutionSession(API.LLVMOrcLLJITGetExecutionSession(lljit))

@property LLJIT execution_session

"""
    ObjectLinkingLayer

An object linking layer, based on RuntimeDyld, for use with
[`linking_layer_creator!`](@ref). Use `register!` to attach a `JITEventListener` to it.

The layer is consumed by returning it from a linking layer creator, which hands it over to
the JIT; otherwise, it needs to be disposed of using `dispose` or the do-block form of the
constructor, which do nothing once it has been consumed.
"""
mutable struct ObjectLinkingLayer
    ref::API.LLVMOrcObjectLayerRef
    owned::Bool

    function ObjectLinkingLayer(ref::API.LLVMOrcObjectLayerRef)
        ref == C_NULL && throw(UndefRefError())
        mark_alloc(new(ref, true))
    end
end
Base.unsafe_convert(::Type{API.LLVMOrcObjectLayerRef}, oll::ObjectLinkingLayer) =
    check_owned(oll).ref

"""
    ObjectLinkingLayer(es::ExecutionSession, triple::String=LLVM.default_triple();
                       override_object_flags=nothing, auto_claim_object_symbols=nothing)

Create a RuntimeDyld-based object linking layer that allocates memory using a
`SectionMemoryManager`.

The layer is configured the same way LLJIT configures its default object layer for
`triple`, which should describe the object files the layer will link. Objects for COFF
targets (e.g., Windows) do not carry reliable symbol visibility information, so on those
targets the layer uses the symbol flags from the IR instead of from the object file
(`override_object_flags`), and takes responsibility for additional symbols that code
generation introduced (`auto_claim_object_symbols`). Pass `true` or `false` to either
keyword argument to override the default.

The triple defaults to the host's. In a [`linking_layer_creator!`](@ref) callback, pass the
triple the callback receives:

```julia
linking_layer_creator!(builder) do es, triple
    ObjectLinkingLayer(es, triple)
end
```

On LLVM 21 and newer, that is the triple of the process executing the code rather than
that of the target machine, so pass the target's triple explicitly when JIT-compiling for
a different object format.
"""
function ObjectLinkingLayer(es::ExecutionSession, triple::String=LLVM.default_triple();
                            override_object_flags::Union{Nothing,Bool}=nothing,
                            auto_claim_object_symbols::Union{Nothing,Bool}=nothing)
    ref = API.LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es)
    API.LLVMOrcRTDyldObjectLinkingLayerApplyTargetDefaults(ref, triple)
    if override_object_flags !== nothing
        API.LLVMOrcRTDyldObjectLinkingLayerSetOverrideObjectFlagsWithResponsibilityFlags(
            ref, override_object_flags)
    end
    if auto_claim_object_symbols !== nothing
        API.LLVMOrcRTDyldObjectLinkingLayerSetAutoClaimResponsibilityForObjectSymbols(
            ref, auto_claim_object_symbols)
    end
    ObjectLinkingLayer(ref)
end

ObjectLinkingLayer(f::Core.Function, args...; kwargs...) =
    with_disposal(f, ObjectLinkingLayer(args...; kwargs...))

# LLVMOrcDisposeObjectLayer leaves the layer registered with its execution session (#629)
dispose(oll::ObjectLinkingLayer) =
    dispose_owned(API.LLVMExtraDisposeRTDyldObjectLinkingLayer, oll)

function register!(oll::ObjectLinkingLayer, listener::JITEventListener)
    API.LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener(oll, listener)
end

mutable struct ObjectLinkingLayerCreator
    cb
    exception::Union{Nothing,Tuple{Any,Vector}}
    ObjectLinkingLayerCreator(cb) = new(cb, nothing)
end

function ollc_callback(ctx::Ptr{Cvoid}, es::API.LLVMOrcExecutionSessionRef, triple::Ptr{Cchar})
    ollc = Base.unsafe_pointer_to_objref(ctx)::ObjectLinkingLayerCreator
    try
        layer = ollc.cb(ExecutionSession(es), Base.unsafe_string(triple))::ObjectLinkingLayer
        return consume!(layer)  # the JIT takes ownership of the layer
    catch err
        _capture_callback_exception!(ollc, err)
        # The C callback has no error return. Give LLJIT a valid default layer
        # so construction can finish normally and the Julia wrapper can throw.
        return API.LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es)
    end
end

"""
    linking_layer_creator!(builder::LLJITBuilder, creator)

Install a Julia object-layer creator, called with the execution session and
target triple. The builder keeps it rooted until it is consumed by
[`LLJIT`](@ref). If it throws, the exception is rethrown as a
[`CallbackException`](@ref) after LLJIT construction returns through LLVM.
"""
function linking_layer_creator!(builder::LLJITBuilder, creator)
    linking_layer_creator!(builder, ObjectLinkingLayerCreator(creator))
end

function linking_layer_creator!(builder::LLJITBuilder, state::ObjectLinkingLayerCreator)
    state.exception = nothing
    push!(builder.roots, state)
    cb = @cfunction(ollc_callback,
                    API.LLVMOrcObjectLayerRef,
                    (Ptr{Cvoid}, API.LLVMOrcExecutionSessionRef, Ptr{Cchar}))
    linking_layer_creator!(builder, cb, Base.pointer_from_objref(state))
end

linking_layer_creator!(creator::Core.Function, builder::LLJITBuilder) =
    linking_layer_creator!(builder, creator)

include("executionengine/ts_module.jl")

"""
    LLVMSymbol

An interned symbol name: an entry in the symbol string pool of an [`ExecutionSession`](@ref).
ORC identifies symbols by their linker-mangled names, which [`mangle`](@ref) computes and
interns. Symbols from the same session are equal if and only if their names are.

Symbols are reference counted. Functions that return a symbol, like `mangle` and
[`intern`](@ref), return a new reference, which should eventually be released with
[`release`](@ref), or be passed to an API that takes ownership of it (e.g.,
[`absolute_symbols`](@ref)). Use [`retain`](@ref) to create an additional
reference, e.g., to pass a symbol to multiple such APIs, or when passing on a symbol that
was only borrowed.
"""
@checked struct LLVMSymbol
    ref::API.LLVMOrcSymbolStringPoolEntryRef
end
Base.unsafe_convert(::Type{API.LLVMOrcSymbolStringPoolEntryRef}, sym::LLVMSymbol) = sym.ref
Base.convert(::Type{API.LLVMOrcSymbolStringPoolEntryRef}, sym::LLVMSymbol) = sym.ref

function Base.cconvert(::Type{Cstring}, sym::LLVMSymbol)
    return API.LLVMOrcSymbolStringPoolEntryStr(sym)
end

Base.String(sym::LLVMSymbol) = Base.unsafe_string(API.LLVMOrcSymbolStringPoolEntryStr(sym))
Base.string(sym::LLVMSymbol) = String(sym)

function Base.show(io::IO, sym::LLVMSymbol)
    show(io, typeof(sym))
    print(io, "(")
    show(io, String(sym))
    print(io, ")")
end

"""
    intern(es::ExecutionSession, name) -> LLVMSymbol

Intern `name`, as is, in the symbol string pool of `es`. The caller owns the returned
reference; see [`LLVMSymbol`](@ref).
"""
function intern(es::ExecutionSession, string)
    entry = API.LLVMOrcExecutionSessionIntern(es, string)
    LLVMSymbol(entry)
end

"""
    release(sym::LLVMSymbol)

Release a reference to the symbol `sym`.
"""
function release(sym::LLVMSymbol)
    API.LLVMOrcReleaseSymbolStringPoolEntry(sym)
end

"""
    retain(sym::LLVMSymbol)

Acquire an additional reference to the symbol `sym`.
"""
function retain(sym::LLVMSymbol)
    API.LLVMOrcRetainSymbolStringPoolEntry(sym)
end

# ORC always uses linker-mangled symbols internally (including for lookups, responsibility object maps, etc).
# IR uses non-linker-mangled names.
# If you're synthesizing IR from a requested-symbols map you'll need to demangle the name.
# Unfortunately we don't have a generic utility for that yet, but on MacOS it just means
# dropping the leading '_' if there is one, or prepending a \01 prefix (see https://llvm.org/docs/LangRef.html#identifiers)

"""
    mangle(jit, name) -> LLVMSymbol

Apply the target's linker mangling to `name` (e.g., prefixing an underscore on macOS), and
intern the result in the JIT's execution session. The caller owns the returned reference;
see [`LLVMSymbol`](@ref).
"""
function mangle(lljit::LLJIT, name)
    entry = API.LLVMOrcLLJITMangleAndIntern(lljit, name)
    return LLVMSymbol(entry)
end


## symbol flags

"""
    SymbolFlags(; exported=true, callable=false, weak=false,
                materialization_side_effects_only=false, target_flags=0)

The flags of a JIT symbol definition, as used by [`absolute_symbols`](@ref),
[`CustomMaterializationUnit`](@ref) and [`lazy_reexports`](@ref): whether the symbol is
`exported` from its JITDylib, whether it is `callable` (a function), whether it is `weak`
(and can be overridden by another definition), and whether it only stands for the side
effects of materializing it, without an address. `target_flags` are specific to the target
(e.g., whether an ARM function uses the Thumb instruction set), and must fit in a byte.
"""
struct SymbolFlags
    exported::Bool
    callable::Bool
    weak::Bool
    materialization_side_effects_only::Bool
    target_flags::UInt8
end

function SymbolFlags(; exported::Bool=true, callable::Bool=false, weak::Bool=false,
                     materialization_side_effects_only::Bool=false,
                     target_flags::Integer=0)
    0 <= target_flags <= typemax(UInt8) ||
        throw(ArgumentError("target_flags must be between 0 and 255, got $target_flags"))
    SymbolFlags(exported, callable, weak, materialization_side_effects_only,
                UInt8(target_flags))
end

function Base.show(io::IO, flags::SymbolFlags)
    default = SymbolFlags()
    kwargs = String[]
    for field in fieldnames(SymbolFlags)
        val = getfield(flags, field)
        val == getfield(default, field) && continue
        push!(kwargs, "$field=$(val isa Bool ? val : Int(val))")
    end
    print(io, "SymbolFlags(", join(kwargs, ", "), ")")
end

function Base.convert(::Type{API.LLVMJITSymbolFlags}, flags::SymbolFlags)
    generic = UInt8(0)
    flags.exported && (generic |= UInt8(API.LLVMJITSymbolGenericFlagsExported))
    flags.weak && (generic |= UInt8(API.LLVMJITSymbolGenericFlagsWeak))
    flags.callable && (generic |= UInt8(API.LLVMJITSymbolGenericFlagsCallable))
    flags.materialization_side_effects_only &&
        (generic |= UInt8(API.LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly))
    return API.LLVMJITSymbolFlags(generic, flags.target_flags)
end

check_symbol_flags(flags::SymbolFlags) = flags
check_symbol_flags(flags) =
    throw(ArgumentError("symbol flags must be SymbolFlags, got a $(typeof(flags))"))

# check the names of a symbol map before any of them is handed over to LLVM
function check_symbol_names(names, what="symbol")
    for name in names
        name isa LLVMSymbol ||
            throw(ArgumentError("$what names must be LLVMSymbols, got a $(typeof(name))"))
    end
    allunique(names) || throw(ArgumentError("duplicate $what names"))
    return
end

"""
    LLVM.JITDylib

A JIT dynamic library: a set of symbol definitions in an execution session, which can be
looked up and linked against.

# Properties

    jd.default_resource_tracker

The resource tracker that tracks code added to the JITDylib without an explicit tracker.
The tracker is owned by the JITDylib, so disposing of it is not required (and does
nothing).
"""
@checked struct JITDylib
    ref::API.LLVMOrcJITDylibRef
end
@properties JITDylib

Base.unsafe_convert(::Type{API.LLVMOrcJITDylibRef}, jd::JITDylib) = jd.ref

main_dylib(lljit::LLJIT) = JITDylib(API.LLVMOrcLLJITGetMainJITDylib(lljit))

@property LLJIT main_dylib


"""
    JITDylib(es::ExecutionSession, name; bare=false)

Adds a new JITDylib to the ExecutionSession. The name must be unique and
the `bare=true` no standard platform symbols are made available.
"""
function JITDylib(es::ExecutionSession, name; bare=false)
    if bare
        ref = API.LLVMOrcExecutionSessionCreateBareJITDylib(es, name)
    else
        ref = Ref{API.LLVMOrcJITDylibRef}()
        @check API.LLVMOrcExecutionSessionCreateJITDylib(es, ref, name)
        ref = ref[]
    end
    JITDylib(ref)
end
Base.string(jd::JITDylib) = unsafe_message(API.LLVMDumpJitDylibToString(jd))

function Base.show(io::IO, ::MIME"text/plain", jd::JITDylib)
    output = string(jd)
    print(io, output)
end

## definition generators

"""
    DefinitionGenerator

A generator that ORC consults when a lookup fails to find a symbol in a
[`JITDylib`](@ref), giving it the opportunity to define that symbol.

Attach a generator to a JITDylib with [`add!`](@ref add!(::JITDylib, ::DefinitionGenerator)),
which transfers ownership to the JITDylib. A generator that is never added should be
disposed of with [`dispose`](@ref dispose(::DefinitionGenerator)).

See also: [`DynamicLibrarySearchGenerator`](@ref), [`CustomDefinitionGenerator`](@ref).
"""
mutable struct DefinitionGenerator
    ref::API.LLVMOrcDefinitionGeneratorRef
    owned::Bool

    function DefinitionGenerator(ref::API.LLVMOrcDefinitionGeneratorRef)
        ref == C_NULL && throw(UndefRefError())
        mark_alloc(new(ref, true))
    end
end
Base.unsafe_convert(::Type{API.LLVMOrcDefinitionGeneratorRef}, dg::DefinitionGenerator) =
    check_owned(dg).ref

"""
    dispose(dg::DefinitionGenerator)

Dispose of a definition generator, unless it was added to a JITDylib (which owns it then).
"""
dispose(dg::DefinitionGenerator) = dispose_owned(API.LLVMOrcDisposeDefinitionGenerator, dg)

"""
    add!(jd::JITDylib, dg::DefinitionGenerator)

Attach the definition generator `dg` to `jd`. The JITDylib takes ownership of the
generator, so `dg` can't be added again, and disposing of it does nothing.
"""
function add!(jd::JITDylib, dg::DefinitionGenerator)
    API.LLVMOrcJITDylibAddGenerator(jd, consume!(dg))
    return
end

"""
    DynamicLibrarySearchGenerator(jit)
    DynamicLibrarySearchGenerator(jit, path::AbstractString)

Create a [`DefinitionGenerator`](@ref) that resolves symbols by looking them up in the
current process, or in the dynamic library at `path`. That library is loaded when creating
the generator, and stays loaded for the remainder of the process.

The generator is specific to the target of `jit`, whose linker mangling it undoes before
looking up symbols. The do-block form disposes of the generator afterwards, unless it was
added to a JITDylib.
"""
DynamicLibrarySearchGenerator(jit::Union{LLJIT,JuliaOJIT}) =
    process_search_generator(global_prefix(jit))

DynamicLibrarySearchGenerator(jit::Union{LLJIT,JuliaOJIT}, path::AbstractString) =
    library_search_generator(path, global_prefix(jit))

DynamicLibrarySearchGenerator(f::Core.Function, args...) =
    with_disposal(f, DynamicLibrarySearchGenerator(args...))

function process_search_generator(prefix)
    ref = Ref{API.LLVMOrcDefinitionGeneratorRef}()
    @check API.LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(ref, prefix, C_NULL,
                                                                    C_NULL)
    DefinitionGenerator(ref[])
end

function library_search_generator(path, prefix)
    ref = Ref{API.LLVMOrcDefinitionGeneratorRef}()
    @check API.LLVMOrcCreateDynamicLibrarySearchGeneratorForPath(ref, path, prefix, C_NULL,
                                                                 C_NULL)
    DefinitionGenerator(ref[])
end

function __try_to_generate(generator::API.LLVMOrcDefinitionGeneratorRef, ctx::Ptr{Cvoid},
                           lookup_state::Ptr{API.LLVMOrcLookupStateRef},
                           kind::API.LLVMOrcLookupKind, jd::API.LLVMOrcJITDylibRef,
                           jd_flags::API.LLVMOrcJITDylibLookupFlags,
                           lookup_set::API.LLVMOrcCLookupSet, lookup_set_size::Csize_t)
    dg = Base.unsafe_pointer_to_objref(ctx)::CustomDefinitionGenerator
    try
        elements = Base.unsafe_wrap(Array, lookup_set, lookup_set_size)
        symbols = [LLVMSymbol(el.Name) => el.LookupFlags for el in elements]
        dg.callback(kind, JITDylib(jd), jd_flags, symbols)
        return API.LLVMErrorRef(C_NULL)
    catch err
        # Julia exceptions cannot unwind through LLVM, so report the failure to ORC
        # and keep the exception around for check_callback_error!.
        _capture_callback_exception!(dg, err)
        msg = try
            sprint(showerror, err)
        catch
            "unprintable $(typeof(err))"
        end
        return API.LLVMCreateStringError("exception in ORC definition generator: $msg")
    end
end

function __dispose_generator(ctx::Ptr{Cvoid})
    dg = Base.unsafe_pointer_to_objref(ctx)::CustomDefinitionGenerator
    @lock CUSTOM_DG_LOCK delete!(CUSTOM_DG_ROOTS, dg)
    return
end

"""
    CustomDefinitionGenerator(f)

Create a definition generator that calls `f(kind, jd, jd_flags, lookup_set)` whenever a
lookup fails to find symbols in the JITDylib the generator is attached to. The arguments
mirror those of LLVM's `DefinitionGenerator::tryToGenerate`:

- `kind::LLVM.LookupKind.T`: whether this is a static (linker) lookup, or a
  `dlsym`-like one;
- `jd::JITDylib`: the JITDylib to define the symbols in;
- `jd_flags::LLVM.JITDylibLookupFlags.T`: whether the lookup matches only exported
  symbols, or all of them;
- `lookup_set::Vector{Pair{LLVMSymbol,LLVM.SymbolLookupFlags.T}}`: the
  linker-mangled names of the symbols that were not found, each paired with a flag
  indicating whether the symbol is required or only weakly referenced.

`f` should define the symbols it can provide in `jd`, e.g., using [`define!`](@ref).
Symbols it does not define are left to other generators and JITDylibs in the search order,
and its return value is ignored.
The names in `lookup_set` are only valid during the call; retain them with `retain`
before handing them to functions that take ownership, like `absolute_symbols`.

If `f` throws, the lookup fails with an LLVM error that includes the exception message. The
original exception can be retrieved by calling `check_callback_error!` on the generator,
which rethrows it as a [`CallbackException`](@ref).

`f` runs synchronously on the thread performing the lookup, while LLVM holds locks that
serialize definition generation. It must not perform lookups that can reach the same
JITDylib again, as that may deadlock. Asynchronous generation (suspending the lookup) is
not supported.

The generator is used like a [`DefinitionGenerator`](@ref): attach it to a JITDylib
with `add!`, which keeps it alive for the lifetime of that JITDylib, or `dispose` it. Its
callback stays rooted until LLVM destroys the generator.
"""
mutable struct CustomDefinitionGenerator
    callback
    exception::Union{Nothing,Tuple{Any,Vector}}
    dg::DefinitionGenerator

    function CustomDefinitionGenerator(callback)
        this = new(callback, nothing)

        # LLVM only holds a raw pointer to the generator, so root it until LLVM disposes
        # of it (either when its JITDylib is destroyed, or when we dispose of it manually).
        @lock CUSTOM_DG_LOCK push!(CUSTOM_DG_ROOTS, this)

        ref = API.LLVMOrcCreateCustomCAPIDefinitionGenerator(
            @cfunction(__try_to_generate, API.LLVMErrorRef,
                       (API.LLVMOrcDefinitionGeneratorRef, Ptr{Cvoid},
                        Ptr{API.LLVMOrcLookupStateRef}, API.LLVMOrcLookupKind,
                        API.LLVMOrcJITDylibRef, API.LLVMOrcJITDylibLookupFlags,
                        API.LLVMOrcCLookupSet, Csize_t)),
            Base.pointer_from_objref(this),
            @cfunction(__dispose_generator, Cvoid, (Ptr{Cvoid},)))
        this.dg = DefinitionGenerator(ref)
        return this
    end
end

const CUSTOM_DG_ROOTS = Base.IdSet{CustomDefinitionGenerator}()
const CUSTOM_DG_LOCK = ReentrantLock()

add!(jd::JITDylib, dg::CustomDefinitionGenerator) = add!(jd, dg.dg)
dispose(dg::CustomDefinitionGenerator) = dispose(dg.dg)

"""
    check_callback_error!(obj)

Rethrow the first exception that was captured from a Julia callback of `obj` (e.g., a
[`CustomDefinitionGenerator`](@ref) or [`CustomMaterializationUnit`](@ref)) as a
[`CallbackException`](@ref), clearing it. Returns `nothing` if no exception was captured.

Exceptions cannot propagate through LLVM, so callbacks that throw are reported to LLVM as
failures instead, typically resulting in a generic error from the operation that triggered
the callback.
"""
function check_callback_error! end

function check_callback_error!(dg::CustomDefinitionGenerator)
    exception = _take_callback_exception!(dg)
    exception === nothing && return nothing
    err, bt = exception
    throw(CallbackException("ORC definition generator", err, bt))
end


"""
    lookup_dylib(es::ExecutionSession, name) -> Union{JITDylib,Nothing}

Get the JITDylib called `name` in `es`, or `nothing` if there is none.
"""
function lookup_dylib(es::ExecutionSession, name)
    ref = API.LLVMOrcExecutionSessionGetJITDylibByName(es, name)
    if ref == C_NULL
        return
    end
    JITDylib(ref)
end

"""
    add!(lljit::LLJIT, jd::JITDylib, obj::MemoryBuffer)
    add!(lljit::LLJIT, jd::JITDylib, tsm::ThreadSafeModule)
    add!(lljit::LLJIT, rt::ResourceTracker, obj_or_tsm)

Add an object file or IR module to `jd`, or to the JITDylib of the resource tracker `rt`.
The code is compiled and linked lazily, when one of its symbols is looked up. The object
or module is consumed, even if adding it fails.
"""
function add!(lljit::LLJIT, jd::JITDylib, obj::MemoryBuffer)
    err = API.LLVMOrcLLJITAddObjectFile(lljit, jd, obj)
    mark_dispose(obj)   # consumed, even on failure
    @check err
    return
end

# LLVMOrcLLJITAddObjectFileWithRT(J, RT, ObjBuffer)

function add!(lljit::LLJIT, jd::JITDylib, mod::ThreadSafeModule)
    # consumed, even on failure
    err = API.LLVMOrcLLJITAddLLVMIRModule(lljit, jd, consume!(mod))
    @check err
    return
end

# LLVMOrcLLJITAddLLVMIRModuleWithRT(J, JD, TSM)

"""
    empty!(jd::JITDylib)

Remove all code and data from `jd`, releasing the resources of all of its resource
trackers.
"""
function Base.empty!(jd::JITDylib)
    @check API.LLVMOrcJITDylibClear(jd)
    return jd
end


## resource trackers

"""
    ResourceTracker(jd::JITDylib)
    ResourceTracker(f, jd::JITDylib)

Create a resource tracker for `jd`. Code added using a tracker, with
`add!(jit, rt, tsm_or_object)`, can later be removed from the JIT using
[`remove!`](@ref remove!(::ResourceTracker)), without affecting other code in `jd`.

Resource trackers are reference counted: the returned reference needs to be released using
[`dispose`](@ref dispose(::ResourceTracker)), or by using the do-block form. Releasing a
tracker does not remove the code it tracks; that code then remains in `jd` until `jd` is
cleared.

See also: the [`default_resource_tracker`](@ref LLVM.JITDylib) property of a
JITDylib.
"""
@checked mutable struct ResourceTracker
    # mutable, so that the memory checker can tell multiple references apart
    ref::API.LLVMOrcResourceTrackerRef
    owned::Bool     # whether we hold a reference that needs to be released
end
ResourceTracker(ref::API.LLVMOrcResourceTrackerRef) = ResourceTracker(ref, true)
Base.unsafe_convert(::Type{API.LLVMOrcResourceTrackerRef}, rt::ResourceTracker) = rt.ref

function ResourceTracker(jd::JITDylib)
    mark_alloc(ResourceTracker(API.LLVMOrcJITDylibCreateResourceTracker(jd)))
end

ResourceTracker(f::Core.Function, jd::JITDylib) =
    with_disposal(f, ResourceTracker(jd))

function default_resource_tracker(jd::JITDylib)
    # contrary to its documentation, LLVMOrcJITDylibGetDefaultResourceTracker does not
    # retain the tracker, so we should not release it either.
    # See https://github.com/llvm/llvm-project/issues/227221
    ResourceTracker(API.LLVMOrcJITDylibGetDefaultResourceTracker(jd), false)
end

@property JITDylib default_resource_tracker

"""
    dispose(rt::ResourceTracker)

Release a reference to the resource tracker `rt`. This does not remove the tracked code.
"""
function dispose(rt::ResourceTracker)
    rt.owned || return
    mark_dispose(API.LLVMOrcReleaseResourceTracker, rt)
end

"""
    remove!(rt::ResourceTracker)

Remove all code and data tracked by `rt` from the JIT. The tracker becomes defunct, and
cannot be used to add code anymore (but still needs to be disposed of).

It is the caller's responsibility to ensure that the removed code is not executing, and
that no pointers into it are used anymore.
"""
function remove!(rt::ResourceTracker)
    @check API.LLVMOrcResourceTrackerRemove(rt)
    return
end

"""
    transfer!(dst::ResourceTracker, src::ResourceTracker)

Transfer tracking of all resources from `src` to `dst`, which should belong to the same
JITDylib.
"""
function transfer!(dst::ResourceTracker, src::ResourceTracker)
    API.LLVMOrcResourceTrackerTransferTo(src, dst)
    return
end

function add!(lljit::LLJIT, rt::ResourceTracker, obj::MemoryBuffer)
    err = API.LLVMOrcLLJITAddObjectFileWithRT(lljit, rt, obj)
    mark_dispose(obj)   # consumed, even on failure
    @check err
    return
end

function add!(lljit::LLJIT, rt::ResourceTracker, mod::ThreadSafeModule)
    # consumed, even on failure
    err = API.LLVMOrcLLJITAddLLVMIRModuleWithRT(lljit, rt, consume!(mod))
    @check err
    return
end


## lookup

"""
    OrcTargetAddress

An address in the process that executes JIT-compiled code, as returned by
[`lookup`](@ref lookup(::LLJIT, ::Any)). Use `pointer` to convert it to a pointer.
"""
struct OrcTargetAddress
    ptr::API.LLVMOrcJITTargetAddress
end
Base.convert(::Type{API.LLVMOrcJITTargetAddress}, addr::OrcTargetAddress) = addr.ptr

Base.pointer(addr::OrcTargetAddress) = reinterpret(Ptr{Cvoid}, addr.ptr % UInt) # LLVMOrcTargetAddress is UInt64 even on 32-bit

OrcTargetAddress(ptr::Ptr{Cvoid}) = OrcTargetAddress(reinterpret(UInt, ptr))

"""
    lookup(lljit::LLJIT, [jd::JITDylib], name) -> OrcTargetAddress

Look up the symbol with (unmangled) name `name` in `jd`, defaulting to the main JITDylib,
materializing it if necessary. Throws an [`LLVMException`](@ref) if the symbol cannot be
found or materialized. Use `pointer` to convert the resulting address to a pointer.
"""
function lookup(lljit::LLJIT, name)
    result = Ref{API.LLVMOrcJITTargetAddress}()
    @check API.LLVMOrcLLJITLookup(lljit, result, name)
    OrcTargetAddress(result[])
end

# state of an asynchronous execution session lookup
mutable struct SessionLookup
    done::Base.Event
    completed::Threads.Atomic{Bool}
    error::API.LLVMErrorRef
    address::API.LLVMOrcJITTargetAddress
    SessionLookup() = new(Base.Event(), Threads.Atomic{Bool}(false), C_NULL, 0)
end

# LLVM only holds a raw pointer to the lookup state, so root it until the lookup completes
const SESSION_LOOKUP_ROOTS = Base.IdSet{SessionLookup}()
const SESSION_LOOKUP_LOCK = ReentrantLock()

function __lookup_result(err::API.LLVMErrorRef, result::API.LLVMOrcCSymbolMapPairs,
                         num_pairs::Csize_t, ctx::Ptr{Cvoid})
    state = Base.unsafe_pointer_to_objref(ctx)::SessionLookup
    state.error = err
    if err == C_NULL
        # we only look up a single symbol
        state.address = unsafe_load(result).Sym.Address
    end
    # may be invoked from another thread, when materialization completes asynchronously
    state.completed[] = true
    notify(state.done)
    return
end

function lookup(lljit::LLJIT, jd::JITDylib, name)
    es = execution_session(lljit)
    order = Ref(API.LLVMOrcCJITDylibSearchOrderElement(
        jd.ref, API.LLVMOrcJITDylibLookupFlagsMatchAllSymbols))
    symbols = Ref(API.LLVMOrcCLookupSetElement(
        mangle(lljit, name), API.LLVMOrcSymbolLookupFlagsRequiredSymbol))

    # like LLJIT::lookup, but with the JITDylib as search order
    state = SessionLookup()
    @lock SESSION_LOOKUP_LOCK push!(SESSION_LOOKUP_ROOTS, state)
    try
        API.LLVMOrcExecutionSessionLookup(es, API.LLVMOrcLookupKindStatic, order, 1,
                                          symbols, 1,
                                          @cfunction(__lookup_result, Cvoid,
                                                     (API.LLVMErrorRef,
                                                      API.LLVMOrcCSymbolMapPairs,
                                                      Csize_t, Ptr{Cvoid})),
                                          Base.pointer_from_objref(state))
        wait(state.done)
    finally
        # only unroot once LLVM is done with the state; if waiting was interrupted,
        # leak it instead.
        if state.completed[]
            @lock SESSION_LOOKUP_LOCK delete!(SESSION_LOOKUP_ROOTS, state)
        end
    end
    @check state.error
    OrcTargetAddress(state.address)
end

"""
    IRTransformLayer

The layer of an [`LLJIT`](@ref) that transforms IR modules before they are compiled,
available as its `ir_transform_layer` property.
"""
@checked struct IRTransformLayer
    ref::API.LLVMOrcIRTransformLayerRef
    jit::LLJIT
end
Base.unsafe_convert(::Type{API.LLVMOrcIRTransformLayerRef}, il::IRTransformLayer) = il.ref

ir_transform_layer(lljit::LLJIT) =
    IRTransformLayer(API.LLVMOrcLLJITGetIRTransformLayer(lljit), lljit)

@property LLJIT ir_transform_layer

mutable struct IRTransform
    callback
    exception::Union{Nothing,Tuple{Any,Vector}}
    IRTransform(callback) = new(callback, nothing)
end

function __ir_transform(ctx::Ptr{Cvoid}, tsm_ref::Ptr{API.LLVMOrcThreadSafeModuleRef},
                        mr::API.LLVMOrcMaterializationResponsibilityRef)
    state = Base.unsafe_pointer_to_objref(ctx)::IRTransform
    tsm = ThreadSafeModule(unsafe_load(tsm_ref); borrowed=true)
    try
        state.callback(tsm, MaterializationResponsibility(mr, false))
        return API.LLVMErrorRef(C_NULL)
    catch err
        _capture_callback_exception!(state, err)
        # on failure, LLVM expects us to have disposed of the module
        API.LLVMOrcDisposeThreadSafeModule(unsafe_load(tsm_ref))
        unsafe_store!(tsm_ref, C_NULL)
        msg = try
            sprint(showerror, err)
        catch
            "unprintable $(typeof(err))"
        end
        return API.LLVMCreateStringError("exception in ORC IR transform: $msg")
    finally
        # the module is only borrowed for the duration of the transformation
        tsm.borrowed = false
    end
end

"""
    transform!(f, layer::IRTransformLayer)

Install `f(tsm::ThreadSafeModule, mr::MaterializationResponsibility)` as the
transformation that `layer` applies to IR modules before they are compiled, replacing any
previous one. `f` should modify the module in place, e.g., by running an optimization
pipeline on it:

```julia
transform!(lljit.ir_transform_layer) do tsm, mr
    tsm() do mod
        run!("default<O2>", mod)
    end
end
```

Both arguments are borrowed: `f` should not dispose of them, or pass them to APIs that take
ownership. The transformation is kept alive for as long as the JIT, and should be installed
before any code is added to it. It may be called on whichever thread materializes code.

If `f` throws, materialization of the module fails, and the original exception can be
retrieved by calling [`check_callback_error!`](@ref) on the layer.
"""
function transform!(f, il::IRTransformLayer)
    state = IRTransform(f)
    # LLVM only holds a raw pointer to the transformation. Earlier transformations may
    # still be in use, so keep all of them alive until the JIT is disposed of.
    push!(il.jit.roots, state)
    API.LLVMOrcIRTransformLayerSetTransform(il,
        @cfunction(__ir_transform, API.LLVMErrorRef,
                   (Ptr{Cvoid}, Ptr{API.LLVMOrcThreadSafeModuleRef},
                    API.LLVMOrcMaterializationResponsibilityRef)),
        Base.pointer_from_objref(state))
    return
end

function check_callback_error!(il::IRTransformLayer)
    for state in il.jit.roots
        state isa IRTransform || continue
        exception = _take_callback_exception!(state)
        if exception !== nothing
            err, bt = exception
            throw(CallbackException("ORC IR transform", err, bt))
        end
    end
    return nothing
end


"""
    MaterializationResponsibility

The responsibility for materializing a set of symbols, as passed to the callback of a
[`CustomMaterializationUnit`](@ref). It is fulfilled by emitting code that defines
these symbols, e.g., using [`emit!`](@ref).

# Properties

    mr.requested_symbols

The names of the symbols that were requested from the materialization unit that `mr` is
responsible for, as a read-only view. The names are only fetched while iterating the view,
so use `collect` to get a vector.

These names are borrowed: retain them before handing them to APIs that take ownership, or
using them after the responsibility has been fulfilled.
"""
@checked mutable struct MaterializationResponsibility
    ref::API.LLVMOrcMaterializationResponsibilityRef
    # whether we own the responsibility, i.e., it has not been consumed (e.g., by emit)
    # and was not borrowed from LLVM
    owned::Bool
end
@properties MaterializationResponsibility
MaterializationResponsibility(ref::API.LLVMOrcMaterializationResponsibilityRef) =
    MaterializationResponsibility(ref, true)
Base.unsafe_convert(::Type{API.LLVMOrcMaterializationResponsibilityRef}, mr::MaterializationResponsibility) = mr.ref

function consume!(mr::MaterializationResponsibility)
    mr.owned || throw(ArgumentError("cannot consume a materialization responsibility that was already consumed or that is borrowed"))
    mr.owned = false
    return mr
end

"""
    emit!(layer, mr::MaterializationResponsibility, tsm::ThreadSafeModule)

Emit the IR module `tsm` through `layer` (an [`IRTransformLayer`](@ref) or
`IRCompileLayer`) to fulfill the responsibility `mr`. Both `mr` and `tsm` are
consumed; a responsibility that is borrowed, e.g., by an IR transformation, cannot be
emitted.
"""
function emit!(il::IRTransformLayer, mr::MaterializationResponsibility, tsm::ThreadSafeModule)
    check_consumable(tsm)
    consume!(mr)
    API.LLVMOrcIRTransformLayerEmit(il, mr, consume!(tsm))
end


struct MaterializationResponsibilityRequestedSymbolSet
    mr::MaterializationResponsibility
end

requested_symbols(mr::MaterializationResponsibility) =
    MaterializationResponsibilityRequestedSymbolSet(mr)

@property MaterializationResponsibility requested_symbols

function collect_requested_symbols(mr::MaterializationResponsibility)
    N = Ref{Csize_t}()
    ptr = API.LLVMOrcMaterializationResponsibilityGetRequestedSymbols(mr, N)
    syms = map(LLVMSymbol, Base.unsafe_wrap(Array, ptr, N[], own=false))
    API.LLVMOrcDisposeSymbols(ptr)
    return syms
end

Base.eltype(::Type{MaterializationResponsibilityRequestedSymbolSet}) = LLVMSymbol

Base.length(set::MaterializationResponsibilityRequestedSymbolSet) =
    length(collect_requested_symbols(set.mr))

# fetch the names once per iteration, as LLVM only provides a copy of all of them
function Base.iterate(set::MaterializationResponsibilityRequestedSymbolSet,
                      state=(collect_requested_symbols(set.mr), 1))
    syms, i = state
    i > length(syms) && return nothing
    return syms[i], (syms, i + 1)
end

abstract type AbstractMaterializationUnit end

"""
    MaterializationUnit

A unit that promises to define a set of symbols, and that materializes their definitions
when one of them is looked up, as created by [`absolute_symbols`](@ref) and
[`lazy_reexports`](@ref) (see [`CustomMaterializationUnit`](@ref) for units implemented
in Julia).

A unit is consumed by adding it to a JITDylib with [`define!`](@ref); otherwise, it needs
to be disposed of using `dispose`, which does nothing once it has been consumed.
"""
mutable struct MaterializationUnit <: AbstractMaterializationUnit
    ref::API.LLVMOrcMaterializationUnitRef
    owned::Bool

    function MaterializationUnit(ref::API.LLVMOrcMaterializationUnitRef)
        ref == C_NULL && throw(UndefRefError())
        mark_alloc(new(ref, true))
    end
end
Base.unsafe_convert(::Type{API.LLVMOrcMaterializationUnitRef}, mu::MaterializationUnit) =
    check_owned(mu).ref

dispose(mu::MaterializationUnit) = dispose_owned(API.LLVMOrcDisposeMaterializationUnit, mu)

"""
    define!(jd::JITDylib, mu)

Add the materialization unit `mu` to `jd`, which takes ownership of it. The unit is
consumed even if this throws: on failure (e.g., because one of its symbols is already
defined in `jd`) it is disposed of before the error is rethrown as an
[`LLVMException`](@ref).
"""
function define!(jd::JITDylib, mu::AbstractMaterializationUnit)
    ref = consume!(materialization_unit(mu))
    err = API.LLVMOrcJITDylibDefine(jd, ref)
    if err != C_NULL
        # on failure, ownership of the materialization unit stays with us
        API.LLVMOrcDisposeMaterializationUnit(ref)
        throw(convert(LLVMException, LLVMError(err)))
    end
    return
end

materialization_unit(mu::MaterializationUnit) = mu


mutable struct CustomMaterializationUnit <: AbstractMaterializationUnit
    materialize
    discard
    exception::Union{Nothing,Tuple{Any,Vector}}
    mu::MaterializationUnit
    function CustomMaterializationUnit(materialize, discard)
        new(materialize, discard, nothing)
    end
end
Base.cconvert(::Type{API.LLVMOrcMaterializationUnitRef}, mu::CustomMaterializationUnit) = mu.mu
materialization_unit(mu::CustomMaterializationUnit) = mu.mu
dispose(mu::CustomMaterializationUnit) = dispose(mu.mu)

# LLVM only holds a raw pointer to custom materialization units, so root them until LLVM
# either materializes or destroys them.
const CUSTOM_MU_ROOTS = Base.IdSet{CustomMaterializationUnit}()
const CUSTOM_MU_LOCK = ReentrantLock()

function check_callback_error!(mu::CustomMaterializationUnit)
    exception = _take_callback_exception!(mu)
    exception === nothing && return nothing
    err, bt = exception
    throw(CallbackException("ORC materialization unit", err, bt))
end

function __materialize(ctx::Ptr{Cvoid}, mr::API.LLVMOrcMaterializationResponsibilityRef)
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    responsibility = MaterializationResponsibility(mr, true)
    try
        mu.materialize(responsibility)
    catch err
        _capture_callback_exception!(mu, err)
        # only fail materialization if the responsibility wasn't handed off already
        if responsibility.owned
            responsibility.owned = false
            API.LLVMOrcMaterializationResponsibilityFailMaterialization(mr)
            API.LLVMOrcDisposeMaterializationResponsibility(mr)
        end
    finally
        # LLVM does not call the destroy callback for materialized units
        @lock CUSTOM_MU_LOCK delete!(CUSTOM_MU_ROOTS, mu)
    end
    nothing
end

function __discard(ctx::Ptr{Cvoid}, jd::API.LLVMOrcJITDylibRef, symbol::API.LLVMOrcSymbolStringPoolEntryRef)
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    try
        mu.discard(JITDylib(jd), LLVMSymbol(symbol))
    catch err
        # ORC's discard callback has no failure return. Preserve the exception
        # on the owned materialization unit for a later Julia-side check.
        _capture_callback_exception!(mu, err)
    end
    nothing
end

function __destroy(ctx::Ptr{Cvoid})
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    @lock CUSTOM_MU_LOCK delete!(CUSTOM_MU_ROOTS, mu)
    nothing
end

"""
    CustomMaterializationUnit(name, symbols, materialize, discard; init=nothing)

Create a materialization unit that promises to define `symbols`, a collection of
`name => flags` pairs mapping each [`LLVMSymbol`](@ref) to its [`SymbolFlags`](@ref).
Add it to a JITDylib with [`define!`](@ref).

When any of these symbols is looked up, `materialize(mr)` is called with a
`MaterializationResponsibility` for the symbols, which it should fulfill, e.g., by
generating IR and emitting it with `emit!(layer, mr, tsm)`; its
[`requested_symbols`](@ref LLVM.MaterializationResponsibility) property tells which symbols were
requested. If a symbol is overridden by another definition before it was materialized,
`discard(jd, name)` is called instead.

Like other [`MaterializationUnit`](@ref)s, the unit is consumed by [`define!`](@ref), and
needs to be disposed of otherwise. Its callbacks stay rooted until LLVM materializes or
destroys the unit.

If `materialize` throws, materialization of the symbols fails, and lookups report an LLVM
error. Retrieve the original exception by calling [`check_callback_error!`](@ref) on the
unit. An exception in `discard` is only reported that way.

The unit takes ownership of the symbol names. `init` can be used to specify an
initializer symbol (an [`LLVMSymbol`](@ref)), which needs to be one of the `symbols`,
with flags that have `materialization_side_effects_only` set. The unit takes ownership of
an additional reference to it.
"""
function CustomMaterializationUnit(name, symbols::Union{AbstractVector{<:Pair},AbstractDict},
                                   materialize, discard;
                                   init::Union{Nothing,LLVMSymbol}=nothing)
    # validate everything before taking ownership of the names
    check_symbol_names([first(pair) for pair in symbols])
    pairs = API.LLVMOrcCSymbolFlagsMapPair[
        API.LLVMOrcCSymbolFlagsMapPair(sym, convert(API.LLVMJITSymbolFlags,
                                                    check_symbol_flags(flags)))
        for (sym, flags) in symbols]
    if init !== nothing
        # LLVM asserts that the initializer is one of the symbols, and requires it to
        # only have side effects
        i = findfirst(pair -> first(pair) == init, collect(symbols))
        i === nothing &&
            throw(ArgumentError("the initializer symbol needs to be one of the symbols"))
        last(collect(symbols)[i]).materialization_side_effects_only ||
            throw(ArgumentError("the initializer symbol needs to be materialization_side_effects_only"))
    end
    init_ref = init === nothing ? API.LLVMOrcSymbolStringPoolEntryRef(C_NULL) : init.ref

    this = CustomMaterializationUnit(materialize, discard)
    # LLVM doesn't call back before the unit is defined or disposed of, so only root the
    # unit once it has been created, so that a failure to create it doesn't leak the root
    ref = API.LLVMOrcCreateCustomMaterializationUnit(
        name,
        Base.pointer_from_objref(this), # escaping this, rooted in CUSTOM_MU_ROOTS below
        pairs,
        length(pairs),
        init_ref,
        @cfunction(__materialize, Cvoid, (Ptr{Cvoid}, API.LLVMOrcMaterializationResponsibilityRef)),
        @cfunction(__discard, Cvoid, (Ptr{Cvoid}, API.LLVMOrcJITDylibRef, API.LLVMOrcSymbolStringPoolEntryRef) ),
        @cfunction(__destroy, Cvoid, (Ptr{Cvoid},))
    )
    @lock CUSTOM_MU_LOCK push!(CUSTOM_MU_ROOTS, this)
    this.mu = MaterializationUnit(ref)
    return this
end

"""
    absolute_symbols(name => address, ...)
    absolute_symbols(name => (address, flags), ...)
    absolute_symbols(pairs)

Create a materialization unit that defines each symbol `name` (a [`LLVMSymbol`](@ref))
at a fixed `address` (a pointer, integer, or [`OrcTargetAddress`](@ref)), e.g., to make
host functions or data available to JIT-compiled code. Symbols default to being exported;
pass [`SymbolFlags`](@ref) to change that (absolute symbols have an address, so they can't
be `materialization_side_effects_only`). The pairs can also be passed as a collection,
e.g., a vector or a dictionary.

The unit takes ownership of the symbol names, and should be added to a JITDylib using
[`define!`](@ref):

```julia
define!(jd, absolute_symbols(mangle(lljit, "counter") => pointer(counter)))
```
"""
absolute_symbols(pair::Pair{LLVMSymbol}, pairs::Pair{LLVMSymbol}...) =
    absolute_symbols([pair, pairs...])

function absolute_symbols(pairs::Union{AbstractVector{<:Pair},AbstractDict})
    # validate everything before taking ownership of the names
    check_symbol_names([first(pair) for pair in pairs])
    symbols = API.LLVMOrcCSymbolMapPair[]
    for (sym, def) in pairs
        address, flags = def isa Tuple{Any,Any} ? def : (def, SymbolFlags())
        # LLVM asserts when resolving such symbols
        check_symbol_flags(flags).materialization_side_effects_only &&
            throw(ArgumentError("absolute symbols can't be materialization_side_effects_only"))
        push!(symbols, API.LLVMOrcCSymbolMapPair(sym, API.LLVMJITEvaluatedSymbol(
            target_address(address), convert(API.LLVMJITSymbolFlags,
                                             check_symbol_flags(flags)))))
    end
    MaterializationUnit(API.LLVMOrcAbsoluteSymbols(symbols, length(symbols)))
end

target_address(ptr::Ptr) = API.LLVMOrcJITTargetAddress(reinterpret(UInt, ptr))
target_address(addr::Integer) = API.LLVMOrcJITTargetAddress(addr)
target_address(addr::OrcTargetAddress) = addr.ptr
target_address(addr) = throw(ArgumentError(
    "symbol addresses must be pointers, integers or OrcTargetAddresses, got a $(typeof(addr))"))

@checked struct IndirectStubsManager
    ref::API.LLVMOrcIndirectStubsManagerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcIndirectStubsManagerRef}, ism::IndirectStubsManager) = ism.ref

"""
    LocalIndirectStubsManager(triple)
    LocalIndirectStubsManager(f, triple)

Create a manager of indirect stubs for the current process, as used by
[`lazy_reexports`](@ref). Needs to be disposed of using `dispose`, or by using the
do-block form, after the last call of a stub it manages.
"""
function LocalIndirectStubsManager(triple)
    ref = API.LLVMOrcCreateLocalIndirectStubsManager(triple)
    IndirectStubsManager(ref)
end

LocalIndirectStubsManager(f::Core.Function, triple) =
    with_disposal(f, LocalIndirectStubsManager(triple))

function dispose(ism::IndirectStubsManager)
    API.LLVMOrcDisposeIndirectStubsManager(ism)
end

@checked mutable struct LazyCallThroughManager
    ref::API.LLVMOrcLazyCallThroughManagerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcLazyCallThroughManagerRef}, lcm::LazyCallThroughManager) = lcm.ref

"""
    LocalLazyCallThroughManager(triple, es::ExecutionSession)
    LocalLazyCallThroughManager(f, triple, es::ExecutionSession)

Create a manager of lazy call-throughs for the current process, as used by
[`lazy_reexports`](@ref). Needs to be disposed of using `dispose`, or by using the
do-block form, after the last call of a stub that uses it.
"""
function LocalLazyCallThroughManager(triple, es)
    ref = Ref{API.LLVMOrcLazyCallThroughManagerRef}()
    @check API.LLVMOrcCreateLocalLazyCallThroughManager(triple, es, C_NULL, ref)
    LazyCallThroughManager(ref[])
end

LocalLazyCallThroughManager(f::Core.Function, triple, es) =
    with_disposal(f, LocalLazyCallThroughManager(triple, es))

function dispose(lcm::LazyCallThroughManager)
    API.LLVMOrcDisposeLazyCallThroughManager(lcm)
end

"""
    lazy_reexports(lctm, ism, source_jd, aliases)

Create a materialization unit that defines lazy reexports of symbols in `source_jd`.
`aliases` is a collection of `alias => target` or `alias => (target, flags)` pairs of
[`LLVMSymbol`](@ref)s, with [`SymbolFlags`](@ref) that default to an exported and
callable symbol. Lazy reexports need to be callable.

Looking up an alias does not materialize its target. Instead, the alias resolves to a stub
(managed by the indirect stubs manager `ism`) that calls into the lazy call-through manager
`lctm` the first time it is called, which then looks up the target and updates the stub.
Both managers must stay alive for as long as the stubs can be called.

The unit takes ownership of one reference for each occurrence of a name: when multiple
aliases share a target, retain the target an additional time for every extra alias.
"""
function lazy_reexports(lctm::LazyCallThroughManager, ism::IndirectStubsManager,
                        jd::JITDylib, aliases::Union{AbstractVector{<:Pair},AbstractDict})
    # validate everything before taking ownership of the names
    check_symbol_names([first(pair) for pair in aliases], "alias")
    entries = API.LLVMOrcCSymbolAliasMapPair[]
    for (alias, def) in aliases
        target, flags = def isa Tuple{Any,Any} ? def : (def, SymbolFlags(callable=true))
        target isa LLVMSymbol ||
            throw(ArgumentError("alias targets must be LLVMSymbols, got a $(typeof(target))"))
        # LLVM asserts that lazy reexports are callable
        check_symbol_flags(flags).callable ||
            throw(ArgumentError("lazy reexports must be callable"))
        push!(entries, API.LLVMOrcCSymbolAliasMapPair(alias,
            API.LLVMOrcCSymbolAliasMapEntry(target, convert(API.LLVMJITSymbolFlags, flags))))
    end
    MaterializationUnit(API.LLVMOrcLazyReexports(lctm, ism, jd, entries, length(entries)))
end


# JuliaOJIT

@vocabulary ORC JuliaOJIT

execution_session(jljit::JuliaOJIT) =
    ExecutionSession(API.JLJITGetLLVMOrcExecutionSession(jljit))

@property JuliaOJIT execution_session

function mangle(jljit::JuliaOJIT, name)
    entry = API.JLJITMangleAndIntern(jljit, name)
    return LLVMSymbol(entry)
end

"""
    JITDylib(jljit::JuliaOJIT[, name])

Get or create a JITDylib from the Julia JIT.
On Julia >= 1.14.0-DEV.2171 (JuliaLang/julia#60988), creates a new JITDylib with
the given name prefix, linked to GlobalJD and SessionJD. On older Julia, returns
the shared external JITDylib (name parameter is ignored).
"""
@static if VERSION >= v"1.14.0-DEV.2171"
    function JITDylib(jljit::JuliaOJIT, name::String="")
        ref = API.JLJITCreateJITDylib(jljit, name)
        JITDylib(ref)
    end
else
    function JITDylib(jljit::JuliaOJIT, name::String="")
        ref = API.JLJITGetExternalJITDylib(jljit)
        JITDylib(ref)
    end
end

"""
    add!(jljit::JuliaOJIT, jd::JITDylib, obj::MemoryBuffer)
    add!(jljit::JuliaOJIT, jd::JITDylib, tsm::ThreadSafeModule)

Add an object file or IR module to `jd` in Julia's JIT. The object or module is consumed,
even if adding it fails.
"""
function add!(jljit::JuliaOJIT, jd::JITDylib, obj::MemoryBuffer)
    err = API.JLJITAddObjectFile(jljit, jd, obj)
    mark_dispose(obj)   # consumed, even on failure
    @check err
    return
end

function decorate_module(mod)
    # Add special values used by debuginfo to build the UnwindData table
    # registration for Win64.
    @static if VERSION >= v"1.14.0-DEV.2446"
        @ccall jl_decorate_llvm_module(mod::LLVM.API.LLVMModuleRef)::Cvoid
        return nothing
    end

    # This mirrors `jl_decorate_module` in Julia's src/jitlayers.cpp.
    # TODO: check the triple, not the system
    if Sys.iswindows() && Sys.ARCH == :x86_64 &&
       !contains(String(inline_asm(mod)), "__UnwindData")
        @static if VERSION >= v"1.12.0-DEV.1297"
            # Julia 1.12 (JuliaLang/julia#54841) rewrote the catchjmp asm to use
            # normal relocations and emit a PLT trampoline to __julia_personality.
            # The section used depends on the LLVM version.
            if LLVM.version() >= v"18"
                section = ".ltext,\"ax\",@progbits"
                offset = ".ltext"
            else
                section = ".text"
                offset = ".text"
            end
            push!(inline_asm(mod), """
                .section $section
                .globl __julia_personality

                .type __UnwindData,@object
                .p2align        2, 0x90
                __UnwindData:
                  .byte 0x09;
                  .byte 4;
                  .byte 2;
                  .byte 0x05;
                  .byte 4;
                  .byte 0x03;
                  .byte 1;
                  .byte 0x50;
                  .int __catchjmp - $offset;
                .size __UnwindData, 12

                .type __catchjmp,@function
                .p2align        2, 0x90
                __catchjmp:
                  movabsq \$__julia_personality, %rax
                  jmpq *%rax
                .size __catchjmp, . - __catchjmp
                """)
        else
            # Julia 1.10 and 1.11
            push!(inline_asm(mod), """
                .section .text
                .type   __UnwindData,@object
                .p2align        2, 0x90
                __UnwindData:
                    .zero   12
                    .size   __UnwindData, 12

                    .type   __catchjmp,@object
                    .p2align        2, 0x90
                __catchjmp:
                    .zero   12
                    .size   __catchjmp, 12""")
        end
    end
end

function add!(jljit::JuliaOJIT, jd::JITDylib, tsm::ThreadSafeModule)
    check_consumable(tsm)
    # Julia's debug info expects certain symbols to be present
    tsm() do mod
        decorate_module(mod)
    end
    # consumed, even on failure
    err = API.JLJITAddLLVMIRModule(jljit, jd, consume!(tsm))
    @check err
    return
end

function lookup(jljit::JuliaOJIT, jd::JITDylib, name, external_jd_only=false)
    result = Ref{API.LLVMOrcJITTargetAddress}()
    @static if VERSION >= v"1.14.0-DEV.2171"
        @check API.JLJITJDLookup(jljit, jd, result, name, external_jd_only)
    else
        @check API.JLJITLookup(jljit, result, name, external_jd_only)
    end
    OrcTargetAddress(result[])
end

@doc """
    lookup(jljit::JuliaOJIT, jd::JITDylib, name, [external_jd_only=false])

Look up the symbol with (unmangled) name `name` in `jd`, and the JITDylibs it links
against, materializing it if necessary.

On Julia versions before 1.14, `jd` is ignored, and the lookup searches all of Julia's
JITDylibs (or only the one returned by `JITDylib(jljit)` if `external_jd_only` is set).
""" lookup(::JuliaOJIT, ::JITDylib, ::Any)

"""
    IRCompileLayer

The layer of Julia's JIT that compiles IR modules, available as the `ir_compile_layer`
property of a [`JuliaOJIT`](@ref), for use with [`emit!`](@ref).
"""
@checked struct IRCompileLayer
    ref::API.LLVMOrcIRCompileLayerRef
    jit
end

Base.unsafe_convert(::Type{API.LLVMOrcIRCompileLayerRef}, il::IRCompileLayer) = il.ref

function emit!(il::IRCompileLayer, mr::MaterializationResponsibility, tsm::ThreadSafeModule)
    mr.owned || throw(ArgumentError("cannot consume a materialization responsibility that was already consumed or that is borrowed"))
    check_consumable(tsm)
    if il.jit isa JuliaOJIT
        # Julia's debug info expects certain symbols to be present
        tsm() do mod
            decorate_module(mod)
        end
    end
    consume!(mr)
    API.LLVMOrcIRCompileLayerEmit(il, mr, consume!(tsm))
end

ir_compile_layer(jljit::JuliaOJIT) = IRCompileLayer(API.JLJITGetIRCompileLayer(jljit), jljit)

@property JuliaOJIT ir_compile_layer
