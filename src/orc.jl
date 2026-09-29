export LLJITBuilder, LLJIT, ExecutionSession, JITDylib, OrcTargetAddress
export TargetMachineBuilder, targetmachinebuilder!, linkinglayercreator!
export mangle, lookup, intern
export ObjectLinkingLayer, register!

include("executionengine/utils.jl")

@checked struct TargetMachineBuilder
    ref::API.LLVMOrcJITTargetMachineBuilderRef
end
Base.unsafe_convert(::Type{API.LLVMOrcJITTargetMachineBuilderRef},
                    tmb::TargetMachineBuilder) = tmb.ref


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

function dispose(tmb::TargetMachineBuilder)
    API.LLVMOrcDisposeJITTargetMachineBuilder(tmb)
end

include("executionengine/lljit.jl")

@checked struct ExecutionSession
    ref::API.LLVMOrcExecutionSessionRef
end
Base.unsafe_convert(::Type{API.LLVMOrcExecutionSessionRef}, es::ExecutionSession) = es.ref

function ExecutionSession(lljit::LLJIT)
    es = API.LLVMOrcLLJITGetExecutionSession(lljit)
    ExecutionSession(es)
end

@checked struct ObjectLinkingLayer
    ref::API.LLVMOrcObjectLayerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcObjectLayerRef}, oll::ObjectLinkingLayer) = oll.ref

"""
    ObjectLinkingLayer(es::ExecutionSession, triple::String=LLVM.triple();
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

The triple defaults to the host's. In a [`linkinglayercreator!`](@ref) callback, pass the
triple the callback receives:

```julia
linkinglayercreator!(builder) do es, triple
    ObjectLinkingLayer(es, triple)
end
```

On LLVM 21 and newer, that is the triple of the process executing the code rather than
that of the target machine, so pass the target's triple explicitly when JIT-compiling for
a different object format.
"""
function ObjectLinkingLayer(es::ExecutionSession, triple::String=LLVM.triple();
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

function dispose(oll::ObjectLinkingLayer)
    API.LLVMOrcDisposeObjectLayer(oll)
end

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
        return layer.ref
    catch err
        _capture_callback_exception!(ollc, err)
        # The C callback has no error return. Give LLJIT a valid default layer
        # so construction can finish normally and the Julia wrapper can throw.
        return API.LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es)
    end
end

"""
    linkinglayercreator!(builder::LLJITBuilder, creator)

Install a Julia object-layer creator, called with the execution session and
target triple. The builder keeps it rooted until it is consumed by
[`LLJIT`](@ref). If it throws, the exception is rethrown as a
[`CallbackException`](@ref) after LLJIT construction returns through LLVM.
"""
function linkinglayercreator!(builder::LLJITBuilder, creator)
    linkinglayercreator!(builder, ObjectLinkingLayerCreator(creator))
end

function linkinglayercreator!(builder::LLJITBuilder, state::ObjectLinkingLayerCreator)
    state.exception = nothing
    push!(builder.roots, state)
    cb = @cfunction(ollc_callback,
                    API.LLVMOrcObjectLayerRef,
                    (Ptr{Cvoid}, API.LLVMOrcExecutionSessionRef, Ptr{Cchar}))
    linkinglayercreator!(builder, cb, Base.pointer_from_objref(state))
end

linkinglayercreator!(creator::Core.Function, builder::LLJITBuilder) =
    linkinglayercreator!(builder, creator)

include("executionengine/ts_module.jl")

"""
    LLVM.LLVMSymbol

An interned symbol name: an entry in the symbol string pool of an [`ExecutionSession`](@ref).
ORC identifies symbols by their linker-mangled names, which [`mangle`](@ref) computes and
interns. Symbols from the same session are equal if and only if their names are.

Symbols are reference counted. Functions that return a symbol, like `mangle` and
[`intern`](@ref), return a new reference, which should eventually be released with
[`LLVM.release`](@ref), or be passed to an API that takes ownership of it (e.g.,
[`LLVM.absolute_symbols`](@ref)). Use [`LLVM.retain`](@ref) to create an additional
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
    intern(es::ExecutionSession, name) -> LLVM.LLVMSymbol

Intern `name`, as is, in the symbol string pool of `es`. The caller owns the returned
reference; see [`LLVM.LLVMSymbol`](@ref).
"""
function intern(es::ExecutionSession, string)
    entry = API.LLVMOrcExecutionSessionIntern(es, string)
    LLVMSymbol(entry)
end

"""
    LLVM.release(sym::LLVM.LLVMSymbol)

Release a reference to the symbol `sym`.
"""
function release(sym::LLVMSymbol)
    API.LLVMOrcReleaseSymbolStringPoolEntry(sym)
end

"""
    LLVM.retain(sym::LLVM.LLVMSymbol)

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
    mangle(jit, name) -> LLVM.LLVMSymbol

Apply the target's linker mangling to `name` (e.g., prefixing an underscore on macOS), and
intern the result in the JIT's execution session. The caller owns the returned reference;
see [`LLVM.LLVMSymbol`](@ref).
"""
function mangle(lljit::LLJIT, name)
    entry = API.LLVMOrcLLJITMangleAndIntern(lljit, name)
    return LLVMSymbol(entry)
end


## symbol flags

"""
    LLVM.symbol_flags(; exported=true, callable=false, weak=false,
                      materialization_side_effects_only=false, target_flags=0)

Create the flags of a JIT symbol definition, as used by [`LLVM.absolute_symbols`](@ref),
[`LLVM.CustomMaterializationUnit`](@ref) and [`LLVM.lazy_reexports`](@ref).
"""
function symbol_flags(; exported::Bool=true, callable::Bool=false, weak::Bool=false,
                      materialization_side_effects_only::Bool=false,
                      target_flags::Integer=0)
    flags = UInt8(0)
    exported && (flags |= UInt8(API.LLVMJITSymbolGenericFlagsExported))
    weak && (flags |= UInt8(API.LLVMJITSymbolGenericFlagsWeak))
    callable && (flags |= UInt8(API.LLVMJITSymbolGenericFlagsCallable))
    materialization_side_effects_only &&
        (flags |= UInt8(API.LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly))
    return API.LLVMJITSymbolFlags(flags, target_flags)
end

@checked struct JITDylib
    ref::API.LLVMOrcJITDylibRef
end
Base.unsafe_convert(::Type{API.LLVMOrcJITDylibRef}, jd::JITDylib) = jd.ref

"""
    JITDylib(lljit::LLJIT)

Get the main JITDylib
"""
function JITDylib(lljit::LLJIT)
    ref = API.LLVMOrcLLJITGetMainJITDylib(lljit)
    JITDylib(ref)
end


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
if version() >= v"13"
Base.string(jd::JITDylib) = unsafe_message(API.LLVMDumpJitDylibToString(jd))

function Base.show(io::IO, ::MIME"text/plain", jd::JITDylib)
    output = string(jd)
    print(io, output)
end
end

## definition generators

"""
    LLVM.DefinitionGenerator

A generator that ORC consults when a lookup fails to find a symbol in a
[`JITDylib`](@ref), giving it the opportunity to define that symbol.

Attach a generator to a JITDylib with [`add!`](@ref add!(::JITDylib, ::LLVM.DefinitionGenerator)),
which transfers ownership to the JITDylib. A generator that is never added should be disposed
of with [`dispose`](@ref dispose(::LLVM.DefinitionGenerator)).

See also: [`LLVM.DynamicLibrarySearchGenerator`](@ref), [`LLVM.CustomDefinitionGenerator`](@ref).
"""
@checked struct DefinitionGenerator
    ref::API.LLVMOrcDefinitionGeneratorRef
end
Base.unsafe_convert(::Type{API.LLVMOrcDefinitionGeneratorRef}, dg::DefinitionGenerator) = dg.ref

"""
    dispose(dg::LLVM.DefinitionGenerator)

Dispose of a definition generator that was not added to a JITDylib.
"""
function dispose(dg::DefinitionGenerator)
    mark_dispose(API.LLVMOrcDisposeDefinitionGenerator, dg)
end

"""
    add!(jd::JITDylib, dg::LLVM.DefinitionGenerator)

Attach the definition generator `dg` to `jd`. The JITDylib takes ownership of the
generator, which should not be used or disposed of afterwards.
"""
function add!(jd::JITDylib, dg::DefinitionGenerator)
    API.LLVMOrcJITDylibAddGenerator(jd, dg)
    mark_dispose(dg)
    return
end

"""
    LLVM.DynamicLibrarySearchGenerator(jit)
    LLVM.DynamicLibrarySearchGenerator(jit, path::AbstractString)

Create a [`LLVM.DefinitionGenerator`](@ref) that resolves symbols by looking them up in the
current process, or in the dynamic library at `path`. That library is loaded when creating
the generator, and stays loaded for the remainder of the process.

The generator is specific to the target of `jit`, whose linker mangling it undoes before
looking up symbols.
"""
DynamicLibrarySearchGenerator(jit::Union{LLJIT,JuliaOJIT}) =
    process_search_generator(global_prefix(jit))

DynamicLibrarySearchGenerator(jit::Union{LLJIT,JuliaOJIT}, path::AbstractString) =
    library_search_generator(path, global_prefix(jit))

function process_search_generator(prefix)
    ref = Ref{API.LLVMOrcDefinitionGeneratorRef}()
    @check API.LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(ref, prefix, C_NULL,
                                                                    C_NULL)
    mark_alloc(DefinitionGenerator(ref[]))
end

function library_search_generator(path, prefix)
    ref = Ref{API.LLVMOrcDefinitionGeneratorRef}()
    @check API.LLVMOrcCreateDynamicLibrarySearchGeneratorForPath(ref, path, prefix, C_NULL,
                                                                 C_NULL)
    mark_alloc(DefinitionGenerator(ref[]))
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
        # and keep the exception around for check_callback_error.
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
    LLVM.CustomDefinitionGenerator(f)

Create a definition generator that calls `f(kind, jd, jd_flags, lookup_set)` whenever a
lookup fails to find symbols in the JITDylib the generator is attached to. The arguments
mirror those of LLVM's `DefinitionGenerator::tryToGenerate`:

- `kind::LLVM.API.LLVMOrcLookupKind`: whether this is a static (linker) lookup, or a
  `dlsym`-like one;
- `jd::JITDylib`: the JITDylib to define the symbols in;
- `jd_flags::LLVM.API.LLVMOrcJITDylibLookupFlags`: whether the lookup matches only exported
  symbols, or all of them;
- `lookup_set::Vector{Pair{LLVM.LLVMSymbol,LLVM.API.LLVMOrcSymbolLookupFlags}}`: the
  linker-mangled names of the symbols that were not found, each paired with a flag
  indicating whether the symbol is required or only weakly referenced.

`f` should define the symbols it can provide in `jd`, e.g., using [`LLVM.define`](@ref).
Symbols it does not define are left to other generators and JITDylibs in the search order.
The names in `lookup_set` are only valid during the call; retain them with `LLVM.retain`
before handing them to functions that take ownership, like `LLVM.absolute_symbols`.

If `f` throws, the lookup fails with an LLVM error that includes the exception message. The
original exception can be retrieved by calling `LLVM.check_callback_error` on the generator,
which rethrows it as a [`CallbackException`](@ref).

`f` runs synchronously on the thread performing the lookup, while LLVM holds locks that
serialize definition generation. It must not perform lookups that can reach the same
JITDylib again, as that may deadlock. Asynchronous generation (suspending the lookup) is
not supported.

The generator is used like a [`LLVM.DefinitionGenerator`](@ref): attach it to a JITDylib
with `add!`, which keeps it alive for the lifetime of that JITDylib, or `dispose` it.
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
        this.dg = mark_alloc(DefinitionGenerator(ref))
        return this
    end
end

const CUSTOM_DG_ROOTS = Base.IdSet{CustomDefinitionGenerator}()
const CUSTOM_DG_LOCK = ReentrantLock()

add!(jd::JITDylib, dg::CustomDefinitionGenerator) = add!(jd, dg.dg)
dispose(dg::CustomDefinitionGenerator) = dispose(dg.dg)

"""
    LLVM.check_callback_error(obj)

Rethrow the first exception that was captured from a Julia callback of `obj` (e.g., a
[`LLVM.CustomDefinitionGenerator`](@ref) or [`LLVM.CustomMaterializationUnit`](@ref)) as a
[`CallbackException`](@ref), clearing it. Returns `nothing` if no exception was captured.

Exceptions cannot propagate through LLVM, so callbacks that throw are reported to LLVM as
failures instead, typically resulting in a generic error from the operation that triggered
the callback.
"""
function check_callback_error end

function check_callback_error(dg::CustomDefinitionGenerator)
    dg.exception === nothing && return nothing
    err, bt = dg.exception
    dg.exception = nothing
    throw(CallbackException("ORC definition generator", err, bt))
end


function lookup_dylib(es::ExecutionSession, name)
    ref = API.LLVMOrcExecutionSessionGetJITDylibByName(es, name)
    if ref == C_NULL
        return
    end
    JITDylib(ref)
end

function add!(lljit::LLJIT, jd::JITDylib, obj::MemoryBuffer)
    err = API.LLVMOrcLLJITAddObjectFile(lljit, jd, obj)
    mark_dispose(obj)   # consumed, even on failure
    @check err
    return
end

# LLVMOrcLLJITAddObjectFileWithRT(J, RT, ObjBuffer)

function add!(lljit::LLJIT, jd::JITDylib, mod::ThreadSafeModule)
    err = API.LLVMOrcLLJITAddLLVMIRModule(lljit, jd, mod)
    mark_dispose(mod)   # consumed, even on failure
    @check err
    return
end

# LLVMOrcLLJITAddLLVMIRModuleWithRT(J, JD, TSM)

function Base.empty!(jd::JITDylib)
    @check API.LLVMOrcJITDylibClear(jd)
    return jd
end


## resource trackers

"""
    LLVM.ResourceTracker(jd::JITDylib)
    LLVM.ResourceTracker(f, jd::JITDylib)

Create a resource tracker for `jd`. Code added using a tracker, with
`add!(jit, rt, tsm_or_object)`, can later be removed from the JIT using
[`remove!`](@ref remove!(::LLVM.ResourceTracker)), without affecting other code in `jd`.

Resource trackers are reference counted: the returned reference needs to be released using
[`dispose`](@ref dispose(::LLVM.ResourceTracker)), or by using the do-block form. Releasing a
tracker does not remove the code it tracks; that code then remains in `jd` until `jd` is
cleared.

See also: [`LLVM.default_resource_tracker`](@ref).
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

function ResourceTracker(f::Core.Function, jd::JITDylib)
    rt = ResourceTracker(jd)
    try
        f(rt)
    finally
        dispose(rt)
    end
end

"""
    LLVM.default_resource_tracker(jd::JITDylib)

Get the resource tracker that tracks code added to `jd` without an explicit tracker. The
tracker is owned by `jd`, so disposing of it is not required (and does nothing).
"""
function default_resource_tracker(jd::JITDylib)
    # contrary to its documentation, LLVMOrcJITDylibGetDefaultResourceTracker does not
    # retain the tracker, so we should not release it either.
    # See https://github.com/llvm/llvm-project/issues/227221
    ResourceTracker(API.LLVMOrcJITDylibGetDefaultResourceTracker(jd), false)
end

"""
    dispose(rt::LLVM.ResourceTracker)

Release a reference to the resource tracker `rt`. This does not remove the tracked code.
"""
function dispose(rt::ResourceTracker)
    rt.owned || return
    mark_dispose(API.LLVMOrcReleaseResourceTracker, rt)
end

"""
    remove!(rt::LLVM.ResourceTracker)

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
    LLVM.transfer!(dst::LLVM.ResourceTracker, src::LLVM.ResourceTracker)

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
    err = API.LLVMOrcLLJITAddLLVMIRModuleWithRT(lljit, rt, mod)
    mark_dispose(mod)   # consumed, even on failure
    @check err
    return
end


## lookup

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
    es = ExecutionSession(lljit)
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

@checked struct IRTransformLayer
    ref::API.LLVMOrcIRTransformLayerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcIRTransformLayerRef}, il::IRTransformLayer) = il.ref

function IRTransformLayer(lljit::LLJIT)
    ref = API.LLVMOrcLLJITGetIRTransformLayer(lljit)
    IRTransformLayer(ref)
end

function set_transform!(il::IRTransformLayer)
    API.LLVMOrcIRTransformLayerSetTransform(il)
end


@checked struct MaterializationResponsibility
    ref::API.LLVMOrcMaterializationResponsibilityRef
end
Base.unsafe_convert(::Type{API.LLVMOrcMaterializationResponsibilityRef}, mr::MaterializationResponsibility) = mr.ref

function emit(il::IRTransformLayer, mr::MaterializationResponsibility, tsm::ThreadSafeModule)
    mark_dispose(tsm)
    API.LLVMOrcIRTransformLayerEmit(il, mr, tsm)
end


"""
    LLVM.requested_symbols(mr::LLVM.MaterializationResponsibility)

Get the names of the symbols that were requested from the materialization unit that `mr`
is responsible for. These names are borrowed: retain them before handing them to APIs that
take ownership, or using them after the responsibility has been fulfilled.
"""
function requested_symbols(mr::MaterializationResponsibility)
    N = Ref{Csize_t}()
    ptr = API.LLVMOrcMaterializationResponsibilityGetRequestedSymbols(mr, N)
    syms = map(LLVMSymbol, Base.unsafe_wrap(Array, ptr, N[], own=false))
    API.LLVMOrcDisposeSymbols(ptr)
    return syms
end

abstract type AbstractMaterializationUnit end

"""
    define(jd::JITDylib, mu)

Add the materialization unit `mu` to `jd`. The unit is consumed, even if this throws: on
failure (e.g., because one of its symbols is already defined in `jd`) it is disposed of
before the error is rethrown as an [`LLVMException`](@ref).
"""
function define(jd::JITDylib, mu::AbstractMaterializationUnit)
    err = API.LLVMOrcJITDylibDefine(jd, mu)
    if err != C_NULL
        # on failure, ownership of the materialization unit stays with us
        API.LLVMOrcDisposeMaterializationUnit(mu)
        throw(convert(LLVMException, LLVMError(err)))
    end
    return
end

@checked struct MaterializationUnit <: AbstractMaterializationUnit
    ref::API.LLVMOrcMaterializationUnitRef
end
Base.unsafe_convert(::Type{API.LLVMOrcMaterializationUnitRef}, mu::MaterializationUnit) = mu.ref


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

# LLVM only holds a raw pointer to custom materialization units, so root them until LLVM
# either materializes or destroys them.
const CUSTOM_MU_ROOTS = Base.IdSet{CustomMaterializationUnit}()
const CUSTOM_MU_LOCK = ReentrantLock()

function check_callback_error(mu::CustomMaterializationUnit)
    mu.exception === nothing && return nothing
    err, bt = mu.exception
    mu.exception = nothing
    throw(CallbackException("ORC materialization unit", err, bt))
end

function __materialize(ctx::Ptr{Cvoid}, mr::API.LLVMOrcMaterializationResponsibilityRef)
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    try
        mu.materialize(MaterializationResponsibility(mr))
    catch err
        _capture_callback_exception!(mu, err)
        API.LLVMOrcMaterializationResponsibilityFailMaterialization(mr)
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
    LLVM.CustomMaterializationUnit(name, symbols, materialize, discard, [init])

Create a materialization unit that promises to define `symbols`, a collection of
`name => flags` pairs mapping each [`LLVM.LLVMSymbol`](@ref) to flags created by
[`LLVM.symbol_flags`](@ref). Add it to a JITDylib with [`LLVM.define`](@ref).

When any of these symbols is looked up, `materialize(mr)` is called with a
`LLVM.MaterializationResponsibility` for the symbols, which it should fulfill, e.g., by
generating IR and emitting it with `LLVM.emit(layer, mr, tsm)`; use
[`LLVM.requested_symbols`](@ref) to see which symbols were requested. If a symbol is
overridden by another definition before it was materialized, `discard(jd, name)` is called
instead.

If `materialize` throws, materialization of the symbols fails, and lookups report an LLVM
error. Retrieve the original exception by calling [`LLVM.check_callback_error`](@ref) on the
unit. An exception in `discard` is only reported that way.

The unit takes ownership of the symbol names. `init` can be used to specify an
initializer symbol, which takes ownership of an additional reference.
"""
function CustomMaterializationUnit(name, symbols::Union{AbstractVector{<:Pair},AbstractDict},
                                   materialize, discard, init=C_NULL)
    # validate everything before taking ownership of the names
    syms = LLVMSymbol[first(pair) for pair in symbols]
    allunique(syms) || throw(ArgumentError("duplicate symbol names"))
    symbols = [API.LLVMOrcCSymbolFlagsMapPair(sym, flags) for (sym, flags) in symbols]
    CustomMaterializationUnit(name, symbols, materialize, discard, init)
end

# raw form, taking a collection of `API.LLVMOrcCSymbolFlagsMapPair`s
function CustomMaterializationUnit(name, symbols, materialize, discard, init=C_NULL)
    this = CustomMaterializationUnit(materialize, discard)
    @lock CUSTOM_MU_LOCK push!(CUSTOM_MU_ROOTS, this)

    ref = API.LLVMOrcCreateCustomMaterializationUnit(
        name,
        Base.pointer_from_objref(this), # escaping this, rooted in CUSTOM_MU_ROOTS
        symbols,
        length(symbols),
        init,
        @cfunction(__materialize, Cvoid, (Ptr{Cvoid}, API.LLVMOrcMaterializationResponsibilityRef)),
        @cfunction(__discard, Cvoid, (Ptr{Cvoid}, API.LLVMOrcJITDylibRef, API.LLVMOrcSymbolStringPoolEntryRef) ),
        @cfunction(__destroy, Cvoid, (Ptr{Cvoid},))
    )
    this.mu = MaterializationUnit(ref)
    return this
end

"""
    LLVM.absolute_symbols(name => address, ...)
    LLVM.absolute_symbols(name => (address, flags), ...)
    LLVM.absolute_symbols(pairs)

Create a materialization unit that defines each symbol `name` (a [`LLVM.LLVMSymbol`](@ref))
at a fixed `address` (a pointer, integer, or [`OrcTargetAddress`](@ref)), e.g., to make
host functions or data available to JIT-compiled code. Symbols default to being exported;
pass `flags` created by [`LLVM.symbol_flags`](@ref) to change that. The pairs can also be
passed as a collection, e.g., a vector or a dictionary.

The unit takes ownership of the symbol names, and should be added to a JITDylib using
[`LLVM.define`](@ref):

```julia
LLVM.define(jd, LLVM.absolute_symbols(mangle(lljit, "counter") => pointer(counter)))
```
"""
absolute_symbols(pair::Pair{LLVMSymbol}, pairs::Pair{LLVMSymbol}...) =
    absolute_symbols([pair, pairs...])

function absolute_symbols(pairs::Union{AbstractVector{<:Pair},AbstractDict})
    # validate everything before taking ownership of the names
    syms = LLVMSymbol[first(pair) for pair in pairs]
    allunique(syms) || throw(ArgumentError("duplicate symbol names"))
    symbols = map(collect(pairs)) do (sym, def)
        address, flags = def isa Tuple ? def : (def, symbol_flags())
        API.LLVMOrcCSymbolMapPair(sym, API.LLVMJITEvaluatedSymbol(_target_address(address),
                                                                 flags))
    end
    absolute_symbols(symbols)
end

_target_address(ptr::Ptr) = API.LLVMOrcJITTargetAddress(reinterpret(UInt, ptr))
_target_address(addr::Integer) = API.LLVMOrcJITTargetAddress(addr)
_target_address(addr::OrcTargetAddress) = addr.ptr

# raw form, taking a collection of `API.LLVMOrcCSymbolMapPair`s
function absolute_symbols(symbols)
    ref = API.LLVMOrcAbsoluteSymbols(symbols, length(symbols))
    MaterializationUnit(ref)
end

@checked struct IndirectStubsManager
    ref::API.LLVMOrcIndirectStubsManagerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcIndirectStubsManagerRef}, ism::IndirectStubsManager) = ism.ref

function LocalIndirectStubsManager(triple)
    ref = API.LLVMOrcCreateLocalIndirectStubsManager(triple)
    IndirectStubsManager(ref)
end

function dispose(ism::IndirectStubsManager)
    API.LLVMOrcDisposeIndirectStubsManager(ism)
end

@checked mutable struct LazyCallThroughManager
    ref::API.LLVMOrcLazyCallThroughManagerRef
end
Base.unsafe_convert(::Type{API.LLVMOrcLazyCallThroughManagerRef}, lcm::LazyCallThroughManager) = lcm.ref

function LocalLazyCallThroughManager(triple, es)
    ref = Ref{API.LLVMOrcLazyCallThroughManagerRef}()
    @check API.LLVMOrcCreateLocalLazyCallThroughManager(triple, es, C_NULL, ref)
    LazyCallThroughManager(ref[])
end

function dispose(lcm::LazyCallThroughManager)
    API.LLVMOrcDisposeLazyCallThroughManager(lcm)
end

"""
    LLVM.lazy_reexports(lctm, ism, source_jd, aliases)

Create a materialization unit that defines lazy reexports of symbols in `source_jd`.
`aliases` is a collection of `alias => target` or `alias => (target, flags)` pairs of
[`LLVM.LLVMSymbol`](@ref)s, with `flags` defaulting to an exported and callable symbol.

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
    syms = LLVMSymbol[first(pair) for pair in aliases]
    allunique(syms) || throw(ArgumentError("duplicate alias names"))
    aliases = map(collect(aliases)) do (alias, def)
        target, flags = def isa Tuple ? def : (def, symbol_flags(callable=true))
        API.LLVMOrcCSymbolAliasMapPair(alias,
            API.LLVMOrcCSymbolAliasMapEntry(target, flags))
    end
    lazy_reexports(lctm, ism, jd, aliases)
end

# raw form, taking a collection of `API.LLVMOrcCSymbolAliasMapPair`s
function lazy_reexports(lctm::LazyCallThroughManager, ism::IndirectStubsManager,
                        jd::JITDylib, aliases)
    ref = API.LLVMOrcLazyReexports(lctm, ism, jd, aliases, length(aliases))
    MaterializationUnit(ref)
end


# JuliaOJIT

export JuliaOJIT

function ExecutionSession(jljit::JuliaOJIT)
    es = API.JLJITGetLLVMOrcExecutionSession(jljit)
    ExecutionSession(es)
end

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
       !contains(inline_asm(mod), "__UnwindData")
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
            inline_asm!(mod, """
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
            inline_asm!(mod, """
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
    # Julia's debug info expects certain symbols to be present
    tsm() do mod
        decorate_module(mod)
    end
    err = API.JLJITAddLLVMIRModule(jljit, jd, tsm)
    mark_dispose(tsm)   # consumed, even on failure
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

@checked struct IRCompileLayer
    ref::API.LLVMOrcIRCompileLayerRef
    jit
end

Base.unsafe_convert(::Type{API.LLVMOrcIRCompileLayerRef}, il::IRCompileLayer) = il.ref

function emit(il::IRCompileLayer, mr::MaterializationResponsibility, tsm::ThreadSafeModule)
    if il.jit isa JuliaOJIT
        # Julia's debug info expects certain symbols to be present
        tsm() do mod
            decorate_module(mod)
        end
    end
    mark_dispose(tsm)
    API.LLVMOrcIRCompileLayerEmit(il, mr, tsm)
end

function IRCompileLayer(jljit::JuliaOJIT)
    ref = API.JLJITGetIRCompileLayer(jljit)
    IRCompileLayer(ref, jljit)
end
