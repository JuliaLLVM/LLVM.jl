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

@checked struct LLVMSymbol
    ref::API.LLVMOrcSymbolStringPoolEntryRef
end
Base.unsafe_convert(::Type{API.LLVMOrcSymbolStringPoolEntryRef}, sym::LLVMSymbol) = sym.ref
Base.convert(::Type{API.LLVMOrcSymbolStringPoolEntryRef}, sym::LLVMSymbol) = sym.ref

function Base.cconvert(::Type{Cstring}, sym::LLVMSymbol)
    return API.LLVMOrcSymbolStringPoolEntryStr(sym)
end

function Base.string(sym::LLVMSymbol)
    cstr = API.LLVMOrcSymbolStringPoolEntryStr(sym)
    Base.unsafe_string(cstr)
end

function intern(es::ExecutionSession, string)
    entry = API.LLVMOrcExecutionSessionIntern(es, string)
    LLVMSymbol(entry)
end

function release(sym::LLVMSymbol)
    API.LLVMOrcReleaseSymbolStringPoolEntry(sym)
end

function retain(sym::LLVMSymbol)
    API.LLVMOrcRetainSymbolStringPoolEntry(sym)
end

# ORC always uses linker-mangled symbols internally (including for lookups, responsibility object maps, etc).
# IR uses non-linker-mangled names.
# If you're synthesizing IR from a requested-symbols map you'll need to demangle the name.
# Unfortunately we don't have a generic utility for that yet, but on MacOS it just means
# dropping the leading '_' if there is one, or prepending a \01 prefix (see https://llvm.org/docs/LangRef.html#identifiers)

function mangle(lljit::LLJIT, name)
    entry = API.LLVMOrcLLJITMangleAndIntern(lljit, name)
    return LLVMSymbol(entry)
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
    process_search_generator(get_prefix(jit))

DynamicLibrarySearchGenerator(jit::Union{LLJIT,JuliaOJIT}, path::AbstractString) =
    library_search_generator(path, get_prefix(jit))

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

# old name, used by downstream packages
CreateDynamicLibrarySearchGeneratorForProcess(prefix) = process_search_generator(prefix)

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

struct OrcTargetAddress
    ptr::API.LLVMOrcJITTargetAddress
end
Base.convert(::Type{API.LLVMOrcJITTargetAddress}, addr::OrcTargetAddress) = addr.ptr

Base.pointer(addr::OrcTargetAddress) = reinterpret(Ptr{Cvoid}, addr.ptr % UInt) # LLVMOrcTargetAddress is UInt64 even on 32-bit

OrcTargetAddress(ptr::Ptr{Cvoid}) = OrcTargetAddress(reinterpret(UInt, ptr))

"""
    lookup(lljit::LLJIT, name)

Takes an unmangled symbol names and searches for it in the LLJIT.
"""
function lookup(lljit::LLJIT, name)
    result = Ref{API.LLVMOrcJITTargetAddress}()
    @check API.LLVMOrcLLJITLookup(lljit, result, name)
    OrcTargetAddress(result[])
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


function get_requested_symbols(mr::MaterializationResponsibility)
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

function reexports(lctm::LazyCallThroughManager, ism::IndirectStubsManager, jd::JITDylib, symbols)
    ref = API.LLVMOrcLazyReexports(lctm, ism, jd, symbols, length(symbols))
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
