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

function ObjectLinkingLayer(es::ExecutionSession)
    ref = API.LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es)
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
end

function ollc_callback(ctx::Ptr{Cvoid}, es::API.LLVMOrcExecutionSessionRef, triple::Ptr{Cchar})
    es = ExecutionSession(es)
    triple = Base.unsafe_string(triple)

    ollc = Base.unsafe_pointer_to_objref(ctx)::ObjectLinkingLayerCreator
    oll = ollc.cb(es, triple)::ObjectLinkingLayer
    return oll.ref
end

"""
    linkinglayercreator!(builder::LLJITBuilder, creator::ObjectLinkingLayerCreator)

!!! warning
    The creator object needs to be rooted by the caller for the lifetime of the
    builder argument.
"""
function linkinglayercreator!(builder::LLJITBuilder, creator::ObjectLinkingLayerCreator)
    cb = @cfunction(ollc_callback,
                    API.LLVMOrcObjectLayerRef,
                    (Ptr{Cvoid}, API.LLVMOrcExecutionSessionRef, Ptr{Cchar}))
    linkinglayercreator!(builder, cb, Base.pointer_from_objref(creator))
end

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
@checked struct DefinitionGenerator
    ref::API.LLVMOrcDefinitionGeneratorRef
end
Base.unsafe_convert(::Type{API.LLVMOrcDefinitionGeneratorRef}, dg::DefinitionGenerator) = dg.ref

function add!(jd::JITDylib, dg::DefinitionGenerator)
    API.LLVMOrcJITDylibAddGenerator(jd, dg)
end

function CreateDynamicLibrarySearchGeneratorForProcess(prefix)
    ref = Ref{API.LLVMOrcDefinitionGeneratorRef}()
    @check API.LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(ref, prefix, C_NULL, C_NULL)
    DefinitionGenerator(ref[])
end

# LLVMOrcCreateCustomCAPIDefinitionGenerator(F, Ctx)

function lookup_dylib(es::ExecutionSession, name)
    ref = API.LLVMOrcExecutionSessionGetJITDylibByName(es, name)
    if ref == C_NULL
        return
    end
    JITDylib(ref)
end

function add!(lljit::LLJIT, jd::JITDylib, obj::MemoryBuffer)
    @check API.LLVMOrcLLJITAddObjectFile(lljit, jd, obj)
    mark_dispose(obj)
    return
end

# LLVMOrcLLJITAddObjectFileWithRT(J, RT, ObjBuffer)

function add!(lljit::LLJIT, jd::JITDylib, mod::ThreadSafeModule)
    @check API.LLVMOrcLLJITAddLLVMIRModule(lljit, jd, mod)
    mark_dispose(mod)
    return
end

# LLVMOrcLLJITAddLLVMIRModuleWithRT(J, JD, TSM)

function Base.empty!(jd::JITDylib)
    API.LLVMOrcJITDylibClear(jd)
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

function define(jd::JITDylib, mu::AbstractMaterializationUnit)
    API.LLVMOrcJITDylibDefine(jd, mu)
end

@checked struct MaterializationUnit <: AbstractMaterializationUnit
    ref::API.LLVMOrcMaterializationUnitRef
end
Base.unsafe_convert(::Type{API.LLVMOrcMaterializationUnitRef}, mu::MaterializationUnit) = mu.ref


mutable struct CustomMaterializationUnit <: AbstractMaterializationUnit
    materialize
    discard
    mu::MaterializationUnit
    function CustomMaterializationUnit(materialize, discard)
        new(materialize, discard)
    end
end
Base.cconvert(::Type{API.LLVMOrcMaterializationUnitRef}, mu::CustomMaterializationUnit) = mu.mu

const CUSTOM_MU_ROOTS = Base.IdSet{CustomMaterializationUnit}()

function __materialize(ctx::Ptr{Cvoid}, mr::API.LLVMOrcMaterializationResponsibilityRef)
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    try
        mu.materialize(MaterializationResponsibility(mr))
    catch err
        bt = catch_backtrace()
        showerror(stderr, err, bt)
        API.LLVMOrcMaterializationResponsibilityFailMaterialization(mr)
    end
    nothing
end

function __discard(ctx::Ptr{Cvoid}, jd::API.LLVMOrcJITDylibRef, symbol::API.LLVMOrcSymbolStringPoolEntryRef)
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    try
        mu.discard(JITDylib(jd), LLVMSymbol(symbol))
    catch err
        bt = catch_backtrace()
        showerror(stderr, err, bt)
    end
    nothing
end

function __destroy(ctx::Ptr{Cvoid})
    mu = Base.unsafe_pointer_to_objref(ctx)::CustomMaterializationUnit
    delete!(CUSTOM_MU_ROOTS, mu)
    nothing
end

function CustomMaterializationUnit(name, symbols, materialize, discard, init=C_NULL)
    this = CustomMaterializationUnit(materialize, discard)
    push!(CUSTOM_MU_ROOTS, this)

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
    JITDylib(jljit::JuliaOJIT)

Get the external JITDylib from the Julia JIT
"""
function JITDylib(jljit::JuliaOJIT)
    ref = API.JLJITGetExternalJITDylib(jljit)
    JITDylib(ref)
end

function add!(jljit::JuliaOJIT, jd::JITDylib, obj::MemoryBuffer)
    @check API.JLJITAddObjectFile(jljit, jd, obj)
    mark_dispose(obj)
    return
end

function decorate_module(mod)
    # TODO: check the triple, not the system
    if Sys.iswindows() && Sys.ARCH == :x86_64 &&
       !contains(inline_asm(mod), "__UnwindData")
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

function add!(jljit::JuliaOJIT, jd::JITDylib, tsm::ThreadSafeModule)
    # Julia's debug info expects certain symbols to be present
    tsm() do mod
        decorate_module(mod)
    end
    @check API.JLJITAddLLVMIRModule(jljit, jd, tsm)
    mark_dispose(tsm)
    return
end

function lookup(jljit::JuliaOJIT, name, external_jd=true)
    result = Ref{API.LLVMOrcJITTargetAddress}()
    @check API.JLJITLookup(jljit, result, name, external_jd)
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
