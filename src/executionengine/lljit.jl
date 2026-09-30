"""
    LLJITBuilder()

Create a builder to customize the construction of an [`LLJIT`](@ref), e.g., using
[`target_machine_builder!`](@ref) or [`linking_layer_creator!`](@ref). The builder is consumed
when constructing the JIT; otherwise, it needs to be disposed of using `dispose`.
"""
@checked struct LLJITBuilder
    ref::API.LLVMOrcLLJITBuilderRef
    roots::Vector{Any}
end
Base.unsafe_convert(::Type{API.LLVMOrcLLJITBuilderRef}, builder::LLJITBuilder) = mark_use(builder).ref

"""
    LLJIT

LLVM's standard ORC-based JIT, which compiles and links code on demand, i.e., when it is
looked up. It needs to be disposed of using `dispose`, or by using the do-block form of its
constructors.

# Properties

    jit.triple

The target triple that the JIT compiles code for. Modules added to the JIT should use it.

    jit.datalayout

The data layout that the JIT compiles code for, as a string. Modules added to the JIT
should use it.

    jit.global_prefix

The character that the JIT's target prepends to global symbols when mangling them (e.g.,
`'_'` on macOS), or `'\\0'` if there is none, as a `Cchar`.

    jit.execution_session

The [`ExecutionSession`](@ref) of a JIT, which manages its JITDylibs and symbol string pool.

    lljit.main_dylib

The main [`JITDylib`](@ref) of the JIT, which `lookup(lljit, name)` searches.

    lljit.ir_transform_layer

The [`IRTransformLayer`](@ref) of the JIT, which transforms IR modules before they are
compiled. Modules added with `add!` pass through this layer, as can modules emitted by a
materialization unit with [`emit`](@ref). By default, it does not change modules; use
[`transform!`](@ref) to install a transformation.
"""
@checked mutable struct LLJIT
    ref::API.LLVMOrcLLJITRef
    roots::Vector{Any}  # Julia objects that LLVM holds on to, e.g., for callbacks
end
LLJIT(ref::API.LLVMOrcLLJITRef) = LLJIT(ref, Any[])
@properties LLJIT

Base.unsafe_convert(::Type{API.LLVMOrcLLJITRef}, lljit::LLJIT) = mark_use(lljit).ref

function LLJITBuilder()
    ref = API.LLVMOrcCreateLLJITBuilder()
    mark_alloc(LLJITBuilder(ref, []))
end

function dispose(builder::LLJITBuilder)
    mark_dispose(API.LLVMOrcDisposeLLJITBuilder, builder)
end

"""
    target_machine_builder!(builder::LLJITBuilder, tmb::TargetMachineBuilder)

Use `tmb` to create the JIT's target machines, taking ownership of it.
"""
function target_machine_builder!(builder::LLJITBuilder, tmb::TargetMachineBuilder)
    API.LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(builder, tmb)
end

"""
    linking_layer_creator!(builder::LLJITBuilder, callback, ctx)

Install a raw LLVM object-layer-creator callback and context pointer.

!!! warning

    The callback must not throw a Julia exception. This low-level overload has
    no exception barrier, and LLVM's callback cannot report an error. Use the
    two-argument overload for a Julia callable.
"""
function linking_layer_creator!(builder::LLJITBuilder, callback, ctx)
    API.LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(builder, callback, ctx)
end

"""
    LLJIT(builder::LLJITBuilder)

Create an LLJIT as configured by `builder`, taking ownership of the builder.
"""
function LLJIT(builder::LLJITBuilder)
    ref = Ref{API.LLVMOrcLLJITRef}()
    err = API.LLVMOrcCreateLLJIT(ref, builder)
    # LLVMOrcCreateLLJIT consumes the builder on both success and failure.
    mark_dispose(builder)
    @check err

    lljit = mark_alloc(LLJIT(ref[]))
    for root in builder.roots
        if root isa ObjectLinkingLayerCreator && root.exception !== nothing
            err, bt = root.exception
            dispose(lljit)
            throw(CallbackException("object linking layer creator", err, bt))
        end
    end
    lljit
end

function dispose(lljit::LLJIT)
    mark_dispose(lljit) do lljit
        @check API.LLVMOrcDisposeLLJIT(lljit)
    end
    empty!(lljit.roots)
    return
end

"""
    LLJIT(; tm::Union{Nothing,TargetMachine}=nothing)
    LLJIT(f; tm=nothing)

Create an LLJIT that compiles for the host, or for the target machine `tm`, taking
ownership of it. The do-block form disposes of the JIT after calling `f(lljit)`.
"""
function LLJIT(; tm::Union{Nothing, TargetMachine} = nothing)
    builder = LLJITBuilder()
    if tm === nothing
        tmb = TargetMachineBuilder()
    else
        tmb = TargetMachineBuilder(tm)
    end
    target_machine_builder!(builder, tmb)
    LLJIT(builder)
end

function LLJIT(f::Core.Function, args...; kwargs...)
    lljit = LLJIT(args...; kwargs...)
    try
        f(lljit)
    finally
        dispose(lljit)
    end
end

function triple(lljit::LLJIT)
    cstr = API.LLVMOrcLLJITGetTripleString(lljit)
    Base.unsafe_string(cstr)
end

function datalayout(lljit::LLJIT)
    Base.unsafe_string(API.LLVMOrcLLJITGetDataLayoutStr(lljit))
end

@property LLJIT triple
@property LLJIT datalayout

function global_prefix(lljit::LLJIT)
    return API.LLVMOrcLLJITGetGlobalPrefix(lljit)
end

@property LLJIT global_prefix


# JuliaOJIT interface

"""
    JuliaOJIT()
    JuliaOJIT(f)

Get a handle to Julia's own JIT, e.g., to add code to it that can be called from Julia
code. The JIT is not owned by LLVM.jl, so disposing of the handle is a no-op.

# Properties

    jljit.ir_compile_layer

The [`IRCompileLayer`](@ref) of Julia's JIT, which compiles IR modules.
"""
@checked mutable struct JuliaOJIT
    ref::API.JuliaOJITRef
end
@properties JuliaOJIT

Base.unsafe_convert(::Type{API.JuliaOJITRef}, jljit::JuliaOJIT) = jljit.ref

function JuliaOJIT()
    JuliaOJIT(API.JLJITGetJuliaOJIT())
end

function triple(jljit::JuliaOJIT)
    cstr = API.JLJITGetTripleString(jljit)
    Base.unsafe_string(cstr)
end

function datalayout(jljit::JuliaOJIT)
    Base.unsafe_string(API.JLJITGetDataLayoutString(jljit))
end

@property JuliaOJIT triple
@property JuliaOJIT datalayout

function global_prefix(jljit::JuliaOJIT)
    return API.JLJITGetGlobalPrefix(jljit)
end

@property JuliaOJIT global_prefix

function dispose(jljit::JuliaOJIT)
    # don't dispose of the Julia JIT
    return nothing
end

function JuliaOJIT(f::Core.Function)
    jljit = JuliaOJIT()
    try
        f(jljit)
    finally
        dispose(jljit)
    end
end
