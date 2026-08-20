@checked struct LLJITBuilder
    ref::API.LLVMOrcLLJITBuilderRef
    roots::Vector{Any}
end
Base.unsafe_convert(::Type{API.LLVMOrcLLJITBuilderRef}, builder::LLJITBuilder) = mark_use(builder).ref

@checked mutable struct LLJIT
    ref::API.LLVMOrcLLJITRef
end
Base.unsafe_convert(::Type{API.LLVMOrcLLJITRef}, lljit::LLJIT) = mark_use(lljit).ref

function LLJITBuilder()
    ref = API.LLVMOrcCreateLLJITBuilder()
    mark_alloc(LLJITBuilder(ref, []))
end

function dispose(builder::LLJITBuilder)
    mark_dispose(API.LLVMOrcDisposeLLJITBuilder, builder)
end

function targetmachinebuilder!(builder::LLJITBuilder, tmb::TargetMachineBuilder)
    API.LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(builder, tmb)
end

"""
    linkinglayercreator!(builder::LLJITBuilder, callback, ctx)

Install a raw LLVM object-layer-creator callback and context pointer.

!!! warning

    The callback must not throw a Julia exception. This low-level overload has
    no exception barrier, and LLVM's callback cannot report an error. Use the
    `ObjectLinkingLayerCreator` overload for a Julia callable.
"""
function linkinglayercreator!(builder::LLJITBuilder, callback, ctx)
    API.LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(builder, callback, ctx)
end

"""
    LLJIT(::LLJITBuilder)

Creates a LLJIT stack based on the provided builder.

!!! note
    Takes ownership of the provided builder.
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
    mark_dispose(API.LLVMOrcDisposeLLJIT, lljit)
end

"""
    LLJIT(;tm::Union{Nothing, TargetMachine})

Use the provided TargetMachine and construct an LLJIT from it.
"""
function LLJIT(; tm::Union{Nothing, TargetMachine} = nothing)
    builder = LLJITBuilder()
    if tm === nothing
        tmb = TargetMachineBuilder()
    else
        tmb = TargetMachineBuilder(tm)
    end
    targetmachinebuilder!(builder, tmb)
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

function get_prefix(lljit::LLJIT)
    return API.LLVMOrcLLJITGetGlobalPrefix(lljit)
end


# JuliaOJIT interface

@checked mutable struct JuliaOJIT
    ref::API.JuliaOJITRef
end

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

function get_prefix(jljit::JuliaOJIT)
    return API.JLJITGetGlobalPrefix(jljit)
end

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
