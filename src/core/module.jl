# Modules represent the top-level structure in an LLVM program.

export dispose, context,
       name, name!,
       triple, triple!,
       datalayout, datalayout!,
       inline_asm, inline_asm!,
       set_used!, set_compiler_used!

"""
    LLVM.Module

Modules are the top level container of all other LLVM IR objects. Each module directly
contains a list of globals variables, a list of functions, a list of libraries (or other
modules) this module depends on, a symbol table, and various data about the target's
characteristics.
"""
Module
# forward definition of Module in src/core/value/constant.jl

Base.unsafe_convert(::Type{API.LLVMModuleRef}, mod::Module) = mark_use(mod).ref

Base.:(==)(x::Module, y::Module) = (x.ref === y.ref)

# forward declarations
@checked struct DataLayout
    ref::API.LLVMTargetDataRef
end
@checked struct Function <: GlobalObject
    ref::API.LLVMValueRef
end

"""
    LLVM.Module(name::String)

Create a new module with the given name.

This object needs to be disposed of using [`dispose`](@ref).
"""
Module(name::String) =
    mark_alloc(Module(API.LLVMModuleCreateWithNameInContext(name, context())))

"""
    copy(mod::LLVM.Module)

Clone the given module.

This object needs to be disposed of using [`dispose`](@ref).
"""
Base.copy(mod::Module) = mark_alloc(Module(API.LLVMCloneModule(mod)))

"""
    dispose(mod::LLVM.Module)

Dispose of the given module, releasing all resources associated with it. The module should
not be used after this operation.
"""
dispose(mod::Module) = mark_dispose(API.LLVMDisposeModule, mod)

function Module(f::Core.Function, args...; kwargs...)
    mod = Module(args...; kwargs...)
    try
        f(mod)
    finally
        dispose(mod)
    end
end

function Base.show(io::IO, mod::Module)
    print(io, "LLVM.Module(\"", name(mod), "\")")
end

function Base.show(io::IO, ::MIME"text/plain", mod::Module)
    output = strip(string(mod))
    print(io, output)
end

"""
    name(mod::LLVM.Module)

Get the name of the given module.
"""
function name(mod::Module)
    out_len = Ref{Csize_t}()
    ptr = convert(Ptr{UInt8}, API.LLVMGetModuleIdentifier(mod, out_len))
    return unsafe_string(ptr, out_len[])
end

"""
    name!(mod::LLVM.Module, name::String)

Set the name of the given module.
"""
name!(mod::Module, str::String) =
    API.LLVMSetModuleIdentifier(mod, str, Csize_t(length(str)))

"""
    triple(mod::LLVM.Module)

Get the target triple of the given module.
"""
triple(mod::Module) = unsafe_string(API.LLVMGetTarget(mod))

"""
    triple!(mod::LLVM.Module, triple::String)

Set the target triple of the given module.
"""
triple!(mod::Module, triple) = API.LLVMSetTarget(mod, triple)

"""
    datalayout(mod::LLVM.Module)

Get the data layout of the given module.
"""
datalayout(mod::Module) = DataLayout(API.LLVMGetModuleDataLayout(mod))

"""
    datalayout!(mod::LLVM.Module, layout)

Set the data layout of the given module. The layout can be a string or a `DataLayout`
object.
"""
datalayout!(mod::Module, layout)
datalayout!(mod::Module, layout::String) = API.LLVMSetDataLayout(mod, layout)
datalayout!(mod::Module, layout::DataLayout) =
    API.LLVMSetModuleDataLayout(mod, layout)

"""
    inline_asm!(mod::LLVM.Module, asm::String; overwrite::Bool=false)

Add module-level inline assembly to the given module. If `overwrite` is `true`, the
existing inline assembly is replaced, otherwise the new assembly is appended.
"""
function inline_asm!(mod::Module, asm::String; overwrite::Bool=false)
    if overwrite
        API.LLVMSetModuleInlineAsm2(mod, asm, length(asm))
    else
        API.LLVMAppendModuleInlineAsm(mod, asm, length(asm))
    end
end

"""
    inline_asm(mod::LLVM.Module) -> String

Get the module-level inline assembly of the given module.
"""
function inline_asm(mod::Module)
    out_len = Ref{Csize_t}()
    ptr = convert(Ptr{UInt8}, API.LLVMGetModuleInlineAsm(mod, out_len))
    return unsafe_string(ptr, out_len[])
end

"""
    context(mod::LLVM.Module)

Get the context in which the given module was created.
"""
context(mod::Module) = Context(API.LLVMGetModuleContext(mod))

"""
    set_used!(mod::LLVM.Module, values::GlobalVariable...)

Mark the given global variables as used in the given module by appending them to the
`llvm.used` metadata node.
"""
set_used!(mod::Module, values::GlobalVariable...) =
    API.LLVMAppendToUsed(mod, collect(values), length(values))

"""
    set_compiler_used!(mod::LLVM.Module, values::GlobalVariable...)

Mark the given global variables as used by the compiler in the given module by appending
them to the `llvm.compiler.used` metadata node. As opposed to [`set_used!`](@ref), this
still allows the linker to remove the variable if it is not actually used.
"""
set_compiler_used!(mod::Module, values::GlobalVariable...) =
    API.LLVMAppendToCompilerUsed(mod, collect(values), length(values))


## textual IR handling

"""
    parse(::Type{Module}, ir::String)

Parse the given LLVM IR string into a module.
"""
function Base.parse(::Type{Module}, ir::String)
    data = unsafe_wrap(Vector{UInt8}, ir)
    membuf = MemoryBuffer(data, "", false)

    out_ref = Ref{API.LLVMModuleRef}()
    out_error = Ref{Cstring}()
    status = API.LLVMParseIRInContext(context(), membuf, out_ref, out_error) |> Bool
    mark_dispose(membuf)

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    mark_alloc(Module(out_ref[]))
end

"""
    string(mod::Module)

Convert the given module to a string.
"""
Base.string(mod::Module) = unsafe_message(API.LLVMPrintModuleToString(mod))


## binary bitcode handling

"""
    parse(::Type{Module}, membuf::MemoryBuffer)

Parse bitcode from the given memory buffer into a module.
"""
function Base.parse(::Type{Module}, membuf::MemoryBuffer)
    out_ref = Ref{API.LLVMModuleRef}()

    status = API.LLVMParseBitcodeInContext2(context(), membuf, out_ref) |> Bool
    @assert !status # caught by diagnostics handler

    mark_alloc(Module(out_ref[]))
end

"""
    parse(::Type{Module}, data::Vector)

Parse bitcode from the given byte vector into a module.
"""
function Base.parse(::Type{Module}, data::Vector)
    @dispose membuf = MemoryBuffer(data, "", false) begin
        parse(Module, membuf)
    end
end

"""
    convert(::Type{MemoryBuffer}, mod::Module)

Convert the given module to a memory buffer containing its bitcode.
"""
Base.convert(::Type{MemoryBuffer}, mod::Module) =
    mark_alloc(MemoryBuffer(API.LLVMWriteBitcodeToMemoryBuffer(mod)))

"""
    convert(::Type{Vector}, mod::Module)

Convert the given module to a byte vector containing its bitcode.
"""
function Base.convert(::Type{Vector{T}}, mod::Module) where {T<:Union{UInt8,Int8}}
    buf = convert(MemoryBuffer, mod)
    vec = convert(Vector{T}, buf)
    dispose(buf)
    return vec
end

"""
    write(io::IO, mod::Module)

Write bitcode of the given module to the given IO stream.
"""
function Base.write(io::IO, mod::Module)
    # XXX: can't use the LLVM API because it returns 0, not the number of bytes written
    #API.LLVMWriteBitcodeToFD(mod, Cint(fd(io)), false, true)
    buf = convert(MemoryBuffer, mod)
    vec = unsafe_wrap(Array, pointer(buf), length(buf))
    nb = write(io, vec)
    dispose(buf)
    return nb
end


## global variable iteration

export globals, prevglobal, nextglobal

struct ModuleGlobalSet
    mod::Module
end

"""
    globals(mod::LLVM.Module)

Get an iterator over the global variables in the given module.
"""
globals(mod::Module) = ModuleGlobalSet(mod)

Base.eltype(::ModuleGlobalSet) = GlobalVariable

function Base.iterate(iter::ModuleGlobalSet, state=API.LLVMGetFirstGlobal(iter.mod))
    state == C_NULL ? nothing : (GlobalVariable(state), API.LLVMGetNextGlobal(state))
end

function Base.first(iter::ModuleGlobalSet)
    ref = API.LLVMGetFirstGlobal(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalVariable(ref)
end

function Base.last(iter::ModuleGlobalSet)
    ref = API.LLVMGetLastGlobal(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalVariable(ref)
end

Base.isempty(iter::ModuleGlobalSet) = API.LLVMGetLastGlobal(iter.mod) == C_NULL

Base.IteratorSize(::ModuleGlobalSet) = Base.SizeUnknown()

"""
    prevglobal(gv::LLVM.GlobalVariable)

Get the previous global variable in the module, or `nothing` if there is none.

See also: [`nextglobal`](@ref).
"""
function prevglobal(gv::GlobalVariable)
    ref = API.LLVMGetPreviousGlobal(gv)
    ref == C_NULL && return nothing
    GlobalVariable(ref)
end

"""
    nextglobal(gv::LLVM.GlobalVariable)

Get the next global variable in the module, or `nothing` if there is none.

See also: [`prevglobal`](@ref).
"""
function nextglobal(gv::GlobalVariable)
    ref = API.LLVMGetNextGlobal(gv)
    ref == C_NULL && return nothing
    GlobalVariable(ref)
end

# partial associative interface

function Base.haskey(iter::ModuleGlobalSet, name::String)
    return API.LLVMGetNamedGlobal(iter.mod, name) != C_NULL
end

function Base.getindex(iter::ModuleGlobalSet, name::String)
    objref = API.LLVMGetNamedGlobal(iter.mod, name)
    objref == C_NULL && throw(KeyError(name))
    return GlobalVariable(objref)
end


## function iteration

export functions, prevfun, nextfun

struct ModuleFunctionSet
    mod::Module
end

"""
    functions(mod::LLVM.Module)

Get an iterator over the functions in the given module.
"""
functions(mod::Module) = ModuleFunctionSet(mod)

Base.eltype(::ModuleFunctionSet) = Function

function Base.iterate(iter::ModuleFunctionSet, state=API.LLVMGetFirstFunction(iter.mod))
    state == C_NULL ? nothing : (Function(state), API.LLVMGetNextFunction(state))
end

function Base.first(iter::ModuleFunctionSet)
    ref = API.LLVMGetFirstFunction(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    Function(ref)
end

function Base.last(iter::ModuleFunctionSet)
    ref = API.LLVMGetFirstFunction(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    Function(ref)
end

Base.isempty(iter::ModuleFunctionSet) = API.LLVMGetLastFunction(iter.mod) == C_NULL

Base.IteratorSize(::ModuleFunctionSet) = Base.SizeUnknown()

"""
    prevfun(fun::LLVM.Function)

Get the previous function in the module, or `nothing` if there is none.
"""
function prevfun(fun::Function)
    ref = API.LLVMGetPreviousFunction(fun)
    ref == C_NULL && return nothing
    Function(ref)
end

"""
    nextfun(fun::LLVM.Function)

Get the next function in the module, or `nothing` if there is none.
"""
function nextfun(fun::Function)
    ref = API.LLVMGetNextFunction(fun)
    ref == C_NULL && return nothing
    Function(ref)
end

# partial associative interface

function Base.haskey(iter::ModuleFunctionSet, name::String)
    return API.LLVMGetNamedFunction(iter.mod, name) != C_NULL
end

function Base.getindex(iter::ModuleFunctionSet, name::String)
    objref = API.LLVMGetNamedFunction(iter.mod, name)
    objref == C_NULL && throw(KeyError(name))
    return Function(objref)
end


## module flag iteration
# TODO: doesn't actually iterate, since we can't list the available keys

export flags

struct ModuleFlagDict <: AbstractDict{String,Metadata}
    mod::Module
end

"""
    flags(mod::LLVM.Module)

Get a dictionary-like object representing the module flags of the given module.

This object can be used to get and set module flags, by calling `getindex` and `setindex!`.
"""
flags(mod::Module) = ModuleFlagDict(mod)

Base.haskey(iter::ModuleFlagDict, name::String) =
    API.LLVMGetModuleFlag(iter.mod, name, length(name)) != C_NULL

function Base.getindex(iter::ModuleFlagDict, name::String)
    objref = API.LLVMGetModuleFlag(iter.mod, name, length(name))
    objref == C_NULL && throw(KeyError(name))
    return Metadata(objref)
end

function Base.setindex!(iter::ModuleFlagDict, val::Metadata,
                        (name, behavior)::Tuple{String, API.LLVMModuleFlagBehavior})
    API.LLVMAddModuleFlag(iter.mod, behavior, name, length(name), val)
end


## sdk version

export sdk_version, sdk_version!

"""
    sdk_version(mod::LLVM.Module)

Get the SDK version of the given module, if it has been set.
"""
function sdk_version!(mod::Module, version::VersionNumber)
    entries = Int32[version.major]
    if version.minor != 0 || version.patch != 0
        push!(entries, version.minor)
        if version.patch != 0
            push!(entries, version.patch)
        end
        # cannot represent prerelease or build metadata
    end
    md = context!(context(mod)) do
        Metadata(ConstantDataArray(entries))
    end

    flags(mod)["SDK Version", LLVM.API.LLVMModuleFlagBehaviorWarning] = md
end

"""
    sdk_version!(mod::LLVM.Module, version::VersionNumber)

Set the SDK version of the given module.
"""
function sdk_version(mod::Module)
    haskey(flags(mod), "SDK Version") || return nothing
    md = flags(mod)["SDK Version"]
    c = context!(context(mod)) do
        Value(md)
    end
    entries = collect(c)
    VersionNumber(map(val->convert(Int, val), entries)...)
end
