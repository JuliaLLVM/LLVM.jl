# Modules represent the top-level structure in an LLVM program.

@vocabulary IR dispose, context

"""
    LLVM.Module

Modules are the top level container of all other LLVM IR objects. Each module directly
contains a list of globals variables, a list of functions, a list of libraries (or other
modules) this module depends on, a symbol table, and various data about the target's
characteristics.

# Properties

    mod.metadata

The named metadata of the module, as a dictionary-like view that maps names to
[`NamedMDNode`](@ref)s. Indexing the view with a name that isn't present creates an empty
named metadata node, so use `haskey` to check whether one exists. To add metadata, append
to the operands of the named metadata node: `push!(mod.metadata[name].operands, node)`.

    mod.name
    mod.name = name::String

The name (module identifier) of the module.

    mod.triple
    mod.triple = triple::String

The target triple of the module, or an empty string if it has none.

    mod.datalayout
    mod.datalayout = layout::Union{String,DataLayout}

The data layout of the module. Either a string or a `DataLayout` object can be assigned.

    mod.inline_asm

The module-level inline assembly of the module, as a view of the fragments of assembly that
the code generator emits as-is. The view supports:

- `push!(mod.inline_asm, asm::AbstractString)`: append a fragment of assembly, terminating
  it with a newline if it doesn't end with one;
- `empty!(mod.inline_asm)`: remove all inline assembly;
- `isempty(mod.inline_asm)`: check whether the module has inline assembly;
- `String(mod.inline_asm)` or `string(mod.inline_asm)`: get the assembly text.

To replace the inline assembly of a module, empty it before adding new fragments.
Iterating the individual fragments is not supported.

    mod.context

The context in which the module was created.

    mod.used
    mod.compiler_used

The global values that are marked as used in the module, as a view of the `llvm.used`
(`mod.used`) or `llvm.compiler.used` (`mod.compiler_used`) global variable. The compiler
and the linker keep the values in `mod.used`, even if they appear to be unused, while
values in `mod.compiler_used` are only kept by the compiler. The view is a set of global
values, which supports:

- `push!(set, gv)` and `union!(set, gvs)`: mark global values as used;
- `delete!(set, gv)` and `setdiff!(set, gvs)`: stop marking global values as used;
- `empty!(set)`: remove the `llvm.used` or `llvm.compiler.used` variable;
- iterating the marked values, `length`, `isempty` and `in`.

    mod.globals

The global variables of the module, as a view that can be iterated, and indexed by name
(`mod.globals["name"]`, `haskey`, `get`). The global variables can be reordered using
[`sort!`](@ref sort!(::LLVM.ModuleGlobalSet)). Create a `GlobalVariable` to add one, or use
[`get!`](@ref get!(::Base.Callable, ::LLVM.ModuleGlobalSet, ::String)) to only create it if
it doesn't exist yet.

    mod.functions

The functions of the module, as a view that can be iterated, and indexed by name
(`mod.functions["name"]`, `haskey`, `get`). The functions can be reordered using
[`sort!`](@ref sort!(::LLVM.ModuleFunctionSet)). Create an `LLVM.Function` to add one, or
use [`get!`](@ref get!(::Base.Callable, ::LLVM.ModuleFunctionSet, ::String)) to only
declare it if it doesn't exist yet, e.g., to call a runtime function.

    mod.aliases

The global aliases of the module, as a view that can be iterated, and indexed by name
(`mod.aliases["name"]`, `haskey`, `get`). Create a `GlobalAlias` to add one.

    mod.ifuncs

The ifuncs of the module, as a view that can be iterated, and indexed by name
(`mod.ifuncs["name"]`, `haskey`, `get`). Create a `GlobalIFunc` to add one.

    mod.flags

The module flags of the module, as a dictionary-like view mapping the name of each flag to
its value. Flags can be looked up by name, and added using
`mod.flags[name, behavior] = md`, where `behavior` is an `LLVM.ModuleFlagBehavior.T`
that determines how the flag is merged when linking modules. Module flags cannot be
removed.

    mod.sdk_version
    mod.sdk_version = version::VersionNumber

The Apple SDK version of the module, or `nothing` if it hasn't been set. The version is
stored in the `SDK Version` module flag, dropping any prerelease or build metadata.

    mod.debug_metadata_version

The debug info version number emitted in the module, or `0` if none is attached.
"""
Module
# forward definition of Module in src/core/value/constant.jl
@properties Module

Base.unsafe_convert(::Type{API.LLVMModuleRef}, mod::Module) = mark_use(mod).ref

Base.:(==)(x::Module, y::Module) = (x.ref === y.ref)

# forward declarations
@checked struct DataLayout
    ref::API.LLVMTargetDataRef
end
@properties DataLayout
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

function name(mod::Module)
    out_len = Ref{Csize_t}()
    ptr = convert(Ptr{UInt8}, API.LLVMGetModuleIdentifier(mod, out_len))
    return unsafe_string(ptr, out_len[])
end

name!(mod::Module, str::String) =
    API.LLVMSetModuleIdentifier(mod, str, Csize_t(length(str)))

@property Module name name!

triple(mod::Module) = unsafe_string(API.LLVMGetTarget(mod))

triple!(mod::Module, triple) = API.LLVMSetTarget(mod, triple)

@property Module triple triple!

datalayout(mod::Module) = DataLayout(API.LLVMGetModuleDataLayout(mod))

datalayout!(mod::Module, layout::String) = API.LLVMSetDataLayout(mod, layout)
datalayout!(mod::Module, layout::DataLayout) =
    API.LLVMSetModuleDataLayout(mod, layout)

@property Module datalayout datalayout!

# LLVM represents module-level inline assembly as a list of fragments (since LLVM 24, each
# with its own target properties), which the C API cannot enumerate yet. By modeling the
# assembly as a collection that is appended to, instead of as a string property, iterating
# the fragments can be supported later without breaking code.
struct ModuleInlineAsm
    mod::Module
end

inline_asm(mod::Module) = ModuleInlineAsm(mod)

@property Module inline_asm

function Base.String(asm::ModuleInlineAsm)
    len = Ref{Csize_t}()
    ptr = API.LLVMGetModuleInlineAsm(asm.mod, len)
    ptr == C_NULL && return ""
    return unsafe_string(convert(Ptr{UInt8}, ptr), len[])
end

Base.print(io::IO, asm::ModuleInlineAsm) = print(io, String(asm))

Base.show(io::IO, asm::ModuleInlineAsm) =
    print(io, "ModuleInlineAsm(", repr(asm.mod.name), "): ", repr(String(asm)))

Base.isempty(asm::ModuleInlineAsm) = isempty(String(asm))

function Base.push!(asm::ModuleInlineAsm, str::AbstractString)
    str = String(str)
    API.LLVMAppendModuleInlineAsm(asm.mod, str, ncodeunits(str))
    return asm
end

function Base.empty!(asm::ModuleInlineAsm)
    API.LLVMSetModuleInlineAsm2(asm.mod, "", 0)
    return asm
end

context(mod::Module) = Context(API.LLVMGetModuleContext(mod))

@property Module context

# `llvm.used` and `llvm.compiler.used` are global variables that LLVM treats specially: the
# global values in their initializer are kept, even if they appear to be unused. The views
# below expose them as sets, which LLVM rebuilds when they are modified.
struct ModuleUsedSet <: AbstractSet{GlobalValue}
    mod::Module
    compiler::Bool
end

used(mod::Module) = ModuleUsedSet(mod, false)
compiler_used(mod::Module) = ModuleUsedSet(mod, true)

@property Module used
@property Module compiler_used

Base.length(set::ModuleUsedSet) =
    Int(set.compiler ? API.LLVMGetNumCompilerUsed(set.mod) : API.LLVMGetNumUsed(set.mod))

# NOTE: optimized `collect`
function Base.collect(set::ModuleUsedSet)
    refs = Vector{API.LLVMValueRef}(undef, length(set))
    set.compiler ? API.LLVMGetCompilerUsed(set.mod, refs) : API.LLVMGetUsed(set.mod, refs)
    return GlobalValue[Value(ref) for ref in refs]
end

# LLVM only supports fetching all values at once
function Base.iterate(set::ModuleUsedSet, (vals, i)=(collect(set), 1))
    i > length(vals) ? nothing : (vals[i], (vals, i+1))
end

Base.in(gv::GlobalValue, set::ModuleUsedSet) = any(==(gv), set)

function Base.union!(set::ModuleUsedSet, gvs)
    vals = GlobalValue[gv for gv in gvs]
    if set.compiler
        API.LLVMAppendToCompilerUsed(set.mod, vals, length(vals))
    else
        API.LLVMAppendToUsed(set.mod, vals, length(vals))
    end
    return set
end
Base.push!(set::ModuleUsedSet, gv::GlobalValue) = union!(set, (gv,))

function Base.setdiff!(set::ModuleUsedSet, gvs)
    vals = GlobalValue[gv for gv in gvs]
    if set.compiler
        API.LLVMRemoveFromCompilerUsed(set.mod, vals, length(vals))
    else
        API.LLVMRemoveFromUsed(set.mod, vals, length(vals))
    end
    return set
end
Base.delete!(set::ModuleUsedSet, gv::GlobalValue) = setdiff!(set, (gv,))

Base.empty!(set::ModuleUsedSet) = setdiff!(set, collect(set))


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
    parse(::Type{Module}, membuf::MemoryBuffer; lazy::Bool=false)

Parse bitcode from the given memory buffer into a module.

If `lazy` is `true`, only the module header is read; function bodies are deserialized on
demand. The module then takes ownership of `membuf`, and the underlying byte storage
(`membuf`'s data) must remain valid for the module's lifetime.
"""
function Base.parse(::Type{Module}, membuf::MemoryBuffer; lazy::Bool=false)
    out_ref = Ref{API.LLVMModuleRef}()
    out_error = Ref{Cstring}()
    ctx = context()
    prepare_diagnostic(ctx)

    # use the variants that return the parser error, instead of the `2` ones that
    # report it through the context's diagnostic handler: contexts we did not create
    # (e.g., Julia's) may not have one installed, making LLVM print the error and exit.
    if lazy
        status = API.LLVMGetBitcodeModuleInContext(ctx, membuf, out_ref, out_error) |> Bool
        # the module only takes ownership of `membuf` on success
        status && API.LLVMDisposeMemoryBuffer(membuf)
        mark_dispose(membuf)
    else
        status = API.LLVMParseBitcodeInContext(ctx, membuf, out_ref, out_error) |> Bool
    end
    status && throw(LLVMException(unsafe_message(out_error[])))
    check_diagnostic(ctx)

    mark_alloc(Module(out_ref[]))
end

"""
    parse(::Type{Module}, data::Vector; lazy::Bool=false)

Parse bitcode from the given byte vector into a module.

If `lazy` is `true`, `data` must remain live (unmutated) for the module's lifetime, as the
bitcode reader keeps reading from it on demand.
"""
function Base.parse(::Type{Module}, data::Vector; lazy::Bool=false)
    if lazy
        # the module takes ownership of `membuf`, so don't @dispose it here
        membuf = MemoryBuffer(data, "", false)
        parse(Module, membuf; lazy=true)
    else
        @dispose membuf = MemoryBuffer(data, "", false) begin
            parse(Module, membuf)
        end
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

struct ModuleGlobalSet
    mod::Module
end

globals(mod::Module) = ModuleGlobalSet(mod)

@property Module globals

Base.eltype(::ModuleGlobalSet) = GlobalVariable

@inline function Base.iterate(iter::ModuleGlobalSet, state=API.LLVMGetFirstGlobal(iter.mod))
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

Base.IteratorSize(::Type{ModuleGlobalSet}) = Base.SizeUnknown()

function next(gv::GlobalVariable)
    ref = API.LLVMGetNextGlobal(gv)
    ref == C_NULL ? nothing : GlobalVariable(ref)
end

function prev(gv::GlobalVariable)
    ref = API.LLVMGetPreviousGlobal(gv)
    ref == C_NULL ? nothing : GlobalVariable(ref)
end

@property GlobalVariable next
@property GlobalVariable prev

# partial associative interface

function Base.haskey(iter::ModuleGlobalSet, name::String)
    return API.LLVMGetNamedGlobal(iter.mod, name) != C_NULL
end

function Base.getindex(iter::ModuleGlobalSet, name::String)
    objref = API.LLVMGetNamedGlobal(iter.mod, name)
    objref == C_NULL && throw(KeyError(name))
    return GlobalVariable(objref)
end

function Base.get(iter::ModuleGlobalSet, name::String, default)
    objref = API.LLVMGetNamedGlobal(iter.mod, name)
    objref == C_NULL ? default : GlobalVariable(objref)
end

"""
    get!(f, mod.globals, name::String)

Look up the global variable called `name`, or call `f()` to create it if the module
doesn't contain one, e.g.:

```julia
gv = get!(mod.globals, "counter") do
    gv = GlobalVariable(mod, LLVM.Int64Type(), "counter")
    gv.initializer = ConstantInt(Int64(0))
    gv
end
```

`f` must return a global variable called `name` in the module. Throws an `ArgumentError`
if another kind of global value, like a function, already uses the name. Like C++'s
`Module::getOrInsertGlobal`, an existing global variable is returned as is, even if it has
a different type.
"""
function Base.get!(f::Base.Callable, iter::ModuleGlobalSet, name::String)
    objref = API.LLVMGetNamedGlobal(iter.mod, name)
    objref == C_NULL || return GlobalVariable(objref)
    check_unused_name(iter.mod, name)
    gv = f()
    gv isa GlobalVariable && gv.name == name && gv.parent == iter.mod ||
        throw(ArgumentError("get! must create a global variable called \"$name\" in the module"))
    return gv
end

# the global values of a module share a namespace
function check_unused_name(mod::Module, name::String)
    for (kind, ref) in (("function", API.LLVMGetNamedFunction(mod, name)),
                        ("global variable", API.LLVMGetNamedGlobal(mod, name)),
                        ("global alias", API.LLVMGetNamedGlobalAlias(mod, name, ncodeunits(name))),
                        ("ifunc", API.LLVMGetNamedGlobalIFunc(mod, name, ncodeunits(name))))
        ref == C_NULL ||
            throw(ArgumentError("Module already contains a $kind called \"$name\""))
    end
end

"""
    sort!(mod.globals; by=gv->gv.name, kwargs...)

Reorder all global variables in a module according to `by`, which defaults to the symbol
name. Additional keyword arguments are forwarded to [`sort!`](@ref).
"""
function Base.sort!(iter::ModuleGlobalSet; by=name, kwargs...)
    elements = collect(iter)
    sort!(elements; by, kwargs...)
    for i in 2:length(elements)
        move_after(elements[i], elements[i-1])
    end
    iter
end


## function iteration

struct ModuleFunctionSet
    mod::Module
end

functions(mod::Module) = ModuleFunctionSet(mod)

@property Module functions

Base.eltype(::ModuleFunctionSet) = Function

@inline function Base.iterate(iter::ModuleFunctionSet, state=API.LLVMGetFirstFunction(iter.mod))
    state == C_NULL ? nothing : (Function(state), API.LLVMGetNextFunction(state))
end

function Base.first(iter::ModuleFunctionSet)
    ref = API.LLVMGetFirstFunction(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    Function(ref)
end

function Base.last(iter::ModuleFunctionSet)
    ref = API.LLVMGetLastFunction(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    Function(ref)
end

Base.isempty(iter::ModuleFunctionSet) = API.LLVMGetLastFunction(iter.mod) == C_NULL

Base.IteratorSize(::Type{ModuleFunctionSet}) = Base.SizeUnknown()

function next(f::Function)
    ref = API.LLVMGetNextFunction(f)
    ref == C_NULL ? nothing : Function(ref)
end

function prev(f::Function)
    ref = API.LLVMGetPreviousFunction(f)
    ref == C_NULL ? nothing : Function(ref)
end

@property Function next
@property Function prev

# partial associative interface

function Base.haskey(iter::ModuleFunctionSet, name::String)
    return API.LLVMGetNamedFunction(iter.mod, name) != C_NULL
end

function Base.getindex(iter::ModuleFunctionSet, name::String)
    objref = API.LLVMGetNamedFunction(iter.mod, name)
    objref == C_NULL && throw(KeyError(name))
    return Function(objref)
end

function Base.get(iter::ModuleFunctionSet, name::String, default)
    objref = API.LLVMGetNamedFunction(iter.mod, name)
    objref == C_NULL ? default : Function(objref)
end

"""
    get!(f, mod.functions, name::String)

Look up the function called `name`, or call `f()` to declare it if the module doesn't
contain one, e.g., to call a runtime function that may or may not have been declared yet:

```julia
abort = get!(mod.functions, "abort") do
    f = LLVM.Function(mod, "abort", LLVM.FunctionType(LLVM.VoidType()))
    push!(f.function_attributes, EnumAttribute(:noreturn))
    f
end
```

`f` must return a function called `name` in the module. Throws an `ArgumentError` if
another kind of global value, like a global variable, already uses the name. Like C++'s
`Module::getOrInsertFunction`, an existing function is returned as is, even if it has a
different function type, so call it using the function type you expect, e.g.,
`call!(builder, ft, abort)`.
"""
function Base.get!(f::Base.Callable, iter::ModuleFunctionSet, name::String)
    objref = API.LLVMGetNamedFunction(iter.mod, name)
    objref == C_NULL || return Function(objref)
    check_unused_name(iter.mod, name)
    fn = f()
    fn isa Function && fn.name == name && fn.parent == iter.mod ||
        throw(ArgumentError("get! must create a function called \"$name\" in the module"))
    return fn
end

"""
    sort!(mod.functions; by=f->f.name, kwargs...)

Reorder all functions in a module according to `by`, which defaults to the symbol name.
Additional keyword arguments are forwarded to [`sort!`](@ref).
"""
function Base.sort!(iter::ModuleFunctionSet; by=name, kwargs...)
    elements = collect(iter)
    sort!(elements; by, kwargs...)
    for i in 2:length(elements)
        move_after(elements[i], elements[i-1])
    end
    iter
end


## global alias iteration

struct ModuleAliasSet
    mod::Module
end

aliases(mod::Module) = ModuleAliasSet(mod)

@property Module aliases

Base.eltype(::ModuleAliasSet) = GlobalAlias

function Base.iterate(iter::ModuleAliasSet, state=API.LLVMGetFirstGlobalAlias(iter.mod))
    state == C_NULL ? nothing : (GlobalAlias(state), API.LLVMGetNextGlobalAlias(state))
end

function Base.first(iter::ModuleAliasSet)
    ref = API.LLVMGetFirstGlobalAlias(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalAlias(ref)
end

function Base.last(iter::ModuleAliasSet)
    ref = API.LLVMGetLastGlobalAlias(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalAlias(ref)
end

Base.isempty(iter::ModuleAliasSet) = API.LLVMGetLastGlobalAlias(iter.mod) == C_NULL

Base.IteratorSize(::Type{ModuleAliasSet}) = Base.SizeUnknown()

function next(alias::GlobalAlias)
    ref = API.LLVMGetNextGlobalAlias(alias)
    ref == C_NULL ? nothing : GlobalAlias(ref)
end

function prev(alias::GlobalAlias)
    ref = API.LLVMGetPreviousGlobalAlias(alias)
    ref == C_NULL ? nothing : GlobalAlias(ref)
end

@property GlobalAlias next
@property GlobalAlias prev

# partial associative interface

function Base.haskey(iter::ModuleAliasSet, name::String)
    return API.LLVMGetNamedGlobalAlias(iter.mod, name, ncodeunits(name)) != C_NULL
end

function Base.getindex(iter::ModuleAliasSet, name::String)
    objref = API.LLVMGetNamedGlobalAlias(iter.mod, name, ncodeunits(name))
    objref == C_NULL && throw(KeyError(name))
    return GlobalAlias(objref)
end

function Base.get(iter::ModuleAliasSet, name::String, default)
    objref = API.LLVMGetNamedGlobalAlias(iter.mod, name, ncodeunits(name))
    objref == C_NULL ? default : GlobalAlias(objref)
end

## ifunc iteration

struct ModuleIFuncSet
    mod::Module
end

ifuncs(mod::Module) = ModuleIFuncSet(mod)

@property Module ifuncs

Base.eltype(::ModuleIFuncSet) = GlobalIFunc

function Base.iterate(iter::ModuleIFuncSet, state=API.LLVMGetFirstGlobalIFunc(iter.mod))
    state == C_NULL ? nothing : (GlobalIFunc(state), API.LLVMGetNextGlobalIFunc(state))
end

function Base.first(iter::ModuleIFuncSet)
    ref = API.LLVMGetFirstGlobalIFunc(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalIFunc(ref)
end

function Base.last(iter::ModuleIFuncSet)
    ref = API.LLVMGetLastGlobalIFunc(iter.mod)
    ref == C_NULL && throw(BoundsError(iter))
    GlobalIFunc(ref)
end

Base.isempty(iter::ModuleIFuncSet) = API.LLVMGetLastGlobalIFunc(iter.mod) == C_NULL

Base.IteratorSize(::Type{ModuleIFuncSet}) = Base.SizeUnknown()

function next(ifunc::GlobalIFunc)
    ref = API.LLVMGetNextGlobalIFunc(ifunc)
    ref == C_NULL ? nothing : GlobalIFunc(ref)
end

function prev(ifunc::GlobalIFunc)
    ref = API.LLVMGetPreviousGlobalIFunc(ifunc)
    ref == C_NULL ? nothing : GlobalIFunc(ref)
end

@property GlobalIFunc next
@property GlobalIFunc prev

# partial associative interface

function Base.haskey(iter::ModuleIFuncSet, name::String)
    return API.LLVMGetNamedGlobalIFunc(iter.mod, name, ncodeunits(name)) != C_NULL
end

function Base.getindex(iter::ModuleIFuncSet, name::String)
    objref = API.LLVMGetNamedGlobalIFunc(iter.mod, name, ncodeunits(name))
    objref == C_NULL && throw(KeyError(name))
    return GlobalIFunc(objref)
end

function Base.get(iter::ModuleIFuncSet, name::String, default)
    objref = API.LLVMGetNamedGlobalIFunc(iter.mod, name, ncodeunits(name))
    objref == C_NULL ? default : GlobalIFunc(objref)
end

## module flag iteration

struct ModuleFlagDict <: AbstractDict{String,Metadata}
    mod::Module
end

flags(mod::Module) = ModuleFlagDict(mod)

@property Module flags

# LLVM only supports fetching all flags at once
function Base.iterate(iter::ModuleFlagDict)
    len = Ref{Csize_t}()
    ptr = API.LLVMCopyModuleFlagsMetadata(iter.mod, len)
    entries = Pair{String,Metadata}[]
    for i in 1:len[]
        keylen = Ref{Csize_t}()
        key = API.LLVMModuleFlagEntriesGetKey(ptr, i-1, keylen)
        md = API.LLVMModuleFlagEntriesGetMetadata(ptr, i-1)
        push!(entries, unsafe_string(convert(Ptr{UInt8}, key), keylen[]) => Metadata(md))
    end
    ptr == C_NULL || API.LLVMDisposeModuleFlagsMetadata(ptr)
    iterate(iter, (entries, 1))
end
function Base.iterate(::ModuleFlagDict, (entries, i))
    i > length(entries) ? nothing : (entries[i], (entries, i+1))
end

Base.length(iter::ModuleFlagDict) = count(Returns(true), iter)

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
    return iter
end


## sdk version

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

function sdk_version(mod::Module)
    haskey(flags(mod), "SDK Version") || return nothing
    md = flags(mod)["SDK Version"]
    c = context!(context(mod)) do
        Value(md)
    end
    entries = collect(c)
    VersionNumber(map(val->convert(Int, val), entries)...)
end

@property Module sdk_version sdk_version!
