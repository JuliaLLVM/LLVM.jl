# scoped names for the enums of the C API

# The enums of the C API are generated per LLVM version, and are the values that LLVM.jl's
# API takes and returns (e.g., `gv.linkage`). Their names are long, and part of the raw
# `LLVM.API` namespace, so we make them available in a module per enum, with the prefix and
# suffix stripped from their names: `LLVM.Linkage.Internal === LLVM.API.LLVMInternalLinkage`.
# These modules only contain aliases of the values of the C API (and `T`, their type), so
# they don't need to be kept in sync with LLVM, and automatically contain the values that
# are available on the current version of LLVM (including those that `API` backfills).
#
# The modules are public, but not part of a vocabulary, as their names are common words
# (`Opcode`, `Linkage`), and qualifying them documents what the value is about. To add an
# enum, add an entry below; don't add enums that are bit flags, or only used internally.

# scope, enum type, description, prefix and suffix of the member names, and renames of the
# stripped names (for when stripping the prefix and suffix is not enough)
const enum_scopes = [
    (:Linkage, :LLVMLinkage, "the linkage of global values",
     "LLVM", "Linkage", ()),
    (:Visibility, :LLVMVisibility, "the visibility of global values",
     "LLVM", "Visibility", ()),
    (:DLLStorageClass, :LLVMDLLStorageClass, "the DLL storage class of global values",
     "LLVM", "StorageClass", (:DLLImport => :Import, :DLLExport => :Export)),
    (:UnnamedAddr, :LLVMUnnamedAddr, "whether the address of a global value is significant",
     "LLVM", "UnnamedAddr", ()),
    (:ThreadLocalMode, :LLVMThreadLocalMode, "the thread-local storage model of global variables",
     "LLVM", "TLSModel", ()),
    (:CallConv, :LLVMCallConv, "calling conventions",
     "LLVM", "CallConv", ()),
    (:IntPredicate, :LLVMIntPredicate, "the predicates of integer comparisons",
     "LLVMInt", "", ()),
    (:RealPredicate, :LLVMRealPredicate, "the predicates of floating-point comparisons",
     "LLVMReal", "", (:PredicateFalse => :False, :PredicateTrue => :True)),
    (:Opcode, :LLVMOpcode, "the opcodes of instructions and constant expressions",
     "LLVM", "", ()),
    (:TypeKind, :LLVMTypeKind, "the kinds of types",
     "LLVM", "TypeKind", ()),
    (:ValueKind, :LLVMValueKind, "the kinds of values",
     "LLVM", "ValueKind", ()),
    (:AtomicOrdering, :LLVMAtomicOrdering, "the orderings of atomic operations",
     "LLVMAtomicOrdering", "", ()),
    (:AtomicRMWBinOp, :LLVMAtomicRMWBinOp, "the operations of `atomicrmw` instructions",
     "LLVMAtomicRMWBinOp", "", ()),
    (:TailCallKind, :LLVMTailCallKind, "the tail call markers of calls",
     "LLVMTailCallKind", "", ()),
    (:InlineAsmDialect, :LLVMInlineAsmDialect, "the dialects of inline assembly",
     "LLVMInlineAsmDialect", "", ()),
    (:ModuleFlagBehavior, :LLVMModuleFlagBehavior, "how module flags are merged when linking",
     "LLVMModuleFlagBehavior", "", ()),
    (:CloneFunctionChangeType, :LLVMCloneFunctionChangeType, "the kind of changes when cloning functions",
     "LLVMCloneFunctionChangeType", "", ()),
    (:DWARFSourceLanguage, :LLVMDWARFSourceLanguage, "the source languages of compile units",
     "LLVMDWARFSourceLanguage", "", ()),
    (:DWARFEmissionKind, :LLVMDWARFEmissionKind, "the amount of debug info to emit for compile units",
     "LLVMDWARFEmission", "", ()),
    (:DebugEmissionKind, :LLVMDebugEmissionKind, "the amount of debug info that Julia's code generator emits (`debug_info_kind` in its `CodegenParams`)",
     "LLVMDebugEmissionKind", "", ()),
    (:CodeGenOptLevel, :LLVMCodeGenOptLevel, "the optimization levels of code generation",
     "LLVMCodeGenLevel", "", ()),
    (:CodeGenFileType, :LLVMCodeGenFileType, "the kinds of files that code generation emits",
     "LLVM", "File", ()),
    (:RelocMode, :LLVMRelocMode, "the relocation models of target machines",
     "LLVMReloc", "", ()),
    (:CodeModel, :LLVMCodeModel, "the code models of target machines",
     "LLVMCodeModel", "", ()),
    (:ByteOrdering, :LLVMByteOrdering, "the byte orders of data layouts",
     "LLVM", "Endian", ()),
    (:LookupKind, :LLVMOrcLookupKind, "the kinds of ORC symbol lookups",
     "LLVMOrcLookupKind", "", ()),
    (:JITDylibLookupFlags, :LLVMOrcJITDylibLookupFlags, "which symbols of a `JITDylib` an ORC lookup matches",
     "LLVMOrcJITDylibLookupFlags", "", ()),
    (:SymbolLookupFlags, :LLVMOrcSymbolLookupFlags, "whether an ORC lookup requires a symbol",
     "LLVMOrcSymbolLookupFlags", "", ()),
]

# the members of an enum: (short name, value, name in the C API), in the order of the
# values, derived from the constants in `API` of that type
function enum_members(T::Type, prefix::String, suffix::String, renames)
    members = Tuple{Symbol,T,Symbol}[]
    for raw in names(API; all=true)
        isdefined(API, raw) && isconst(API, raw) || continue
        val = getfield(API, raw)
        typeof(val) === T || continue
        str = String(raw)
        startswith(str, prefix) ||
            error("Unexpected member $raw of $T, which should start with $prefix")
        str = chopprefix(str, prefix)
        str = chopsuffix(str, suffix)
        name = get(Dict(renames), Symbol(str), Symbol(str))
        Base.isidentifier(name) || error("Invalid name $name for member $raw of $T")
        any(m -> m[1] == name, members) && error("Duplicate name $name for member $raw of $T")
        push!(members, (name, val, raw))
    end
    sort!(members; by=m->(Integer(m[2]), m[3]))
end

function enum_doc(scope::Symbol, T::Type, description::String, members)
    io = IOBuffer()
    println(io, "    LLVM.", scope)
    println(io)
    print(io, "Values of the `LLVM.API.", nameof(T), "` enum, for ", description, ", e.g., ")
    println(io, "`LLVM.", scope, ".", first(members)[1], "`. Their type is `LLVM.", scope,
            ".T`. The available values depend on the version of LLVM:")
    println(io)
    println(io, "| Name | Value | C API |")
    println(io, "|:---- |:----- |:----- |")
    for (name, val, raw) in members
        println(io, "| `", name, "` | ", Integer(val), " | `", raw, "` |")
    end
    return String(take!(io))
end

for (scope, typename, description, prefix, suffix, renames) in enum_scopes
    # some enums are only available on some versions of LLVM
    isdefined(API, typename) || continue
    T = getfield(API, typename)
    members = enum_members(T, prefix, suffix, renames)

    # a bare module, so that names like `TypeKind.Function` don't clash with Base
    body = Expr(:block, :(const T = $T))
    for (name, val, _) in members
        push!(body.args, :(const $name = $val))
    end
    if VERSION >= v"1.11"
        push!(body.args, Expr(:public, :T, first.(members)...))
    end
    Core.eval(@__MODULE__, Expr(:module, false, scope, body))
    Core.eval(@__MODULE__, :(@public $scope))
    Core.eval(@__MODULE__, :(@doc $(enum_doc(scope, T, description, members)) $scope))

    # display values using their scoped name, and a constructor for unnamed values
    display_names = Dict{Integer,Symbol}()
    for (name, val, _) in members
        get!(display_names, Integer(val), name)
    end
    @eval begin
        function Base.show(io::IO, x::$T)
            name = get($display_names, Integer(x), nothing)
            if name === nothing
                print(io, "LLVM.", $(QuoteNode(scope)), ".T(", Integer(x), ")")
            else
                print(io, "LLVM.", $(QuoteNode(scope)), ".", name)
            end
        end
        Base.show(io::IO, ::MIME"text/plain", x::$T) = show(io, x)
        Base.print(io::IO, x::$T) = show(io, x)
    end
end
