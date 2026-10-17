# scoped names for the enums of the C API

# The enums of the C API are the values that LLVM.jl's API takes and returns (e.g.,
# `gv.linkage`). Their names are long, and part of the raw `LLVM.API` namespace, so we make
# them available in a module per enum, with the prefix and suffix stripped from their names:
# `LLVM.Linkage.Internal === LLVM.API.LLVMInternalLinkage`. These modules only contain
# aliases of the values of the C API (and `T`, their type), so they don't need to be kept in
# sync with LLVM. Like the enums in `API`, they contain the values of every version of LLVM
# that LLVM.jl supports.
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

# the enums of `API` contain the members of every version of LLVM; their translation tables
# say which ones a version has
function isavailable(x::CEnum.Cenum, major::Integer)
    tables = API.abi_tables(typeof(x), major)
    tables === nothing && return true
    val = Integer(x)
    0 <= val < length(tables.to) || return true
    return @inbounds(tables.to[val+1]) >= 0
end
isavailable(x::CEnum.Cenum) = isavailable(x, version().major)

# the LLVM version that introduced each atomicrmw operation, indexed by its C API value.
# LLVM.jl supports some of them on versions whose C API does not name them yet.
const ATOMIC_RMW_BINOP_SINCE = (
    ntuple(_ -> v"0", 15)...,   # Xchg through FMin
    v"16", v"16",               # UIncWrap, UDecWrap
    v"20", v"20",               # USubCond, USubSat
    v"21", v"21",               # FMaximum, FMinimum
    v"23", v"23",               # FMaximumNum, FMinimumNum
)

function isavailable(op::API.LLVMAtomicRMWBinOp, major::Integer)
    since = get(ATOMIC_RMW_BINOP_SINCE, Integer(op) + 1, nothing)
    since !== nothing && major >= since.major
end

"""
    isavailable(op::LLVM.AtomicRMWBinOp.T)

Check whether the atomic read-modify-write operation `op` is supported by the version of
LLVM in use. All operations can be named on every LLVM version, and are listed by
`instances(LLVM.AtomicRMWBinOp.T)`, but instructions can only be created with the ones
that are available, e.g., `filter(LLVM.isavailable, instances(LLVM.AtomicRMWBinOp.T))`.
"""
isavailable(op::API.LLVMAtomicRMWBinOp) = isavailable(op, version().major)
@public isavailable

# the versions of LLVM that have a member of an enum, e.g., "all" or "19+"
function availability(@nospecialize(x))
    versions = [v for v in API.WRAPPED_VERSIONS if isavailable(x, v)]
    versions == API.WRAPPED_VERSIONS && return "all"
    isempty(versions) && return "none"
    lo, hi = first(versions), last(versions)
    hi == last(API.WRAPPED_VERSIONS) && return "$lo+"
    lo == hi ? "$lo" : "$(lo)–$(hi)"
end

# the members of an enum: (short name, integer value, name in the C API), in the order of
# the values, derived from the constants in `API` of that type.
#
# this code runs when the package is defined, and anything it compiles is stored in the
# package image. so don't specialize on the type of the enum, which would compile (and
# store) the sorting, broadcasting and printing machinery once for every enum.
Base.@nospecializeinfer function enum_members(@nospecialize(T::Type), prefix::String,
                                               suffix::String, renames)
    renames = Dict{Symbol,Symbol}(renames)
    members = Tuple{Symbol,Int64,Symbol}[]
    for raw in names(API; all=true)
        isdefined(API, raw) && isconst(API, raw) || continue
        val = getfield(API, raw)
        typeof(val) === T || continue
        str = String(raw)
        startswith(str, prefix) ||
            error("Unexpected member $raw of $T, which should start with $prefix")
        str = chopprefix(str, prefix)
        str = chopsuffix(str, suffix)
        name = get(renames, Symbol(str), Symbol(str))
        Base.isidentifier(name) || error("Invalid name $name for member $raw of $T")
        any(m -> m[1] == name, members) && error("Duplicate name $name for member $raw of $T")
        push!(members, (name, Int64(Integer(val)::Integer), raw))
    end
    # a simple sort, as the default algorithm compiles a lot of code (for small tables)
    sort!(members; by=m->(m[2], m[3]), alg=InsertionSort)
end

Base.@nospecializeinfer function enum_doc(scope::Symbol, @nospecialize(T::Type),
                                          description::String,
                                          members::Vector{Tuple{Symbol,Int64,Symbol}})
    io = IOBuffer()
    println(io, "    LLVM.", scope)
    println(io)
    print(io, "Values of the `LLVM.API.", nameof(T), "` enum, for ", description, ", e.g., ")
    println(io, "`LLVM.", scope, ".", first(members)[1], "`. Their type is `LLVM.", scope,
            ".T`. Some values are only available on some versions of LLVM (see ",
            "[`LLVM.isavailable`](@ref)):")
    println(io)
    println(io, "| Name | Value | C API | Availability |")
    println(io, "|:---- |:----- |:----- |:---- |")
    for (name, val, raw) in members
        println(io, "| `", name, "` | ", val, " | `", raw, "` | ",
                availability(getfield(API, raw)), " |")
    end
    return String(take!(io))
end

# the scope of every enum type, and the names to display its values with
const enum_display_names = IdDict{Type,Tuple{Symbol,Dict{Int64,Symbol}}}()

for (scope, typename, description, prefix, suffix, renames) in enum_scopes
    # some enums are only available on some versions of LLVM
    isdefined(API, typename) || continue
    T = getfield(API, typename)
    members = enum_members(T, prefix, suffix, renames)

    # a bare module, so that names like `TypeKind.Function` don't clash with Base
    body = Expr(:block, :(const T = $T))
    for (name, _, raw) in members
        push!(body.args, :(const $name = $(getfield(API, raw))))
    end
    if VERSION >= v"1.11"
        push!(body.args, Expr(:public, :T, first.(members)...))
    end
    Core.eval(@__MODULE__, Expr(:module, false, scope, body))
    Core.eval(@__MODULE__, :(@public $scope))
    Core.eval(@__MODULE__, :(@doc $(enum_doc(scope, T, description, members)) $scope))

    display_names = Dict{Int64,Symbol}()
    for (name, val, _) in members
        get!(display_names, val, name)
    end
    enum_display_names[T] = (scope, display_names)
end

# display values using their scoped name, and a constructor for unnamed values. these are
# defined once for all enums, as every method that's added to `show` makes loading LLVM.jl
# slower (Julia has to check it against the many other methods of `show`).
const ScopedEnum = Union{keys(enum_display_names)...}
function Base.show(io::IO, x::ScopedEnum)
    scope, display_names = enum_display_names[typeof(x)]
    show_enum(io, scope, display_names, Int64(Integer(x)))
end
@noinline function show_enum(@nospecialize(io::IO), scope::Symbol,
                             display_names::Dict{Int64,Symbol}, val::Int64)
    name = get(display_names, val, nothing)
    if name === nothing
        print(io, "LLVM.", scope, ".T(", val, ")")
    else
        print(io, "LLVM.", scope, ".", name)
    end
end
Base.show(io::IO, ::MIME"text/plain", x::ScopedEnum) = show(io, x)
Base.print(io::IO, x::ScopedEnum) = show(io, x)
