## instruction insertion

@vocabulary Build dbg_declare!, dbg_value!

"""
    dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                 expr::DIExpression, debugloc::DILocation,
                 pos::InsertionPoint{Instruction})

Insert a debug record that declares `storage` as the address of the variable `var` at the
given position, e.g., `LLVM.after(alloca)`. Returns a `DbgRecord` on LLVM ≥ 19, or
the `llvm.dbg.declare` call [`Instruction`](@ref) on LLVM < 19.

Debug records can not be inserted at the end of a block that has a terminator; use
`LLVM.before(bb.terminator)` instead. Several records inserted at a position that is
before the debug records of an instruction (e.g., `LLVM.after(inst)` or
`LLVM.at_begin(bb)`) end up in reverse order, as each one is inserted in front of the
others. Use `LLVM.before(inst)` to append records to the ones of `inst`.
"""
dbg_declare!

"""
    dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
               expr::DIExpression, debugloc::DILocation,
               pos::InsertionPoint{Instruction})

Insert a debug record that describes `val` as the value of the variable `var` at the given
position. Returns a `DbgRecord` on LLVM ≥ 19, or the `llvm.dbg.value` call
[`Instruction`](@ref) on LLVM < 19. See [`dbg_declare!`](@ref) for which positions can be
used.
"""
dbg_value!

# debug records can't be inserted after a terminator, or refer to values of another context
function check_record_position(pos::InsertionPoint{Instruction}, val=nothing)
    bb = check_valid(pos)
    pos.anchor == C_NULL && API.LLVMGetBasicBlockTerminator(bb) != C_NULL &&
        throw(ArgumentError("Cannot insert debug records after the terminator of a basic block"))
    val === nothing || API.LLVMGetValueContext(val) == API.LLVMGetValueContext(bb) ||
        throw(ArgumentError("Cannot insert a debug record for a value of another context"))
    return bb
end

@static if version() >= v"19"

@vocabulary IR DbgRecord

@doc """
    DbgRecord

A non-instruction debug record attached to a basic block, replacing the
legacy `llvm.dbg.*` intrinsics in LLVM ≥ 19.

# Properties

    record.kind

The kind of the debug record, an `LLVM.API.LLVMDbgRecordKind`: `LLVMDbgRecordDeclare`,
`LLVMDbgRecordValue` or `LLVMDbgRecordAssign` for variable records, which describe the
location of a source variable, or `LLVMDbgRecordLabel` for label records.

Variable records can be further inspected using the following properties:
- `record.variable`: the source variable that is described;
- `record.expression`: the expression that computes the variable's location;
- `record.value`: the IR value used by that expression, or `record.location_operands` for
  expressions that use several values.

The source location of every record is available as `record.debug_location`.

    record.debug_location

The source location of the debug record.

    record.variable

The source variable described by a variable record.

    record.expression

The expression that computes the location of the variable described by a variable record,
in terms of its `location_operands`.

    record.location_operands

The IR values that are used to compute the location of the variable described by a
variable record, as a read-only view. There is usually only one, but records that use a
`!DIArgList` can refer to several. Entries are `nothing` if the value has been deleted.

See also the `LLVM.DbgRecord` property.

    record.value

The IR value used to compute the location of the variable described by a variable record,
or `nothing` if that value has been deleted. Records that refer to several values need to
be inspected using their `location_operands` instead.

    record.next
    record.prev

The next or previous debug record attached to the same instruction, or `nothing` if there
is none. `prev` requires LLVM 20+.
"""
@checked struct DbgRecord
    ref::API.LLVMDbgRecordRef
end
@properties DbgRecord

Base.unsafe_convert(::Type{API.LLVMDbgRecordRef}, record::DbgRecord) = record.ref

function Base.show(io::IO, record::DbgRecord)
    str_ptr = API.LLVMPrintDbgRecordToString(record)
    str = unsafe_string(str_ptr)
    print(io, rstrip(str))
    # LLVMPrintDbgRecordToString-returned memory is freed by LLVMDisposeMessage
    API.LLVMDisposeMessage(str_ptr)
end

# record iteration

struct DbgRecordIterator
    inst::Instruction
end

debug_records(inst::Instruction) = DbgRecordIterator(inst)

@property Instruction debug_records

Base.IteratorSize(::Type{DbgRecordIterator}) = Base.SizeUnknown()
Base.eltype(::Type{DbgRecordIterator}) = DbgRecord

function Base.iterate(iter::DbgRecordIterator)
    ref = @static if version() >= v"22"
        API.LLVMGetFirstDbgRecord(iter.inst)
    else
        # the upstream function crashes on instructions without debug records
        API.LLVMGetFirstDbgRecord2(iter.inst)
    end
    iterate(iter, ref)
end
function Base.iterate(::DbgRecordIterator, ref::API.LLVMDbgRecordRef)
    ref == C_NULL && return nothing
    return DbgRecord(ref), API.LLVMGetNextDbgRecord(ref)
end

# record inspection

kind(record::DbgRecord) = API.LLVMDbgRecordGetKind(record)

function check_variable_record(record::DbgRecord)
    kind(record) == API.LLVMDbgRecordLabel &&
        throw(ArgumentError("Label records do not describe a variable"))
    return
end

debug_location(record::DbgRecord) =
    Metadata(API.LLVMDbgRecordGetDebugLoc(record))::DILocation

function variable(record::DbgRecord)
    check_variable_record(record)
    Metadata(API.LLVMDbgVariableRecordGetVariable(record))::DILocalVariable
end

function expression(record::DbgRecord)
    check_variable_record(record)
    Metadata(API.LLVMDbgVariableRecordGetExpression(record))::DIExpression
end

struct DbgRecordLocationOperandSet <: AbstractVector{Union{Value,Nothing}}
    record::DbgRecord
end

function location_operands(record::DbgRecord)
    check_variable_record(record)
    DbgRecordLocationOperandSet(record)
end

Base.size(iter::DbgRecordLocationOperandSet) =
    (Int(API.LLVMExtraDbgVariableRecordGetNumValues(iter.record)),)

Base.IndexStyle(::Type{DbgRecordLocationOperandSet}) = IndexLinear()

function Base.getindex(iter::DbgRecordLocationOperandSet, i::Int)
    @boundscheck 1 <= i <= length(iter) || throw(BoundsError(iter, i))
    ref = API.LLVMDbgVariableRecordGetValue(iter.record, i-1)
    return ref == C_NULL ? nothing : Value(ref)
end

function value(record::DbgRecord)
    check_variable_record(record)
    n = API.LLVMExtraDbgVariableRecordGetNumValues(record)
    n == 1 ||
        throw(ArgumentError("Debug record refers to $n values, use its location_operands"))
    ref = API.LLVMDbgVariableRecordGetValue(record, 0)
    return ref == C_NULL ? nothing : Value(ref)
end

function next(record::DbgRecord)
    ref = API.LLVMGetNextDbgRecord(record)
    ref == C_NULL ? nothing : DbgRecord(ref)
end

function prev(record::DbgRecord)
    ref = API.LLVMGetPreviousDbgRecord(record)
    ref == C_NULL ? nothing : DbgRecord(ref)
end

@property DbgRecord kind
@property DbgRecord debug_location
@property DbgRecord variable
@property DbgRecord expression
@property DbgRecord value
@property DbgRecord location_operands
@property DbgRecord next
@static if version() >= v"20"
    @property DbgRecord prev
end

function dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                      expr::DIExpression, debugloc::DILocation,
                      pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, storage)
    DbgRecord(API.LLVMExtraDIBuilderInsertDeclareRecordAt(
        builder, storage, var, expr, debugloc, bb, pos.anchor, pos.head))
end

function dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, val)
    DbgRecord(API.LLVMExtraDIBuilderInsertDbgValueRecordAt(
        builder, val, var, expr, debugloc, bb, pos.anchor, pos.head))
end

else # LLVM < 19: debug intrinsics, which are ordinary instructions

function dbg_declare!(builder::DIBuilder, storage::Value, var::DILocalVariable,
                      expr::DIExpression, debugloc::DILocation,
                      pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, storage)
    # at the end of a block without a terminator, AtEnd inserts at the end
    Instruction(pos.anchor == C_NULL ?
        API.LLVMDIBuilderInsertDeclareAtEnd(builder, storage, var, expr, debugloc, bb) :
        API.LLVMDIBuilderInsertDeclareBefore(builder, storage, var, expr, debugloc,
                                             pos.anchor))
end

function dbg_value!(builder::DIBuilder, val::Value, var::DILocalVariable,
                    expr::DIExpression, debugloc::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos, val)
    Instruction(pos.anchor == C_NULL ?
        API.LLVMDIBuilderInsertDbgValueAtEnd(builder, val, var, expr, debugloc, bb) :
        API.LLVMDIBuilderInsertDbgValueBefore(builder, val, var, expr, debugloc,
                                              pos.anchor))
end

end # @static version check


## label (LLVM 20+)

@static if version() >= v"20"

@vocabulary IR DILabel
@vocabulary Build label!, dbg_label!

@doc """
    DILabel

A debug-info label, describing a source-level code location by name.
Requires LLVM 20+.
"""
@checked struct DILabel <: DINode
    ref::API.LLVMMetadataRef
end
register(DILabel, API.LLVMDILabelMetadataKind)

@doc """
    label!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
           file::DIFile, line::Integer;
           always_preserve::Bool=false) -> DILabel

Create a new [`DILabel`](@ref). Requires LLVM 20+.
"""
function label!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                file::DIFile, line::Integer;
                always_preserve::Bool=false)
    name = String(name)
    DILabel(API.LLVMDIBuilderCreateLabel(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), always_preserve))
end

@doc """
    dbg_label!(builder::DIBuilder, label::DILabel, location::DILocation,
               pos::InsertionPoint{Instruction}) -> DbgRecord

Insert a debug record for the label `label` at the given position. See
[`dbg_declare!`](@ref) for which positions can be used. Requires LLVM 20+.
"""
function dbg_label!(builder::DIBuilder, label::DILabel, location::DILocation,
                    pos::InsertionPoint{Instruction})
    bb = check_record_position(pos)
    DbgRecord(API.LLVMExtraDIBuilderInsertLabelAt(builder, label, location, bb, pos.anchor,
                                                  pos.head))
end

end # @static version check
