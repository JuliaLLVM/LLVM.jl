# Insertion points: positions in the lists of instructions, basic blocks, functions and
# global variables, where objects can be inserted or moved to.

# these are thin wrappers around the C API, so don't specialize them on the concrete type
# of their arguments
@nospecialize

@vocabulary IR InsertionPoint, move!
@public before, after, at_begin, at_end, after_phis, after_def, anchor

"""
    InsertionPoint{T}

A position in a list of IR objects of type `T`: the instructions of a basic block
(`InsertionPoint{Instruction}`), the blocks of a function (`InsertionPoint{BasicBlock}`),
or the functions or global variables of a module (`InsertionPoint{Function}` and
`InsertionPoint{GlobalVariable}`).

Insertion points are created with [`LLVM.before`](@ref), [`LLVM.after`](@ref),
[`LLVM.at_begin`](@ref), [`LLVM.at_end`](@ref), [`LLVM.after_phis`](@ref) and
[`LLVM.after_def`](@ref), and are
used to position an instruction builder ([`position!`](@ref)), to move objects
([`move!`](@ref)), to create basic blocks ([`BasicBlock`](@ref)), and to insert debug
records ([`dbg_value!`](@ref) and friends):

```julia
position!(builder, LLVM.after(inst))
move!(inst, LLVM.at_begin(bb))
BasicBlock(LLVM.after(entry), "cont")
```

An insertion point is resolved when it is created: `LLVM.after(x)` is the position before
the object that follows `x`, or the end of the list if `x` is the last one. Repeated
insertion of instructions, blocks, functions or globals at the same point preserves order.
An object added after `x` in the meantime appears before later insertions at a saved
`LLVM.after(x)` point. Debug records inserted at a head position reverse order on LLVM
19 and later; see [`dbg_declare!`](@ref).

Like an iterator, an insertion point refers to the object that it inserts before (its
anchor), and is invalidated when that object is erased, or when it is moved elsewhere.
Using an invalid insertion point is undefined behavior; moving the anchor to another
block, function or module is detected, but erasing it is not.
"""
struct InsertionPoint{T}
    # the list that the insertion point is in: the value ref of a basic block (for
    # instructions) or a function (for basic blocks), or a module ref
    parent::Ptr{Cvoid}
    # the object to insert before, or C_NULL for the end of the list
    anchor::API.LLVMValueRef
    # whether to insert before the debug records attached to the anchor (or trailing the
    # block, at the end of a block), like the head bit of a C++ iterator
    head::Bool

    # debug records, and with them the head bit, only exist since LLVM 19
    InsertionPoint{T}(parent, anchor, head::Bool) where {T} =
        new{T}(parent, anchor, atleast(19) && head)
end

parent_block(pos::InsertionPoint{Instruction}) =
    BasicBlock(convert(API.LLVMValueRef, pos.parent))
parent_function(pos::InsertionPoint{BasicBlock}) =
    Function(convert(API.LLVMValueRef, pos.parent))
parent_module(pos::Union{InsertionPoint{Function},InsertionPoint{GlobalVariable}}) =
    Module(convert(API.LLVMModuleRef, pos.parent))

# check that the anchor of an insertion point is still part of the same list, and return
# the parent of the list
function check_valid(pos::InsertionPoint{Instruction})
    pos.anchor == C_NULL ||
        API.LLVMGetInstructionParent(pos.anchor) ==
            API.LLVMValueAsBasicBlock(convert(API.LLVMValueRef, pos.parent)) ||
        throw(ArgumentError("Insertion point is invalid: its instruction was moved to another basic block"))
    return parent_block(pos)
end
function check_valid(pos::InsertionPoint{BasicBlock})
    pos.anchor == C_NULL ||
        API.LLVMGetBasicBlockParent(API.LLVMValueAsBasicBlock(pos.anchor)) ==
            convert(API.LLVMValueRef, pos.parent) ||
        throw(ArgumentError("Insertion point is invalid: its basic block was moved to another function"))
    return parent_function(pos)
end
function check_valid(pos::Union{InsertionPoint{Function},InsertionPoint{GlobalVariable}})
    pos.anchor == C_NULL ||
        API.LLVMGetGlobalParent(pos.anchor) == convert(API.LLVMModuleRef, pos.parent) ||
        throw(ArgumentError("Insertion point is invalid: its anchor was moved to another module"))
    return parent_module(pos)
end

function Base.show(io::IO, pos::InsertionPoint{T}) where {T}
    print(io, "InsertionPoint{", nameof(T), "}(")
    if T <: Instruction
        show(io, parent_block(pos))
    elseif T <: BasicBlock
        show(io, parent_function(pos))
    else
        show(io, parent_module(pos))
    end
    if pos.anchor == C_NULL
        print(io, ", at the end")
        pos.head && print(io, ", before the trailing debug records")
    else
        print(io, ", before ")
        show(io, Value(pos.anchor))
        pos.head && print(io, " and its debug records")
    end
    print(io, ")")
end

"""
    LLVM.anchor(pos::InsertionPoint)

The object that an [`InsertionPoint`](@ref) inserts before: an instruction, basic block,
function or global variable, or `nothing` at the end of the list. For example,
`LLVM.anchor(LLVM.after_phis(bb))` is the first instruction of `bb` that is not a PHI node
or an exception-handling pad, like C++'s `&*bb->getFirstInsertionPt()`, for a block that
has such a position (see [`LLVM.after_phis`](@ref)).
"""
function anchor(pos::InsertionPoint{Instruction})
    check_valid(pos)
    return pos.anchor == C_NULL ? nothing : Instruction(pos.anchor)
end
function anchor(pos::InsertionPoint{BasicBlock})
    check_valid(pos)
    return pos.anchor == C_NULL ? nothing : BasicBlock(API.LLVMValueAsBasicBlock(pos.anchor))
end
function anchor(pos::InsertionPoint{T}) where {T<:Union{Function,GlobalVariable}}
    check_valid(pos)
    return pos.anchor == C_NULL ? nothing : Value(pos.anchor)::T
end


## instructions

"""
    LLVM.before(inst::Instruction)
    LLVM.before(bb::BasicBlock)
    LLVM.before(f::LLVM.Function)
    LLVM.before(gv::GlobalVariable)

The [`InsertionPoint`](@ref) right before the given object, which needs to be part of a
basic block, function or module.

For an instruction, objects that are inserted there come after the debug records attached
to the instruction, i.e., between those records and the instruction. Use
[`LLVM.after`](@ref) on the previous instruction to insert before them.
"""
function before(inst::Instruction)
    bb = API.LLVMGetInstructionParent(check_attached(inst))
    InsertionPoint{Instruction}(API.LLVMBasicBlockAsValue(bb), propref(inst), false)
end

"""
    LLVM.after(inst::Instruction)
    LLVM.after(bb::BasicBlock)
    LLVM.after(f::LLVM.Function)
    LLVM.after(gv::GlobalVariable)

The [`InsertionPoint`](@ref) right after the given object, which needs to be part of a
basic block, function or module. This is the position before the next object, or the end
of the list if there is none.

For an instruction, objects that are inserted there come before the debug records that are
attached to the next instruction.

Insertion points are literal: after a `phi` instruction, objects are inserted there even
if the next instruction is another `phi` instruction. Use [`LLVM.after_phis`](@ref) to get
the first position after the PHI nodes of a block.
"""
function after(inst::Instruction)
    bb = API.LLVMGetInstructionParent(check_attached(inst))
    InsertionPoint{Instruction}(API.LLVMBasicBlockAsValue(bb),
                                API.LLVMGetNextInstruction(inst), true)
end

"""
    LLVM.at_begin(bb::BasicBlock)
    LLVM.at_begin(f::LLVM.Function)
    LLVM.at_begin(mod.functions)
    LLVM.at_begin(mod.globals)

The [`InsertionPoint`](@ref) at the beginning of the instructions of a basic block, the
blocks of a function, or the functions or global variables of a module.

The beginning of a block is literal: it is the position before any PHI nodes, and before
the debug records attached to the first instruction. Use [`LLVM.after_phis`](@ref) for the
first position where instructions other than PHI nodes can be inserted.
"""
at_begin(bb::BasicBlock) =
    InsertionPoint{Instruction}(propref(bb), API.LLVMGetFirstInstruction(bb), true)

"""
    LLVM.at_end(bb::BasicBlock)
    LLVM.at_end(f::LLVM.Function)
    LLVM.at_end(mod.functions)
    LLVM.at_end(mod.globals)

The [`InsertionPoint`](@ref) at the end of the instructions of a basic block, the blocks
of a function, or the functions or global variables of a module.

The end of a block is literal: if the block has a terminator, the position is after it.
An instruction builder can be positioned there (e.g., to build the terminator of an
unterminated block), but instructions and debug records can not be inserted there. Use
`LLVM.before(bb.terminator)` to insert them at the end of a terminated block.
"""
at_end(bb::BasicBlock) = InsertionPoint{Instruction}(propref(bb), C_NULL, false)

"""
    LLVM.after_phis(bb::BasicBlock)

The first [`InsertionPoint`](@ref) of a basic block where instructions other than PHI nodes
can be inserted: after the PHI nodes of the block, and after an exception-handling pad
like a `landingpad`, if any. Returns `nothing` if there is no such position, e.g., in a
block that only contains a `catchswitch`.
"""
function after_phis(bb::BasicBlock)
    anchor = Ref{API.LLVMValueRef}()
    head = Ref{API.LLVMBool}()
    Bool(API.LLVMExtraGetFirstInsertionPt(bb, anchor, head)) || return nothing
    InsertionPoint{Instruction}(propref(bb), anchor[], Bool(head[]))
end

"""
    LLVM.after_def(inst::Instruction)

The first [`InsertionPoint`](@ref) where the value that the instruction defines is
available, i.e., where instructions that use it can be inserted. This is the position after
the instruction, except for a `phi` instruction, where it is after the PHI nodes (and
exception-handling pad) of its block, and for an `invoke` instruction, where it is at the
beginning of its normal destination (after its PHI nodes). In the latter case the position
does not necessarily dominate all uses of the value.

Returns `nothing` if there is no such position, e.g., for a `callbr` instruction, whose
value is available in several destinations, or if the position would be in a block that
only contains a `catchswitch`.
"""
function after_def(inst::Instruction)
    check_attached(inst)
    value_type(inst) isa VoidType &&
        throw(ArgumentError("Instruction does not define a value"))
    # terminators other than invokes and callbrs (e.g., a catchswitch) have no position
    # after them in their block
    isterminator(inst) && !(inst isa Union{InvokeInst,CallBrInst}) && return nothing
    bb = Ref{API.LLVMBasicBlockRef}()
    anchor = Ref{API.LLVMValueRef}()
    head = Ref{API.LLVMBool}()
    Bool(API.LLVMExtraGetInsertionPointAfterDef(inst, bb, anchor, head)) || return nothing
    InsertionPoint{Instruction}(API.LLVMBasicBlockAsValue(bb[]), anchor[], Bool(head[]))
end

"""
    move!(inst::Instruction, pos::InsertionPoint{Instruction}; debug_records=false)
    move!(bb::BasicBlock, pos::InsertionPoint{BasicBlock})
    move!(f::LLVM.Function, pos::InsertionPoint{LLVM.Function})
    move!(gv::GlobalVariable, pos::InsertionPoint{GlobalVariable})

Move an object to the given insertion point, and return it. Objects that are not part of a
list (e.g., an instruction created with `copy`, or a block removed with `remove!`) are
inserted there. Instructions and blocks can be moved to another block or function,
while functions and global variables can only be moved within their module.

Instructions keep their debug location. By default, the debug records that are attached to
an instruction (the ones right before it) stay where they were, like with LLVM's
`moveBefore`. With `debug_records=true`, they move along with the instruction, like with
`moveBeforePreserving`, and the instruction is inserted before the debug records at the
insertion point, which remain attached to the instruction they precede. This only makes a
difference on LLVM 19 and later, which represents debug information as records.

An instruction can not be moved to the end of a block that has a terminator, use
`LLVM.before(bb.terminator)` instead.

It is up to the caller to keep the IR valid: moving an object does not update branches,
PHI nodes or debug scopes, and does not check that values still dominate their uses.
"""
function move!(inst::Instruction, pos::InsertionPoint{Instruction};
               debug_records::Bool=false)
    bb = check_valid(pos)
    if pos.anchor == C_NULL
        term = API.LLVMGetBasicBlockTerminator(bb)
        term == C_NULL || term == propref(inst) ||
            throw(ArgumentError("Cannot insert an instruction after the terminator of a basic block"))
    end
    API.LLVMGetValueContext(inst) == API.LLVMGetValueContext(bb) ||
        throw(ArgumentError("Cannot move an instruction to another context"))
    if debug_records
        API.LLVMExtraMoveInstructionPreserving(inst, bb, pos.anchor, pos.head)
    else
        API.LLVMExtraMoveInstruction(inst, bb, pos.anchor, pos.head)
    end
    return inst
end


"""
    Instruction(ce::ConstantExpr)

Create a new instruction that computes the same value as the constant expression `ce`, from
the same operands, like C++'s `ConstantExpr::getAsInstruction`. For example, a
`getelementptr` constant expression becomes a `getelementptr` instruction. The instruction
is not part of a basic block: insert it with [`move!`](@ref), or delete it with
[`erase!`](@ref). The constant expression is not changed: replace its uses by the
instruction where needed, e.g., with `replace_uses!` or by assigning to the operands of the
users.
"""
Instruction(ce::ConstantExpr) =
    Instruction(API.LLVMExtraConstantExprGetAsInstruction(ce))

"""
    Instruction(pos::InsertionPoint{Instruction}, ce::ConstantExpr, name::AbstractString="")

Create a new instruction that computes the same value as the constant expression `ce`, like
[`Instruction(::ConstantExpr)`](@ref), insert it at `pos` with the given name, and return
it, e.g., `Instruction(LLVM.before(user), ce)`.
"""
function Instruction(pos::InsertionPoint{Instruction}, ce::ConstantExpr,
                     name::AbstractString="")
    inst = insert_new!(pos, Instruction(ce))
    isempty(name) || name!(inst, name)
    return inst
end

# insert a new instruction, deleting it if that fails
function insert_new!(pos::InsertionPoint{Instruction}, inst::Instruction)
    try
        move!(inst, pos)
    catch
        API.LLVMDeleteInstruction(inst)
        rethrow()
    end
end


## creating instructions without a builder

# the instruction that LLVMExtra created, or NULL if the operands were invalid
function created_instruction(ref::API.LLVMValueRef, what::String, operands)
    ref == C_NULL &&
        throw(ArgumentError("Invalid operands for $what: " * join(map(string, operands), ", ")))
    return Instruction(ref)
end

"""
    LLVM.SelectInst([pos::InsertionPoint{Instruction},] cond::Value, a::Value, b::Value,
                    name::AbstractString="")

Create a `select` instruction that chooses between `a` and `b` based on `cond`, like C++'s
`SelectInst::Create`. Unlike [`select!`](@ref), this always creates an instruction, also
when the operands are constants, and the instruction gets no debug location.

Without an insertion point, the instruction is not part of a basic block: insert it with
[`move!`](@ref), or delete it with [`erase!`](@ref). With one, it is inserted there, e.g.,
`SelectInst(LLVM.before(inst), cond, a, b)`.

The other instructions that can be created this way are
[`ExtractElementInst`](@ref LLVM.ExtractElementInst(::LLVM.Value, ::LLVM.Value)),
[`ExtractValueInst`](@ref LLVM.ExtractValueInst(::LLVM.Value, ::Integer)) and the
[casts](@ref LLVM.AddrSpaceCastInst(::LLVM.Value, ::LLVM.LLVMType, ::AbstractString)).
"""
SelectInst(cond::Value, a::Value, b::Value, name::AbstractString="") =
    created_instruction(API.LLVMExtraCreateSelect(cond, a, b, name), "select",
                        (cond, a, b))::SelectInst

"""
    LLVM.ExtractElementInst([pos::InsertionPoint{Instruction},] vec::Value, idx::Value,
                            name::AbstractString="")

Create an `extractelement` instruction that extracts element `idx` of the vector `vec`,
like C++'s `ExtractElementInst::Create`. Unlike [`extract_element!`](@ref), this always
creates an instruction; see [`LLVM.SelectInst`](@ref LLVM.SelectInst(::LLVM.Value,
::LLVM.Value, ::LLVM.Value)).
"""
ExtractElementInst(vec::Value, idx::Value, name::AbstractString="") =
    created_instruction(API.LLVMExtraCreateExtractElement(vec, idx, name),
                        "extractelement", (vec, idx))::ExtractElementInst

"""
    LLVM.ExtractValueInst([pos::InsertionPoint{Instruction},] agg::Value, index::Integer,
                          name::AbstractString="")
    LLVM.ExtractValueInst([pos::InsertionPoint{Instruction},] agg::Value,
                          indices::AbstractVector{<:Integer}, name::AbstractString="")

Create an `extractvalue` instruction that extracts an element of the aggregate `agg`, like
C++'s `ExtractValueInst::Create`. The zero-based indices select the element, like for
[`extract_value!`](@ref), but need to be non-empty. Unlike `extract_value!`, this always
creates an instruction; see [`LLVM.SelectInst`](@ref LLVM.SelectInst(::LLVM.Value,
::LLVM.Value, ::LLVM.Value)).
"""
ExtractValueInst(agg::Value, index::Integer, name::AbstractString="") =
    ExtractValueInst(agg, [index], name)
function ExtractValueInst(agg::Value, indices::AbstractVector{<:Integer},
                          name::AbstractString="")
    name = api_string(name)
    isempty(indices) &&
        throw(ArgumentError("An extractvalue instruction needs at least one index"))
    # LLVM checks that the indices are in bounds
    idxs = Cuint[0 <= i <= typemax(Cuint) ? Cuint(i) :
                 throw(ArgumentError("Invalid extractvalue index $i")) for i in indices]
    created_instruction(API.LLVMExtraCreateExtractValue(agg, idxs, length(idxs), name),
                        "extractvalue", (agg,))::ExtractValueInst
end

const cast_opcodes = (:Trunc, :ZExt, :SExt, :FPToUI, :FPToSI, :UIToFP, :SIToFP, :FPTrunc,
                      :FPExt, :PtrToInt, :PtrToAddr, :IntToPtr, :BitCast, :AddrSpaceCast)

for op in cast_opcodes
    T = Symbol(op, :Inst)
    mnemonic = lowercase(string(op))
    check = op === :PtrToAddr ?
        :(version() >= v"22" || throw(ArgumentError("ptrtoaddr requires LLVM 22 or later"))) :
        nothing
    @eval function $T(val::Value, ty::LLVMType, name::AbstractString="")
        $check
        created_instruction(API.LLVMExtraCreateCast(API.$(Symbol(:LLVM, op)), val, ty, name),
                            $mnemonic, (val, ty))::$T
    end
end

@doc """
    LLVM.AddrSpaceCastInst([pos::InsertionPoint{Instruction},] val::Value, ty::LLVMType,
                           name::AbstractString="")

Create an `addrspacecast` instruction that casts `val` to the type `ty`, like C++'s
`CastInst::Create`. Unlike [`addrspacecast!`](@ref), this always creates an instruction;
see [`LLVM.SelectInst`](@ref LLVM.SelectInst(::LLVM.Value, ::LLVM.Value, ::LLVM.Value)).

The other casts are created the same way: `LLVM.TruncInst`, `LLVM.ZExtInst`,
`LLVM.SExtInst`, `LLVM.FPToUIInst`, `LLVM.FPToSIInst`, `LLVM.UIToFPInst`,
`LLVM.SIToFPInst`, `LLVM.FPTruncInst`, `LLVM.FPExtInst`, `LLVM.PtrToIntInst`,
`LLVM.PtrToAddrInst` (LLVM 22+), `LLVM.IntToPtrInst` and `LLVM.BitCastInst`.
""" AddrSpaceCastInst(::Value, ::LLVMType, ::AbstractString)

for T in (:SelectInst, :ExtractElementInst, :ExtractValueInst,
          (Symbol(op, :Inst) for op in cast_opcodes)...)
    @eval $T(pos::InsertionPoint{Instruction}, args...) = insert_new!(pos, $T(args...))
end


## basic blocks

function before(bb::BasicBlock)
    f = API.LLVMGetBasicBlockParent(bb)
    f == C_NULL && throw(ArgumentError("Basic block is not part of a function"))
    InsertionPoint{BasicBlock}(f, propref(bb), false)
end

function after(bb::BasicBlock)
    f = API.LLVMGetBasicBlockParent(bb)
    f == C_NULL && throw(ArgumentError("Basic block is not part of a function"))
    next = API.LLVMGetNextBasicBlock(bb)
    InsertionPoint{BasicBlock}(f, next == C_NULL ? C_NULL : API.LLVMBasicBlockAsValue(next),
                               false)
end

function at_begin(f::Function)
    first = API.LLVMGetFirstBasicBlock(f)
    InsertionPoint{BasicBlock}(propref(f),
                               first == C_NULL ? C_NULL : API.LLVMBasicBlockAsValue(first),
                               false)
end

at_end(f::Function) = InsertionPoint{BasicBlock}(propref(f), C_NULL, false)

function move!(bb::BasicBlock, pos::InsertionPoint{BasicBlock})
    f = check_valid(pos)
    API.LLVMGetValueContext(bb) == API.LLVMGetValueContext(f) ||
        throw(ArgumentError("Cannot move a basic block to another context"))
    anchor = pos.anchor == C_NULL ? C_NULL : API.LLVMValueAsBasicBlock(pos.anchor)
    API.LLVMExtraMoveBasicBlock(bb, f, anchor)
    return bb
end

"""
    BasicBlock(pos::InsertionPoint{BasicBlock}, name::AbstractString)

Create a new, empty basic block with the given name, and insert it at the given position,
e.g., `BasicBlock(LLVM.after(entry), "cont")`.
"""
function BasicBlock(pos::InsertionPoint{BasicBlock}, name::AbstractString)
    f = check_valid(pos)
    if pos.anchor == C_NULL
        BasicBlock(API.LLVMAppendBasicBlockInContext(context(f), f, name))
    else
        BasicBlock(API.LLVMInsertBasicBlockInContext(context(f),
                                                     API.LLVMValueAsBasicBlock(pos.anchor),
                                                     name))
    end
end


## functions and global variables

for (T, first, next, move) in
        ((:Function, :LLVMGetFirstFunction, :LLVMGetNextFunction, :LLVMExtraMoveFunction),
         (:GlobalVariable, :LLVMGetFirstGlobal, :LLVMGetNextGlobal, :LLVMExtraMoveGlobal))
    local kind = T === :Function ? "Function" : "Global variable"
    local set = T === :Function ? :ModuleFunctionSet : :ModuleGlobalSet
    @eval begin
        function before(x::$T)
            mod = API.LLVMGetGlobalParent(x)
            mod == C_NULL && throw(ArgumentError($(kind * " is not part of a module")))
            InsertionPoint{$T}(mod, propref(x), false)
        end

        function after(x::$T)
            mod = API.LLVMGetGlobalParent(x)
            mod == C_NULL && throw(ArgumentError($(kind * " is not part of a module")))
            InsertionPoint{$T}(mod, API.$next(x), false)
        end

        at_begin(iter::$set) =
            InsertionPoint{$T}(Base.unsafe_convert(API.LLVMModuleRef, iter.mod),
                               API.$first(iter.mod), false)
        at_end(iter::$set) =
            InsertionPoint{$T}(Base.unsafe_convert(API.LLVMModuleRef, iter.mod), C_NULL, false)

        function move!(x::$T, pos::InsertionPoint{$T})
            mod = check_valid(pos)
            parent = API.LLVMGetGlobalParent(x)
            parent == C_NULL || parent == Base.unsafe_convert(API.LLVMModuleRef, mod) ||
                throw(ArgumentError($(kind * " can only be moved within its module")))
            API.$move(x, mod, pos.anchor)
            return x
        end
    end
end

@specialize
