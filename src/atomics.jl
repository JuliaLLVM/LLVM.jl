# expansion of atomic operations

@vocabulary Build atomic_rmw_value!, atomic_cmpxchg_value!, PartwordMask, partword_mask!,
                  extract_masked_value!, insert_masked_value!
@vocabulary IR lower_atomic!, expand_to_cmpxchg!, cast_atomic_to_integer!, expand_partword!

"""
    atomic_rmw_value!(builder::IRBuilder, op::LLVM.AtomicRMWBinOp.T, loaded::Value,
                      val::Value)

Emit the computation of an `atomicrmw` on values in registers: the value that
`atomicrmw op` stores when it loads `loaded` and has operand `val`. This is useful to
implement an `atomicrmw` in terms of other operations, e.g., in a `cmpxchg` loop.

Depending on the operation and the version of LLVM, this can call intrinsics, which are
declared in the module if needed, e.g., `llvm.maxnum` for `fmax`, or `llvm.usub.sat` for
`usub_sat` on LLVM 20 and later.

This uses LLVM's `buildAtomicRMWValue`.
"""
function atomic_rmw_value!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, loaded::Value,
                           val::Value)
    check_available(op)
    Value(API.LLVMExtraBuildAtomicRMWValue(builder, Integer(op), loaded, val))
end

"""
    atomic_cmpxchg_value!(builder::IRBuilder, ptr::Value, cmp::Value, new::Value;
                          align::Integer) -> (loaded, success)

Emit a non-atomic compare-and-exchange: load the value at `ptr`, and store `new` if it is
equal to `cmp`, or store back the loaded value otherwise. Returns the loaded value and
whether it was equal to `cmp`.

This uses LLVM's `buildCmpXchgValue` (on LLVM 20 and later).
"""
function atomic_cmpxchg_value!(builder::IRBuilder, ptr::Value, cmp::Value, new::Value;
                               align::Integer)
    check_alignment(align)
    success = Ref{API.LLVMValueRef}()
    loaded = API.LLVMExtraBuildCmpXchgValue(builder, ptr, cmp, new, align, success)
    return Value(loaded), Value(success[])
end

"""
    lower_atomic!(inst::Union{AtomicRMWInst, AtomicCmpXchgInst}) -> Bool

Replace an `atomicrmw` or `cmpxchg` instruction with non-atomic code that loads the value,
computes the result, and stores it, with the same alignment and volatility. This is only
valid when no competing or asynchronous access or synchronization requirement is lost,
e.g., for thread-private memory. The instruction is erased, so builders positioned at it
need to be repositioned. Volatile cmpxchg lowering can split the block and move following
instructions. The computation can call intrinsics
(see [`atomic_rmw_value!`](@ref)).

This is based on LLVM's `lowerAtomicRMWInst` and `lowerAtomicCmpXchgInst`, which don't
preserve the alignment and volatility.
"""
lower_atomic!(inst::AtomicRMWInst) = API.LLVMExtraLowerAtomicRMWInst(inst) |> Bool
lower_atomic!(inst::AtomicCmpXchgInst) = API.LLVMExtraLowerAtomicCmpXchgInst(inst) |> Bool

"""
    expand_to_cmpxchg!(inst::AtomicRMWInst) -> Bool

Replace an `atomicrmw` with a loop around a `cmpxchg` of the same size, ordering,
synchronization scope and volatility, e.g., for operations that the target does not
support natively. Floating-point and vector values are loaded and compared as integers, so
the expansion only needs integer atomics, and metadata that remains valid for the `cmpxchg`
is copied (see [`copy_atomic_metadata!`](@ref)). The instruction is erased.

This changes the control flow: the block containing the instruction is split, with the
instructions that follow it moving to a new block after the loop. Positions after the
instruction (e.g., of builders, or when iterating the block's instructions) refer to that
new block afterwards, and the computation can call intrinsics (see
[`atomic_rmw_value!`](@ref)).

This is a copy of LLVM's `expandAtomicRMWToCmpXchg`, which is meant for use during code
generation: here, the loop starts with an atomic load (like it does from LLVM 23), so that
the result is also valid IR to optimize.
"""
expand_to_cmpxchg!(inst::AtomicRMWInst) = API.LLVMExtraExpandAtomicRMWToCmpXchg(inst) |> Bool

"""
    cast_atomic_to_integer!(inst::Union{LoadInst, StoreInst, AtomicRMWInst}) -> Instruction

Replace an atomic load, store or `atomicrmw xchg` of a floating-point or pointer value with
one of an integer of the same size, for targets that only support integer atomics.
Returns the new instruction, or `inst` if it already accesses an integer. The size of
pointers is taken from the data layout of the module.

This is a copy of the corresponding functionality of AtomicExpandPass.
"""
function cast_atomic_to_integer!(inst::Union{LoadInst,StoreInst,AtomicRMWInst})
    if inst isa AtomicRMWInst && binop(inst) != API.LLVMAtomicRMWBinOpXchg
        throw(ArgumentError("Only atomicrmw xchg instructions can be cast to an integer type"))
    end
    T = inst isa LoadInst ? value_type(inst) : value_type(inst.value_operand)
    if T isa PointerType
        mod = parent(parent(parent(inst)))
        Bool(API.LLVMExtraIsNonIntegralPointerType(mod, T)) &&
            throw(ArgumentError("cannot cast an atomic access to a non-integral pointer value"))
    end
    Instruction(API.LLVMExtraCastAtomicToInteger(inst))
end

function check_partword(mod::Module, T::LLVMType, ptr::Value, align::Integer,
                        word_size::Integer)
    T isa Union{IntegerType,FloatingPointType} ||
        (T isa VectorType && element_type(T) isa Union{IntegerType,FloatingPointType}) ||
        throw(ArgumentError("partword value type must be an integer, floating point, or fixed vector of those types"))
    issized(T) || throw(ArgumentError("partword value type must have a fixed size"))
    n = storage_size(mod.datalayout, T)
    (n > 0 && ispow2(n)) || throw(ArgumentError("partword value must have a power-of-two byte size"))
    if n < word_size
        align >= n || throw(ArgumentError("partword value must be naturally aligned"))
        if align < word_size && Bool(API.LLVMExtraIsNonIntegralPointerType(mod, value_type(ptr)))
            throw(ArgumentError("partword address has a non-integral pointer representation"))
        end
    end
    return n
end

"""
    PartwordMask

The values needed to access a value that is smaller than a word as part of the word
containing it, as computed by [`partword_mask!`](@ref):

- `word_type`, `value_type`: the types of the word and the value, and `int_value_type`
  the integer type of the same size as the value;
- `aligned_addr`, `aligned_addr_alignment`: the address of the word and its alignment;
- `shift`: the number of bits to shift the word right by to get the value;
- `mask`: the bits of the word that hold the value, and `inv_mask` the others (`nothing`
  if the value is not smaller than a word).
"""
struct PartwordMask
    word_type::LLVMType
    value_type::LLVMType
    int_value_type::LLVMType
    aligned_addr::Value
    aligned_addr_alignment::Int
    shift::Value
    mask::Value
    inv_mask::Union{Nothing,Value}
end

PartwordMask(raw::API.LLVMExtraPartwordMaskValues) =
    PartwordMask(LLVMType(raw.WordType), LLVMType(raw.ValueType), LLVMType(raw.IntValueType),
                 Value(raw.AlignedAddr), raw.AlignedAddrAlignment, Value(raw.ShiftAmt),
                 Value(raw.Mask), raw.InvMask == C_NULL ? nothing : Value(raw.InvMask))

raw_mask(pm::PartwordMask) =
    Ref(API.LLVMExtraPartwordMaskValues(
        pm.word_type.ref, pm.value_type.ref, pm.int_value_type.ref, pm.aligned_addr.ref,
        pm.aligned_addr_alignment, pm.shift.ref, pm.mask.ref,
        pm.inv_mask === nothing ? C_NULL : pm.inv_mask.ref))

"""
    partword_mask!(builder::IRBuilder, T::LLVMType, ptr::Value; align::Integer,
                   word_size::Integer) -> PartwordMask

Emit the computation of where a value of type `T` at `ptr`, with alignment `align`, is
located in the `word_size`-byte word that contains it. This is useful to implement
atomics on values smaller than the target supports: use [`extract_masked_value!`](@ref)
and [`insert_masked_value!`](@ref) to access the value in the word. The result depends on
the data layout of the module (its endianness and index width).

`T` must be an integer, floating-point, or fixed vector of those types, with a positive
power-of-two byte size. Values smaller than `word_size` must be naturally aligned. For
values at least `word_size` bytes wide, the result is an identity mask and no alignment
check is needed.

This is a copy of the partword support of AtomicExpandPass, except that the address of the
word is computed by subtracting the offset of the value from `ptr`, as
`getelementptr i8, ptr, -(ptrtoint(ptr) & (word_size - 1))`, which keeps the provenance of
`ptr` and needs no intrinsic (AtomicExpandPass uses `llvm.ptrmask`, which not every target
supports).
"""
function partword_mask!(builder::IRBuilder, T::LLVMType, ptr::Value; align::Integer,
                        word_size::Integer)
    check_alignment(align)
    check_alignment(word_size)
    mod = parent(parent(insert_block(builder)))
    check_partword(mod, T, ptr, align, word_size)
    raw = Ref{API.LLVMExtraPartwordMaskValues}()
    API.LLVMExtraCreatePartwordMaskValues(builder, T, ptr, align, word_size, raw)
    return PartwordMask(raw[])
end

"""
    extract_masked_value!(builder::IRBuilder, word::Value, mask::PartwordMask)

Extract the value described by `mask` from `word`.
"""
extract_masked_value!(builder::IRBuilder, word::Value, pm::PartwordMask) =
    Value(API.LLVMExtraExtractMaskedValue(builder, word,
                                          raw_mask(pm)))

"""
    insert_masked_value!(builder::IRBuilder, word::Value, val::Value, mask::PartwordMask)

Replace the value described by `mask` in `word` with `val`, returning the new word.
"""
insert_masked_value!(builder::IRBuilder, word::Value, val::Value, pm::PartwordMask) =
    Value(API.LLVMExtraInsertMaskedValue(builder, word, val,
                                         raw_mask(pm)))

"""
    expand_partword!(inst::Union{AtomicRMWInst, AtomicCmpXchgInst}, word_size::Integer) -> Bool

Replace an `atomicrmw` or `cmpxchg` on a value smaller than `word_size` bytes by
operations on the word containing it, for targets that only support atomics of at least
that size. Bitwise operations become an `atomicrmw` on the word; other operations become a
loop around a `cmpxchg` of the word. The instruction is erased, unless the value is not
smaller than `word_size`, in which case this returns `false`. The result depends on the
data layout of the module (see [`partword_mask!`](@ref)).

The rest of the word must be accessible, as it is read and written back (atomically, so
this doesn't affect the other values in the word). A value smaller than `word_size` must
be naturally aligned. Only integer, floating-point and fixed-vector values are supported;
pointer-valued partword operations are unsupported.

Operations that become a `cmpxchg` loop change the control flow like
[`expand_to_cmpxchg!`](@ref): the block containing the instruction is split, and the
instructions that follow it move to a new block. The computation can call intrinsics (see
[`atomic_rmw_value!`](@ref)).

This is a copy of the partword expansion of AtomicExpandPass.
"""
function expand_partword!(inst::AtomicRMWInst, word_size::Integer)
    check_alignment(word_size)
    mod = parent(parent(parent(inst)))
    check_partword(mod, value_type(inst), inst.pointer_operand, alignment(inst), word_size)
    API.LLVMExtraExpandPartwordAtomicRMW(inst, word_size) |> Bool
end
function expand_partword!(inst::AtomicCmpXchgInst, word_size::Integer)
    check_alignment(word_size)
    mod = parent(parent(parent(inst)))
    check_partword(mod, value_type(inst.compare_operand), inst.pointer_operand,
                   alignment(inst), word_size)
    API.LLVMExtraExpandPartwordCmpXchg(inst, word_size) |> Bool
end
