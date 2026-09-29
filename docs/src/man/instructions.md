# Instructions

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Instructions represent the operations that are executed by the program. They are grouped in
basic blocks, and are available as the `instructions` property of a block. To create
instructions, an instruction builder is used.

The abstract `LLVM.Instruction` type supports a few additional APIs on top of the
functionality from `User` and `Value`:

- `inst.parent`: the parent basic block of the instruction, or `nothing` if it is detached.
- `inst.opcode`: the opcode of the instruction.
- `inst.debug_location`: the debug location of the instruction, or `nothing`.
- `inst.alignment`: the alignment of memory instructions (`alloca`, `load`, `store`,
  `atomicrmw` and `cmpxchg`).
- `remove!`/`erase!`: delete the instruction from its parent basic block, or additionally
  also delete the instruction itself.
- `copy(inst)`: clone an instruction


## Creating instructions

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()))
    bb = BasicBlock(fun, "entry")
end
```

Instructions are created using an `IRBuilder`. This object is first positioned, and then used
to create instructions by calling specific functions.

To position an `IRBuilder`, several APIs are available:

- `position`: get the basic block where the builder is currently positioned.
- `position!(builder, ::Instruction)`: position the builder before an instruction.
- `position!(builder, ::Instruction; after=true)`: position the builder after an
  instruction, which is at the end of its basic block if it is the last instruction.
- `position!(builder, ::BasicBlock)`: position the builder at the end of a basic block.
- `position!(builder)`: clear the position of the builder.

Given a pre-created `Instruction`, or more commonly an instruction that has been `delete!`d
from a basic block, it is possible to insert it back into a different basic block by
calling the `insert!` function.

The essential functionality of the `IRBuilder` is the ability to create instructions. This
is done by calling specific functions:

```jldoctest
julia> builder = IRBuilder();

julia> position!(builder, bb)

julia> ret!(builder);

julia> bb
entry:
  ret void
```

For a full list of functions that can be used to create instructions, consult the API
reference.

Most of these functions correspond to a function of the C API, e.g., `add!` builds an
`add` instruction using `LLVMBuildAdd`. Some also support functionality that C++'s
`IRBuilder` offers, like accessing nested elements of an aggregate using a vector of
(zero-based) indices, as in textual IR:

```jldoctest
julia> typ = LLVM.StructType([LLVM.Int32Type(), LLVM.ArrayType(LLVM.Int8Type(), 4)]);

julia> f = LLVM.Function(mod, "extract", LLVM.FunctionType(LLVM.Int8Type(), [typ]));

julia> builder = IRBuilder();

julia> position!(builder, BasicBlock(f, "entry"))

julia> ev = extract_value!(builder, f.parameters[1], [1, 2])
%1 = extractvalue { i32, [4 x i8] } %0, 1, 2

julia> ev.indices == [1, 2]
true
```

### Attributes

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type()]))
    push!(fun.function_attributes, EnumAttribute(:nounwind))
    push!(fun.parameter_attributes[1], EnumAttribute(:noundef))
    push!(fun.return_attributes, EnumAttribute(:noundef))
    caller = LLVM.Function(mod, "CallSomeFunction", fun.function_type)
    top = BasicBlock(caller, "top")
    builder = LLVM.IRBuilder();
    position!(builder, top)
end
```

Call and invoke instructions can have attributes just like functions. They can be set and
retrieved using the views returned by the `function_attributes`, `argument_attributes` and
`return_attributes` properties, to respectively set attributes on the instruction, its
arguments and its return value:

```jldoctest function
julia> instr = call!(builder, fun.function_type, fun, LLVM.Value[ fun.parameters... ]);

julia> push!(instr.function_attributes, EnumAttribute(:nounwind));

julia> push!(instr.argument_attributes[1], EnumAttribute(:noundef));

julia> push!(instr.return_attributes, EnumAttribute(:noundef));

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"

; Function Attrs: nounwind
declare noundef i32 @SomeFunction(i32 noundef) #0

define i32 @CallSomeFunction(i32 %0) {
top:
  %1 = call noundef i32 @SomeFunction(i32 noundef %0) #0
}

attributes #0 = { nounwind }
```

Like the attributes of functions, these views can be indexed by the kind of an attribute
(e.g., `haskey(instr.function_attributes, :nounwind)`), and attributes can be removed using
`delete!`. They only contain the attributes of the call site, not those of the called
function.

### Debug location

When creating instructions with an `IRBuilder`, it is possible to set a debug location for
the instructions it creates by assigning to the `debug_location` property of the builder
(assign `nothing` to clear it). Instructions have a `debug_location` property too, so an
existing instruction can be given the builder's current debug location using
`inst.debug_location = builder.debug_location`.


## Memory instructions

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()))
    bb = BasicBlock(fun, "entry")
    builder = IRBuilder()
    position!(builder, bb)
end
```

Stack allocations and memory accesses (loads, stores, and atomic read-modify-write and
compare-and-exchange instructions) have an alignment, which can be specified using the
`align` keyword argument when building the instruction, and inspected or changed afterwards
using the `alignment` property:

```jldoctest
julia> slot = alloca!(builder, LLVM.Int64Type(); align=16)
%0 = alloca i64, align 16

julia> slot.alignment = 32;

julia> Int(slot.alignment)
32
```

Memory accesses can also be marked volatile, using the `inst.volatile` property or the
`volatile` keyword argument when building the instruction.

The operands of memory instructions are available as properties too, so that code that
inspects or rewrites them doesn't need to know their position in `inst.operands`:

- `inst.pointer_operand`: the address that a load, store, `atomicrmw` or `cmpxchg`
  instruction accesses, or that a `getelementptr` instruction indexes into.
- `inst.value_operand`: the value that a store or `atomicrmw` instruction writes.
- `alloca.allocated_type`: the type that an `alloca` instruction allocates.
- `gep.source_element_type`: the type that a `getelementptr` instruction indexes into.
- `gep.inbounds`: whether a `getelementptr` instruction is `inbounds`, which can also be
  assigned to.

```jldoctest
julia> slot = alloca!(builder, LLVM.Int64Type());

julia> store = store!(builder, ConstantInt(Int64(1)), slot)
store i64 1, ptr %0, align 4

julia> store.pointer_operand == slot
true

julia> store.value_operand
i64 1

julia> slot.allocated_type
i64
```


## Atomic instructions

Atomic instructions support a few additional APIs:

- `isatomic`: check if the instruction is atomic.
- `cmpxchg.weak`: whether a compare-and-swap instruction is weak, i.e., may fail
  spuriously.
- `inst.syncscope`: the synchronization scope of the instruction, a `SyncScope`.
- `inst.ordering`: the ordering of the instruction.
- `inst.success_ordering`, `inst.failure_ordering`: the success and failure orderings of an
  atomic compare-and-swap instruction.
- `inst.binop`: the binary operation of an atomic read-modify-write instruction.

All of these properties, except for `binop`, can also be assigned to.


## Call sites instructions

Call site instructions include calls, invokes, and `callbr` instructions. These instruction
types support a few additional APIs:

- `call.callconv`: the calling convention of the call site.
- `call.tailcall`: whether a `call` instruction is a tail call, i.e., is marked `tail` or
  `musttail`.
- `call.tailcall_kind`: the tail call marker of a `call` instruction, e.g.,
  `LLVM.API.LLVMTailCallKindMustTail`.
- `call.called_type`: the function type of the called value of the call site.
- `call.called_operand`: the called value of the call site, which can be any value (e.g.,
  a function pointer). Assigning to it replaces the callee, but keeps the function type,
  arguments and attributes of the call.
- `call.called_function`: the function that is called directly, or `nothing` (e.g., for
  calls of a function pointer). Like C++'s `CallBase::getCalledFunction`, this does not
  look through casts.
- `call.arguments`: the arguments of the call site, as a mutable view.

To check whether a call calls a specific intrinsic, pass its callee to `isintrinsic`, e.g.,
`isintrinsic(call.called_operand, Intrinsic("llvm.memcpy"))`.

### Operand bundles

Calls can also be associated with operand bundles, which are tagged sets of SSA values that
can be associated with certain LLVM instructions, but cannot be dropped like metadata can.

To inspect the operand bundles of a call site, use its `operand_bundles` property, a
read-only view. The operand bundles themselves are copies that support the following APIs:

- `bundle.tag`: the tag of the operand bundle.
- `bundle.inputs`: the inputs of the operand bundle.

Operand bundles can also be created directly, using the `OperandBundle` constructor:

```jldoctest
julia> OperandBundle("deopt")
"deopt"()

julia> OperandBundle("deopt", [LLVM.ConstantInt(1)])
"deopt"(i64 1)
```

Whether constructed directly or looked up from a call site, operand bundles can be attached
to a call site when calling the `call!` function on an `IRBuilder`.


## Terminator instructions

Terminator instructions are the last instructions in a basic block, and are used to control
the flow of execution. They support a few additional APIs:

- `isterminator`: check if the instruction is a terminator.
- `term.successors`: the successors of the terminator, as a mutable view.

If the terminator is a branch, it's possible to check if the branch is conditional using the
`isconditional` function, and get or set the condition using the `condition` property.

If the terminator is a switch, it's possible to get the default destination using the
`default_dest` property, and to get or set the value of each case using the `case_values`
property, a mutable view (`switch.case_values[i]` is the value of the case that branches to
`switch.successors[i+1]`).


## Phi nodes

Phi nodes are used to select a value based on the predecessor of a basic block. It's
possible to inspect, and mutate, the incoming values using the view returned by the
`incoming` property, which supports the following APIs:

- `getindex`: get the incoming value at a specific index.
- `push!`: add an incoming value (a value, block tuple) to the phi node.
- `append!`: append multiple incoming values (an array of value, block tuples).


## Poison-generating flags

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int32Type()]))
    bb = BasicBlock(fun, "entry")
    builder = IRBuilder();
    position!(builder, bb)
end
```

Several integer instructions can carry flags that make the result poison when an assumption
about the operands does not hold, which enables more aggressive optimization. Each flag is
a `Bool` property, which only exists on the instructions that support the flag:

- `inst.nuw` and `inst.nsw`: no unsigned or signed wrap, for `add`, `sub`, `mul`, `shl` and
  (on LLVM 19+) `trunc`;
- `inst.exact`: for `udiv`, `sdiv`, `lshr` and `ashr`;
- `inst.disjoint`: for `or` (LLVM 18+);
- `inst.nneg`: non-negative operand, for `zext` (LLVM 18+) and `uitofp` (LLVM 19+);
- `inst.samesign`: operands of equal sign, for `icmp` (LLVM 20+).

```jldoctest
julia> x, y = fun.parameters;

julia> inst = add!(builder, x, y)
%2 = add i32 %0, %1

julia> inst.nuw = true;

julia> inst.nuw, inst.nsw
(true, false)

julia> inst
%2 = add nuw i32 %0, %1
```


## Fast math flags

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType(), [LLVM.FloatType()]))
    bb = BasicBlock(fun, "entry")
    builder = IRBuilder();
    position!(builder, bb)
end
```

Floating-point instructions can be configured with different fast math flags, affecting
optimizations that can be performed on the instruction. The `fast_math` property returns
a `FastMathFlags` view of these flags, with a `Bool` property per flag that can be read and
assigned to:

```jldoctest
julia> inst = fadd!(builder, fun.parameters[1], ConstantFP(1f0))
%1 = fadd float %0, 1.000000e+00

julia> inst.fast_math
FastMathFlags()

julia> inst.fast_math.nnan = true;

julia> inst
%1 = fadd nnan float %0, 1.000000e+00
```

Assigning to the `fast_math` property replaces all flags, clearing the ones that are not
specified. The `fast` pseudo-flag stands for all flags:

```jldoctest
julia> inst = fadd!(builder, fun.parameters[1], ConstantFP(1f0));

julia> inst.fast_math = (; ninf=true, nsz=true);

julia> inst.fast_math
FastMathFlags(ninf=true, nsz=true)

julia> inst.fast_math.fast = true;

julia> inst
%1 = fadd fast float %0, 1.000000e+00

julia> NamedTuple(inst.fast_math)
(nnan = true, ninf = true, nsz = true, arcp = true, contract = true, afn = true, reassoc = true)
```
