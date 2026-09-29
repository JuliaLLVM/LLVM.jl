# Instructions

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Instructions represent the operations that are executed by the program. They are grouped in
basic blocks, and can be iterated using the `instructions` function. To create instructions,
an instruction builder is used.

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
    using LLVM

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

### Attributes

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
    fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()]))
    push!(function_attributes(fun), StringAttribute("nounwind"))
    push!(parameter_attributes(fun, 1), StringAttribute("nocapture"))
    push!(return_attributes(fun), StringAttribute("sret"))
    caller = LLVM.Function(mod, "CallSomeFunction", fun.function_type)
    top = BasicBlock(caller, "top")
    builder = LLVM.IRBuilder();
    position!(builder, top)
end
```

Call and invoke instructions can have attributes just like functions.
They can be set and retrieved using the iterators returned by the
`function_attributes`, `argument_attributes` and `return_attributes` functions
to respectively set attributes on the instructions, its arguments and its return value:

```jldoctest function
julia> instr = call!(builder, fun.function_type, fun, LLVM.Value[ parameters(fun)... ]);

julia> push!(function_attributes(instr), StringAttribute("nounwind"))

julia> push!(argument_attributes(instr, 1), StringAttribute("nocapture"))

julia> push!(return_attributes(instr), StringAttribute("sret"))

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"

declare "sret" void @SomeFunction(i32 "nocapture") #0

define void @CallSomeFunction(i32 %0) {
top:
  call "sret" void @SomeFunction(i32 "nocapture" %0) #0
}

attributes #0 = { "nounwind" }
```

### Debug location

When creating instructions with an `IRBuilder`, it is possible to set a debug location for
the instructions it creates by assigning to the `debug_location` property of the builder
(assign `nothing` to clear it). Instructions have a `debug_location` property too, so an
existing instruction can be given the builder's current debug location using
`inst.debug_location = builder.debug_location`.


## Memory instructions

```@meta
DocTestSetup = quote
    using LLVM

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

Memory accesses can also be marked volatile, using `isvolatile`/`volatile!` or the
`volatile` keyword argument when building the instruction.


## Atomic instructions

Atomic instructions support a few additional APIs:

- `isatomic`: check if the instruction is atomic.
- `isweak`/`weak!`: check if the instruction is weak, or set it to be weak.
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
- `istailcall`/`tailcall!`: get or set whether the call site is a tail call.
- `call.called_type`: the function type of the called value of the call site.
- `call.called_operand`: the called value of the call site.
- `arguments`: get the arguments of the call site.

### Operand bundles

Calls can also be associated with operand bundles, which are tagged sets of SSA values that
can be associated with certain LLVM instructions, but cannot be dropped like metadata can.

To inspect the operand bundle of a call site, use the iterator returned by the
`operand_bundles` function on a call site instruction. This iterator returns objects
that support the following APIs:

- `bundle.tag`: the tag of the operand bundle.
- `inputs`: get the inputs of the operand bundle, which itself is an iterator that can be
  indexed.

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
- `successors`: get the successors of the terminator.

If the terminator is a branch, it's possible to check if the branch is conditional using the
`isconditional` function, and get or set the condition using the `condition` property.

If the terminator is a switch, it's possible to get the default destination using the
`default_dest` property, and to get or set the value of each case using `case_value` and
`case_value!`.


## Phi nodes

Phi nodes are used to select a value based on the predecessor of a basic block. It's
possible to inspect, and mutate, the incoming values using the iterator returned by
the `incoming` function, which supports the following APIs:

- `getindex`: get the incoming value at a specific index.
- `push!`: add an incoming value (a value, block tuple) to the phi node.
- `append!`: append multiple incoming values (an array of value, block tuples).


## Poison-generating flags

```@meta
DocTestSetup = quote
    using LLVM

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
about the operands does not hold, which enables more aggressive optimization. Each flag can
be queried and set with a pair of functions, which throw an `ArgumentError` when used with
an instruction that does not support the flag:

- `hasnuw`/`nuw!` and `hasnsw`/`nsw!`: no unsigned or signed wrap, for `add`, `sub`, `mul`,
  `shl` and (on LLVM 19+) `trunc`;
- `isexact`/`exact!`: for `udiv`, `sdiv`, `lshr` and `ashr`;
- `hasdisjoint`/`disjoint!`: for `or` (LLVM 18+);
- `hasnneg`/`nneg!`: non-negative operand, for `zext` (LLVM 18+) and `uitofp` (LLVM 19+);
- `hassamesign`/`samesign!`: operands of equal sign, for `icmp` (LLVM 20+).

```jldoctest
julia> x, y = parameters(fun);

julia> inst = add!(builder, x, y)
%2 = add i32 %0, %1

julia> nuw!(inst, true)

julia> hasnuw(inst), hasnsw(inst)
(true, false)

julia> inst
%2 = add nuw i32 %0, %1
```


## Fast math flags

```@meta
DocTestSetup = quote
    using LLVM

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

Arithmetic instructions can be configured with different fast math flags, affecting
optimizations that can be performed on the instruction. These flags can be queried using
the `fast_math` property, and added using the `fast_math!` function:

```jldoctest
julia> inst = fadd!(builder, parameters(fun)[1], ConstantFP(1f0))
%1 = fadd float %0, 1.000000e+00

julia> inst.fast_math
(nnan = false, ninf = false, nsz = false, arcp = false, contract = false, afn = false, reassoc = false)

julia> fast_math!(inst; nnan=true)

julia> inst
%1 = fadd nnan float %0, 1.000000e+00
```
