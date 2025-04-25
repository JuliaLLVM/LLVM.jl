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

- `parent`: get the parent basic block of the instruction.
- `opcode`: get the opcode of the instruction.
- `remove!`/`erase!`: delete the instruction from its parent basic block, or additionally
  also delete the instruction itself.
- `Instruction(::Instruction)`: clone an instruction


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
    caller = LLVM.Function(mod, "CallSomeFunction", function_type(fun))
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
julia> instr = call!(builder, function_type(fun), fun, LLVM.Value[ parameters(fun)... ]);

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
the instruction. This is done by calling the `debuglocation!` function on the builder:

- `debuglocation!(builder)`: clear the debug location.
- `debuglocation!(builder, ::Metadata)`: set the debug location to a specific metadata.
- `debuglocation!(builder, ::Instruction)`: set the debug location to the same as another
  instruction.


## Atomic instructions

Atomic instructions support a few additional APIs:

- `is_atomic`: check if the instruction is atomic.
- `isweak`/`weak!`: check if the instruction is weak, or set it to be weak.
- `syncscope`/`syncscope!`: get or set the synchronization scope of the instruction to
  a specific `SyncScope`
- `ordering`/`ordering!`: get or set the ordering of the instruction.
- `success_ordering`/`success_ordering!`: get or set the success ordering of an atomic
  compare-and-swap instruction.
- `failure_ordering`/`failure_ordering!`: get or set the failure ordering of an atomic
- `binop`: to get the binary operation of an atomic read-modify-write instruction.


## Call sites instructions

Call site instructions include calls, invokes, and `callbr` instructions. These instruction
types support a few additional APIs:

- `callconv`/`callconv!`: get or set the calling convention of the call site.
- `istailcall`/`istailcall!`: get or set whether the call site is a tail call.
- `called_type`: get the function type of the called value of the call site.
- `called_operand`: get the called value of the call site.
- `arguments`: get the arguments of the call site.

### Operand bundles

Calls can also be associated with operand bundles, which are tagged sets of SSA values that
can be associated with certain LLVM instructions, but cannot be dropped like metadata can.

To inspect the operand bundle of a call site, use the iterator returned by the
`operand_bundles` function on a call site instruction. This iterator returns objects
that support the following APIs:

- `tag`: get the tag of the operand bundle.
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
`isconditional` function, and get or set the condition using the `condition` and
`condition!` functions.

If the terminator is a switch, it's possible to get the default destination using the
`default_dest` function.


## Phi nodes

Phi nodes are used to select a value based on the predecessor of a basic block. It's
possible to inspect, and mutate, the incoming values using the iterator returned by
the `incoming` function, which supports the following APIs:

- `getindex`: get the incoming value at a specific index.
- `push!`: add an incoming value (a value, block tuple) to the phi node.
- `append!`: append multiple incoming values (an array of value, block tuples).


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
optimizations that can be performed on the instruction. These flags can be queried and
set using respectively the `fast_math` and `fast_math!` functions:

```jldoctest
julia> inst = fadd!(builder, parameters(fun)[1], ConstantFP(1f0))
%1 = fadd float %0, 1.000000e+00

julia> fast_math(inst)
(nnan = false, ninf = false, nsz = false, arcp = false, contract = false, afn = false, reassoc = false)

julia> fast_math!(inst; nnan=true)

julia> inst
%1 = fadd nnan float %0, 1.000000e+00
```
