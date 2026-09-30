# Instructions

```@docs
Instruction
copy(::Instruction)
remove!(::Instruction)
erase!(::Instruction)
move_before(::Instruction, ::Instruction)
move_after(::Instruction, ::Instruction)
comes_before
may_read_from_memory
may_write_to_memory
may_have_side_effects
```

## Creating instructions

```@docs
IRBuilder
IRBuilder()
dispose(::IRBuilder)
position
position!(::IRBuilder, ::Instruction)
position!(::IRBuilder, ::BasicBlock)
position!(::IRBuilder)
insert!(::IRBuilder, ::Instruction, ::String)
```

## Atomic instructions

```@docs
LLVM.AtomicInst
LLVM.MemAccessInst
```

```@docs
isatomic
SyncScope
LLVM.isavailable
merged_ordering
strongest_failure_ordering
is_stronger
is_acquire_or_stronger
is_release_or_stronger
parse(::Type{LLVM.API.LLVMAtomicOrdering}, ::AbstractString)
parse(::Type{LLVM.API.LLVMAtomicRMWBinOp}, ::AbstractString)
mmra!
copy_atomic_metadata!
```

### Building memory accesses and atomics

```@docs
alloca!
array_alloca!
load!
store!
fence!
atomic_rmw!
atomic_cmpxchg!
```

### Expanding atomics

```@docs
atomic_rmw_value!
atomic_cmpxchg_value!
lower_atomic!
expand_to_cmpxchg!
cast_atomic_to_integer!
expand_partword!
PartwordMask
partword_mask!
extract_masked_value!
insert_masked_value!
```

## Call instructions

```@docs
LLVM.CallBase
```

### Operand Bundles

```@docs
OperandBundle
```

## Terminator instructions

```@docs
LLVM.TerminatorInst
```

```@docs
isterminator
isconditional
```

## Aggregate instructions

```@docs
extract_value!
insert_value!
```

## Floating Point instructions

```@docs
LLVM.FPMathInst
```

```@docs
FastMathFlags
supports_fast_math
```

## Alignment

```@docs
LLVM.AlignedInst
```

## Poison-generating flags

```@docs
LLVM.NoWrapInst
LLVM.ExactInst
LLVM.NonNegInst
```
