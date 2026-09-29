# Instructions

```@docs
Instruction
copy(::Instruction)
remove!(::Instruction)
erase!(::Instruction)
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

## Attributes

```@docs
function_attributes(::LLVM.CallBase)
argument_attributes(::LLVM.CallBase, ::Integer)
return_attributes(::LLVM.CallBase)
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

```@docs
arguments
```

### Operand Bundles

```@docs
OperandBundle
operand_bundles
inputs
```

## Terminator instructions

```@docs
isterminator
isconditional
case_value
case_value!
successors(::Instruction)
```

## Phi instructions

```@docs
incoming
```

## Floating Point instructions

```@docs
LLVM.FPMathInst
```

```@docs
FastMathFlags
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
