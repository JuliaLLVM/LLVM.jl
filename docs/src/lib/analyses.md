# Analyses

## IR verification

```@docs
verify
verification_error
```

## Dominator and post-dominator

```@docs
DomTree
dispose(::DomTree)
PostDomTree
dispose(::PostDomTree)
dominates
recalculate!
split_block!
split_block_and_insert_if_then!
split_block_and_insert_if_then_else!
```

## Constant folding and simplification

```@docs
constant_fold_cast
simplify_instruction
```

## Constant ranges and known bits

```@docs
ConstantRange
isfullset
iswrappedset
intersect_with
binary_op
cast_op
allowed_icmp_region
KnownBits
ConstantRange(::KnownBits)
ConstantRange(::MDNode)
```

## Assumptions and value tracking

```@docs
AssumptionCache
AssumptionEntry
ConstantRange(::LLVM.Value)
KnownBits(::LLVM.Value)
is_valid_assume_for_context
is_guaranteed_not_to_be_poison
is_guaranteed_not_to_be_undef_or_poison
program_undefined_if_poison
is_known_non_negative
num_sign_bits
```

## Lazy value info

```@docs
LazyValueInfo
```

## Loops and scalar evolution

```@docs
LoopInfo
Loop
ScalarEvolution
SCEV
is_scevable
scev_add
scev_mul
scev_cast
contains_scev
ConstantRange(::ScalarEvolution, ::SCEV)
is_loop_invariant
backedge_taken_count
step_recurrence
reverse_postorder
```

Passes that change loops or values keep scalar evolution up to date by making it forget
what changed:

```@docs
forget!
forget_loop_dispositions!
```

## Speculation

```@docs
is_safe_to_speculatively_execute
```

## Memory SSA

```@docs
MemorySSA
```
