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
```

## Assumptions and value tracking

```@docs
AssumptionCache
AssumptionEntry
ConstantRange(::LLVM.Value)
KnownBits(::LLVM.Value)
is_valid_assume_for_context
is_guaranteed_not_to_be_poison
program_undefined_if_poison
```

## Lazy value info

```@docs
LazyValueInfo
```
