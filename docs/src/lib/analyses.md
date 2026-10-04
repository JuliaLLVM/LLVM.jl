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
