# Transforms

## Pass builders

```@docs
NewPMPassBuilder
run!
```

## Pass managers

```@docs
LLVM.NewPMPassManager
add!
```

## Alias analyses

```@docs
```

## Custom passes

```@docs
LLVM.NewPMCustomPass
register!
```

## Custom target info

```@docs
AbstractTargetTransformInfo
target_transform_info!
LLVM.flat_address_space
LLVM.has_branch_divergence
LLVM.is_single_threaded
LLVM.is_noop_addr_space_cast
LLVM.is_valid_addr_space_cast
LLVM.addrspaces_may_alias
LLVM.can_have_non_undef_global_initializer_in_address_space
LLVM.is_source_of_divergence
LLVM.is_always_uniform
LLVM.get_assumed_addr_space
LLVM.get_predicated_addr_space
LLVM.rewrite_intrinsic_with_address_space
LLVM.collect_flat_address_operands
```

## IR cloning

```@docs
clone_into!
clone
```
