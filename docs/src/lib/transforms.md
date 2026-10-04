# Transforms

## Pass builders

```@docs
PassBuilder
run!
PassException
```

## Pass managers

```@docs
LLVM.PassManager
add!(::LLVM.AbstractPassManager, ::Any)
```

## Passes and pipelines

The functions that return the names of LLVM's passes are listed on the [Passes](passes.md)
page.

```@docs
DefaultPipeline
```

## Custom passes

```@docs
LLVM.CustomPass
register!
run!(::LLVM.CustomPass, ::Union{LLVM.Function, LLVM.Module}, ::Union{Nothing, LLVM.TargetMachine})
register_callbacks!
```

### Analyses in custom passes

```@docs
FunctionAnalysisManager
PreservedAnalyses
AllAnalyses
invalidate!
```

## Custom target info

```@docs
LLVM.AbstractTargetTransformInfo
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
