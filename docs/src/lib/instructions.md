# Instructions

```@docs
Instruction
copy(::Instruction)
remove!(::Instruction)
erase!(::Instruction)
LLVM.parent(::Instruction)
```

## Creating instructions

```@docs
IRBuilder
IRBuilder()
dispose(::IRBuilder)
context(::IRBuilder)
position
position!(::IRBuilder, ::Instruction)
position!(::IRBuilder, ::BasicBlock)
position!(::IRBuilder)
insert!(::IRBuilder, ::Instruction, ::String)
debuglocation
debuglocation!
```

## Attributes

```@docs
function_attributes(::LLVM.CallBase)
argument_attributes(::LLVM.CallBase, ::Integer)
return_attributes(::LLVM.CallBase)
```

## Comparison instructions

```@docs
predicate
```

## Atomic instructions

```@docs
is_atomic
ordering
ordering!
SyncScope
syncscope
syncscope!
binop
isweak
weak!
success_ordering
success_ordering!
failure_ordering
failure_ordering!
```

## Call instructions

```@docs
callconv(::LLVM.CallBase)
callconv!(::LLVM.CallBase, ::Any)
istailcall
tailcall!
called_operand
arguments
called_type
```

### Operand Bundles

```@docs
OperandBundle
operand_bundles
tag
inputs
```

## Terminator instructions

```@docs
isterminator
isconditional
condition
condition!
default_dest
successors(::Instruction)
```

## Phi instructions

```@docs
incoming
```

## Floating Point instructions

```@docs
fast_math
fast_math!
```

## Alignment

```@docs
alignment
alignment!
```
