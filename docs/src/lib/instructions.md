# Instructions

```@docs
Instruction
copy(::Instruction)
remove!(::Instruction)
erase!(::Instruction)
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
position!(::IRBuilder, ::InsertionPoint{Instruction})
position!(::Function, ::IRBuilder, ::InsertionPoint{Instruction})
position!(::IRBuilder)
```

### Arithmetic and logic

```@docs
add!(::IRBuilder, ::Value, ::Value)
nswadd!
nuwadd!
fadd!
sub!
nswsub!
nuwsub!
fsub!
mul!
nswmul!
nuwmul!
fmul!
udiv!
exactudiv!
sdiv!
exactsdiv!
fdiv!
urem!
srem!
frem!
neg!
nswneg!
fneg!
shl!
lshr!
ashr!
and!
or!
xor!
not!
binop!
```

### Conversions

```@docs
trunc!
zext!
sext!
fptoui!
fptosi!
uitofp!
sitofp!
fptrunc!
fpext!
ptrtoint!
inttoptr!
bitcast!
addrspacecast!
zextorbitcast!
sextorbitcast!
truncorbitcast!
pointercast!
intcast!
fpcast!
cast!
```

### Comparisons and selection

```@docs
icmp!
fcmp!
select!
phi!
isnull!
isnotnull!
```

### Memory

```@docs
gep!
inbounds_gep!
struct_gep!
ptrdiff!
malloc!
array_malloc!
free!
memset!
memcpy!
memmove!
globalstring!
globalstring_ptr!
```

### Vectors

```@docs
extract_element!
insert_element!
shuffle_vector!
```

### Calls and control flow

```@docs
call!
invoke!
ret!
br!
switch!
indirectbr!
unreachable!
resume!
landingpad!
va_arg!
```

The types of the instructions are listed on the [Instruction types](@ref) page.

## Insertion points

```@docs
InsertionPoint
LLVM.before
LLVM.after
LLVM.at_begin
LLVM.at_end
LLVM.after_phis
move!
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
