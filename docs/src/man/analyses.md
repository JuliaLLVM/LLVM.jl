# Analyses

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.Analysis, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

LLVM.jl provides access to several of LLVM's analyses, which are part of the
`LLVM.Analysis` vocabulary. Most of them are available to custom passes, through the
analysis manager of a pass pipeline (see [`FunctionAnalysisManager`](@ref)).


## IR verification

IR contained in modules and functions can be verified using the `verify` function,
throwing a Julia exception when the IR is invalid:

```jldoctest
julia> mod = parse(LLVM.Module,  """
         define i32 @example(i1 %cond, i32 %val) {
         entry:
           br i1 %cond, label %foo, label %bar
         foo:
           %ret = add i32 %val, 1
           br label %bar
         bar:
           ret i32 %ret
         }""");

julia> verify(mod)
ERROR: LLVM error: Instruction does not dominate all uses!
  %ret = add i32 %val, 1
  ret i32 %ret
```

Functions can be verified in the same way, using `verify(f)`. To handle invalid IR without
catching an exception, e.g., to report it with more context, use `verification_error`,
which returns the verifier's message, or `nothing` if the IR is valid.


## Dominator and post-dominator

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.Analysis, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
      define i32 @example(i1 %cond, i32 %val) {
      entry:
        br i1 %cond, label %foo, label %bar
      foo:
        %ret = add i32 %val, 1
        br label %bar
      bar:
        ret i32 %ret
      }"""
    mod = parse(LLVM.Module, ir)
    fun = only(mod.functions)
    entry, foo, bar = fun.blocks
end
```

Dominator and post-dominator analyses can be performed on functions by constructing
respectively a `DomTree` and `PostDomTree` object, and using the `dominates` function:

```jldoctest
julia> fun
define i32 @example(i1 %cond, i32 %val) {
entry:
  br i1 %cond, label %foo, label %bar

foo:                                              ; preds = %entry
  %ret = add i32 %val, 1
  br label %bar

bar:                                              ; preds = %foo, %entry
  ret i32 %ret
}

julia> tree = DomTree(fun);

julia> dominates(tree, first(entry.instructions), first(foo.instructions))
true
julia> dominates(tree, first(foo.instructions), first(bar.instructions))
false

julia> tree = PostDomTree(fun);

julia> dominates(tree, first(bar.instructions), first(foo.instructions))
true

julia> dominates(tree, first(foo.instructions), first(entry.instructions))
false
```


## Constant ranges and known bits

Many analyses describe the possible values of integers as a range, or as the bits that are
known to be zero or one. These are represented by the `ConstantRange` and `KnownBits`
values, which can also be used on their own. Computations on them are performed by LLVM:

```jldoctest
julia> r = ConstantRange(64, 0, 100)
ConstantRange(64, 0, 100)

julia> r.unsigned_max
0x0000000000000063

julia> r * ConstantRange(64, 4) + ConstantRange(64, 1)
ConstantRange(64, 1, 398)

julia> intersect_with(r, ConstantRange(64, 50, 200))
ConstantRange(64, 50, 100)

julia> ConstantRange(KnownBits(64, ~UInt64(0xff), 0))
ConstantRange(64, 0, 256)
```

The range and known bits of an integer value can be computed using LLVM's value tracking,
by passing the value to the `ConstantRange` or `KnownBits` constructor. To also use the
assumptions (`llvm.assume` calls) that hold at a certain point, pass the instruction at
that point together with the function's assumption cache and dominator tree, which are
typically obtained from the analysis manager of a custom pass:

```julia
function my_pass!(f::LLVM.Function, am)
    ac, dt = am[AssumptionCache], am[DomTree]
    for bb in f.blocks, inst in bb.instructions
        inst isa GetElementPtrInst || continue
        idx = last(inst.operands)
        r = ConstantRange(idx; at=inst, assumptions=ac, domtree=dt)
        # ...
    end
    return false
end
```
