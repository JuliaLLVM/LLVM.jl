# Analyses

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

LLVM supports many analyses, but only few are available through the C API, and thus
available in LLVM.jl.


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


## Dominator and post-dominator

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

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
