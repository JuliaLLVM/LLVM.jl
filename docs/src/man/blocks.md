# Basic blocks

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Basic blocks are sequences of instructions that are executed in order. They are the building
blocks of functions, and can be looked up using the `blocks` property of a function, or by
constructing them directly:

```jldoctest
julia> bb = BasicBlock("SomeBlock")
SomeBlock:                                        ; No predecessors!
```

A detached basic block often not what you want; using the `BasicBlock(::Function)`
constructor you can instead append to a function, or insert it at another position using an
insertion point, e.g., `BasicBlock(LLVM.after(entry), "cont")`.

Basic blocks support a couple of specific APIs:

- `bb.name`: the name of the basic block.
- `bb.parent`: the parent function of the basic block, or `nothing` if it is detached.
- `bb.terminator`: the terminator instruction of the block, or `nothing` if it has none.
- `move!(bb, pos)`: move the block to an insertion point, e.g., `LLVM.before(other)` or
  `LLVM.at_end(f)`, also in another function. A detached block is inserted there.
- `remove!`/`erase!`: delete the basic block from its parent function, or additionally also
  delete the block itself.


## Control flow

The control flow between basic blocks can be inspected using the following properties:

- `bb.predecessors`: the blocks that branch to the basic block. This is a read-only view,
  derived from the uses of the block.
- `bb.successors`: the blocks the basic block branches to, i.e., the successors of its
  terminator. This view is mutable: `bb.successors[i] = other` changes the destination of
  the terminator.


## Instructions

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
        define i64 @"add"(i64 %0, i64 %1) {
        top:
          %2 = add i64 %1, %0
          ret i64 %2
        }"""
    mod = parse(LLVM.Module, ir);
    fun = only(mod.functions);
    bb = fun.entry
end
```

The main purpose of basic blocks is to contain instructions, which are available as the
`instructions` property, a view that always reflects the current contents of the block:

```jldoctest
julia> bb
top:
  %2 = add i64 %1, %0
  ret i64 %2

julia> collect(bb.instructions)
2-element Vector{Instruction}:
 %2 = add i64 %1, %0
 ret i64 %2
```

In addition to iterating the instructions of a block, it is possible to move from one
instruction to the previous or next one using respectively the `inst.prev` and `inst.next`
properties, which are `nothing` at the start and the end of the block.
