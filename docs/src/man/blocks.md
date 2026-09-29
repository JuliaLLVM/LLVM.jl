# Basic blocks

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Basic blocks are sequences of instructions that are executed in order. They are the building
blocks of functions, and can be looked up using the `blocks` iterator, or by constructing
them directly:

```jldoctest
julia> bb = BasicBlock("SomeBlock")
SomeBlock:                                        ; No predecessors!
```

A detached basic block often not what you want; using the `BasicBlock(::Function)`
constructor you can instead append to a function, or insert before another block using the
`BasicBlock(::BasicBlock)` constructor.

Basic blocks support a couple of specific APIs:

- `bb.name`: the name of the basic block.
- `bb.parent`: the parent function of the basic block, or `nothing` if it is detached.
- `bb.terminator`: the terminator instruction of the block, or `nothing` if it has none.
- `move_before`/`move_after`: move the block before or after another block.
- `remove!`/`erase!`: delete the basic block from its parent function, or additionally also
  delete the block itself.


## Control flow

The LLVM C API supports a couple of functions to inspect the control flow of basic blocks:

- `predecessors`: get the predecessors of a basic block.
- `successors`: get the successors of a basic block.


## Instructions

```@meta
DocTestSetup = quote
    using LLVM

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
    fun = only(functions(mod));
    bb = fun.entry
end
```

The main purpose of basic blocks is to contain instructions, which can be iterated using the
`instructions` function:

```jldoctest
julia> bb
top:
  %2 = add i64 %1, %0
  ret i64 %2

julia> collect(instructions(bb))
2-element Vector{Instruction}:
 %2 = add i64 %1, %0
 ret i64 %2
```

In addition to the iteration interface, it is possible to move from one instruction to the
previous or next one using respectively the `previnst` and `nextinst` functions.
