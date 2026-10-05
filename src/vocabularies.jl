# Vocabularies
#
# LLVM.jl's API uses many common words (`verify`, `add!`, `lookup`, `Context`, ...) that
# would clash with other packages if they were all exported. Instead, `using LLVM` only
# brings `@dispose` into scope, while the rest of the API is public and can be used
# qualified (`LLVM.verify(mod)`), or brought into scope per subsystem by opting into one of
# the vocabularies below (`using LLVM.IR`). The vocabularies are meant to be `using`-ed; for
# qualified use, `LLVM.verify` is as short as `IR.verify`.
#
# The vocabularies re-export bindings that are defined in LLVM, so `LLVM.IR.verify` is
# `LLVM.verify`. Names are added to them using `@vocabulary` where they are defined.

@public IR, Build, Passes, Analysis, ORC

"""
    LLVM.IR

The LLVM IR object model: contexts, modules, values, types, metadata and debug info, along
with predicates and operations to inspect and modify them (`isdeclaration`, `erase!`,
`replace_uses!`, `verify`, ...).

    using LLVM, LLVM.IR

    for f in mod.functions
        isdeclaration(f) && continue
        for bb in f.blocks, inst in bb.instructions
            # ...
        end
    end

The attributes, relationships and contents of these objects are accessed as properties,
like `fn.name`, `gv.linkage`, `inst.parent` or `f.blocks`, rather than using functions.
"""
module IR
    import ..LLVM
    LLVM.@reexport IR
end

"""
    LLVM.Build

Construction of IR: the `IRBuilder` and its instruction-building functions (`add!`,
`load!`, `call!`, `ret!`, ...), constant expressions (`const_add`, ...), and the
`DIBuilder` for debug info.

    using LLVM, LLVM.IR, LLVM.Build

    @dispose builder=IRBuilder() begin
        position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
        ret!(builder, add!(builder, f.parameters...))
    end
"""
module Build
    import ..LLVM
    LLVM.@reexport Build dispose
end

"""
    LLVM.Passes

Optimization passes and pipelines: the `PassBuilder`, pass managers, pass constructors
like `InstCombinePass`, custom passes and pipeline callbacks.

    using LLVM, LLVM.Passes

    @dispose pb=PassBuilder() begin
        add!(pb, InstCombinePass())
        run!(pb, mod)
    end
"""
module Passes
    import ..LLVM
    LLVM.@reexport Passes
end

"""
    LLVM.Analysis

LLVM's analyses and the values they compute: dominator trees, and constant ranges and known
bits of integers. Analyses can be used in custom passes through the analysis manager of a
pass pipeline (see `FunctionPass` and `FunctionAnalysisManager`).

    using LLVM, LLVM.IR, LLVM.Analysis

    r = ConstantRange(64, 0, 100) + ConstantRange(64, 1)   # [1, 101)
"""
module Analysis
    import ..LLVM
    LLVM.@reexport Analysis DomTree PostDomTree dominates dispose
end

"""
    LLVM.ORC

The ORC just-in-time compiler: `LLJIT`, execution sessions, JIT dylibs, thread-safe
modules and contexts, symbols and their definitions, definition generators, resource
trackers, materialization units, and the layers of the JIT.
"""
module ORC
    import ..LLVM
    LLVM.@reexport ORC dispose add! remove!
end
