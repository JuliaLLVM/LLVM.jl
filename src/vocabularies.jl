# Vocabularies
#
# LLVM.jl's API uses many common words (`functions`, `add!`, `lookup`, `Context`, ...) that
# would clash with other packages if they were all exported. Instead, `using LLVM` only
# brings `@dispose` into scope, while the rest of the API is public and can be used
# qualified (`LLVM.functions(mod)`), or brought into scope per subsystem by opting into one
# of the vocabularies below (`using LLVM.IR`). The vocabularies are meant to be `using`-ed;
# for qualified use, `LLVM.functions` is as short as `IR.functions`.
#
# The vocabularies re-export bindings that are defined in LLVM, so `LLVM.IR.functions` is
# `LLVM.functions`. Names are added to them using `@vocabulary` where they are defined.

@public IR, Build, Passes, ORC

"""
    LLVM.IR

The LLVM IR object model: contexts, modules, values, types, metadata and debug info, along
with functions to traverse and modify them (`functions`, `blocks`, `instructions`,
`operands`, `uses`, `erase!`, `replace_uses!`, ...).

    using LLVM, LLVM.IR

    for f in functions(mod), bb in blocks(f), inst in instructions(bb)
        # ...
    end

The attributes and relationships of these objects are accessed as properties, like
`fn.name`, `gv.linkage` or `inst.parent`, rather than using functions.
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
        position!(builder, BasicBlock(f, "entry"))
        ret!(builder, add!(builder, parameters(f)...))
    end
"""
module Build
    import ..LLVM
    LLVM.@reexport Build dispose
end

"""
    LLVM.Passes

Optimization passes and pipelines: the `NewPMPassBuilder`, pass managers, pass constructors
like `InstCombinePass`, custom passes and pipeline callbacks.

    using LLVM, LLVM.Passes

    @dispose pb=NewPMPassBuilder() begin
        add!(pb, InstCombinePass())
        run!(pb, mod)
    end
"""
module Passes
    import ..LLVM
    LLVM.@reexport Passes
end

"""
    LLVM.ORC

The ORC just-in-time compiler: `LLJIT`, execution sessions, JIT dylibs, thread-safe
modules and contexts, and the layers of the JIT.
"""
module ORC
    import ..LLVM
    LLVM.@reexport ORC dispose emit add!
end
