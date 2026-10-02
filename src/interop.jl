# Julia integration: generating `llvmcall`s, inline assembly, intrinsics, `Core.LLVMPtr`
# support, and Julia's own LLVM passes.
#
# Unlike the vocabularies in vocabularies.jl, which re-export functionality defined in LLVM,
# Interop implements its own. It is layered on top of LLVM, as if it were a separate
# package: it only uses LLVM's public API, with the exception of the `@*_pass` macros that
# define Julia's passes, and LLVM does not depend on it (except for the precompilation
# workload). Note that loading LLVM.jl also loads Interop, so its methods on types and
# functions that belong to others (e.g., `unsafe_load(::Core.LLVMPtr)`) are always defined.
"""
    LLVM.Interop

Integration of LLVM.jl with Julia: generating functions from LLVM IR (`@llvmgenerated`),
inline assembly (`@asmcall`), intrinsics (`assume`, `trap`), support for `Core.LLVMPtr`,
and Julia's own LLVM passes and pipeline (`JuliaPipeline`).

    using LLVM, LLVM.Interop
"""
module Interop

using ..LLVM
using ..LLVM.IR, ..LLVM.Build, ..LLVM.Passes
import ..LLVM: API

include("interop/base.jl")
include("interop/generated.jl")
include("interop/asmcall.jl")
include("interop/pointer.jl")
include("interop/utils.jl")
include("interop/intrinsics.jl")
include("interop/passes.jl")
# XXX: tighten to the actual DEV bump once JuliaLang/julia#52945 lands
@static if VERSION >= v"1.14.0-DEV"
    include("interop/dialect.jl")
end

end
