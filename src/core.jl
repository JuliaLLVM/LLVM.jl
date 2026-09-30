# Interface to libLLVMCore, which implements the LLVM intermediate representation as well as
# other related types and utilities.

# NOTE: we don't completely stick to the C API's organization here

include("core/context.jl")
include("core/type.jl")
include("core/value.jl")
include("core/metadata.jl")
include("core/module.jl")
include("core/basicblock.jl")
include("core/attributes.jl")
include("core/function.jl")
include("core/instructions.jl")
include("core/insertion.jl")
