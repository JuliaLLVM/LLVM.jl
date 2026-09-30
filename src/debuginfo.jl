# Interface to LLVM's debug information: metadata nodes that describe the source program,
# the DIBuilder that creates them, and debug records that attach them to instructions.

include("debuginfo/builder.jl")
include("debuginfo/nodes.jl")
include("debuginfo/types.jl")
include("debuginfo/scopes.jl")
include("debuginfo/variables.jl")
include("debuginfo/records.jl")
include("debuginfo/macros.jl")
include("debuginfo/ir.jl")
