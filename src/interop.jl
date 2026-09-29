module Interop

using ..LLVM
import ..LLVM: API

include("interop/base.jl")
include("interop/generated.jl")
include("interop/asmcall.jl")
include("interop/pointer.jl")
include("interop/utils.jl")
include("interop/intrinsics.jl")
include("interop/passes.jl")

end
