module Interop

using ..LLVM
import ..LLVM: API

include("interop/base.jl")
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
