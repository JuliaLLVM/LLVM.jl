@testset "Aqua" begin

using Aqua

Aqua.test_all(LLVM;
    stale_deps=(ignore=[:Requires],),
)

end
