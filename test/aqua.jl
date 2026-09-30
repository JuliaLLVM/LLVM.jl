@testset "Aqua" begin

using Aqua

# Aqua's persistent task check loads LLVM.jl in a new environment, which does not see the
# preferences of the test environment (e.g., a locally-built libLLVMExtra, like on CI).
# Put the environments of this process, and LLVM.jl's own project (whose
# LocalPreferences.toml has the preferences when testing in the workspace), in its load
# path, so that it loads LLVM.jl the same way.
load_path = join(["@"; Base.load_path(); pkgdir(LLVM)], Sys.iswindows() ? ';' : ':')
withenv("JULIA_LOAD_PATH" => load_path) do
    Aqua.test_all(LLVM;
        stale_deps=(ignore=[:Requires],),
    )
end

end
