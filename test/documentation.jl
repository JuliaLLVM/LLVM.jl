@testset "documentation" begin

# whether a binding has a docstring (like `Docs.hasdoc`, which Julia 1.10 doesn't have)
function hasdoc(mod::Module, sym::Symbol)
    binding = Base.Docs.Binding(mod, sym)
    for b in (binding, Base.Docs.aliasof(binding)), m in Base.Docs.modules
        haskey(Base.Docs.meta(m), b) && return true
    end
    return false
end

# the public names of LLVM.jl: those declared using `@public` (including the vocabularies,
# whose names resolve to the same bindings), and the exports of LLVM.Interop. names that
# are only defined for some versions of LLVM are only checked when they exist.
public = Set{Tuple{Module,Symbol}}()
for (mod, names) in LLVM.public_names, name in names
    push!(public, (mod, name))
end
for name in names(LLVM.Interop)
    push!(public, (LLVM.Interop, name))
end
filter!(public) do (mod, name)
    isdefined(mod, name) && name !== :API    # the C API is documented by LLVM
end

undocumented = sort([string(mod, ".", name) for (mod, name) in public
                     if !hasdoc(mod, name)])
@test undocumented == String[]

# `@public` records the same names that Julia considers public
if VERSION >= v"1.11"
    @test all(((mod, name),) -> Base.ispublic(mod, name), public)
end

end
