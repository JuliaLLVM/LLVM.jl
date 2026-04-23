@testset "linker" begin

@dispose ctx=Context() builder=IRBuilder() begin
    mod1 = let
        mod = LLVM.Module("SomeModule")
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)

        mod
    end

    mod2 = let
        mod = LLVM.Module("SomeOtherModule")
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeOtherFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)

        mod
    end

    link!(mod1, mod2)
    @test haskey(functions(mod1), "SomeFunction")
    @test haskey(functions(mod1), "SomeOtherFunction")
    dispose(mod1)
end

@testset "only_needed" begin
    @dispose ctx=Context() begin
        # `dst` declares (but does not define) `needed`; it does not reference `extra`.
        @dispose dst=parse(LLVM.Module, """
            declare void @needed()
            define void @caller() {
              call void @needed()
              ret void
            }""") begin
            src = parse(LLVM.Module, """
                define void @needed() { ret void }
                define void @extra() { ret void }""")

            link!(dst, src; only_needed=true)

            @test haskey(functions(dst), "caller")
            # `needed` is referenced from `dst`, so it must be linked in (and defined now).
            @test haskey(functions(dst), "needed")
            @test !isempty(blocks(functions(dst)["needed"]))
            # `extra` is not referenced from `dst`, so it must be skipped.
            @test !haskey(functions(dst), "extra")
        end
    end
end

@testset "override_from_src" begin
    @dispose ctx=Context() begin
        @dispose dst=parse(LLVM.Module, "@glob = global i32 1") begin
            src = parse(LLVM.Module, "@glob = global i32 2")

            link!(dst, src; override_from_src=true)

            glob = globals(dst)["glob"]
            init = LLVM.initializer(glob)
            @test convert(Int, init) == 2
        end
    end
end

end
