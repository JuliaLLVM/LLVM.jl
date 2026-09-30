@testset "insertion points" begin

@testset "after_phis" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    f = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type()]))
    x = f.parameters[1]
    entry = BasicBlock(f, "entry")
    body = BasicBlock(f, "body")
    position!(builder, entry)
    br!(builder, body)

    # an empty block
    @test LLVM.after_phis(body) == LLVM.at_end(body)

    # only PHI nodes
    position!(builder, body)
    phi1 = phi!(builder, LLVM.Int32Type())
    push!(phi1.incoming, (x, entry))
    phi2 = phi!(builder, LLVM.Int32Type())
    push!(phi2.incoming, (x, entry))
    @test LLVM.after_phis(body) == LLVM.at_end(body)

    # the first instruction after the PHI nodes
    a = add!(builder, phi1, phi2)
    ret!(builder, a)
    @test LLVM.after_phis(body) == LLVM.after(phi2)
    b = copy(a)
    move!(b, LLVM.after_phis(body))
    @test b.next == a
    verify(mod)
end

# blocks without legal insertion point
if LLVM.version() >= v"17"
@dispose ctx=Context() begin
    mod = parse(LLVM.Module, """
        declare void @g()
        declare i32 @pers(...)

        define void @f() personality ptr @pers {
        entry:
          invoke void @g() to label %cont unwind label %dispatch
        cont:
          ret void
        dispatch:
          %cs = catchswitch within none [label %handler] unwind to caller
        handler:
          %cp = catchpad within %cs []
          catchret from %cp to label %cont
        lpad:
          %lp = landingpad { ptr, i32 } cleanup
          resume { ptr, i32 } %lp
        }""")
    f = mod.functions["f"]
    blocks = Dict(bb.name => bb for bb in f.blocks)
    @test LLVM.after_phis(blocks["dispatch"]) === nothing
    @test LLVM.after_phis(blocks["handler"]) == LLVM.after(first(blocks["handler"].instructions))
    @test LLVM.after_phis(blocks["lpad"]) == LLVM.after(first(blocks["lpad"].instructions))
    dispose(mod)
end
end
end

@testset "instructions" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type()])
    f = LLVM.Function(mod, "f", ft)
    entry = BasicBlock(f, "entry")
    exit = BasicBlock(f, "exit")
    x = f.parameters[1]
    position!(builder, entry)
    a = add!(builder, x, x, "a")
    b = mul!(builder, x, x, "b")
    br = br!(builder, exit)
    position!(builder, exit)
    c = sub!(builder, a, b, "c")
    ret = ret!(builder, c)

    # within a block
    @test move!(b, LLVM.before(a)) === b
    @test collect(entry.instructions) == [b, a, br]
    move!(b, LLVM.after(a))
    @test collect(entry.instructions) == [a, b, br]
    move!(b, LLVM.at_begin(entry))
    @test collect(entry.instructions) == [b, a, br]

    # moving an instruction next to itself doesn't do anything
    move!(a, LLVM.before(a))
    move!(a, LLVM.after(a))
    @test collect(entry.instructions) == [b, a, br]

    # to another block
    move!(b, LLVM.before(c))
    @test collect(exit.instructions) == [b, c, ret]
    @test b.parent == exit
    move!(b, LLVM.before(br))
    @test collect(entry.instructions) == [a, b, br]

    # not after a terminator
    @test_throws ArgumentError move!(b, LLVM.at_end(entry))
    @test_throws ArgumentError move!(b, LLVM.after(br))
    @test move!(br, LLVM.at_end(entry)) === br
    verify(mod)

    # instructions that are not part of a block are inserted
    d = copy(c)
    d.name = "d"
    @test d.parent === nothing
    move!(d, LLVM.before(ret))
    @test d.parent == exit
    @test d.name == "d"
    remove!(d)
    move!(d, LLVM.at_begin(exit))
    @test first(exit.instructions) == d
    erase!(d)

    # an unterminated block can be extended at the end
    cont = BasicBlock(f, "cont")
    e = copy(c)
    move!(e, LLVM.at_end(cont))
    @test collect(cont.instructions) == [e]
    erase!(e)
    erase!(cont)

    # instructions can not be moved to another context
    e = copy(c)
    @dispose ctx2=Context() mod2=LLVM.Module("OtherModule") begin
        f2 = LLVM.Function(mod2, "f", LLVM.FunctionType(LLVM.VoidType()))
        bb2 = BasicBlock(f2, "entry")
        @test_throws ArgumentError move!(e, LLVM.at_end(bb2))
    end
    erase!(e)

    verify(mod)
end
end

# positions next to instructions with debug records
if LLVM.version() >= v"19"
@testset "debug records" begin
@dispose ctx=Context() begin
    mod = parse(LLVM.Module, """
        define void @f(i32 %x) !dbg !5 {
          %p = alloca i32, align 4
          %a = add i32 %x, %x
          %b = add i32 %x, %x
            #dbg_value(i32 %x, !9, !DIExpression(), !10)
          ret void, !dbg !10
        }

        !llvm.dbg.cu = !{!0}
        !llvm.module.flags = !{!3}

        !0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, emissionKind: FullDebug)
        !1 = !DIFile(filename: "test.c", directory: "/tmp")
        !3 = !{i32 2, !"Debug Info Version", i32 3}
        !5 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !6, unit: !0, spFlags: DISPFlagDefinition)
        !6 = !DISubroutineType(types: !7)
        !7 = !{null}
        !8 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
        !9 = !DILocalVariable(name: "v", scope: !5, file: !1, line: 2, type: !8)
        !10 = !DILocation(line: 2, column: 1, scope: !5)
        """)
    f = mod.functions["f"]
    alloca, a, b, ret = f.entry.instructions
    records(inst) = collect(inst.debug_records)
    (record,) = records(ret)

    # before an instruction: after its debug records, which now precede the moved one
    move!(a, LLVM.before(ret))
    @test a.next == ret
    @test records(a) == [record] && isempty(records(ret))

    # after an instruction: before the debug records of the next one
    move!(b, LLVM.after(alloca))
    @test alloca.next == b
    @test isempty(records(b)) && records(a) == [record]

    # moving an instruction after the previous one moves it before its debug records
    move!(a, LLVM.after(b))
    @test isempty(records(a)) && records(ret) == [record]

    # the debug records of a moved instruction stay where they were
    move!(a, LLVM.before(ret))
    move!(a, LLVM.after(alloca))
    @test isempty(records(a)) && isempty(records(b)) && records(ret) == [record]
    move!(a, LLVM.at_begin(f.entry))
    @test first(f.entry.instructions) == a

    verify(mod)
    dispose(mod)
end
end
end

@testset "basic blocks" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    f1 = LLVM.Function(mod, "f1", ft)
    f2 = LLVM.Function(mod, "f2", ft)
    names(f) = [bb.name for bb in f.blocks]
    function block!(pos, name)
        bb = BasicBlock(pos, name)
        position!(builder, bb)
        ret!(builder)
        bb
    end
    a1 = block!(LLVM.at_end(f1), "a1")
    a3 = block!(LLVM.after(a1), "a3")
    a2 = block!(LLVM.before(a3), "a2")
    a0 = block!(LLVM.at_begin(f1), "a0")
    @test names(f1) == ["a0", "a1", "a2", "a3"]
    b1 = block!(LLVM.at_begin(f2), "b1")
    @test names(f2) == ["b1"]
    @test LLVM.before(a1) isa InsertionPoint{BasicBlock}
    @test LLVM.at_begin(f1) == LLVM.before(a0)
    @test LLVM.after(a3) == LLVM.at_end(f1)

    # within a function
    @test move!(a0, LLVM.at_end(f1)) === a0
    @test names(f1) == ["a1", "a2", "a3", "a0"]
    move!(a0, LLVM.before(a1))
    @test names(f1) == ["a0", "a1", "a2", "a3"]
    move!(a0, LLVM.before(a0))
    move!(a0, LLVM.after(a0))
    @test names(f1) == ["a0", "a1", "a2", "a3"]

    # to another function, before or after a block
    move!(a3, LLVM.before(b1))
    @test a3.parent == f2
    @test names(f1) == ["a0", "a1", "a2"]
    @test names(f2) == ["a3", "b1"]
    move!(a2, LLVM.after(b1))
    @test a2.parent == f2
    @test names(f2) == ["a3", "b1", "a2"]
    move!(a3, LLVM.at_end(f1))
    move!(a2, LLVM.before(a3))
    @test names(f1) == ["a0", "a1", "a2", "a3"]
    verify(mod)

    # blocks that are not part of a function are inserted
    d = BasicBlock("d")
    move!(d, LLVM.after(a0))
    @test d.parent == f1
    @test names(f1) == ["a0", "d", "a1", "a2", "a3"]
    remove!(d)
    @test d.parent === nothing
    move!(d, LLVM.at_begin(f2))
    @test names(f2) == ["d", "b1"]
    remove!(d)
    erase!(d)
    c = clone(a1; dest=nothing)
    move!(c, LLVM.at_end(f2))
    @test c.parent == f2
    verify(mod)

    # insertion points refer to blocks in a function
    detached = BasicBlock("detached")
    @test_throws ArgumentError LLVM.before(detached)
    @test_throws ArgumentError LLVM.after(detached)
    erase!(detached)
    pos = LLVM.before(a1)
    move!(a1, LLVM.at_end(f2))
    @test_throws ArgumentError BasicBlock(pos, "invalid")
end
end

@testset "functions and globals" begin
@dispose ctx=Context() mod=LLVM.Module("SomeModule") mod2=LLVM.Module("OtherModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    f = LLVM.Function(mod, "f", ft)
    g = LLVM.Function(mod, "g", ft)
    h = LLVM.Function(mod2, "h", ft)
    @test LLVM.before(f) isa InsertionPoint{LLVM.Function}
    @test LLVM.at_begin(mod.functions) == LLVM.before(f)
    @test LLVM.after(g) == LLVM.at_end(mod.functions)
    @test_throws ArgumentError move!(h, LLVM.before(f))
    @test occursin("before", sprint(show, LLVM.before(f)))

    gv = GlobalVariable(mod, LLVM.Int32Type(), "gv")
    hv = GlobalVariable(mod2, LLVM.Int32Type(), "hv")
    @test LLVM.before(gv) isa InsertionPoint{GlobalVariable}
    @test LLVM.at_begin(mod.globals) == LLVM.before(gv)
    @test LLVM.after(gv) == LLVM.at_end(mod.globals)
    @test_throws ArgumentError move!(hv, LLVM.after(gv))
end
end

end
