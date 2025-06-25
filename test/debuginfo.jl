@testset "debuginfo" begin

DEBUG_METADATA_VERSION()

@dispose ctx=Context() begin
      mod = parse(LLVM.Module,  """
          define void @foo() !dbg !15 {
            %1 = alloca i32, align 4
            call void @llvm.dbg.declare(metadata i32* %1, metadata !19, metadata !DIExpression()), !dbg !21
            store i32 0, i32* %1, align 4, !dbg !21
            ret void, !dbg !22
          }

          define void @bar() {
            ret void;
          }

          declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

          !llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10}
          !llvm.dbg.cu = !{!11}
          !llvm.ident = !{!14}

          !0 = !{i32 2, !"SDK Version", [2 x i32] [i32 12, i32 3]}
          !1 = !{i32 7, !"Dwarf Version", i32 4}
          !2 = !{i32 2, !"Debug Info Version", i32 3}
          !3 = !{i32 1, !"wchar_size", i32 4}
          !4 = !{i32 1, !"branch-target-enforcement", i32 0}
          !5 = !{i32 1, !"sign-return-address", i32 0}
          !6 = !{i32 1, !"sign-return-address-all", i32 0}
          !7 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
          !8 = !{i32 7, !"PIC Level", i32 2}
          !9 = !{i32 7, !"uwtable", i32 1}
          !10 = !{i32 7, !"frame-pointer", i32 1}
          !11 = distinct !DICompileUnit(language: DW_LANG_C99, file: !12, producer: "Apple clang version 13.1.6 (clang-1316.0.21.2.5)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !13, splitDebugInlining: false, nameTableKind: None, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
          !12 = !DIFile(filename: "/tmp/test.c", directory: "/Users/tim/Julia/pkg/LLVM")
          !13 = !{}
          !14 = !{!"Apple clang version 13.1.6 (clang-1316.0.21.2.5)"}
          !15 = distinct !DISubprogram(name: "foo", scope: !16, file: !16, line: 1, type: !17, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !11, retainedNodes: !13)
          !16 = !DIFile(filename: "test.c", directory: "/tmp")
          !17 = !DISubroutineType(types: !18)
          !18 = !{null}
          !19 = !DILocalVariable(name: "foobar", scope: !15, file: !16, line: 2, type: !20)
          !20 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
          !21 = !DILocation(line: 2, column: 9, scope: !15)
          !22 = !DILocation(line: 3, column: 5, scope: !15)""")

    foo = functions(mod)["foo"]

    let sp = subprogram(foo)
      @test sp !== nothing
      @test LLVM.line(sp) == 1

      bar = functions(mod)["bar"]
      @test subprogram(bar) === nothing
      subprogram!(bar, sp)
      @test subprogram(bar) == sp
    end

    bb = entry(foo)

    if LLVM.version() < v"19"
      # LLVM 19 switched from debug intrinsics to records
      let inst = collect(instructions(bb))[2]
        diloc = metadata(inst)[LLVM.MD_dbg]::LLVM.DILocation
        @test LLVM.line(diloc) == 2
        @test LLVM.column(diloc) == 9
        @test LLVM.inlined_at(diloc) === nothing

        discope = LLVM.scope(diloc)::LLVM.DIScope
        @test LLVM.name(discope) == "foo"

        difile = LLVM.file(discope)::LLVM.DIFile
        @test LLVM.directory(difile) == "/tmp"
        @test LLVM.filename(difile) == "test.c"
        @test LLVM.source(difile) == ""

        divar = Metadata(operands(inst)[2])::LLVM.DILocalVariable
        @test LLVM.line(divar) == 2
        @test LLVM.file(divar) == difile
        @test LLVM.scope(divar) == discope
        # TODO: get type and test DIType
      end
    end

    let inst = collect(instructions(bb))[3]
      @test !isempty(metadata(inst))
      strip_debuginfo!(mod)
      @test isempty(metadata(inst))
    end

    dispose(mod)
end

end
