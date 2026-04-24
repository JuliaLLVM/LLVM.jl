@testset "debuginfo" begin

DEBUG_METADATA_VERSION()

@testset "DIBuilder lifecycle" begin
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        dib = DIBuilder(mod)
        LLVM.finalize!(dib)
        dispose(dib)
    end

    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        dib = DIBuilder(mod; allow_unresolved=false)
        LLVM.finalize!(dib)
        dispose(dib)
    end

    # do-block form
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        DIBuilder(mod) do dib
            LLVM.finalize!(dib)
        end
    end
end

@testset "DIBuilder: file/compile-unit/module/namespace" begin
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        DIBuilder(mod) do dib
            file = LLVM.file!(dib, "test.jl", "/tmp")
            @test file isa DIFile
            @test LLVM.filename(file) == "test.jl"
            @test LLVM.directory(file) == "/tmp"

            cu = LLVM.compileunit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                                   file, "LLVM.jl Tests")
            @test cu isa DICompileUnit

            ns = LLVM.namespace!(dib, cu, "MyNamespace")
            @test ns isa DINamespace
            @test LLVM.name(ns) == "MyNamespace"

            dm = LLVM.dimodule!(dib, cu, "MyModule")
            @test dm isa DIModule
            @test LLVM.name(dm) == "MyModule"

            LLVM.finalize!(dib)
        end

        # emitted DWARF should round-trip as text IR (compile unit is retained)
        ir = string(mod)
        @test occursin("DICompileUnit", ir)
        @test occursin("DIFile", ir)
    end
end

@testset "DIBuilder: lexical blocks" begin
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        DIBuilder(mod) do dib
            file = LLVM.file!(dib, "test.jl", "/tmp")
            cu = LLVM.compileunit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                                   file, "LLVM.jl Tests")

            lb = LLVM.lexicalblock!(dib, cu, file, 3, 5)
            @test lb isa DILexicalBlock

            lbf = LLVM.lexicalblockfile!(dib, lb, file, 0)
            @test lbf isa DILexicalBlockFile

            # DILocation with lexical block scope
            loc = DILocation(10, 20, lb)
            @test LLVM.line(loc) == 10
            @test LLVM.column(loc) == 20
            @test LLVM.scope(loc) == lb

            # inlined_at chain
            outer = DILocation(5, 1, cu)
            inner = DILocation(10, 20, lb, outer)
            @test LLVM.inlined_at(inner) == outer

            LLVM.finalize!(dib)
        end
    end
end

@testset "DIBuilder: type constructors" begin
    DW_ATE_signed = 0x05
    DW_TAG_structure_type = 0x13
    DW_TAG_const_type = 0x26
    DW_TAG_reference_type = 0x10

    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        DIBuilder(mod) do dib
            file = LLVM.file!(dib, "test.jl", "/tmp")
            cu = LLVM.compileunit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                                   file, "LLVM.jl Tests")

            # basic types
            i64 = LLVM.basictype!(dib, "Int64", 64, DW_ATE_signed)
            @test i64 isa LLVM.DIBasicType
            @test LLVM.name(i64) == "Int64"
            @test LLVM.align(i64) == 0
            @test LLVM.tag(i64) != 0

            @test LLVM.unspecifiedtype!(dib, "unspec") isa LLVM.DIBasicType
            @test LLVM.nullptrtype!(dib) isa LLVM.DIBasicType

            # derived types
            ptr = LLVM.pointertype!(dib, i64, 64; name="i64_ptr")
            @test ptr isa LLVM.DIDerivedType

            td = LLVM.typedeftype!(dib, i64, "MyInt", file, 1, cu)
            @test td isa LLVM.DIDerivedType

            cq = LLVM.qualifiedtype!(dib, DW_TAG_const_type, i64)
            @test cq isa LLVM.DIDerivedType

            at2 = LLVM.artificialtype!(dib, i64)
            @test at2 isa LLVM.DIDerivedType

            op = LLVM.objectpointertype!(dib, i64)
            @test op isa LLVM.DIDerivedType

            ref = LLVM.referencetype!(dib, DW_TAG_reference_type, i64)
            @test ref isa LLVM.DIDerivedType

            # composite types
            mem = LLVM.membertype!(dib, cu, "x", file, 2, 64, 64, 0, i64)
            @test mem isa LLVM.DIDerivedType

            st = LLVM.structtype!(dib, cu, "Point", file, 1, 64, 64, LLVM.Metadata[mem])
            @test st isa LLVM.DICompositeType
            @test LLVM.name(st) == "Point"

            un = LLVM.uniontype!(dib, cu, "U", file, 1, 64, 64, LLVM.Metadata[mem])
            @test un isa LLVM.DICompositeType

            ct = LLVM.classtype!(dib, cu, "C", file, 1, 64, 64, 0, LLVM.Metadata[mem])
            @test ct isa LLVM.DICompositeType

            # arrays/vectors via subrange
            sr = LLVM.getorcreatesubrange!(dib, 0, 10)
            @test sr isa LLVM.DISubrange
            aty = LLVM.arraytype!(dib, 640, 64, i64, [sr])
            @test aty isa LLVM.DICompositeType

            vty = LLVM.vectortype!(dib, 256, 64, i64, [sr])
            @test vty isa LLVM.DICompositeType

            # enumerations
            e1 = LLVM.enumerator!(dib, "A", 0)
            @test e1 isa LLVM.DIEnumerator
            et = LLVM.enumerationtype!(dib, cu, "Color", file, 1, 32, 32, LLVM.Metadata[e1])
            @test et isa LLVM.DICompositeType

            # bitfield + static + member-pointer + inheritance
            @test LLVM.bitfieldmembertype!(dib, cu, "b", file, 1, 3, 0, 0, i64) isa LLVM.DIDerivedType
            @test LLVM.staticmembertype!(dib, cu, "s", file, 1, i64) isa LLVM.DIDerivedType
            @test LLVM.memberpointertype!(dib, i64, st, 64) isa LLVM.DIDerivedType

            base = LLVM.classtype!(dib, cu, "Base", file, 1, 64, 64, 0, LLVM.Metadata[])
            @test LLVM.inheritance!(dib, ct, base, 0) isa LLVM.DIDerivedType

            # subroutine
            sroute = LLVM.subroutinetype!(dib, file, LLVM.Metadata[i64, i64])
            @test sroute isa LLVM.DISubroutineType

            # forward decl / replaceable composite
            @test LLVM.forwarddecl!(dib, DW_TAG_structure_type, "Fwd", cu, file, 1) isa LLVM.DICompositeType
            @test LLVM.replaceablecompositetype!(dib, DW_TAG_structure_type, "Rep", cu, file, 1) isa LLVM.DICompositeType

            LLVM.finalize!(dib)
        end
    end
end

@testset "DIBuilder: subprograms, variables, expressions" begin
    DW_ATE_signed = 0x05

    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        DIBuilder(mod) do dib
            file = LLVM.file!(dib, "test.jl", "/tmp")
            cu = LLVM.compileunit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                                   file, "LLVM.jl Tests")

            i64 = LLVM.basictype!(dib, "Int64", 64, DW_ATE_signed)
            stype = LLVM.subroutinetype!(dib, file, LLVM.Metadata[i64, i64, i64])

            # subprogram
            sp = LLVM.subprogram!(dib, file, "add", "add", file, 1, stype, 1)
            @test sp isa DISubProgram
            @test LLVM.line(sp) == 1

            # variables
            v = LLVM.autovariable!(dib, sp, "x", file, 2, i64)
            @test v isa LLVM.DILocalVariable
            @test LLVM.line(v) == 2
            @test LLVM.file(v) == file
            @test LLVM.scope(v) == sp

            p = LLVM.parametervariable!(dib, sp, "a", 1, file, 1, i64)
            @test p isa LLVM.DILocalVariable

            # expressions
            e = LLVM.expression!(dib)
            @test e isa LLVM.DIExpression

            ce = LLVM.constantvalueexpression!(dib, 42)
            @test ce isa LLVM.DIExpression

            # global variable expression + accessors
            gve = LLVM.globalvariableexpression!(dib, cu, "g", "g",
                                                  file, 1, i64, false, e)
            @test gve isa LLVM.DIGlobalVariableExpression
            gv = LLVM.variable(gve)
            @test gv isa LLVM.DIGlobalVariable
            @test LLVM.line(gv) == 1
            @test LLVM.expression(gve) isa LLVM.DIExpression

            # temp global forward decl
            tgv = LLVM.tempglobalvariablefwddecl!(dib, cu, "tg", "tg",
                                                   file, 2, i64, false)
            @test tgv isa LLVM.DIGlobalVariable

            LLVM.finalize_subprogram!(dib, sp)
            LLVM.finalize!(dib)
        end
    end
end

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
