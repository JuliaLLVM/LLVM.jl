@testset "string arguments" begin

# a string type with UTF-16 code units, so that `ncodeunits` doesn't match the number of
# bytes of its `String` conversion (as passed to LLVM) for non-ASCII text. only covers the
# Basic Multilingual Plane, which suffices for these tests.
struct UTF16TestString <: AbstractString
    units::Vector{UInt16}
end
UTF16TestString(s::String) = UTF16TestString(transcode(UInt16, s))
Base.ncodeunits(s::UTF16TestString) = length(s.units)
Base.codeunit(::UTF16TestString) = UInt16
Base.codeunit(s::UTF16TestString, i::Integer) = s.units[i]
Base.isvalid(s::UTF16TestString, i::Int) = checkbounds(Bool, s.units, i)
Base.iterate(s::UTF16TestString, i::Int=1) =
    i > ncodeunits(s) ? nothing : (Char(s.units[i]), i + 1)

let s = UTF16TestString("∇f")
    @test ncodeunits(s) == 2
    @test String(s) == "∇f"
    @test ncodeunits(String(s)) == 4
end

@testset "encoding" begin
    # functions that pass the length of a string to LLVM measure its `String` conversion
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        attr = StringAttribute(UTF16TestString("kïnd"), UTF16TestString("välue"))
        @test attr.kind == "kïnd"
        @test attr.value == "välue"

        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "f", ft)
        attrs = fn.function_attributes
        push!(attrs, attr)
        @test haskey(attrs, UTF16TestString("kïnd"))
        @test attrs[UTF16TestString("kïnd")] == attr
        delete!(attrs, UTF16TestString("kïnd"))
        @test isempty(attrs)

        @dispose builder=IRBuilder() begin
            position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
            call = call!(builder, ft, fn)
            ret!(builder)
            attrs = call.function_attributes
            push!(attrs, attr)
            @test haskey(attrs, UTF16TestString("kïnd"))
            @test attrs[UTF16TestString("kïnd")] == attr
            delete!(attrs, UTF16TestString("kïnd"))
            @test isempty(attrs)
        end

        @test Intrinsic(UTF16TestString("llvm.trap")) == Intrinsic("llvm.trap")

        DIBuilder(mod) do dib
            file = LLVM.file!(dib, UTF16TestString("tëst.jl"), UTF16TestString("/tmp/∂ir"))
            @test file.filename == "tëst.jl"
            @test file.directory == "/tmp/∂ir"
            ty = LLVM.basic_type!(dib, UTF16TestString("∇f"), 64, 0x05)
            @test ty.name == "∇f"
        end
    end
end

# a substring that doesn't extend to the end of its parent string, so a pointer to its data
# isn't NUL-terminated (`sub("x")` is `"x"` inside `"«x»"`)
function sub(s::String)
    t = "«" * s * "»"
    SubString(t, nextind(t, 1), prevind(t, lastindex(t)))
end

let s = sub("fün")
    @test s isa SubString{String}
    @test s == "fün"
end

@testset "names and lookups" begin
    @dispose ctx=Context() mod=LLVM.Module(sub("SomeMödule")) begin
        @test mod.name == "SomeMödule"
        mod.name = UTF16TestString("Mödule")
        @test mod.name == "Mödule"
        mod.name = sub("Mod")
        @test mod.name == "Mod"

        ft = LLVM.FunctionType(LLVM.VoidType())

        # functions
        fn = LLVM.Function(mod, sub("fün"), ft)
        @test fn.name == "fün"
        @test mod.functions[sub("fün")] == fn
        @test haskey(mod.functions, sub("fün"))
        @test get(mod.functions, sub("fün"), nothing) == fn
        @test get(mod.functions, sub("other"), nothing) === nothing
        @test_throws KeyError mod.functions[sub("other")]
        @test get!(() -> error("unreachable"), mod.functions, sub("fün")) == fn
        g = get!(mod.functions, sub("g")) do
            LLVM.Function(mod, sub("g"), ft)
        end
        @test g.name == "g"
        fn.name = sub("f")
        @test fn.name == "f"
        @test haskey(mod.functions, "f")

        # global variables
        gv = GlobalVariable(mod, LLVM.Int32Type(), sub("glöbal"))
        @test gv.name == "glöbal"
        @test mod.globals[sub("glöbal")] == gv
        @test haskey(mod.globals, sub("glöbal"))
        @test get(mod.globals, sub("glöbal"), nothing) == gv
        @test get!(() -> error("unreachable"), mod.globals, sub("glöbal")) == gv
        gv2 = get!(mod.globals, sub("gv2")) do
            GlobalVariable(mod, LLVM.Int32Type(), sub("gv2"))
        end
        @test gv2.name == "gv2"

        # names of global values are checked for clashes
        @test_throws ArgumentError get!(() -> error("unreachable"), mod.functions,
                                        sub("glöbal"))
        @test_throws ArgumentError get!(() -> error("unreachable"), mod.globals, sub("f"))

        gv.section = sub("sëction")
        @test gv.section == "sëction"
        fn.gc = sub("shadow-stack")
        @test fn.gc == "shadow-stack"

        # aliases
        alias = GlobalAlias(mod, gv, sub("aliäs"))
        @test alias.name == "aliäs"
        @test mod.aliases[sub("aliäs")] == alias
        @test mod.aliases[UTF16TestString("aliäs")] == alias
        @test haskey(mod.aliases, sub("aliäs"))
        @test get(mod.aliases, sub("aliäs"), nothing) == alias
        alias2 = GlobalAlias(mod, gv.global_value_type, gv, sub("alias2"))
        @test alias2.name == "alias2"

        # ifuncs
        resolver_ft = LLVM.FunctionType(LLVM.PointerType(ft))
        resolver = LLVM.Function(mod, "resolver", resolver_ft)
        @dispose builder=IRBuilder() begin
            position!(builder, LLVM.at_end(BasicBlock(resolver, sub("entry"))))
            ret!(builder, fn)
        end
        ifunc = GlobalIFunc(mod, ft, resolver, UTF16TestString("ïfunc"))
        @test ifunc.name == "ïfunc"
        @test mod.ifuncs[sub("ïfunc")] == ifunc
        @test haskey(mod.ifuncs, UTF16TestString("ïfunc"))
        @test get(mod.ifuncs, sub("ïfunc"), nothing) == ifunc

        # module flags
        md = Metadata(ConstantInt(Int32(42)))
        mod.flags[sub("flög"), LLVM.API.LLVMModuleFlagBehaviorError] = md
        mod.flags[UTF16TestString("flög2"), LLVM.API.LLVMModuleFlagBehaviorError] = md
        @test mod.flags[sub("flög")] == md
        @test haskey(mod.flags, UTF16TestString("flög"))
        @test haskey(mod.flags, "flög2")

        # named metadata
        node = get!(mod.metadata, sub("nämed"))
        @test node.name == "nämed"
        @test haskey(mod.metadata, sub("nämed"))
        @test haskey(mod.metadata, UTF16TestString("nämed"))
        @test mod.metadata[sub("nämed")] == node
        @test get(mod.metadata, sub("other"), nothing) === nothing

        # named types
        st = LLVM.StructType(sub("strüct"))
        @test st.name == "strüct"
        @test ctx.types[sub("strüct")] == st
        @test haskey(ctx.types, sub("strüct"))
        @test get(ctx.types, sub("strüct"), nothing) == st

        # basic blocks
        bb = BasicBlock(fn, sub("ëntry"))
        @test bb.name == "ëntry"
        bb2 = BasicBlock(LLVM.after(bb), sub("cont"))
        @test bb2.name == "cont"
        detached = BasicBlock(sub("detached"))
        @test detached.name == "detached"
        erase!(detached)
        bb.name = sub("top")
        @test bb.name == "top"
    end
end

@testset "IR construction" begin
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
        ft = LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type(), LLVM.Int32Type()])
        fn = LLVM.Function(mod, "f", ft)
        position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
        x, y = fn.parameters
        s = add!(builder, x, y, sub("süm"))
        @test s.name == "süm"
        a = alloca!(builder, LLVM.Int32Type(), sub("slöt"))
        @test a.name == "slöt"
        c = icmp!(builder, LLVM.API.LLVMIntEQ, x, y, sub("cmp"))
        @test c.name == "cmp"
        l = load!(builder, LLVM.Int32Type(), a, sub("löad"))
        @test l.name == "löad"
        bundle = OperandBundle(UTF16TestString("bündle"), Value[x])
        @test bundle.tag == "bündle"
        ret!(builder, s)

        # strings are bytes, which can include NULs
        str = globalstring!(builder, sub("a\0b"), sub("strïng"))
        @test str.name == "strïng"
        @test str.initializer == ConstantDataArray(UInt8['a', 0x00, 'b', 0x00])
        str = globalstring!(mod, sub("ab"); add_null=false)
        @test str.initializer == ConstantDataArray(UInt8['a', 'b'])

        asm = InlineAsm(LLVM.FunctionType(LLVM.VoidType()), sub("nop"), sub(""), false)
        @test asm isa InlineAsm

        cloned = LLVM.clone(fn.entry; suffix=sub(".clöne"))
        @test cloned.name == "entry.clöne"

        @test MDString(UTF16TestString("mëtadata")) == MDString("mëtadata")
        @test string(MDString(sub("mëtadata"))) == string(MDString("mëtadata"))

        tag = LLVM.Interop.tbaa_make_child(sub("custom"))
        @test convert(String, tag.operands[1].operands[1]) == "custom_tbaa_custom"

        # attribute kinds
        @test EnumAttribute(sub("nounwind")) == EnumAttribute(:nounwind)
        @test EnumAttribute(sub("align"), 16) == EnumAttribute(:align, 16)
        @test TypeAttribute(sub("sret"), LLVM.Int32Type()) ==
              TypeAttribute(:sret, LLVM.Int32Type())
        if LLVM.version() >= v"19"
            @test ConstantRangeAttribute(sub("range"), 32, UInt64[0], UInt64[100]) ==
                  ConstantRangeAttribute(:range, 32, UInt64[0], UInt64[100])
        end
        @test_throws ArgumentError EnumAttribute(sub("nonexisting"))

        # module properties
        mod.datalayout = sub("e-p:64:64:64")
        @test string(mod.datalayout) == "e-p:64:64:64"
        mod.triple = sub("x86_64-unknown-linux-gnu")
        @test mod.triple == "x86_64-unknown-linux-gnu"
        dl = LLVM.DataLayout(sub("e-p:64:64:64"))
        @test string(dl) == "e-p:64:64:64"
        dispose(dl)
    end
end

@testset "textual inputs" begin
    @dispose ctx=Context() begin
        ir = """
            define void @parsed() {
              ret void
            }"""
        # a substring of a larger string, which LLVM must not read beyond
        @dispose mod=parse(LLVM.Module, SubString(ir * "\ngarbage", 1, ncodeunits(ir))) begin
            @test haskey(mod.functions, "parsed")
            @test run!(sub("no-op-module"), mod) === nothing
            @dispose pb=PassBuilder() begin
                add!(pb, LLVM.PassManager(sub("module"))) do mpm
                    add!(mpm, sub("no-op-module"))
                end
                @test string(pb) == "module(no-op-module)"
                @test run!(pb, mod) === nothing
            end
        end
        @dispose mod=parse(LLVM.Module, UTF16TestString(ir)) begin
            @test haskey(mod.functions, "parsed")
        end
        @test_throws LLVMException parse(LLVM.Module, sub("garbage"))

        @dispose buf=MemoryBuffer(UInt8[1, 2, 3], sub("büffer")) begin
            @test convert(Vector{UInt8}, buf) == UInt8[1, 2, 3]
        end
        mktemp() do path, io
            write(io, "contents")
            close(io)
            @dispose buf=MemoryBufferFile(SubString(path * "x", 1, ncodeunits(path))) begin
                @test String(convert(Vector{UInt8}, buf)) == "contents"
            end
        end
    end
end

@testset "targets" begin
    LLVM.InitializeNativeTarget()
    LLVM.InitializeNativeAsmPrinter()
    triple = LLVM.default_triple()
    @test LLVM.normalize(sub(triple)) == LLVM.normalize(triple)
    target = LLVM.Target(; triple)
    @dispose tm=LLVM.TargetMachine(target, sub(triple); cpu=sub(LLVM.host_cpu_name()),
                                   features=sub("")) begin
        @test tm.triple == triple
        @test tm.cpu == LLVM.host_cpu_name()
        @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
            mktemp() do path, io
                LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile,
                          SubString(path * "x", 1, ncodeunits(path)))
                @test isfile(path)
                @test !isfile(path * "x")
            end
        end
    end
    @dispose tm=LLVM.JITTargetMachine(; triple=sub(triple), cpu=sub(""),
                                      features=sub("")) begin
        # JITTargetMachine forces ELF on Windows
        @test tm.triple == LLVM.normalize(triple) * (Sys.iswindows() ? "-elf" : "")
    end

    if :X86 in LLVM.backends()
        LLVM.InitializeX86TargetInfo()
        LLVM.InitializeX86TargetMC()
        LLVM.InitializeX86Disassembler()
        LLVM.Disassembler(sub("x86_64-pc-linux-gnu"); cpu=sub(""),
                          features=sub("")) do dis
            insts = collect(LLVM.disassemble(dis, UInt8[0xc3]))
            @test only(insts).text == "\tretq"
        end
    end
end

@testset "execution" begin
    LLVM.InitializeNativeTarget()
    LLVM.InitializeNativeAsmPrinter()
    @dispose ctx=Context() begin
        mod = parse(LLVM.Module, """
            define i32 @answer() {
              ret i32 42
            }""")
        @dispose engine=LLVM.JIT(mod) begin
            @test engine.functions[sub("answer")] isa LLVM.Function
            @test haskey(engine.functions, sub("answer"))
            @test get(engine.functions, sub("other"), nothing) === nothing
            @test ccall(LLVM.lookup(engine, sub("answer")), Int32, ()) == 42
        end
    end

    @dispose ts_ctx=ThreadSafeContext() tsm=ThreadSafeModule(sub("tsm")) begin
        tsm() do mod
            @test mod.name == "tsm"
        end
    end

    @dispose lljit=LLJIT() begin
        es = lljit.execution_session
        @dispose oll=ObjectLinkingLayer(es, sub(LLVM.default_triple())) begin end
        sym = LLVM.intern(es, sub("symböl"))
        @test String(sym) == "symböl"
        LLVM.release(sym)
    end
end

@testset "NUL bytes" begin
    # strings with NUL bytes can't be passed to LLVM, as before
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
        @test_throws ArgumentError mod.functions[sub("a\0b")]
        @test_throws ArgumentError mod.aliases[sub("a\0b")]
        @test_throws ArgumentError MDString(sub("a\0b"))
        @test_throws ArgumentError LLVM.Module(sub("a\0b"))
    end
end

end
