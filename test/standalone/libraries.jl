# use a standalone LLVM library next to Julia's LLVM, in the same process
#
# usage: julia --project=test test/standalone/libraries.jl LIBRARY

using Test
using LLVM
using LLVM: dispose, @dispose, Library, library, library!, LLVMException

const lib = only(ARGS)
const IR = """
    define i32 @f(i32 %x) {
      %y = add i32 %x, 1
      ret i32 %y
    }"""

@testset "LLVM libraries" begin

julia = Library(:julia)
@test library() === julia

@testset "registration" begin
    other = LLVM.register_library!(:other, lib)
    @test other !== julia
    @test other.name === :other
    @test Library(:other) === other
    # registering the same library again, also under another name, gives the same one
    @test LLVM.register_library!(:other, lib) === other
    @test LLVM.register_library!(:another, lib) === other
    @test_throws "reserved" LLVM.register_library!(:julia, lib)
    @test_throws "Another LLVM library" LLVM.register_library!(:other, LLVM.library().path)
    @test_throws "No LLVM library" Library(:nonexistent)
end
other = Library(:other)

@testset "selection" begin
    @test library!(library, other) === other
    @test library!(library, :other) === other
    @test library!(() -> LLVM.version(), other) == other.version
    @test library() === julia

    # nested scopes, and exceptions, restore the current library
    library!(other) do
        library!(julia) do
            library!(other) do
                @test library() === other
            end
            @test library() === julia
        end
        @test library() === other
        @test_throws ErrorException library!(julia) do
            error("oops")
        end
        @test library() === other
    end
    @test library() === julia

    # tasks inherit the current library
    @test library!(() -> fetch(Threads.@spawn library()), other) === other

    # contexts select their library
    LLVM.Context() do julia_ctx
        LLVM.Context(; library=other) do ctx
            @test ctx.library === other
            @test library() === other
            LLVM.context!(julia_ctx) do
                @test library() === julia
                @test LLVM.context() == julia_ctx
            end
            @test LLVM.context() == ctx
        end
        @test library() === julia
    end
end

@testset "contexts" begin
    # each library has its own active contexts, so contexts of different libraries can be
    # active at the same time, and their lifetimes can interleave
    julia_ctx = LLVM.Context()
    other_ctx = LLVM.Context(; library=other)
    @test LLVM.context() == julia_ctx
    @test library!(LLVM.context, other) == other_ctx
    inner_ctx = LLVM.Context()
    @test LLVM.context() == inner_ctx
    # contexts of one library are deactivated in reverse order
    @test_throws "Deactivating wrong context" dispose(julia_ctx)
    dispose(inner_ctx)
    @test LLVM.context() == julia_ctx
    dispose(julia_ctx)
    @test LLVM.context(; throw_error=false) === nothing
    @test_throws "No LLVM context of LLVM library :julia is active, but one of :other is" LLVM.context()
    @test library!(LLVM.context, other) == other_ctx
    dispose(other_ctx)
    @test_throws "No LLVM context is active" LLVM.context()

    # thread-safe contexts too, and thread-safe modules use those of their module's library
    LLVM.ThreadSafeContext() do julia_ts_ctx
        other_ts_ctx = LLVM.ThreadSafeContext(; library=other)
        @test LLVM.ts_context() == julia_ts_ctx
        @test library!(LLVM.ts_context, other) == other_ts_ctx
        mod = LLVM.context!(LLVM.context(other_ts_ctx)) do
            parse(LLVM.Module, IR)
        end
        LLVM.ThreadSafeModule(mod) do tsm
            @test tsm.library === other
        end
        dispose(other_ts_ctx)
        @test LLVM.ts_context(; throw_error=false) == julia_ts_ctx
    end
end

@testset "IR" begin
    # parse IR from Julia's LLVM, process it with the other library, and keep the module
    # after leaving the library's scope
    bitcode = LLVM.Context() do ctx
        @dispose mod=parse(LLVM.Module, IR) begin
            convert(Vector{UInt8}, mod)
        end
    end
    ctx, mod = library!(other) do
        ctx = LLVM.Context()
        mod = if other.version >= LLVM.Library(:julia).version
            # bitcode is only compatible with newer versions of LLVM
            parse(LLVM.Module, bitcode)
        else
            parse(LLVM.Module, IR)
        end
        @test mod.library === other
        @test mod.context == ctx
        gv = LLVM.GlobalVariable(mod, LLVM.Int32Type(), "gv")
        push!(mod.used, gv)
        mod.flags["flag", LLVM.API.LLVMModuleFlagBehaviorWarning] =
            LLVM.Metadata(LLVM.ConstantInt(Int32(1)))
        LLVM.deactivate(ctx)
        ctx, mod
    end
    @test library() === julia
    @test occursin("add i32 %x, 1", string(mod))
    # views of the module use its library
    @test count(Returns(true), mod.functions) == 1
    @test only(collect(mod.used)) isa LLVM.GlobalVariable
    @test only(collect(mod.flags)).first == "flag"
    @test mod.flags["flag"] isa LLVM.Metadata
    @test sprint(show, MIME"text/plain"(), mod) == strip(string(mod))
    @test mod.name == ""
    @test mod.context.library === other
    copied = copy(mod)
    @test copied.library === other
    dispose(copied)
    @test LLVM.verification_error(mod) === nothing
    # passes run with the library of the module
    LLVM.run!("instcombine", mod)
    @test !isempty(convert(Vector{UInt8}, mod))
    io = IOBuffer()
    write(io, mod)
    @test position(io) > 0

    # Julia's LLVM keeps working
    LLVM.Context() do ctx
        @dispose julia_mod=parse(LLVM.Module, IR) begin
            @test julia_mod.library === julia
            # but objects of different libraries can't be mixed
            @test_throws "belongs to LLVM library :other" LLVM.link!(julia_mod, mod)
        end
    end
    # nor can contexts of another library be used implicitly
    LLVM.activate(ctx)
    @test_throws "No LLVM context of LLVM library :julia is active, but one of :other is" LLVM.Module("foo")
    @test library!(() -> LLVM.Module("foo"), other) isa LLVM.Module
    dispose(ctx)
end

@testset "custom passes" begin
    LLVM.Context(; library=other) do ctx
        @dispose mod=parse(LLVM.Module, IR) pb=LLVM.PassBuilder() begin
            seen = Ref{Any}(nothing)
            pass = LLVM.ModulePass("seen", m -> (seen[] = library(); false))
            LLVM.register!(pb, pass)
            LLVM.add!(pb, pass)
            LLVM.add!(pb, "default<O2>")
            LLVM.run!(pb, mod)
            @test seen[] === other
        end
    end
end

@testset "passes" begin
    # the names of passes depend on the version of the library they are used with
    flatten(lib) = lib.version >= v"19" ? "flatten-cfg" : "flattencfg"
    @test library!(LLVM.Passes.FlattenCFGPass, other) == flatten(other)
    @test LLVM.Passes.FlattenCFGPass() == flatten(julia)
    @test ("flatten-cfg" in library!(LLVM.function_passes, other)) == (other.version >= v"19")
    if other.version >= v"21"
        @test_throws "only on LLVM 20 and earlier" library!(LLVM.Passes.InstrOrderFilePass, other)
    end

    # Julia's passes only work with Julia's LLVM
    @test "RemoveNI" in LLVM.module_passes(julia)
    @test !("RemoveNI" in LLVM.module_passes(other))
    @test_throws "requires the LLVM library that Julia uses" library!(LLVM.Interop.AllocOptPass, other)
    LLVM.Context(; library=other) do ctx
        @dispose mod=parse(LLVM.Module, IR) begin
            @test_throws "Julia's function pass `AllocOpt` requires the LLVM library that Julia uses" LLVM.run!("AllocOpt", mod)
        end
    end
end

@testset "version-dependent functionality" begin
    # the same code works with libraries of different versions, also when switching back
    # and forth between them
    ir = """
        define i32 @g(i64 %x) {
          %t = trunc i64 %x to i32
          ret i32 %t
        }"""
    function check(lib)
        @test library() === lib
        LLVM.Context() do ctx
            @test !LLVM.supports_typed_pointers(ctx) || lib.version < v"17"
            @dispose mod=parse(LLVM.Module, ir) begin
                @test LLVM.version(mod) == lib.version
                trunc = first(first(first(mod.functions).blocks).instructions)
                @test hasproperty(trunc, :nuw) == (lib.version >= v"19")
                @test (:nuw in propertynames(trunc)) == (lib.version >= v"19")
                @test LLVM.ArrayType(LLVM.Int8Type(), 4).length == 4
            end
        end
        @test LLVM.Passes.FlattenCFGPass() ==
              (lib.version >= v"19" ? "flatten-cfg" : "flattencfg")
    end
    for lib in (julia, other, julia, other)
        library!(() -> check(lib), lib)
    end

    # objects use the version of their library, also when another library is current
    ctx = LLVM.Context(; library=other)
    @test library() === julia
    @test LLVM.version(ctx) == other.version
    other.version >= v"17" && @test !LLVM.supports_typed_pointers(ctx)
    LLVM.opaque_pointers!(ctx, true)
    dispose(ctx)
    @test_throws "does not remember its LLVM library" LLVM.version(1)
end

@testset "code generation" begin
    LLVM.Context(; library=other) do ctx
        LLVM.JITTargetMachine() do tm
            @test tm.library === other
            @test tm.target.library === other
            @dispose mod=parse(LLVM.Module, IR) begin
                asm = String(LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile))
                @test occursin("f", asm)
            end
        end
    end
end

@testset "ORC" begin
    # a JIT and its objects remember their library, also when used outside of its scope
    jit, obj = library!(other) do
        jit = LLVM.LLJIT()
        obj = LLVM.Context() do ctx
            @dispose mod=parse(LLVM.Module, IR) tm=LLVM.JITTargetMachine() begin
                mod.triple = jit.triple
                mod.datalayout = jit.datalayout_string
                LLVM.emit(tm, mod, LLVM.API.LLVMObjectFile)
            end
        end
        jit, obj
    end
    @test jit.library === other
    jd = jit.main_dylib
    @test jd.library === other
    @test jit.execution_session.library === other
    sym = LLVM.mangle(jit, "f")
    @test sym.library === other
    LLVM.release(sym)
    transformed = Ref(0)
    LLVM.transform!(jit.ir_transform_layer) do tsm, mr
        @test library() === other
        transformed[] += 1
    end

    # objects of another library are rejected before being consumed
    julia_buf = LLVM.MemoryBuffer(obj)
    @test_throws "belongs to LLVM library :julia" LLVM.add!(jit, jd, julia_buf)
    @test julia_buf.owned
    LLVM.LLJIT() do julia_jit
        @test_throws "belongs to LLVM library :other" LLVM.add!(julia_jit, jd, julia_buf)
        @test julia_buf.owned
    end
    dispose(julia_buf)

    buf = library!(() -> LLVM.MemoryBuffer(obj), other)
    LLVM.add!(jit, jd, buf)
    @test ccall(pointer(LLVM.lookup(jit, "f")), Int32, (Int32,), 41) == 42

    # IR modules pass through the transform layer
    tsm = library!(other) do
        LLVM.ThreadSafeContext() do ts_ctx
            mod = LLVM.context!(LLVM.context(ts_ctx)) do
                mod = parse(LLVM.Module, replace(IR, "@f" => "@g"))
                mod.triple = jit.triple
                mod.datalayout = jit.datalayout_string
                mod
            end
            LLVM.ThreadSafeModule(mod)
        end
    end
    LLVM.add!(jit, jd, tsm)
    @test ccall(pointer(LLVM.lookup(jit, "g")), Int32, (Int32,), 1) == 2
    @test transformed[] == 1
    # errors are reported by the library of the JIT
    @test_throws LLVMException LLVM.lookup(jit, "undefined")
    dispose(jit)

    # target machines are only consumed by a JIT of their library
    tm = library!(LLVM.JITTargetMachine, other)
    jit = LLVM.LLJIT(; tm)
    @test jit.library === other
    @test !tm.owned
    dispose(jit)
end

@testset "Julia integration" begin
    # Julia's code generator uses Julia's LLVM, also when another library is current
    f(x) = LLVM.Interop.@asmcall("", "=r,0", Int, Tuple{Int}, x)
    @test library!(() -> f(42), other) == 42
    library!(other) do
        @test LLVM.Interop.isboxed(Any)
        LLVM.Context() do ctx
            @test_throws "requires the LLVM library that Julia uses" convert(LLVM.LLVMType, Int)
        end
    end
end

end
