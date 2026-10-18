@testset "targetmachine" begin

host_triple = LLVM.default_triple()
host_t = LLVM.Target(triple=host_triple)

let
    tm = LLVM.TargetMachine(host_t, host_triple)
    dispose(tm)
end

LLVM.TargetMachine(host_t, host_triple) do tm
end

# the host CPU
@test LLVM.host_cpu_name() isa String
@test !isempty(LLVM.host_cpu_name())
@test LLVM.host_cpu_features() isa String
@dispose tm=LLVM.TargetMachine(host_t, host_triple; cpu=LLVM.host_cpu_name(),
                               features=LLVM.host_cpu_features(),
                               opt_level=LLVM.CodeGenOptLevel.Aggressive) begin
    @test tm.cpu == LLVM.host_cpu_name()
    @test tm.features == LLVM.host_cpu_features()
end
# the CPU and features are keyword arguments
@test_throws MethodError LLVM.TargetMachine(host_t, host_triple, LLVM.host_cpu_name())

LLVM.JITTargetMachine(; cpu=LLVM.host_cpu_name(), opt_level=LLVM.CodeGenOptLevel.None) do tm
    @test tm.cpu == LLVM.host_cpu_name()
end
@test_throws MethodError LLVM.JITTargetMachine(LLVM.default_triple())

@dispose tm=LLVM.TargetMachine(host_t, host_triple) begin
    @test tm.target == host_t
    @test tm.triple == host_triple
    @test tm.cpu == ""
    @test tm.features == ""
    LLVM.asm_verbosity!(tm, true)

    # emission
    @dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
        ft = LLVM.FunctionType(LLVM.VoidType())
        fn = LLVM.Function(mod, "SomeFunction", ft)

        entry = BasicBlock(fn, "entry")
        position!(builder, LLVM.at_end(entry))

        ret!(builder)

        asm = String(convert(Vector{UInt8}, LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile)))

        mktemp() do path, io
            LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile, path)
            @test asm == read(path, String)
        end

        bad_ir = raw"""define void @bad() {
          %x = call i8 asm sideeffect "mov %ah, ${0:h}", "=r,~{eax},~{ebx},~{ecx},~{edx},~{dirflag},~{fpsr},~{flags}"()
          ret void
        }"""
        @dispose badmod=parse(LLVM.Module, bad_ir) begin
            @test_throws LLVMException LLVM.emit(tm, badmod, LLVM.API.LLVMAssemblyFile)
            mktemp() do path, io
                @test_throws LLVMException LLVM.emit(tm, badmod, LLVM.API.LLVMAssemblyFile,
                                                    path)
            end
        end
        @test String(LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile)) == asm

        @test_throws LLVMException LLVM.emit(tm, mod, LLVM.API.LLVMAssemblyFile, "/")
    end

    dispose(LLVM.DataLayout(tm))
end

end


@testset "triples" begin

# triples are interpreted by LLVM as they are spelled
let triple = LLVM.Triple("x86_64-unknown-linux-gnu")
    @test string(triple) == "x86_64-unknown-linux-gnu"
    @test triple.arch == :x86_64
    @test triple.vendor == :unknown
    @test triple.os == :linux
    @test triple.environment == :gnu
    @test triple.object_format == :elf
    @test LLVM.isx86(triple)
    @test LLVM.islinux(triple)
    @test !LLVM.iswindows(triple)
    @test !LLVM.isdarwin(triple)
    @test !LLVM.isaarch64(triple)
    @test LLVM.normalize(triple) == triple
end
let triple = LLVM.Triple("i686-unknown-linux-gnu")
    @test triple.arch == :i386
    @test LLVM.isx86(triple)
end

# triples are not normalized, so spellings that only normalization understands aren't
# recognized, as with C++'s `Triple(M.getTargetTriple())`
let triple = LLVM.Triple("x86_64-w64-mingw32")
    @test LLVM.isx86(triple)
    @test !LLVM.iswindows(triple)
    @test triple.os == :unknown
    normalized = LLVM.normalize(triple)
    @test normalized isa LLVM.Triple
    @test normalized == LLVM.Triple("x86_64-w64-windows-gnu")
    @test LLVM.iswindows(normalized)
    @test normalized.environment == :gnu
    @test normalized.object_format == :coff
end
let triple = LLVM.Triple("aarch64-linux-gnu")
    @test LLVM.isaarch64(triple)
    @test !LLVM.islinux(triple)
    @test LLVM.islinux(LLVM.normalize(triple))
end

# the empty triple
let triple = LLVM.Triple("")
    @test string(triple) == ""
    @test triple.arch == :unknown
    @test triple.os == :unknown
    @test triple.environment == :unknown
    @test !LLVM.isx86(triple)
    @test !LLVM.islinux(triple)
end

# architectures and operating systems
let triple = LLVM.Triple("thumbv7-unknown-linux-gnueabihf")
    @test triple.arch == :thumb
    @test LLVM.isthumb(triple)
    @test !LLVM.isarm(triple)
    @test triple.environment == :gnueabihf
end
let triple = LLVM.Triple("armv7l-unknown-linux-gnueabihf")
    @test triple.arch == :arm
    @test LLVM.isarm(triple)
    @test !LLVM.isthumb(triple)
end
let triple = LLVM.Triple("riscv64-unknown-linux-gnu")
    @test triple.arch == :riscv64
    @test LLVM.isriscv(triple)
end
let triple = LLVM.Triple("powerpc64le-unknown-linux-gnu")
    @test triple.arch == :powerpc64le
    @test LLVM.isppc(triple)
end
let triple = LLVM.Triple("aarch64-apple-darwin")
    @test triple.arch == :aarch64
    @test triple.vendor == :apple
    @test triple.os == :darwin
    @test triple.object_format == :macho
    @test LLVM.isaarch64(triple)
    @test LLVM.isdarwin(triple)
    @test !LLVM.islinux(triple)
end
let triple = LLVM.Triple("arm64-apple-macosx")
    @test triple.arch == :aarch64
    @test LLVM.isdarwin(triple)
end
let triple = LLVM.Triple("x86_64-pc-windows-msvc")
    @test LLVM.iswindows(triple)
    @test triple.environment == :msvc
    @test triple.object_format == :coff
end
let triple = LLVM.Triple("amdgcn-amd-amdhsa")
    # LLVM 23 renamed the architecture
    @test triple.arch == (LLVM.version() >= v"23" ? :amdgpu : :amdgcn)
    @test triple.os == :amdhsa
    @test LLVM.isamdgpu(triple)
end
let triple = LLVM.Triple("nvptx64-nvidia-cuda")
    @test triple.arch == :nvptx64
    @test triple.os == :cuda
    @test LLVM.isnvptx(triple)
    @test !LLVM.isamdgpu(triple)
end
let triple = LLVM.Triple("spirv64")
    @test triple.arch == :spirv64
    @test triple.object_format == :spirv
    @test LLVM.isspirv(triple)
end
let triple = LLVM.Triple("wasm32-unknown-unknown")
    @test triple.object_format == :wasm
    @test LLVM.iswasm(triple)
end

# triples are values
@test LLVM.Triple("x86_64-unknown-linux-gnu") == LLVM.Triple(SubString("x86_64-unknown-linux-gnu-", 1, 24))
@test LLVM.Triple("x86_64-linux-gnu") != LLVM.Triple("x86_64-unknown-linux-gnu")
@test hash(LLVM.Triple("spirv64")) == hash(LLVM.Triple("spirv64"))
@test repr(LLVM.Triple("spirv64")) == "LLVM.Triple(\"spirv64\")"
@test propertynames(LLVM.Triple("spirv64")) ==
      (:arch, :vendor, :os, :environment, :object_format)

# the triple of a module
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    mod.triple = "nvptx64-nvidia-cuda"
    @test LLVM.isnvptx(LLVM.Triple(mod.triple))
end

end
