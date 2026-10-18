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


@testset "subtargets" begin

LLVM.InitializeAllTargetInfos()
LLVM.InitializeAllTargetMCs()

# what LLVM prints to stderr while running f
function llvm_stderr(f)
    mktemp() do path, io
        redirect_stderr(f, io)
        close(io)
        read(path, String)
    end
end

if :X86 in LLVM.backends()
    LLVM.InitializeX86Target()
    x86 = LLVM.Target(triple="x86_64-unknown-linux-gnu")
    x86_triple = LLVM.Triple("x86_64-unknown-linux-gnu")

    let sti = LLVM.SubtargetInfo(x86, x86_triple)
        @test sti.triple == "x86_64-unknown-linux-gnu"
        @test sti.cpu isa String
        @test sti.features isa String
        @test propertynames(sti) == (:triple, :cpu, :features)

        names = LLVM.processor_features(sti)
        @test issorted(names)
        @test "avx2" in names
        @test "fma" in names

        # the generic CPU has the x86-64 baseline
        @test LLVM.hasfeature(sti, "sse2")
        @test !LLVM.hasfeature(sti, "avx2")
        @test LLVM.hasfeatures(sti, "+sse2,-avx2")
        @test !LLVM.hasfeatures(sti, "+sse2,+avx2")
        @test LLVM.hasfeatures(sti, "")
        @test LLVM.hasfeatures(sti, ",+sse2,")

        # every flag is checked by itself, also on versions of LLVM whose checkFeatures
        # requires the features that `avx` implies, like `sse2`, to be disabled too
        @test LLVM.hasfeatures(sti, "-avx")

        # unknown features and malformed flags
        @test_throws ArgumentError LLVM.hasfeature(sti, "bogus")
        @test_throws ArgumentError LLVM.hasfeature(sti, "+sse2")
        @test_throws ArgumentError LLVM.hasfeatures(sti, "sse2")
        @test_throws ArgumentError LLVM.hasfeatures(sti, "+bogus")
        @test_throws ArgumentError LLVM.hasfeatures(sti, "+avx2,+bogus")

        dispose(sti)
    end

    # CPU tables
    LLVM.SubtargetInfo(x86, "x86_64-unknown-linux-gnu"; cpu="haswell") do sti
        @test sti.cpu == "haswell"
        @test LLVM.hasfeature(sti, "avx2")
        @test LLVM.hasfeature(sti, "fma")
        @test !LLVM.hasfeature(sti, "avx512f")
    end

    has(features, name; cpu="") =
        LLVM.SubtargetInfo(sti -> LLVM.hasfeature(sti, name), x86, x86_triple; cpu, features)

    # implied features
    @test has("+avx2", "avx")
    @test !has("+avx2", "fma")
    @test has("+avx512f", "fma")
    LLVM.SubtargetInfo(x86, x86_triple; features="+avx2") do sti
        @test occursin("+avx2", sti.features)
        @test LLVM.hasfeatures(sti, "+avx2,+avx,-fma")
    end

    # the last setting wins, and disabling a feature disables the features that imply it
    @test !has("+fma,-fma", "fma")
    @test has("-fma,+fma", "fma")
    @test !has("+avx2,-avx", "avx2")
    @test !has("-fma", "fma"; cpu="haswell")

    # unknown features and CPUs are ignored, with a warning
    let output = llvm_stderr() do
            @test !has("+bogus", "avx")
        end
        @test occursin("'+bogus' is not a recognized feature", output)
    end
    let output = llvm_stderr() do
            @test has("", "sse2"; cpu="bogus")
        end
        @test occursin("'bogus' is not a recognized processor", output)
    end

    # the subtarget of a function under a target machine
    @dispose ctx=Context() tm=LLVM.TargetMachine(x86, string(x86_triple); cpu="haswell",
                                                 features="+avx512f") mod=parse(LLVM.Module, """
            define void @plain() {
              ret void
            }
            define void @with_cpu() #0 {
              ret void
            }
            define void @with_features() #1 {
              ret void
            }
            attributes #0 = { "target-cpu"="x86-64" }
            attributes #1 = { "target-features"="+sse4.1" }""") begin
        let sti = LLVM.SubtargetInfo(tm, mod.functions["plain"])
            @test sti.triple == "x86_64-unknown-linux-gnu"
            @test sti.cpu == "haswell"
            @test LLVM.hasfeature(sti, "avx512f")
            @test LLVM.hasfeature(sti, "bmi2")
            # it belongs to the target machine
            dispose(sti)
            @test sti.cpu == "haswell"
        end

        # the function's attributes replace the target machine's CPU and features
        let sti = LLVM.SubtargetInfo(tm, mod.functions["with_cpu"])
            @test sti.cpu == "x86-64"
            @test LLVM.hasfeature(sti, "avx512f")
            @test !LLVM.hasfeature(sti, "bmi2")     # a feature of haswell
        end
        let sti = LLVM.SubtargetInfo(tm, mod.functions["with_features"])
            @test sti.cpu == "haswell"
            @test LLVM.hasfeature(sti, "sse4.1")
            @test LLVM.hasfeature(sti, "avx2")  # from the CPU
            @test !LLVM.hasfeature(sti, "avx512f")
        end
    end
end

if :AArch64 in LLVM.backends()
    LLVM.InitializeAArch64Target()
    aarch64 = LLVM.Target(triple="aarch64-unknown-linux-gnu")

    LLVM.SubtargetInfo(aarch64, "aarch64-unknown-linux-gnu") do sti
        @test "neon" in LLVM.processor_features(sti)
        @test LLVM.hasfeature(sti, "neon")
    end
    LLVM.SubtargetInfo(aarch64, "aarch64-unknown-linux-gnu"; features="-neon") do sti
        @test !LLVM.hasfeature(sti, "neon")
    end
end

if :NVPTX in LLVM.backends()
    nvptx = LLVM.Target(triple="nvptx64-nvidia-cuda")

    LLVM.SubtargetInfo(nvptx, "nvptx64-nvidia-cuda"; cpu="sm_70", features="+ptx64") do sti
        @test sti.triple == "nvptx64-nvidia-cuda"
        @test sti.cpu == "sm_70"
        @test LLVM.hasfeature(sti, "sm_70")
        @test LLVM.hasfeature(sti, "ptx64")
        @test !LLVM.hasfeature(sti, "sm_80")
        # features of other targets aren't known
        @test "fma" ∉ LLVM.processor_features(sti)
        @test_throws ArgumentError LLVM.hasfeature(sti, "fma")
    end
end

# targets that aren't registered
@test_throws ArgumentError LLVM.Target(triple="bogus-unknown-unknown")

end
