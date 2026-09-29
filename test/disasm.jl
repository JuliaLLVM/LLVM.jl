@testset "disassembler" begin

if :X86 in LLVM.backends()
@testset "x86" begin
    LLVM.InitializeX86TargetInfo()
    LLVM.InitializeX86TargetMC()
    LLVM.InitializeX86Disassembler()

    # a function captured with perf's jitdump support; `address` is the address the code
    # was loaded at, which is used to report the address of every instruction.
    code = UInt8[0x55,                                                  # push rbp
                 0x48, 0x89, 0xe5,                                      # mov rbp, rsp
                 0x49, 0x8b, 0x45, 0x10,                                # mov rax, [r13+16]
                 0x48, 0x8b, 0x40, 0x10,                                # mov rax, [rax+16]
                 0x48, 0x8b, 0x00,                                      # mov rax, [rax]
                 0x48, 0x8b, 0x16,                                      # mov rdx, [rsi]
                 0x48, 0x83, 0xc6, 0x08,                                # add rsi, 8
                 0x48, 0xb8, 0xe0, 0x7c, 0xf5, 0x81, 0xe4, 0x7f, 0x00, 0x00, # movabs rax, ...
                 0xff, 0xd0,                                            # call rax
                 0x5d,                                                  # pop rbp
                 0xc3]                                                  # ret
    address = 0x00007fe48befcde0

    LLVM.Disassembler("x86_64-pc-linux-gnu") do dis
        insts = collect(LLVM.disassemble(dis, code; address))
        @test eltype(insts) == @NamedTuple{address::UInt64, size::Int, text::Union{String,Nothing}}
        @test length(insts) == 11
        @test sum(inst.size for inst in insts) == length(code)
        @test first(insts).address == address
        @test last(insts).address == address + length(code) - 1
        @test all(insts[i+1].address == insts[i].address + insts[i].size for i in 1:length(insts)-1)
        @test [inst.text for inst in insts] ==
              ["\tpushq\t%rbp", "\tmovq\t%rsp, %rbp", "\tmovq\t16(%r13), %rax",
               "\tmovq\t16(%rax), %rax", "\tmovq\t(%rax), %rax", "\tmovq\t(%rsi), %rdx",
               "\taddq\t\$8, %rsi", "\tmovabsq\t\$140619409620192, %rax", "\tcallq\t*%rax",
               "\tpopq\t%rbp", "\tretq"]

        # the address defaults to zero
        @test first(LLVM.disassemble(dis, code)).address == 0

        # printing
        @test sprint(LLVM.disassemble, dis, code) ==
              join([inst.text for inst in insts], '\n') * '\n'

        # any vector of bytes is accepted
        @test collect(LLVM.disassemble(dis, view(code, 1:4); address)) == insts[1:2]
        @test collect(LLVM.disassemble(dis, @view code[end:-1:end-1])) ==
              [(; address=UInt64(0), size=1, text="\tretq"),
               (; address=UInt64(1), size=1, text="\tpopq\t%rbp")]

        # empty input
        @test isempty(LLVM.disassemble(dis, UInt8[]))
    end

    # bytes that cannot be decoded are skipped one at a time
    LLVM.Disassembler("x86_64-pc-linux-gnu") do dis
        # 0x06 (push es) is invalid in 64-bit mode
        bad = UInt8[0x06, 0xc3]
        @test collect(LLVM.disassemble(dis, bad; address=0x10)) ==
              [(; address=UInt64(0x10), size=1, text=nothing),
               (; address=UInt64(0x11), size=1, text="\tretq")]
        @test sprint(LLVM.disassemble, dis, bad) == "\t.byte\t0x06\n\tretq\n"

        # a truncated instruction at the end of the input
        @test [inst.text for inst in LLVM.disassemble(dis, code[1:end-3])][end] === nothing
    end

    # options
    LLVM.Disassembler("x86_64-pc-linux-gnu"; alternate_syntax=true) do dis
        @test sprint(LLVM.disassemble, dis, code[1:4]) == "\tpush\trbp\n\tmov\trbp, rsp\n"
    end
    LLVM.Disassembler("x86_64-pc-linux-gnu"; hex_immediates=true) do dis
        @test sprint(LLVM.disassemble, dis, code[19:22]) == "\taddq\t\$0x8, %rsi\n"
    end
    LLVM.Disassembler("x86_64-pc-linux-gnu"; alternate_syntax=true, hex_immediates=true) do dis
        @test sprint(LLVM.disassemble, dis, code[19:22]) == "\tadd\trsi, 0x8\n"
    end
    LLVM.Disassembler("x86_64-pc-linux-gnu"; comments=true) do dis
        @test sprint(LLVM.disassemble, dis, code[1:4]) == "\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n"
    end

    # CPU and features
    avx = UInt8[0xc5, 0xfc, 0x58, 0xc1]     # vaddps ymm0, ymm0, ymm1
    LLVM.Disassembler("x86_64-pc-linux-gnu"; cpu="haswell") do dis
        @test only(LLVM.disassemble(dis, avx)).text == "\tvaddps\t%ymm1, %ymm0, %ymm0"
    end

    # manual disposal
    dis = LLVM.Disassembler("x86_64-pc-linux-gnu")
    @test only(LLVM.disassemble(dis, UInt8[0xc3])).text == "\tretq"
    dispose(dis)
end
end

if :AArch64 in LLVM.backends()
@testset "AArch64" begin
    LLVM.InitializeAArch64TargetInfo()
    LLVM.InitializeAArch64TargetMC()
    LLVM.InitializeAArch64Disassembler()

    code = UInt8[0x00, 0x04, 0x00, 0x91,    # add x0, x0, #1
                 0xc0, 0x03, 0x5f, 0xd6]    # ret
    LLVM.Disassembler("aarch64-linux-gnu") do dis
        @test collect(LLVM.disassemble(dis, code; address=0x1000)) ==
              [(; address=UInt64(0x1000), size=4, text="\tadd\tx0, x0, #1"),
               (; address=UInt64(0x1004), size=4, text="\tret")]
    end
    LLVM.Disassembler("aarch64-linux-gnu"; alternate_syntax=true) do dis
        @test sprint(LLVM.disassemble, dis, code) == "\tadd\tx0, x0, #1\n\tret\n"
    end
end
end

@test_throws ArgumentError LLVM.Disassembler("unknown-unknown-unknown")

if :BPF in LLVM.backends()
    LLVM.InitializeBPFTargetInfo()
    LLVM.InitializeBPFTargetMC()
    LLVM.InitializeBPFDisassembler()
    @test_throws ArgumentError LLVM.Disassembler("bpfel"; alternate_syntax=true)
end
@test_throws ArgumentError LLVM.Disassembler("wasm32-unknown-unknown"; alternate_syntax=true)

end
