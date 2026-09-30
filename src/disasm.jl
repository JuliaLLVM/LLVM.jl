## disassembler

@public Disassembler, dispose, disassemble

"""
    Disassembler

A disassembler for machine code of a specific target, which decodes raw bytes into
textual assembly instructions.

See also: [`disassemble`](@ref).
"""
@checked struct Disassembler
    ref::API.LLVMDisasmContextRef
end

Base.unsafe_convert(::Type{API.LLVMDisasmContextRef}, dis::Disassembler) = mark_use(dis).ref

"""
    Disassembler(triple::String; [cpu::String], [features::String],
                 [hex_immediates::Bool], [alternate_syntax::Bool], [comments::Bool])

Create a disassembler for the given target triple, CPU, and features. This requires the
target's info, machine code layer and disassembler to be initialized, e.g., using
[`LLVM.InitializeAllTargetInfos`](@ref), [`LLVM.InitializeAllTargetMCs`](@ref) and
[`LLVM.InitializeAllDisassemblers`](@ref).

The following keyword arguments customize the textual output:
- `hex_immediates`: print immediate operands in hexadecimal;
- `alternate_syntax`: use the target's alternate assembly dialect, e.g., Intel instead of
  AT&T syntax on X86, or Apple syntax on AArch64;
- `comments`: annotate instructions with target-specific comments, if any.

This object needs to be disposed of using [`dispose`](@ref).
"""
function Disassembler(triple::String; cpu::String="", features::String="",
                      hex_immediates::Bool=false, alternate_syntax::Bool=false,
                      comments::Bool=false)
    # the WebAssembly back-end asserts when asked for an alternate syntax
    if alternate_syntax && first(split(triple, '-')) in ("wasm32", "wasm64")
        throw(ArgumentError("Target triple '$triple' does not have an alternate assembly syntax"))
    end

    ref = API.LLVMCreateDisasmCPUFeatures(triple, cpu, features, C_NULL, 0, C_NULL, C_NULL)
    if ref == C_NULL
        throw(ArgumentError("Cannot create a disassembler for triple '$triple'; make sure " *
                            "the target info, machine code layer and disassembler have " *
                            "been initialized"))
    end
    dis = mark_alloc(Disassembler(ref))

    # switching to the alternate syntax replaces the instruction printer, discarding any
    # printer options set before it, so it needs to be configured first.
    if alternate_syntax &&
       API.LLVMSetDisasmOptions(dis, API.LLVMDisassembler_Option_AsmPrinterVariant) == 0
        dispose(dis)
        throw(ArgumentError("Target triple '$triple' does not have an alternate assembly syntax"))
    end
    options = UInt64(0)
    hex_immediates && (options |= API.LLVMDisassembler_Option_PrintImmHex)
    comments && (options |= API.LLVMDisassembler_Option_SetInstrComments)
    if options != 0 && API.LLVMSetDisasmOptions(dis, options) == 0
        dispose(dis)
        throw(ArgumentError("Failed to configure the disassembler for triple '$triple'"))
    end

    return dis
end

"""
    dispose(dis::Disassembler)

Dispose of the given disassembler.
"""
dispose(dis::Disassembler) = mark_dispose(API.LLVMDisasmDispose, dis)

function Disassembler(f::Core.Function, args...; kwargs...)
    dis = Disassembler(args...; kwargs...)
    try
        f(dis)
    finally
        dispose(dis)
    end
end


## disassembly

@public DisassembledInstruction
const DisassembledInstruction =
    @NamedTuple{address::UInt64, size::Int, text::Union{String,Nothing}}

struct Disassembly{T<:DenseVector{UInt8}}
    dis::Disassembler
    code::T
    address::UInt64
    buf::Vector{UInt8}  # output buffer, grown when an instruction's text doesn't fit
end

"""
    disassemble(dis::Disassembler, code::AbstractVector{UInt8}; address::Integer=0)

Disassemble the machine code in `code`, returning a lazy iterator of instructions.
The `address` keyword specifies where the first byte of `code` resides in the target's
address space, which is used to compute the address of every instruction.

Each instruction is a named tuple with fields:
- `address::UInt64`: the address of the instruction;
- `size::Int`: the number of bytes the instruction occupies;
- `text::Union{String,Nothing}`: the textual representation of the instruction, or
  `nothing` if the bytes could not be decoded. In that case, `size` is 1 and disassembly
  resumes at the next byte, which may not be an instruction boundary on targets with
  fixed-width instructions.

The iterator borrows the disassembler, so it must be consumed before the disassembler is
disposed of. The disassembler must not be used by multiple tasks concurrently.

# Examples

```julia
Disassembler("x86_64-linux-gnu") do dis
    for (; address, text) in disassemble(dis, code; address=0x1000)
        println(string(address; base=16), ": ", something(text, "<invalid>"))
    end
end
```
"""
function disassemble(dis::Disassembler, code::AbstractVector{UInt8}; address::Integer=0)
    # make sure we can take a pointer to the code
    code = code isa DenseVector{UInt8} ? code : Vector{UInt8}(code)
    Disassembly(dis, code, UInt64(address), Vector{UInt8}(undef, 256))
end

"""
    disassemble(io::IO, dis::Disassembler, code::AbstractVector{UInt8}; address::Integer=0)

Disassemble the machine code in `code`, and print the instructions to `io`, one per line.
Bytes that could not be decoded are printed as `.byte` directives.
"""
function disassemble(io::IO, dis::Disassembler, code::AbstractVector{UInt8}; kwargs...)
    iter = disassemble(dis, code; kwargs...)
    for (; address, text) in iter
        if text === nothing
            byte = iter.code[begin + Int(address - iter.address)]
            print(io, "\t.byte\t0x", string(byte; base=16, pad=2))
        else
            print(io, text)
        end
        println(io)
    end
    return
end

Base.IteratorSize(::Type{<:Disassembly}) = Base.SizeUnknown()
Base.eltype(::Type{<:Disassembly}) = DisassembledInstruction

function Base.iterate(iter::Disassembly, offset::Int=0)
    remaining = length(iter.code) - offset
    remaining <= 0 && return nothing
    address = iter.address + offset

    buf = iter.buf
    while true
        size = GC.@preserve iter buf begin
            API.LLVMDisasmInstruction(iter.dis, pointer(iter.code) + offset, remaining,
                                      address, pointer(buf), length(buf))
        end
        if size == 0
            # decoding failed. the C API does not tell us how many bytes to skip,
            # so resume at the next byte.
            return DisassembledInstruction((address, 1, nothing)), offset + 1
        end

        # the output is truncated to fit the buffer; if it's full, grow and try again
        len = something(findfirst(iszero, buf)) - 1
        if len == length(buf) - 1
            resize!(buf, 2 * length(buf))
            continue
        end

        text = GC.@preserve buf unsafe_string(pointer(buf), len)
        return DisassembledInstruction((address, size, text)), offset + Int(size)
    end
end
