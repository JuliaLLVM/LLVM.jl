@checked struct DisasmContext
    ref::API.LLVMDisasmContextRef
end
Base.unsafe_convert(::Type{API.LLVMDisasmContextRef}, dc::DisasmContext) = dc.ref

function create_disasm(triple)
    ctx = DisasmContext(API.LLVMCreateDisasm(triple, C_NULL, 0, C_NULL, C_NULL))
    return ctx
end

function dispose(ctx::DisasmContext)
    API.LLVMDisasmDispose(ctx.ref)
    return nothing
end

function disassemble_code(io, ctx::DisasmContext, native_code::Vector{UInt8}, code_addr::Csize_t = 0; bufsize=32)
    pos = 1
    buf = Vector{UInt8}(undef, bufsize)

    while pos < length(native_code)
        GC.@preserve native_code buf begin
            bytes = pointer(native_code, pos)
            pc = code_addr + pos - 1
            nbytes = length(native_code) - pos + 1
            # Need to use pointer(buf) here since the API is annotated as Cstring
            nb = API.LLVMDisasmInstruction(ctx, bytes, nbytes, pc, pointer(buf), bufsize)
        end
        if nb == 0
            # Disassembly failed
            break
        end
        pos += nb
        for byte in buf
            byte == 0x00 && break
            write(io, byte)
        end
        write(io, '\n')
    end
    return nothing
end