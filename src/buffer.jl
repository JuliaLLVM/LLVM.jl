@vocabulary IR MemoryBuffer, MemoryBufferFile, dispose

"""
    MemoryBuffer

A memory buffer representing a simple block of memory.

Some operations take ownership of a memory buffer, like adding an object file to a JIT or
lazily parsing bitcode. These consume the buffer: it can't be used anymore afterwards, and
disposing of it does nothing, so that it can be disposed of unconditionally, e.g., using
the do-block form of its constructor.
"""
mutable struct MemoryBuffer
    ref::API.LLVMMemoryBufferRef
    owned::Bool

    function MemoryBuffer(ref::API.LLVMMemoryBufferRef)
        ref == C_NULL && throw(UndefRefError())
        new(ref, true)
    end
end

Base.unsafe_convert(::Type{API.LLVMMemoryBufferRef}, membuf::MemoryBuffer) =
    check_owned(membuf).ref

consume!(membuf::MemoryBuffer) = consume_owned!(membuf)

function adopt(membuf::MemoryBuffer)
    membuf.owned || throw(ArgumentError("This MemoryBuffer has been consumed or disposed of"))
    return mark_adopt(membuf)
end

"""
    MemoryBuffer(data::Vector{T}, name::String="", copy::Bool=true)

Create a memory buffer from the given data. If `copy` is `true`, the data is copied into the
buffer. Otherwise, the user is responsible for keeping the data alive across the lifetime of
the buffer.

This object needs to be disposed of using [`dispose`](@ref).
"""
function MemoryBuffer(data::Vector{T}, name::String="", copy::Bool=true) where {T<:Union{UInt8,Int8}}
    ptr = pointer(data)
    len = Csize_t(length(data))
    membuf = if copy
        MemoryBuffer(API.LLVMCreateMemoryBufferWithMemoryRangeCopy(ptr, len, name))
    else
        MemoryBuffer(API.LLVMCreateMemoryBufferWithMemoryRange(ptr, len, name, false))
    end
    mark_alloc(membuf)
end

MemoryBuffer(f::Core.Function, args...; kwargs...) =
    with_disposal(f, MemoryBuffer(args...; kwargs...))

"""
    MemoryBufferFile(path::String)

Create a memory buffer from the contents of a file.

This object needs to be disposed of using [`dispose`](@ref).
"""
function MemoryBufferFile(path::String)
    out_ref = Ref{API.LLVMMemoryBufferRef}()

    out_error = Ref{Cstring}()
    status = API.LLVMCreateMemoryBufferWithContentsOfFile(path, out_ref, out_error) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end

    mark_alloc(MemoryBuffer(out_ref[]))
end

MemoryBufferFile(f::Core.Function, args...; kwargs...) =
    with_disposal(f, MemoryBufferFile(args...; kwargs...))

"""
    dispose(membuf::MemoryBuffer)

Dispose of the given memory buffer, unless it has been consumed.
"""
dispose(membuf::MemoryBuffer) = dispose_owned(API.LLVMDisposeMemoryBuffer, membuf)

Base.length(membuf::MemoryBuffer) = API.LLVMGetBufferSize(membuf)

Base.pointer(membuf::MemoryBuffer) = convert(Ptr{UInt8}, API.LLVMGetBufferStart(membuf))

Base.convert(::Type{Vector{UInt8}}, membuf::MemoryBuffer) =
    copy(unsafe_wrap(Array, pointer(membuf), length(membuf)))
