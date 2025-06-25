export MemoryBuffer, MemoryBufferFile, dispose

"""
    MemoryBuffer

A memory buffer representing a simple block of memory.
"""
@checked struct MemoryBuffer
    ref::API.LLVMMemoryBufferRef
end

Base.unsafe_convert(::Type{API.LLVMMemoryBufferRef}, membuf::MemoryBuffer) =
    mark_use(membuf).ref

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

function MemoryBuffer(f::Core.Function, args...; kwargs...)
    membuf = MemoryBuffer(args...; kwargs...)
    try
        f(membuf)
    finally
        dispose(membuf)
    end
end

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

function MemoryBufferFile(f::Core.Function, args...; kwargs...)
    membuf = MemoryBufferFile(args...; kwargs...)
    try
        f(membuf)
    finally
        dispose(membuf)
    end
end

"""
    dispose(membuf::MemoryBuffer)

Dispose of the given memory buffer.
"""
dispose(membuf::MemoryBuffer) = mark_dispose(API.LLVMDisposeMemoryBuffer, membuf)

Base.length(membuf::MemoryBuffer) = API.LLVMGetBufferSize(membuf)

Base.pointer(membuf::MemoryBuffer) = convert(Ptr{UInt8}, API.LLVMGetBufferStart(membuf))

Base.convert(::Type{Vector{UInt8}}, membuf::MemoryBuffer) =
    copy(unsafe_wrap(Array, pointer(membuf), length(membuf)))
