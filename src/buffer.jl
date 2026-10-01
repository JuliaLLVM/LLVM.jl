@vocabulary IR MemoryBuffer, MemoryBufferFile, dispose

"""
    MemoryBuffer

A memory buffer representing a simple block of memory.

Some operations take ownership of a memory buffer, like adding an object file to a JIT or
lazily parsing bitcode. These consume the buffer: it can't be used anymore afterwards, and
disposing of it does nothing, so that it can be disposed of unconditionally, e.g., using
the do-block form of its constructor.

Foreign code that takes ownership of a buffer can also keep it alive and let callers keep
reading it, e.g., clang's `SourceManager` when creating a file from a buffer. To hand a
buffer over to such code while keeping it usable, use
[`LLVM.consume!(buf; borrow=true)`](@ref LLVM.consume!).
"""
mutable struct MemoryBuffer
    ref::API.LLVMMemoryBufferRef
    owned::Bool     # whether we own the buffer, i.e., it wasn't consumed or disposed of
    borrowed::Bool  # whether the buffer was handed over, but can still be used

    function MemoryBuffer(ref::API.LLVMMemoryBufferRef)
        ref == C_NULL && throw(UndefRefError())
        new(ref, true, false)
    end
end

function check_usable(membuf::MemoryBuffer)
    membuf.borrowed && return membuf
    return check_owned(membuf)
end

Base.unsafe_convert(::Type{API.LLVMMemoryBufferRef}, membuf::MemoryBuffer) =
    check_usable(membuf).ref

function consume!(membuf::MemoryBuffer; borrow::Bool=false)
    membuf.borrowed && throw(ArgumentError("A borrowed MemoryBuffer can't be consumed"))
    borrow || return consume_owned!(membuf)
    check_owned(membuf)
    membuf.owned = false
    membuf.borrowed = true
    # the buffer is owned elsewhere now, so stop tracking it, without considering it
    # disposed of (which would make using it look like a use after free)
    mark_untracked(membuf)
    return membuf.ref
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

Dispose of the given memory buffer, unless it has been consumed. Disposing of a buffer that
was consumed with `borrow=true` does nothing, and leaves it usable.
"""
dispose(membuf::MemoryBuffer) = dispose_owned(API.LLVMDisposeMemoryBuffer, membuf)

Base.length(membuf::MemoryBuffer) = API.LLVMGetBufferSize(membuf)

Base.pointer(membuf::MemoryBuffer) = convert(Ptr{UInt8}, API.LLVMGetBufferStart(membuf))

Base.convert(::Type{Vector{UInt8}}, membuf::MemoryBuffer) =
    copy(unsafe_wrap(Array, pointer(membuf), length(membuf)))
