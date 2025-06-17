## data layout

export DataLayout, dispose,
       byteorder, pointersize, intptr, globals_addrspace,
       sizeof, storage_size, abi_size,
       abi_alignment, frame_alignment, preferred_alignment,
       element_at, offsetof

"""
    DataLayout

A parsed version of the target data layout string in and methods for querying it.

The target data layout string is specified by the target - a frontend generating LLVM IR is
required to generate the right target data for the target being codegen'd to.
"""
DataLayout
# forward definition of DataLayout in src/module.jl

Base.unsafe_convert(::Type{API.LLVMTargetDataRef}, dl::DataLayout) = mark_use(dl).ref

"""
    DataLayout(rep::String)

Create a target data layout from the given string representation.

This object needs to be disposed of using [`dispose`](@ref).
"""
DataLayout(rep::String) = mark_alloc(DataLayout(API.LLVMCreateTargetData(rep)))

"""
    DataLayout(tm::TargetMachine)

Create a target data layout from the given target machine.

This object needs to be disposed of using [`dispose`](@ref).
"""
DataLayout(tm::TargetMachine) = mark_alloc(DataLayout(API.LLVMCreateTargetDataLayout(tm)))

"""
    dispose(dl::DataLayout)

Dispose of the given target data layout.
"""
dispose(dl::DataLayout) = mark_dispose(API.LLVMDisposeTargetData, dl)

function DataLayout(f::Core.Function, args...; kwargs...)
    dl = DataLayout(args...; kwargs...)
    try
        f(dl)
    finally
        dispose(dl)
    end
end

Base.string(dl::DataLayout) =
    unsafe_message(API.LLVMCopyStringRepOfTargetData(dl))

function Base.show(io::IO, dl::DataLayout)
    @printf(io, "DataLayout(%s)", string(dl))
end

"""
    byteorder(dl::DataLayout)

Get the byte order of the target data layout.
"""
byteorder(dl::DataLayout) = API.LLVMByteOrder(dl)

"""
    pointersize(dl::DataLayout, [addrspace::Integer])

Get the pointer size of the target data layout.
"""
pointersize(dl::DataLayout, addrspace::Integer=0) =
    API.LLVMPointerSizeForAS(dl, addrspace)

"""
    intptr(dl::DataLayout, [addrspace::Integer])

Get the integer type that is the same size as a pointer for the target data layout.
"""
intptr(dl::DataLayout, addrspace::Integer=0) =
    IntegerType(API.LLVMIntPtrTypeForASInContext(context(), dl, addrspace))

"""
    globals_addrspace(dl::DataLayout)

Get the address space used for global variables in the target data layout.
"""
globals_addrspace(dl::DataLayout) = API.LLVMGlobalsAddressSpace(dl) |> Int

"""
    sizeof(dl::DataLayout, typ::LLVMType)

Get the size of the given type in bytes for the target data layout.
"""
Base.sizeof(dl::DataLayout, typ::LLVMType) = Int(API.LLVMSizeOfTypeInBits(dl, typ) / 8)

"""
    storage_size(dl::DataLayout, typ::LLVMType)

Get the storage size of the given type in bytes for the target data layout.
"""
storage_size(dl::DataLayout, typ::LLVMType) = API.LLVMStoreSizeOfType(dl, typ)

"""
    abi_size(dl::DataLayout, typ::LLVMType)

Get the ABI size of the given type in bytes for the target data layout.
"""
abi_size(dl::DataLayout, typ::LLVMType) = API.LLVMABISizeOfType(dl, typ)

"""
    abi_alignment(dl::DataLayout, typ::LLVMType)

Get the ABI alignment of the given type in bytes for the target data layout.
"""
abi_alignment(dl::DataLayout, typ::LLVMType) =
    API.LLVMABIAlignmentOfType(dl, typ)

"""
    frame_alignment(dl::DataLayout, typ::LLVMType)

Get the call frame alignment of the given type in bytes for the target data layout.
"""
frame_alignment(dl::DataLayout, typ::LLVMType) =
    API.LLVMCallFrameAlignmentOfType(dl, typ)


"""
    preferred_alignment(dl::DataLayout, typ::LLVMType)
    preferred_alignment(dl::DataLayout, var::GlobalVariable)

Get the preferred alignment of the given type or global variable in bytes for the target
data layout.
"""
preferred_alignment(::DataLayout, ::Union{LLVMType, GlobalVariable})

preferred_alignment(dl::DataLayout, typ::LLVMType) =
    API.LLVMPreferredAlignmentOfType(dl, typ)
preferred_alignment(dl::DataLayout, var::GlobalVariable) =
    API.LLVMPreferredAlignmentOfGlobal(dl, var)

"""
    element_at(dl::DataLayout, typ::StructType, offset::Integer)

Get the element at the given offset in a struct type for the target data layout.

See also: [`offsetof`](@ref).
"""
element_at(dl::DataLayout, typ::StructType, offset::Integer) =
    API.LLVMElementAtOffset(dl, typ, Culonglong(offset))

"""
    offsetof(dl::DataLayout, typ::StructType, element::Integer)

Get the offset of the given element in a struct type for the target data layout.

See also: [`element_at`](@ref).
"""
offsetof(dl::DataLayout, typ::StructType, element::Integer) =
    API.LLVMOffsetOfElement(dl, typ, element)
