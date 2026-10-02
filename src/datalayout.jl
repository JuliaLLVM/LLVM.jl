## data layout

@public DataLayout, dispose, pointersize, intptr, bit_size, storage_size, abi_size,
        abi_alignment, frame_alignment, preferred_alignment, element_at, offsetof

"""
    DataLayout

A parsed version of the target data layout string in and methods for querying it.

The target data layout string is specified by the target - a frontend generating LLVM IR is
required to generate the right target data for the target being codegen'd to.

# Properties

    dl.byteorder

The byte order of the data layout, `LLVM.ByteOrdering.Big` or `LLVM.ByteOrdering.Little`.

    dl.globals_addrspace

The address space used for global variables by the data layout.
"""
DataLayout
# forward definition of DataLayout in src/module.jl

Base.unsafe_convert(::Type{API.LLVMTargetDataRef}, dl::DataLayout) = mark_use(dl).ref

"""
    DataLayout(rep::AbstractString)

Create a target data layout from the given string representation.

This object needs to be disposed of using [`dispose`](@ref).
"""
DataLayout(rep::AbstractString) = mark_alloc(DataLayout(API.LLVMCreateTargetData(rep)))

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

DataLayout(f::Core.Function, args...; kwargs...) =
    with_disposal(f, DataLayout(args...; kwargs...))

Base.string(dl::DataLayout) =
    unsafe_message(API.LLVMCopyStringRepOfTargetData(dl))

function Base.show(io::IO, dl::DataLayout)
    @printf(io, "DataLayout(%s)", string(dl))
end

byteorder(dl::DataLayout) = API.LLVMByteOrder(dl)

@property DataLayout byteorder

"""
    pointersize(dl::DataLayout, [addrspace::Integer])

Get the size of pointers in the given address space (0 by default) for the target data
layout, in bytes.
"""
pointersize(dl::DataLayout, addrspace::Integer=0) =
    Int(API.LLVMPointerSizeForAS(dl, addrspace))

"""
    intptr(dl::DataLayout, [addrspace::Integer])

Get the integer type that is the same size as a pointer for the target data layout.
"""
intptr(dl::DataLayout, addrspace::Integer=0) =
    IntegerType(API.LLVMIntPtrTypeForASInContext(context(), dl, addrspace))

globals_addrspace(dl::DataLayout) = API.LLVMGlobalsAddressSpace(dl) |> Int

@property DataLayout globals_addrspace

function check_layout_type(typ::LLVMType)
    issized(typ) || throw(ArgumentError("type $typ has no fixed layout"))
    return typ
end

"""
    bit_size(dl::DataLayout, typ::LLVMType)

Get the size of the given type in bits for the target data layout, like C++'s
`DataLayout::getTypeSizeInBits`, e.g., 1 for `i1`.

See also: [`storage_size`](@ref), [`abi_size`](@ref).
"""
bit_size(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMSizeOfTypeInBits(dl, check_layout_type(typ)))

"""
    storage_size(dl::DataLayout, typ::LLVMType)

Get the number of bytes that storing a value of the given type may overwrite, for the
target data layout, like C++'s `DataLayout::getTypeStoreSize`, e.g., 1 for `i1` and 2 for
`i9`.
"""
storage_size(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMStoreSizeOfType(dl, check_layout_type(typ)))

"""
    abi_size(dl::DataLayout, typ::LLVMType)

Get the offset in bytes between successive values of the given type in memory, including
alignment padding, for the target data layout, like C++'s `DataLayout::getTypeAllocSize`.
"""
abi_size(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMABISizeOfType(dl, check_layout_type(typ)))

"""
    abi_alignment(dl::DataLayout, typ::LLVMType)

Get the ABI alignment of the given type in bytes for the target data layout.
"""
abi_alignment(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMABIAlignmentOfType(dl, check_layout_type(typ)))

"""
    frame_alignment(dl::DataLayout, typ::LLVMType)

Get the call frame alignment of the given type in bytes for the target data layout.
"""
frame_alignment(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMCallFrameAlignmentOfType(dl, check_layout_type(typ)))


"""
    preferred_alignment(dl::DataLayout, typ::LLVMType)
    preferred_alignment(dl::DataLayout, var::GlobalVariable)

Get the preferred alignment of the given type or global variable in bytes for the target
data layout.
"""
preferred_alignment(::DataLayout, ::Union{LLVMType, GlobalVariable})

preferred_alignment(dl::DataLayout, typ::LLVMType) =
    Int(API.LLVMPreferredAlignmentOfType(dl, check_layout_type(typ)))
preferred_alignment(dl::DataLayout, var::GlobalVariable) =
    Int(API.LLVMPreferredAlignmentOfGlobal(dl, var))

"""
    element_at(dl::DataLayout, typ::StructType, offset::Integer)

Get the index of the element of a struct type that contains the given byte offset, for the
target data layout. Like the `elements` of the struct type, elements are numbered from 1.

See also: [`offsetof`](@ref).
"""
function element_at(dl::DataLayout, typ::StructType, offset::Integer)
    0 <= offset < abi_size(dl, typ) ||
        throw(ArgumentError("Offset $offset is outside of struct type $typ"))
    Int(API.LLVMElementAtOffset(dl, typ, Culonglong(offset))) + 1
end

"""
    offsetof(dl::DataLayout, typ::StructType, i::Integer)

Get the byte offset of the `i`th element of a struct type, for the target data layout.
Like the `elements` of the struct type, elements are numbered from 1.

See also: [`element_at`](@ref).
"""
function offsetof(dl::DataLayout, typ::StructType, i::Integer)
    check_layout_type(typ)
    1 <= i <= length(elements(typ)) || throw(BoundsError(elements(typ), i))
    Int(API.LLVMOffsetOfElement(dl, typ, i - 1))
end
