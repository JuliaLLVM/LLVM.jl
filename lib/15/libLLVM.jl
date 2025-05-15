using CEnum

const IS_LIBC_MUSL = occursin("musl", Base.MACHINE)

if Sys.islinux() && Sys.ARCH === :aarch64 && !IS_LIBC_MUSL
    const __off_t = Clong
    const off_t = __off_t
elseif Sys.islinux() && Sys.ARCH === :aarch64 && IS_LIBC_MUSL
    const off_t = Clong
elseif Sys.islinux() && startswith(string(Sys.ARCH), "arm") && !IS_LIBC_MUSL
    const __off_t = Clong
    const off_t = __off_t
elseif Sys.islinux() && startswith(string(Sys.ARCH), "arm") && IS_LIBC_MUSL
    const off_t = Clonglong
elseif Sys.islinux() && Sys.ARCH === :i686 && !IS_LIBC_MUSL
    const __off_t = Clong
    const off_t = __off_t
elseif Sys.islinux() && Sys.ARCH === :i686 && IS_LIBC_MUSL
    const off_t = Clonglong
elseif Sys.iswindows() && Sys.ARCH === :i686
    const off32_t = Clong
    const off_t = off32_t
elseif Sys.islinux() && Sys.ARCH === :powerpc64le
    const __off_t = Clong
    const off_t = __off_t
elseif Sys.isapple()
    const __darwin_off_t = Int64
    const off_t = __darwin_off_t
elseif Sys.islinux() && Sys.ARCH === :x86_64 && !IS_LIBC_MUSL
    const __off_t = Clong
    const off_t = __off_t
elseif Sys.islinux() && Sys.ARCH === :x86_64 && IS_LIBC_MUSL
    const off_t = Clong
elseif Sys.isbsd() && !Sys.isapple()
    const __off_t = Int64
    const off_t = __off_t
elseif Sys.iswindows() && Sys.ARCH === :x86_64
    const off32_t = Clong
    const off_t = off32_t
end



mutable struct LLVMOpaqueMemoryBuffer end

mutable struct LLVMOpaqueContext end

mutable struct LLVMOpaqueModule end

mutable struct LLVMOpaqueType end

mutable struct LLVMOpaqueValue end

mutable struct LLVMOpaqueBasicBlock end

mutable struct LLVMOpaqueMetadata end

mutable struct LLVMOpaqueNamedMDNode end

mutable struct LLVMOpaqueValueMetadataEntry end

mutable struct LLVMOpaqueBuilder end

mutable struct LLVMOpaqueDIBuilder end

mutable struct LLVMOpaqueModuleProvider end

mutable struct LLVMOpaquePassManager end

mutable struct LLVMOpaquePassRegistry end

mutable struct LLVMOpaqueUse end

mutable struct LLVMOpaqueAttributeRef end

mutable struct LLVMOpaqueDiagnosticInfo end

mutable struct LLVMComdat end

mutable struct LLVMOpaqueModuleFlagEntry end

mutable struct LLVMOpaqueJITEventListener end

mutable struct LLVMOpaqueBinary end

mutable struct LLVMOpaqueTargetData end

mutable struct LLVMOpaqueTargetLibraryInfotData end

mutable struct LLVMOpaqueTargetMachine end

mutable struct LLVMTarget end

mutable struct LLVMOpaqueError end

mutable struct LLVMOrcOpaqueExecutionSession end

mutable struct LLVMOrcOpaqueSymbolStringPool end

mutable struct LLVMOrcOpaqueSymbolStringPoolEntry end

mutable struct LLVMOrcOpaqueJITDylib end

mutable struct LLVMOrcOpaqueMaterializationUnit end

mutable struct LLVMOrcOpaqueMaterializationResponsibility end

mutable struct LLVMOrcOpaqueResourceTracker end

mutable struct LLVMOrcOpaqueDefinitionGenerator end

mutable struct LLVMOrcOpaqueLookupState end

mutable struct LLVMOrcOpaqueThreadSafeContext end

mutable struct LLVMOrcOpaqueThreadSafeModule end

mutable struct LLVMOrcOpaqueJITTargetMachineBuilder end

mutable struct LLVMOrcOpaqueObjectLayer end

mutable struct LLVMOrcOpaqueObjectLinkingLayer end

mutable struct LLVMOrcOpaqueIRTransformLayer end

mutable struct LLVMOrcOpaqueObjectTransformLayer end

mutable struct LLVMOrcOpaqueIndirectStubsManager end

mutable struct LLVMOrcOpaqueLazyCallThroughManager end

mutable struct LLVMOrcOpaqueDumpObjects end

mutable struct LLVMOpaqueGenericValue end

mutable struct LLVMOpaqueExecutionEngine end

mutable struct LLVMOpaqueMCJITMemoryManager end

"""
    LLVMVerifierFailureAction

` LLVMCAnalysis Analysis`

` LLVMC`

@{
"""
@cenum LLVMVerifierFailureAction::UInt32 begin
    LLVMAbortProcessAction = 0
    LLVMPrintMessageAction = 1
    LLVMReturnStatusAction = 2
end

"""
The top-level container for all other LLVM Intermediate Representation (IR) objects.

# See also
llvm::Module
"""
const LLVMModuleRef = Ptr{LLVMOpaqueModule}

"""
` LLVMCSupportTypes Types and Enumerations`

@{
"""
const LLVMBool = Cint

function LLVMVerifyModule(M, Action, OutMessage)
    ccall((:LLVMVerifyModule, libllvm), LLVMBool, (LLVMModuleRef, LLVMVerifierFailureAction, Ptr{Cstring}), M, Action, OutMessage)
end

"""
Represents an individual value in LLVM IR.

This models llvm::Value.
"""
const LLVMValueRef = Ptr{LLVMOpaqueValue}

function LLVMVerifyFunction(Fn, Action)
    ccall((:LLVMVerifyFunction, libllvm), LLVMBool, (LLVMValueRef, LLVMVerifierFailureAction), Fn, Action)
end

function LLVMViewFunctionCFG(Fn)
    ccall((:LLVMViewFunctionCFG, libllvm), Cvoid, (LLVMValueRef,), Fn)
end

function LLVMViewFunctionCFGOnly(Fn)
    ccall((:LLVMViewFunctionCFGOnly, libllvm), Cvoid, (LLVMValueRef,), Fn)
end

"""
Used to pass regions of memory through LLVM interfaces.

# See also
llvm::MemoryBuffer
"""
const LLVMMemoryBufferRef = Ptr{LLVMOpaqueMemoryBuffer}

"""
    LLVMParseBitcode(MemBuf, OutModule, OutMessage)

` LLVMCBitReader Bit Reader`

` LLVMC`

@{
"""
function LLVMParseBitcode(MemBuf, OutModule, OutMessage)
    ccall((:LLVMParseBitcode, libllvm), LLVMBool, (LLVMMemoryBufferRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), MemBuf, OutModule, OutMessage)
end

function LLVMParseBitcode2(MemBuf, OutModule)
    ccall((:LLVMParseBitcode2, libllvm), LLVMBool, (LLVMMemoryBufferRef, Ptr{LLVMModuleRef}), MemBuf, OutModule)
end

"""
The top-level container for all LLVM global data. See the LLVMContext class.
"""
const LLVMContextRef = Ptr{LLVMOpaqueContext}

function LLVMParseBitcodeInContext(ContextRef, MemBuf, OutModule, OutMessage)
    ccall((:LLVMParseBitcodeInContext, libllvm), LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), ContextRef, MemBuf, OutModule, OutMessage)
end

function LLVMParseBitcodeInContext2(ContextRef, MemBuf, OutModule)
    ccall((:LLVMParseBitcodeInContext2, libllvm), LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, Ptr{LLVMModuleRef}), ContextRef, MemBuf, OutModule)
end

"""
    LLVMGetBitcodeModuleInContext(ContextRef, MemBuf, OutM, OutMessage)

Reads a module from the specified path, returning via the OutMP parameter a module provider which performs lazy deserialization. Returns 0 on success. Optionally returns a human-readable error message via OutMessage. This is deprecated. Use [`LLVMGetBitcodeModuleInContext2`](@ref).
"""
function LLVMGetBitcodeModuleInContext(ContextRef, MemBuf, OutM, OutMessage)
    ccall((:LLVMGetBitcodeModuleInContext, libllvm), LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), ContextRef, MemBuf, OutM, OutMessage)
end

"""
    LLVMGetBitcodeModuleInContext2(ContextRef, MemBuf, OutM)

Reads a module from the given memory buffer, returning via the OutMP parameter a module provider which performs lazy deserialization.

Returns 0 on success.

Takes ownership of `MemBuf` if (and only if) the module was read successfully.
"""
function LLVMGetBitcodeModuleInContext2(ContextRef, MemBuf, OutM)
    ccall((:LLVMGetBitcodeModuleInContext2, libllvm), LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, Ptr{LLVMModuleRef}), ContextRef, MemBuf, OutM)
end

function LLVMGetBitcodeModule(MemBuf, OutM, OutMessage)
    ccall((:LLVMGetBitcodeModule, libllvm), LLVMBool, (LLVMMemoryBufferRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), MemBuf, OutM, OutMessage)
end

function LLVMGetBitcodeModule2(MemBuf, OutM)
    ccall((:LLVMGetBitcodeModule2, libllvm), LLVMBool, (LLVMMemoryBufferRef, Ptr{LLVMModuleRef}), MemBuf, OutM)
end

"""
    LLVMWriteBitcodeToFile(M, Path)

Writes a module to the specified path. Returns 0 on success.
"""
function LLVMWriteBitcodeToFile(M, Path)
    ccall((:LLVMWriteBitcodeToFile, libllvm), Cint, (LLVMModuleRef, Cstring), M, Path)
end

"""
    LLVMWriteBitcodeToFD(M, FD, ShouldClose, Unbuffered)

Writes a module to an open file descriptor. Returns 0 on success.
"""
function LLVMWriteBitcodeToFD(M, FD, ShouldClose, Unbuffered)
    ccall((:LLVMWriteBitcodeToFD, libllvm), Cint, (LLVMModuleRef, Cint, Cint, Cint), M, FD, ShouldClose, Unbuffered)
end

"""
    LLVMWriteBitcodeToFileHandle(M, Handle)

Deprecated for [`LLVMWriteBitcodeToFD`](@ref). Writes a module to an open file descriptor. Returns 0 on success. Closes the Handle.
"""
function LLVMWriteBitcodeToFileHandle(M, Handle)
    ccall((:LLVMWriteBitcodeToFileHandle, libllvm), Cint, (LLVMModuleRef, Cint), M, Handle)
end

"""
    LLVMWriteBitcodeToMemoryBuffer(M)

Writes a module to a new memory buffer and returns it.
"""
function LLVMWriteBitcodeToMemoryBuffer(M)
    ccall((:LLVMWriteBitcodeToMemoryBuffer, libllvm), LLVMMemoryBufferRef, (LLVMModuleRef,), M)
end

"""
    LLVMComdatSelectionKind

` LLVMCCoreComdat Comdats`

` LLVMCCore`

@{

| Enumerator                           | Note                                                      |
| :----------------------------------- | :-------------------------------------------------------- |
| LLVMAnyComdatSelectionKind           | The linker may choose any COMDAT.                         |
| LLVMExactMatchComdatSelectionKind    | The data referenced by the COMDAT must be the same.       |
| LLVMLargestComdatSelectionKind       | The linker will choose the largest COMDAT.                |
| LLVMNoDeduplicateComdatSelectionKind | No deduplication is performed.                            |
| LLVMSameSizeComdatSelectionKind      | The data referenced by the COMDAT must be the same size.  |
"""
@cenum LLVMComdatSelectionKind::UInt32 begin
    LLVMAnyComdatSelectionKind = 0
    LLVMExactMatchComdatSelectionKind = 1
    LLVMLargestComdatSelectionKind = 2
    LLVMNoDeduplicateComdatSelectionKind = 3
    LLVMSameSizeComdatSelectionKind = 4
end

"""
# See also
llvm::Comdat
"""
const LLVMComdatRef = Ptr{LLVMComdat}

"""
    LLVMGetOrInsertComdat(M, Name)

Return the Comdat in the module with the specified name. It is created if it didn't already exist.

# See also
llvm::Module::getOrInsertComdat()
"""
function LLVMGetOrInsertComdat(M, Name)
    ccall((:LLVMGetOrInsertComdat, libllvm), LLVMComdatRef, (LLVMModuleRef, Cstring), M, Name)
end

"""
    LLVMGetComdat(V)

Get the Comdat assigned to the given global object.

# See also
llvm::GlobalObject::getComdat()
"""
function LLVMGetComdat(V)
    ccall((:LLVMGetComdat, libllvm), LLVMComdatRef, (LLVMValueRef,), V)
end

"""
    LLVMSetComdat(V, C)

Assign the Comdat to the given global object.

# See also
llvm::GlobalObject::setComdat()
"""
function LLVMSetComdat(V, C)
    ccall((:LLVMSetComdat, libllvm), Cvoid, (LLVMValueRef, LLVMComdatRef), V, C)
end

function LLVMGetComdatSelectionKind(C)
    ccall((:LLVMGetComdatSelectionKind, libllvm), LLVMComdatSelectionKind, (LLVMComdatRef,), C)
end

function LLVMSetComdatSelectionKind(C, Kind)
    ccall((:LLVMSetComdatSelectionKind, libllvm), Cvoid, (LLVMComdatRef, LLVMComdatSelectionKind), C, Kind)
end

"""
    LLVMOpcode

External users depend on the following values being stable. It is not safe to reorder them.
"""
@cenum LLVMOpcode::UInt32 begin
    LLVMRet = 1
    LLVMBr = 2
    LLVMSwitch = 3
    LLVMIndirectBr = 4
    LLVMInvoke = 5
    LLVMUnreachable = 7
    LLVMCallBr = 67
    LLVMFNeg = 66
    LLVMAdd = 8
    LLVMFAdd = 9
    LLVMSub = 10
    LLVMFSub = 11
    LLVMMul = 12
    LLVMFMul = 13
    LLVMUDiv = 14
    LLVMSDiv = 15
    LLVMFDiv = 16
    LLVMURem = 17
    LLVMSRem = 18
    LLVMFRem = 19
    LLVMShl = 20
    LLVMLShr = 21
    LLVMAShr = 22
    LLVMAnd = 23
    LLVMOr = 24
    LLVMXor = 25
    LLVMAlloca = 26
    LLVMLoad = 27
    LLVMStore = 28
    LLVMGetElementPtr = 29
    LLVMTrunc = 30
    LLVMZExt = 31
    LLVMSExt = 32
    LLVMFPToUI = 33
    LLVMFPToSI = 34
    LLVMUIToFP = 35
    LLVMSIToFP = 36
    LLVMFPTrunc = 37
    LLVMFPExt = 38
    LLVMPtrToInt = 39
    LLVMIntToPtr = 40
    LLVMBitCast = 41
    LLVMAddrSpaceCast = 60
    LLVMICmp = 42
    LLVMFCmp = 43
    LLVMPHI = 44
    LLVMCall = 45
    LLVMSelect = 46
    LLVMUserOp1 = 47
    LLVMUserOp2 = 48
    LLVMVAArg = 49
    LLVMExtractElement = 50
    LLVMInsertElement = 51
    LLVMShuffleVector = 52
    LLVMExtractValue = 53
    LLVMInsertValue = 54
    LLVMFreeze = 68
    LLVMFence = 55
    LLVMAtomicCmpXchg = 56
    LLVMAtomicRMW = 57
    LLVMResume = 58
    LLVMLandingPad = 59
    LLVMCleanupRet = 61
    LLVMCatchRet = 62
    LLVMCatchPad = 63
    LLVMCleanupPad = 64
    LLVMCatchSwitch = 65
end

"""
    LLVMTypeKind

| Enumerator                 | Note                                            |
| :------------------------- | :---------------------------------------------- |
| LLVMVoidTypeKind           | type with no size                               |
| LLVMHalfTypeKind           | 16 bit floating point type                      |
| LLVMFloatTypeKind          | 32 bit floating point type                      |
| LLVMDoubleTypeKind         | 64 bit floating point type                      |
| LLVMX86\\_FP80TypeKind     | 80 bit floating point type (X87)                |
| LLVMFP128TypeKind          | 128 bit floating point type (112-bit mantissa)  |
| LLVMPPC\\_FP128TypeKind    | 128 bit floating point type (two 64-bits)       |
| LLVMLabelTypeKind          | Labels                                          |
| LLVMIntegerTypeKind        | Arbitrary bit width integers                    |
| LLVMFunctionTypeKind       | Functions                                       |
| LLVMStructTypeKind         | Structures                                      |
| LLVMArrayTypeKind          | Arrays                                          |
| LLVMPointerTypeKind        | Pointers                                        |
| LLVMVectorTypeKind         | Fixed width SIMD vector type                    |
| LLVMMetadataTypeKind       | Metadata                                        |
| LLVMX86\\_MMXTypeKind      | X86 MMX                                         |
| LLVMTokenTypeKind          | Tokens                                          |
| LLVMScalableVectorTypeKind | Scalable SIMD vector type                       |
| LLVMBFloatTypeKind         | 16 bit brain floating point type                |
| LLVMX86\\_AMXTypeKind      | X86 AMX                                         |
"""
@cenum LLVMTypeKind::UInt32 begin
    LLVMVoidTypeKind = 0
    LLVMHalfTypeKind = 1
    LLVMFloatTypeKind = 2
    LLVMDoubleTypeKind = 3
    LLVMX86_FP80TypeKind = 4
    LLVMFP128TypeKind = 5
    LLVMPPC_FP128TypeKind = 6
    LLVMLabelTypeKind = 7
    LLVMIntegerTypeKind = 8
    LLVMFunctionTypeKind = 9
    LLVMStructTypeKind = 10
    LLVMArrayTypeKind = 11
    LLVMPointerTypeKind = 12
    LLVMVectorTypeKind = 13
    LLVMMetadataTypeKind = 14
    LLVMX86_MMXTypeKind = 15
    LLVMTokenTypeKind = 16
    LLVMScalableVectorTypeKind = 17
    LLVMBFloatTypeKind = 18
    LLVMX86_AMXTypeKind = 19
end

"""
    LLVMLinkage

| Enumerator                     | Note                                               |
| :----------------------------- | :------------------------------------------------- |
| LLVMExternalLinkage            | Externally visible function                        |
| LLVMLinkOnceAnyLinkage         | Keep one copy of function when linking (inline)    |
| LLVMLinkOnceODRLinkage         | Same, but only replaced by something equivalent.   |
| LLVMLinkOnceODRAutoHideLinkage | Obsolete                                           |
| LLVMWeakAnyLinkage             | Keep one copy of function when linking (weak)      |
| LLVMWeakODRLinkage             | Same, but only replaced by something equivalent.   |
| LLVMAppendingLinkage           | Special purpose, only applies to global arrays     |
| LLVMInternalLinkage            | Rename collisions when linking (static functions)  |
| LLVMPrivateLinkage             | Like Internal, but omit from symbol table          |
| LLVMDLLImportLinkage           | Obsolete                                           |
| LLVMDLLExportLinkage           |                                                    |
| LLVMExternalWeakLinkage        | ExternalWeak linkage description                   |
| LLVMGhostLinkage               | Obsolete                                           |
| LLVMCommonLinkage              | Tentative definitions                              |
| LLVMLinkerPrivateLinkage       | Like Private, but linker removes.                  |
| LLVMLinkerPrivateWeakLinkage   | Like LinkerPrivate, but is weak.                   |
"""
@cenum LLVMLinkage::UInt32 begin
    LLVMExternalLinkage = 0
    LLVMAvailableExternallyLinkage = 1
    LLVMLinkOnceAnyLinkage = 2
    LLVMLinkOnceODRLinkage = 3
    LLVMLinkOnceODRAutoHideLinkage = 4
    LLVMWeakAnyLinkage = 5
    LLVMWeakODRLinkage = 6
    LLVMAppendingLinkage = 7
    LLVMInternalLinkage = 8
    LLVMPrivateLinkage = 9
    LLVMDLLImportLinkage = 10
    LLVMDLLExportLinkage = 11
    LLVMExternalWeakLinkage = 12
    LLVMGhostLinkage = 13
    LLVMCommonLinkage = 14
    LLVMLinkerPrivateLinkage = 15
    LLVMLinkerPrivateWeakLinkage = 16
end

"""
    LLVMVisibility

| Enumerator              | Note                 |
| :---------------------- | :------------------- |
| LLVMDefaultVisibility   | The GV is visible    |
| LLVMHiddenVisibility    | The GV is hidden     |
| LLVMProtectedVisibility | The GV is protected  |
"""
@cenum LLVMVisibility::UInt32 begin
    LLVMDefaultVisibility = 0
    LLVMHiddenVisibility = 1
    LLVMProtectedVisibility = 2
end

"""
    LLVMUnnamedAddr

| Enumerator            | Note                                          |
| :-------------------- | :-------------------------------------------- |
| LLVMNoUnnamedAddr     | Address of the GV is significant.             |
| LLVMLocalUnnamedAddr  | Address of the GV is locally insignificant.   |
| LLVMGlobalUnnamedAddr | Address of the GV is globally insignificant.  |
"""
@cenum LLVMUnnamedAddr::UInt32 begin
    LLVMNoUnnamedAddr = 0
    LLVMLocalUnnamedAddr = 1
    LLVMGlobalUnnamedAddr = 2
end

"""
    LLVMDLLStorageClass

| Enumerator                | Note                                 |
| :------------------------ | :----------------------------------- |
| LLVMDLLImportStorageClass | Function to be imported from DLL.    |
| LLVMDLLExportStorageClass | Function to be accessible from DLL.  |
"""
@cenum LLVMDLLStorageClass::UInt32 begin
    LLVMDefaultStorageClass = 0
    LLVMDLLImportStorageClass = 1
    LLVMDLLExportStorageClass = 2
end

@cenum LLVMCallConv::UInt32 begin
    LLVMCCallConv = 0
    LLVMFastCallConv = 8
    LLVMColdCallConv = 9
    LLVMGHCCallConv = 10
    LLVMHiPECallConv = 11
    LLVMWebKitJSCallConv = 12
    LLVMAnyRegCallConv = 13
    LLVMPreserveMostCallConv = 14
    LLVMPreserveAllCallConv = 15
    LLVMSwiftCallConv = 16
    LLVMCXXFASTTLSCallConv = 17
    LLVMX86StdcallCallConv = 64
    LLVMX86FastcallCallConv = 65
    LLVMARMAPCSCallConv = 66
    LLVMARMAAPCSCallConv = 67
    LLVMARMAAPCSVFPCallConv = 68
    LLVMMSP430INTRCallConv = 69
    LLVMX86ThisCallCallConv = 70
    LLVMPTXKernelCallConv = 71
    LLVMPTXDeviceCallConv = 72
    LLVMSPIRFUNCCallConv = 75
    LLVMSPIRKERNELCallConv = 76
    LLVMIntelOCLBICallConv = 77
    LLVMX8664SysVCallConv = 78
    LLVMWin64CallConv = 79
    LLVMX86VectorCallCallConv = 80
    LLVMHHVMCallConv = 81
    LLVMHHVMCCallConv = 82
    LLVMX86INTRCallConv = 83
    LLVMAVRINTRCallConv = 84
    LLVMAVRSIGNALCallConv = 85
    LLVMAVRBUILTINCallConv = 86
    LLVMAMDGPUVSCallConv = 87
    LLVMAMDGPUGSCallConv = 88
    LLVMAMDGPUPSCallConv = 89
    LLVMAMDGPUCSCallConv = 90
    LLVMAMDGPUKERNELCallConv = 91
    LLVMX86RegCallCallConv = 92
    LLVMAMDGPUHSCallConv = 93
    LLVMMSP430BUILTINCallConv = 94
    LLVMAMDGPULSCallConv = 95
    LLVMAMDGPUESCallConv = 96
end

@cenum LLVMValueKind::UInt32 begin
    LLVMArgumentValueKind = 0
    LLVMBasicBlockValueKind = 1
    LLVMMemoryUseValueKind = 2
    LLVMMemoryDefValueKind = 3
    LLVMMemoryPhiValueKind = 4
    LLVMFunctionValueKind = 5
    LLVMGlobalAliasValueKind = 6
    LLVMGlobalIFuncValueKind = 7
    LLVMGlobalVariableValueKind = 8
    LLVMBlockAddressValueKind = 9
    LLVMConstantExprValueKind = 10
    LLVMConstantArrayValueKind = 11
    LLVMConstantStructValueKind = 12
    LLVMConstantVectorValueKind = 13
    LLVMUndefValueValueKind = 14
    LLVMConstantAggregateZeroValueKind = 15
    LLVMConstantDataArrayValueKind = 16
    LLVMConstantDataVectorValueKind = 17
    LLVMConstantIntValueKind = 18
    LLVMConstantFPValueKind = 19
    LLVMConstantPointerNullValueKind = 20
    LLVMConstantTokenNoneValueKind = 21
    LLVMMetadataAsValueValueKind = 22
    LLVMInlineAsmValueKind = 23
    LLVMInstructionValueKind = 24
    LLVMPoisonValueValueKind = 25
end

"""
    LLVMIntPredicate

| Enumerator | Note                       |
| :--------- | :------------------------- |
| LLVMIntEQ  | equal                      |
| LLVMIntNE  | not equal                  |
| LLVMIntUGT | unsigned greater than      |
| LLVMIntUGE | unsigned greater or equal  |
| LLVMIntULT | unsigned less than         |
| LLVMIntULE | unsigned less or equal     |
| LLVMIntSGT | signed greater than        |
| LLVMIntSGE | signed greater or equal    |
| LLVMIntSLT | signed less than           |
| LLVMIntSLE | signed less or equal       |
"""
@cenum LLVMIntPredicate::UInt32 begin
    LLVMIntEQ = 32
    LLVMIntNE = 33
    LLVMIntUGT = 34
    LLVMIntUGE = 35
    LLVMIntULT = 36
    LLVMIntULE = 37
    LLVMIntSGT = 38
    LLVMIntSGE = 39
    LLVMIntSLT = 40
    LLVMIntSLE = 41
end

"""
    LLVMRealPredicate

| Enumerator             | Note                                       |
| :--------------------- | :----------------------------------------- |
| LLVMRealPredicateFalse | Always false (always folded)               |
| LLVMRealOEQ            | True if ordered and equal                  |
| LLVMRealOGT            | True if ordered and greater than           |
| LLVMRealOGE            | True if ordered and greater than or equal  |
| LLVMRealOLT            | True if ordered and less than              |
| LLVMRealOLE            | True if ordered and less than or equal     |
| LLVMRealONE            | True if ordered and operands are unequal   |
| LLVMRealORD            | True if ordered (no nans)                  |
| LLVMRealUNO            | True if unordered: isnan(X) | isnan(Y)     |
| LLVMRealUEQ            | True if unordered or equal                 |
| LLVMRealUGT            | True if unordered or greater than          |
| LLVMRealUGE            | True if unordered, greater than, or equal  |
| LLVMRealULT            | True if unordered or less than             |
| LLVMRealULE            | True if unordered, less than, or equal     |
| LLVMRealUNE            | True if unordered or not equal             |
| LLVMRealPredicateTrue  | Always true (always folded)                |
"""
@cenum LLVMRealPredicate::UInt32 begin
    LLVMRealPredicateFalse = 0
    LLVMRealOEQ = 1
    LLVMRealOGT = 2
    LLVMRealOGE = 3
    LLVMRealOLT = 4
    LLVMRealOLE = 5
    LLVMRealONE = 6
    LLVMRealORD = 7
    LLVMRealUNO = 8
    LLVMRealUEQ = 9
    LLVMRealUGT = 10
    LLVMRealUGE = 11
    LLVMRealULT = 12
    LLVMRealULE = 13
    LLVMRealUNE = 14
    LLVMRealPredicateTrue = 15
end

"""
    LLVMLandingPadClauseTy

| Enumerator           | Note             |
| :------------------- | :--------------- |
| LLVMLandingPadCatch  | A catch clause   |
| LLVMLandingPadFilter | A filter clause  |
"""
@cenum LLVMLandingPadClauseTy::UInt32 begin
    LLVMLandingPadCatch = 0
    LLVMLandingPadFilter = 1
end

@cenum LLVMThreadLocalMode::UInt32 begin
    LLVMNotThreadLocal = 0
    LLVMGeneralDynamicTLSModel = 1
    LLVMLocalDynamicTLSModel = 2
    LLVMInitialExecTLSModel = 3
    LLVMLocalExecTLSModel = 4
end

"""
    LLVMAtomicOrdering

| Enumerator                               | Note                                                                                                                                                                            |
| :--------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| LLVMAtomicOrderingNotAtomic              | A load or store which is not atomic                                                                                                                                             |
| LLVMAtomicOrderingUnordered              | Lowest level of atomicity, guarantees somewhat sane results, lock free.                                                                                                         |
| LLVMAtomicOrderingMonotonic              | guarantees that if you take all the operations affecting a specific address, a consistent ordering exists                                                                       |
| LLVMAtomicOrderingAcquire                | Acquire provides a barrier of the sort necessary to acquire a lock to access other memory with normal loads and stores.                                                         |
| LLVMAtomicOrderingRelease                | Release is similar to Acquire, but with a barrier of the sort necessary to release a lock.                                                                                      |
| LLVMAtomicOrderingAcquireRelease         | provides both an Acquire and a Release barrier (for fences and operations which both read and write memory).                                                                    |
| LLVMAtomicOrderingSequentiallyConsistent | provides Acquire semantics for loads and Release semantics for stores. Additionally, it guarantees that a total ordering exists between all SequentiallyConsistent operations.  |
"""
@cenum LLVMAtomicOrdering::UInt32 begin
    LLVMAtomicOrderingNotAtomic = 0
    LLVMAtomicOrderingUnordered = 1
    LLVMAtomicOrderingMonotonic = 2
    LLVMAtomicOrderingAcquire = 4
    LLVMAtomicOrderingRelease = 5
    LLVMAtomicOrderingAcquireRelease = 6
    LLVMAtomicOrderingSequentiallyConsistent = 7
end

"""
    LLVMAtomicRMWBinOp

| Enumerator             | Note                                                                                                        |
| :--------------------- | :---------------------------------------------------------------------------------------------------------- |
| LLVMAtomicRMWBinOpXchg | Set the new value and return the one old                                                                    |
| LLVMAtomicRMWBinOpAdd  | Add a value and return the old one                                                                          |
| LLVMAtomicRMWBinOpSub  | Subtract a value and return the old one                                                                     |
| LLVMAtomicRMWBinOpAnd  | And a value and return the old one                                                                          |
| LLVMAtomicRMWBinOpNand | Not-And a value and return the old one                                                                      |
| LLVMAtomicRMWBinOpOr   | OR a value and return the old one                                                                           |
| LLVMAtomicRMWBinOpXor  | Xor a value and return the old one                                                                          |
| LLVMAtomicRMWBinOpMax  | Sets the value if it's greater than the original using a signed comparison and return the old one           |
| LLVMAtomicRMWBinOpMin  | Sets the value if it's Smaller than the original using a signed comparison and return the old one           |
| LLVMAtomicRMWBinOpUMax | Sets the value if it's greater than the original using an unsigned comparison and return the old one        |
| LLVMAtomicRMWBinOpUMin |                                                                                                             |
| LLVMAtomicRMWBinOpFAdd | Add a floating point value and return the old one                                                           |
| LLVMAtomicRMWBinOpFSub | Subtract a floating point value and return the old one                                                      |
| LLVMAtomicRMWBinOpFMax | Sets the value if it's greater than the original using an floating point comparison and return the old one  |
| LLVMAtomicRMWBinOpFMin | Sets the value if it's smaller than the original using an floating point comparison and return the old one  |
"""
@cenum LLVMAtomicRMWBinOp::UInt32 begin
    LLVMAtomicRMWBinOpXchg = 0
    LLVMAtomicRMWBinOpAdd = 1
    LLVMAtomicRMWBinOpSub = 2
    LLVMAtomicRMWBinOpAnd = 3
    LLVMAtomicRMWBinOpNand = 4
    LLVMAtomicRMWBinOpOr = 5
    LLVMAtomicRMWBinOpXor = 6
    LLVMAtomicRMWBinOpMax = 7
    LLVMAtomicRMWBinOpMin = 8
    LLVMAtomicRMWBinOpUMax = 9
    LLVMAtomicRMWBinOpUMin = 10
    LLVMAtomicRMWBinOpFAdd = 11
    LLVMAtomicRMWBinOpFSub = 12
    LLVMAtomicRMWBinOpFMax = 13
    LLVMAtomicRMWBinOpFMin = 14
end

@cenum LLVMDiagnosticSeverity::UInt32 begin
    LLVMDSError = 0
    LLVMDSWarning = 1
    LLVMDSRemark = 2
    LLVMDSNote = 3
end

@cenum LLVMInlineAsmDialect::UInt32 begin
    LLVMInlineAsmDialectATT = 0
    LLVMInlineAsmDialectIntel = 1
end

"""
    LLVMModuleFlagBehavior

| Enumerator                         | Note                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| :--------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LLVMModuleFlagBehaviorError        | Emits an error if two values disagree, otherwise the resulting value is that of the operands.  # See also Module::ModFlagBehavior::Error                                                                                                                                                                                                                                                                                                                                                                           |
| LLVMModuleFlagBehaviorWarning      | Emits a warning if two values disagree. The result value will be the operand for the flag from the first module being linked.  # See also Module::ModFlagBehavior::Warning                                                                                                                                                                                                                                                                                                                                         |
| LLVMModuleFlagBehaviorRequire      | Adds a requirement that another module flag be present and have a specified value after linking is performed. The value must be a metadata pair, where the first element of the pair is the ID of the module flag to be restricted, and the second element of the pair is the value the module flag should be restricted to. This behavior can be used to restrict the allowable results (via triggering of an error) of linking IDs with the **Override** behavior.  # See also Module::ModFlagBehavior::Require  |
| LLVMModuleFlagBehaviorOverride     | Uses the specified value, regardless of the behavior or value of the other module. If both modules specify **Override**, but the values differ, an error will be emitted.  # See also Module::ModFlagBehavior::Override                                                                                                                                                                                                                                                                                            |
| LLVMModuleFlagBehaviorAppend       | Appends the two values, which are required to be metadata nodes.  # See also Module::ModFlagBehavior::Append                                                                                                                                                                                                                                                                                                                                                                                                       |
| LLVMModuleFlagBehaviorAppendUnique | Appends the two values, which are required to be metadata nodes. However, duplicate entries in the second list are dropped during the append operation.  # See also Module::ModFlagBehavior::AppendUnique                                                                                                                                                                                                                                                                                                          |
"""
@cenum LLVMModuleFlagBehavior::UInt32 begin
    LLVMModuleFlagBehaviorError = 0
    LLVMModuleFlagBehaviorWarning = 1
    LLVMModuleFlagBehaviorRequire = 2
    LLVMModuleFlagBehaviorOverride = 3
    LLVMModuleFlagBehaviorAppend = 4
    LLVMModuleFlagBehaviorAppendUnique = 5
end

"""
    ##Ctag#230

Attribute index are either LLVMAttributeReturnIndex, LLVMAttributeFunctionIndex or a parameter number from 1 to N.
"""
@cenum var"##Ctag#230"::Int32 begin
    LLVMAttributeReturnIndex = 0
    LLVMAttributeFunctionIndex = -1
end

const LLVMAttributeIndex = Cuint

"""
# See also
llvm::PassRegistry
"""
const LLVMPassRegistryRef = Ptr{LLVMOpaquePassRegistry}

"""
    LLVMInitializeCore(R)

@}
"""
function LLVMInitializeCore(R)
    ccall((:LLVMInitializeCore, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

"""
    LLVMShutdown()

Deallocate and destroy all ManagedStatic variables.

# See also
llvm::llvm\\_shutdown, ManagedStatic
"""
function LLVMShutdown()
    ccall((:LLVMShutdown, libllvm), Cvoid, ())
end

function LLVMCreateMessage(Message)
    ccall((:LLVMCreateMessage, libllvm), Cstring, (Cstring,), Message)
end

function LLVMDisposeMessage(Message)
    ccall((:LLVMDisposeMessage, libllvm), Cvoid, (Cstring,), Message)
end

# typedef void ( * LLVMDiagnosticHandler ) ( LLVMDiagnosticInfoRef , void * )
"""
` LLVMCCoreContext Contexts`

Contexts are execution states for the core LLVM IR system.

Most types are tied to a context instance. Multiple contexts can exist simultaneously. A single context is not thread safe. However, different contexts can execute on different threads simultaneously.

@{
"""
const LLVMDiagnosticHandler = Ptr{Cvoid}

# typedef void ( * LLVMYieldCallback ) ( LLVMContextRef , void * )
const LLVMYieldCallback = Ptr{Cvoid}

"""
    LLVMContextCreate()

Create a new context.

Every call to this function should be paired with a call to [`LLVMContextDispose`](@ref)() or the context will leak memory.
"""
function LLVMContextCreate()
    ccall((:LLVMContextCreate, libllvm), LLVMContextRef, ())
end

"""
    LLVMGetGlobalContext()

Obtain the global context instance.
"""
function LLVMGetGlobalContext()
    ccall((:LLVMGetGlobalContext, libllvm), LLVMContextRef, ())
end

"""
    LLVMContextSetDiagnosticHandler(C, Handler, DiagnosticContext)

Set the diagnostic handler for this context.
"""
function LLVMContextSetDiagnosticHandler(C, Handler, DiagnosticContext)
    ccall((:LLVMContextSetDiagnosticHandler, libllvm), Cvoid, (LLVMContextRef, LLVMDiagnosticHandler, Ptr{Cvoid}), C, Handler, DiagnosticContext)
end

"""
    LLVMContextGetDiagnosticHandler(C)

Get the diagnostic handler of this context.
"""
function LLVMContextGetDiagnosticHandler(C)
    ccall((:LLVMContextGetDiagnosticHandler, libllvm), LLVMDiagnosticHandler, (LLVMContextRef,), C)
end

"""
    LLVMContextGetDiagnosticContext(C)

Get the diagnostic context of this context.
"""
function LLVMContextGetDiagnosticContext(C)
    ccall((:LLVMContextGetDiagnosticContext, libllvm), Ptr{Cvoid}, (LLVMContextRef,), C)
end

"""
    LLVMContextSetYieldCallback(C, Callback, OpaqueHandle)

Set the yield callback function for this context.

# See also
LLVMContext::setYieldCallback()
"""
function LLVMContextSetYieldCallback(C, Callback, OpaqueHandle)
    ccall((:LLVMContextSetYieldCallback, libllvm), Cvoid, (LLVMContextRef, LLVMYieldCallback, Ptr{Cvoid}), C, Callback, OpaqueHandle)
end

"""
    LLVMContextShouldDiscardValueNames(C)

Retrieve whether the given context is set to discard all value names.

# See also
LLVMContext::shouldDiscardValueNames()
"""
function LLVMContextShouldDiscardValueNames(C)
    ccall((:LLVMContextShouldDiscardValueNames, libllvm), LLVMBool, (LLVMContextRef,), C)
end

"""
    LLVMContextSetDiscardValueNames(C, Discard)

Set whether the given context discards all value names.

If true, only the names of GlobalValue objects will be available in the IR. This can be used to save memory and runtime, especially in release mode.

# See also
LLVMContext::setDiscardValueNames()
"""
function LLVMContextSetDiscardValueNames(C, Discard)
    ccall((:LLVMContextSetDiscardValueNames, libllvm), Cvoid, (LLVMContextRef, LLVMBool), C, Discard)
end

"""
    LLVMContextSetOpaquePointers(C, OpaquePointers)

Set whether the given context is in opaque pointer mode.

# See also
LLVMContext::setOpaquePointers()
"""
function LLVMContextSetOpaquePointers(C, OpaquePointers)
    ccall((:LLVMContextSetOpaquePointers, libllvm), Cvoid, (LLVMContextRef, LLVMBool), C, OpaquePointers)
end

"""
    LLVMContextDispose(C)

Destroy a context instance.

This should be called for every call to [`LLVMContextCreate`](@ref)() or memory will be leaked.
"""
function LLVMContextDispose(C)
    ccall((:LLVMContextDispose, libllvm), Cvoid, (LLVMContextRef,), C)
end

"""
# See also
llvm::DiagnosticInfo
"""
const LLVMDiagnosticInfoRef = Ptr{LLVMOpaqueDiagnosticInfo}

"""
    LLVMGetDiagInfoDescription(DI)

Return a string representation of the DiagnosticInfo. Use [`LLVMDisposeMessage`](@ref) to free the string.

# See also
DiagnosticInfo::print()
"""
function LLVMGetDiagInfoDescription(DI)
    ccall((:LLVMGetDiagInfoDescription, libllvm), Cstring, (LLVMDiagnosticInfoRef,), DI)
end

"""
    LLVMGetDiagInfoSeverity(DI)

Return an enum [`LLVMDiagnosticSeverity`](@ref).

# See also
DiagnosticInfo::getSeverity()
"""
function LLVMGetDiagInfoSeverity(DI)
    ccall((:LLVMGetDiagInfoSeverity, libllvm), LLVMDiagnosticSeverity, (LLVMDiagnosticInfoRef,), DI)
end

function LLVMGetMDKindIDInContext(C, Name, SLen)
    ccall((:LLVMGetMDKindIDInContext, libllvm), Cuint, (LLVMContextRef, Cstring, Cuint), C, Name, SLen)
end

function LLVMGetMDKindID(Name, SLen)
    ccall((:LLVMGetMDKindID, libllvm), Cuint, (Cstring, Cuint), Name, SLen)
end

"""
    LLVMGetEnumAttributeKindForName(Name, SLen)

Return an unique id given the name of a enum attribute, or 0 if no attribute by that name exists.

See http://llvm.org/docs/LangRef.html#parameter-attributes and http://llvm.org/docs/LangRef.html#function-attributes for the list of available attributes.

NB: Attribute names and/or id are subject to change without going through the C API deprecation cycle.
"""
function LLVMGetEnumAttributeKindForName(Name, SLen)
    ccall((:LLVMGetEnumAttributeKindForName, libllvm), Cuint, (Cstring, Csize_t), Name, SLen)
end

function LLVMGetLastEnumAttributeKind()
    ccall((:LLVMGetLastEnumAttributeKind, libllvm), Cuint, ())
end

"""
Used to represent an attributes.

# See also
llvm::Attribute
"""
const LLVMAttributeRef = Ptr{LLVMOpaqueAttributeRef}

"""
    LLVMCreateEnumAttribute(C, KindID, Val)

Create an enum attribute.
"""
function LLVMCreateEnumAttribute(C, KindID, Val)
    ccall((:LLVMCreateEnumAttribute, libllvm), LLVMAttributeRef, (LLVMContextRef, Cuint, UInt64), C, KindID, Val)
end

"""
    LLVMGetEnumAttributeKind(A)

Get the unique id corresponding to the enum attribute passed as argument.
"""
function LLVMGetEnumAttributeKind(A)
    ccall((:LLVMGetEnumAttributeKind, libllvm), Cuint, (LLVMAttributeRef,), A)
end

"""
    LLVMGetEnumAttributeValue(A)

Get the enum attribute's value. 0 is returned if none exists.
"""
function LLVMGetEnumAttributeValue(A)
    ccall((:LLVMGetEnumAttributeValue, libllvm), UInt64, (LLVMAttributeRef,), A)
end

"""
Each value in the LLVM IR has a type, an [`LLVMTypeRef`](@ref).

# See also
llvm::Type
"""
const LLVMTypeRef = Ptr{LLVMOpaqueType}

"""
    LLVMCreateTypeAttribute(C, KindID, type_ref)

Create a type attribute
"""
function LLVMCreateTypeAttribute(C, KindID, type_ref)
    ccall((:LLVMCreateTypeAttribute, libllvm), LLVMAttributeRef, (LLVMContextRef, Cuint, LLVMTypeRef), C, KindID, type_ref)
end

"""
    LLVMGetTypeAttributeValue(A)

Get the type attribute's value.
"""
function LLVMGetTypeAttributeValue(A)
    ccall((:LLVMGetTypeAttributeValue, libllvm), LLVMTypeRef, (LLVMAttributeRef,), A)
end

"""
    LLVMCreateStringAttribute(C, K, KLength, V, VLength)

Create a string attribute.
"""
function LLVMCreateStringAttribute(C, K, KLength, V, VLength)
    ccall((:LLVMCreateStringAttribute, libllvm), LLVMAttributeRef, (LLVMContextRef, Cstring, Cuint, Cstring, Cuint), C, K, KLength, V, VLength)
end

"""
    LLVMGetStringAttributeKind(A, Length)

Get the string attribute's kind.
"""
function LLVMGetStringAttributeKind(A, Length)
    ccall((:LLVMGetStringAttributeKind, libllvm), Cstring, (LLVMAttributeRef, Ptr{Cuint}), A, Length)
end

"""
    LLVMGetStringAttributeValue(A, Length)

Get the string attribute's value.
"""
function LLVMGetStringAttributeValue(A, Length)
    ccall((:LLVMGetStringAttributeValue, libllvm), Cstring, (LLVMAttributeRef, Ptr{Cuint}), A, Length)
end

"""
    LLVMIsEnumAttribute(A)

Check for the different types of attributes.
"""
function LLVMIsEnumAttribute(A)
    ccall((:LLVMIsEnumAttribute, libllvm), LLVMBool, (LLVMAttributeRef,), A)
end

function LLVMIsStringAttribute(A)
    ccall((:LLVMIsStringAttribute, libllvm), LLVMBool, (LLVMAttributeRef,), A)
end

function LLVMIsTypeAttribute(A)
    ccall((:LLVMIsTypeAttribute, libllvm), LLVMBool, (LLVMAttributeRef,), A)
end

"""
    LLVMGetTypeByName2(C, Name)

Obtain a Type from a context by its registered name.
"""
function LLVMGetTypeByName2(C, Name)
    ccall((:LLVMGetTypeByName2, libllvm), LLVMTypeRef, (LLVMContextRef, Cstring), C, Name)
end

"""
    LLVMModuleCreateWithName(ModuleID)

Create a new, empty module in the global context.

This is equivalent to calling [`LLVMModuleCreateWithNameInContext`](@ref) with [`LLVMGetGlobalContext`](@ref)() as the context parameter.

Every invocation should be paired with [`LLVMDisposeModule`](@ref)() or memory will be leaked.
"""
function LLVMModuleCreateWithName(ModuleID)
    ccall((:LLVMModuleCreateWithName, libllvm), LLVMModuleRef, (Cstring,), ModuleID)
end

"""
    LLVMModuleCreateWithNameInContext(ModuleID, C)

Create a new, empty module in a specific context.

Every invocation should be paired with [`LLVMDisposeModule`](@ref)() or memory will be leaked.
"""
function LLVMModuleCreateWithNameInContext(ModuleID, C)
    ccall((:LLVMModuleCreateWithNameInContext, libllvm), LLVMModuleRef, (Cstring, LLVMContextRef), ModuleID, C)
end

"""
    LLVMCloneModule(M)

Return an exact copy of the specified module.
"""
function LLVMCloneModule(M)
    ccall((:LLVMCloneModule, libllvm), LLVMModuleRef, (LLVMModuleRef,), M)
end

"""
    LLVMDisposeModule(M)

Destroy a module instance.

This must be called for every created module or memory will be leaked.
"""
function LLVMDisposeModule(M)
    ccall((:LLVMDisposeModule, libllvm), Cvoid, (LLVMModuleRef,), M)
end

"""
    LLVMGetModuleIdentifier(M, Len)

Obtain the identifier of a module.

# Arguments
* `M`: Module to obtain identifier of
* `Len`: Out parameter which holds the length of the returned string.
# Returns
The identifier of M.
# See also
Module::getModuleIdentifier()
"""
function LLVMGetModuleIdentifier(M, Len)
    ccall((:LLVMGetModuleIdentifier, libllvm), Cstring, (LLVMModuleRef, Ptr{Csize_t}), M, Len)
end

"""
    LLVMSetModuleIdentifier(M, Ident, Len)

Set the identifier of a module to a string Ident with length Len.

# Arguments
* `M`: The module to set identifier
* `Ident`: The string to set M's identifier to
* `Len`: Length of Ident
# See also
Module::setModuleIdentifier()
"""
function LLVMSetModuleIdentifier(M, Ident, Len)
    ccall((:LLVMSetModuleIdentifier, libllvm), Cvoid, (LLVMModuleRef, Cstring, Csize_t), M, Ident, Len)
end

"""
    LLVMGetSourceFileName(M, Len)

Obtain the module's original source file name.

# Arguments
* `M`: Module to obtain the name of
* `Len`: Out parameter which holds the length of the returned string
# Returns
The original source file name of M
# See also
Module::getSourceFileName()
"""
function LLVMGetSourceFileName(M, Len)
    ccall((:LLVMGetSourceFileName, libllvm), Cstring, (LLVMModuleRef, Ptr{Csize_t}), M, Len)
end

"""
    LLVMSetSourceFileName(M, Name, Len)

Set the original source file name of a module to a string Name with length Len.

# Arguments
* `M`: The module to set the source file name of
* `Name`: The string to set M's source file name to
* `Len`: Length of Name
# See also
Module::setSourceFileName()
"""
function LLVMSetSourceFileName(M, Name, Len)
    ccall((:LLVMSetSourceFileName, libllvm), Cvoid, (LLVMModuleRef, Cstring, Csize_t), M, Name, Len)
end

"""
    LLVMGetDataLayoutStr(M)

Obtain the data layout for a module.

[`LLVMGetDataLayout`](@ref) is DEPRECATED, as the name is not only incorrect, but match the name of another method on the module. Prefer the use of [`LLVMGetDataLayoutStr`](@ref), which is not ambiguous.

# See also
Module::getDataLayoutStr()
"""
function LLVMGetDataLayoutStr(M)
    ccall((:LLVMGetDataLayoutStr, libllvm), Cstring, (LLVMModuleRef,), M)
end

function LLVMGetDataLayout(M)
    ccall((:LLVMGetDataLayout, libllvm), Cstring, (LLVMModuleRef,), M)
end

"""
    LLVMSetDataLayout(M, DataLayoutStr)

Set the data layout for a module.

# See also
Module::setDataLayout()
"""
function LLVMSetDataLayout(M, DataLayoutStr)
    ccall((:LLVMSetDataLayout, libllvm), Cvoid, (LLVMModuleRef, Cstring), M, DataLayoutStr)
end

"""
    LLVMGetTarget(M)

Obtain the target triple for a module.

# See also
Module::getTargetTriple()
"""
function LLVMGetTarget(M)
    ccall((:LLVMGetTarget, libllvm), Cstring, (LLVMModuleRef,), M)
end

"""
    LLVMSetTarget(M, Triple)

Set the target triple for a module.

# See also
Module::setTargetTriple()
"""
function LLVMSetTarget(M, Triple)
    ccall((:LLVMSetTarget, libllvm), Cvoid, (LLVMModuleRef, Cstring), M, Triple)
end

"""
# See also
llvm::Module::ModuleFlagEntry
"""
const LLVMModuleFlagEntry = LLVMOpaqueModuleFlagEntry

"""
    LLVMCopyModuleFlagsMetadata(M, Len)

Returns the module flags as an array of flag-key-value triples. The caller is responsible for freeing this array by calling [`LLVMDisposeModuleFlagsMetadata`](@ref).

# See also
Module::getModuleFlagsMetadata()
"""
function LLVMCopyModuleFlagsMetadata(M, Len)
    ccall((:LLVMCopyModuleFlagsMetadata, libllvm), Ptr{LLVMModuleFlagEntry}, (LLVMModuleRef, Ptr{Csize_t}), M, Len)
end

"""
    LLVMDisposeModuleFlagsMetadata(Entries)

Destroys module flags metadata entries.
"""
function LLVMDisposeModuleFlagsMetadata(Entries)
    ccall((:LLVMDisposeModuleFlagsMetadata, libllvm), Cvoid, (Ptr{LLVMModuleFlagEntry},), Entries)
end

"""
    LLVMModuleFlagEntriesGetFlagBehavior(Entries, Index)

Returns the flag behavior for a module flag entry at a specific index.

# See also
Module::ModuleFlagEntry::Behavior
"""
function LLVMModuleFlagEntriesGetFlagBehavior(Entries, Index)
    ccall((:LLVMModuleFlagEntriesGetFlagBehavior, libllvm), LLVMModuleFlagBehavior, (Ptr{LLVMModuleFlagEntry}, Cuint), Entries, Index)
end

"""
    LLVMModuleFlagEntriesGetKey(Entries, Index, Len)

Returns the key for a module flag entry at a specific index.

# See also
Module::ModuleFlagEntry::Key
"""
function LLVMModuleFlagEntriesGetKey(Entries, Index, Len)
    ccall((:LLVMModuleFlagEntriesGetKey, libllvm), Cstring, (Ptr{LLVMModuleFlagEntry}, Cuint, Ptr{Csize_t}), Entries, Index, Len)
end

"""
Represents an LLVM Metadata.

This models llvm::Metadata.
"""
const LLVMMetadataRef = Ptr{LLVMOpaqueMetadata}

"""
    LLVMModuleFlagEntriesGetMetadata(Entries, Index)

Returns the metadata for a module flag entry at a specific index.

# See also
Module::ModuleFlagEntry::Val
"""
function LLVMModuleFlagEntriesGetMetadata(Entries, Index)
    ccall((:LLVMModuleFlagEntriesGetMetadata, libllvm), LLVMMetadataRef, (Ptr{LLVMModuleFlagEntry}, Cuint), Entries, Index)
end

"""
    LLVMGetModuleFlag(M, Key, KeyLen)

Add a module-level flag to the module-level flags metadata if it doesn't already exist.

# See also
Module::getModuleFlag()
"""
function LLVMGetModuleFlag(M, Key, KeyLen)
    ccall((:LLVMGetModuleFlag, libllvm), LLVMMetadataRef, (LLVMModuleRef, Cstring, Csize_t), M, Key, KeyLen)
end

"""
    LLVMAddModuleFlag(M, Behavior, Key, KeyLen, Val)

Add a module-level flag to the module-level flags metadata if it doesn't already exist.

# See also
Module::addModuleFlag()
"""
function LLVMAddModuleFlag(M, Behavior, Key, KeyLen, Val)
    ccall((:LLVMAddModuleFlag, libllvm), Cvoid, (LLVMModuleRef, LLVMModuleFlagBehavior, Cstring, Csize_t, LLVMMetadataRef), M, Behavior, Key, KeyLen, Val)
end

"""
    LLVMDumpModule(M)

Dump a representation of a module to stderr.

# See also
Module::dump()
"""
function LLVMDumpModule(M)
    ccall((:LLVMDumpModule, libllvm), Cvoid, (LLVMModuleRef,), M)
end

"""
    LLVMPrintModuleToFile(M, Filename, ErrorMessage)

Print a representation of a module to a file. The ErrorMessage needs to be disposed with [`LLVMDisposeMessage`](@ref). Returns 0 on success, 1 otherwise.

# See also
Module::print()
"""
function LLVMPrintModuleToFile(M, Filename, ErrorMessage)
    ccall((:LLVMPrintModuleToFile, libllvm), LLVMBool, (LLVMModuleRef, Cstring, Ptr{Cstring}), M, Filename, ErrorMessage)
end

"""
    LLVMPrintModuleToString(M)

Return a string representation of the module. Use [`LLVMDisposeMessage`](@ref) to free the string.

# See also
Module::print()
"""
function LLVMPrintModuleToString(M)
    ccall((:LLVMPrintModuleToString, libllvm), Cstring, (LLVMModuleRef,), M)
end

"""
    LLVMGetModuleInlineAsm(M, Len)

Get inline assembly for a module.

# See also
Module::getModuleInlineAsm()
"""
function LLVMGetModuleInlineAsm(M, Len)
    ccall((:LLVMGetModuleInlineAsm, libllvm), Cstring, (LLVMModuleRef, Ptr{Csize_t}), M, Len)
end

"""
    LLVMSetModuleInlineAsm2(M, Asm, Len)

Set inline assembly for a module.

# See also
Module::setModuleInlineAsm()
"""
function LLVMSetModuleInlineAsm2(M, Asm, Len)
    ccall((:LLVMSetModuleInlineAsm2, libllvm), Cvoid, (LLVMModuleRef, Cstring, Csize_t), M, Asm, Len)
end

"""
    LLVMAppendModuleInlineAsm(M, Asm, Len)

Append inline assembly to a module.

# See also
Module::appendModuleInlineAsm()
"""
function LLVMAppendModuleInlineAsm(M, Asm, Len)
    ccall((:LLVMAppendModuleInlineAsm, libllvm), Cvoid, (LLVMModuleRef, Cstring, Csize_t), M, Asm, Len)
end

"""
    LLVMGetInlineAsm(Ty, AsmString, AsmStringSize, Constraints, ConstraintsSize, HasSideEffects, IsAlignStack, Dialect, CanThrow)

Create the specified uniqued inline asm string.

# See also
InlineAsm::get()
"""
function LLVMGetInlineAsm(Ty, AsmString, AsmStringSize, Constraints, ConstraintsSize, HasSideEffects, IsAlignStack, Dialect, CanThrow)
    ccall((:LLVMGetInlineAsm, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring, Csize_t, Cstring, Csize_t, LLVMBool, LLVMBool, LLVMInlineAsmDialect, LLVMBool), Ty, AsmString, AsmStringSize, Constraints, ConstraintsSize, HasSideEffects, IsAlignStack, Dialect, CanThrow)
end

"""
    LLVMGetModuleContext(M)

Obtain the context to which this module is associated.

# See also
Module::getContext()
"""
function LLVMGetModuleContext(M)
    ccall((:LLVMGetModuleContext, libllvm), LLVMContextRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetTypeByName(M, Name)

Deprecated: Use [`LLVMGetTypeByName2`](@ref) instead.
"""
function LLVMGetTypeByName(M, Name)
    ccall((:LLVMGetTypeByName, libllvm), LLVMTypeRef, (LLVMModuleRef, Cstring), M, Name)
end

"""
Represents an LLVM Named Metadata Node.

This models llvm::NamedMDNode.
"""
const LLVMNamedMDNodeRef = Ptr{LLVMOpaqueNamedMDNode}

"""
    LLVMGetFirstNamedMetadata(M)

Obtain an iterator to the first NamedMDNode in a Module.

# See also
llvm::Module::named\\_metadata\\_begin()
"""
function LLVMGetFirstNamedMetadata(M)
    ccall((:LLVMGetFirstNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetLastNamedMetadata(M)

Obtain an iterator to the last NamedMDNode in a Module.

# See also
llvm::Module::named\\_metadata\\_end()
"""
function LLVMGetLastNamedMetadata(M)
    ccall((:LLVMGetLastNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetNextNamedMetadata(NamedMDNode)

Advance a NamedMDNode iterator to the next NamedMDNode.

Returns NULL if the iterator was already at the end and there are no more named metadata nodes.
"""
function LLVMGetNextNamedMetadata(NamedMDNode)
    ccall((:LLVMGetNextNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMNamedMDNodeRef,), NamedMDNode)
end

"""
    LLVMGetPreviousNamedMetadata(NamedMDNode)

Decrement a NamedMDNode iterator to the previous NamedMDNode.

Returns NULL if the iterator was already at the beginning and there are no previous named metadata nodes.
"""
function LLVMGetPreviousNamedMetadata(NamedMDNode)
    ccall((:LLVMGetPreviousNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMNamedMDNodeRef,), NamedMDNode)
end

"""
    LLVMGetNamedMetadata(M, Name, NameLen)

Retrieve a NamedMDNode with the given name, returning NULL if no such node exists.

# See also
llvm::Module::getNamedMetadata()
"""
function LLVMGetNamedMetadata(M, Name, NameLen)
    ccall((:LLVMGetNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMModuleRef, Cstring, Csize_t), M, Name, NameLen)
end

"""
    LLVMGetOrInsertNamedMetadata(M, Name, NameLen)

Retrieve a NamedMDNode with the given name, creating a new node if no such node exists.

# See also
llvm::Module::getOrInsertNamedMetadata()
"""
function LLVMGetOrInsertNamedMetadata(M, Name, NameLen)
    ccall((:LLVMGetOrInsertNamedMetadata, libllvm), LLVMNamedMDNodeRef, (LLVMModuleRef, Cstring, Csize_t), M, Name, NameLen)
end

"""
    LLVMGetNamedMetadataName(NamedMD, NameLen)

Retrieve the name of a NamedMDNode.

# See also
llvm::NamedMDNode::getName()
"""
function LLVMGetNamedMetadataName(NamedMD, NameLen)
    ccall((:LLVMGetNamedMetadataName, libllvm), Cstring, (LLVMNamedMDNodeRef, Ptr{Csize_t}), NamedMD, NameLen)
end

"""
    LLVMGetNamedMetadataNumOperands(M, Name)

Obtain the number of operands for named metadata in a module.

# See also
llvm::Module::getNamedMetadata()
"""
function LLVMGetNamedMetadataNumOperands(M, Name)
    ccall((:LLVMGetNamedMetadataNumOperands, libllvm), Cuint, (LLVMModuleRef, Cstring), M, Name)
end

"""
    LLVMGetNamedMetadataOperands(M, Name, Dest)

Obtain the named metadata operands for a module.

The passed [`LLVMValueRef`](@ref) pointer should refer to an array of [`LLVMValueRef`](@ref) at least [`LLVMGetNamedMetadataNumOperands`](@ref) long. This array will be populated with the [`LLVMValueRef`](@ref) instances. Each instance corresponds to a llvm::MDNode.

# See also
llvm::Module::getNamedMetadata(), llvm::MDNode::getOperand()
"""
function LLVMGetNamedMetadataOperands(M, Name, Dest)
    ccall((:LLVMGetNamedMetadataOperands, libllvm), Cvoid, (LLVMModuleRef, Cstring, Ptr{LLVMValueRef}), M, Name, Dest)
end

"""
    LLVMAddNamedMetadataOperand(M, Name, Val)

Add an operand to named metadata.

# See also
llvm::Module::getNamedMetadata(), llvm::MDNode::addOperand()
"""
function LLVMAddNamedMetadataOperand(M, Name, Val)
    ccall((:LLVMAddNamedMetadataOperand, libllvm), Cvoid, (LLVMModuleRef, Cstring, LLVMValueRef), M, Name, Val)
end

"""
    LLVMGetDebugLocDirectory(Val, Length)

Return the directory of the debug location for this value, which must be an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.

# See also
llvm::Instruction::getDebugLoc(), llvm::GlobalVariable::getDebugInfo(), llvm::Function::getSubprogram()
"""
function LLVMGetDebugLocDirectory(Val, Length)
    ccall((:LLVMGetDebugLocDirectory, libllvm), Cstring, (LLVMValueRef, Ptr{Cuint}), Val, Length)
end

"""
    LLVMGetDebugLocFilename(Val, Length)

Return the filename of the debug location for this value, which must be an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.

# See also
llvm::Instruction::getDebugLoc(), llvm::GlobalVariable::getDebugInfo(), llvm::Function::getSubprogram()
"""
function LLVMGetDebugLocFilename(Val, Length)
    ccall((:LLVMGetDebugLocFilename, libllvm), Cstring, (LLVMValueRef, Ptr{Cuint}), Val, Length)
end

"""
    LLVMGetDebugLocLine(Val)

Return the line number of the debug location for this value, which must be an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.

# See also
llvm::Instruction::getDebugLoc(), llvm::GlobalVariable::getDebugInfo(), llvm::Function::getSubprogram()
"""
function LLVMGetDebugLocLine(Val)
    ccall((:LLVMGetDebugLocLine, libllvm), Cuint, (LLVMValueRef,), Val)
end

"""
    LLVMGetDebugLocColumn(Val)

Return the column number of the debug location for this value, which must be an llvm::Instruction.

# See also
llvm::Instruction::getDebugLoc()
"""
function LLVMGetDebugLocColumn(Val)
    ccall((:LLVMGetDebugLocColumn, libllvm), Cuint, (LLVMValueRef,), Val)
end

"""
    LLVMAddFunction(M, Name, FunctionTy)

Add a function to a module under a specified name.

# See also
llvm::Function::Create()
"""
function LLVMAddFunction(M, Name, FunctionTy)
    ccall((:LLVMAddFunction, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring, LLVMTypeRef), M, Name, FunctionTy)
end

"""
    LLVMGetNamedFunction(M, Name)

Obtain a Function value from a Module by its name.

The returned value corresponds to a llvm::Function value.

# See also
llvm::Module::getFunction()
"""
function LLVMGetNamedFunction(M, Name)
    ccall((:LLVMGetNamedFunction, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring), M, Name)
end

"""
    LLVMGetFirstFunction(M)

Obtain an iterator to the first Function in a Module.

# See also
llvm::Module::begin()
"""
function LLVMGetFirstFunction(M)
    ccall((:LLVMGetFirstFunction, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetLastFunction(M)

Obtain an iterator to the last Function in a Module.

# See also
llvm::Module::end()
"""
function LLVMGetLastFunction(M)
    ccall((:LLVMGetLastFunction, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetNextFunction(Fn)

Advance a Function iterator to the next Function.

Returns NULL if the iterator was already at the end and there are no more functions.
"""
function LLVMGetNextFunction(Fn)
    ccall((:LLVMGetNextFunction, libllvm), LLVMValueRef, (LLVMValueRef,), Fn)
end

"""
    LLVMGetPreviousFunction(Fn)

Decrement a Function iterator to the previous Function.

Returns NULL if the iterator was already at the beginning and there are no previous functions.
"""
function LLVMGetPreviousFunction(Fn)
    ccall((:LLVMGetPreviousFunction, libllvm), LLVMValueRef, (LLVMValueRef,), Fn)
end

"""
    LLVMSetModuleInlineAsm(M, Asm)

Deprecated: Use [`LLVMSetModuleInlineAsm2`](@ref) instead.
"""
function LLVMSetModuleInlineAsm(M, Asm)
    ccall((:LLVMSetModuleInlineAsm, libllvm), Cvoid, (LLVMModuleRef, Cstring), M, Asm)
end

"""
    LLVMGetTypeKind(Ty)

Obtain the enumerated type of a Type instance.

# See also
llvm::Type:getTypeID()
"""
function LLVMGetTypeKind(Ty)
    ccall((:LLVMGetTypeKind, libllvm), LLVMTypeKind, (LLVMTypeRef,), Ty)
end

"""
    LLVMTypeIsSized(Ty)

Whether the type has a known size.

Things that don't have a size are abstract types, labels, and void.a

# See also
llvm::Type::isSized()
"""
function LLVMTypeIsSized(Ty)
    ccall((:LLVMTypeIsSized, libllvm), LLVMBool, (LLVMTypeRef,), Ty)
end

"""
    LLVMGetTypeContext(Ty)

Obtain the context to which this type instance is associated.

# See also
llvm::Type::getContext()
"""
function LLVMGetTypeContext(Ty)
    ccall((:LLVMGetTypeContext, libllvm), LLVMContextRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMDumpType(Val)

Dump a representation of a type to stderr.

# See also
llvm::Type::dump()
"""
function LLVMDumpType(Val)
    ccall((:LLVMDumpType, libllvm), Cvoid, (LLVMTypeRef,), Val)
end

"""
    LLVMPrintTypeToString(Val)

Return a string representation of the type. Use [`LLVMDisposeMessage`](@ref) to free the string.

# See also
llvm::Type::print()
"""
function LLVMPrintTypeToString(Val)
    ccall((:LLVMPrintTypeToString, libllvm), Cstring, (LLVMTypeRef,), Val)
end

"""
    LLVMInt1TypeInContext(C)

Obtain an integer type from a context with specified bit width.
"""
function LLVMInt1TypeInContext(C)
    ccall((:LLVMInt1TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMInt8TypeInContext(C)
    ccall((:LLVMInt8TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMInt16TypeInContext(C)
    ccall((:LLVMInt16TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMInt32TypeInContext(C)
    ccall((:LLVMInt32TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMInt64TypeInContext(C)
    ccall((:LLVMInt64TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMInt128TypeInContext(C)
    ccall((:LLVMInt128TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

function LLVMIntTypeInContext(C, NumBits)
    ccall((:LLVMIntTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef, Cuint), C, NumBits)
end

"""
    LLVMInt1Type()

Obtain an integer type from the global context with a specified bit width.
"""
function LLVMInt1Type()
    ccall((:LLVMInt1Type, libllvm), LLVMTypeRef, ())
end

function LLVMInt8Type()
    ccall((:LLVMInt8Type, libllvm), LLVMTypeRef, ())
end

function LLVMInt16Type()
    ccall((:LLVMInt16Type, libllvm), LLVMTypeRef, ())
end

function LLVMInt32Type()
    ccall((:LLVMInt32Type, libllvm), LLVMTypeRef, ())
end

function LLVMInt64Type()
    ccall((:LLVMInt64Type, libllvm), LLVMTypeRef, ())
end

function LLVMInt128Type()
    ccall((:LLVMInt128Type, libllvm), LLVMTypeRef, ())
end

function LLVMIntType(NumBits)
    ccall((:LLVMIntType, libllvm), LLVMTypeRef, (Cuint,), NumBits)
end

function LLVMGetIntTypeWidth(IntegerTy)
    ccall((:LLVMGetIntTypeWidth, libllvm), Cuint, (LLVMTypeRef,), IntegerTy)
end

"""
    LLVMHalfTypeInContext(C)

Obtain a 16-bit floating point type from a context.
"""
function LLVMHalfTypeInContext(C)
    ccall((:LLVMHalfTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMBFloatTypeInContext(C)

Obtain a 16-bit brain floating point type from a context.
"""
function LLVMBFloatTypeInContext(C)
    ccall((:LLVMBFloatTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMFloatTypeInContext(C)

Obtain a 32-bit floating point type from a context.
"""
function LLVMFloatTypeInContext(C)
    ccall((:LLVMFloatTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMDoubleTypeInContext(C)

Obtain a 64-bit floating point type from a context.
"""
function LLVMDoubleTypeInContext(C)
    ccall((:LLVMDoubleTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMX86FP80TypeInContext(C)

Obtain a 80-bit floating point type (X87) from a context.
"""
function LLVMX86FP80TypeInContext(C)
    ccall((:LLVMX86FP80TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMFP128TypeInContext(C)

Obtain a 128-bit floating point type (112-bit mantissa) from a context.
"""
function LLVMFP128TypeInContext(C)
    ccall((:LLVMFP128TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMPPCFP128TypeInContext(C)

Obtain a 128-bit floating point type (two 64-bits) from a context.
"""
function LLVMPPCFP128TypeInContext(C)
    ccall((:LLVMPPCFP128TypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMHalfType()

Obtain a floating point type from the global context.

These map to the functions in this group of the same name.
"""
function LLVMHalfType()
    ccall((:LLVMHalfType, libllvm), LLVMTypeRef, ())
end

function LLVMBFloatType()
    ccall((:LLVMBFloatType, libllvm), LLVMTypeRef, ())
end

function LLVMFloatType()
    ccall((:LLVMFloatType, libllvm), LLVMTypeRef, ())
end

function LLVMDoubleType()
    ccall((:LLVMDoubleType, libllvm), LLVMTypeRef, ())
end

function LLVMX86FP80Type()
    ccall((:LLVMX86FP80Type, libllvm), LLVMTypeRef, ())
end

function LLVMFP128Type()
    ccall((:LLVMFP128Type, libllvm), LLVMTypeRef, ())
end

function LLVMPPCFP128Type()
    ccall((:LLVMPPCFP128Type, libllvm), LLVMTypeRef, ())
end

"""
    LLVMFunctionType(ReturnType, ParamTypes, ParamCount, IsVarArg)

Obtain a function type consisting of a specified signature.

The function is defined as a tuple of a return Type, a list of parameter types, and whether the function is variadic.
"""
function LLVMFunctionType(ReturnType, ParamTypes, ParamCount, IsVarArg)
    ccall((:LLVMFunctionType, libllvm), LLVMTypeRef, (LLVMTypeRef, Ptr{LLVMTypeRef}, Cuint, LLVMBool), ReturnType, ParamTypes, ParamCount, IsVarArg)
end

"""
    LLVMIsFunctionVarArg(FunctionTy)

Returns whether a function type is variadic.
"""
function LLVMIsFunctionVarArg(FunctionTy)
    ccall((:LLVMIsFunctionVarArg, libllvm), LLVMBool, (LLVMTypeRef,), FunctionTy)
end

"""
    LLVMGetReturnType(FunctionTy)

Obtain the Type this function Type returns.
"""
function LLVMGetReturnType(FunctionTy)
    ccall((:LLVMGetReturnType, libllvm), LLVMTypeRef, (LLVMTypeRef,), FunctionTy)
end

"""
    LLVMCountParamTypes(FunctionTy)

Obtain the number of parameters this function accepts.
"""
function LLVMCountParamTypes(FunctionTy)
    ccall((:LLVMCountParamTypes, libllvm), Cuint, (LLVMTypeRef,), FunctionTy)
end

"""
    LLVMGetParamTypes(FunctionTy, Dest)

Obtain the types of a function's parameters.

The Dest parameter should point to a pre-allocated array of [`LLVMTypeRef`](@ref) at least [`LLVMCountParamTypes`](@ref)() large. On return, the first [`LLVMCountParamTypes`](@ref)() entries in the array will be populated with [`LLVMTypeRef`](@ref) instances.

# Arguments
* `FunctionTy`: The function type to operate on.
* `Dest`: Memory address of an array to be filled with result.
"""
function LLVMGetParamTypes(FunctionTy, Dest)
    ccall((:LLVMGetParamTypes, libllvm), Cvoid, (LLVMTypeRef, Ptr{LLVMTypeRef}), FunctionTy, Dest)
end

"""
    LLVMStructTypeInContext(C, ElementTypes, ElementCount, Packed)

Create a new structure type in a context.

A structure is specified by a list of inner elements/types and whether these can be packed together.

# See also
llvm::StructType::create()
"""
function LLVMStructTypeInContext(C, ElementTypes, ElementCount, Packed)
    ccall((:LLVMStructTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef, Ptr{LLVMTypeRef}, Cuint, LLVMBool), C, ElementTypes, ElementCount, Packed)
end

"""
    LLVMStructType(ElementTypes, ElementCount, Packed)

Create a new structure type in the global context.

# See also
llvm::StructType::create()
"""
function LLVMStructType(ElementTypes, ElementCount, Packed)
    ccall((:LLVMStructType, libllvm), LLVMTypeRef, (Ptr{LLVMTypeRef}, Cuint, LLVMBool), ElementTypes, ElementCount, Packed)
end

"""
    LLVMStructCreateNamed(C, Name)

Create an empty structure in a context having a specified name.

# See also
llvm::StructType::create()
"""
function LLVMStructCreateNamed(C, Name)
    ccall((:LLVMStructCreateNamed, libllvm), LLVMTypeRef, (LLVMContextRef, Cstring), C, Name)
end

"""
    LLVMGetStructName(Ty)

Obtain the name of a structure.

# See also
llvm::StructType::getName()
"""
function LLVMGetStructName(Ty)
    ccall((:LLVMGetStructName, libllvm), Cstring, (LLVMTypeRef,), Ty)
end

"""
    LLVMStructSetBody(StructTy, ElementTypes, ElementCount, Packed)

Set the contents of a structure type.

# See also
llvm::StructType::setBody()
"""
function LLVMStructSetBody(StructTy, ElementTypes, ElementCount, Packed)
    ccall((:LLVMStructSetBody, libllvm), Cvoid, (LLVMTypeRef, Ptr{LLVMTypeRef}, Cuint, LLVMBool), StructTy, ElementTypes, ElementCount, Packed)
end

"""
    LLVMCountStructElementTypes(StructTy)

Get the number of elements defined inside the structure.

# See also
llvm::StructType::getNumElements()
"""
function LLVMCountStructElementTypes(StructTy)
    ccall((:LLVMCountStructElementTypes, libllvm), Cuint, (LLVMTypeRef,), StructTy)
end

"""
    LLVMGetStructElementTypes(StructTy, Dest)

Get the elements within a structure.

The function is passed the address of a pre-allocated array of [`LLVMTypeRef`](@ref) at least [`LLVMCountStructElementTypes`](@ref)() long. After invocation, this array will be populated with the structure's elements. The objects in the destination array will have a lifetime of the structure type itself, which is the lifetime of the context it is contained in.
"""
function LLVMGetStructElementTypes(StructTy, Dest)
    ccall((:LLVMGetStructElementTypes, libllvm), Cvoid, (LLVMTypeRef, Ptr{LLVMTypeRef}), StructTy, Dest)
end

"""
    LLVMStructGetTypeAtIndex(StructTy, i)

Get the type of the element at a given index in the structure.

# See also
llvm::StructType::getTypeAtIndex()
"""
function LLVMStructGetTypeAtIndex(StructTy, i)
    ccall((:LLVMStructGetTypeAtIndex, libllvm), LLVMTypeRef, (LLVMTypeRef, Cuint), StructTy, i)
end

"""
    LLVMIsPackedStruct(StructTy)

Determine whether a structure is packed.

# See also
llvm::StructType::isPacked()
"""
function LLVMIsPackedStruct(StructTy)
    ccall((:LLVMIsPackedStruct, libllvm), LLVMBool, (LLVMTypeRef,), StructTy)
end

"""
    LLVMIsOpaqueStruct(StructTy)

Determine whether a structure is opaque.

# See also
llvm::StructType::isOpaque()
"""
function LLVMIsOpaqueStruct(StructTy)
    ccall((:LLVMIsOpaqueStruct, libllvm), LLVMBool, (LLVMTypeRef,), StructTy)
end

"""
    LLVMIsLiteralStruct(StructTy)

Determine whether a structure is literal.

# See also
llvm::StructType::isLiteral()
"""
function LLVMIsLiteralStruct(StructTy)
    ccall((:LLVMIsLiteralStruct, libllvm), LLVMBool, (LLVMTypeRef,), StructTy)
end

"""
    LLVMGetElementType(Ty)

Obtain the element type of an array or vector type.

This currently also works for pointer types, but this usage is deprecated.

# See also
llvm::SequentialType::getElementType()
"""
function LLVMGetElementType(Ty)
    ccall((:LLVMGetElementType, libllvm), LLVMTypeRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMGetSubtypes(Tp, Arr)

Returns type's subtypes

# See also
llvm::Type::subtypes()
"""
function LLVMGetSubtypes(Tp, Arr)
    ccall((:LLVMGetSubtypes, libllvm), Cvoid, (LLVMTypeRef, Ptr{LLVMTypeRef}), Tp, Arr)
end

"""
    LLVMGetNumContainedTypes(Tp)

Return the number of types in the derived type.

# See also
llvm::Type::getNumContainedTypes()
"""
function LLVMGetNumContainedTypes(Tp)
    ccall((:LLVMGetNumContainedTypes, libllvm), Cuint, (LLVMTypeRef,), Tp)
end

"""
    LLVMArrayType(ElementType, ElementCount)

Create a fixed size array type that refers to a specific type.

The created type will exist in the context that its element type exists in.

# See also
llvm::ArrayType::get()
"""
function LLVMArrayType(ElementType, ElementCount)
    ccall((:LLVMArrayType, libllvm), LLVMTypeRef, (LLVMTypeRef, Cuint), ElementType, ElementCount)
end

"""
    LLVMGetArrayLength(ArrayTy)

Obtain the length of an array type.

This only works on types that represent arrays.

# See also
llvm::ArrayType::getNumElements()
"""
function LLVMGetArrayLength(ArrayTy)
    ccall((:LLVMGetArrayLength, libllvm), Cuint, (LLVMTypeRef,), ArrayTy)
end

"""
    LLVMPointerType(ElementType, AddressSpace)

Create a pointer type that points to a defined type.

The created type will exist in the context that its pointee type exists in.

# See also
llvm::PointerType::get()
"""
function LLVMPointerType(ElementType, AddressSpace)
    ccall((:LLVMPointerType, libllvm), LLVMTypeRef, (LLVMTypeRef, Cuint), ElementType, AddressSpace)
end

"""
    LLVMPointerTypeIsOpaque(Ty)

Determine whether a pointer is opaque.

True if this is an instance of an opaque PointerType.

# See also
llvm::Type::isOpaquePointerTy()
"""
function LLVMPointerTypeIsOpaque(Ty)
    ccall((:LLVMPointerTypeIsOpaque, libllvm), LLVMBool, (LLVMTypeRef,), Ty)
end

"""
    LLVMPointerTypeInContext(C, AddressSpace)

Create an opaque pointer type in a context.

# See also
llvm::PointerType::get()
"""
function LLVMPointerTypeInContext(C, AddressSpace)
    ccall((:LLVMPointerTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef, Cuint), C, AddressSpace)
end

"""
    LLVMGetPointerAddressSpace(PointerTy)

Obtain the address space of a pointer type.

This only works on types that represent pointers.

# See also
llvm::PointerType::getAddressSpace()
"""
function LLVMGetPointerAddressSpace(PointerTy)
    ccall((:LLVMGetPointerAddressSpace, libllvm), Cuint, (LLVMTypeRef,), PointerTy)
end

"""
    LLVMVectorType(ElementType, ElementCount)

Create a vector type that contains a defined type and has a specific number of elements.

The created type will exist in the context thats its element type exists in.

# See also
llvm::VectorType::get()
"""
function LLVMVectorType(ElementType, ElementCount)
    ccall((:LLVMVectorType, libllvm), LLVMTypeRef, (LLVMTypeRef, Cuint), ElementType, ElementCount)
end

"""
    LLVMScalableVectorType(ElementType, ElementCount)

Create a vector type that contains a defined type and has a scalable number of elements.

The created type will exist in the context thats its element type exists in.

# See also
llvm::ScalableVectorType::get()
"""
function LLVMScalableVectorType(ElementType, ElementCount)
    ccall((:LLVMScalableVectorType, libllvm), LLVMTypeRef, (LLVMTypeRef, Cuint), ElementType, ElementCount)
end

"""
    LLVMGetVectorSize(VectorTy)

Obtain the (possibly scalable) number of elements in a vector type.

This only works on types that represent vectors (fixed or scalable).

# See also
llvm::VectorType::getNumElements()
"""
function LLVMGetVectorSize(VectorTy)
    ccall((:LLVMGetVectorSize, libllvm), Cuint, (LLVMTypeRef,), VectorTy)
end

"""
    LLVMVoidTypeInContext(C)

Create a void type in a context.
"""
function LLVMVoidTypeInContext(C)
    ccall((:LLVMVoidTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMLabelTypeInContext(C)

Create a label type in a context.
"""
function LLVMLabelTypeInContext(C)
    ccall((:LLVMLabelTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMX86MMXTypeInContext(C)

Create a X86 MMX type in a context.
"""
function LLVMX86MMXTypeInContext(C)
    ccall((:LLVMX86MMXTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMX86AMXTypeInContext(C)

Create a X86 AMX type in a context.
"""
function LLVMX86AMXTypeInContext(C)
    ccall((:LLVMX86AMXTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMTokenTypeInContext(C)

Create a token type in a context.
"""
function LLVMTokenTypeInContext(C)
    ccall((:LLVMTokenTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMMetadataTypeInContext(C)

Create a metadata type in a context.
"""
function LLVMMetadataTypeInContext(C)
    ccall((:LLVMMetadataTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef,), C)
end

"""
    LLVMVoidType()

These are similar to the above functions except they operate on the global context.
"""
function LLVMVoidType()
    ccall((:LLVMVoidType, libllvm), LLVMTypeRef, ())
end

function LLVMLabelType()
    ccall((:LLVMLabelType, libllvm), LLVMTypeRef, ())
end

function LLVMX86MMXType()
    ccall((:LLVMX86MMXType, libllvm), LLVMTypeRef, ())
end

function LLVMX86AMXType()
    ccall((:LLVMX86AMXType, libllvm), LLVMTypeRef, ())
end

"""
    LLVMTypeOf(Val)

Obtain the type of a value.

# See also
llvm::Value::getType()
"""
function LLVMTypeOf(Val)
    ccall((:LLVMTypeOf, libllvm), LLVMTypeRef, (LLVMValueRef,), Val)
end

"""
    LLVMGetValueKind(Val)

Obtain the enumerated type of a Value instance.

# See also
llvm::Value::getValueID()
"""
function LLVMGetValueKind(Val)
    ccall((:LLVMGetValueKind, libllvm), LLVMValueKind, (LLVMValueRef,), Val)
end

"""
    LLVMGetValueName2(Val, Length)

Obtain the string name of a value.

# See also
llvm::Value::getName()
"""
function LLVMGetValueName2(Val, Length)
    ccall((:LLVMGetValueName2, libllvm), Cstring, (LLVMValueRef, Ptr{Csize_t}), Val, Length)
end

"""
    LLVMSetValueName2(Val, Name, NameLen)

Set the string name of a value.

# See also
llvm::Value::setName()
"""
function LLVMSetValueName2(Val, Name, NameLen)
    ccall((:LLVMSetValueName2, libllvm), Cvoid, (LLVMValueRef, Cstring, Csize_t), Val, Name, NameLen)
end

"""
    LLVMDumpValue(Val)

Dump a representation of a value to stderr.

# See also
llvm::Value::dump()
"""
function LLVMDumpValue(Val)
    ccall((:LLVMDumpValue, libllvm), Cvoid, (LLVMValueRef,), Val)
end

"""
    LLVMPrintValueToString(Val)

Return a string representation of the value. Use [`LLVMDisposeMessage`](@ref) to free the string.

# See also
llvm::Value::print()
"""
function LLVMPrintValueToString(Val)
    ccall((:LLVMPrintValueToString, libllvm), Cstring, (LLVMValueRef,), Val)
end

"""
    LLVMReplaceAllUsesWith(OldVal, NewVal)

Replace all uses of a value with another one.

# See also
llvm::Value::replaceAllUsesWith()
"""
function LLVMReplaceAllUsesWith(OldVal, NewVal)
    ccall((:LLVMReplaceAllUsesWith, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), OldVal, NewVal)
end

"""
    LLVMIsConstant(Val)

Determine whether the specified value instance is constant.
"""
function LLVMIsConstant(Val)
    ccall((:LLVMIsConstant, libllvm), LLVMBool, (LLVMValueRef,), Val)
end

"""
    LLVMIsUndef(Val)

Determine whether a value instance is undefined.
"""
function LLVMIsUndef(Val)
    ccall((:LLVMIsUndef, libllvm), LLVMBool, (LLVMValueRef,), Val)
end

"""
    LLVMIsPoison(Val)

Determine whether a value instance is poisonous.
"""
function LLVMIsPoison(Val)
    ccall((:LLVMIsPoison, libllvm), LLVMBool, (LLVMValueRef,), Val)
end

function LLVMIsAArgument(Val)
    ccall((:LLVMIsAArgument, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsABasicBlock(Val)
    ccall((:LLVMIsABasicBlock, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAInlineAsm(Val)
    ccall((:LLVMIsAInlineAsm, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUser(Val)
    ccall((:LLVMIsAUser, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstant(Val)
    ccall((:LLVMIsAConstant, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsABlockAddress(Val)
    ccall((:LLVMIsABlockAddress, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantAggregateZero(Val)
    ccall((:LLVMIsAConstantAggregateZero, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantArray(Val)
    ccall((:LLVMIsAConstantArray, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantDataSequential(Val)
    ccall((:LLVMIsAConstantDataSequential, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantDataArray(Val)
    ccall((:LLVMIsAConstantDataArray, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantDataVector(Val)
    ccall((:LLVMIsAConstantDataVector, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantExpr(Val)
    ccall((:LLVMIsAConstantExpr, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantFP(Val)
    ccall((:LLVMIsAConstantFP, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantInt(Val)
    ccall((:LLVMIsAConstantInt, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantPointerNull(Val)
    ccall((:LLVMIsAConstantPointerNull, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantStruct(Val)
    ccall((:LLVMIsAConstantStruct, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantTokenNone(Val)
    ccall((:LLVMIsAConstantTokenNone, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAConstantVector(Val)
    ccall((:LLVMIsAConstantVector, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGlobalValue(Val)
    ccall((:LLVMIsAGlobalValue, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGlobalAlias(Val)
    ccall((:LLVMIsAGlobalAlias, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGlobalObject(Val)
    ccall((:LLVMIsAGlobalObject, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFunction(Val)
    ccall((:LLVMIsAFunction, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGlobalVariable(Val)
    ccall((:LLVMIsAGlobalVariable, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGlobalIFunc(Val)
    ccall((:LLVMIsAGlobalIFunc, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUndefValue(Val)
    ccall((:LLVMIsAUndefValue, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAPoisonValue(Val)
    ccall((:LLVMIsAPoisonValue, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAInstruction(Val)
    ccall((:LLVMIsAInstruction, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUnaryOperator(Val)
    ccall((:LLVMIsAUnaryOperator, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsABinaryOperator(Val)
    ccall((:LLVMIsABinaryOperator, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACallInst(Val)
    ccall((:LLVMIsACallInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAIntrinsicInst(Val)
    ccall((:LLVMIsAIntrinsicInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsADbgInfoIntrinsic(Val)
    ccall((:LLVMIsADbgInfoIntrinsic, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsADbgVariableIntrinsic(Val)
    ccall((:LLVMIsADbgVariableIntrinsic, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsADbgDeclareInst(Val)
    ccall((:LLVMIsADbgDeclareInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsADbgLabelInst(Val)
    ccall((:LLVMIsADbgLabelInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMemIntrinsic(Val)
    ccall((:LLVMIsAMemIntrinsic, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMemCpyInst(Val)
    ccall((:LLVMIsAMemCpyInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMemMoveInst(Val)
    ccall((:LLVMIsAMemMoveInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMemSetInst(Val)
    ccall((:LLVMIsAMemSetInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACmpInst(Val)
    ccall((:LLVMIsACmpInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFCmpInst(Val)
    ccall((:LLVMIsAFCmpInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAICmpInst(Val)
    ccall((:LLVMIsAICmpInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAExtractElementInst(Val)
    ccall((:LLVMIsAExtractElementInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAGetElementPtrInst(Val)
    ccall((:LLVMIsAGetElementPtrInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAInsertElementInst(Val)
    ccall((:LLVMIsAInsertElementInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAInsertValueInst(Val)
    ccall((:LLVMIsAInsertValueInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsALandingPadInst(Val)
    ccall((:LLVMIsALandingPadInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAPHINode(Val)
    ccall((:LLVMIsAPHINode, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsASelectInst(Val)
    ccall((:LLVMIsASelectInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAShuffleVectorInst(Val)
    ccall((:LLVMIsAShuffleVectorInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAStoreInst(Val)
    ccall((:LLVMIsAStoreInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsABranchInst(Val)
    ccall((:LLVMIsABranchInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAIndirectBrInst(Val)
    ccall((:LLVMIsAIndirectBrInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAInvokeInst(Val)
    ccall((:LLVMIsAInvokeInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAReturnInst(Val)
    ccall((:LLVMIsAReturnInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsASwitchInst(Val)
    ccall((:LLVMIsASwitchInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUnreachableInst(Val)
    ccall((:LLVMIsAUnreachableInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAResumeInst(Val)
    ccall((:LLVMIsAResumeInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACleanupReturnInst(Val)
    ccall((:LLVMIsACleanupReturnInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACatchReturnInst(Val)
    ccall((:LLVMIsACatchReturnInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACatchSwitchInst(Val)
    ccall((:LLVMIsACatchSwitchInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACallBrInst(Val)
    ccall((:LLVMIsACallBrInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFuncletPadInst(Val)
    ccall((:LLVMIsAFuncletPadInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACatchPadInst(Val)
    ccall((:LLVMIsACatchPadInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACleanupPadInst(Val)
    ccall((:LLVMIsACleanupPadInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUnaryInstruction(Val)
    ccall((:LLVMIsAUnaryInstruction, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAAllocaInst(Val)
    ccall((:LLVMIsAAllocaInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsACastInst(Val)
    ccall((:LLVMIsACastInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAAddrSpaceCastInst(Val)
    ccall((:LLVMIsAAddrSpaceCastInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsABitCastInst(Val)
    ccall((:LLVMIsABitCastInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFPExtInst(Val)
    ccall((:LLVMIsAFPExtInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFPToSIInst(Val)
    ccall((:LLVMIsAFPToSIInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFPToUIInst(Val)
    ccall((:LLVMIsAFPToUIInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFPTruncInst(Val)
    ccall((:LLVMIsAFPTruncInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAIntToPtrInst(Val)
    ccall((:LLVMIsAIntToPtrInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAPtrToIntInst(Val)
    ccall((:LLVMIsAPtrToIntInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsASExtInst(Val)
    ccall((:LLVMIsASExtInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsASIToFPInst(Val)
    ccall((:LLVMIsASIToFPInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsATruncInst(Val)
    ccall((:LLVMIsATruncInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAUIToFPInst(Val)
    ccall((:LLVMIsAUIToFPInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAZExtInst(Val)
    ccall((:LLVMIsAZExtInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAExtractValueInst(Val)
    ccall((:LLVMIsAExtractValueInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsALoadInst(Val)
    ccall((:LLVMIsALoadInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAVAArgInst(Val)
    ccall((:LLVMIsAVAArgInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFreezeInst(Val)
    ccall((:LLVMIsAFreezeInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAAtomicCmpXchgInst(Val)
    ccall((:LLVMIsAAtomicCmpXchgInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAAtomicRMWInst(Val)
    ccall((:LLVMIsAAtomicRMWInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAFenceInst(Val)
    ccall((:LLVMIsAFenceInst, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMDNode(Val)
    ccall((:LLVMIsAMDNode, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

function LLVMIsAMDString(Val)
    ccall((:LLVMIsAMDString, libllvm), LLVMValueRef, (LLVMValueRef,), Val)
end

"""
    LLVMGetValueName(Val)

Deprecated: Use [`LLVMGetValueName2`](@ref) instead.
"""
function LLVMGetValueName(Val)
    ccall((:LLVMGetValueName, libllvm), Cstring, (LLVMValueRef,), Val)
end

"""
    LLVMSetValueName(Val, Name)

Deprecated: Use [`LLVMSetValueName2`](@ref) instead.
"""
function LLVMSetValueName(Val, Name)
    ccall((:LLVMSetValueName, libllvm), Cvoid, (LLVMValueRef, Cstring), Val, Name)
end

"""
Used to get the users and usees of a Value.

# See also
llvm::Use
"""
const LLVMUseRef = Ptr{LLVMOpaqueUse}

"""
    LLVMGetFirstUse(Val)

Obtain the first use of a value.

Uses are obtained in an iterator fashion. First, call this function to obtain a reference to the first use. Then, call [`LLVMGetNextUse`](@ref)() on that instance and all subsequently obtained instances until [`LLVMGetNextUse`](@ref)() returns NULL.

# See also
llvm::Value::use\\_begin()
"""
function LLVMGetFirstUse(Val)
    ccall((:LLVMGetFirstUse, libllvm), LLVMUseRef, (LLVMValueRef,), Val)
end

"""
    LLVMGetNextUse(U)

Obtain the next use of a value.

This effectively advances the iterator. It returns NULL if you are on the final use and no more are available.
"""
function LLVMGetNextUse(U)
    ccall((:LLVMGetNextUse, libllvm), LLVMUseRef, (LLVMUseRef,), U)
end

"""
    LLVMGetUser(U)

Obtain the user value for a user.

The returned value corresponds to a llvm::User type.

# See also
llvm::Use::getUser()
"""
function LLVMGetUser(U)
    ccall((:LLVMGetUser, libllvm), LLVMValueRef, (LLVMUseRef,), U)
end

"""
    LLVMGetUsedValue(U)

Obtain the value this use corresponds to.

# See also
llvm::Use::get().
"""
function LLVMGetUsedValue(U)
    ccall((:LLVMGetUsedValue, libllvm), LLVMValueRef, (LLVMUseRef,), U)
end

"""
    LLVMGetOperand(Val, Index)

Obtain an operand at a specific index in a llvm::User value.

# See also
llvm::User::getOperand()
"""
function LLVMGetOperand(Val, Index)
    ccall((:LLVMGetOperand, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), Val, Index)
end

"""
    LLVMGetOperandUse(Val, Index)

Obtain the use of an operand at a specific index in a llvm::User value.

# See also
llvm::User::getOperandUse()
"""
function LLVMGetOperandUse(Val, Index)
    ccall((:LLVMGetOperandUse, libllvm), LLVMUseRef, (LLVMValueRef, Cuint), Val, Index)
end

"""
    LLVMSetOperand(User, Index, Val)

Set an operand at a specific index in a llvm::User value.

# See also
llvm::User::setOperand()
"""
function LLVMSetOperand(User, Index, Val)
    ccall((:LLVMSetOperand, libllvm), Cvoid, (LLVMValueRef, Cuint, LLVMValueRef), User, Index, Val)
end

"""
    LLVMGetNumOperands(Val)

Obtain the number of operands in a llvm::User value.

# See also
llvm::User::getNumOperands()
"""
function LLVMGetNumOperands(Val)
    ccall((:LLVMGetNumOperands, libllvm), Cint, (LLVMValueRef,), Val)
end

"""
    LLVMConstNull(Ty)

Obtain a constant value referring to the null instance of a type.

# See also
llvm::Constant::getNullValue()
"""
function LLVMConstNull(Ty)
    ccall((:LLVMConstNull, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMConstAllOnes(Ty)

Obtain a constant value referring to the instance of a type consisting of all ones.

This is only valid for integer types.

# See also
llvm::Constant::getAllOnesValue()
"""
function LLVMConstAllOnes(Ty)
    ccall((:LLVMConstAllOnes, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMGetUndef(Ty)

Obtain a constant value referring to an undefined value of a type.

# See also
llvm::UndefValue::get()
"""
function LLVMGetUndef(Ty)
    ccall((:LLVMGetUndef, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMGetPoison(Ty)

Obtain a constant value referring to a poison value of a type.

# See also
llvm::PoisonValue::get()
"""
function LLVMGetPoison(Ty)
    ccall((:LLVMGetPoison, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMIsNull(Val)

Determine whether a value instance is null.

# See also
llvm::Constant::isNullValue()
"""
function LLVMIsNull(Val)
    ccall((:LLVMIsNull, libllvm), LLVMBool, (LLVMValueRef,), Val)
end

"""
    LLVMConstPointerNull(Ty)

Obtain a constant that is a constant pointer pointing to NULL for a specified type.
"""
function LLVMConstPointerNull(Ty)
    ccall((:LLVMConstPointerNull, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

"""
    LLVMConstInt(IntTy, N, SignExtend)

Obtain a constant value for an integer type.

The returned value corresponds to a llvm::ConstantInt.

# Arguments
* `IntTy`: Integer type to obtain value of.
* `N`: The value the returned instance should refer to.
* `SignExtend`: Whether to sign extend the produced value.
# See also
llvm::ConstantInt::get()
"""
function LLVMConstInt(IntTy, N, SignExtend)
    ccall((:LLVMConstInt, libllvm), LLVMValueRef, (LLVMTypeRef, Culonglong, LLVMBool), IntTy, N, SignExtend)
end

"""
    LLVMConstIntOfArbitraryPrecision(IntTy, NumWords, Words)

Obtain a constant value for an integer of arbitrary precision.

# See also
llvm::ConstantInt::get()
"""
function LLVMConstIntOfArbitraryPrecision(IntTy, NumWords, Words)
    ccall((:LLVMConstIntOfArbitraryPrecision, libllvm), LLVMValueRef, (LLVMTypeRef, Cuint, Ptr{UInt64}), IntTy, NumWords, Words)
end

"""
    LLVMConstIntOfString(IntTy, Text, Radix)

Obtain a constant value for an integer parsed from a string.

A similar API, [`LLVMConstIntOfStringAndSize`](@ref) is also available. If the string's length is available, it is preferred to call that function instead.

# See also
llvm::ConstantInt::get()
"""
function LLVMConstIntOfString(IntTy, Text, Radix)
    ccall((:LLVMConstIntOfString, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring, UInt8), IntTy, Text, Radix)
end

"""
    LLVMConstIntOfStringAndSize(IntTy, Text, SLen, Radix)

Obtain a constant value for an integer parsed from a string with specified length.

# See also
llvm::ConstantInt::get()
"""
function LLVMConstIntOfStringAndSize(IntTy, Text, SLen, Radix)
    ccall((:LLVMConstIntOfStringAndSize, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring, Cuint, UInt8), IntTy, Text, SLen, Radix)
end

"""
    LLVMConstReal(RealTy, N)

Obtain a constant value referring to a double floating point value.
"""
function LLVMConstReal(RealTy, N)
    ccall((:LLVMConstReal, libllvm), LLVMValueRef, (LLVMTypeRef, Cdouble), RealTy, N)
end

"""
    LLVMConstRealOfString(RealTy, Text)

Obtain a constant for a floating point value parsed from a string.

A similar API, [`LLVMConstRealOfStringAndSize`](@ref) is also available. It should be used if the input string's length is known.
"""
function LLVMConstRealOfString(RealTy, Text)
    ccall((:LLVMConstRealOfString, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring), RealTy, Text)
end

"""
    LLVMConstRealOfStringAndSize(RealTy, Text, SLen)

Obtain a constant for a floating point value parsed from a string.
"""
function LLVMConstRealOfStringAndSize(RealTy, Text, SLen)
    ccall((:LLVMConstRealOfStringAndSize, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring, Cuint), RealTy, Text, SLen)
end

"""
    LLVMConstIntGetZExtValue(ConstantVal)

Obtain the zero extended value for an integer constant value.

# See also
llvm::ConstantInt::getZExtValue()
"""
function LLVMConstIntGetZExtValue(ConstantVal)
    ccall((:LLVMConstIntGetZExtValue, libllvm), Culonglong, (LLVMValueRef,), ConstantVal)
end

"""
    LLVMConstIntGetSExtValue(ConstantVal)

Obtain the sign extended value for an integer constant value.

# See also
llvm::ConstantInt::getSExtValue()
"""
function LLVMConstIntGetSExtValue(ConstantVal)
    ccall((:LLVMConstIntGetSExtValue, libllvm), Clonglong, (LLVMValueRef,), ConstantVal)
end

"""
    LLVMConstRealGetDouble(ConstantVal, losesInfo)

Obtain the double value for an floating point constant value. losesInfo indicates if some precision was lost in the conversion.

# See also
llvm::ConstantFP::getDoubleValue
"""
function LLVMConstRealGetDouble(ConstantVal, losesInfo)
    ccall((:LLVMConstRealGetDouble, libllvm), Cdouble, (LLVMValueRef, Ptr{LLVMBool}), ConstantVal, losesInfo)
end

"""
    LLVMConstStringInContext(C, Str, Length, DontNullTerminate)

Create a ConstantDataSequential and initialize it with a string.

# See also
llvm::ConstantDataArray::getString()
"""
function LLVMConstStringInContext(C, Str, Length, DontNullTerminate)
    ccall((:LLVMConstStringInContext, libllvm), LLVMValueRef, (LLVMContextRef, Cstring, Cuint, LLVMBool), C, Str, Length, DontNullTerminate)
end

"""
    LLVMConstString(Str, Length, DontNullTerminate)

Create a ConstantDataSequential with string content in the global context.

This is the same as [`LLVMConstStringInContext`](@ref) except it operates on the global context.

# See also
[`LLVMConstStringInContext`](@ref)(), llvm::ConstantDataArray::getString()
"""
function LLVMConstString(Str, Length, DontNullTerminate)
    ccall((:LLVMConstString, libllvm), LLVMValueRef, (Cstring, Cuint, LLVMBool), Str, Length, DontNullTerminate)
end

"""
    LLVMIsConstantString(c)

Returns true if the specified constant is an array of i8.

# See also
ConstantDataSequential::getAsString()
"""
function LLVMIsConstantString(c)
    ccall((:LLVMIsConstantString, libllvm), LLVMBool, (LLVMValueRef,), c)
end

"""
    LLVMGetAsString(c, Length)

Get the given constant data sequential as a string.

# See also
ConstantDataSequential::getAsString()
"""
function LLVMGetAsString(c, Length)
    ccall((:LLVMGetAsString, libllvm), Cstring, (LLVMValueRef, Ptr{Csize_t}), c, Length)
end

"""
    LLVMConstStructInContext(C, ConstantVals, Count, Packed)

Create an anonymous ConstantStruct with the specified values.

# See also
llvm::ConstantStruct::getAnon()
"""
function LLVMConstStructInContext(C, ConstantVals, Count, Packed)
    ccall((:LLVMConstStructInContext, libllvm), LLVMValueRef, (LLVMContextRef, Ptr{LLVMValueRef}, Cuint, LLVMBool), C, ConstantVals, Count, Packed)
end

"""
    LLVMConstStruct(ConstantVals, Count, Packed)

Create a ConstantStruct in the global Context.

This is the same as [`LLVMConstStructInContext`](@ref) except it operates on the global Context.

# See also
[`LLVMConstStructInContext`](@ref)()
"""
function LLVMConstStruct(ConstantVals, Count, Packed)
    ccall((:LLVMConstStruct, libllvm), LLVMValueRef, (Ptr{LLVMValueRef}, Cuint, LLVMBool), ConstantVals, Count, Packed)
end

"""
    LLVMConstArray(ElementTy, ConstantVals, Length)

Create a ConstantArray from values.

# See also
llvm::ConstantArray::get()
"""
function LLVMConstArray(ElementTy, ConstantVals, Length)
    ccall((:LLVMConstArray, libllvm), LLVMValueRef, (LLVMTypeRef, Ptr{LLVMValueRef}, Cuint), ElementTy, ConstantVals, Length)
end

"""
    LLVMConstNamedStruct(StructTy, ConstantVals, Count)

Create a non-anonymous ConstantStruct from values.

# See also
llvm::ConstantStruct::get()
"""
function LLVMConstNamedStruct(StructTy, ConstantVals, Count)
    ccall((:LLVMConstNamedStruct, libllvm), LLVMValueRef, (LLVMTypeRef, Ptr{LLVMValueRef}, Cuint), StructTy, ConstantVals, Count)
end

"""
    LLVMGetAggregateElement(C, Idx)

Get element of a constant aggregate (struct, array or vector) at the specified index. Returns null if the index is out of range, or it's not possible to determine the element (e.g., because the constant is a constant expression.)

# See also
llvm::Constant::getAggregateElement()
"""
function LLVMGetAggregateElement(C, Idx)
    ccall((:LLVMGetAggregateElement, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), C, Idx)
end

function LLVMGetElementAsConstant(C, idx)
    ccall((:LLVMGetElementAsConstant, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), C, idx)
end

"""
    LLVMConstVector(ScalarConstantVals, Size)

Create a ConstantVector from values.

# See also
llvm::ConstantVector::get()
"""
function LLVMConstVector(ScalarConstantVals, Size)
    ccall((:LLVMConstVector, libllvm), LLVMValueRef, (Ptr{LLVMValueRef}, Cuint), ScalarConstantVals, Size)
end

"""
    LLVMGetConstOpcode(ConstantVal)

` LLVMCCoreValueConstantExpressions Constant Expressions`

Functions in this group correspond to APIs on llvm::ConstantExpr.

@{

# See also
llvm::ConstantExpr.
"""
function LLVMGetConstOpcode(ConstantVal)
    ccall((:LLVMGetConstOpcode, libllvm), LLVMOpcode, (LLVMValueRef,), ConstantVal)
end

function LLVMAlignOf(Ty)
    ccall((:LLVMAlignOf, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

function LLVMSizeOf(Ty)
    ccall((:LLVMSizeOf, libllvm), LLVMValueRef, (LLVMTypeRef,), Ty)
end

function LLVMConstNeg(ConstantVal)
    ccall((:LLVMConstNeg, libllvm), LLVMValueRef, (LLVMValueRef,), ConstantVal)
end

function LLVMConstNSWNeg(ConstantVal)
    ccall((:LLVMConstNSWNeg, libllvm), LLVMValueRef, (LLVMValueRef,), ConstantVal)
end

function LLVMConstNUWNeg(ConstantVal)
    ccall((:LLVMConstNUWNeg, libllvm), LLVMValueRef, (LLVMValueRef,), ConstantVal)
end

function LLVMConstFNeg(ConstantVal)
    ccall((:LLVMConstFNeg, libllvm), LLVMValueRef, (LLVMValueRef,), ConstantVal)
end

function LLVMConstNot(ConstantVal)
    ccall((:LLVMConstNot, libllvm), LLVMValueRef, (LLVMValueRef,), ConstantVal)
end

function LLVMConstAdd(LHSConstant, RHSConstant)
    ccall((:LLVMConstAdd, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNSWAdd(LHSConstant, RHSConstant)
    ccall((:LLVMConstNSWAdd, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNUWAdd(LHSConstant, RHSConstant)
    ccall((:LLVMConstNUWAdd, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstSub(LHSConstant, RHSConstant)
    ccall((:LLVMConstSub, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNSWSub(LHSConstant, RHSConstant)
    ccall((:LLVMConstNSWSub, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNUWSub(LHSConstant, RHSConstant)
    ccall((:LLVMConstNUWSub, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstMul(LHSConstant, RHSConstant)
    ccall((:LLVMConstMul, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNSWMul(LHSConstant, RHSConstant)
    ccall((:LLVMConstNSWMul, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstNUWMul(LHSConstant, RHSConstant)
    ccall((:LLVMConstNUWMul, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstAnd(LHSConstant, RHSConstant)
    ccall((:LLVMConstAnd, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstOr(LHSConstant, RHSConstant)
    ccall((:LLVMConstOr, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstXor(LHSConstant, RHSConstant)
    ccall((:LLVMConstXor, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstICmp(Predicate, LHSConstant, RHSConstant)
    ccall((:LLVMConstICmp, libllvm), LLVMValueRef, (LLVMIntPredicate, LLVMValueRef, LLVMValueRef), Predicate, LHSConstant, RHSConstant)
end

function LLVMConstFCmp(Predicate, LHSConstant, RHSConstant)
    ccall((:LLVMConstFCmp, libllvm), LLVMValueRef, (LLVMRealPredicate, LLVMValueRef, LLVMValueRef), Predicate, LHSConstant, RHSConstant)
end

function LLVMConstShl(LHSConstant, RHSConstant)
    ccall((:LLVMConstShl, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstLShr(LHSConstant, RHSConstant)
    ccall((:LLVMConstLShr, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstAShr(LHSConstant, RHSConstant)
    ccall((:LLVMConstAShr, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), LHSConstant, RHSConstant)
end

function LLVMConstGEP(ConstantVal, ConstantIndices, NumIndices)
    ccall((:LLVMConstGEP, libllvm), LLVMValueRef, (LLVMValueRef, Ptr{LLVMValueRef}, Cuint), ConstantVal, ConstantIndices, NumIndices)
end

function LLVMConstGEP2(Ty, ConstantVal, ConstantIndices, NumIndices)
    ccall((:LLVMConstGEP2, libllvm), LLVMValueRef, (LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint), Ty, ConstantVal, ConstantIndices, NumIndices)
end

function LLVMConstInBoundsGEP(ConstantVal, ConstantIndices, NumIndices)
    ccall((:LLVMConstInBoundsGEP, libllvm), LLVMValueRef, (LLVMValueRef, Ptr{LLVMValueRef}, Cuint), ConstantVal, ConstantIndices, NumIndices)
end

function LLVMConstInBoundsGEP2(Ty, ConstantVal, ConstantIndices, NumIndices)
    ccall((:LLVMConstInBoundsGEP2, libllvm), LLVMValueRef, (LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint), Ty, ConstantVal, ConstantIndices, NumIndices)
end

function LLVMConstTrunc(ConstantVal, ToType)
    ccall((:LLVMConstTrunc, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstSExt(ConstantVal, ToType)
    ccall((:LLVMConstSExt, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstZExt(ConstantVal, ToType)
    ccall((:LLVMConstZExt, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstFPTrunc(ConstantVal, ToType)
    ccall((:LLVMConstFPTrunc, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstFPExt(ConstantVal, ToType)
    ccall((:LLVMConstFPExt, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstUIToFP(ConstantVal, ToType)
    ccall((:LLVMConstUIToFP, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstSIToFP(ConstantVal, ToType)
    ccall((:LLVMConstSIToFP, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstFPToUI(ConstantVal, ToType)
    ccall((:LLVMConstFPToUI, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstFPToSI(ConstantVal, ToType)
    ccall((:LLVMConstFPToSI, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstPtrToInt(ConstantVal, ToType)
    ccall((:LLVMConstPtrToInt, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstIntToPtr(ConstantVal, ToType)
    ccall((:LLVMConstIntToPtr, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstBitCast(ConstantVal, ToType)
    ccall((:LLVMConstBitCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstAddrSpaceCast(ConstantVal, ToType)
    ccall((:LLVMConstAddrSpaceCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstZExtOrBitCast(ConstantVal, ToType)
    ccall((:LLVMConstZExtOrBitCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstSExtOrBitCast(ConstantVal, ToType)
    ccall((:LLVMConstSExtOrBitCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstTruncOrBitCast(ConstantVal, ToType)
    ccall((:LLVMConstTruncOrBitCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstPointerCast(ConstantVal, ToType)
    ccall((:LLVMConstPointerCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstIntCast(ConstantVal, ToType, isSigned)
    ccall((:LLVMConstIntCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef, LLVMBool), ConstantVal, ToType, isSigned)
end

function LLVMConstFPCast(ConstantVal, ToType)
    ccall((:LLVMConstFPCast, libllvm), LLVMValueRef, (LLVMValueRef, LLVMTypeRef), ConstantVal, ToType)
end

function LLVMConstSelect(ConstantCondition, ConstantIfTrue, ConstantIfFalse)
    ccall((:LLVMConstSelect, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef, LLVMValueRef), ConstantCondition, ConstantIfTrue, ConstantIfFalse)
end

function LLVMConstExtractElement(VectorConstant, IndexConstant)
    ccall((:LLVMConstExtractElement, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef), VectorConstant, IndexConstant)
end

function LLVMConstInsertElement(VectorConstant, ElementValueConstant, IndexConstant)
    ccall((:LLVMConstInsertElement, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef, LLVMValueRef), VectorConstant, ElementValueConstant, IndexConstant)
end

function LLVMConstShuffleVector(VectorAConstant, VectorBConstant, MaskConstant)
    ccall((:LLVMConstShuffleVector, libllvm), LLVMValueRef, (LLVMValueRef, LLVMValueRef, LLVMValueRef), VectorAConstant, VectorBConstant, MaskConstant)
end

"""
Represents a basic block of instructions in LLVM IR.

This models llvm::BasicBlock.
"""
const LLVMBasicBlockRef = Ptr{LLVMOpaqueBasicBlock}

function LLVMBlockAddress(F, BB)
    ccall((:LLVMBlockAddress, libllvm), LLVMValueRef, (LLVMValueRef, LLVMBasicBlockRef), F, BB)
end

"""
    LLVMConstInlineAsm(Ty, AsmString, Constraints, HasSideEffects, IsAlignStack)

Deprecated: Use [`LLVMGetInlineAsm`](@ref) instead.
"""
function LLVMConstInlineAsm(Ty, AsmString, Constraints, HasSideEffects, IsAlignStack)
    ccall((:LLVMConstInlineAsm, libllvm), LLVMValueRef, (LLVMTypeRef, Cstring, Cstring, LLVMBool, LLVMBool), Ty, AsmString, Constraints, HasSideEffects, IsAlignStack)
end

"""
    LLVMGetGlobalParent(Global)

` LLVMCCoreValueConstantGlobals Global Values`

This group contains functions that operate on global values. Functions in this group relate to functions in the llvm::GlobalValue class tree.

@{

# See also
llvm::GlobalValue
"""
function LLVMGetGlobalParent(Global)
    ccall((:LLVMGetGlobalParent, libllvm), LLVMModuleRef, (LLVMValueRef,), Global)
end

function LLVMIsDeclaration(Global)
    ccall((:LLVMIsDeclaration, libllvm), LLVMBool, (LLVMValueRef,), Global)
end

function LLVMGetLinkage(Global)
    ccall((:LLVMGetLinkage, libllvm), LLVMLinkage, (LLVMValueRef,), Global)
end

function LLVMSetLinkage(Global, Linkage)
    ccall((:LLVMSetLinkage, libllvm), Cvoid, (LLVMValueRef, LLVMLinkage), Global, Linkage)
end

function LLVMGetSection(Global)
    ccall((:LLVMGetSection, libllvm), Cstring, (LLVMValueRef,), Global)
end

function LLVMSetSection(Global, Section)
    ccall((:LLVMSetSection, libllvm), Cvoid, (LLVMValueRef, Cstring), Global, Section)
end

function LLVMGetVisibility(Global)
    ccall((:LLVMGetVisibility, libllvm), LLVMVisibility, (LLVMValueRef,), Global)
end

function LLVMSetVisibility(Global, Viz)
    ccall((:LLVMSetVisibility, libllvm), Cvoid, (LLVMValueRef, LLVMVisibility), Global, Viz)
end

function LLVMGetDLLStorageClass(Global)
    ccall((:LLVMGetDLLStorageClass, libllvm), LLVMDLLStorageClass, (LLVMValueRef,), Global)
end

function LLVMSetDLLStorageClass(Global, Class)
    ccall((:LLVMSetDLLStorageClass, libllvm), Cvoid, (LLVMValueRef, LLVMDLLStorageClass), Global, Class)
end

function LLVMGetUnnamedAddress(Global)
    ccall((:LLVMGetUnnamedAddress, libllvm), LLVMUnnamedAddr, (LLVMValueRef,), Global)
end

function LLVMSetUnnamedAddress(Global, UnnamedAddr)
    ccall((:LLVMSetUnnamedAddress, libllvm), Cvoid, (LLVMValueRef, LLVMUnnamedAddr), Global, UnnamedAddr)
end

"""
    LLVMGlobalGetValueType(Global)

Returns the "value type" of a global value. This differs from the formal type of a global value which is always a pointer type.

# See also
llvm::GlobalValue::getValueType()
"""
function LLVMGlobalGetValueType(Global)
    ccall((:LLVMGlobalGetValueType, libllvm), LLVMTypeRef, (LLVMValueRef,), Global)
end

"""
    LLVMHasUnnamedAddr(Global)

Deprecated: Use [`LLVMGetUnnamedAddress`](@ref) instead.
"""
function LLVMHasUnnamedAddr(Global)
    ccall((:LLVMHasUnnamedAddr, libllvm), LLVMBool, (LLVMValueRef,), Global)
end

"""
    LLVMSetUnnamedAddr(Global, HasUnnamedAddr)

Deprecated: Use [`LLVMSetUnnamedAddress`](@ref) instead.
"""
function LLVMSetUnnamedAddr(Global, HasUnnamedAddr)
    ccall((:LLVMSetUnnamedAddr, libllvm), Cvoid, (LLVMValueRef, LLVMBool), Global, HasUnnamedAddr)
end

"""
    LLVMGetAlignment(V)

Obtain the preferred alignment of the value.

# See also
llvm::AllocaInst::getAlignment(), llvm::LoadInst::getAlignment(), llvm::StoreInst::getAlignment(), llvm::AtomicRMWInst::setAlignment(), llvm::AtomicCmpXchgInst::setAlignment(), llvm::GlobalValue::getAlignment()
"""
function LLVMGetAlignment(V)
    ccall((:LLVMGetAlignment, libllvm), Cuint, (LLVMValueRef,), V)
end

"""
    LLVMSetAlignment(V, Bytes)

Set the preferred alignment of the value.

# See also
llvm::AllocaInst::setAlignment(), llvm::LoadInst::setAlignment(), llvm::StoreInst::setAlignment(), llvm::AtomicRMWInst::setAlignment(), llvm::AtomicCmpXchgInst::setAlignment(), llvm::GlobalValue::setAlignment()
"""
function LLVMSetAlignment(V, Bytes)
    ccall((:LLVMSetAlignment, libllvm), Cvoid, (LLVMValueRef, Cuint), V, Bytes)
end

"""
    LLVMGlobalSetMetadata(Global, Kind, MD)

Sets a metadata attachment, erasing the existing metadata attachment if it already exists for the given kind.

# See also
llvm::GlobalObject::setMetadata()
"""
function LLVMGlobalSetMetadata(Global, Kind, MD)
    ccall((:LLVMGlobalSetMetadata, libllvm), Cvoid, (LLVMValueRef, Cuint, LLVMMetadataRef), Global, Kind, MD)
end

"""
    LLVMGlobalEraseMetadata(Global, Kind)

Erases a metadata attachment of the given kind if it exists.

# See also
llvm::GlobalObject::eraseMetadata()
"""
function LLVMGlobalEraseMetadata(Global, Kind)
    ccall((:LLVMGlobalEraseMetadata, libllvm), Cvoid, (LLVMValueRef, Cuint), Global, Kind)
end

"""
    LLVMGlobalClearMetadata(Global)

Removes all metadata attachments from this value.

# See also
llvm::GlobalObject::clearMetadata()
"""
function LLVMGlobalClearMetadata(Global)
    ccall((:LLVMGlobalClearMetadata, libllvm), Cvoid, (LLVMValueRef,), Global)
end

"""
Represents an entry in a Global Object's metadata attachments.

This models std::pair<unsigned, MDNode *>
"""
const LLVMValueMetadataEntry = LLVMOpaqueValueMetadataEntry

"""
    LLVMGlobalCopyAllMetadata(Value, NumEntries)

Retrieves an array of metadata entries representing the metadata attached to this value. The caller is responsible for freeing this array by calling [`LLVMDisposeValueMetadataEntries`](@ref).

# See also
llvm::GlobalObject::getAllMetadata()
"""
function LLVMGlobalCopyAllMetadata(Value, NumEntries)
    ccall((:LLVMGlobalCopyAllMetadata, libllvm), Ptr{LLVMValueMetadataEntry}, (LLVMValueRef, Ptr{Csize_t}), Value, NumEntries)
end

"""
    LLVMDisposeValueMetadataEntries(Entries)

Destroys value metadata entries.
"""
function LLVMDisposeValueMetadataEntries(Entries)
    ccall((:LLVMDisposeValueMetadataEntries, libllvm), Cvoid, (Ptr{LLVMValueMetadataEntry},), Entries)
end

"""
    LLVMValueMetadataEntriesGetKind(Entries, Index)

Returns the kind of a value metadata entry at a specific index.
"""
function LLVMValueMetadataEntriesGetKind(Entries, Index)
    ccall((:LLVMValueMetadataEntriesGetKind, libllvm), Cuint, (Ptr{LLVMValueMetadataEntry}, Cuint), Entries, Index)
end

"""
    LLVMValueMetadataEntriesGetMetadata(Entries, Index)

Returns the underlying metadata node of a value metadata entry at a specific index.
"""
function LLVMValueMetadataEntriesGetMetadata(Entries, Index)
    ccall((:LLVMValueMetadataEntriesGetMetadata, libllvm), LLVMMetadataRef, (Ptr{LLVMValueMetadataEntry}, Cuint), Entries, Index)
end

"""
    LLVMAddGlobal(M, Ty, Name)

` LLVMCoreValueConstantGlobalVariable Global Variables`

This group contains functions that operate on global variable values.

@{

# See also
llvm::GlobalVariable
"""
function LLVMAddGlobal(M, Ty, Name)
    ccall((:LLVMAddGlobal, libllvm), LLVMValueRef, (LLVMModuleRef, LLVMTypeRef, Cstring), M, Ty, Name)
end

function LLVMAddGlobalInAddressSpace(M, Ty, Name, AddressSpace)
    ccall((:LLVMAddGlobalInAddressSpace, libllvm), LLVMValueRef, (LLVMModuleRef, LLVMTypeRef, Cstring, Cuint), M, Ty, Name, AddressSpace)
end

function LLVMGetNamedGlobal(M, Name)
    ccall((:LLVMGetNamedGlobal, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring), M, Name)
end

function LLVMGetFirstGlobal(M)
    ccall((:LLVMGetFirstGlobal, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

function LLVMGetLastGlobal(M)
    ccall((:LLVMGetLastGlobal, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

function LLVMGetNextGlobal(GlobalVar)
    ccall((:LLVMGetNextGlobal, libllvm), LLVMValueRef, (LLVMValueRef,), GlobalVar)
end

function LLVMGetPreviousGlobal(GlobalVar)
    ccall((:LLVMGetPreviousGlobal, libllvm), LLVMValueRef, (LLVMValueRef,), GlobalVar)
end

function LLVMDeleteGlobal(GlobalVar)
    ccall((:LLVMDeleteGlobal, libllvm), Cvoid, (LLVMValueRef,), GlobalVar)
end

function LLVMGetInitializer(GlobalVar)
    ccall((:LLVMGetInitializer, libllvm), LLVMValueRef, (LLVMValueRef,), GlobalVar)
end

function LLVMSetInitializer(GlobalVar, ConstantVal)
    ccall((:LLVMSetInitializer, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), GlobalVar, ConstantVal)
end

function LLVMIsThreadLocal(GlobalVar)
    ccall((:LLVMIsThreadLocal, libllvm), LLVMBool, (LLVMValueRef,), GlobalVar)
end

function LLVMSetThreadLocal(GlobalVar, IsThreadLocal)
    ccall((:LLVMSetThreadLocal, libllvm), Cvoid, (LLVMValueRef, LLVMBool), GlobalVar, IsThreadLocal)
end

function LLVMIsGlobalConstant(GlobalVar)
    ccall((:LLVMIsGlobalConstant, libllvm), LLVMBool, (LLVMValueRef,), GlobalVar)
end

function LLVMSetGlobalConstant(GlobalVar, IsConstant)
    ccall((:LLVMSetGlobalConstant, libllvm), Cvoid, (LLVMValueRef, LLVMBool), GlobalVar, IsConstant)
end

function LLVMGetThreadLocalMode(GlobalVar)
    ccall((:LLVMGetThreadLocalMode, libllvm), LLVMThreadLocalMode, (LLVMValueRef,), GlobalVar)
end

function LLVMSetThreadLocalMode(GlobalVar, Mode)
    ccall((:LLVMSetThreadLocalMode, libllvm), Cvoid, (LLVMValueRef, LLVMThreadLocalMode), GlobalVar, Mode)
end

function LLVMIsExternallyInitialized(GlobalVar)
    ccall((:LLVMIsExternallyInitialized, libllvm), LLVMBool, (LLVMValueRef,), GlobalVar)
end

function LLVMSetExternallyInitialized(GlobalVar, IsExtInit)
    ccall((:LLVMSetExternallyInitialized, libllvm), Cvoid, (LLVMValueRef, LLVMBool), GlobalVar, IsExtInit)
end

function LLVMAddAlias(M, Ty, Aliasee, Name)
    ccall((:LLVMAddAlias, libllvm), LLVMValueRef, (LLVMModuleRef, LLVMTypeRef, LLVMValueRef, Cstring), M, Ty, Aliasee, Name)
end

"""
    LLVMAddAlias2(M, ValueTy, AddrSpace, Aliasee, Name)

Add a GlobalAlias with the given value type, address space and aliasee.

# See also
llvm::GlobalAlias::create()
"""
function LLVMAddAlias2(M, ValueTy, AddrSpace, Aliasee, Name)
    ccall((:LLVMAddAlias2, libllvm), LLVMValueRef, (LLVMModuleRef, LLVMTypeRef, Cuint, LLVMValueRef, Cstring), M, ValueTy, AddrSpace, Aliasee, Name)
end

"""
    LLVMGetNamedGlobalAlias(M, Name, NameLen)

Obtain a GlobalAlias value from a Module by its name.

The returned value corresponds to a llvm::GlobalAlias value.

# See also
llvm::Module::getNamedAlias()
"""
function LLVMGetNamedGlobalAlias(M, Name, NameLen)
    ccall((:LLVMGetNamedGlobalAlias, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring, Csize_t), M, Name, NameLen)
end

"""
    LLVMGetFirstGlobalAlias(M)

Obtain an iterator to the first GlobalAlias in a Module.

# See also
llvm::Module::alias\\_begin()
"""
function LLVMGetFirstGlobalAlias(M)
    ccall((:LLVMGetFirstGlobalAlias, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetLastGlobalAlias(M)

Obtain an iterator to the last GlobalAlias in a Module.

# See also
llvm::Module::alias\\_end()
"""
function LLVMGetLastGlobalAlias(M)
    ccall((:LLVMGetLastGlobalAlias, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetNextGlobalAlias(GA)

Advance a GlobalAlias iterator to the next GlobalAlias.

Returns NULL if the iterator was already at the end and there are no more global aliases.
"""
function LLVMGetNextGlobalAlias(GA)
    ccall((:LLVMGetNextGlobalAlias, libllvm), LLVMValueRef, (LLVMValueRef,), GA)
end

"""
    LLVMGetPreviousGlobalAlias(GA)

Decrement a GlobalAlias iterator to the previous GlobalAlias.

Returns NULL if the iterator was already at the beginning and there are no previous global aliases.
"""
function LLVMGetPreviousGlobalAlias(GA)
    ccall((:LLVMGetPreviousGlobalAlias, libllvm), LLVMValueRef, (LLVMValueRef,), GA)
end

"""
    LLVMAliasGetAliasee(Alias)

Retrieve the target value of an alias.
"""
function LLVMAliasGetAliasee(Alias)
    ccall((:LLVMAliasGetAliasee, libllvm), LLVMValueRef, (LLVMValueRef,), Alias)
end

"""
    LLVMAliasSetAliasee(Alias, Aliasee)

Set the target value of an alias.
"""
function LLVMAliasSetAliasee(Alias, Aliasee)
    ccall((:LLVMAliasSetAliasee, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), Alias, Aliasee)
end

"""
    LLVMDeleteFunction(Fn)

Remove a function from its containing module and deletes it.

# See also
llvm::Function::eraseFromParent()
"""
function LLVMDeleteFunction(Fn)
    ccall((:LLVMDeleteFunction, libllvm), Cvoid, (LLVMValueRef,), Fn)
end

"""
    LLVMHasPersonalityFn(Fn)

Check whether the given function has a personality function.

# See also
llvm::Function::hasPersonalityFn()
"""
function LLVMHasPersonalityFn(Fn)
    ccall((:LLVMHasPersonalityFn, libllvm), LLVMBool, (LLVMValueRef,), Fn)
end

"""
    LLVMGetPersonalityFn(Fn)

Obtain the personality function attached to the function.

# See also
llvm::Function::getPersonalityFn()
"""
function LLVMGetPersonalityFn(Fn)
    ccall((:LLVMGetPersonalityFn, libllvm), LLVMValueRef, (LLVMValueRef,), Fn)
end

"""
    LLVMSetPersonalityFn(Fn, PersonalityFn)

Set the personality function attached to the function.

# See also
llvm::Function::setPersonalityFn()
"""
function LLVMSetPersonalityFn(Fn, PersonalityFn)
    ccall((:LLVMSetPersonalityFn, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), Fn, PersonalityFn)
end

"""
    LLVMLookupIntrinsicID(Name, NameLen)

Obtain the intrinsic ID number which matches the given function name.

# See also
llvm::Function::lookupIntrinsicID()
"""
function LLVMLookupIntrinsicID(Name, NameLen)
    ccall((:LLVMLookupIntrinsicID, libllvm), Cuint, (Cstring, Csize_t), Name, NameLen)
end

"""
    LLVMGetIntrinsicID(Fn)

Obtain the ID number from a function instance.

# See also
llvm::Function::getIntrinsicID()
"""
function LLVMGetIntrinsicID(Fn)
    ccall((:LLVMGetIntrinsicID, libllvm), Cuint, (LLVMValueRef,), Fn)
end

"""
    LLVMGetIntrinsicDeclaration(Mod, ID, ParamTypes, ParamCount)

Create or insert the declaration of an intrinsic. For overloaded intrinsics, parameter types must be provided to uniquely identify an overload.

# See also
llvm::Intrinsic::getDeclaration()
"""
function LLVMGetIntrinsicDeclaration(Mod, ID, ParamTypes, ParamCount)
    ccall((:LLVMGetIntrinsicDeclaration, libllvm), LLVMValueRef, (LLVMModuleRef, Cuint, Ptr{LLVMTypeRef}, Csize_t), Mod, ID, ParamTypes, ParamCount)
end

"""
    LLVMIntrinsicGetType(Ctx, ID, ParamTypes, ParamCount)

Retrieves the type of an intrinsic. For overloaded intrinsics, parameter types must be provided to uniquely identify an overload.

# See also
llvm::Intrinsic::getType()
"""
function LLVMIntrinsicGetType(Ctx, ID, ParamTypes, ParamCount)
    ccall((:LLVMIntrinsicGetType, libllvm), LLVMTypeRef, (LLVMContextRef, Cuint, Ptr{LLVMTypeRef}, Csize_t), Ctx, ID, ParamTypes, ParamCount)
end

"""
    LLVMIntrinsicGetName(ID, NameLength)

Retrieves the name of an intrinsic.

# See also
llvm::Intrinsic::getName()
"""
function LLVMIntrinsicGetName(ID, NameLength)
    ccall((:LLVMIntrinsicGetName, libllvm), Cstring, (Cuint, Ptr{Csize_t}), ID, NameLength)
end

"""
    LLVMIntrinsicCopyOverloadedName(ID, ParamTypes, ParamCount, NameLength)

Deprecated: Use [`LLVMIntrinsicCopyOverloadedName2`](@ref) instead.
"""
function LLVMIntrinsicCopyOverloadedName(ID, ParamTypes, ParamCount, NameLength)
    ccall((:LLVMIntrinsicCopyOverloadedName, libllvm), Cstring, (Cuint, Ptr{LLVMTypeRef}, Csize_t, Ptr{Csize_t}), ID, ParamTypes, ParamCount, NameLength)
end

"""
    LLVMIntrinsicCopyOverloadedName2(Mod, ID, ParamTypes, ParamCount, NameLength)

Copies the name of an overloaded intrinsic identified by a given list of parameter types.

Unlike [`LLVMIntrinsicGetName`](@ref), the caller is responsible for freeing the returned string.

This version also supports unnamed types.

# See also
llvm::Intrinsic::getName()
"""
function LLVMIntrinsicCopyOverloadedName2(Mod, ID, ParamTypes, ParamCount, NameLength)
    ccall((:LLVMIntrinsicCopyOverloadedName2, libllvm), Cstring, (LLVMModuleRef, Cuint, Ptr{LLVMTypeRef}, Csize_t, Ptr{Csize_t}), Mod, ID, ParamTypes, ParamCount, NameLength)
end

"""
    LLVMIntrinsicIsOverloaded(ID)

Obtain if the intrinsic identified by the given ID is overloaded.

# See also
llvm::Intrinsic::isOverloaded()
"""
function LLVMIntrinsicIsOverloaded(ID)
    ccall((:LLVMIntrinsicIsOverloaded, libllvm), LLVMBool, (Cuint,), ID)
end

"""
    LLVMGetFunctionCallConv(Fn)

Obtain the calling function of a function.

The returned value corresponds to the [`LLVMCallConv`](@ref) enumeration.

# See also
llvm::Function::getCallingConv()
"""
function LLVMGetFunctionCallConv(Fn)
    ccall((:LLVMGetFunctionCallConv, libllvm), Cuint, (LLVMValueRef,), Fn)
end

"""
    LLVMSetFunctionCallConv(Fn, CC)

Set the calling convention of a function.

# Arguments
* `Fn`: Function to operate on
* `CC`: [`LLVMCallConv`](@ref) to set calling convention to
# See also
llvm::Function::setCallingConv()
"""
function LLVMSetFunctionCallConv(Fn, CC)
    ccall((:LLVMSetFunctionCallConv, libllvm), Cvoid, (LLVMValueRef, Cuint), Fn, CC)
end

"""
    LLVMGetGC(Fn)

Obtain the name of the garbage collector to use during code generation.

# See also
llvm::Function::getGC()
"""
function LLVMGetGC(Fn)
    ccall((:LLVMGetGC, libllvm), Cstring, (LLVMValueRef,), Fn)
end

"""
    LLVMSetGC(Fn, Name)

Define the garbage collector to use during code generation.

# See also
llvm::Function::setGC()
"""
function LLVMSetGC(Fn, Name)
    ccall((:LLVMSetGC, libllvm), Cvoid, (LLVMValueRef, Cstring), Fn, Name)
end

"""
    LLVMAddAttributeAtIndex(F, Idx, A)

Add an attribute to a function.

# See also
llvm::Function::addAttribute()
"""
function LLVMAddAttributeAtIndex(F, Idx, A)
    ccall((:LLVMAddAttributeAtIndex, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, LLVMAttributeRef), F, Idx, A)
end

function LLVMGetAttributeCountAtIndex(F, Idx)
    ccall((:LLVMGetAttributeCountAtIndex, libllvm), Cuint, (LLVMValueRef, LLVMAttributeIndex), F, Idx)
end

function LLVMGetAttributesAtIndex(F, Idx, Attrs)
    ccall((:LLVMGetAttributesAtIndex, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Ptr{LLVMAttributeRef}), F, Idx, Attrs)
end

function LLVMGetEnumAttributeAtIndex(F, Idx, KindID)
    ccall((:LLVMGetEnumAttributeAtIndex, libllvm), LLVMAttributeRef, (LLVMValueRef, LLVMAttributeIndex, Cuint), F, Idx, KindID)
end

function LLVMGetStringAttributeAtIndex(F, Idx, K, KLen)
    ccall((:LLVMGetStringAttributeAtIndex, libllvm), LLVMAttributeRef, (LLVMValueRef, LLVMAttributeIndex, Cstring, Cuint), F, Idx, K, KLen)
end

function LLVMRemoveEnumAttributeAtIndex(F, Idx, KindID)
    ccall((:LLVMRemoveEnumAttributeAtIndex, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Cuint), F, Idx, KindID)
end

function LLVMRemoveStringAttributeAtIndex(F, Idx, K, KLen)
    ccall((:LLVMRemoveStringAttributeAtIndex, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Cstring, Cuint), F, Idx, K, KLen)
end

"""
    LLVMAddTargetDependentFunctionAttr(Fn, A, V)

Add a target-dependent attribute to a function

# See also
llvm::AttrBuilder::addAttribute()
"""
function LLVMAddTargetDependentFunctionAttr(Fn, A, V)
    ccall((:LLVMAddTargetDependentFunctionAttr, libllvm), Cvoid, (LLVMValueRef, Cstring, Cstring), Fn, A, V)
end

"""
    LLVMCountParams(Fn)

Obtain the number of parameters in a function.

# See also
llvm::Function::arg\\_size()
"""
function LLVMCountParams(Fn)
    ccall((:LLVMCountParams, libllvm), Cuint, (LLVMValueRef,), Fn)
end

"""
    LLVMGetParams(Fn, Params)

Obtain the parameters in a function.

The takes a pointer to a pre-allocated array of [`LLVMValueRef`](@ref) that is at least [`LLVMCountParams`](@ref)() long. This array will be filled with [`LLVMValueRef`](@ref) instances which correspond to the parameters the function receives. Each [`LLVMValueRef`](@ref) corresponds to a llvm::Argument instance.

# See also
llvm::Function::arg\\_begin()
"""
function LLVMGetParams(Fn, Params)
    ccall((:LLVMGetParams, libllvm), Cvoid, (LLVMValueRef, Ptr{LLVMValueRef}), Fn, Params)
end

"""
    LLVMGetParam(Fn, Index)

Obtain the parameter at the specified index.

Parameters are indexed from 0.

# See also
llvm::Function::arg\\_begin()
"""
function LLVMGetParam(Fn, Index)
    ccall((:LLVMGetParam, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), Fn, Index)
end

"""
    LLVMGetParamParent(Inst)

Obtain the function to which this argument belongs.

Unlike other functions in this group, this one takes an [`LLVMValueRef`](@ref) that corresponds to a llvm::Attribute.

The returned [`LLVMValueRef`](@ref) is the llvm::Function to which this argument belongs.
"""
function LLVMGetParamParent(Inst)
    ccall((:LLVMGetParamParent, libllvm), LLVMValueRef, (LLVMValueRef,), Inst)
end

"""
    LLVMGetFirstParam(Fn)

Obtain the first parameter to a function.

# See also
llvm::Function::arg\\_begin()
"""
function LLVMGetFirstParam(Fn)
    ccall((:LLVMGetFirstParam, libllvm), LLVMValueRef, (LLVMValueRef,), Fn)
end

"""
    LLVMGetLastParam(Fn)

Obtain the last parameter to a function.

# See also
llvm::Function::arg\\_end()
"""
function LLVMGetLastParam(Fn)
    ccall((:LLVMGetLastParam, libllvm), LLVMValueRef, (LLVMValueRef,), Fn)
end

"""
    LLVMGetNextParam(Arg)

Obtain the next parameter to a function.

This takes an [`LLVMValueRef`](@ref) obtained from [`LLVMGetFirstParam`](@ref)() (which is actually a wrapped iterator) and obtains the next parameter from the underlying iterator.
"""
function LLVMGetNextParam(Arg)
    ccall((:LLVMGetNextParam, libllvm), LLVMValueRef, (LLVMValueRef,), Arg)
end

"""
    LLVMGetPreviousParam(Arg)

Obtain the previous parameter to a function.

This is the opposite of [`LLVMGetNextParam`](@ref)().
"""
function LLVMGetPreviousParam(Arg)
    ccall((:LLVMGetPreviousParam, libllvm), LLVMValueRef, (LLVMValueRef,), Arg)
end

"""
    LLVMSetParamAlignment(Arg, Align)

Set the alignment for a function parameter.

# See also
llvm::Argument::addAttr(), llvm::AttrBuilder::addAlignmentAttr()
"""
function LLVMSetParamAlignment(Arg, Align)
    ccall((:LLVMSetParamAlignment, libllvm), Cvoid, (LLVMValueRef, Cuint), Arg, Align)
end

"""
    LLVMAddGlobalIFunc(M, Name, NameLen, Ty, AddrSpace, Resolver)

Add a global indirect function to a module under a specified name.

# See also
llvm::GlobalIFunc::create()
"""
function LLVMAddGlobalIFunc(M, Name, NameLen, Ty, AddrSpace, Resolver)
    ccall((:LLVMAddGlobalIFunc, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring, Csize_t, LLVMTypeRef, Cuint, LLVMValueRef), M, Name, NameLen, Ty, AddrSpace, Resolver)
end

"""
    LLVMGetNamedGlobalIFunc(M, Name, NameLen)

Obtain a GlobalIFunc value from a Module by its name.

The returned value corresponds to a llvm::GlobalIFunc value.

# See also
llvm::Module::getNamedIFunc()
"""
function LLVMGetNamedGlobalIFunc(M, Name, NameLen)
    ccall((:LLVMGetNamedGlobalIFunc, libllvm), LLVMValueRef, (LLVMModuleRef, Cstring, Csize_t), M, Name, NameLen)
end

"""
    LLVMGetFirstGlobalIFunc(M)

Obtain an iterator to the first GlobalIFunc in a Module.

# See also
llvm::Module::ifunc\\_begin()
"""
function LLVMGetFirstGlobalIFunc(M)
    ccall((:LLVMGetFirstGlobalIFunc, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetLastGlobalIFunc(M)

Obtain an iterator to the last GlobalIFunc in a Module.

# See also
llvm::Module::ifunc\\_end()
"""
function LLVMGetLastGlobalIFunc(M)
    ccall((:LLVMGetLastGlobalIFunc, libllvm), LLVMValueRef, (LLVMModuleRef,), M)
end

"""
    LLVMGetNextGlobalIFunc(IFunc)

Advance a GlobalIFunc iterator to the next GlobalIFunc.

Returns NULL if the iterator was already at the end and there are no more global aliases.
"""
function LLVMGetNextGlobalIFunc(IFunc)
    ccall((:LLVMGetNextGlobalIFunc, libllvm), LLVMValueRef, (LLVMValueRef,), IFunc)
end

"""
    LLVMGetPreviousGlobalIFunc(IFunc)

Decrement a GlobalIFunc iterator to the previous GlobalIFunc.

Returns NULL if the iterator was already at the beginning and there are no previous global aliases.
"""
function LLVMGetPreviousGlobalIFunc(IFunc)
    ccall((:LLVMGetPreviousGlobalIFunc, libllvm), LLVMValueRef, (LLVMValueRef,), IFunc)
end

"""
    LLVMGetGlobalIFuncResolver(IFunc)

Retrieves the resolver function associated with this indirect function, or NULL if it doesn't not exist.

# See also
llvm::GlobalIFunc::getResolver()
"""
function LLVMGetGlobalIFuncResolver(IFunc)
    ccall((:LLVMGetGlobalIFuncResolver, libllvm), LLVMValueRef, (LLVMValueRef,), IFunc)
end

"""
    LLVMSetGlobalIFuncResolver(IFunc, Resolver)

Sets the resolver function associated with this indirect function.

# See also
llvm::GlobalIFunc::setResolver()
"""
function LLVMSetGlobalIFuncResolver(IFunc, Resolver)
    ccall((:LLVMSetGlobalIFuncResolver, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), IFunc, Resolver)
end

"""
    LLVMEraseGlobalIFunc(IFunc)

Remove a global indirect function from its parent module and delete it.

# See also
llvm::GlobalIFunc::eraseFromParent()
"""
function LLVMEraseGlobalIFunc(IFunc)
    ccall((:LLVMEraseGlobalIFunc, libllvm), Cvoid, (LLVMValueRef,), IFunc)
end

"""
    LLVMRemoveGlobalIFunc(IFunc)

Remove a global indirect function from its parent module.

This unlinks the global indirect function from its containing module but keeps it alive.

# See also
llvm::GlobalIFunc::removeFromParent()
"""
function LLVMRemoveGlobalIFunc(IFunc)
    ccall((:LLVMRemoveGlobalIFunc, libllvm), Cvoid, (LLVMValueRef,), IFunc)
end

"""
    LLVMMDStringInContext2(C, Str, SLen)

Create an MDString value from a given string value.

The MDString value does not take ownership of the given string, it remains the responsibility of the caller to free it.

# See also
llvm::MDString::get()
"""
function LLVMMDStringInContext2(C, Str, SLen)
    ccall((:LLVMMDStringInContext2, libllvm), LLVMMetadataRef, (LLVMContextRef, Cstring, Csize_t), C, Str, SLen)
end

"""
    LLVMMDNodeInContext2(C, MDs, Count)

Create an MDNode value with the given array of operands.

# See also
llvm::MDNode::get()
"""
function LLVMMDNodeInContext2(C, MDs, Count)
    ccall((:LLVMMDNodeInContext2, libllvm), LLVMMetadataRef, (LLVMContextRef, Ptr{LLVMMetadataRef}, Csize_t), C, MDs, Count)
end

"""
    LLVMMetadataAsValue(C, MD)

Obtain a Metadata as a Value.
"""
function LLVMMetadataAsValue(C, MD)
    ccall((:LLVMMetadataAsValue, libllvm), LLVMValueRef, (LLVMContextRef, LLVMMetadataRef), C, MD)
end

"""
    LLVMValueAsMetadata(Val)

Obtain a Value as a Metadata.
"""
function LLVMValueAsMetadata(Val)
    ccall((:LLVMValueAsMetadata, libllvm), LLVMMetadataRef, (LLVMValueRef,), Val)
end

"""
    LLVMGetMDString(V, Length)

Obtain the underlying string from a MDString value.

# Arguments
* `V`: Instance to obtain string from.
* `Length`: Memory address which will hold length of returned string.
# Returns
String data in MDString.
"""
function LLVMGetMDString(V, Length)
    ccall((:LLVMGetMDString, libllvm), Cstring, (LLVMValueRef, Ptr{Cuint}), V, Length)
end

"""
    LLVMGetMDNodeNumOperands(V)

Obtain the number of operands from an MDNode value.

# Arguments
* `V`: MDNode to get number of operands from.
# Returns
Number of operands of the MDNode.
"""
function LLVMGetMDNodeNumOperands(V)
    ccall((:LLVMGetMDNodeNumOperands, libllvm), Cuint, (LLVMValueRef,), V)
end

"""
    LLVMGetMDNodeOperands(V, Dest)

Obtain the given MDNode's operands.

The passed [`LLVMValueRef`](@ref) pointer should point to enough memory to hold all of the operands of the given MDNode (see [`LLVMGetMDNodeNumOperands`](@ref)) as LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the MDNode's operands.

# Arguments
* `V`: MDNode to get the operands from.
* `Dest`: Destination array for operands.
"""
function LLVMGetMDNodeOperands(V, Dest)
    ccall((:LLVMGetMDNodeOperands, libllvm), Cvoid, (LLVMValueRef, Ptr{LLVMValueRef}), V, Dest)
end

"""
    LLVMMDStringInContext(C, Str, SLen)

Deprecated: Use [`LLVMMDStringInContext2`](@ref) instead.
"""
function LLVMMDStringInContext(C, Str, SLen)
    ccall((:LLVMMDStringInContext, libllvm), LLVMValueRef, (LLVMContextRef, Cstring, Cuint), C, Str, SLen)
end

"""
    LLVMMDString(Str, SLen)

Deprecated: Use [`LLVMMDStringInContext2`](@ref) instead.
"""
function LLVMMDString(Str, SLen)
    ccall((:LLVMMDString, libllvm), LLVMValueRef, (Cstring, Cuint), Str, SLen)
end

"""
    LLVMMDNodeInContext(C, Vals, Count)

Deprecated: Use [`LLVMMDNodeInContext2`](@ref) instead.
"""
function LLVMMDNodeInContext(C, Vals, Count)
    ccall((:LLVMMDNodeInContext, libllvm), LLVMValueRef, (LLVMContextRef, Ptr{LLVMValueRef}, Cuint), C, Vals, Count)
end

"""
    LLVMMDNode(Vals, Count)

Deprecated: Use [`LLVMMDNodeInContext2`](@ref) instead.
"""
function LLVMMDNode(Vals, Count)
    ccall((:LLVMMDNode, libllvm), LLVMValueRef, (Ptr{LLVMValueRef}, Cuint), Vals, Count)
end

"""
    LLVMBasicBlockAsValue(BB)

Convert a basic block instance to a value type.
"""
function LLVMBasicBlockAsValue(BB)
    ccall((:LLVMBasicBlockAsValue, libllvm), LLVMValueRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMValueIsBasicBlock(Val)

Determine whether an [`LLVMValueRef`](@ref) is itself a basic block.
"""
function LLVMValueIsBasicBlock(Val)
    ccall((:LLVMValueIsBasicBlock, libllvm), LLVMBool, (LLVMValueRef,), Val)
end

"""
    LLVMValueAsBasicBlock(Val)

Convert an [`LLVMValueRef`](@ref) to an [`LLVMBasicBlockRef`](@ref) instance.
"""
function LLVMValueAsBasicBlock(Val)
    ccall((:LLVMValueAsBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), Val)
end

"""
    LLVMGetBasicBlockName(BB)

Obtain the string name of a basic block.
"""
function LLVMGetBasicBlockName(BB)
    ccall((:LLVMGetBasicBlockName, libllvm), Cstring, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMGetBasicBlockParent(BB)

Obtain the function to which a basic block belongs.

# See also
llvm::BasicBlock::getParent()
"""
function LLVMGetBasicBlockParent(BB)
    ccall((:LLVMGetBasicBlockParent, libllvm), LLVMValueRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMGetBasicBlockTerminator(BB)

Obtain the terminator instruction for a basic block.

If the basic block does not have a terminator (it is not well-formed if it doesn't), then NULL is returned.

The returned [`LLVMValueRef`](@ref) corresponds to an llvm::Instruction.

# See also
llvm::BasicBlock::getTerminator()
"""
function LLVMGetBasicBlockTerminator(BB)
    ccall((:LLVMGetBasicBlockTerminator, libllvm), LLVMValueRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMCountBasicBlocks(Fn)

Obtain the number of basic blocks in a function.

# Arguments
* `Fn`: Function value to operate on.
"""
function LLVMCountBasicBlocks(Fn)
    ccall((:LLVMCountBasicBlocks, libllvm), Cuint, (LLVMValueRef,), Fn)
end

"""
    LLVMGetBasicBlocks(Fn, BasicBlocks)

Obtain all of the basic blocks in a function.

This operates on a function value. The BasicBlocks parameter is a pointer to a pre-allocated array of [`LLVMBasicBlockRef`](@ref) of at least [`LLVMCountBasicBlocks`](@ref)() in length. This array is populated with [`LLVMBasicBlockRef`](@ref) instances.
"""
function LLVMGetBasicBlocks(Fn, BasicBlocks)
    ccall((:LLVMGetBasicBlocks, libllvm), Cvoid, (LLVMValueRef, Ptr{LLVMBasicBlockRef}), Fn, BasicBlocks)
end

"""
    LLVMGetFirstBasicBlock(Fn)

Obtain the first basic block in a function.

The returned basic block can be used as an iterator. You will likely eventually call into [`LLVMGetNextBasicBlock`](@ref)() with it.

# See also
llvm::Function::begin()
"""
function LLVMGetFirstBasicBlock(Fn)
    ccall((:LLVMGetFirstBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), Fn)
end

"""
    LLVMGetLastBasicBlock(Fn)

Obtain the last basic block in a function.

# See also
llvm::Function::end()
"""
function LLVMGetLastBasicBlock(Fn)
    ccall((:LLVMGetLastBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), Fn)
end

"""
    LLVMGetNextBasicBlock(BB)

Advance a basic block iterator.
"""
function LLVMGetNextBasicBlock(BB)
    ccall((:LLVMGetNextBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMGetPreviousBasicBlock(BB)

Go backwards in a basic block iterator.
"""
function LLVMGetPreviousBasicBlock(BB)
    ccall((:LLVMGetPreviousBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMGetEntryBasicBlock(Fn)

Obtain the basic block that corresponds to the entry point of a function.

# See also
llvm::Function::getEntryBlock()
"""
function LLVMGetEntryBasicBlock(Fn)
    ccall((:LLVMGetEntryBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), Fn)
end

"""
Represents an LLVM basic block builder.

This models llvm::IRBuilder.
"""
const LLVMBuilderRef = Ptr{LLVMOpaqueBuilder}

"""
    LLVMInsertExistingBasicBlockAfterInsertBlock(Builder, BB)

Insert the given basic block after the insertion point of the given builder.

The insertion point must be valid.

# See also
llvm::Function::BasicBlockListType::insertAfter()
"""
function LLVMInsertExistingBasicBlockAfterInsertBlock(Builder, BB)
    ccall((:LLVMInsertExistingBasicBlockAfterInsertBlock, libllvm), Cvoid, (LLVMBuilderRef, LLVMBasicBlockRef), Builder, BB)
end

"""
    LLVMAppendExistingBasicBlock(Fn, BB)

Append the given basic block to the basic block list of the given function.

# See also
llvm::Function::BasicBlockListType::push\\_back()
"""
function LLVMAppendExistingBasicBlock(Fn, BB)
    ccall((:LLVMAppendExistingBasicBlock, libllvm), Cvoid, (LLVMValueRef, LLVMBasicBlockRef), Fn, BB)
end

"""
    LLVMCreateBasicBlockInContext(C, Name)

Create a new basic block without inserting it into a function.

# See also
llvm::BasicBlock::Create()
"""
function LLVMCreateBasicBlockInContext(C, Name)
    ccall((:LLVMCreateBasicBlockInContext, libllvm), LLVMBasicBlockRef, (LLVMContextRef, Cstring), C, Name)
end

"""
    LLVMAppendBasicBlockInContext(C, Fn, Name)

Append a basic block to the end of a function.

# See also
llvm::BasicBlock::Create()
"""
function LLVMAppendBasicBlockInContext(C, Fn, Name)
    ccall((:LLVMAppendBasicBlockInContext, libllvm), LLVMBasicBlockRef, (LLVMContextRef, LLVMValueRef, Cstring), C, Fn, Name)
end

"""
    LLVMAppendBasicBlock(Fn, Name)

Append a basic block to the end of a function using the global context.

# See also
llvm::BasicBlock::Create()
"""
function LLVMAppendBasicBlock(Fn, Name)
    ccall((:LLVMAppendBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef, Cstring), Fn, Name)
end

"""
    LLVMInsertBasicBlockInContext(C, BB, Name)

Insert a basic block in a function before another basic block.

The function to add to is determined by the function of the passed basic block.

# See also
llvm::BasicBlock::Create()
"""
function LLVMInsertBasicBlockInContext(C, BB, Name)
    ccall((:LLVMInsertBasicBlockInContext, libllvm), LLVMBasicBlockRef, (LLVMContextRef, LLVMBasicBlockRef, Cstring), C, BB, Name)
end

"""
    LLVMInsertBasicBlock(InsertBeforeBB, Name)

Insert a basic block in a function using the global context.

# See also
llvm::BasicBlock::Create()
"""
function LLVMInsertBasicBlock(InsertBeforeBB, Name)
    ccall((:LLVMInsertBasicBlock, libllvm), LLVMBasicBlockRef, (LLVMBasicBlockRef, Cstring), InsertBeforeBB, Name)
end

"""
    LLVMDeleteBasicBlock(BB)

Remove a basic block from a function and delete it.

This deletes the basic block from its containing function and deletes the basic block itself.

# See also
llvm::BasicBlock::eraseFromParent()
"""
function LLVMDeleteBasicBlock(BB)
    ccall((:LLVMDeleteBasicBlock, libllvm), Cvoid, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMRemoveBasicBlockFromParent(BB)

Remove a basic block from a function.

This deletes the basic block from its containing function but keep the basic block alive.

# See also
llvm::BasicBlock::removeFromParent()
"""
function LLVMRemoveBasicBlockFromParent(BB)
    ccall((:LLVMRemoveBasicBlockFromParent, libllvm), Cvoid, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMMoveBasicBlockBefore(BB, MovePos)

Move a basic block to before another one.

# See also
llvm::BasicBlock::moveBefore()
"""
function LLVMMoveBasicBlockBefore(BB, MovePos)
    ccall((:LLVMMoveBasicBlockBefore, libllvm), Cvoid, (LLVMBasicBlockRef, LLVMBasicBlockRef), BB, MovePos)
end

"""
    LLVMMoveBasicBlockAfter(BB, MovePos)

Move a basic block to after another one.

# See also
llvm::BasicBlock::moveAfter()
"""
function LLVMMoveBasicBlockAfter(BB, MovePos)
    ccall((:LLVMMoveBasicBlockAfter, libllvm), Cvoid, (LLVMBasicBlockRef, LLVMBasicBlockRef), BB, MovePos)
end

"""
    LLVMGetFirstInstruction(BB)

Obtain the first instruction in a basic block.

The returned [`LLVMValueRef`](@ref) corresponds to a llvm::Instruction instance.
"""
function LLVMGetFirstInstruction(BB)
    ccall((:LLVMGetFirstInstruction, libllvm), LLVMValueRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMGetLastInstruction(BB)

Obtain the last instruction in a basic block.

The returned [`LLVMValueRef`](@ref) corresponds to an LLVM:Instruction.
"""
function LLVMGetLastInstruction(BB)
    ccall((:LLVMGetLastInstruction, libllvm), LLVMValueRef, (LLVMBasicBlockRef,), BB)
end

"""
    LLVMHasMetadata(Val)

Determine whether an instruction has any metadata attached.
"""
function LLVMHasMetadata(Val)
    ccall((:LLVMHasMetadata, libllvm), Cint, (LLVMValueRef,), Val)
end

"""
    LLVMGetMetadata(Val, KindID)

Return metadata associated with an instruction value.
"""
function LLVMGetMetadata(Val, KindID)
    ccall((:LLVMGetMetadata, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), Val, KindID)
end

"""
    LLVMSetMetadata(Val, KindID, Node)

Set metadata associated with an instruction value.
"""
function LLVMSetMetadata(Val, KindID, Node)
    ccall((:LLVMSetMetadata, libllvm), Cvoid, (LLVMValueRef, Cuint, LLVMValueRef), Val, KindID, Node)
end

"""
    LLVMInstructionGetAllMetadataOtherThanDebugLoc(Instr, NumEntries)

Returns the metadata associated with an instruction value, but filters out all the debug locations.

# See also
llvm::Instruction::getAllMetadataOtherThanDebugLoc()
"""
function LLVMInstructionGetAllMetadataOtherThanDebugLoc(Instr, NumEntries)
    ccall((:LLVMInstructionGetAllMetadataOtherThanDebugLoc, libllvm), Ptr{LLVMValueMetadataEntry}, (LLVMValueRef, Ptr{Csize_t}), Instr, NumEntries)
end

"""
    LLVMGetInstructionParent(Inst)

Obtain the basic block to which an instruction belongs.

# See also
llvm::Instruction::getParent()
"""
function LLVMGetInstructionParent(Inst)
    ccall((:LLVMGetInstructionParent, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), Inst)
end

"""
    LLVMGetNextInstruction(Inst)

Obtain the instruction that occurs after the one specified.

The next instruction will be from the same basic block.

If this is the last instruction in a basic block, NULL will be returned.
"""
function LLVMGetNextInstruction(Inst)
    ccall((:LLVMGetNextInstruction, libllvm), LLVMValueRef, (LLVMValueRef,), Inst)
end

"""
    LLVMGetPreviousInstruction(Inst)

Obtain the instruction that occurred before this one.

If the instruction is the first instruction in a basic block, NULL will be returned.
"""
function LLVMGetPreviousInstruction(Inst)
    ccall((:LLVMGetPreviousInstruction, libllvm), LLVMValueRef, (LLVMValueRef,), Inst)
end

"""
    LLVMInstructionRemoveFromParent(Inst)

Remove an instruction.

The instruction specified is removed from its containing building block but is kept alive.

# See also
llvm::Instruction::removeFromParent()
"""
function LLVMInstructionRemoveFromParent(Inst)
    ccall((:LLVMInstructionRemoveFromParent, libllvm), Cvoid, (LLVMValueRef,), Inst)
end

"""
    LLVMInstructionEraseFromParent(Inst)

Remove and delete an instruction.

The instruction specified is removed from its containing building block and then deleted.

# See also
llvm::Instruction::eraseFromParent()
"""
function LLVMInstructionEraseFromParent(Inst)
    ccall((:LLVMInstructionEraseFromParent, libllvm), Cvoid, (LLVMValueRef,), Inst)
end

"""
    LLVMDeleteInstruction(Inst)

Delete an instruction.

The instruction specified is deleted. It must have previously been removed from its containing building block.

# See also
llvm::Value::deleteValue()
"""
function LLVMDeleteInstruction(Inst)
    ccall((:LLVMDeleteInstruction, libllvm), Cvoid, (LLVMValueRef,), Inst)
end

"""
    LLVMGetInstructionOpcode(Inst)

Obtain the code opcode for an individual instruction.

# See also
llvm::Instruction::getOpCode()
"""
function LLVMGetInstructionOpcode(Inst)
    ccall((:LLVMGetInstructionOpcode, libllvm), LLVMOpcode, (LLVMValueRef,), Inst)
end

"""
    LLVMGetICmpPredicate(Inst)

Obtain the predicate of an instruction.

This is only valid for instructions that correspond to llvm::ICmpInst or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.

# See also
llvm::ICmpInst::getPredicate()
"""
function LLVMGetICmpPredicate(Inst)
    ccall((:LLVMGetICmpPredicate, libllvm), LLVMIntPredicate, (LLVMValueRef,), Inst)
end

"""
    LLVMGetFCmpPredicate(Inst)

Obtain the float predicate of an instruction.

This is only valid for instructions that correspond to llvm::FCmpInst or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.

# See also
llvm::FCmpInst::getPredicate()
"""
function LLVMGetFCmpPredicate(Inst)
    ccall((:LLVMGetFCmpPredicate, libllvm), LLVMRealPredicate, (LLVMValueRef,), Inst)
end

"""
    LLVMInstructionClone(Inst)

Create a copy of 'this' instruction that is identical in all ways except the following: * The instruction has no parent * The instruction has no name

# See also
llvm::Instruction::clone()
"""
function LLVMInstructionClone(Inst)
    ccall((:LLVMInstructionClone, libllvm), LLVMValueRef, (LLVMValueRef,), Inst)
end

"""
    LLVMIsATerminatorInst(Inst)

Determine whether an instruction is a terminator. This routine is named to be compatible with historical functions that did this by querying the underlying C++ type.

# See also
llvm::Instruction::isTerminator()
"""
function LLVMIsATerminatorInst(Inst)
    ccall((:LLVMIsATerminatorInst, libllvm), LLVMValueRef, (LLVMValueRef,), Inst)
end

"""
    LLVMGetNumArgOperands(Instr)

Obtain the argument count for a call instruction.

This expects an [`LLVMValueRef`](@ref) that corresponds to a llvm::CallInst, llvm::InvokeInst, or llvm:FuncletPadInst.

# See also
llvm::CallInst::getNumArgOperands(), llvm::InvokeInst::getNumArgOperands(), llvm::FuncletPadInst::getNumArgOperands()
"""
function LLVMGetNumArgOperands(Instr)
    ccall((:LLVMGetNumArgOperands, libllvm), Cuint, (LLVMValueRef,), Instr)
end

"""
    LLVMSetInstructionCallConv(Instr, CC)

Set the calling convention for a call instruction.

This expects an [`LLVMValueRef`](@ref) that corresponds to a llvm::CallInst or llvm::InvokeInst.

# See also
llvm::CallInst::setCallingConv(), llvm::InvokeInst::setCallingConv()
"""
function LLVMSetInstructionCallConv(Instr, CC)
    ccall((:LLVMSetInstructionCallConv, libllvm), Cvoid, (LLVMValueRef, Cuint), Instr, CC)
end

"""
    LLVMGetInstructionCallConv(Instr)

Obtain the calling convention for a call instruction.

This is the opposite of [`LLVMSetInstructionCallConv`](@ref)(). Reads its usage.

# See also
[`LLVMSetInstructionCallConv`](@ref)()
"""
function LLVMGetInstructionCallConv(Instr)
    ccall((:LLVMGetInstructionCallConv, libllvm), Cuint, (LLVMValueRef,), Instr)
end

function LLVMSetInstrParamAlignment(Instr, Idx, Align)
    ccall((:LLVMSetInstrParamAlignment, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Cuint), Instr, Idx, Align)
end

function LLVMAddCallSiteAttribute(C, Idx, A)
    ccall((:LLVMAddCallSiteAttribute, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, LLVMAttributeRef), C, Idx, A)
end

function LLVMGetCallSiteAttributeCount(C, Idx)
    ccall((:LLVMGetCallSiteAttributeCount, libllvm), Cuint, (LLVMValueRef, LLVMAttributeIndex), C, Idx)
end

function LLVMGetCallSiteAttributes(C, Idx, Attrs)
    ccall((:LLVMGetCallSiteAttributes, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Ptr{LLVMAttributeRef}), C, Idx, Attrs)
end

function LLVMGetCallSiteEnumAttribute(C, Idx, KindID)
    ccall((:LLVMGetCallSiteEnumAttribute, libllvm), LLVMAttributeRef, (LLVMValueRef, LLVMAttributeIndex, Cuint), C, Idx, KindID)
end

function LLVMGetCallSiteStringAttribute(C, Idx, K, KLen)
    ccall((:LLVMGetCallSiteStringAttribute, libllvm), LLVMAttributeRef, (LLVMValueRef, LLVMAttributeIndex, Cstring, Cuint), C, Idx, K, KLen)
end

function LLVMRemoveCallSiteEnumAttribute(C, Idx, KindID)
    ccall((:LLVMRemoveCallSiteEnumAttribute, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Cuint), C, Idx, KindID)
end

function LLVMRemoveCallSiteStringAttribute(C, Idx, K, KLen)
    ccall((:LLVMRemoveCallSiteStringAttribute, libllvm), Cvoid, (LLVMValueRef, LLVMAttributeIndex, Cstring, Cuint), C, Idx, K, KLen)
end

"""
    LLVMGetCalledFunctionType(C)

Obtain the function type called by this instruction.

# See also
llvm::CallBase::getFunctionType()
"""
function LLVMGetCalledFunctionType(C)
    ccall((:LLVMGetCalledFunctionType, libllvm), LLVMTypeRef, (LLVMValueRef,), C)
end

"""
    LLVMGetCalledValue(Instr)

Obtain the pointer to the function invoked by this instruction.

This expects an [`LLVMValueRef`](@ref) that corresponds to a llvm::CallInst or llvm::InvokeInst.

# See also
llvm::CallInst::getCalledOperand(), llvm::InvokeInst::getCalledOperand()
"""
function LLVMGetCalledValue(Instr)
    ccall((:LLVMGetCalledValue, libllvm), LLVMValueRef, (LLVMValueRef,), Instr)
end

"""
    LLVMIsTailCall(CallInst)

Obtain whether a call instruction is a tail call.

This only works on llvm::CallInst instructions.

# See also
llvm::CallInst::isTailCall()
"""
function LLVMIsTailCall(CallInst)
    ccall((:LLVMIsTailCall, libllvm), LLVMBool, (LLVMValueRef,), CallInst)
end

"""
    LLVMSetTailCall(CallInst, IsTailCall)

Set whether a call instruction is a tail call.

This only works on llvm::CallInst instructions.

# See also
llvm::CallInst::setTailCall()
"""
function LLVMSetTailCall(CallInst, IsTailCall)
    ccall((:LLVMSetTailCall, libllvm), Cvoid, (LLVMValueRef, LLVMBool), CallInst, IsTailCall)
end

"""
    LLVMGetNormalDest(InvokeInst)

Return the normal destination basic block.

This only works on llvm::InvokeInst instructions.

# See also
llvm::InvokeInst::getNormalDest()
"""
function LLVMGetNormalDest(InvokeInst)
    ccall((:LLVMGetNormalDest, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), InvokeInst)
end

"""
    LLVMGetUnwindDest(InvokeInst)

Return the unwind destination basic block.

Works on llvm::InvokeInst, llvm::CleanupReturnInst, and llvm::CatchSwitchInst instructions.

# See also
llvm::InvokeInst::getUnwindDest(), llvm::CleanupReturnInst::getUnwindDest(), llvm::CatchSwitchInst::getUnwindDest()
"""
function LLVMGetUnwindDest(InvokeInst)
    ccall((:LLVMGetUnwindDest, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), InvokeInst)
end

"""
    LLVMSetNormalDest(InvokeInst, B)

Set the normal destination basic block.

This only works on llvm::InvokeInst instructions.

# See also
llvm::InvokeInst::setNormalDest()
"""
function LLVMSetNormalDest(InvokeInst, B)
    ccall((:LLVMSetNormalDest, libllvm), Cvoid, (LLVMValueRef, LLVMBasicBlockRef), InvokeInst, B)
end

"""
    LLVMSetUnwindDest(InvokeInst, B)

Set the unwind destination basic block.

Works on llvm::InvokeInst, llvm::CleanupReturnInst, and llvm::CatchSwitchInst instructions.

# See also
llvm::InvokeInst::setUnwindDest(), llvm::CleanupReturnInst::setUnwindDest(), llvm::CatchSwitchInst::setUnwindDest()
"""
function LLVMSetUnwindDest(InvokeInst, B)
    ccall((:LLVMSetUnwindDest, libllvm), Cvoid, (LLVMValueRef, LLVMBasicBlockRef), InvokeInst, B)
end

"""
    LLVMGetNumSuccessors(Term)

Return the number of successors that this terminator has.

# See also
llvm::Instruction::getNumSuccessors
"""
function LLVMGetNumSuccessors(Term)
    ccall((:LLVMGetNumSuccessors, libllvm), Cuint, (LLVMValueRef,), Term)
end

"""
    LLVMGetSuccessor(Term, i)

Return the specified successor.

# See also
llvm::Instruction::getSuccessor
"""
function LLVMGetSuccessor(Term, i)
    ccall((:LLVMGetSuccessor, libllvm), LLVMBasicBlockRef, (LLVMValueRef, Cuint), Term, i)
end

"""
    LLVMSetSuccessor(Term, i, block)

Update the specified successor to point at the provided block.

# See also
llvm::Instruction::setSuccessor
"""
function LLVMSetSuccessor(Term, i, block)
    ccall((:LLVMSetSuccessor, libllvm), Cvoid, (LLVMValueRef, Cuint, LLVMBasicBlockRef), Term, i, block)
end

"""
    LLVMIsConditional(Branch)

Return if a branch is conditional.

This only works on llvm::BranchInst instructions.

# See also
llvm::BranchInst::isConditional
"""
function LLVMIsConditional(Branch)
    ccall((:LLVMIsConditional, libllvm), LLVMBool, (LLVMValueRef,), Branch)
end

"""
    LLVMGetCondition(Branch)

Return the condition of a branch instruction.

This only works on llvm::BranchInst instructions.

# See also
llvm::BranchInst::getCondition
"""
function LLVMGetCondition(Branch)
    ccall((:LLVMGetCondition, libllvm), LLVMValueRef, (LLVMValueRef,), Branch)
end

"""
    LLVMSetCondition(Branch, Cond)

Set the condition of a branch instruction.

This only works on llvm::BranchInst instructions.

# See also
llvm::BranchInst::setCondition
"""
function LLVMSetCondition(Branch, Cond)
    ccall((:LLVMSetCondition, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), Branch, Cond)
end

"""
    LLVMGetSwitchDefaultDest(SwitchInstr)

Obtain the default destination basic block of a switch instruction.

This only works on llvm::SwitchInst instructions.

# See also
llvm::SwitchInst::getDefaultDest()
"""
function LLVMGetSwitchDefaultDest(SwitchInstr)
    ccall((:LLVMGetSwitchDefaultDest, libllvm), LLVMBasicBlockRef, (LLVMValueRef,), SwitchInstr)
end

"""
    LLVMGetAllocatedType(Alloca)

Obtain the type that is being allocated by the alloca instruction.
"""
function LLVMGetAllocatedType(Alloca)
    ccall((:LLVMGetAllocatedType, libllvm), LLVMTypeRef, (LLVMValueRef,), Alloca)
end

"""
    LLVMIsInBounds(GEP)

Check whether the given GEP operator is inbounds.
"""
function LLVMIsInBounds(GEP)
    ccall((:LLVMIsInBounds, libllvm), LLVMBool, (LLVMValueRef,), GEP)
end

"""
    LLVMSetIsInBounds(GEP, InBounds)

Set the given GEP instruction to be inbounds or not.
"""
function LLVMSetIsInBounds(GEP, InBounds)
    ccall((:LLVMSetIsInBounds, libllvm), Cvoid, (LLVMValueRef, LLVMBool), GEP, InBounds)
end

"""
    LLVMGetGEPSourceElementType(GEP)

Get the source element type of the given GEP operator.
"""
function LLVMGetGEPSourceElementType(GEP)
    ccall((:LLVMGetGEPSourceElementType, libllvm), LLVMTypeRef, (LLVMValueRef,), GEP)
end

"""
    LLVMAddIncoming(PhiNode, IncomingValues, IncomingBlocks, Count)

Add an incoming value to the end of a PHI list.
"""
function LLVMAddIncoming(PhiNode, IncomingValues, IncomingBlocks, Count)
    ccall((:LLVMAddIncoming, libllvm), Cvoid, (LLVMValueRef, Ptr{LLVMValueRef}, Ptr{LLVMBasicBlockRef}, Cuint), PhiNode, IncomingValues, IncomingBlocks, Count)
end

"""
    LLVMCountIncoming(PhiNode)

Obtain the number of incoming basic blocks to a PHI node.
"""
function LLVMCountIncoming(PhiNode)
    ccall((:LLVMCountIncoming, libllvm), Cuint, (LLVMValueRef,), PhiNode)
end

"""
    LLVMGetIncomingValue(PhiNode, Index)

Obtain an incoming value to a PHI node as an [`LLVMValueRef`](@ref).
"""
function LLVMGetIncomingValue(PhiNode, Index)
    ccall((:LLVMGetIncomingValue, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), PhiNode, Index)
end

"""
    LLVMGetIncomingBlock(PhiNode, Index)

Obtain an incoming value to a PHI node as an [`LLVMBasicBlockRef`](@ref).
"""
function LLVMGetIncomingBlock(PhiNode, Index)
    ccall((:LLVMGetIncomingBlock, libllvm), LLVMBasicBlockRef, (LLVMValueRef, Cuint), PhiNode, Index)
end

"""
    LLVMGetNumIndices(Inst)

Obtain the number of indices. NB: This also works on GEP operators.
"""
function LLVMGetNumIndices(Inst)
    ccall((:LLVMGetNumIndices, libllvm), Cuint, (LLVMValueRef,), Inst)
end

"""
    LLVMGetIndices(Inst)

Obtain the indices as an array.
"""
function LLVMGetIndices(Inst)
    ccall((:LLVMGetIndices, libllvm), Ptr{Cuint}, (LLVMValueRef,), Inst)
end

"""
    LLVMCreateBuilderInContext(C)

` LLVMCCoreInstructionBuilder Instruction Builders`

An instruction builder represents a point within a basic block and is the exclusive means of building instructions using the C interface.

@{
"""
function LLVMCreateBuilderInContext(C)
    ccall((:LLVMCreateBuilderInContext, libllvm), LLVMBuilderRef, (LLVMContextRef,), C)
end

function LLVMCreateBuilder()
    ccall((:LLVMCreateBuilder, libllvm), LLVMBuilderRef, ())
end

function LLVMPositionBuilder(Builder, Block, Instr)
    ccall((:LLVMPositionBuilder, libllvm), Cvoid, (LLVMBuilderRef, LLVMBasicBlockRef, LLVMValueRef), Builder, Block, Instr)
end

function LLVMPositionBuilderBefore(Builder, Instr)
    ccall((:LLVMPositionBuilderBefore, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef), Builder, Instr)
end

function LLVMPositionBuilderAtEnd(Builder, Block)
    ccall((:LLVMPositionBuilderAtEnd, libllvm), Cvoid, (LLVMBuilderRef, LLVMBasicBlockRef), Builder, Block)
end

function LLVMGetInsertBlock(Builder)
    ccall((:LLVMGetInsertBlock, libllvm), LLVMBasicBlockRef, (LLVMBuilderRef,), Builder)
end

function LLVMClearInsertionPosition(Builder)
    ccall((:LLVMClearInsertionPosition, libllvm), Cvoid, (LLVMBuilderRef,), Builder)
end

function LLVMInsertIntoBuilder(Builder, Instr)
    ccall((:LLVMInsertIntoBuilder, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef), Builder, Instr)
end

function LLVMInsertIntoBuilderWithName(Builder, Instr, Name)
    ccall((:LLVMInsertIntoBuilderWithName, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef, Cstring), Builder, Instr, Name)
end

function LLVMDisposeBuilder(Builder)
    ccall((:LLVMDisposeBuilder, libllvm), Cvoid, (LLVMBuilderRef,), Builder)
end

"""
    LLVMGetCurrentDebugLocation2(Builder)

Get location information used by debugging information.

# See also
llvm::IRBuilder::getCurrentDebugLocation()
"""
function LLVMGetCurrentDebugLocation2(Builder)
    ccall((:LLVMGetCurrentDebugLocation2, libllvm), LLVMMetadataRef, (LLVMBuilderRef,), Builder)
end

"""
    LLVMSetCurrentDebugLocation2(Builder, Loc)

Set location information used by debugging information.

To clear the location metadata of the given instruction, pass NULL to `Loc`.

# See also
llvm::IRBuilder::SetCurrentDebugLocation()
"""
function LLVMSetCurrentDebugLocation2(Builder, Loc)
    ccall((:LLVMSetCurrentDebugLocation2, libllvm), Cvoid, (LLVMBuilderRef, LLVMMetadataRef), Builder, Loc)
end

"""
    LLVMSetInstDebugLocation(Builder, Inst)

Attempts to set the debug location for the given instruction using the current debug location for the given builder. If the builder has no current debug location, this function is a no-op.

!!! compat "Deprecated"

    [`LLVMSetInstDebugLocation`](@ref) is deprecated in favor of the more general [`LLVMAddMetadataToInst`](@ref).

# See also
llvm::IRBuilder::SetInstDebugLocation()
"""
function LLVMSetInstDebugLocation(Builder, Inst)
    ccall((:LLVMSetInstDebugLocation, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef), Builder, Inst)
end

"""
    LLVMAddMetadataToInst(Builder, Inst)

Adds the metadata registered with the given builder to the given instruction.

# See also
llvm::IRBuilder::AddMetadataToInst()
"""
function LLVMAddMetadataToInst(Builder, Inst)
    ccall((:LLVMAddMetadataToInst, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef), Builder, Inst)
end

"""
    LLVMBuilderGetDefaultFPMathTag(Builder)

Get the dafult floating-point math metadata for a given builder.

# See also
llvm::IRBuilder::getDefaultFPMathTag()
"""
function LLVMBuilderGetDefaultFPMathTag(Builder)
    ccall((:LLVMBuilderGetDefaultFPMathTag, libllvm), LLVMMetadataRef, (LLVMBuilderRef,), Builder)
end

"""
    LLVMBuilderSetDefaultFPMathTag(Builder, FPMathTag)

Set the default floating-point math metadata for the given builder.

To clear the metadata, pass NULL to `FPMathTag`.

# See also
llvm::IRBuilder::setDefaultFPMathTag()
"""
function LLVMBuilderSetDefaultFPMathTag(Builder, FPMathTag)
    ccall((:LLVMBuilderSetDefaultFPMathTag, libllvm), Cvoid, (LLVMBuilderRef, LLVMMetadataRef), Builder, FPMathTag)
end

"""
    LLVMSetCurrentDebugLocation(Builder, L)

Deprecated: Passing the NULL location will crash. Use [`LLVMGetCurrentDebugLocation2`](@ref) instead.
"""
function LLVMSetCurrentDebugLocation(Builder, L)
    ccall((:LLVMSetCurrentDebugLocation, libllvm), Cvoid, (LLVMBuilderRef, LLVMValueRef), Builder, L)
end

"""
    LLVMGetCurrentDebugLocation(Builder)

Deprecated: Returning the NULL location will crash. Use [`LLVMGetCurrentDebugLocation2`](@ref) instead.
"""
function LLVMGetCurrentDebugLocation(Builder)
    ccall((:LLVMGetCurrentDebugLocation, libllvm), LLVMValueRef, (LLVMBuilderRef,), Builder)
end

function LLVMBuildRetVoid(arg1)
    ccall((:LLVMBuildRetVoid, libllvm), LLVMValueRef, (LLVMBuilderRef,), arg1)
end

function LLVMBuildRet(arg1, V)
    ccall((:LLVMBuildRet, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef), arg1, V)
end

function LLVMBuildAggregateRet(arg1, RetVals, N)
    ccall((:LLVMBuildAggregateRet, libllvm), LLVMValueRef, (LLVMBuilderRef, Ptr{LLVMValueRef}, Cuint), arg1, RetVals, N)
end

function LLVMBuildBr(arg1, Dest)
    ccall((:LLVMBuildBr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMBasicBlockRef), arg1, Dest)
end

function LLVMBuildCondBr(arg1, If, Then, Else)
    ccall((:LLVMBuildCondBr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMBasicBlockRef, LLVMBasicBlockRef), arg1, If, Then, Else)
end

function LLVMBuildSwitch(arg1, V, Else, NumCases)
    ccall((:LLVMBuildSwitch, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMBasicBlockRef, Cuint), arg1, V, Else, NumCases)
end

function LLVMBuildIndirectBr(B, Addr, NumDests)
    ccall((:LLVMBuildIndirectBr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cuint), B, Addr, NumDests)
end

function LLVMBuildInvoke(arg1, Fn, Args, NumArgs, Then, Catch, Name)
    ccall((:LLVMBuildInvoke, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, LLVMBasicBlockRef, LLVMBasicBlockRef, Cstring), arg1, Fn, Args, NumArgs, Then, Catch, Name)
end

function LLVMBuildInvoke2(arg1, Ty, Fn, Args, NumArgs, Then, Catch, Name)
    ccall((:LLVMBuildInvoke2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, LLVMBasicBlockRef, LLVMBasicBlockRef, Cstring), arg1, Ty, Fn, Args, NumArgs, Then, Catch, Name)
end

function LLVMBuildUnreachable(arg1)
    ccall((:LLVMBuildUnreachable, libllvm), LLVMValueRef, (LLVMBuilderRef,), arg1)
end

function LLVMBuildResume(B, Exn)
    ccall((:LLVMBuildResume, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef), B, Exn)
end

function LLVMBuildLandingPad(B, Ty, PersFn, NumClauses, Name)
    ccall((:LLVMBuildLandingPad, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cuint, Cstring), B, Ty, PersFn, NumClauses, Name)
end

function LLVMBuildCleanupRet(B, CatchPad, BB)
    ccall((:LLVMBuildCleanupRet, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMBasicBlockRef), B, CatchPad, BB)
end

function LLVMBuildCatchRet(B, CatchPad, BB)
    ccall((:LLVMBuildCatchRet, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMBasicBlockRef), B, CatchPad, BB)
end

function LLVMBuildCatchPad(B, ParentPad, Args, NumArgs, Name)
    ccall((:LLVMBuildCatchPad, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, ParentPad, Args, NumArgs, Name)
end

function LLVMBuildCleanupPad(B, ParentPad, Args, NumArgs, Name)
    ccall((:LLVMBuildCleanupPad, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, ParentPad, Args, NumArgs, Name)
end

function LLVMBuildCatchSwitch(B, ParentPad, UnwindBB, NumHandlers, Name)
    ccall((:LLVMBuildCatchSwitch, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMBasicBlockRef, Cuint, Cstring), B, ParentPad, UnwindBB, NumHandlers, Name)
end

function LLVMAddCase(Switch, OnVal, Dest)
    ccall((:LLVMAddCase, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef, LLVMBasicBlockRef), Switch, OnVal, Dest)
end

function LLVMAddDestination(IndirectBr, Dest)
    ccall((:LLVMAddDestination, libllvm), Cvoid, (LLVMValueRef, LLVMBasicBlockRef), IndirectBr, Dest)
end

function LLVMGetNumClauses(LandingPad)
    ccall((:LLVMGetNumClauses, libllvm), Cuint, (LLVMValueRef,), LandingPad)
end

function LLVMGetClause(LandingPad, Idx)
    ccall((:LLVMGetClause, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), LandingPad, Idx)
end

function LLVMAddClause(LandingPad, ClauseVal)
    ccall((:LLVMAddClause, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), LandingPad, ClauseVal)
end

function LLVMIsCleanup(LandingPad)
    ccall((:LLVMIsCleanup, libllvm), LLVMBool, (LLVMValueRef,), LandingPad)
end

function LLVMSetCleanup(LandingPad, Val)
    ccall((:LLVMSetCleanup, libllvm), Cvoid, (LLVMValueRef, LLVMBool), LandingPad, Val)
end

function LLVMAddHandler(CatchSwitch, Dest)
    ccall((:LLVMAddHandler, libllvm), Cvoid, (LLVMValueRef, LLVMBasicBlockRef), CatchSwitch, Dest)
end

function LLVMGetNumHandlers(CatchSwitch)
    ccall((:LLVMGetNumHandlers, libllvm), Cuint, (LLVMValueRef,), CatchSwitch)
end

"""
    LLVMGetHandlers(CatchSwitch, Handlers)

Obtain the basic blocks acting as handlers for a catchswitch instruction.

The Handlers parameter should point to a pre-allocated array of LLVMBasicBlockRefs at least [`LLVMGetNumHandlers`](@ref)() large. On return, the first [`LLVMGetNumHandlers`](@ref)() entries in the array will be populated with [`LLVMBasicBlockRef`](@ref) instances.

# Arguments
* `CatchSwitch`: The catchswitch instruction to operate on.
* `Handlers`: Memory address of an array to be filled with basic blocks.
"""
function LLVMGetHandlers(CatchSwitch, Handlers)
    ccall((:LLVMGetHandlers, libllvm), Cvoid, (LLVMValueRef, Ptr{LLVMBasicBlockRef}), CatchSwitch, Handlers)
end

function LLVMGetArgOperand(Funclet, i)
    ccall((:LLVMGetArgOperand, libllvm), LLVMValueRef, (LLVMValueRef, Cuint), Funclet, i)
end

function LLVMSetArgOperand(Funclet, i, value)
    ccall((:LLVMSetArgOperand, libllvm), Cvoid, (LLVMValueRef, Cuint, LLVMValueRef), Funclet, i, value)
end

"""
    LLVMGetParentCatchSwitch(CatchPad)

Get the parent catchswitch instruction of a catchpad instruction.

This only works on llvm::CatchPadInst instructions.

# See also
llvm::CatchPadInst::getCatchSwitch()
"""
function LLVMGetParentCatchSwitch(CatchPad)
    ccall((:LLVMGetParentCatchSwitch, libllvm), LLVMValueRef, (LLVMValueRef,), CatchPad)
end

"""
    LLVMSetParentCatchSwitch(CatchPad, CatchSwitch)

Set the parent catchswitch instruction of a catchpad instruction.

This only works on llvm::CatchPadInst instructions.

# See also
llvm::CatchPadInst::setCatchSwitch()
"""
function LLVMSetParentCatchSwitch(CatchPad, CatchSwitch)
    ccall((:LLVMSetParentCatchSwitch, libllvm), Cvoid, (LLVMValueRef, LLVMValueRef), CatchPad, CatchSwitch)
end

function LLVMBuildAdd(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildAdd, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNSWAdd(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNSWAdd, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNUWAdd(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNUWAdd, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildFAdd(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildFAdd, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildSub(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildSub, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNSWSub(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNSWSub, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNUWSub(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNUWSub, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildFSub(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildFSub, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildMul(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildMul, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNSWMul(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNSWMul, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildNUWMul(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildNUWMul, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildFMul(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildFMul, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildUDiv(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildUDiv, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildExactUDiv(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildExactUDiv, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildSDiv(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildSDiv, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildExactSDiv(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildExactSDiv, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildFDiv(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildFDiv, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildURem(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildURem, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildSRem(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildSRem, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildFRem(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildFRem, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildShl(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildShl, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildLShr(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildLShr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildAShr(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildAShr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildAnd(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildAnd, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildOr(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildOr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildXor(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildXor, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildBinOp(B, Op, LHS, RHS, Name)
    ccall((:LLVMBuildBinOp, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMOpcode, LLVMValueRef, LLVMValueRef, Cstring), B, Op, LHS, RHS, Name)
end

function LLVMBuildNeg(arg1, V, Name)
    ccall((:LLVMBuildNeg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, V, Name)
end

function LLVMBuildNSWNeg(B, V, Name)
    ccall((:LLVMBuildNSWNeg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), B, V, Name)
end

function LLVMBuildNUWNeg(B, V, Name)
    ccall((:LLVMBuildNUWNeg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), B, V, Name)
end

function LLVMBuildFNeg(arg1, V, Name)
    ccall((:LLVMBuildFNeg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, V, Name)
end

function LLVMBuildNot(arg1, V, Name)
    ccall((:LLVMBuildNot, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, V, Name)
end

function LLVMBuildMalloc(arg1, Ty, Name)
    ccall((:LLVMBuildMalloc, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, Cstring), arg1, Ty, Name)
end

function LLVMBuildArrayMalloc(arg1, Ty, Val, Name)
    ccall((:LLVMBuildArrayMalloc, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cstring), arg1, Ty, Val, Name)
end

"""
    LLVMBuildMemSet(B, Ptr, Val, Len, Align)

Creates and inserts a memset to the specified pointer and the specified value.

# See also
llvm::IRRBuilder::CreateMemSet()
"""
function LLVMBuildMemSet(B, Ptr, Val, Len, Align)
    ccall((:LLVMBuildMemSet, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, Cuint), B, Ptr, Val, Len, Align)
end

"""
    LLVMBuildMemCpy(B, Dst, DstAlign, Src, SrcAlign, Size)

Creates and inserts a memcpy between the specified pointers.

# See also
llvm::IRRBuilder::CreateMemCpy()
"""
function LLVMBuildMemCpy(B, Dst, DstAlign, Src, SrcAlign, Size)
    ccall((:LLVMBuildMemCpy, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cuint, LLVMValueRef, Cuint, LLVMValueRef), B, Dst, DstAlign, Src, SrcAlign, Size)
end

"""
    LLVMBuildMemMove(B, Dst, DstAlign, Src, SrcAlign, Size)

Creates and inserts a memmove between the specified pointers.

# See also
llvm::IRRBuilder::CreateMemMove()
"""
function LLVMBuildMemMove(B, Dst, DstAlign, Src, SrcAlign, Size)
    ccall((:LLVMBuildMemMove, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cuint, LLVMValueRef, Cuint, LLVMValueRef), B, Dst, DstAlign, Src, SrcAlign, Size)
end

function LLVMBuildAlloca(arg1, Ty, Name)
    ccall((:LLVMBuildAlloca, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, Cstring), arg1, Ty, Name)
end

function LLVMBuildArrayAlloca(arg1, Ty, Val, Name)
    ccall((:LLVMBuildArrayAlloca, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cstring), arg1, Ty, Val, Name)
end

function LLVMBuildFree(arg1, PointerVal)
    ccall((:LLVMBuildFree, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef), arg1, PointerVal)
end

function LLVMBuildLoad(arg1, PointerVal, Name)
    ccall((:LLVMBuildLoad, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, PointerVal, Name)
end

function LLVMBuildLoad2(arg1, Ty, PointerVal, Name)
    ccall((:LLVMBuildLoad2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cstring), arg1, Ty, PointerVal, Name)
end

function LLVMBuildStore(arg1, Val, Ptr)
    ccall((:LLVMBuildStore, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef), arg1, Val, Ptr)
end

function LLVMBuildGEP(B, Pointer, Indices, NumIndices, Name)
    ccall((:LLVMBuildGEP, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, Pointer, Indices, NumIndices, Name)
end

function LLVMBuildInBoundsGEP(B, Pointer, Indices, NumIndices, Name)
    ccall((:LLVMBuildInBoundsGEP, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, Pointer, Indices, NumIndices, Name)
end

function LLVMBuildStructGEP(B, Pointer, Idx, Name)
    ccall((:LLVMBuildStructGEP, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cuint, Cstring), B, Pointer, Idx, Name)
end

function LLVMBuildGEP2(B, Ty, Pointer, Indices, NumIndices, Name)
    ccall((:LLVMBuildGEP2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, Ty, Pointer, Indices, NumIndices, Name)
end

function LLVMBuildInBoundsGEP2(B, Ty, Pointer, Indices, NumIndices, Name)
    ccall((:LLVMBuildInBoundsGEP2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), B, Ty, Pointer, Indices, NumIndices, Name)
end

function LLVMBuildStructGEP2(B, Ty, Pointer, Idx, Name)
    ccall((:LLVMBuildStructGEP2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Cuint, Cstring), B, Ty, Pointer, Idx, Name)
end

function LLVMBuildGlobalString(B, Str, Name)
    ccall((:LLVMBuildGlobalString, libllvm), LLVMValueRef, (LLVMBuilderRef, Cstring, Cstring), B, Str, Name)
end

function LLVMBuildGlobalStringPtr(B, Str, Name)
    ccall((:LLVMBuildGlobalStringPtr, libllvm), LLVMValueRef, (LLVMBuilderRef, Cstring, Cstring), B, Str, Name)
end

function LLVMGetVolatile(MemoryAccessInst)
    ccall((:LLVMGetVolatile, libllvm), LLVMBool, (LLVMValueRef,), MemoryAccessInst)
end

function LLVMSetVolatile(MemoryAccessInst, IsVolatile)
    ccall((:LLVMSetVolatile, libllvm), Cvoid, (LLVMValueRef, LLVMBool), MemoryAccessInst, IsVolatile)
end

function LLVMGetWeak(CmpXchgInst)
    ccall((:LLVMGetWeak, libllvm), LLVMBool, (LLVMValueRef,), CmpXchgInst)
end

function LLVMSetWeak(CmpXchgInst, IsWeak)
    ccall((:LLVMSetWeak, libllvm), Cvoid, (LLVMValueRef, LLVMBool), CmpXchgInst, IsWeak)
end

function LLVMGetOrdering(MemoryAccessInst)
    ccall((:LLVMGetOrdering, libllvm), LLVMAtomicOrdering, (LLVMValueRef,), MemoryAccessInst)
end

function LLVMSetOrdering(MemoryAccessInst, Ordering)
    ccall((:LLVMSetOrdering, libllvm), Cvoid, (LLVMValueRef, LLVMAtomicOrdering), MemoryAccessInst, Ordering)
end

function LLVMGetAtomicRMWBinOp(AtomicRMWInst)
    ccall((:LLVMGetAtomicRMWBinOp, libllvm), LLVMAtomicRMWBinOp, (LLVMValueRef,), AtomicRMWInst)
end

function LLVMSetAtomicRMWBinOp(AtomicRMWInst, BinOp)
    ccall((:LLVMSetAtomicRMWBinOp, libllvm), Cvoid, (LLVMValueRef, LLVMAtomicRMWBinOp), AtomicRMWInst, BinOp)
end

function LLVMBuildTrunc(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildTrunc, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildZExt(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildZExt, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildSExt(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildSExt, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildFPToUI(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildFPToUI, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildFPToSI(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildFPToSI, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildUIToFP(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildUIToFP, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildSIToFP(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildSIToFP, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildFPTrunc(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildFPTrunc, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildFPExt(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildFPExt, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildPtrToInt(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildPtrToInt, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildIntToPtr(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildIntToPtr, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildBitCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildBitCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildAddrSpaceCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildAddrSpaceCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildZExtOrBitCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildZExtOrBitCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildSExtOrBitCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildSExtOrBitCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildTruncOrBitCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildTruncOrBitCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildCast(B, Op, Val, DestTy, Name)
    ccall((:LLVMBuildCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMOpcode, LLVMValueRef, LLVMTypeRef, Cstring), B, Op, Val, DestTy, Name)
end

function LLVMBuildPointerCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildPointerCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMBuildIntCast2(arg1, Val, DestTy, IsSigned, Name)
    ccall((:LLVMBuildIntCast2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, LLVMBool, Cstring), arg1, Val, DestTy, IsSigned, Name)
end

function LLVMBuildFPCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildFPCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

"""
    LLVMBuildIntCast(arg1, Val, DestTy, Name)

Deprecated: This cast is always signed. Use [`LLVMBuildIntCast2`](@ref) instead.
"""
function LLVMBuildIntCast(arg1, Val, DestTy, Name)
    ccall((:LLVMBuildIntCast, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, Val, DestTy, Name)
end

function LLVMGetCastOpcode(Src, SrcIsSigned, DestTy, DestIsSigned)
    ccall((:LLVMGetCastOpcode, libllvm), LLVMOpcode, (LLVMValueRef, LLVMBool, LLVMTypeRef, LLVMBool), Src, SrcIsSigned, DestTy, DestIsSigned)
end

function LLVMBuildICmp(arg1, Op, LHS, RHS, Name)
    ccall((:LLVMBuildICmp, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMIntPredicate, LLVMValueRef, LLVMValueRef, Cstring), arg1, Op, LHS, RHS, Name)
end

function LLVMBuildFCmp(arg1, Op, LHS, RHS, Name)
    ccall((:LLVMBuildFCmp, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMRealPredicate, LLVMValueRef, LLVMValueRef, Cstring), arg1, Op, LHS, RHS, Name)
end

function LLVMBuildPhi(arg1, Ty, Name)
    ccall((:LLVMBuildPhi, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, Cstring), arg1, Ty, Name)
end

function LLVMBuildCall(arg1, Fn, Args, NumArgs, Name)
    ccall((:LLVMBuildCall, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), arg1, Fn, Args, NumArgs, Name)
end

function LLVMBuildCall2(arg1, arg2, Fn, Args, NumArgs, Name)
    ccall((:LLVMBuildCall2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, Ptr{LLVMValueRef}, Cuint, Cstring), arg1, arg2, Fn, Args, NumArgs, Name)
end

function LLVMBuildSelect(arg1, If, Then, Else, Name)
    ccall((:LLVMBuildSelect, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, If, Then, Else, Name)
end

function LLVMBuildVAArg(arg1, List, Ty, Name)
    ccall((:LLVMBuildVAArg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, Cstring), arg1, List, Ty, Name)
end

function LLVMBuildExtractElement(arg1, VecVal, Index, Name)
    ccall((:LLVMBuildExtractElement, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, VecVal, Index, Name)
end

function LLVMBuildInsertElement(arg1, VecVal, EltVal, Index, Name)
    ccall((:LLVMBuildInsertElement, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, VecVal, EltVal, Index, Name)
end

function LLVMBuildShuffleVector(arg1, V1, V2, Mask, Name)
    ccall((:LLVMBuildShuffleVector, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, V1, V2, Mask, Name)
end

function LLVMBuildExtractValue(arg1, AggVal, Index, Name)
    ccall((:LLVMBuildExtractValue, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cuint, Cstring), arg1, AggVal, Index, Name)
end

function LLVMBuildInsertValue(arg1, AggVal, EltVal, Index, Name)
    ccall((:LLVMBuildInsertValue, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cuint, Cstring), arg1, AggVal, EltVal, Index, Name)
end

function LLVMBuildFreeze(arg1, Val, Name)
    ccall((:LLVMBuildFreeze, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, Val, Name)
end

function LLVMBuildIsNull(arg1, Val, Name)
    ccall((:LLVMBuildIsNull, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, Val, Name)
end

function LLVMBuildIsNotNull(arg1, Val, Name)
    ccall((:LLVMBuildIsNotNull, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, Cstring), arg1, Val, Name)
end

function LLVMBuildPtrDiff(arg1, LHS, RHS, Name)
    ccall((:LLVMBuildPtrDiff, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, LHS, RHS, Name)
end

function LLVMBuildPtrDiff2(arg1, ElemTy, LHS, RHS, Name)
    ccall((:LLVMBuildPtrDiff2, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMTypeRef, LLVMValueRef, LLVMValueRef, Cstring), arg1, ElemTy, LHS, RHS, Name)
end

function LLVMBuildFence(B, ordering, singleThread, Name)
    ccall((:LLVMBuildFence, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMAtomicOrdering, LLVMBool, Cstring), B, ordering, singleThread, Name)
end

function LLVMBuildAtomicRMW(B, op, PTR, Val, ordering, singleThread)
    ccall((:LLVMBuildAtomicRMW, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMAtomicRMWBinOp, LLVMValueRef, LLVMValueRef, LLVMAtomicOrdering, LLVMBool), B, op, PTR, Val, ordering, singleThread)
end

function LLVMBuildAtomicCmpXchg(B, Ptr, Cmp, New, SuccessOrdering, FailureOrdering, SingleThread)
    ccall((:LLVMBuildAtomicCmpXchg, libllvm), LLVMValueRef, (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMAtomicOrdering, LLVMAtomicOrdering, LLVMBool), B, Ptr, Cmp, New, SuccessOrdering, FailureOrdering, SingleThread)
end

"""
    LLVMGetNumMaskElements(ShuffleVectorInst)

Get the number of elements in the mask of a ShuffleVector instruction.
"""
function LLVMGetNumMaskElements(ShuffleVectorInst)
    ccall((:LLVMGetNumMaskElements, libllvm), Cuint, (LLVMValueRef,), ShuffleVectorInst)
end

"""
    LLVMGetUndefMaskElem()

# Returns
a constant that specifies that the result of a `ShuffleVectorInst` is undefined.
"""
function LLVMGetUndefMaskElem()
    ccall((:LLVMGetUndefMaskElem, libllvm), Cint, ())
end

"""
    LLVMGetMaskValue(ShuffleVectorInst, Elt)

Get the mask value at position Elt in the mask of a ShuffleVector instruction.

# Returns
the result of [`LLVMGetUndefMaskElem`](@ref)() if the mask value is undef at that position.
"""
function LLVMGetMaskValue(ShuffleVectorInst, Elt)
    ccall((:LLVMGetMaskValue, libllvm), Cint, (LLVMValueRef, Cuint), ShuffleVectorInst, Elt)
end

function LLVMIsAtomicSingleThread(AtomicInst)
    ccall((:LLVMIsAtomicSingleThread, libllvm), LLVMBool, (LLVMValueRef,), AtomicInst)
end

function LLVMSetAtomicSingleThread(AtomicInst, SingleThread)
    ccall((:LLVMSetAtomicSingleThread, libllvm), Cvoid, (LLVMValueRef, LLVMBool), AtomicInst, SingleThread)
end

function LLVMGetCmpXchgSuccessOrdering(CmpXchgInst)
    ccall((:LLVMGetCmpXchgSuccessOrdering, libllvm), LLVMAtomicOrdering, (LLVMValueRef,), CmpXchgInst)
end

function LLVMSetCmpXchgSuccessOrdering(CmpXchgInst, Ordering)
    ccall((:LLVMSetCmpXchgSuccessOrdering, libllvm), Cvoid, (LLVMValueRef, LLVMAtomicOrdering), CmpXchgInst, Ordering)
end

function LLVMGetCmpXchgFailureOrdering(CmpXchgInst)
    ccall((:LLVMGetCmpXchgFailureOrdering, libllvm), LLVMAtomicOrdering, (LLVMValueRef,), CmpXchgInst)
end

function LLVMSetCmpXchgFailureOrdering(CmpXchgInst, Ordering)
    ccall((:LLVMSetCmpXchgFailureOrdering, libllvm), Cvoid, (LLVMValueRef, LLVMAtomicOrdering), CmpXchgInst, Ordering)
end

"""
Interface used to provide a module to JIT or interpreter. This is now just a synonym for llvm::Module, but we have to keep using the different type to keep binary compatibility.
"""
const LLVMModuleProviderRef = Ptr{LLVMOpaqueModuleProvider}

"""
    LLVMCreateModuleProviderForExistingModule(M)

Changes the type of M so it can be passed to FunctionPassManagers and the JIT. They take ModuleProviders for historical reasons.
"""
function LLVMCreateModuleProviderForExistingModule(M)
    ccall((:LLVMCreateModuleProviderForExistingModule, libllvm), LLVMModuleProviderRef, (LLVMModuleRef,), M)
end

"""
    LLVMDisposeModuleProvider(M)

Destroys the module M.
"""
function LLVMDisposeModuleProvider(M)
    ccall((:LLVMDisposeModuleProvider, libllvm), Cvoid, (LLVMModuleProviderRef,), M)
end

"""
    LLVMCreateMemoryBufferWithContentsOfFile(Path, OutMemBuf, OutMessage)

` LLVMCCoreMemoryBuffers Memory Buffers`

@{
"""
function LLVMCreateMemoryBufferWithContentsOfFile(Path, OutMemBuf, OutMessage)
    ccall((:LLVMCreateMemoryBufferWithContentsOfFile, libllvm), LLVMBool, (Cstring, Ptr{LLVMMemoryBufferRef}, Ptr{Cstring}), Path, OutMemBuf, OutMessage)
end

function LLVMCreateMemoryBufferWithSTDIN(OutMemBuf, OutMessage)
    ccall((:LLVMCreateMemoryBufferWithSTDIN, libllvm), LLVMBool, (Ptr{LLVMMemoryBufferRef}, Ptr{Cstring}), OutMemBuf, OutMessage)
end

function LLVMCreateMemoryBufferWithMemoryRange(InputData, InputDataLength, BufferName, RequiresNullTerminator)
    ccall((:LLVMCreateMemoryBufferWithMemoryRange, libllvm), LLVMMemoryBufferRef, (Cstring, Csize_t, Cstring, LLVMBool), InputData, InputDataLength, BufferName, RequiresNullTerminator)
end

function LLVMCreateMemoryBufferWithMemoryRangeCopy(InputData, InputDataLength, BufferName)
    ccall((:LLVMCreateMemoryBufferWithMemoryRangeCopy, libllvm), LLVMMemoryBufferRef, (Cstring, Csize_t, Cstring), InputData, InputDataLength, BufferName)
end

function LLVMGetBufferStart(MemBuf)
    ccall((:LLVMGetBufferStart, libllvm), Cstring, (LLVMMemoryBufferRef,), MemBuf)
end

function LLVMGetBufferSize(MemBuf)
    ccall((:LLVMGetBufferSize, libllvm), Csize_t, (LLVMMemoryBufferRef,), MemBuf)
end

function LLVMDisposeMemoryBuffer(MemBuf)
    ccall((:LLVMDisposeMemoryBuffer, libllvm), Cvoid, (LLVMMemoryBufferRef,), MemBuf)
end

"""
    LLVMGetGlobalPassRegistry()

Return the global pass registry, for use with initialization functions.

# See also
llvm::PassRegistry::getPassRegistry
"""
function LLVMGetGlobalPassRegistry()
    ccall((:LLVMGetGlobalPassRegistry, libllvm), LLVMPassRegistryRef, ())
end

"""
# See also
llvm::PassManagerBase
"""
const LLVMPassManagerRef = Ptr{LLVMOpaquePassManager}

"""
    LLVMCreatePassManager()

Constructs a new whole-module pass pipeline. This type of pipeline is suitable for link-time optimization and whole-module transformations.

# See also
llvm::PassManager::PassManager
"""
function LLVMCreatePassManager()
    ccall((:LLVMCreatePassManager, libllvm), LLVMPassManagerRef, ())
end

"""
    LLVMCreateFunctionPassManagerForModule(M)

Constructs a new function-by-function pass pipeline over the module provider. It does not take ownership of the module provider. This type of pipeline is suitable for code generation and JIT compilation tasks.

# See also
llvm::FunctionPassManager::FunctionPassManager
"""
function LLVMCreateFunctionPassManagerForModule(M)
    ccall((:LLVMCreateFunctionPassManagerForModule, libllvm), LLVMPassManagerRef, (LLVMModuleRef,), M)
end

"""
    LLVMCreateFunctionPassManager(MP)

Deprecated: Use [`LLVMCreateFunctionPassManagerForModule`](@ref) instead.
"""
function LLVMCreateFunctionPassManager(MP)
    ccall((:LLVMCreateFunctionPassManager, libllvm), LLVMPassManagerRef, (LLVMModuleProviderRef,), MP)
end

"""
    LLVMRunPassManager(PM, M)

Initializes, executes on the provided module, and finalizes all of the passes scheduled in the pass manager. Returns 1 if any of the passes modified the module, 0 otherwise.

# See also
llvm::PassManager::run(Module&)
"""
function LLVMRunPassManager(PM, M)
    ccall((:LLVMRunPassManager, libllvm), LLVMBool, (LLVMPassManagerRef, LLVMModuleRef), PM, M)
end

"""
    LLVMInitializeFunctionPassManager(FPM)

Initializes all of the function passes scheduled in the function pass manager. Returns 1 if any of the passes modified the module, 0 otherwise.

# See also
llvm::FunctionPassManager::doInitialization
"""
function LLVMInitializeFunctionPassManager(FPM)
    ccall((:LLVMInitializeFunctionPassManager, libllvm), LLVMBool, (LLVMPassManagerRef,), FPM)
end

"""
    LLVMRunFunctionPassManager(FPM, F)

Executes all of the function passes scheduled in the function pass manager on the provided function. Returns 1 if any of the passes modified the function, false otherwise.

# See also
llvm::FunctionPassManager::run(Function&)
"""
function LLVMRunFunctionPassManager(FPM, F)
    ccall((:LLVMRunFunctionPassManager, libllvm), LLVMBool, (LLVMPassManagerRef, LLVMValueRef), FPM, F)
end

"""
    LLVMFinalizeFunctionPassManager(FPM)

Finalizes all of the function passes scheduled in the function pass manager. Returns 1 if any of the passes modified the module, 0 otherwise.

# See also
llvm::FunctionPassManager::doFinalization
"""
function LLVMFinalizeFunctionPassManager(FPM)
    ccall((:LLVMFinalizeFunctionPassManager, libllvm), LLVMBool, (LLVMPassManagerRef,), FPM)
end

"""
    LLVMDisposePassManager(PM)

Frees the memory of a pass pipeline. For function pipelines, does not free the module provider.

# See also
llvm::PassManagerBase::~PassManagerBase.
"""
function LLVMDisposePassManager(PM)
    ccall((:LLVMDisposePassManager, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMStartMultithreaded()

Deprecated: Multi-threading can only be enabled/disabled with the compile time define LLVM\\_ENABLE\\_THREADS. This function always returns [`LLVMIsMultithreaded`](@ref)().
"""
function LLVMStartMultithreaded()
    ccall((:LLVMStartMultithreaded, libllvm), LLVMBool, ())
end

"""
    LLVMStopMultithreaded()

Deprecated: Multi-threading can only be enabled/disabled with the compile time define LLVM\\_ENABLE\\_THREADS.
"""
function LLVMStopMultithreaded()
    ccall((:LLVMStopMultithreaded, libllvm), Cvoid, ())
end

"""
    LLVMIsMultithreaded()

Check whether LLVM is executing in thread-safe mode or not.

# See also
llvm::llvm\\_is\\_multithreaded
"""
function LLVMIsMultithreaded()
    ccall((:LLVMIsMultithreaded, libllvm), LLVMBool, ())
end

"""
    LLVMDIFlags

Debug info flags.
"""
@cenum LLVMDIFlags::UInt32 begin
    LLVMDIFlagZero = 0
    LLVMDIFlagPrivate = 1
    LLVMDIFlagProtected = 2
    LLVMDIFlagPublic = 3
    LLVMDIFlagFwdDecl = 4
    LLVMDIFlagAppleBlock = 8
    LLVMDIFlagReservedBit4 = 16
    LLVMDIFlagVirtual = 32
    LLVMDIFlagArtificial = 64
    LLVMDIFlagExplicit = 128
    LLVMDIFlagPrototyped = 256
    LLVMDIFlagObjcClassComplete = 512
    LLVMDIFlagObjectPointer = 1024
    LLVMDIFlagVector = 2048
    LLVMDIFlagStaticMember = 4096
    LLVMDIFlagLValueReference = 8192
    LLVMDIFlagRValueReference = 16384
    LLVMDIFlagReserved = 32768
    LLVMDIFlagSingleInheritance = 65536
    LLVMDIFlagMultipleInheritance = 131072
    LLVMDIFlagVirtualInheritance = 196608
    LLVMDIFlagIntroducedVirtual = 262144
    LLVMDIFlagBitField = 524288
    LLVMDIFlagNoReturn = 1048576
    LLVMDIFlagTypePassByValue = 4194304
    LLVMDIFlagTypePassByReference = 8388608
    LLVMDIFlagEnumClass = 16777216
    LLVMDIFlagFixedEnum = 16777216
    LLVMDIFlagThunk = 33554432
    LLVMDIFlagNonTrivial = 67108864
    LLVMDIFlagBigEndian = 134217728
    LLVMDIFlagLittleEndian = 268435456
    LLVMDIFlagIndirectVirtualBase = 36
    LLVMDIFlagAccessibility = 3
    LLVMDIFlagPtrToMemberRep = 196608
end

"""
    LLVMDWARFSourceLanguage

Source languages known by DWARF.
"""
@cenum LLVMDWARFSourceLanguage::UInt32 begin
    LLVMDWARFSourceLanguageC89 = 0
    LLVMDWARFSourceLanguageC = 1
    LLVMDWARFSourceLanguageAda83 = 2
    LLVMDWARFSourceLanguageC_plus_plus = 3
    LLVMDWARFSourceLanguageCobol74 = 4
    LLVMDWARFSourceLanguageCobol85 = 5
    LLVMDWARFSourceLanguageFortran77 = 6
    LLVMDWARFSourceLanguageFortran90 = 7
    LLVMDWARFSourceLanguagePascal83 = 8
    LLVMDWARFSourceLanguageModula2 = 9
    LLVMDWARFSourceLanguageJava = 10
    LLVMDWARFSourceLanguageC99 = 11
    LLVMDWARFSourceLanguageAda95 = 12
    LLVMDWARFSourceLanguageFortran95 = 13
    LLVMDWARFSourceLanguagePLI = 14
    LLVMDWARFSourceLanguageObjC = 15
    LLVMDWARFSourceLanguageObjC_plus_plus = 16
    LLVMDWARFSourceLanguageUPC = 17
    LLVMDWARFSourceLanguageD = 18
    LLVMDWARFSourceLanguagePython = 19
    LLVMDWARFSourceLanguageOpenCL = 20
    LLVMDWARFSourceLanguageGo = 21
    LLVMDWARFSourceLanguageModula3 = 22
    LLVMDWARFSourceLanguageHaskell = 23
    LLVMDWARFSourceLanguageC_plus_plus_03 = 24
    LLVMDWARFSourceLanguageC_plus_plus_11 = 25
    LLVMDWARFSourceLanguageOCaml = 26
    LLVMDWARFSourceLanguageRust = 27
    LLVMDWARFSourceLanguageC11 = 28
    LLVMDWARFSourceLanguageSwift = 29
    LLVMDWARFSourceLanguageJulia = 30
    LLVMDWARFSourceLanguageDylan = 31
    LLVMDWARFSourceLanguageC_plus_plus_14 = 32
    LLVMDWARFSourceLanguageFortran03 = 33
    LLVMDWARFSourceLanguageFortran08 = 34
    LLVMDWARFSourceLanguageRenderScript = 35
    LLVMDWARFSourceLanguageBLISS = 36
    LLVMDWARFSourceLanguageMips_Assembler = 37
    LLVMDWARFSourceLanguageGOOGLE_RenderScript = 38
    LLVMDWARFSourceLanguageBORLAND_Delphi = 39
end

"""
    LLVMDWARFEmissionKind

The amount of debug information to emit.
"""
@cenum LLVMDWARFEmissionKind::UInt32 begin
    LLVMDWARFEmissionNone = 0
    LLVMDWARFEmissionFull = 1
    LLVMDWARFEmissionLineTablesOnly = 2
end

"""
    ##Ctag#231

The kind of metadata nodes.
"""
@cenum var"##Ctag#231"::UInt32 begin
    LLVMMDStringMetadataKind = 0
    LLVMConstantAsMetadataMetadataKind = 1
    LLVMLocalAsMetadataMetadataKind = 2
    LLVMDistinctMDOperandPlaceholderMetadataKind = 3
    LLVMMDTupleMetadataKind = 4
    LLVMDILocationMetadataKind = 5
    LLVMDIExpressionMetadataKind = 6
    LLVMDIGlobalVariableExpressionMetadataKind = 7
    LLVMGenericDINodeMetadataKind = 8
    LLVMDISubrangeMetadataKind = 9
    LLVMDIEnumeratorMetadataKind = 10
    LLVMDIBasicTypeMetadataKind = 11
    LLVMDIDerivedTypeMetadataKind = 12
    LLVMDICompositeTypeMetadataKind = 13
    LLVMDISubroutineTypeMetadataKind = 14
    LLVMDIFileMetadataKind = 15
    LLVMDICompileUnitMetadataKind = 16
    LLVMDISubprogramMetadataKind = 17
    LLVMDILexicalBlockMetadataKind = 18
    LLVMDILexicalBlockFileMetadataKind = 19
    LLVMDINamespaceMetadataKind = 20
    LLVMDIModuleMetadataKind = 21
    LLVMDITemplateTypeParameterMetadataKind = 22
    LLVMDITemplateValueParameterMetadataKind = 23
    LLVMDIGlobalVariableMetadataKind = 24
    LLVMDILocalVariableMetadataKind = 25
    LLVMDILabelMetadataKind = 26
    LLVMDIObjCPropertyMetadataKind = 27
    LLVMDIImportedEntityMetadataKind = 28
    LLVMDIMacroMetadataKind = 29
    LLVMDIMacroFileMetadataKind = 30
    LLVMDICommonBlockMetadataKind = 31
    LLVMDIStringTypeMetadataKind = 32
    LLVMDIGenericSubrangeMetadataKind = 33
    LLVMDIArgListMetadataKind = 34
end

const LLVMMetadataKind = Cuint

"""
An LLVM DWARF type encoding.
"""
const LLVMDWARFTypeEncoding = Cuint

"""
    LLVMDWARFMacinfoRecordType

Describes the kind of macro declaration used for [`LLVMDIBuilderCreateMacro`](@ref).

!!! note

    Values are from DW\\_MACINFO\\_* constants in the DWARF specification.

# See also
llvm::dwarf::MacinfoRecordType
"""
@cenum LLVMDWARFMacinfoRecordType::UInt32 begin
    LLVMDWARFMacinfoRecordTypeDefine = 1
    LLVMDWARFMacinfoRecordTypeMacro = 2
    LLVMDWARFMacinfoRecordTypeStartFile = 3
    LLVMDWARFMacinfoRecordTypeEndFile = 4
    LLVMDWARFMacinfoRecordTypeVendorExt = 255
end

"""
    LLVMDebugMetadataVersion()

The current debug metadata version number.
"""
function LLVMDebugMetadataVersion()
    ccall((:LLVMDebugMetadataVersion, libllvm), Cuint, ())
end

"""
    LLVMGetModuleDebugMetadataVersion(Module)

The version of debug metadata that's present in the provided `Module`.
"""
function LLVMGetModuleDebugMetadataVersion(Module)
    ccall((:LLVMGetModuleDebugMetadataVersion, libllvm), Cuint, (LLVMModuleRef,), Module)
end

"""
    LLVMStripModuleDebugInfo(Module)

Strip debug info in the module if it exists. To do this, we remove all calls to the debugger intrinsics and any named metadata for debugging. We also remove debug locations for instructions. Return true if module is modified.
"""
function LLVMStripModuleDebugInfo(Module)
    ccall((:LLVMStripModuleDebugInfo, libllvm), LLVMBool, (LLVMModuleRef,), Module)
end

"""
Represents an LLVM debug info builder.

This models llvm::DIBuilder.
"""
const LLVMDIBuilderRef = Ptr{LLVMOpaqueDIBuilder}

"""
    LLVMCreateDIBuilderDisallowUnresolved(M)

Construct a builder for a module, and do not allow for unresolved nodes attached to the module.
"""
function LLVMCreateDIBuilderDisallowUnresolved(M)
    ccall((:LLVMCreateDIBuilderDisallowUnresolved, libllvm), LLVMDIBuilderRef, (LLVMModuleRef,), M)
end

"""
    LLVMCreateDIBuilder(M)

Construct a builder for a module and collect unresolved nodes attached to the module in order to resolve cycles during a call to [`LLVMDIBuilderFinalize`](@ref).
"""
function LLVMCreateDIBuilder(M)
    ccall((:LLVMCreateDIBuilder, libllvm), LLVMDIBuilderRef, (LLVMModuleRef,), M)
end

"""
    LLVMDisposeDIBuilder(Builder)

Deallocates the `DIBuilder` and everything it owns.

!!! note

    You must call [`LLVMDIBuilderFinalize`](@ref) before this
"""
function LLVMDisposeDIBuilder(Builder)
    ccall((:LLVMDisposeDIBuilder, libllvm), Cvoid, (LLVMDIBuilderRef,), Builder)
end

"""
    LLVMDIBuilderFinalize(Builder)

Construct any deferred debug info descriptors.
"""
function LLVMDIBuilderFinalize(Builder)
    ccall((:LLVMDIBuilderFinalize, libllvm), Cvoid, (LLVMDIBuilderRef,), Builder)
end

"""
    LLVMDIBuilderFinalizeSubprogram(Builder, Subprogram)

Finalize a specific subprogram. No new variables may be added to this subprogram afterwards.
"""
function LLVMDIBuilderFinalizeSubprogram(Builder, Subprogram)
    ccall((:LLVMDIBuilderFinalizeSubprogram, libllvm), Cvoid, (LLVMDIBuilderRef, LLVMMetadataRef), Builder, Subprogram)
end

"""
    LLVMDIBuilderCreateCompileUnit(Builder, Lang, FileRef, Producer, ProducerLen, isOptimized, Flags, FlagsLen, RuntimeVer, SplitName, SplitNameLen, Kind, DWOId, SplitDebugInlining, DebugInfoForProfiling, SysRoot, SysRootLen, SDK, SDKLen)

A CompileUnit provides an anchor for all debugging information generated during this instance of compilation.

# Arguments
* `Lang`: Source programming language, eg. `LLVMDWARFSourceLanguageC99`
* `FileRef`: File info.
* `Producer`: Identify the producer of debugging information and code. Usually this is a compiler version string.
* `ProducerLen`: The length of the C string passed to `Producer`.
* `isOptimized`: A boolean flag which indicates whether optimization is enabled or not.
* `Flags`: This string lists command line options. This string is directly embedded in debug info output which may be used by a tool analyzing generated debugging information.
* `FlagsLen`: The length of the C string passed to `Flags`.
* `RuntimeVer`: This indicates runtime version for languages like Objective-C.
* `SplitName`: The name of the file that we'll split debug info out into.
* `SplitNameLen`: The length of the C string passed to `SplitName`.
* `Kind`: The kind of debug information to generate.
* `DWOId`: The DWOId if this is a split skeleton compile unit.
* `SplitDebugInlining`: Whether to emit inline debug info.
* `DebugInfoForProfiling`: Whether to emit extra debug info for profile collection.
* `SysRoot`: The Clang system root (value of -isysroot).
* `SysRootLen`: The length of the C string passed to `SysRoot`.
* `SDK`: The SDK. On Darwin, the last component of the sysroot.
* `SDKLen`: The length of the C string passed to `SDK`.
"""
function LLVMDIBuilderCreateCompileUnit(Builder, Lang, FileRef, Producer, ProducerLen, isOptimized, Flags, FlagsLen, RuntimeVer, SplitName, SplitNameLen, Kind, DWOId, SplitDebugInlining, DebugInfoForProfiling, SysRoot, SysRootLen, SDK, SDKLen)
    ccall((:LLVMDIBuilderCreateCompileUnit, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMDWARFSourceLanguage, LLVMMetadataRef, Cstring, Csize_t, LLVMBool, Cstring, Csize_t, Cuint, Cstring, Csize_t, LLVMDWARFEmissionKind, Cuint, LLVMBool, LLVMBool, Cstring, Csize_t, Cstring, Csize_t), Builder, Lang, FileRef, Producer, ProducerLen, isOptimized, Flags, FlagsLen, RuntimeVer, SplitName, SplitNameLen, Kind, DWOId, SplitDebugInlining, DebugInfoForProfiling, SysRoot, SysRootLen, SDK, SDKLen)
end

"""
    LLVMDIBuilderCreateFile(Builder, Filename, FilenameLen, Directory, DirectoryLen)

Create a file descriptor to hold debugging information for a file.

# Arguments
* `Builder`: The `DIBuilder`.
* `Filename`: File name.
* `FilenameLen`: The length of the C string passed to `Filename`.
* `Directory`: Directory.
* `DirectoryLen`: The length of the C string passed to `Directory`.
"""
function LLVMDIBuilderCreateFile(Builder, Filename, FilenameLen, Directory, DirectoryLen)
    ccall((:LLVMDIBuilderCreateFile, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t, Cstring, Csize_t), Builder, Filename, FilenameLen, Directory, DirectoryLen)
end

"""
    LLVMDIBuilderCreateModule(Builder, ParentScope, Name, NameLen, ConfigMacros, ConfigMacrosLen, IncludePath, IncludePathLen, APINotesFile, APINotesFileLen)

Creates a new descriptor for a module with the specified parent scope.

# Arguments
* `Builder`: The `DIBuilder`.
* `ParentScope`: The parent scope containing this module declaration.
* `Name`: Module name.
* `NameLen`: The length of the C string passed to `Name`.
* `ConfigMacros`: A space-separated shell-quoted list of -D macro definitions as they would appear on a command line.
* `ConfigMacrosLen`: The length of the C string passed to `ConfigMacros`.
* `IncludePath`: The path to the module map file.
* `IncludePathLen`: The length of the C string passed to `IncludePath`.
* `APINotesFile`: The path to an API notes file for the module.
* `APINotesFileLen`: The length of the C string passed to `APINotestFile`.
"""
function LLVMDIBuilderCreateModule(Builder, ParentScope, Name, NameLen, ConfigMacros, ConfigMacrosLen, IncludePath, IncludePathLen, APINotesFile, APINotesFileLen)
    ccall((:LLVMDIBuilderCreateModule, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, Cstring, Csize_t, Cstring, Csize_t, Cstring, Csize_t), Builder, ParentScope, Name, NameLen, ConfigMacros, ConfigMacrosLen, IncludePath, IncludePathLen, APINotesFile, APINotesFileLen)
end

"""
    LLVMDIBuilderCreateNameSpace(Builder, ParentScope, Name, NameLen, ExportSymbols)

Creates a new descriptor for a namespace with the specified parent scope.

# Arguments
* `Builder`: The `DIBuilder`.
* `ParentScope`: The parent scope containing this module declaration.
* `Name`: NameSpace name.
* `NameLen`: The length of the C string passed to `Name`.
* `ExportSymbols`: Whether or not the namespace exports symbols, e.g. this is true of C++ inline namespaces.
"""
function LLVMDIBuilderCreateNameSpace(Builder, ParentScope, Name, NameLen, ExportSymbols)
    ccall((:LLVMDIBuilderCreateNameSpace, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMBool), Builder, ParentScope, Name, NameLen, ExportSymbols)
end

"""
    LLVMDIBuilderCreateFunction(Builder, Scope, Name, NameLen, LinkageName, LinkageNameLen, File, LineNo, Ty, IsLocalToUnit, IsDefinition, ScopeLine, Flags, IsOptimized)

Create a new descriptor for the specified subprogram.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: Function scope.
* `Name`: Function name.
* `NameLen`: Length of enumeration name.
* `LinkageName`: Mangled function name.
* `LinkageNameLen`: Length of linkage name.
* `File`: File where this variable is defined.
* `LineNo`: Line number.
* `Ty`: Function type.
* `IsLocalToUnit`: True if this function is not externally visible.
* `IsDefinition`: True if this is a function definition.
* `ScopeLine`: Set to the beginning of the scope this starts
* `Flags`: E.g.: `LLVMDIFlagLValueReference`. These flags are used to emit dwarf attributes.
* `IsOptimized`: True if optimization is ON.
"""
function LLVMDIBuilderCreateFunction(Builder, Scope, Name, NameLen, LinkageName, LinkageNameLen, File, LineNo, Ty, IsLocalToUnit, IsDefinition, ScopeLine, Flags, IsOptimized)
    ccall((:LLVMDIBuilderCreateFunction, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMBool, LLVMBool, Cuint, LLVMDIFlags, LLVMBool), Builder, Scope, Name, NameLen, LinkageName, LinkageNameLen, File, LineNo, Ty, IsLocalToUnit, IsDefinition, ScopeLine, Flags, IsOptimized)
end

"""
    LLVMDIBuilderCreateLexicalBlock(Builder, Scope, File, Line, Column)

Create a descriptor for a lexical block with the specified parent context.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: Parent lexical block.
* `File`: Source file.
* `Line`: The line in the source file.
* `Column`: The column in the source file.
"""
function LLVMDIBuilderCreateLexicalBlock(Builder, Scope, File, Line, Column)
    ccall((:LLVMDIBuilderCreateLexicalBlock, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, Cuint, Cuint), Builder, Scope, File, Line, Column)
end

"""
    LLVMDIBuilderCreateLexicalBlockFile(Builder, Scope, File, Discriminator)

Create a descriptor for a lexical block with a new file attached.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: Lexical block.
* `File`: Source file.
* `Discriminator`: DWARF path discriminator value.
"""
function LLVMDIBuilderCreateLexicalBlockFile(Builder, Scope, File, Discriminator)
    ccall((:LLVMDIBuilderCreateLexicalBlockFile, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, Cuint), Builder, Scope, File, Discriminator)
end

"""
    LLVMDIBuilderCreateImportedModuleFromNamespace(Builder, Scope, NS, File, Line)

Create a descriptor for an imported namespace. Suitable for e.g. C++ using declarations.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: The scope this module is imported into
* `File`: File where the declaration is located.
* `Line`: Line number of the declaration.
"""
function LLVMDIBuilderCreateImportedModuleFromNamespace(Builder, Scope, NS, File, Line)
    ccall((:LLVMDIBuilderCreateImportedModuleFromNamespace, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, Cuint), Builder, Scope, NS, File, Line)
end

"""
    LLVMDIBuilderCreateImportedModuleFromAlias(Builder, Scope, ImportedEntity, File, Line, Elements, NumElements)

Create a descriptor for an imported module that aliases another imported entity descriptor.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: The scope this module is imported into
* `ImportedEntity`: Previous imported entity to alias.
* `File`: File where the declaration is located.
* `Line`: Line number of the declaration.
* `Elements`: Renamed elements.
* `NumElements`: Number of renamed elements.
"""
function LLVMDIBuilderCreateImportedModuleFromAlias(Builder, Scope, ImportedEntity, File, Line, Elements, NumElements)
    ccall((:LLVMDIBuilderCreateImportedModuleFromAlias, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, Cuint, Ptr{LLVMMetadataRef}, Cuint), Builder, Scope, ImportedEntity, File, Line, Elements, NumElements)
end

"""
    LLVMDIBuilderCreateImportedModuleFromModule(Builder, Scope, M, File, Line, Elements, NumElements)

Create a descriptor for an imported module.

# Arguments
* `Builder`: The `DIBuilder`.
* `Scope`: The scope this module is imported into
* `M`: The module being imported here
* `File`: File where the declaration is located.
* `Line`: Line number of the declaration.
* `Elements`: Renamed elements.
* `NumElements`: Number of renamed elements.
"""
function LLVMDIBuilderCreateImportedModuleFromModule(Builder, Scope, M, File, Line, Elements, NumElements)
    ccall((:LLVMDIBuilderCreateImportedModuleFromModule, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, Cuint, Ptr{LLVMMetadataRef}, Cuint), Builder, Scope, M, File, Line, Elements, NumElements)
end

"""
    LLVMDIBuilderCreateImportedDeclaration(Builder, Scope, Decl, File, Line, Name, NameLen, Elements, NumElements)

Create a descriptor for an imported function, type, or variable. Suitable for e.g. FORTRAN-style USE declarations.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: The scope this module is imported into.
* `Decl`: The declaration (or definition) of a function, type, or variable.
* `File`: File where the declaration is located.
* `Line`: Line number of the declaration.
* `Name`: A name that uniquely identifies this imported declaration.
* `NameLen`: The length of the C string passed to `Name`.
* `Elements`: Renamed elements.
* `NumElements`: Number of renamed elements.
"""
function LLVMDIBuilderCreateImportedDeclaration(Builder, Scope, Decl, File, Line, Name, NameLen, Elements, NumElements)
    ccall((:LLVMDIBuilderCreateImportedDeclaration, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, Cuint, Cstring, Csize_t, Ptr{LLVMMetadataRef}, Cuint), Builder, Scope, Decl, File, Line, Name, NameLen, Elements, NumElements)
end

"""
    LLVMDIBuilderCreateDebugLocation(Ctx, Line, Column, Scope, InlinedAt)

Creates a new DebugLocation that describes a source location.

!!! note

    If the item to which this location is attached cannot be attributed to a source line, pass 0 for the line and column.

# Arguments
* `Line`: The line in the source file.
* `Column`: The column in the source file.
* `Scope`: The scope in which the location resides.
* `InlinedAt`: The scope where this location was inlined, if at all. (optional).
"""
function LLVMDIBuilderCreateDebugLocation(Ctx, Line, Column, Scope, InlinedAt)
    ccall((:LLVMDIBuilderCreateDebugLocation, libllvm), LLVMMetadataRef, (LLVMContextRef, Cuint, Cuint, LLVMMetadataRef, LLVMMetadataRef), Ctx, Line, Column, Scope, InlinedAt)
end

"""
    LLVMDILocationGetLine(Location)

Get the line number of this debug location.

# Arguments
* `Location`: The debug location.
# See also
DILocation::getLine()
"""
function LLVMDILocationGetLine(Location)
    ccall((:LLVMDILocationGetLine, libllvm), Cuint, (LLVMMetadataRef,), Location)
end

"""
    LLVMDILocationGetColumn(Location)

Get the column number of this debug location.

# Arguments
* `Location`: The debug location.
# See also
DILocation::getColumn()
"""
function LLVMDILocationGetColumn(Location)
    ccall((:LLVMDILocationGetColumn, libllvm), Cuint, (LLVMMetadataRef,), Location)
end

"""
    LLVMDILocationGetScope(Location)

Get the local scope associated with this debug location.

# Arguments
* `Location`: The debug location.
# See also
DILocation::getScope()
"""
function LLVMDILocationGetScope(Location)
    ccall((:LLVMDILocationGetScope, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), Location)
end

"""
    LLVMDILocationGetInlinedAt(Location)

Get the "inline at" location associated with this debug location.

# Arguments
* `Location`: The debug location.
# See also
DILocation::getInlinedAt()
"""
function LLVMDILocationGetInlinedAt(Location)
    ccall((:LLVMDILocationGetInlinedAt, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), Location)
end

"""
    LLVMDIScopeGetFile(Scope)

Get the metadata of the file associated with a given scope.

# Arguments
* `Scope`: The scope object.
# See also
DIScope::getFile()
"""
function LLVMDIScopeGetFile(Scope)
    ccall((:LLVMDIScopeGetFile, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), Scope)
end

"""
    LLVMDIFileGetDirectory(File, Len)

Get the directory of a given file.

# Arguments
* `File`: The file object.
* `Len`: The length of the returned string.
# See also
DIFile::getDirectory()
"""
function LLVMDIFileGetDirectory(File, Len)
    ccall((:LLVMDIFileGetDirectory, libllvm), Cstring, (LLVMMetadataRef, Ptr{Cuint}), File, Len)
end

"""
    LLVMDIFileGetFilename(File, Len)

Get the name of a given file.

# Arguments
* `File`: The file object.
* `Len`: The length of the returned string.
# See also
DIFile::getFilename()
"""
function LLVMDIFileGetFilename(File, Len)
    ccall((:LLVMDIFileGetFilename, libllvm), Cstring, (LLVMMetadataRef, Ptr{Cuint}), File, Len)
end

"""
    LLVMDIFileGetSource(File, Len)

Get the source of a given file.

# Arguments
* `File`: The file object.
* `Len`: The length of the returned string.
# See also
DIFile::getSource()
"""
function LLVMDIFileGetSource(File, Len)
    ccall((:LLVMDIFileGetSource, libllvm), Cstring, (LLVMMetadataRef, Ptr{Cuint}), File, Len)
end

"""
    LLVMDIBuilderGetOrCreateTypeArray(Builder, Data, NumElements)

Create a type array.

# Arguments
* `Builder`: The DIBuilder.
* `Data`: The type elements.
* `NumElements`: Number of type elements.
"""
function LLVMDIBuilderGetOrCreateTypeArray(Builder, Data, NumElements)
    ccall((:LLVMDIBuilderGetOrCreateTypeArray, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Ptr{LLVMMetadataRef}, Csize_t), Builder, Data, NumElements)
end

"""
    LLVMDIBuilderCreateSubroutineType(Builder, File, ParameterTypes, NumParameterTypes, Flags)

Create subroutine type.

# Arguments
* `Builder`: The DIBuilder.
* `File`: The file in which the subroutine resides.
* `ParameterTypes`: An array of subroutine parameter types. This includes return type at 0th index.
* `NumParameterTypes`: The number of parameter types in `ParameterTypes`
* `Flags`: E.g.: `LLVMDIFlagLValueReference`. These flags are used to emit dwarf attributes.
"""
function LLVMDIBuilderCreateSubroutineType(Builder, File, ParameterTypes, NumParameterTypes, Flags)
    ccall((:LLVMDIBuilderCreateSubroutineType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Ptr{LLVMMetadataRef}, Cuint, LLVMDIFlags), Builder, File, ParameterTypes, NumParameterTypes, Flags)
end

"""
    LLVMDIBuilderCreateMacro(Builder, ParentMacroFile, Line, RecordType, Name, NameLen, Value, ValueLen)

Create debugging information entry for a macro.

# Arguments
* `Builder`: The DIBuilder.
* `ParentMacroFile`: Macro parent (could be NULL).
* `Line`: Source line number where the macro is defined.
* `RecordType`: DW\\_MACINFO\\_define or DW\\_MACINFO\\_undef.
* `Name`: Macro name.
* `NameLen`: Macro name length.
* `Value`: Macro value.
* `ValueLen`: Macro value length.
"""
function LLVMDIBuilderCreateMacro(Builder, ParentMacroFile, Line, RecordType, Name, NameLen, Value, ValueLen)
    ccall((:LLVMDIBuilderCreateMacro, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cuint, LLVMDWARFMacinfoRecordType, Cstring, Csize_t, Cstring, Csize_t), Builder, ParentMacroFile, Line, RecordType, Name, NameLen, Value, ValueLen)
end

"""
    LLVMDIBuilderCreateTempMacroFile(Builder, ParentMacroFile, Line, File)

Create debugging information temporary entry for a macro file. List of macro node direct children will be calculated by DIBuilder, using the `ParentMacroFile` relationship.

# Arguments
* `Builder`: The DIBuilder.
* `ParentMacroFile`: Macro parent (could be NULL).
* `Line`: Source line number where the macro file is included.
* `File`: File descriptor containing the name of the macro file.
"""
function LLVMDIBuilderCreateTempMacroFile(Builder, ParentMacroFile, Line, File)
    ccall((:LLVMDIBuilderCreateTempMacroFile, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cuint, LLVMMetadataRef), Builder, ParentMacroFile, Line, File)
end

"""
    LLVMDIBuilderCreateEnumerator(Builder, Name, NameLen, Value, IsUnsigned)

Create debugging information entry for an enumerator.

# Arguments
* `Builder`: The DIBuilder.
* `Name`: Enumerator name.
* `NameLen`: Length of enumerator name.
* `Value`: Enumerator value.
* `IsUnsigned`: True if the value is unsigned.
"""
function LLVMDIBuilderCreateEnumerator(Builder, Name, NameLen, Value, IsUnsigned)
    ccall((:LLVMDIBuilderCreateEnumerator, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t, Int64, LLVMBool), Builder, Name, NameLen, Value, IsUnsigned)
end

"""
    LLVMDIBuilderCreateEnumerationType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Elements, NumElements, ClassTy)

Create debugging information entry for an enumeration.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Scope in which this enumeration is defined.
* `Name`: Enumeration name.
* `NameLen`: Length of enumeration name.
* `File`: File where this member is defined.
* `LineNumber`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `Elements`: Enumeration elements.
* `NumElements`: Number of enumeration elements.
* `ClassTy`: Underlying type of a C++11/ObjC fixed enum.
"""
function LLVMDIBuilderCreateEnumerationType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Elements, NumElements, ClassTy)
    ccall((:LLVMDIBuilderCreateEnumerationType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, Ptr{LLVMMetadataRef}, Cuint, LLVMMetadataRef), Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Elements, NumElements, ClassTy)
end

"""
    LLVMDIBuilderCreateUnionType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, Elements, NumElements, RunTimeLang, UniqueId, UniqueIdLen)

Create debugging information entry for a union.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Scope in which this union is defined.
* `Name`: Union name.
* `NameLen`: Length of union name.
* `File`: File where this member is defined.
* `LineNumber`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `Flags`: Flags to encode member attribute, e.g. private
* `Elements`: Union elements.
* `NumElements`: Number of union elements.
* `RunTimeLang`: Optional parameter, Objective-C runtime version.
* `UniqueId`: A unique identifier for the union.
* `UniqueIdLen`: Length of unique identifier.
"""
function LLVMDIBuilderCreateUnionType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, Elements, NumElements, RunTimeLang, UniqueId, UniqueIdLen)
    ccall((:LLVMDIBuilderCreateUnionType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, LLVMDIFlags, Ptr{LLVMMetadataRef}, Cuint, Cuint, Cstring, Csize_t), Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, Elements, NumElements, RunTimeLang, UniqueId, UniqueIdLen)
end

"""
    LLVMDIBuilderCreateArrayType(Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)

Create debugging information entry for an array.

# Arguments
* `Builder`: The DIBuilder.
* `Size`: Array size.
* `AlignInBits`: Alignment.
* `Ty`: Element type.
* `Subscripts`: Subscripts.
* `NumSubscripts`: Number of subscripts.
"""
function LLVMDIBuilderCreateArrayType(Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)
    ccall((:LLVMDIBuilderCreateArrayType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, UInt64, UInt32, LLVMMetadataRef, Ptr{LLVMMetadataRef}, Cuint), Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)
end

"""
    LLVMDIBuilderCreateVectorType(Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)

Create debugging information entry for a vector type.

# Arguments
* `Builder`: The DIBuilder.
* `Size`: Vector size.
* `AlignInBits`: Alignment.
* `Ty`: Element type.
* `Subscripts`: Subscripts.
* `NumSubscripts`: Number of subscripts.
"""
function LLVMDIBuilderCreateVectorType(Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)
    ccall((:LLVMDIBuilderCreateVectorType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, UInt64, UInt32, LLVMMetadataRef, Ptr{LLVMMetadataRef}, Cuint), Builder, Size, AlignInBits, Ty, Subscripts, NumSubscripts)
end

"""
    LLVMDIBuilderCreateUnspecifiedType(Builder, Name, NameLen)

Create a DWARF unspecified type.

# Arguments
* `Builder`: The DIBuilder.
* `Name`: The unspecified type's name.
* `NameLen`: Length of type name.
"""
function LLVMDIBuilderCreateUnspecifiedType(Builder, Name, NameLen)
    ccall((:LLVMDIBuilderCreateUnspecifiedType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t), Builder, Name, NameLen)
end

"""
    LLVMDIBuilderCreateBasicType(Builder, Name, NameLen, SizeInBits, Encoding, Flags)

Create debugging information entry for a basic type.

# Arguments
* `Builder`: The DIBuilder.
* `Name`: Type name.
* `NameLen`: Length of type name.
* `SizeInBits`: Size of the type.
* `Encoding`: DWARF encoding code, e.g. `LLVMDWARFTypeEncoding_float`.
* `Flags`: Flags to encode optional attribute like endianity
"""
function LLVMDIBuilderCreateBasicType(Builder, Name, NameLen, SizeInBits, Encoding, Flags)
    ccall((:LLVMDIBuilderCreateBasicType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t, UInt64, LLVMDWARFTypeEncoding, LLVMDIFlags), Builder, Name, NameLen, SizeInBits, Encoding, Flags)
end

"""
    LLVMDIBuilderCreatePointerType(Builder, PointeeTy, SizeInBits, AlignInBits, AddressSpace, Name, NameLen)

Create debugging information entry for a pointer.

# Arguments
* `Builder`: The DIBuilder.
* `PointeeTy`: Type pointed by this pointer.
* `SizeInBits`: Size.
* `AlignInBits`: Alignment. (optional, pass 0 to ignore)
* `AddressSpace`: DWARF address space. (optional, pass 0 to ignore)
* `Name`: Pointer type name. (optional)
* `NameLen`: Length of pointer type name. (optional)
"""
function LLVMDIBuilderCreatePointerType(Builder, PointeeTy, SizeInBits, AlignInBits, AddressSpace, Name, NameLen)
    ccall((:LLVMDIBuilderCreatePointerType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, UInt64, UInt32, Cuint, Cstring, Csize_t), Builder, PointeeTy, SizeInBits, AlignInBits, AddressSpace, Name, NameLen)
end

"""
    LLVMDIBuilderCreateStructType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, DerivedFrom, Elements, NumElements, RunTimeLang, VTableHolder, UniqueId, UniqueIdLen)

Create debugging information entry for a struct.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Scope in which this struct is defined.
* `Name`: Struct name.
* `NameLen`: Struct name length.
* `File`: File where this member is defined.
* `LineNumber`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `Flags`: Flags to encode member attribute, e.g. private
* `Elements`: Struct elements.
* `NumElements`: Number of struct elements.
* `RunTimeLang`: Optional parameter, Objective-C runtime version.
* `VTableHolder`: The object containing the vtable for the struct.
* `UniqueId`: A unique identifier for the struct.
* `UniqueIdLen`: Length of the unique identifier for the struct.
"""
function LLVMDIBuilderCreateStructType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, DerivedFrom, Elements, NumElements, RunTimeLang, VTableHolder, UniqueId, UniqueIdLen)
    ccall((:LLVMDIBuilderCreateStructType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, LLVMDIFlags, LLVMMetadataRef, Ptr{LLVMMetadataRef}, Cuint, Cuint, LLVMMetadataRef, Cstring, Csize_t), Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, Flags, DerivedFrom, Elements, NumElements, RunTimeLang, VTableHolder, UniqueId, UniqueIdLen)
end

"""
    LLVMDIBuilderCreateMemberType(Builder, Scope, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty)

Create debugging information entry for a member.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Member scope.
* `Name`: Member name.
* `NameLen`: Length of member name.
* `File`: File where this member is defined.
* `LineNo`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `OffsetInBits`: Member offset.
* `Flags`: Flags to encode member attribute, e.g. private
* `Ty`: Parent type.
"""
function LLVMDIBuilderCreateMemberType(Builder, Scope, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty)
    ccall((:LLVMDIBuilderCreateMemberType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, UInt64, LLVMDIFlags, LLVMMetadataRef), Builder, Scope, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty)
end

"""
    LLVMDIBuilderCreateStaticMemberType(Builder, Scope, Name, NameLen, File, LineNumber, Type, Flags, ConstantVal, AlignInBits)

Create debugging information entry for a C++ static data member.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Member scope.
* `Name`: Member name.
* `NameLen`: Length of member name.
* `File`: File where this member is declared.
* `LineNumber`: Line number.
* `Type`: Type of the static member.
* `Flags`: Flags to encode member attribute, e.g. private.
* `ConstantVal`: Const initializer of the member.
* `AlignInBits`: Member alignment.
"""
function LLVMDIBuilderCreateStaticMemberType(Builder, Scope, Name, NameLen, File, LineNumber, Type, Flags, ConstantVal, AlignInBits)
    ccall((:LLVMDIBuilderCreateStaticMemberType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMDIFlags, LLVMValueRef, UInt32), Builder, Scope, Name, NameLen, File, LineNumber, Type, Flags, ConstantVal, AlignInBits)
end

"""
    LLVMDIBuilderCreateMemberPointerType(Builder, PointeeType, ClassType, SizeInBits, AlignInBits, Flags)

Create debugging information entry for a pointer to member.

# Arguments
* `Builder`: The DIBuilder.
* `PointeeType`: Type pointed to by this pointer.
* `ClassType`: Type for which this pointer points to members of.
* `SizeInBits`: Size.
* `AlignInBits`: Alignment.
* `Flags`: Flags.
"""
function LLVMDIBuilderCreateMemberPointerType(Builder, PointeeType, ClassType, SizeInBits, AlignInBits, Flags)
    ccall((:LLVMDIBuilderCreateMemberPointerType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, UInt64, UInt32, LLVMDIFlags), Builder, PointeeType, ClassType, SizeInBits, AlignInBits, Flags)
end

"""
    LLVMDIBuilderCreateObjCIVar(Builder, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty, PropertyNode)

Create debugging information entry for Objective-C instance variable.

# Arguments
* `Builder`: The DIBuilder.
* `Name`: Member name.
* `NameLen`: The length of the C string passed to `Name`.
* `File`: File where this member is defined.
* `LineNo`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `OffsetInBits`: Member offset.
* `Flags`: Flags to encode member attribute, e.g. private
* `Ty`: Parent type.
* `PropertyNode`: Property associated with this ivar.
"""
function LLVMDIBuilderCreateObjCIVar(Builder, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty, PropertyNode)
    ccall((:LLVMDIBuilderCreateObjCIVar, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, UInt64, LLVMDIFlags, LLVMMetadataRef, LLVMMetadataRef), Builder, Name, NameLen, File, LineNo, SizeInBits, AlignInBits, OffsetInBits, Flags, Ty, PropertyNode)
end

"""
    LLVMDIBuilderCreateObjCProperty(Builder, Name, NameLen, File, LineNo, GetterName, GetterNameLen, SetterName, SetterNameLen, PropertyAttributes, Ty)

Create debugging information entry for Objective-C property.

# Arguments
* `Builder`: The DIBuilder.
* `Name`: Property name.
* `NameLen`: The length of the C string passed to `Name`.
* `File`: File where this property is defined.
* `LineNo`: Line number.
* `GetterName`: Name of the Objective C property getter selector.
* `GetterNameLen`: The length of the C string passed to `GetterName`.
* `SetterName`: Name of the Objective C property setter selector.
* `SetterNameLen`: The length of the C string passed to `SetterName`.
* `PropertyAttributes`: Objective C property attributes.
* `Ty`: Type.
"""
function LLVMDIBuilderCreateObjCProperty(Builder, Name, NameLen, File, LineNo, GetterName, GetterNameLen, SetterName, SetterNameLen, PropertyAttributes, Ty)
    ccall((:LLVMDIBuilderCreateObjCProperty, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, Cstring, Csize_t, Cstring, Csize_t, Cuint, LLVMMetadataRef), Builder, Name, NameLen, File, LineNo, GetterName, GetterNameLen, SetterName, SetterNameLen, PropertyAttributes, Ty)
end

"""
    LLVMDIBuilderCreateObjectPointerType(Builder, Type)

Create a uniqued DIType* clone with FlagObjectPointer and FlagArtificial set.

# Arguments
* `Builder`: The DIBuilder.
* `Type`: The underlying type to which this pointer points.
"""
function LLVMDIBuilderCreateObjectPointerType(Builder, Type)
    ccall((:LLVMDIBuilderCreateObjectPointerType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef), Builder, Type)
end

"""
    LLVMDIBuilderCreateQualifiedType(Builder, Tag, Type)

Create debugging information entry for a qualified type, e.g. 'const int'.

# Arguments
* `Builder`: The DIBuilder.
* `Tag`: Tag identifying type, e.g. LLVMDWARFTypeQualifier\\_volatile\\_type
* `Type`: Base Type.
"""
function LLVMDIBuilderCreateQualifiedType(Builder, Tag, Type)
    ccall((:LLVMDIBuilderCreateQualifiedType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cuint, LLVMMetadataRef), Builder, Tag, Type)
end

"""
    LLVMDIBuilderCreateReferenceType(Builder, Tag, Type)

Create debugging information entry for a c++ style reference or rvalue reference type.

# Arguments
* `Builder`: The DIBuilder.
* `Tag`: Tag identifying type,
* `Type`: Base Type.
"""
function LLVMDIBuilderCreateReferenceType(Builder, Tag, Type)
    ccall((:LLVMDIBuilderCreateReferenceType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cuint, LLVMMetadataRef), Builder, Tag, Type)
end

"""
    LLVMDIBuilderCreateNullPtrType(Builder)

Create C++11 nullptr type.

# Arguments
* `Builder`: The DIBuilder.
"""
function LLVMDIBuilderCreateNullPtrType(Builder)
    ccall((:LLVMDIBuilderCreateNullPtrType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef,), Builder)
end

"""
    LLVMDIBuilderCreateTypedef(Builder, Type, Name, NameLen, File, LineNo, Scope, AlignInBits)

Create debugging information entry for a typedef.

# Arguments
* `Builder`: The DIBuilder.
* `Type`: Original type.
* `Name`: Typedef name.
* `File`: File where this type is defined.
* `LineNo`: Line number.
* `Scope`: The surrounding context for the typedef.
"""
function LLVMDIBuilderCreateTypedef(Builder, Type, Name, NameLen, File, LineNo, Scope, AlignInBits)
    ccall((:LLVMDIBuilderCreateTypedef, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, UInt32), Builder, Type, Name, NameLen, File, LineNo, Scope, AlignInBits)
end

"""
    LLVMDIBuilderCreateInheritance(Builder, Ty, BaseTy, BaseOffset, VBPtrOffset, Flags)

Create debugging information entry to establish inheritance relationship between two types.

# Arguments
* `Builder`: The DIBuilder.
* `Ty`: Original type.
* `BaseTy`: Base type. Ty is inherits from base.
* `BaseOffset`: Base offset.
* `VBPtrOffset`: Virtual base pointer offset.
* `Flags`: Flags to describe inheritance attribute, e.g. private
"""
function LLVMDIBuilderCreateInheritance(Builder, Ty, BaseTy, BaseOffset, VBPtrOffset, Flags)
    ccall((:LLVMDIBuilderCreateInheritance, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, LLVMMetadataRef, UInt64, UInt32, LLVMDIFlags), Builder, Ty, BaseTy, BaseOffset, VBPtrOffset, Flags)
end

"""
    LLVMDIBuilderCreateForwardDecl(Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, UniqueIdentifier, UniqueIdentifierLen)

Create a permanent forward-declared type.

# Arguments
* `Builder`: The DIBuilder.
* `Tag`: A unique tag for this type.
* `Name`: Type name.
* `NameLen`: Length of type name.
* `Scope`: Type scope.
* `File`: File where this type is defined.
* `Line`: Line number where this type is defined.
* `RuntimeLang`: Indicates runtime version for languages like Objective-C.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `UniqueIdentifier`: A unique identifier for the type.
* `UniqueIdentifierLen`: Length of the unique identifier.
"""
function LLVMDIBuilderCreateForwardDecl(Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, UniqueIdentifier, UniqueIdentifierLen)
    ccall((:LLVMDIBuilderCreateForwardDecl, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cuint, Cstring, Csize_t, LLVMMetadataRef, LLVMMetadataRef, Cuint, Cuint, UInt64, UInt32, Cstring, Csize_t), Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, UniqueIdentifier, UniqueIdentifierLen)
end

"""
    LLVMDIBuilderCreateReplaceableCompositeType(Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, Flags, UniqueIdentifier, UniqueIdentifierLen)

Create a temporary forward-declared type.

# Arguments
* `Builder`: The DIBuilder.
* `Tag`: A unique tag for this type.
* `Name`: Type name.
* `NameLen`: Length of type name.
* `Scope`: Type scope.
* `File`: File where this type is defined.
* `Line`: Line number where this type is defined.
* `RuntimeLang`: Indicates runtime version for languages like Objective-C.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `Flags`: Flags.
* `UniqueIdentifier`: A unique identifier for the type.
* `UniqueIdentifierLen`: Length of the unique identifier.
"""
function LLVMDIBuilderCreateReplaceableCompositeType(Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, Flags, UniqueIdentifier, UniqueIdentifierLen)
    ccall((:LLVMDIBuilderCreateReplaceableCompositeType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Cuint, Cstring, Csize_t, LLVMMetadataRef, LLVMMetadataRef, Cuint, Cuint, UInt64, UInt32, LLVMDIFlags, Cstring, Csize_t), Builder, Tag, Name, NameLen, Scope, File, Line, RuntimeLang, SizeInBits, AlignInBits, Flags, UniqueIdentifier, UniqueIdentifierLen)
end

"""
    LLVMDIBuilderCreateBitFieldMemberType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, OffsetInBits, StorageOffsetInBits, Flags, Type)

Create debugging information entry for a bit field member.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: Member scope.
* `Name`: Member name.
* `NameLen`: Length of member name.
* `File`: File where this member is defined.
* `LineNumber`: Line number.
* `SizeInBits`: Member size.
* `OffsetInBits`: Member offset.
* `StorageOffsetInBits`: Member storage offset.
* `Flags`: Flags to encode member attribute.
* `Type`: Parent type.
"""
function LLVMDIBuilderCreateBitFieldMemberType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, OffsetInBits, StorageOffsetInBits, Flags, Type)
    ccall((:LLVMDIBuilderCreateBitFieldMemberType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt64, UInt64, LLVMDIFlags, LLVMMetadataRef), Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, OffsetInBits, StorageOffsetInBits, Flags, Type)
end

"""
    LLVMDIBuilderCreateClassType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, OffsetInBits, Flags, DerivedFrom, Elements, NumElements, VTableHolder, TemplateParamsNode, UniqueIdentifier, UniqueIdentifierLen)

Create debugging information entry for a class.

# Arguments
* `Scope`: Scope in which this class is defined.
* `Name`: Class name.
* `NameLen`: The length of the C string passed to `Name`.
* `File`: File where this member is defined.
* `LineNumber`: Line number.
* `SizeInBits`: Member size.
* `AlignInBits`: Member alignment.
* `OffsetInBits`: Member offset.
* `Flags`: Flags to encode member attribute, e.g. private.
* `DerivedFrom`: Debug info of the base class of this type.
* `Elements`: Class members.
* `NumElements`: Number of class elements.
* `VTableHolder`: Debug info of the base class that contains vtable for this type. This is used in DW\\_AT\\_containing\\_type. See DWARF documentation for more info.
* `TemplateParamsNode`: Template type parameters.
* `UniqueIdentifier`: A unique identifier for the type.
* `UniqueIdentifierLen`: Length of the unique identifier.
"""
function LLVMDIBuilderCreateClassType(Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, OffsetInBits, Flags, DerivedFrom, Elements, NumElements, VTableHolder, TemplateParamsNode, UniqueIdentifier, UniqueIdentifierLen)
    ccall((:LLVMDIBuilderCreateClassType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, UInt64, UInt32, UInt64, LLVMDIFlags, LLVMMetadataRef, Ptr{LLVMMetadataRef}, Cuint, LLVMMetadataRef, LLVMMetadataRef, Cstring, Csize_t), Builder, Scope, Name, NameLen, File, LineNumber, SizeInBits, AlignInBits, OffsetInBits, Flags, DerivedFrom, Elements, NumElements, VTableHolder, TemplateParamsNode, UniqueIdentifier, UniqueIdentifierLen)
end

"""
    LLVMDIBuilderCreateArtificialType(Builder, Type)

Create a uniqued DIType* clone with FlagArtificial set.

# Arguments
* `Builder`: The DIBuilder.
* `Type`: The underlying type.
"""
function LLVMDIBuilderCreateArtificialType(Builder, Type)
    ccall((:LLVMDIBuilderCreateArtificialType, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef), Builder, Type)
end

"""
    LLVMDITypeGetName(DType, Length)

Get the name of this DIType.

# Arguments
* `DType`: The DIType.
* `Length`: The length of the returned string.
# See also
DIType::getName()
"""
function LLVMDITypeGetName(DType, Length)
    ccall((:LLVMDITypeGetName, libllvm), Cstring, (LLVMMetadataRef, Ptr{Csize_t}), DType, Length)
end

"""
    LLVMDITypeGetSizeInBits(DType)

Get the size of this DIType in bits.

# Arguments
* `DType`: The DIType.
# See also
DIType::getSizeInBits()
"""
function LLVMDITypeGetSizeInBits(DType)
    ccall((:LLVMDITypeGetSizeInBits, libllvm), UInt64, (LLVMMetadataRef,), DType)
end

"""
    LLVMDITypeGetOffsetInBits(DType)

Get the offset of this DIType in bits.

# Arguments
* `DType`: The DIType.
# See also
DIType::getOffsetInBits()
"""
function LLVMDITypeGetOffsetInBits(DType)
    ccall((:LLVMDITypeGetOffsetInBits, libllvm), UInt64, (LLVMMetadataRef,), DType)
end

"""
    LLVMDITypeGetAlignInBits(DType)

Get the alignment of this DIType in bits.

# Arguments
* `DType`: The DIType.
# See also
DIType::getAlignInBits()
"""
function LLVMDITypeGetAlignInBits(DType)
    ccall((:LLVMDITypeGetAlignInBits, libllvm), UInt32, (LLVMMetadataRef,), DType)
end

"""
    LLVMDITypeGetLine(DType)

Get the source line where this DIType is declared.

# Arguments
* `DType`: The DIType.
# See also
DIType::getLine()
"""
function LLVMDITypeGetLine(DType)
    ccall((:LLVMDITypeGetLine, libllvm), Cuint, (LLVMMetadataRef,), DType)
end

"""
    LLVMDITypeGetFlags(DType)

Get the flags associated with this DIType.

# Arguments
* `DType`: The DIType.
# See also
DIType::getFlags()
"""
function LLVMDITypeGetFlags(DType)
    ccall((:LLVMDITypeGetFlags, libllvm), LLVMDIFlags, (LLVMMetadataRef,), DType)
end

"""
    LLVMDIBuilderGetOrCreateSubrange(Builder, LowerBound, Count)

Create a descriptor for a value range.

# Arguments
* `Builder`: The DIBuilder.
* `LowerBound`: Lower bound of the subrange, e.g. 0 for C, 1 for Fortran.
* `Count`: Count of elements in the subrange.
"""
function LLVMDIBuilderGetOrCreateSubrange(Builder, LowerBound, Count)
    ccall((:LLVMDIBuilderGetOrCreateSubrange, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Int64, Int64), Builder, LowerBound, Count)
end

"""
    LLVMDIBuilderGetOrCreateArray(Builder, Data, NumElements)

Create an array of DI Nodes.

# Arguments
* `Builder`: The DIBuilder.
* `Data`: The DI Node elements.
* `NumElements`: Number of DI Node elements.
"""
function LLVMDIBuilderGetOrCreateArray(Builder, Data, NumElements)
    ccall((:LLVMDIBuilderGetOrCreateArray, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Ptr{LLVMMetadataRef}, Csize_t), Builder, Data, NumElements)
end

"""
    LLVMDIBuilderCreateExpression(Builder, Addr, Length)

Create a new descriptor for the specified variable which has a complex address expression for its address.

# Arguments
* `Builder`: The DIBuilder.
* `Addr`: An array of complex address operations.
* `Length`: Length of the address operation array.
"""
function LLVMDIBuilderCreateExpression(Builder, Addr, Length)
    ccall((:LLVMDIBuilderCreateExpression, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, Ptr{UInt64}, Csize_t), Builder, Addr, Length)
end

"""
    LLVMDIBuilderCreateConstantValueExpression(Builder, Value)

Create a new descriptor for the specified variable that does not have an address, but does have a constant value.

# Arguments
* `Builder`: The DIBuilder.
* `Value`: The constant value.
"""
function LLVMDIBuilderCreateConstantValueExpression(Builder, Value)
    ccall((:LLVMDIBuilderCreateConstantValueExpression, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, UInt64), Builder, Value)
end

"""
    LLVMDIBuilderCreateGlobalVariableExpression(Builder, Scope, Name, NameLen, Linkage, LinkLen, File, LineNo, Ty, LocalToUnit, Expr, Decl, AlignInBits)

Create a new descriptor for the specified variable.

# Arguments
* `Scope`: Variable scope.
* `Name`: Name of the variable.
* `NameLen`: The length of the C string passed to `Name`.
* `Linkage`: Mangled name of the variable.
* `LinkLen`: The length of the C string passed to `Linkage`.
* `File`: File where this variable is defined.
* `LineNo`: Line number.
* `Ty`: Variable Type.
* `LocalToUnit`: Boolean flag indicate whether this variable is externally visible or not.
* `Expr`: The location of the global relative to the attached GlobalVariable.
* `Decl`: Reference to the corresponding declaration. variables.
* `AlignInBits`: Variable alignment(or 0 if no alignment attr was specified)
"""
function LLVMDIBuilderCreateGlobalVariableExpression(Builder, Scope, Name, NameLen, Linkage, LinkLen, File, LineNo, Ty, LocalToUnit, Expr, Decl, AlignInBits)
    ccall((:LLVMDIBuilderCreateGlobalVariableExpression, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMBool, LLVMMetadataRef, LLVMMetadataRef, UInt32), Builder, Scope, Name, NameLen, Linkage, LinkLen, File, LineNo, Ty, LocalToUnit, Expr, Decl, AlignInBits)
end

"""
    LLVMDIGlobalVariableExpressionGetVariable(GVE)

Retrieves the `DIVariable` associated with this global variable expression.

# Arguments
* `GVE`: The global variable expression.
# See also
llvm::DIGlobalVariableExpression::getVariable()
"""
function LLVMDIGlobalVariableExpressionGetVariable(GVE)
    ccall((:LLVMDIGlobalVariableExpressionGetVariable, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), GVE)
end

"""
    LLVMDIGlobalVariableExpressionGetExpression(GVE)

Retrieves the `DIExpression` associated with this global variable expression.

# Arguments
* `GVE`: The global variable expression.
# See also
llvm::DIGlobalVariableExpression::getExpression()
"""
function LLVMDIGlobalVariableExpressionGetExpression(GVE)
    ccall((:LLVMDIGlobalVariableExpressionGetExpression, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), GVE)
end

"""
    LLVMDIVariableGetFile(Var)

Get the metadata of the file associated with a given variable.

# Arguments
* `Var`: The variable object.
# See also
DIVariable::getFile()
"""
function LLVMDIVariableGetFile(Var)
    ccall((:LLVMDIVariableGetFile, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), Var)
end

"""
    LLVMDIVariableGetScope(Var)

Get the metadata of the scope associated with a given variable.

# Arguments
* `Var`: The variable object.
# See also
DIVariable::getScope()
"""
function LLVMDIVariableGetScope(Var)
    ccall((:LLVMDIVariableGetScope, libllvm), LLVMMetadataRef, (LLVMMetadataRef,), Var)
end

"""
    LLVMDIVariableGetLine(Var)

Get the source line where this `DIVariable` is declared.

# Arguments
* `Var`: The DIVariable.
# See also
DIVariable::getLine()
"""
function LLVMDIVariableGetLine(Var)
    ccall((:LLVMDIVariableGetLine, libllvm), Cuint, (LLVMMetadataRef,), Var)
end

"""
    LLVMTemporaryMDNode(Ctx, Data, NumElements)

Create a new temporary `MDNode`. Suitable for use in constructing cyclic `MDNode` structures. A temporary `MDNode` is not uniqued, may be RAUW'd, and must be manually deleted with [`LLVMDisposeTemporaryMDNode`](@ref).

# Arguments
* `Ctx`: The context in which to construct the temporary node.
* `Data`: The metadata elements.
* `NumElements`: Number of metadata elements.
"""
function LLVMTemporaryMDNode(Ctx, Data, NumElements)
    ccall((:LLVMTemporaryMDNode, libllvm), LLVMMetadataRef, (LLVMContextRef, Ptr{LLVMMetadataRef}, Csize_t), Ctx, Data, NumElements)
end

"""
    LLVMDisposeTemporaryMDNode(TempNode)

Deallocate a temporary node.

Calls `replaceAllUsesWith`(nullptr) before deleting, so any remaining references will be reset.

# Arguments
* `TempNode`: The temporary metadata node.
"""
function LLVMDisposeTemporaryMDNode(TempNode)
    ccall((:LLVMDisposeTemporaryMDNode, libllvm), Cvoid, (LLVMMetadataRef,), TempNode)
end

"""
    LLVMMetadataReplaceAllUsesWith(TempTargetMetadata, Replacement)

Replace all uses of temporary metadata.

# Arguments
* `TempTargetMetadata`: The temporary metadata node.
* `Replacement`: The replacement metadata node.
"""
function LLVMMetadataReplaceAllUsesWith(TempTargetMetadata, Replacement)
    ccall((:LLVMMetadataReplaceAllUsesWith, libllvm), Cvoid, (LLVMMetadataRef, LLVMMetadataRef), TempTargetMetadata, Replacement)
end

"""
    LLVMDIBuilderCreateTempGlobalVariableFwdDecl(Builder, Scope, Name, NameLen, Linkage, LnkLen, File, LineNo, Ty, LocalToUnit, Decl, AlignInBits)

Create a new descriptor for the specified global variable that is temporary and meant to be RAUWed.

# Arguments
* `Scope`: Variable scope.
* `Name`: Name of the variable.
* `NameLen`: The length of the C string passed to `Name`.
* `Linkage`: Mangled name of the variable.
* `LnkLen`: The length of the C string passed to `Linkage`.
* `File`: File where this variable is defined.
* `LineNo`: Line number.
* `Ty`: Variable Type.
* `LocalToUnit`: Boolean flag indicate whether this variable is externally visible or not.
* `Decl`: Reference to the corresponding declaration.
* `AlignInBits`: Variable alignment(or 0 if no alignment attr was specified)
"""
function LLVMDIBuilderCreateTempGlobalVariableFwdDecl(Builder, Scope, Name, NameLen, Linkage, LnkLen, File, LineNo, Ty, LocalToUnit, Decl, AlignInBits)
    ccall((:LLVMDIBuilderCreateTempGlobalVariableFwdDecl, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMBool, LLVMMetadataRef, UInt32), Builder, Scope, Name, NameLen, Linkage, LnkLen, File, LineNo, Ty, LocalToUnit, Decl, AlignInBits)
end

"""
    LLVMDIBuilderInsertDeclareBefore(Builder, Storage, VarInfo, Expr, DebugLoc, Instr)

Insert a new llvm.dbg.declare intrinsic call before the given instruction.

# Arguments
* `Builder`: The DIBuilder.
* `Storage`: The storage of the variable to declare.
* `VarInfo`: The variable's debug info descriptor.
* `Expr`: A complex location expression for the variable.
* `DebugLoc`: Debug info location.
* `Instr`: Instruction acting as a location for the new intrinsic.
"""
function LLVMDIBuilderInsertDeclareBefore(Builder, Storage, VarInfo, Expr, DebugLoc, Instr)
    ccall((:LLVMDIBuilderInsertDeclareBefore, libllvm), LLVMValueRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMValueRef), Builder, Storage, VarInfo, Expr, DebugLoc, Instr)
end

"""
    LLVMDIBuilderInsertDeclareAtEnd(Builder, Storage, VarInfo, Expr, DebugLoc, Block)

Insert a new llvm.dbg.declare intrinsic call at the end of the given basic block. If the basic block has a terminator instruction, the intrinsic is inserted before that terminator instruction.

# Arguments
* `Builder`: The DIBuilder.
* `Storage`: The storage of the variable to declare.
* `VarInfo`: The variable's debug info descriptor.
* `Expr`: A complex location expression for the variable.
* `DebugLoc`: Debug info location.
* `Block`: Basic block acting as a location for the new intrinsic.
"""
function LLVMDIBuilderInsertDeclareAtEnd(Builder, Storage, VarInfo, Expr, DebugLoc, Block)
    ccall((:LLVMDIBuilderInsertDeclareAtEnd, libllvm), LLVMValueRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMBasicBlockRef), Builder, Storage, VarInfo, Expr, DebugLoc, Block)
end

"""
    LLVMDIBuilderInsertDbgValueBefore(Builder, Val, VarInfo, Expr, DebugLoc, Instr)

Insert a new llvm.dbg.value intrinsic call before the given instruction.

# Arguments
* `Builder`: The DIBuilder.
* `Val`: The value of the variable.
* `VarInfo`: The variable's debug info descriptor.
* `Expr`: A complex location expression for the variable.
* `DebugLoc`: Debug info location.
* `Instr`: Instruction acting as a location for the new intrinsic.
"""
function LLVMDIBuilderInsertDbgValueBefore(Builder, Val, VarInfo, Expr, DebugLoc, Instr)
    ccall((:LLVMDIBuilderInsertDbgValueBefore, libllvm), LLVMValueRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMValueRef), Builder, Val, VarInfo, Expr, DebugLoc, Instr)
end

"""
    LLVMDIBuilderInsertDbgValueAtEnd(Builder, Val, VarInfo, Expr, DebugLoc, Block)

Insert a new llvm.dbg.value intrinsic call at the end of the given basic block. If the basic block has a terminator instruction, the intrinsic is inserted before that terminator instruction.

# Arguments
* `Builder`: The DIBuilder.
* `Val`: The value of the variable.
* `VarInfo`: The variable's debug info descriptor.
* `Expr`: A complex location expression for the variable.
* `DebugLoc`: Debug info location.
* `Block`: Basic block acting as a location for the new intrinsic.
"""
function LLVMDIBuilderInsertDbgValueAtEnd(Builder, Val, VarInfo, Expr, DebugLoc, Block)
    ccall((:LLVMDIBuilderInsertDbgValueAtEnd, libllvm), LLVMValueRef, (LLVMDIBuilderRef, LLVMValueRef, LLVMMetadataRef, LLVMMetadataRef, LLVMMetadataRef, LLVMBasicBlockRef), Builder, Val, VarInfo, Expr, DebugLoc, Block)
end

"""
    LLVMDIBuilderCreateAutoVariable(Builder, Scope, Name, NameLen, File, LineNo, Ty, AlwaysPreserve, Flags, AlignInBits)

Create a new descriptor for a local auto variable.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: The local scope the variable is declared in.
* `Name`: Variable name.
* `NameLen`: Length of variable name.
* `File`: File where this variable is defined.
* `LineNo`: Line number.
* `Ty`: Metadata describing the type of the variable.
* `AlwaysPreserve`: If true, this descriptor will survive optimizations.
* `Flags`: Flags.
* `AlignInBits`: Variable alignment.
"""
function LLVMDIBuilderCreateAutoVariable(Builder, Scope, Name, NameLen, File, LineNo, Ty, AlwaysPreserve, Flags, AlignInBits)
    ccall((:LLVMDIBuilderCreateAutoVariable, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMBool, LLVMDIFlags, UInt32), Builder, Scope, Name, NameLen, File, LineNo, Ty, AlwaysPreserve, Flags, AlignInBits)
end

"""
    LLVMDIBuilderCreateParameterVariable(Builder, Scope, Name, NameLen, ArgNo, File, LineNo, Ty, AlwaysPreserve, Flags)

Create a new descriptor for a function parameter variable.

# Arguments
* `Builder`: The DIBuilder.
* `Scope`: The local scope the variable is declared in.
* `Name`: Variable name.
* `NameLen`: Length of variable name.
* `ArgNo`: Unique argument number for this variable; starts at 1.
* `File`: File where this variable is defined.
* `LineNo`: Line number.
* `Ty`: Metadata describing the type of the variable.
* `AlwaysPreserve`: If true, this descriptor will survive optimizations.
* `Flags`: Flags.
"""
function LLVMDIBuilderCreateParameterVariable(Builder, Scope, Name, NameLen, ArgNo, File, LineNo, Ty, AlwaysPreserve, Flags)
    ccall((:LLVMDIBuilderCreateParameterVariable, libllvm), LLVMMetadataRef, (LLVMDIBuilderRef, LLVMMetadataRef, Cstring, Csize_t, Cuint, LLVMMetadataRef, Cuint, LLVMMetadataRef, LLVMBool, LLVMDIFlags), Builder, Scope, Name, NameLen, ArgNo, File, LineNo, Ty, AlwaysPreserve, Flags)
end

"""
    LLVMGetSubprogram(Func)

Get the metadata of the subprogram attached to a function.

# See also
llvm::Function::getSubprogram()
"""
function LLVMGetSubprogram(Func)
    ccall((:LLVMGetSubprogram, libllvm), LLVMMetadataRef, (LLVMValueRef,), Func)
end

"""
    LLVMSetSubprogram(Func, SP)

Set the subprogram attached to a function.

# See also
llvm::Function::setSubprogram()
"""
function LLVMSetSubprogram(Func, SP)
    ccall((:LLVMSetSubprogram, libllvm), Cvoid, (LLVMValueRef, LLVMMetadataRef), Func, SP)
end

"""
    LLVMDISubprogramGetLine(Subprogram)

Get the line associated with a given subprogram.

# Arguments
* `Subprogram`: The subprogram object.
# See also
DISubprogram::getLine()
"""
function LLVMDISubprogramGetLine(Subprogram)
    ccall((:LLVMDISubprogramGetLine, libllvm), Cuint, (LLVMMetadataRef,), Subprogram)
end

"""
    LLVMInstructionGetDebugLoc(Inst)

Get the debug location for the given instruction.

# See also
llvm::Instruction::getDebugLoc()
"""
function LLVMInstructionGetDebugLoc(Inst)
    ccall((:LLVMInstructionGetDebugLoc, libllvm), LLVMMetadataRef, (LLVMValueRef,), Inst)
end

"""
    LLVMInstructionSetDebugLoc(Inst, Loc)

Set the debug location for the given instruction.

To clear the location metadata of the given instruction, pass NULL to `Loc`.

# See also
llvm::Instruction::setDebugLoc()
"""
function LLVMInstructionSetDebugLoc(Inst, Loc)
    ccall((:LLVMInstructionSetDebugLoc, libllvm), Cvoid, (LLVMValueRef, LLVMMetadataRef), Inst, Loc)
end

"""
    LLVMGetMetadataKind(Metadata)

Obtain the enumerated type of a Metadata instance.

# See also
llvm::Metadata::getMetadataID()
"""
function LLVMGetMetadataKind(Metadata)
    ccall((:LLVMGetMetadataKind, libllvm), LLVMMetadataKind, (LLVMMetadataRef,), Metadata)
end

# typedef int ( * LLVMOpInfoCallback ) ( void * DisInfo , uint64_t PC , uint64_t Offset , uint64_t OpSize , uint64_t InstSize , int TagType , void * TagBuf )
"""
The type for the operand information call back function. This is called to get the symbolic information for an operand of an instruction. Typically this is from the relocation information, symbol table, etc. That block of information is saved when the disassembler context is created and passed to the call back in the DisInfo parameter. The instruction containing operand is at the PC parameter. For some instruction sets, there can be more than one operand with symbolic information. To determine the symbolic operand information for each operand, the bytes for the specific operand in the instruction are specified by the Offset parameter and its byte widith is the OpSize parameter. For instructions sets with fixed widths and one symbolic operand per instruction, the Offset parameter will be zero and InstSize parameter will be the instruction width. The information is returned in TagBuf and is Triple specific with its specific information defined by the value of TagType for that Triple. If symbolic information is returned the function * returns 1, otherwise it returns 0.
"""
const LLVMOpInfoCallback = Ptr{Cvoid}

# typedef const char * ( * LLVMSymbolLookupCallback ) ( void * DisInfo , uint64_t ReferenceValue , uint64_t * ReferenceType , uint64_t ReferencePC , const char * * ReferenceName )
"""
The type for the symbol lookup function. This may be called by the disassembler for things like adding a comment for a PC plus a constant offset load instruction to use a symbol name instead of a load address value. It is passed the block information is saved when the disassembler context is created and the ReferenceValue to look up as a symbol. If no symbol is found for the ReferenceValue NULL is returned. The ReferenceType of the instruction is passed indirectly as is the PC of the instruction in ReferencePC. If the output reference can be determined its type is returned indirectly in ReferenceType along with ReferenceName if any, or that is set to NULL.
"""
const LLVMSymbolLookupCallback = Ptr{Cvoid}

"""
An opaque reference to a disassembler context.
"""
const LLVMDisasmContextRef = Ptr{Cvoid}

"""
    LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)

Create a disassembler for the TripleName. Symbolic disassembly is supported by passing a block of information in the DisInfo parameter and specifying the TagType and callback functions as described above. These can all be passed as NULL. If successful, this returns a disassembler context. If not, it returns NULL. This function is equivalent to calling [`LLVMCreateDisasmCPUFeatures`](@ref)() with an empty CPU name and feature set.
"""
function LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)
    ccall((:LLVMCreateDisasm, libllvm), LLVMDisasmContextRef, (Cstring, Ptr{Cvoid}, Cint, LLVMOpInfoCallback, LLVMSymbolLookupCallback), TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)
end

"""
    LLVMCreateDisasmCPU(Triple, CPU, DisInfo, TagType, GetOpInfo, SymbolLookUp)

Create a disassembler for the TripleName and a specific CPU. Symbolic disassembly is supported by passing a block of information in the DisInfo parameter and specifying the TagType and callback functions as described above. These can all be passed * as NULL. If successful, this returns a disassembler context. If not, it returns NULL. This function is equivalent to calling [`LLVMCreateDisasmCPUFeatures`](@ref)() with an empty feature set.
"""
function LLVMCreateDisasmCPU(Triple, CPU, DisInfo, TagType, GetOpInfo, SymbolLookUp)
    ccall((:LLVMCreateDisasmCPU, libllvm), LLVMDisasmContextRef, (Cstring, Cstring, Ptr{Cvoid}, Cint, LLVMOpInfoCallback, LLVMSymbolLookupCallback), Triple, CPU, DisInfo, TagType, GetOpInfo, SymbolLookUp)
end

"""
    LLVMCreateDisasmCPUFeatures(Triple, CPU, Features, DisInfo, TagType, GetOpInfo, SymbolLookUp)

Create a disassembler for the TripleName, a specific CPU and specific feature string. Symbolic disassembly is supported by passing a block of information in the DisInfo parameter and specifying the TagType and callback functions as described above. These can all be passed * as NULL. If successful, this returns a disassembler context. If not, it returns NULL.
"""
function LLVMCreateDisasmCPUFeatures(Triple, CPU, Features, DisInfo, TagType, GetOpInfo, SymbolLookUp)
    ccall((:LLVMCreateDisasmCPUFeatures, libllvm), LLVMDisasmContextRef, (Cstring, Cstring, Cstring, Ptr{Cvoid}, Cint, LLVMOpInfoCallback, LLVMSymbolLookupCallback), Triple, CPU, Features, DisInfo, TagType, GetOpInfo, SymbolLookUp)
end

"""
    LLVMSetDisasmOptions(DC, Options)

Set the disassembler's options. Returns 1 if it can set the Options and 0 otherwise.
"""
function LLVMSetDisasmOptions(DC, Options)
    ccall((:LLVMSetDisasmOptions, libllvm), Cint, (LLVMDisasmContextRef, UInt64), DC, Options)
end

"""
    LLVMDisasmDispose(DC)

Dispose of a disassembler context.
"""
function LLVMDisasmDispose(DC)
    ccall((:LLVMDisasmDispose, libllvm), Cvoid, (LLVMDisasmContextRef,), DC)
end

"""
    LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize)

Disassemble a single instruction using the disassembler context specified in the parameter DC. The bytes of the instruction are specified in the parameter Bytes, and contains at least BytesSize number of bytes. The instruction is at the address specified by the PC parameter. If a valid instruction can be disassembled, its string is returned indirectly in OutString whose size is specified in the parameter OutStringSize. This function returns the number of bytes in the instruction or zero if there was no valid instruction.
"""
function LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize)
    ccall((:LLVMDisasmInstruction, libllvm), Csize_t, (LLVMDisasmContextRef, Ptr{UInt8}, UInt64, UInt64, Cstring, Csize_t), DC, Bytes, BytesSize, PC, OutString, OutStringSize)
end

"""
    LLVMOpInfoSymbol1

The initial support in LLVM MC for the most general form of a relocatable expression is "AddSymbol - SubtractSymbol + Offset". For some Darwin targets this full form is encoded in the relocation information so that AddSymbol and SubtractSymbol can be link edited independent of each other. Many other platforms only allow a relocatable expression of the form AddSymbol + Offset to be encoded.

The [`LLVMOpInfoCallback`](@ref)() for the TagType value of 1 uses the struct [`LLVMOpInfo1`](@ref). The value of the relocatable expression for the operand, including any PC adjustment, is passed in to the call back in the Value field. The symbolic information about the operand is returned using all the fields of the structure with the Offset of the relocatable expression returned in the Value field. It is possible that some symbols in the relocatable expression were assembly temporary symbols, for example "Ldata - LpicBase + constant", and only the Values of the symbols without symbol names are present in the relocation information. The VariantKind type is one of the Target specific #defines below and is used to print operands like "\\_foo@GOT", ":lower16:\\_foo", etc.
"""
struct LLVMOpInfoSymbol1
    Present::UInt64
    Name::Cstring
    Value::UInt64
end

struct LLVMOpInfo1
    AddSymbol::LLVMOpInfoSymbol1
    SubtractSymbol::LLVMOpInfoSymbol1
    Value::UInt64
    VariantKind::UInt64
end

"""
Opaque reference to an error instance. Null serves as the 'success' value.
"""
const LLVMErrorRef = Ptr{LLVMOpaqueError}

"""
Error type identifier.
"""
const LLVMErrorTypeId = Ptr{Cvoid}

"""
    LLVMGetErrorTypeId(Err)

Returns the type id for the given error instance, which must be a failure value (i.e. non-null).
"""
function LLVMGetErrorTypeId(Err)
    ccall((:LLVMGetErrorTypeId, libllvm), LLVMErrorTypeId, (LLVMErrorRef,), Err)
end

"""
    LLVMConsumeError(Err)

Dispose of the given error without handling it. This operation consumes the error, and the given [`LLVMErrorRef`](@ref) value is not usable once this call returns. Note: This method *only* needs to be called if the error is not being passed to some other consuming operation, e.g. [`LLVMGetErrorMessage`](@ref).
"""
function LLVMConsumeError(Err)
    ccall((:LLVMConsumeError, libllvm), Cvoid, (LLVMErrorRef,), Err)
end

"""
    LLVMGetErrorMessage(Err)

Returns the given string's error message. This operation consumes the error, and the given [`LLVMErrorRef`](@ref) value is not usable once this call returns. The caller is responsible for disposing of the string by calling [`LLVMDisposeErrorMessage`](@ref).
"""
function LLVMGetErrorMessage(Err)
    ccall((:LLVMGetErrorMessage, libllvm), Cstring, (LLVMErrorRef,), Err)
end

"""
    LLVMDisposeErrorMessage(ErrMsg)

Dispose of the given error message.
"""
function LLVMDisposeErrorMessage(ErrMsg)
    ccall((:LLVMDisposeErrorMessage, libllvm), Cvoid, (Cstring,), ErrMsg)
end

"""
    LLVMGetStringErrorTypeId()

Returns the type id for llvm StringError.
"""
function LLVMGetStringErrorTypeId()
    ccall((:LLVMGetStringErrorTypeId, libllvm), LLVMErrorTypeId, ())
end

"""
    LLVMCreateStringError(ErrMsg)

Create a StringError.
"""
function LLVMCreateStringError(ErrMsg)
    ccall((:LLVMCreateStringError, libllvm), LLVMErrorRef, (Cstring,), ErrMsg)
end

# typedef void ( * LLVMFatalErrorHandler ) ( const char * Reason )
"""
` LLVMCError`

@{
"""
const LLVMFatalErrorHandler = Ptr{Cvoid}

"""
    LLVMInstallFatalErrorHandler(Handler)

Install a fatal error handler. By default, if LLVM detects a fatal error, it will call exit(1). This may not be appropriate in many contexts. For example, doing exit(1) will bypass many crash reporting/tracing system tools. This function allows you to install a callback that will be invoked prior to the call to exit(1).
"""
function LLVMInstallFatalErrorHandler(Handler)
    ccall((:LLVMInstallFatalErrorHandler, libllvm), Cvoid, (LLVMFatalErrorHandler,), Handler)
end

"""
    LLVMResetFatalErrorHandler()

Reset the fatal error handler. This resets LLVM's fatal error handling behavior to the default.
"""
function LLVMResetFatalErrorHandler()
    ccall((:LLVMResetFatalErrorHandler, libllvm), Cvoid, ())
end

"""
    LLVMEnablePrettyStackTrace()

Enable LLVM's built-in stack trace code. This intercepts the OS's crash signals and prints which component of LLVM you were in at the time if the crash.
"""
function LLVMEnablePrettyStackTrace()
    ccall((:LLVMEnablePrettyStackTrace, libllvm), Cvoid, ())
end

"""
    LLVMLinkInMCJIT()

` LLVMCExecutionEngine Execution Engine`

` LLVMC`

@{
"""
function LLVMLinkInMCJIT()
    ccall((:LLVMLinkInMCJIT, libllvm), Cvoid, ())
end

function LLVMLinkInInterpreter()
    ccall((:LLVMLinkInInterpreter, libllvm), Cvoid, ())
end

const LLVMGenericValueRef = Ptr{LLVMOpaqueGenericValue}

const LLVMExecutionEngineRef = Ptr{LLVMOpaqueExecutionEngine}

const LLVMMCJITMemoryManagerRef = Ptr{LLVMOpaqueMCJITMemoryManager}

@cenum LLVMCodeModel::UInt32 begin
    LLVMCodeModelDefault = 0
    LLVMCodeModelJITDefault = 1
    LLVMCodeModelTiny = 2
    LLVMCodeModelSmall = 3
    LLVMCodeModelKernel = 4
    LLVMCodeModelMedium = 5
    LLVMCodeModelLarge = 6
end

struct LLVMMCJITCompilerOptions
    OptLevel::Cuint
    CodeModel::LLVMCodeModel
    NoFramePointerElim::LLVMBool
    EnableFastISel::LLVMBool
    MCJMM::LLVMMCJITMemoryManagerRef
end

function LLVMCreateGenericValueOfInt(Ty, N, IsSigned)
    ccall((:LLVMCreateGenericValueOfInt, libllvm), LLVMGenericValueRef, (LLVMTypeRef, Culonglong, LLVMBool), Ty, N, IsSigned)
end

function LLVMCreateGenericValueOfPointer(P)
    ccall((:LLVMCreateGenericValueOfPointer, libllvm), LLVMGenericValueRef, (Ptr{Cvoid},), P)
end

function LLVMCreateGenericValueOfFloat(Ty, N)
    ccall((:LLVMCreateGenericValueOfFloat, libllvm), LLVMGenericValueRef, (LLVMTypeRef, Cdouble), Ty, N)
end

function LLVMGenericValueIntWidth(GenValRef)
    ccall((:LLVMGenericValueIntWidth, libllvm), Cuint, (LLVMGenericValueRef,), GenValRef)
end

function LLVMGenericValueToInt(GenVal, IsSigned)
    ccall((:LLVMGenericValueToInt, libllvm), Culonglong, (LLVMGenericValueRef, LLVMBool), GenVal, IsSigned)
end

function LLVMGenericValueToPointer(GenVal)
    ccall((:LLVMGenericValueToPointer, libllvm), Ptr{Cvoid}, (LLVMGenericValueRef,), GenVal)
end

function LLVMGenericValueToFloat(TyRef, GenVal)
    ccall((:LLVMGenericValueToFloat, libllvm), Cdouble, (LLVMTypeRef, LLVMGenericValueRef), TyRef, GenVal)
end

function LLVMDisposeGenericValue(GenVal)
    ccall((:LLVMDisposeGenericValue, libllvm), Cvoid, (LLVMGenericValueRef,), GenVal)
end

function LLVMCreateExecutionEngineForModule(OutEE, M, OutError)
    ccall((:LLVMCreateExecutionEngineForModule, libllvm), LLVMBool, (Ptr{LLVMExecutionEngineRef}, LLVMModuleRef, Ptr{Cstring}), OutEE, M, OutError)
end

function LLVMCreateInterpreterForModule(OutInterp, M, OutError)
    ccall((:LLVMCreateInterpreterForModule, libllvm), LLVMBool, (Ptr{LLVMExecutionEngineRef}, LLVMModuleRef, Ptr{Cstring}), OutInterp, M, OutError)
end

function LLVMCreateJITCompilerForModule(OutJIT, M, OptLevel, OutError)
    ccall((:LLVMCreateJITCompilerForModule, libllvm), LLVMBool, (Ptr{LLVMExecutionEngineRef}, LLVMModuleRef, Cuint, Ptr{Cstring}), OutJIT, M, OptLevel, OutError)
end

function LLVMInitializeMCJITCompilerOptions(Options, SizeOfOptions)
    ccall((:LLVMInitializeMCJITCompilerOptions, libllvm), Cvoid, (Ptr{LLVMMCJITCompilerOptions}, Csize_t), Options, SizeOfOptions)
end

"""
    LLVMCreateMCJITCompilerForModule(OutJIT, M, Options, SizeOfOptions, OutError)

Create an MCJIT execution engine for a module, with the given options. It is the responsibility of the caller to ensure that all fields in Options up to the given SizeOfOptions are initialized. It is correct to pass a smaller value of SizeOfOptions that omits some fields. The canonical way of using this is:

[`LLVMMCJITCompilerOptions`](@ref) options; [`LLVMInitializeMCJITCompilerOptions`](@ref)(&options, sizeof(options)); ... fill in those options you care about [`LLVMCreateMCJITCompilerForModule`](@ref)(&jit, mod, &options, sizeof(options), &error);

Note that this is also correct, though possibly suboptimal:

[`LLVMCreateMCJITCompilerForModule`](@ref)(&jit, mod, 0, 0, &error);
"""
function LLVMCreateMCJITCompilerForModule(OutJIT, M, Options, SizeOfOptions, OutError)
    ccall((:LLVMCreateMCJITCompilerForModule, libllvm), LLVMBool, (Ptr{LLVMExecutionEngineRef}, LLVMModuleRef, Ptr{LLVMMCJITCompilerOptions}, Csize_t, Ptr{Cstring}), OutJIT, M, Options, SizeOfOptions, OutError)
end

function LLVMDisposeExecutionEngine(EE)
    ccall((:LLVMDisposeExecutionEngine, libllvm), Cvoid, (LLVMExecutionEngineRef,), EE)
end

function LLVMRunStaticConstructors(EE)
    ccall((:LLVMRunStaticConstructors, libllvm), Cvoid, (LLVMExecutionEngineRef,), EE)
end

function LLVMRunStaticDestructors(EE)
    ccall((:LLVMRunStaticDestructors, libllvm), Cvoid, (LLVMExecutionEngineRef,), EE)
end

function LLVMRunFunctionAsMain(EE, F, ArgC, ArgV, EnvP)
    ccall((:LLVMRunFunctionAsMain, libllvm), Cint, (LLVMExecutionEngineRef, LLVMValueRef, Cuint, Ptr{Cstring}, Ptr{Cstring}), EE, F, ArgC, ArgV, EnvP)
end

function LLVMRunFunction(EE, F, NumArgs, Args)
    ccall((:LLVMRunFunction, libllvm), LLVMGenericValueRef, (LLVMExecutionEngineRef, LLVMValueRef, Cuint, Ptr{LLVMGenericValueRef}), EE, F, NumArgs, Args)
end

function LLVMFreeMachineCodeForFunction(EE, F)
    ccall((:LLVMFreeMachineCodeForFunction, libllvm), Cvoid, (LLVMExecutionEngineRef, LLVMValueRef), EE, F)
end

function LLVMAddModule(EE, M)
    ccall((:LLVMAddModule, libllvm), Cvoid, (LLVMExecutionEngineRef, LLVMModuleRef), EE, M)
end

function LLVMRemoveModule(EE, M, OutMod, OutError)
    ccall((:LLVMRemoveModule, libllvm), LLVMBool, (LLVMExecutionEngineRef, LLVMModuleRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), EE, M, OutMod, OutError)
end

function LLVMFindFunction(EE, Name, OutFn)
    ccall((:LLVMFindFunction, libllvm), LLVMBool, (LLVMExecutionEngineRef, Cstring, Ptr{LLVMValueRef}), EE, Name, OutFn)
end

function LLVMRecompileAndRelinkFunction(EE, Fn)
    ccall((:LLVMRecompileAndRelinkFunction, libllvm), Ptr{Cvoid}, (LLVMExecutionEngineRef, LLVMValueRef), EE, Fn)
end

const LLVMTargetDataRef = Ptr{LLVMOpaqueTargetData}

function LLVMGetExecutionEngineTargetData(EE)
    ccall((:LLVMGetExecutionEngineTargetData, libllvm), LLVMTargetDataRef, (LLVMExecutionEngineRef,), EE)
end

"""
` LLVMCTarget`

@{
"""
const LLVMTargetMachineRef = Ptr{LLVMOpaqueTargetMachine}

function LLVMGetExecutionEngineTargetMachine(EE)
    ccall((:LLVMGetExecutionEngineTargetMachine, libllvm), LLVMTargetMachineRef, (LLVMExecutionEngineRef,), EE)
end

function LLVMAddGlobalMapping(EE, Global, Addr)
    ccall((:LLVMAddGlobalMapping, libllvm), Cvoid, (LLVMExecutionEngineRef, LLVMValueRef, Ptr{Cvoid}), EE, Global, Addr)
end

function LLVMGetPointerToGlobal(EE, Global)
    ccall((:LLVMGetPointerToGlobal, libllvm), Ptr{Cvoid}, (LLVMExecutionEngineRef, LLVMValueRef), EE, Global)
end

function LLVMGetGlobalValueAddress(EE, Name)
    ccall((:LLVMGetGlobalValueAddress, libllvm), UInt64, (LLVMExecutionEngineRef, Cstring), EE, Name)
end

function LLVMGetFunctionAddress(EE, Name)
    ccall((:LLVMGetFunctionAddress, libllvm), UInt64, (LLVMExecutionEngineRef, Cstring), EE, Name)
end

"""
    LLVMExecutionEngineGetErrMsg(EE, OutError)

Returns true on error, false on success. If true is returned then the error message is copied to OutStr and cleared in the ExecutionEngine instance.
"""
function LLVMExecutionEngineGetErrMsg(EE, OutError)
    ccall((:LLVMExecutionEngineGetErrMsg, libllvm), LLVMBool, (LLVMExecutionEngineRef, Ptr{Cstring}), EE, OutError)
end

# typedef uint8_t * ( * LLVMMemoryManagerAllocateCodeSectionCallback ) ( void * Opaque , uintptr_t Size , unsigned Alignment , unsigned SectionID , const char * SectionName )
const LLVMMemoryManagerAllocateCodeSectionCallback = Ptr{Cvoid}

# typedef uint8_t * ( * LLVMMemoryManagerAllocateDataSectionCallback ) ( void * Opaque , uintptr_t Size , unsigned Alignment , unsigned SectionID , const char * SectionName , LLVMBool IsReadOnly )
const LLVMMemoryManagerAllocateDataSectionCallback = Ptr{Cvoid}

# typedef LLVMBool ( * LLVMMemoryManagerFinalizeMemoryCallback ) ( void * Opaque , char * * ErrMsg )
const LLVMMemoryManagerFinalizeMemoryCallback = Ptr{Cvoid}

# typedef void ( * LLVMMemoryManagerDestroyCallback ) ( void * Opaque )
const LLVMMemoryManagerDestroyCallback = Ptr{Cvoid}

"""
    LLVMCreateSimpleMCJITMemoryManager(Opaque, AllocateCodeSection, AllocateDataSection, FinalizeMemory, Destroy)

Create a simple custom MCJIT memory manager. This memory manager can intercept allocations in a module-oblivious way. This will return NULL if any of the passed functions are NULL.

# Arguments
* `Opaque`: An opaque client object to pass back to the callbacks.
* `AllocateCodeSection`: Allocate a block of memory for executable code.
* `AllocateDataSection`: Allocate a block of memory for data.
* `FinalizeMemory`: Set page permissions and flush cache. Return 0 on success, 1 on error.
"""
function LLVMCreateSimpleMCJITMemoryManager(Opaque, AllocateCodeSection, AllocateDataSection, FinalizeMemory, Destroy)
    ccall((:LLVMCreateSimpleMCJITMemoryManager, libllvm), LLVMMCJITMemoryManagerRef, (Ptr{Cvoid}, LLVMMemoryManagerAllocateCodeSectionCallback, LLVMMemoryManagerAllocateDataSectionCallback, LLVMMemoryManagerFinalizeMemoryCallback, LLVMMemoryManagerDestroyCallback), Opaque, AllocateCodeSection, AllocateDataSection, FinalizeMemory, Destroy)
end

function LLVMDisposeMCJITMemoryManager(MM)
    ccall((:LLVMDisposeMCJITMemoryManager, libllvm), Cvoid, (LLVMMCJITMemoryManagerRef,), MM)
end

"""
# See also
llvm::JITEventListener
"""
const LLVMJITEventListenerRef = Ptr{LLVMOpaqueJITEventListener}

function LLVMCreateGDBRegistrationListener()
    ccall((:LLVMCreateGDBRegistrationListener, libllvm), LLVMJITEventListenerRef, ())
end

function LLVMCreateIntelJITEventListener()
    ccall((:LLVMCreateIntelJITEventListener, libllvm), LLVMJITEventListenerRef, ())
end

function LLVMCreateOProfileJITEventListener()
    ccall((:LLVMCreateOProfileJITEventListener, libllvm), LLVMJITEventListenerRef, ())
end

function LLVMCreatePerfJITEventListener()
    ccall((:LLVMCreatePerfJITEventListener, libllvm), LLVMJITEventListenerRef, ())
end

"""
    LLVMParseIRInContext(ContextRef, MemBuf, OutM, OutMessage)

Read LLVM IR from a memory buffer and convert it into an in-memory Module object. Returns 0 on success. Optionally returns a human-readable description of any errors that occurred during parsing IR. OutMessage must be disposed with [`LLVMDisposeMessage`](@ref).

# See also
llvm::ParseIR()
"""
function LLVMParseIRInContext(ContextRef, MemBuf, OutM, OutMessage)
    ccall((:LLVMParseIRInContext, libllvm), LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, Ptr{LLVMModuleRef}, Ptr{Cstring}), ContextRef, MemBuf, OutM, OutMessage)
end

function LLVMInitializeTransformUtils(R)
    ccall((:LLVMInitializeTransformUtils, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeScalarOpts(R)
    ccall((:LLVMInitializeScalarOpts, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeObjCARCOpts(R)
    ccall((:LLVMInitializeObjCARCOpts, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeVectorization(R)
    ccall((:LLVMInitializeVectorization, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeInstCombine(R)
    ccall((:LLVMInitializeInstCombine, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeAggressiveInstCombiner(R)
    ccall((:LLVMInitializeAggressiveInstCombiner, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeIPO(R)
    ccall((:LLVMInitializeIPO, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeInstrumentation(R)
    ccall((:LLVMInitializeInstrumentation, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeAnalysis(R)
    ccall((:LLVMInitializeAnalysis, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeIPA(R)
    ccall((:LLVMInitializeIPA, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeCodeGen(R)
    ccall((:LLVMInitializeCodeGen, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

function LLVMInitializeTarget(R)
    ccall((:LLVMInitializeTarget, libllvm), Cvoid, (LLVMPassRegistryRef,), R)
end

# typedef LLVMOrcObjectLayerRef ( * LLVMOrcLLJITBuilderObjectLinkingLayerCreatorFunction ) ( void * Ctx , LLVMOrcExecutionSessionRef ES , const char * Triple )
"""
A function for constructing an ObjectLinkingLayer instance to be used by an LLJIT instance.

Clients can call [`LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator`](@ref) to set the creator function to use when constructing an LLJIT instance. This can be used to override the default linking layer implementation that would otherwise be chosen by LLJITBuilder.

Object linking layers returned by this function will become owned by the LLJIT instance. The client is not responsible for managing their lifetimes after the function returns.
"""
const LLVMOrcLLJITBuilderObjectLinkingLayerCreatorFunction = Ptr{Cvoid}

mutable struct LLVMOrcOpaqueLLJITBuilder end

"""
A reference to an orc::LLJITBuilder instance.
"""
const LLVMOrcLLJITBuilderRef = Ptr{LLVMOrcOpaqueLLJITBuilder}

mutable struct LLVMOrcOpaqueLLJIT end

"""
A reference to an orc::LLJIT instance.
"""
const LLVMOrcLLJITRef = Ptr{LLVMOrcOpaqueLLJIT}

"""
    LLVMOrcCreateLLJITBuilder()

Create an LLVMOrcLLJITBuilder.

The client owns the resulting LLJITBuilder and should dispose of it using [`LLVMOrcDisposeLLJITBuilder`](@ref) once they are done with it.
"""
function LLVMOrcCreateLLJITBuilder()
    ccall((:LLVMOrcCreateLLJITBuilder, libllvm), LLVMOrcLLJITBuilderRef, ())
end

"""
    LLVMOrcDisposeLLJITBuilder(Builder)

Dispose of an [`LLVMOrcLLJITBuilderRef`](@ref). This should only be called if ownership has not been passed to [`LLVMOrcCreateLLJIT`](@ref) (e.g. because some error prevented that function from being called).
"""
function LLVMOrcDisposeLLJITBuilder(Builder)
    ccall((:LLVMOrcDisposeLLJITBuilder, libllvm), Cvoid, (LLVMOrcLLJITBuilderRef,), Builder)
end

"""
A reference to an orc::JITTargetMachineBuilder instance.
"""
const LLVMOrcJITTargetMachineBuilderRef = Ptr{LLVMOrcOpaqueJITTargetMachineBuilder}

"""
    LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(Builder, JTMB)

Set the JITTargetMachineBuilder to be used when constructing the LLJIT instance. Calling this function is optional: if it is not called then the LLJITBuilder will use JITTargeTMachineBuilder::detectHost to construct a JITTargetMachineBuilder.

This function takes ownership of the JTMB argument: clients should not dispose of the JITTargetMachineBuilder after calling this function.
"""
function LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(Builder, JTMB)
    ccall((:LLVMOrcLLJITBuilderSetJITTargetMachineBuilder, libllvm), Cvoid, (LLVMOrcLLJITBuilderRef, LLVMOrcJITTargetMachineBuilderRef), Builder, JTMB)
end

"""
    LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(Builder, F, Ctx)

Set an ObjectLinkingLayer creator function for this LLJIT instance.
"""
function LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(Builder, F, Ctx)
    ccall((:LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator, libllvm), Cvoid, (LLVMOrcLLJITBuilderRef, LLVMOrcLLJITBuilderObjectLinkingLayerCreatorFunction, Ptr{Cvoid}), Builder, F, Ctx)
end

"""
    LLVMOrcCreateLLJIT(Result, Builder)

Create an LLJIT instance from an LLJITBuilder.

This operation takes ownership of the Builder argument: clients should not dispose of the builder after calling this function (even if the function returns an error). If a null Builder argument is provided then a default-constructed LLJITBuilder will be used.

On success the resulting LLJIT instance is uniquely owned by the client and automatically manages the memory of all JIT'd code and all modules that are transferred to it (e.g. via [`LLVMOrcLLJITAddLLVMIRModule`](@ref)). Disposing of the LLJIT instance will free all memory managed by the JIT, including JIT'd code and not-yet compiled modules.
"""
function LLVMOrcCreateLLJIT(Result, Builder)
    ccall((:LLVMOrcCreateLLJIT, libllvm), LLVMErrorRef, (Ptr{LLVMOrcLLJITRef}, LLVMOrcLLJITBuilderRef), Result, Builder)
end

"""
    LLVMOrcDisposeLLJIT(J)

Dispose of an LLJIT instance.
"""
function LLVMOrcDisposeLLJIT(J)
    ccall((:LLVMOrcDisposeLLJIT, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef,), J)
end

"""
A reference to an orc::ExecutionSession instance.
"""
const LLVMOrcExecutionSessionRef = Ptr{LLVMOrcOpaqueExecutionSession}

"""
    LLVMOrcLLJITGetExecutionSession(J)

Get a reference to the ExecutionSession for this LLJIT instance.

The ExecutionSession is owned by the LLJIT instance. The client is not responsible for managing its memory.
"""
function LLVMOrcLLJITGetExecutionSession(J)
    ccall((:LLVMOrcLLJITGetExecutionSession, libllvm), LLVMOrcExecutionSessionRef, (LLVMOrcLLJITRef,), J)
end

"""
A reference to an orc::JITDylib instance.
"""
const LLVMOrcJITDylibRef = Ptr{LLVMOrcOpaqueJITDylib}

"""
    LLVMOrcLLJITGetMainJITDylib(J)

Return a reference to the Main JITDylib.

The JITDylib is owned by the LLJIT instance. The client is not responsible for managing its memory.
"""
function LLVMOrcLLJITGetMainJITDylib(J)
    ccall((:LLVMOrcLLJITGetMainJITDylib, libllvm), LLVMOrcJITDylibRef, (LLVMOrcLLJITRef,), J)
end

"""
    LLVMOrcLLJITGetTripleString(J)

Return the target triple for this LLJIT instance. This string is owned by the LLJIT instance and should not be freed by the client.
"""
function LLVMOrcLLJITGetTripleString(J)
    ccall((:LLVMOrcLLJITGetTripleString, libllvm), Cstring, (LLVMOrcLLJITRef,), J)
end

"""
    LLVMOrcLLJITGetGlobalPrefix(J)

Returns the global prefix character according to the LLJIT's DataLayout.
"""
function LLVMOrcLLJITGetGlobalPrefix(J)
    ccall((:LLVMOrcLLJITGetGlobalPrefix, libllvm), Cchar, (LLVMOrcLLJITRef,), J)
end

"""
A reference to an orc::SymbolStringPool table entry.
"""
const LLVMOrcSymbolStringPoolEntryRef = Ptr{LLVMOrcOpaqueSymbolStringPoolEntry}

"""
    LLVMOrcLLJITMangleAndIntern(J, UnmangledName)

Mangles the given string according to the LLJIT instance's DataLayout, then interns the result in the SymbolStringPool and returns a reference to the pool entry. Clients should call [`LLVMOrcReleaseSymbolStringPoolEntry`](@ref) to decrement the ref-count on the pool entry once they are finished with this value.
"""
function LLVMOrcLLJITMangleAndIntern(J, UnmangledName)
    ccall((:LLVMOrcLLJITMangleAndIntern, libllvm), LLVMOrcSymbolStringPoolEntryRef, (LLVMOrcLLJITRef, Cstring), J, UnmangledName)
end

"""
    LLVMOrcLLJITAddObjectFile(J, JD, ObjBuffer)

Add a buffer representing an object file to the given JITDylib in the given LLJIT instance. This operation transfers ownership of the buffer to the LLJIT instance. The buffer should not be disposed of or referenced once this function returns.

Resources associated with the given object will be tracked by the given JITDylib's default resource tracker.
"""
function LLVMOrcLLJITAddObjectFile(J, JD, ObjBuffer)
    ccall((:LLVMOrcLLJITAddObjectFile, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef, LLVMOrcJITDylibRef, LLVMMemoryBufferRef), J, JD, ObjBuffer)
end

"""
A reference to an orc::ResourceTracker instance.
"""
const LLVMOrcResourceTrackerRef = Ptr{LLVMOrcOpaqueResourceTracker}

"""
    LLVMOrcLLJITAddObjectFileWithRT(J, RT, ObjBuffer)

Add a buffer representing an object file to the given ResourceTracker's JITDylib in the given LLJIT instance. This operation transfers ownership of the buffer to the LLJIT instance. The buffer should not be disposed of or referenced once this function returns.

Resources associated with the given object will be tracked by ResourceTracker RT.
"""
function LLVMOrcLLJITAddObjectFileWithRT(J, RT, ObjBuffer)
    ccall((:LLVMOrcLLJITAddObjectFileWithRT, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef, LLVMOrcResourceTrackerRef, LLVMMemoryBufferRef), J, RT, ObjBuffer)
end

"""
A reference to an orc::ThreadSafeModule instance.
"""
const LLVMOrcThreadSafeModuleRef = Ptr{LLVMOrcOpaqueThreadSafeModule}

"""
    LLVMOrcLLJITAddLLVMIRModule(J, JD, TSM)

Add an IR module to the given JITDylib in the given LLJIT instance. This operation transfers ownership of the TSM argument to the LLJIT instance. The TSM argument should not be disposed of or referenced once this function returns.

Resources associated with the given Module will be tracked by the given JITDylib's default resource tracker.
"""
function LLVMOrcLLJITAddLLVMIRModule(J, JD, TSM)
    ccall((:LLVMOrcLLJITAddLLVMIRModule, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef, LLVMOrcJITDylibRef, LLVMOrcThreadSafeModuleRef), J, JD, TSM)
end

"""
    LLVMOrcLLJITAddLLVMIRModuleWithRT(J, JD, TSM)

Add an IR module to the given ResourceTracker's JITDylib in the given LLJIT instance. This operation transfers ownership of the TSM argument to the LLJIT instance. The TSM argument should not be disposed of or referenced once this function returns.

Resources associated with the given Module will be tracked by ResourceTracker RT.
"""
function LLVMOrcLLJITAddLLVMIRModuleWithRT(J, JD, TSM)
    ccall((:LLVMOrcLLJITAddLLVMIRModuleWithRT, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef, LLVMOrcResourceTrackerRef, LLVMOrcThreadSafeModuleRef), J, JD, TSM)
end

"""
Represents an address in the executor process.
"""
const LLVMOrcExecutorAddress = UInt64

"""
    LLVMOrcLLJITLookup(J, Result, Name)

Look up the given symbol in the main JITDylib of the given LLJIT instance.

This operation does not take ownership of the Name argument.
"""
function LLVMOrcLLJITLookup(J, Result, Name)
    ccall((:LLVMOrcLLJITLookup, libllvm), LLVMErrorRef, (LLVMOrcLLJITRef, Ptr{LLVMOrcExecutorAddress}, Cstring), J, Result, Name)
end

"""
A reference to an orc::ObjectLayer instance.
"""
const LLVMOrcObjectLayerRef = Ptr{LLVMOrcOpaqueObjectLayer}

"""
    LLVMOrcLLJITGetObjLinkingLayer(J)

Returns a non-owning reference to the LLJIT instance's object linking layer.
"""
function LLVMOrcLLJITGetObjLinkingLayer(J)
    ccall((:LLVMOrcLLJITGetObjLinkingLayer, libllvm), LLVMOrcObjectLayerRef, (LLVMOrcLLJITRef,), J)
end

"""
A reference to an orc::ObjectTransformLayer instance.
"""
const LLVMOrcObjectTransformLayerRef = Ptr{LLVMOrcOpaqueObjectTransformLayer}

"""
    LLVMOrcLLJITGetObjTransformLayer(J)

Returns a non-owning reference to the LLJIT instance's object linking layer.
"""
function LLVMOrcLLJITGetObjTransformLayer(J)
    ccall((:LLVMOrcLLJITGetObjTransformLayer, libllvm), LLVMOrcObjectTransformLayerRef, (LLVMOrcLLJITRef,), J)
end

"""
A reference to an orc::IRTransformLayer instance.
"""
const LLVMOrcIRTransformLayerRef = Ptr{LLVMOrcOpaqueIRTransformLayer}

"""
    LLVMOrcLLJITGetIRTransformLayer(J)

Returns a non-owning reference to the LLJIT instance's IR transform layer.
"""
function LLVMOrcLLJITGetIRTransformLayer(J)
    ccall((:LLVMOrcLLJITGetIRTransformLayer, libllvm), LLVMOrcIRTransformLayerRef, (LLVMOrcLLJITRef,), J)
end

"""
    LLVMOrcLLJITGetDataLayoutStr(J)

Get the LLJIT instance's default data layout string.

This string is owned by the LLJIT instance and does not need to be freed by the caller.
"""
function LLVMOrcLLJITGetDataLayoutStr(J)
    ccall((:LLVMOrcLLJITGetDataLayoutStr, libllvm), Cstring, (LLVMOrcLLJITRef,), J)
end

"""
    LLVMLinkerMode

` LLVMCCoreLinker Linker`

` LLVMCCore`

@{
"""
@cenum LLVMLinkerMode::UInt32 begin
    LLVMLinkerDestroySource = 0
    LLVMLinkerPreserveSource_Removed = 1
end

function LLVMLinkModules2(Dest, Src)
    ccall((:LLVMLinkModules2, libllvm), LLVMBool, (LLVMModuleRef, LLVMModuleRef), Dest, Src)
end

mutable struct LLVMOpaqueSectionIterator end

"""
` LLVMCObject Object file reading and writing`

` LLVMC`

@{
"""
const LLVMSectionIteratorRef = Ptr{LLVMOpaqueSectionIterator}

mutable struct LLVMOpaqueSymbolIterator end

const LLVMSymbolIteratorRef = Ptr{LLVMOpaqueSymbolIterator}

mutable struct LLVMOpaqueRelocationIterator end

const LLVMRelocationIteratorRef = Ptr{LLVMOpaqueRelocationIterator}

"""
    LLVMBinaryType

| Enumerator                         | Note                           |
| :--------------------------------- | :----------------------------- |
| LLVMBinaryTypeArchive              | Archive file.                  |
| LLVMBinaryTypeMachOUniversalBinary | Mach-O Universal Binary file.  |
| LLVMBinaryTypeCOFFImportFile       | COFF Import file.              |
| LLVMBinaryTypeIR                   | LLVM IR.                       |
| LLVMBinaryTypeWinRes               | Windows resource (.res) file.  |
| LLVMBinaryTypeCOFF                 | COFF Object file.              |
| LLVMBinaryTypeELF32L               | ELF 32-bit, little endian.     |
| LLVMBinaryTypeELF32B               | ELF 32-bit, big endian.        |
| LLVMBinaryTypeELF64L               | ELF 64-bit, little endian.     |
| LLVMBinaryTypeELF64B               | ELF 64-bit, big endian.        |
| LLVMBinaryTypeMachO32L             | MachO 32-bit, little endian.   |
| LLVMBinaryTypeMachO32B             | MachO 32-bit, big endian.      |
| LLVMBinaryTypeMachO64L             | MachO 64-bit, little endian.   |
| LLVMBinaryTypeMachO64B             | MachO 64-bit, big endian.      |
| LLVMBinaryTypeWasm                 | Web Assembly.                  |
| LLVMBinaryTypeOffload              | Offloading fatbinary.          |
"""
@cenum LLVMBinaryType::UInt32 begin
    LLVMBinaryTypeArchive = 0
    LLVMBinaryTypeMachOUniversalBinary = 1
    LLVMBinaryTypeCOFFImportFile = 2
    LLVMBinaryTypeIR = 3
    LLVMBinaryTypeWinRes = 4
    LLVMBinaryTypeCOFF = 5
    LLVMBinaryTypeELF32L = 6
    LLVMBinaryTypeELF32B = 7
    LLVMBinaryTypeELF64L = 8
    LLVMBinaryTypeELF64B = 9
    LLVMBinaryTypeMachO32L = 10
    LLVMBinaryTypeMachO32B = 11
    LLVMBinaryTypeMachO64L = 12
    LLVMBinaryTypeMachO64B = 13
    LLVMBinaryTypeWasm = 14
    LLVMBinaryTypeOffload = 15
end

"""
# See also
llvm::object::Binary
"""
const LLVMBinaryRef = Ptr{LLVMOpaqueBinary}

"""
    LLVMCreateBinary(MemBuf, Context, ErrorMessage)

Create a binary file from the given memory buffer.

The exact type of the binary file will be inferred automatically, and the appropriate implementation selected. The context may be NULL except if the resulting file is an LLVM IR file.

The memory buffer is not consumed by this function. It is the responsibilty of the caller to free it with [`LLVMDisposeMemoryBuffer`](@ref).

If NULL is returned, the `ErrorMessage` parameter is populated with the error's description. It is then the caller's responsibility to free this message by calling [`LLVMDisposeMessage`](@ref).

# See also
llvm::object::createBinary
"""
function LLVMCreateBinary(MemBuf, Context, ErrorMessage)
    ccall((:LLVMCreateBinary, libllvm), LLVMBinaryRef, (LLVMMemoryBufferRef, LLVMContextRef, Ptr{Cstring}), MemBuf, Context, ErrorMessage)
end

"""
    LLVMDisposeBinary(BR)

Dispose of a binary file.

The binary file does not own its backing buffer. It is the responsibilty of the caller to free it with [`LLVMDisposeMemoryBuffer`](@ref).
"""
function LLVMDisposeBinary(BR)
    ccall((:LLVMDisposeBinary, libllvm), Cvoid, (LLVMBinaryRef,), BR)
end

"""
    LLVMBinaryCopyMemoryBuffer(BR)

Retrieves a copy of the memory buffer associated with this object file.

The returned buffer is merely a shallow copy and does not own the actual backing buffer of the binary. Nevertheless, it is the responsibility of the caller to free it with [`LLVMDisposeMemoryBuffer`](@ref).

# See also
llvm::object::getMemoryBufferRef
"""
function LLVMBinaryCopyMemoryBuffer(BR)
    ccall((:LLVMBinaryCopyMemoryBuffer, libllvm), LLVMMemoryBufferRef, (LLVMBinaryRef,), BR)
end

"""
    LLVMBinaryGetType(BR)

Retrieve the specific type of a binary.

# See also
llvm::object::Binary::getType
"""
function LLVMBinaryGetType(BR)
    ccall((:LLVMBinaryGetType, libllvm), LLVMBinaryType, (LLVMBinaryRef,), BR)
end

function LLVMMachOUniversalBinaryCopyObjectForArch(BR, Arch, ArchLen, ErrorMessage)
    ccall((:LLVMMachOUniversalBinaryCopyObjectForArch, libllvm), LLVMBinaryRef, (LLVMBinaryRef, Cstring, Csize_t, Ptr{Cstring}), BR, Arch, ArchLen, ErrorMessage)
end

"""
    LLVMObjectFileCopySectionIterator(BR)

Retrieve a copy of the section iterator for this object file.

If there are no sections, the result is NULL.

The returned iterator is merely a shallow copy. Nevertheless, it is the responsibility of the caller to free it with [`LLVMDisposeSectionIterator`](@ref).

# See also
llvm::object::sections()
"""
function LLVMObjectFileCopySectionIterator(BR)
    ccall((:LLVMObjectFileCopySectionIterator, libllvm), LLVMSectionIteratorRef, (LLVMBinaryRef,), BR)
end

"""
    LLVMObjectFileIsSectionIteratorAtEnd(BR, SI)

Returns whether the given section iterator is at the end.

# See also
llvm::object::section\\_end
"""
function LLVMObjectFileIsSectionIteratorAtEnd(BR, SI)
    ccall((:LLVMObjectFileIsSectionIteratorAtEnd, libllvm), LLVMBool, (LLVMBinaryRef, LLVMSectionIteratorRef), BR, SI)
end

"""
    LLVMObjectFileCopySymbolIterator(BR)

Retrieve a copy of the symbol iterator for this object file.

If there are no symbols, the result is NULL.

The returned iterator is merely a shallow copy. Nevertheless, it is the responsibility of the caller to free it with [`LLVMDisposeSymbolIterator`](@ref).

# See also
llvm::object::symbols()
"""
function LLVMObjectFileCopySymbolIterator(BR)
    ccall((:LLVMObjectFileCopySymbolIterator, libllvm), LLVMSymbolIteratorRef, (LLVMBinaryRef,), BR)
end

"""
    LLVMObjectFileIsSymbolIteratorAtEnd(BR, SI)

Returns whether the given symbol iterator is at the end.

# See also
llvm::object::symbol\\_end
"""
function LLVMObjectFileIsSymbolIteratorAtEnd(BR, SI)
    ccall((:LLVMObjectFileIsSymbolIteratorAtEnd, libllvm), LLVMBool, (LLVMBinaryRef, LLVMSymbolIteratorRef), BR, SI)
end

function LLVMDisposeSectionIterator(SI)
    ccall((:LLVMDisposeSectionIterator, libllvm), Cvoid, (LLVMSectionIteratorRef,), SI)
end

function LLVMMoveToNextSection(SI)
    ccall((:LLVMMoveToNextSection, libllvm), Cvoid, (LLVMSectionIteratorRef,), SI)
end

function LLVMMoveToContainingSection(Sect, Sym)
    ccall((:LLVMMoveToContainingSection, libllvm), Cvoid, (LLVMSectionIteratorRef, LLVMSymbolIteratorRef), Sect, Sym)
end

function LLVMDisposeSymbolIterator(SI)
    ccall((:LLVMDisposeSymbolIterator, libllvm), Cvoid, (LLVMSymbolIteratorRef,), SI)
end

function LLVMMoveToNextSymbol(SI)
    ccall((:LLVMMoveToNextSymbol, libllvm), Cvoid, (LLVMSymbolIteratorRef,), SI)
end

function LLVMGetSectionName(SI)
    ccall((:LLVMGetSectionName, libllvm), Cstring, (LLVMSectionIteratorRef,), SI)
end

function LLVMGetSectionSize(SI)
    ccall((:LLVMGetSectionSize, libllvm), UInt64, (LLVMSectionIteratorRef,), SI)
end

function LLVMGetSectionContents(SI)
    ccall((:LLVMGetSectionContents, libllvm), Cstring, (LLVMSectionIteratorRef,), SI)
end

function LLVMGetSectionAddress(SI)
    ccall((:LLVMGetSectionAddress, libllvm), UInt64, (LLVMSectionIteratorRef,), SI)
end

function LLVMGetSectionContainsSymbol(SI, Sym)
    ccall((:LLVMGetSectionContainsSymbol, libllvm), LLVMBool, (LLVMSectionIteratorRef, LLVMSymbolIteratorRef), SI, Sym)
end

function LLVMGetRelocations(Section)
    ccall((:LLVMGetRelocations, libllvm), LLVMRelocationIteratorRef, (LLVMSectionIteratorRef,), Section)
end

function LLVMDisposeRelocationIterator(RI)
    ccall((:LLVMDisposeRelocationIterator, libllvm), Cvoid, (LLVMRelocationIteratorRef,), RI)
end

function LLVMIsRelocationIteratorAtEnd(Section, RI)
    ccall((:LLVMIsRelocationIteratorAtEnd, libllvm), LLVMBool, (LLVMSectionIteratorRef, LLVMRelocationIteratorRef), Section, RI)
end

function LLVMMoveToNextRelocation(RI)
    ccall((:LLVMMoveToNextRelocation, libllvm), Cvoid, (LLVMRelocationIteratorRef,), RI)
end

function LLVMGetSymbolName(SI)
    ccall((:LLVMGetSymbolName, libllvm), Cstring, (LLVMSymbolIteratorRef,), SI)
end

function LLVMGetSymbolAddress(SI)
    ccall((:LLVMGetSymbolAddress, libllvm), UInt64, (LLVMSymbolIteratorRef,), SI)
end

function LLVMGetSymbolSize(SI)
    ccall((:LLVMGetSymbolSize, libllvm), UInt64, (LLVMSymbolIteratorRef,), SI)
end

function LLVMGetRelocationOffset(RI)
    ccall((:LLVMGetRelocationOffset, libllvm), UInt64, (LLVMRelocationIteratorRef,), RI)
end

function LLVMGetRelocationSymbol(RI)
    ccall((:LLVMGetRelocationSymbol, libllvm), LLVMSymbolIteratorRef, (LLVMRelocationIteratorRef,), RI)
end

function LLVMGetRelocationType(RI)
    ccall((:LLVMGetRelocationType, libllvm), UInt64, (LLVMRelocationIteratorRef,), RI)
end

function LLVMGetRelocationTypeName(RI)
    ccall((:LLVMGetRelocationTypeName, libllvm), Cstring, (LLVMRelocationIteratorRef,), RI)
end

function LLVMGetRelocationValueString(RI)
    ccall((:LLVMGetRelocationValueString, libllvm), Cstring, (LLVMRelocationIteratorRef,), RI)
end

mutable struct LLVMOpaqueObjectFile end

"""
Deprecated: Use [`LLVMBinaryRef`](@ref) instead.
"""
const LLVMObjectFileRef = Ptr{LLVMOpaqueObjectFile}

"""
    LLVMCreateObjectFile(MemBuf)

Deprecated: Use [`LLVMCreateBinary`](@ref) instead.
"""
function LLVMCreateObjectFile(MemBuf)
    ccall((:LLVMCreateObjectFile, libllvm), LLVMObjectFileRef, (LLVMMemoryBufferRef,), MemBuf)
end

"""
    LLVMDisposeObjectFile(ObjectFile)

Deprecated: Use [`LLVMDisposeBinary`](@ref) instead.
"""
function LLVMDisposeObjectFile(ObjectFile)
    ccall((:LLVMDisposeObjectFile, libllvm), Cvoid, (LLVMObjectFileRef,), ObjectFile)
end

"""
    LLVMGetSections(ObjectFile)

Deprecated: Use [`LLVMObjectFileCopySectionIterator`](@ref) instead.
"""
function LLVMGetSections(ObjectFile)
    ccall((:LLVMGetSections, libllvm), LLVMSectionIteratorRef, (LLVMObjectFileRef,), ObjectFile)
end

"""
    LLVMIsSectionIteratorAtEnd(ObjectFile, SI)

Deprecated: Use [`LLVMObjectFileIsSectionIteratorAtEnd`](@ref) instead.
"""
function LLVMIsSectionIteratorAtEnd(ObjectFile, SI)
    ccall((:LLVMIsSectionIteratorAtEnd, libllvm), LLVMBool, (LLVMObjectFileRef, LLVMSectionIteratorRef), ObjectFile, SI)
end

"""
    LLVMGetSymbols(ObjectFile)

Deprecated: Use [`LLVMObjectFileCopySymbolIterator`](@ref) instead.
"""
function LLVMGetSymbols(ObjectFile)
    ccall((:LLVMGetSymbols, libllvm), LLVMSymbolIteratorRef, (LLVMObjectFileRef,), ObjectFile)
end

"""
    LLVMIsSymbolIteratorAtEnd(ObjectFile, SI)

Deprecated: Use [`LLVMObjectFileIsSymbolIteratorAtEnd`](@ref) instead.
"""
function LLVMIsSymbolIteratorAtEnd(ObjectFile, SI)
    ccall((:LLVMIsSymbolIteratorAtEnd, libllvm), LLVMBool, (LLVMObjectFileRef, LLVMSymbolIteratorRef), ObjectFile, SI)
end

"""
Represents an address in the executor process.
"""
const LLVMOrcJITTargetAddress = UInt64

"""
    LLVMJITSymbolGenericFlags

Represents generic linkage flags for a symbol definition.
"""
@cenum LLVMJITSymbolGenericFlags::UInt32 begin
    LLVMJITSymbolGenericFlagsNone = 0
    LLVMJITSymbolGenericFlagsExported = 1
    LLVMJITSymbolGenericFlagsWeak = 2
    LLVMJITSymbolGenericFlagsCallable = 4
    LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly = 8
end

"""
Represents target specific flags for a symbol definition.
"""
const LLVMJITSymbolTargetFlags = UInt8

"""
    LLVMJITSymbolFlags

Represents the linkage flags for a symbol definition.
"""
struct LLVMJITSymbolFlags
    GenericFlags::UInt8
    TargetFlags::UInt8
end

"""
    LLVMJITEvaluatedSymbol

Represents an evaluated symbol address and flags.
"""
struct LLVMJITEvaluatedSymbol
    Address::LLVMOrcExecutorAddress
    Flags::LLVMJITSymbolFlags
end

# typedef void ( * LLVMOrcErrorReporterFunction ) ( void * Ctx , LLVMErrorRef Err )
"""
Error reporter function.
"""
const LLVMOrcErrorReporterFunction = Ptr{Cvoid}

"""
A reference to an orc::SymbolStringPool.
"""
const LLVMOrcSymbolStringPoolRef = Ptr{LLVMOrcOpaqueSymbolStringPool}

"""
    LLVMOrcCSymbolFlagsMapPair

Represents a pair of a symbol name and [`LLVMJITSymbolFlags`](@ref).
"""
struct LLVMOrcCSymbolFlagsMapPair
    Name::LLVMOrcSymbolStringPoolEntryRef
    Flags::LLVMJITSymbolFlags
end

"""
Represents a list of (SymbolStringPtr, JITSymbolFlags) pairs that can be used to construct a SymbolFlagsMap.
"""
const LLVMOrcCSymbolFlagsMapPairs = Ptr{LLVMOrcCSymbolFlagsMapPair}

"""
    LLVMOrcCSymbolMapPair

Represents a pair of a symbol name and an evaluated symbol.
"""
struct LLVMOrcCSymbolMapPair
    Name::LLVMOrcSymbolStringPoolEntryRef
    Sym::LLVMJITEvaluatedSymbol
end

"""
Represents a list of (SymbolStringPtr, JITEvaluatedSymbol) pairs that can be used to construct a SymbolMap.
"""
const LLVMOrcCSymbolMapPairs = Ptr{LLVMOrcCSymbolMapPair}

"""
    LLVMOrcCSymbolAliasMapEntry

Represents a SymbolAliasMapEntry
"""
struct LLVMOrcCSymbolAliasMapEntry
    Name::LLVMOrcSymbolStringPoolEntryRef
    Flags::LLVMJITSymbolFlags
end

"""
    LLVMOrcCSymbolAliasMapPair

Represents a pair of a symbol name and SymbolAliasMapEntry.
"""
struct LLVMOrcCSymbolAliasMapPair
    Name::LLVMOrcSymbolStringPoolEntryRef
    Entry::LLVMOrcCSymbolAliasMapEntry
end

"""
Represents a list of (SymbolStringPtr, (SymbolStringPtr, JITSymbolFlags)) pairs that can be used to construct a SymbolFlagsMap.
"""
const LLVMOrcCSymbolAliasMapPairs = Ptr{LLVMOrcCSymbolAliasMapPair}

"""
    LLVMOrcCSymbolsList

Represents a list of [`LLVMOrcSymbolStringPoolEntryRef`](@ref) and the associated length.
"""
struct LLVMOrcCSymbolsList
    Symbols::Ptr{LLVMOrcSymbolStringPoolEntryRef}
    Length::Csize_t
end

"""
    LLVMOrcCDependenceMapPair

Represents a pair of a JITDylib and [`LLVMOrcCSymbolsList`](@ref).
"""
struct LLVMOrcCDependenceMapPair
    JD::LLVMOrcJITDylibRef
    Names::LLVMOrcCSymbolsList
end

"""
Represents a list of (JITDylibRef, ([`LLVMOrcSymbolStringPoolEntryRef`](@ref)*, size\\_t)) pairs that can be used to construct a SymbolDependenceMap.
"""
const LLVMOrcCDependenceMapPairs = Ptr{LLVMOrcCDependenceMapPair}

"""
    LLVMOrcLookupKind

Lookup kind. This can be used by definition generators when deciding whether to produce a definition for a requested symbol.

This enum should be kept in sync with llvm::orc::LookupKind.
"""
@cenum LLVMOrcLookupKind::UInt32 begin
    LLVMOrcLookupKindStatic = 0
    LLVMOrcLookupKindDLSym = 1
end

"""
    LLVMOrcJITDylibLookupFlags

JITDylib lookup flags. This can be used by definition generators when deciding whether to produce a definition for a requested symbol.

This enum should be kept in sync with llvm::orc::JITDylibLookupFlags.
"""
@cenum LLVMOrcJITDylibLookupFlags::UInt32 begin
    LLVMOrcJITDylibLookupFlagsMatchExportedSymbolsOnly = 0
    LLVMOrcJITDylibLookupFlagsMatchAllSymbols = 1
end

"""
    LLVMOrcCJITDylibSearchOrderElement

An element type for a JITDylib search order.
"""
struct LLVMOrcCJITDylibSearchOrderElement
    JD::LLVMOrcJITDylibRef
    JDLookupFlags::LLVMOrcJITDylibLookupFlags
end

"""
A JITDylib search order.

The list is terminated with an element containing a null pointer for the JD field.
"""
const LLVMOrcCJITDylibSearchOrder = Ptr{LLVMOrcCJITDylibSearchOrderElement}

"""
    LLVMOrcSymbolLookupFlags

Symbol lookup flags for lookup sets. This should be kept in sync with llvm::orc::SymbolLookupFlags.
"""
@cenum LLVMOrcSymbolLookupFlags::UInt32 begin
    LLVMOrcSymbolLookupFlagsRequiredSymbol = 0
    LLVMOrcSymbolLookupFlagsWeaklyReferencedSymbol = 1
end

"""
    LLVMOrcCLookupSetElement

An element type for a symbol lookup set.
"""
struct LLVMOrcCLookupSetElement
    Name::LLVMOrcSymbolStringPoolEntryRef
    LookupFlags::LLVMOrcSymbolLookupFlags
end

"""
A set of symbols to look up / generate.

The list is terminated with an element containing a null pointer for the Name field.

If a client creates an instance of this type then they are responsible for freeing it, and for ensuring that all strings have been retained over the course of its life. Clients receiving a copy from a callback are not responsible for managing lifetime or retain counts.
"""
const LLVMOrcCLookupSet = Ptr{LLVMOrcCLookupSetElement}

"""
A reference to a uniquely owned orc::MaterializationUnit instance.
"""
const LLVMOrcMaterializationUnitRef = Ptr{LLVMOrcOpaqueMaterializationUnit}

"""
A reference to a uniquely owned orc::MaterializationResponsibility instance.

Ownership must be passed to a lower-level layer in a JIT stack.
"""
const LLVMOrcMaterializationResponsibilityRef = Ptr{LLVMOrcOpaqueMaterializationResponsibility}

# typedef void ( * LLVMOrcMaterializationUnitMaterializeFunction ) ( void * Ctx , LLVMOrcMaterializationResponsibilityRef MR )
"""
A MaterializationUnit materialize callback.

Ownership of the Ctx and MR arguments passes to the callback which must adhere to the [`LLVMOrcMaterializationResponsibilityRef`](@ref) contract (see comment for that type).

If this callback is called then the LLVMOrcMaterializationUnitDestroy callback will NOT be called.
"""
const LLVMOrcMaterializationUnitMaterializeFunction = Ptr{Cvoid}

# typedef void ( * LLVMOrcMaterializationUnitDiscardFunction ) ( void * Ctx , LLVMOrcJITDylibRef JD , LLVMOrcSymbolStringPoolEntryRef Symbol )
"""
A MaterializationUnit discard callback.

Ownership of JD and Symbol remain with the caller: These arguments should not be disposed of or released.
"""
const LLVMOrcMaterializationUnitDiscardFunction = Ptr{Cvoid}

# typedef void ( * LLVMOrcMaterializationUnitDestroyFunction ) ( void * Ctx )
"""
A MaterializationUnit destruction callback.

If a custom MaterializationUnit is destroyed before its Materialize function is called then this function will be called to provide an opportunity for the underlying program representation to be destroyed.
"""
const LLVMOrcMaterializationUnitDestroyFunction = Ptr{Cvoid}

"""
A reference to an orc::DefinitionGenerator.
"""
const LLVMOrcDefinitionGeneratorRef = Ptr{LLVMOrcOpaqueDefinitionGenerator}

"""
An opaque lookup state object. Instances of this type can be captured to suspend a lookup while a custom generator function attempts to produce a definition.

If a client captures a lookup state object then they must eventually call [`LLVMOrcLookupStateContinueLookup`](@ref) to restart the lookup. This is required in order to release memory allocated for the lookup state, even if errors have occurred while the lookup was suspended (if these errors have made the lookup impossible to complete then it will issue its own error before destruction).
"""
const LLVMOrcLookupStateRef = Ptr{LLVMOrcOpaqueLookupState}

# typedef LLVMErrorRef ( * LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction ) ( LLVMOrcDefinitionGeneratorRef GeneratorObj , void * Ctx , LLVMOrcLookupStateRef * LookupState , LLVMOrcLookupKind Kind , LLVMOrcJITDylibRef JD , LLVMOrcJITDylibLookupFlags JDLookupFlags , LLVMOrcCLookupSet LookupSet , size_t LookupSetSize )
"""
A custom generator function. This can be used to create a custom generator object using [`LLVMOrcCreateCustomCAPIDefinitionGenerator`](@ref). The resulting object can be attached to a JITDylib, via [`LLVMOrcJITDylibAddGenerator`](@ref), to receive callbacks when lookups fail to match existing definitions.

GeneratorObj will contain the address of the custom generator object.

Ctx will contain the context object passed to [`LLVMOrcCreateCustomCAPIDefinitionGenerator`](@ref).

LookupState will contain a pointer to an [`LLVMOrcLookupStateRef`](@ref) object. This can optionally be modified to make the definition generation process asynchronous: If the LookupStateRef value is copied, and the original [`LLVMOrcLookupStateRef`](@ref) set to null, the lookup will be suspended. Once the asynchronous definition process has been completed clients must call [`LLVMOrcLookupStateContinueLookup`](@ref) to continue the lookup (this should be done unconditionally, even if errors have occurred in the mean time, to free the lookup state memory and notify the query object of the failures). If LookupState is captured this function must return [`LLVMErrorSuccess`](@ref).

The Kind argument can be inspected to determine the lookup kind (e.g. as-if-during-static-link, or as-if-during-dlsym).

The JD argument specifies which JITDylib the definitions should be generated into.

The JDLookupFlags argument can be inspected to determine whether the original lookup included non-exported symobls.

Finally, the LookupSet argument contains the set of symbols that could not be found in JD already (the set of generation candidates).
"""
const LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction = Ptr{Cvoid}

# typedef void ( * LLVMOrcDisposeCAPIDefinitionGeneratorFunction ) ( void * Ctx )
"""
Disposer for a custom generator.

Will be called by ORC when the JITDylib that the generator is attached to is destroyed.
"""
const LLVMOrcDisposeCAPIDefinitionGeneratorFunction = Ptr{Cvoid}

# typedef int ( * LLVMOrcSymbolPredicate ) ( void * Ctx , LLVMOrcSymbolStringPoolEntryRef Sym )
"""
Predicate function for SymbolStringPoolEntries.
"""
const LLVMOrcSymbolPredicate = Ptr{Cvoid}

"""
A reference to an orc::ThreadSafeContext instance.
"""
const LLVMOrcThreadSafeContextRef = Ptr{LLVMOrcOpaqueThreadSafeContext}

# typedef LLVMErrorRef ( * LLVMOrcGenericIRModuleOperationFunction ) ( void * Ctx , LLVMModuleRef M )
"""
A function for inspecting/mutating IR modules, suitable for use with [`LLVMOrcThreadSafeModuleWithModuleDo`](@ref).
"""
const LLVMOrcGenericIRModuleOperationFunction = Ptr{Cvoid}

"""
A reference to an orc::ObjectLinkingLayer instance.
"""
const LLVMOrcObjectLinkingLayerRef = Ptr{LLVMOrcOpaqueObjectLinkingLayer}

# typedef LLVMErrorRef ( * LLVMOrcIRTransformLayerTransformFunction ) ( void * Ctx , LLVMOrcThreadSafeModuleRef * ModInOut , LLVMOrcMaterializationResponsibilityRef MR )
"""
A function for applying transformations as part of an transform layer.

Implementations of this type are responsible for managing the lifetime of the Module pointed to by ModInOut: If the [`LLVMModuleRef`](@ref) value is overwritten then the function is responsible for disposing of the incoming module. If the module is simply accessed/mutated in-place then ownership returns to the caller and the function does not need to do any lifetime management.

Clients can call [`LLVMOrcLLJITGetIRTransformLayer`](@ref) to obtain the transform layer of a LLJIT instance, and use [`LLVMOrcIRTransformLayerSetTransform`](@ref) to set the function. This can be used to override the default transform layer.
"""
const LLVMOrcIRTransformLayerTransformFunction = Ptr{Cvoid}

# typedef LLVMErrorRef ( * LLVMOrcObjectTransformLayerTransformFunction ) ( void * Ctx , LLVMMemoryBufferRef * ObjInOut )
"""
A function for applying transformations to an object file buffer.

Implementations of this type are responsible for managing the lifetime of the memory buffer pointed to by ObjInOut: If the [`LLVMMemoryBufferRef`](@ref) value is overwritten then the function is responsible for disposing of the incoming buffer. If the buffer is simply accessed/mutated in-place then ownership returns to the caller and the function does not need to do any lifetime management.

The transform is allowed to return an error, in which case the ObjInOut buffer should be disposed of and set to null.
"""
const LLVMOrcObjectTransformLayerTransformFunction = Ptr{Cvoid}

"""
A reference to an orc::IndirectStubsManager instance.
"""
const LLVMOrcIndirectStubsManagerRef = Ptr{LLVMOrcOpaqueIndirectStubsManager}

"""
A reference to an orc::LazyCallThroughManager instance.
"""
const LLVMOrcLazyCallThroughManagerRef = Ptr{LLVMOrcOpaqueLazyCallThroughManager}

"""
A reference to an orc::DumpObjects object.

Can be used to dump object files to disk with unique names. Useful as an ObjectTransformLayer transform.
"""
const LLVMOrcDumpObjectsRef = Ptr{LLVMOrcOpaqueDumpObjects}

"""
    LLVMOrcExecutionSessionSetErrorReporter(ES, ReportError, Ctx)

Attach a custom error reporter function to the ExecutionSession.

The error reporter will be called to deliver failure notices that can not be directly reported to a caller. For example, failure to resolve symbols in the JIT linker is typically reported via the error reporter (callers requesting definitions from the JIT will typically be delivered a FailureToMaterialize error instead).
"""
function LLVMOrcExecutionSessionSetErrorReporter(ES, ReportError, Ctx)
    ccall((:LLVMOrcExecutionSessionSetErrorReporter, libllvm), Cvoid, (LLVMOrcExecutionSessionRef, LLVMOrcErrorReporterFunction, Ptr{Cvoid}), ES, ReportError, Ctx)
end

"""
    LLVMOrcExecutionSessionGetSymbolStringPool(ES)

Return a reference to the SymbolStringPool for an ExecutionSession.

Ownership of the pool remains with the ExecutionSession: The caller is not required to free the pool.
"""
function LLVMOrcExecutionSessionGetSymbolStringPool(ES)
    ccall((:LLVMOrcExecutionSessionGetSymbolStringPool, libllvm), LLVMOrcSymbolStringPoolRef, (LLVMOrcExecutionSessionRef,), ES)
end

"""
    LLVMOrcSymbolStringPoolClearDeadEntries(SSP)

Clear all unreferenced symbol string pool entries.

This can be called at any time to release unused entries in the ExecutionSession's string pool. Since it locks the pool (preventing interning of any new strings) it is recommended that it only be called infrequently, ideally when the caller has reason to believe that some entries will have become unreferenced, e.g. after removing a module or closing a JITDylib.
"""
function LLVMOrcSymbolStringPoolClearDeadEntries(SSP)
    ccall((:LLVMOrcSymbolStringPoolClearDeadEntries, libllvm), Cvoid, (LLVMOrcSymbolStringPoolRef,), SSP)
end

"""
    LLVMOrcExecutionSessionIntern(ES, Name)

Intern a string in the ExecutionSession's SymbolStringPool and return a reference to it. This increments the ref-count of the pool entry, and the returned value should be released once the client is done with it by calling LLVMOrReleaseSymbolStringPoolEntry.

Since strings are uniqued within the SymbolStringPool LLVMOrcSymbolStringPoolEntryRefs can be compared by value to test string equality.

Note that this function does not perform linker-mangling on the string.
"""
function LLVMOrcExecutionSessionIntern(ES, Name)
    ccall((:LLVMOrcExecutionSessionIntern, libllvm), LLVMOrcSymbolStringPoolEntryRef, (LLVMOrcExecutionSessionRef, Cstring), ES, Name)
end

# typedef void ( * LLVMOrcExecutionSessionLookupHandleResultFunction ) ( LLVMErrorRef Err , LLVMOrcCSymbolMapPairs Result , size_t NumPairs , void * Ctx )
"""
Callback type for ExecutionSession lookups.

If Err is [`LLVMErrorSuccess`](@ref) then Result will contain a pointer to a list of ( SymbolStringPtr, JITEvaluatedSymbol ) pairs of length NumPairs.

If Err is a failure value then Result and Ctx are undefined and should not be accessed. The Callback is responsible for handling the error value (e.g. by calling [`LLVMGetErrorMessage`](@ref) + [`LLVMDisposeErrorMessage`](@ref)).

The caller retains ownership of the Result array and will release all contained symbol names. Clients are responsible for retaining any symbol names that they wish to hold after the function returns.
"""
const LLVMOrcExecutionSessionLookupHandleResultFunction = Ptr{Cvoid}

"""
    LLVMOrcExecutionSessionLookup(ES, K, SearchOrder, SearchOrderSize, Symbols, SymbolsSize, HandleResult, Ctx)

Look up symbols in an execution session.

This is a wrapper around the general ExecutionSession::lookup function.

The SearchOrder argument contains a list of (JITDylibs, JITDylibSearchFlags) pairs that describe the search order. The JITDylibs will be searched in the given order to try to find the symbols in the Symbols argument.

The Symbols argument should contain a null-terminated array of (SymbolStringPtr, SymbolLookupFlags) pairs describing the symbols to be searched for. This function takes ownership of the elements of the Symbols array. The Name fields of the Symbols elements are taken to have been retained by the client for this function. The client should *not* release the Name fields, but are still responsible for destroying the array itself.

The HandleResult function will be called once all searched for symbols have been found, or an error occurs. The HandleResult function will be passed an [`LLVMErrorRef`](@ref) indicating success or failure, and (on success) a null-terminated [`LLVMOrcCSymbolMapPairs`](@ref) array containing the function result, and the Ctx value passed to the lookup function.

The client is fully responsible for managing the lifetime of the Ctx object. A common idiom is to allocate the context prior to the lookup and deallocate it in the handler.

THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
"""
function LLVMOrcExecutionSessionLookup(ES, K, SearchOrder, SearchOrderSize, Symbols, SymbolsSize, HandleResult, Ctx)
    ccall((:LLVMOrcExecutionSessionLookup, libllvm), Cvoid, (LLVMOrcExecutionSessionRef, LLVMOrcLookupKind, LLVMOrcCJITDylibSearchOrder, Csize_t, LLVMOrcCLookupSet, Csize_t, LLVMOrcExecutionSessionLookupHandleResultFunction, Ptr{Cvoid}), ES, K, SearchOrder, SearchOrderSize, Symbols, SymbolsSize, HandleResult, Ctx)
end

"""
    LLVMOrcRetainSymbolStringPoolEntry(S)

Increments the ref-count for a SymbolStringPool entry.
"""
function LLVMOrcRetainSymbolStringPoolEntry(S)
    ccall((:LLVMOrcRetainSymbolStringPoolEntry, libllvm), Cvoid, (LLVMOrcSymbolStringPoolEntryRef,), S)
end

"""
    LLVMOrcReleaseSymbolStringPoolEntry(S)

Reduces the ref-count for of a SymbolStringPool entry.
"""
function LLVMOrcReleaseSymbolStringPoolEntry(S)
    ccall((:LLVMOrcReleaseSymbolStringPoolEntry, libllvm), Cvoid, (LLVMOrcSymbolStringPoolEntryRef,), S)
end

"""
    LLVMOrcSymbolStringPoolEntryStr(S)

Return the c-string for the given symbol. This string will remain valid until the entry is freed (once all LLVMOrcSymbolStringPoolEntryRefs have been released).
"""
function LLVMOrcSymbolStringPoolEntryStr(S)
    ccall((:LLVMOrcSymbolStringPoolEntryStr, libllvm), Cstring, (LLVMOrcSymbolStringPoolEntryRef,), S)
end

"""
    LLVMOrcReleaseResourceTracker(RT)

Reduces the ref-count of a ResourceTracker.
"""
function LLVMOrcReleaseResourceTracker(RT)
    ccall((:LLVMOrcReleaseResourceTracker, libllvm), Cvoid, (LLVMOrcResourceTrackerRef,), RT)
end

"""
    LLVMOrcResourceTrackerTransferTo(SrcRT, DstRT)

Transfers tracking of all resources associated with resource tracker SrcRT to resource tracker DstRT.
"""
function LLVMOrcResourceTrackerTransferTo(SrcRT, DstRT)
    ccall((:LLVMOrcResourceTrackerTransferTo, libllvm), Cvoid, (LLVMOrcResourceTrackerRef, LLVMOrcResourceTrackerRef), SrcRT, DstRT)
end

"""
    LLVMOrcResourceTrackerRemove(RT)

Remove all resources associated with the given tracker. See ResourceTracker::remove().
"""
function LLVMOrcResourceTrackerRemove(RT)
    ccall((:LLVMOrcResourceTrackerRemove, libllvm), LLVMErrorRef, (LLVMOrcResourceTrackerRef,), RT)
end

"""
    LLVMOrcDisposeDefinitionGenerator(DG)

Dispose of a JITDylib::DefinitionGenerator. This should only be called if ownership has not been passed to a JITDylib (e.g. because some error prevented the client from calling [`LLVMOrcJITDylibAddGenerator`](@ref)).
"""
function LLVMOrcDisposeDefinitionGenerator(DG)
    ccall((:LLVMOrcDisposeDefinitionGenerator, libllvm), Cvoid, (LLVMOrcDefinitionGeneratorRef,), DG)
end

"""
    LLVMOrcDisposeMaterializationUnit(MU)

Dispose of a MaterializationUnit.
"""
function LLVMOrcDisposeMaterializationUnit(MU)
    ccall((:LLVMOrcDisposeMaterializationUnit, libllvm), Cvoid, (LLVMOrcMaterializationUnitRef,), MU)
end

"""
    LLVMOrcCreateCustomMaterializationUnit(Name, Ctx, Syms, NumSyms, InitSym, Materialize, Discard, Destroy)

Create a custom MaterializationUnit.

Name is a name for this MaterializationUnit to be used for identification and logging purposes (e.g. if this MaterializationUnit produces an object buffer then the name of that buffer will be derived from this name).

The Syms list contains the names and linkages of the symbols provided by this unit. This function takes ownership of the elements of the Syms array. The Name fields of the array elements are taken to have been retained for this function. The client should *not* release the elements of the array, but is still responsible for destroying the array itself.

The InitSym argument indicates whether or not this MaterializationUnit contains static initializers. If three are no static initializers (the common case) then this argument should be null. If there are static initializers then InitSym should be set to a unique name that also appears in the Syms list with the LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly flag set. This function takes ownership of the InitSym, which should have been retained twice on behalf of this function: once for the Syms entry and once for InitSym. If clients wish to use the InitSym value after this function returns they must retain it once more for themselves.

If any of the symbols in the Syms list is looked up then the Materialize function will be called.

If any of the symbols in the Syms list is overridden then the Discard function will be called.

The caller owns the underling MaterializationUnit and is responsible for either passing it to a JITDylib (via [`LLVMOrcJITDylibDefine`](@ref)) or disposing of it by calling [`LLVMOrcDisposeMaterializationUnit`](@ref).
"""
function LLVMOrcCreateCustomMaterializationUnit(Name, Ctx, Syms, NumSyms, InitSym, Materialize, Discard, Destroy)
    ccall((:LLVMOrcCreateCustomMaterializationUnit, libllvm), LLVMOrcMaterializationUnitRef, (Cstring, Ptr{Cvoid}, LLVMOrcCSymbolFlagsMapPairs, Csize_t, LLVMOrcSymbolStringPoolEntryRef, LLVMOrcMaterializationUnitMaterializeFunction, LLVMOrcMaterializationUnitDiscardFunction, LLVMOrcMaterializationUnitDestroyFunction), Name, Ctx, Syms, NumSyms, InitSym, Materialize, Discard, Destroy)
end

"""
    LLVMOrcAbsoluteSymbols(Syms, NumPairs)

Create a MaterializationUnit to define the given symbols as pointing to the corresponding raw addresses.

This function takes ownership of the elements of the Syms array. The Name fields of the array elements are taken to have been retained for this function. This allows the following pattern...

size\\_t NumPairs; [`LLVMOrcCSymbolMapPairs`](@ref) Sym; -- Build Syms array -- [`LLVMOrcMaterializationUnitRef`](@ref) MU = [`LLVMOrcAbsoluteSymbols`](@ref)(Syms, NumPairs);

... without requiring cleanup of the elements of the Sym array afterwards.

The client is still responsible for deleting the Sym array itself.

If a client wishes to reuse elements of the Sym array after this call they must explicitly retain each of the elements for themselves.
"""
function LLVMOrcAbsoluteSymbols(Syms, NumPairs)
    ccall((:LLVMOrcAbsoluteSymbols, libllvm), LLVMOrcMaterializationUnitRef, (LLVMOrcCSymbolMapPairs, Csize_t), Syms, NumPairs)
end

"""
    LLVMOrcLazyReexports(LCTM, ISM, SourceRef, CallableAliases, NumPairs)

Create a MaterializationUnit to define lazy re-expots. These are callable entry points that call through to the given symbols.

This function takes ownership of the CallableAliases array. The Name fields of the array elements are taken to have been retained for this function. This allows the following pattern...

size\\_t NumPairs; [`LLVMOrcCSymbolAliasMapPairs`](@ref) CallableAliases; -- Build CallableAliases array -- [`LLVMOrcMaterializationUnitRef`](@ref) MU = [`LLVMOrcLazyReexports`](@ref)(LCTM, ISM, JD, CallableAliases, NumPairs);

... without requiring cleanup of the elements of the CallableAliases array afterwards.

The client is still responsible for deleting the CallableAliases array itself.

If a client wishes to reuse elements of the CallableAliases array after this call they must explicitly retain each of the elements for themselves.
"""
function LLVMOrcLazyReexports(LCTM, ISM, SourceRef, CallableAliases, NumPairs)
    ccall((:LLVMOrcLazyReexports, libllvm), LLVMOrcMaterializationUnitRef, (LLVMOrcLazyCallThroughManagerRef, LLVMOrcIndirectStubsManagerRef, LLVMOrcJITDylibRef, LLVMOrcCSymbolAliasMapPairs, Csize_t), LCTM, ISM, SourceRef, CallableAliases, NumPairs)
end

"""
    LLVMOrcDisposeMaterializationResponsibility(MR)

Disposes of the passed MaterializationResponsibility object.

This should only be done after the symbols covered by the object have either been resolved and emitted (via [`LLVMOrcMaterializationResponsibilityNotifyResolved`](@ref) and [`LLVMOrcMaterializationResponsibilityNotifyEmitted`](@ref)) or failed (via [`LLVMOrcMaterializationResponsibilityFailMaterialization`](@ref)).
"""
function LLVMOrcDisposeMaterializationResponsibility(MR)
    ccall((:LLVMOrcDisposeMaterializationResponsibility, libllvm), Cvoid, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityGetTargetDylib(MR)

Returns the target JITDylib that these symbols are being materialized into.
"""
function LLVMOrcMaterializationResponsibilityGetTargetDylib(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetTargetDylib, libllvm), LLVMOrcJITDylibRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityGetExecutionSession(MR)

Returns the ExecutionSession for this MaterializationResponsibility.
"""
function LLVMOrcMaterializationResponsibilityGetExecutionSession(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetExecutionSession, libllvm), LLVMOrcExecutionSessionRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityGetSymbols(MR, NumPairs)

Returns the symbol flags map for this responsibility instance.

The length of the array is returned in NumPairs and the caller is responsible for the returned memory and needs to call [`LLVMOrcDisposeCSymbolFlagsMap`](@ref).

To use the returned symbols beyond the livetime of the MaterializationResponsibility requires the caller to retain the symbols explicitly.
"""
function LLVMOrcMaterializationResponsibilityGetSymbols(MR, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityGetSymbols, libllvm), LLVMOrcCSymbolFlagsMapPairs, (LLVMOrcMaterializationResponsibilityRef, Ptr{Csize_t}), MR, NumPairs)
end

"""
    LLVMOrcDisposeCSymbolFlagsMap(Pairs)

Disposes of the passed LLVMOrcCSymbolFlagsMap.

Does not release the entries themselves.
"""
function LLVMOrcDisposeCSymbolFlagsMap(Pairs)
    ccall((:LLVMOrcDisposeCSymbolFlagsMap, libllvm), Cvoid, (LLVMOrcCSymbolFlagsMapPairs,), Pairs)
end

"""
    LLVMOrcMaterializationResponsibilityGetInitializerSymbol(MR)

Returns the initialization pseudo-symbol, if any. This symbol will also be present in the SymbolFlagsMap for this MaterializationResponsibility object.

The returned symbol is not retained over any mutating operation of the MaterializationResponsbility or beyond the lifetime thereof.
"""
function LLVMOrcMaterializationResponsibilityGetInitializerSymbol(MR)
    ccall((:LLVMOrcMaterializationResponsibilityGetInitializerSymbol, libllvm), LLVMOrcSymbolStringPoolEntryRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityGetRequestedSymbols(MR, NumSymbols)

Returns the names of any symbols covered by this MaterializationResponsibility object that have queries pending. This information can be used to return responsibility for unrequested symbols back to the JITDylib via the delegate method.
"""
function LLVMOrcMaterializationResponsibilityGetRequestedSymbols(MR, NumSymbols)
    ccall((:LLVMOrcMaterializationResponsibilityGetRequestedSymbols, libllvm), Ptr{LLVMOrcSymbolStringPoolEntryRef}, (LLVMOrcMaterializationResponsibilityRef, Ptr{Csize_t}), MR, NumSymbols)
end

"""
    LLVMOrcDisposeSymbols(Symbols)

Disposes of the passed [`LLVMOrcSymbolStringPoolEntryRef`](@ref)* .

Does not release the symbols themselves.
"""
function LLVMOrcDisposeSymbols(Symbols)
    ccall((:LLVMOrcDisposeSymbols, libllvm), Cvoid, (Ptr{LLVMOrcSymbolStringPoolEntryRef},), Symbols)
end

"""
    LLVMOrcMaterializationResponsibilityNotifyResolved(MR, Symbols, NumPairs)

Notifies the target JITDylib that the given symbols have been resolved. This will update the given symbols' addresses in the JITDylib, and notify any pending queries on the given symbols of their resolution. The given symbols must be ones covered by this MaterializationResponsibility instance. Individual calls to this method may resolve a subset of the symbols, but all symbols must have been resolved prior to calling emit.

This method will return an error if any symbols being resolved have been moved to the error state due to the failure of a dependency. If this method returns an error then clients should log it and call [`LLVMOrcMaterializationResponsibilityFailMaterialization`](@ref). If no dependencies have been registered for the symbols covered by this MaterializationResponsibiility then this method is guaranteed to return [`LLVMErrorSuccess`](@ref).
"""
function LLVMOrcMaterializationResponsibilityNotifyResolved(MR, Symbols, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityNotifyResolved, libllvm), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcCSymbolMapPairs, Csize_t), MR, Symbols, NumPairs)
end

"""
    LLVMOrcMaterializationResponsibilityNotifyEmitted(MR)

Notifies the target JITDylib (and any pending queries on that JITDylib) that all symbols covered by this MaterializationResponsibility instance have been emitted.

This method will return an error if any symbols being resolved have been moved to the error state due to the failure of a dependency. If this method returns an error then clients should log it and call [`LLVMOrcMaterializationResponsibilityFailMaterialization`](@ref). If no dependencies have been registered for the symbols covered by this MaterializationResponsibiility then this method is guaranteed to return [`LLVMErrorSuccess`](@ref).
"""
function LLVMOrcMaterializationResponsibilityNotifyEmitted(MR)
    ccall((:LLVMOrcMaterializationResponsibilityNotifyEmitted, libllvm), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityDefineMaterializing(MR, Pairs, NumPairs)

Attempt to claim responsibility for new definitions. This method can be used to claim responsibility for symbols that are added to a materialization unit during the compilation process (e.g. literal pool symbols). Symbol linkage rules are the same as for symbols that are defined up front: duplicate strong definitions will result in errors. Duplicate weak definitions will be discarded (in which case they will not be added to this responsibility instance).

This method can be used by materialization units that want to add additional symbols at materialization time (e.g. stubs, compile callbacks, metadata)
"""
function LLVMOrcMaterializationResponsibilityDefineMaterializing(MR, Pairs, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityDefineMaterializing, libllvm), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcCSymbolFlagsMapPairs, Csize_t), MR, Pairs, NumPairs)
end

"""
    LLVMOrcMaterializationResponsibilityFailMaterialization(MR)

Notify all not-yet-emitted covered by this MaterializationResponsibility instance that an error has occurred. This will remove all symbols covered by this MaterializationResponsibilty from the target JITDylib, and send an error to any queries waiting on these symbols.
"""
function LLVMOrcMaterializationResponsibilityFailMaterialization(MR)
    ccall((:LLVMOrcMaterializationResponsibilityFailMaterialization, libllvm), Cvoid, (LLVMOrcMaterializationResponsibilityRef,), MR)
end

"""
    LLVMOrcMaterializationResponsibilityReplace(MR, MU)

Transfers responsibility to the given MaterializationUnit for all symbols defined by that MaterializationUnit. This allows materializers to break up work based on run-time information (e.g. by introspecting which symbols have actually been looked up and materializing only those).
"""
function LLVMOrcMaterializationResponsibilityReplace(MR, MU)
    ccall((:LLVMOrcMaterializationResponsibilityReplace, libllvm), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcMaterializationUnitRef), MR, MU)
end

"""
    LLVMOrcMaterializationResponsibilityDelegate(MR, Symbols, NumSymbols, Result)

Delegates responsibility for the given symbols to the returned materialization responsibility. Useful for breaking up work between threads, or different kinds of materialization processes.

The caller retains responsibility of the the passed MaterializationResponsibility.
"""
function LLVMOrcMaterializationResponsibilityDelegate(MR, Symbols, NumSymbols, Result)
    ccall((:LLVMOrcMaterializationResponsibilityDelegate, libllvm), LLVMErrorRef, (LLVMOrcMaterializationResponsibilityRef, Ptr{LLVMOrcSymbolStringPoolEntryRef}, Csize_t, Ptr{LLVMOrcMaterializationResponsibilityRef}), MR, Symbols, NumSymbols, Result)
end

"""
    LLVMOrcMaterializationResponsibilityAddDependencies(MR, Name, Dependencies, NumPairs)

Adds dependencies to a symbol that the MaterializationResponsibility is responsible for.

This function takes ownership of Dependencies struct. The Names array have been retained for this function. This allows the following pattern...

[`LLVMOrcSymbolStringPoolEntryRef`](@ref) Names[] = {...}; [`LLVMOrcCDependenceMapPair`](@ref) Dependence = {JD, {Names, sizeof(Names)}} [`LLVMOrcMaterializationResponsibilityAddDependencies`](@ref)(JD, Name, &Dependence, 1);

... without requiring cleanup of the elements of the Names array afterwards.

The client is still responsible for deleting the Dependencies.Names array itself.
"""
function LLVMOrcMaterializationResponsibilityAddDependencies(MR, Name, Dependencies, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityAddDependencies, libllvm), Cvoid, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcSymbolStringPoolEntryRef, LLVMOrcCDependenceMapPairs, Csize_t), MR, Name, Dependencies, NumPairs)
end

"""
    LLVMOrcMaterializationResponsibilityAddDependenciesForAll(MR, Dependencies, NumPairs)

Adds dependencies to all symbols that the MaterializationResponsibility is responsible for. See [`LLVMOrcMaterializationResponsibilityAddDependencies`](@ref) for notes about memory responsibility.
"""
function LLVMOrcMaterializationResponsibilityAddDependenciesForAll(MR, Dependencies, NumPairs)
    ccall((:LLVMOrcMaterializationResponsibilityAddDependenciesForAll, libllvm), Cvoid, (LLVMOrcMaterializationResponsibilityRef, LLVMOrcCDependenceMapPairs, Csize_t), MR, Dependencies, NumPairs)
end

"""
    LLVMOrcExecutionSessionCreateBareJITDylib(ES, Name)

Create a "bare" JITDylib.

The client is responsible for ensuring that the JITDylib's name is unique, e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.

This call does not install any library code or symbols into the newly created JITDylib. The client is responsible for all configuration.
"""
function LLVMOrcExecutionSessionCreateBareJITDylib(ES, Name)
    ccall((:LLVMOrcExecutionSessionCreateBareJITDylib, libllvm), LLVMOrcJITDylibRef, (LLVMOrcExecutionSessionRef, Cstring), ES, Name)
end

"""
    LLVMOrcExecutionSessionCreateJITDylib(ES, Result, Name)

Create a JITDylib.

The client is responsible for ensuring that the JITDylib's name is unique, e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.

If a Platform is attached to the ExecutionSession then Platform::setupJITDylib will be called to install standard platform symbols (e.g. standard library interposes). If no Platform is installed then this call is equivalent to LLVMExecutionSessionRefCreateBareJITDylib and will always return success.
"""
function LLVMOrcExecutionSessionCreateJITDylib(ES, Result, Name)
    ccall((:LLVMOrcExecutionSessionCreateJITDylib, libllvm), LLVMErrorRef, (LLVMOrcExecutionSessionRef, Ptr{LLVMOrcJITDylibRef}, Cstring), ES, Result, Name)
end

"""
    LLVMOrcExecutionSessionGetJITDylibByName(ES, Name)

Returns the JITDylib with the given name, or NULL if no such JITDylib exists.
"""
function LLVMOrcExecutionSessionGetJITDylibByName(ES, Name)
    ccall((:LLVMOrcExecutionSessionGetJITDylibByName, libllvm), LLVMOrcJITDylibRef, (LLVMOrcExecutionSessionRef, Cstring), ES, Name)
end

"""
    LLVMOrcJITDylibCreateResourceTracker(JD)

Return a reference to a newly created resource tracker associated with JD. The tracker is returned with an initial ref-count of 1, and must be released with [`LLVMOrcReleaseResourceTracker`](@ref) when no longer needed.
"""
function LLVMOrcJITDylibCreateResourceTracker(JD)
    ccall((:LLVMOrcJITDylibCreateResourceTracker, libllvm), LLVMOrcResourceTrackerRef, (LLVMOrcJITDylibRef,), JD)
end

"""
    LLVMOrcJITDylibGetDefaultResourceTracker(JD)

Return a reference to the default resource tracker for the given JITDylib. This operation will increase the retain count of the tracker: Clients should call [`LLVMOrcReleaseResourceTracker`](@ref) when the result is no longer needed.
"""
function LLVMOrcJITDylibGetDefaultResourceTracker(JD)
    ccall((:LLVMOrcJITDylibGetDefaultResourceTracker, libllvm), LLVMOrcResourceTrackerRef, (LLVMOrcJITDylibRef,), JD)
end

"""
    LLVMOrcJITDylibDefine(JD, MU)

Add the given MaterializationUnit to the given JITDylib.

If this operation succeeds then JITDylib JD will take ownership of MU. If the operation fails then ownership remains with the caller who should call [`LLVMOrcDisposeMaterializationUnit`](@ref) to destroy it.
"""
function LLVMOrcJITDylibDefine(JD, MU)
    ccall((:LLVMOrcJITDylibDefine, libllvm), LLVMErrorRef, (LLVMOrcJITDylibRef, LLVMOrcMaterializationUnitRef), JD, MU)
end

"""
    LLVMOrcJITDylibClear(JD)

Calls remove on all trackers associated with this JITDylib, see JITDylib::clear().
"""
function LLVMOrcJITDylibClear(JD)
    ccall((:LLVMOrcJITDylibClear, libllvm), LLVMErrorRef, (LLVMOrcJITDylibRef,), JD)
end

"""
    LLVMOrcJITDylibAddGenerator(JD, DG)

Add a DefinitionGenerator to the given JITDylib.

The JITDylib will take ownership of the given generator: The client is no longer responsible for managing its memory.
"""
function LLVMOrcJITDylibAddGenerator(JD, DG)
    ccall((:LLVMOrcJITDylibAddGenerator, libllvm), Cvoid, (LLVMOrcJITDylibRef, LLVMOrcDefinitionGeneratorRef), JD, DG)
end

"""
    LLVMOrcCreateCustomCAPIDefinitionGenerator(F, Ctx, Dispose)

Create a custom generator.

The F argument will be used to implement the DefinitionGenerator's tryToGenerate method (see [`LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction`](@ref)).

Ctx is a context object that will be passed to F. This argument is permitted to be null.

Dispose is the disposal function for Ctx. This argument is permitted to be null (in which case the client is responsible for the lifetime of Ctx).
"""
function LLVMOrcCreateCustomCAPIDefinitionGenerator(F, Ctx, Dispose)
    ccall((:LLVMOrcCreateCustomCAPIDefinitionGenerator, libllvm), LLVMOrcDefinitionGeneratorRef, (LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction, Ptr{Cvoid}, LLVMOrcDisposeCAPIDefinitionGeneratorFunction), F, Ctx, Dispose)
end

"""
    LLVMOrcLookupStateContinueLookup(S, Err)

Continue a lookup that was suspended in a generator (see [`LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction`](@ref)).
"""
function LLVMOrcLookupStateContinueLookup(S, Err)
    ccall((:LLVMOrcLookupStateContinueLookup, libllvm), Cvoid, (LLVMOrcLookupStateRef, LLVMErrorRef), S, Err)
end

"""
    LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(Result, GlobalPrefx, Filter, FilterCtx)

Get a DynamicLibrarySearchGenerator that will reflect process symbols into the JITDylib. On success the resulting generator is owned by the client. Ownership is typically transferred by adding the instance to a JITDylib using [`LLVMOrcJITDylibAddGenerator`](@ref),

The GlobalPrefix argument specifies the character that appears on the front of linker-mangled symbols for the target platform (e.g. '\\_' on MachO). If non-null, this character will be stripped from the start of all symbol strings before passing the remaining substring to dlsym.

The optional Filter and Ctx arguments can be used to supply a symbol name filter: Only symbols for which the filter returns true will be visible to JIT'd code. If the Filter argument is null then all process symbols will be visible to JIT'd code. Note that the symbol name passed to the Filter function is the full mangled symbol: The client is responsible for stripping the global prefix if present.
"""
function LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(Result, GlobalPrefx, Filter, FilterCtx)
    ccall((:LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess, libllvm), LLVMErrorRef, (Ptr{LLVMOrcDefinitionGeneratorRef}, Cchar, LLVMOrcSymbolPredicate, Ptr{Cvoid}), Result, GlobalPrefx, Filter, FilterCtx)
end

"""
    LLVMOrcCreateDynamicLibrarySearchGeneratorForPath(Result, FileName, GlobalPrefix, Filter, FilterCtx)

Get a LLVMOrcCreateDynamicLibararySearchGeneratorForPath that will reflect library symbols into the JITDylib. On success the resulting generator is owned by the client. Ownership is typically transferred by adding the instance to a JITDylib using [`LLVMOrcJITDylibAddGenerator`](@ref),

The GlobalPrefix argument specifies the character that appears on the front of linker-mangled symbols for the target platform (e.g. '\\_' on MachO). If non-null, this character will be stripped from the start of all symbol strings before passing the remaining substring to dlsym.

The optional Filter and Ctx arguments can be used to supply a symbol name filter: Only symbols for which the filter returns true will be visible to JIT'd code. If the Filter argument is null then all library symbols will be visible to JIT'd code. Note that the symbol name passed to the Filter function is the full mangled symbol: The client is responsible for stripping the global prefix if present.

THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
"""
function LLVMOrcCreateDynamicLibrarySearchGeneratorForPath(Result, FileName, GlobalPrefix, Filter, FilterCtx)
    ccall((:LLVMOrcCreateDynamicLibrarySearchGeneratorForPath, libllvm), LLVMErrorRef, (Ptr{LLVMOrcDefinitionGeneratorRef}, Cstring, Cchar, LLVMOrcSymbolPredicate, Ptr{Cvoid}), Result, FileName, GlobalPrefix, Filter, FilterCtx)
end

"""
    LLVMOrcCreateStaticLibrarySearchGeneratorForPath(Result, ObjLayer, FileName, TargetTriple)

Get a [`LLVMOrcCreateStaticLibrarySearchGeneratorForPath`](@ref) that will reflect static library symbols into the JITDylib. On success the resulting generator is owned by the client. Ownership is typically transferred by adding the instance to a JITDylib using [`LLVMOrcJITDylibAddGenerator`](@ref),

Call with the optional TargetTriple argument will succeed if the file at the given path is a static library or a MachO universal binary containing a static library that is compatible with the given triple. Otherwise it will return an error.

THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
"""
function LLVMOrcCreateStaticLibrarySearchGeneratorForPath(Result, ObjLayer, FileName, TargetTriple)
    ccall((:LLVMOrcCreateStaticLibrarySearchGeneratorForPath, libllvm), LLVMErrorRef, (Ptr{LLVMOrcDefinitionGeneratorRef}, LLVMOrcObjectLayerRef, Cstring, Cstring), Result, ObjLayer, FileName, TargetTriple)
end

"""
    LLVMOrcCreateNewThreadSafeContext()

Create a ThreadSafeContext containing a new LLVMContext.

Ownership of the underlying ThreadSafeContext data is shared: Clients can and should dispose of their ThreadSafeContext as soon as they no longer need to refer to it directly. Other references (e.g. from ThreadSafeModules) will keep the data alive as long as it is needed.
"""
function LLVMOrcCreateNewThreadSafeContext()
    ccall((:LLVMOrcCreateNewThreadSafeContext, libllvm), LLVMOrcThreadSafeContextRef, ())
end

"""
    LLVMOrcThreadSafeContextGetContext(TSCtx)

Get a reference to the wrapped LLVMContext.
"""
function LLVMOrcThreadSafeContextGetContext(TSCtx)
    ccall((:LLVMOrcThreadSafeContextGetContext, libllvm), LLVMContextRef, (LLVMOrcThreadSafeContextRef,), TSCtx)
end

"""
    LLVMOrcDisposeThreadSafeContext(TSCtx)

Dispose of a ThreadSafeContext.
"""
function LLVMOrcDisposeThreadSafeContext(TSCtx)
    ccall((:LLVMOrcDisposeThreadSafeContext, libllvm), Cvoid, (LLVMOrcThreadSafeContextRef,), TSCtx)
end

"""
    LLVMOrcCreateNewThreadSafeModule(M, TSCtx)

Create a ThreadSafeModule wrapper around the given LLVM module. This takes ownership of the M argument which should not be disposed of or referenced after this function returns.

Ownership of the ThreadSafeModule is unique: If it is transferred to the JIT (e.g. by [`LLVMOrcLLJITAddLLVMIRModule`](@ref)) then the client is no longer responsible for it. If it is not transferred to the JIT then the client should call [`LLVMOrcDisposeThreadSafeModule`](@ref) to dispose of it.
"""
function LLVMOrcCreateNewThreadSafeModule(M, TSCtx)
    ccall((:LLVMOrcCreateNewThreadSafeModule, libllvm), LLVMOrcThreadSafeModuleRef, (LLVMModuleRef, LLVMOrcThreadSafeContextRef), M, TSCtx)
end

"""
    LLVMOrcDisposeThreadSafeModule(TSM)

Dispose of a ThreadSafeModule. This should only be called if ownership has not been passed to LLJIT (e.g. because some error prevented the client from adding this to the JIT).
"""
function LLVMOrcDisposeThreadSafeModule(TSM)
    ccall((:LLVMOrcDisposeThreadSafeModule, libllvm), Cvoid, (LLVMOrcThreadSafeModuleRef,), TSM)
end

"""
    LLVMOrcThreadSafeModuleWithModuleDo(TSM, F, Ctx)

Apply the given function to the module contained in this ThreadSafeModule.
"""
function LLVMOrcThreadSafeModuleWithModuleDo(TSM, F, Ctx)
    ccall((:LLVMOrcThreadSafeModuleWithModuleDo, libllvm), LLVMErrorRef, (LLVMOrcThreadSafeModuleRef, LLVMOrcGenericIRModuleOperationFunction, Ptr{Cvoid}), TSM, F, Ctx)
end

"""
    LLVMOrcJITTargetMachineBuilderDetectHost(Result)

Create a JITTargetMachineBuilder by detecting the host.

On success the client owns the resulting JITTargetMachineBuilder. It must be passed to a consuming operation (e.g. [`LLVMOrcLLJITBuilderSetJITTargetMachineBuilder`](@ref)) or disposed of by calling [`LLVMOrcDisposeJITTargetMachineBuilder`](@ref).
"""
function LLVMOrcJITTargetMachineBuilderDetectHost(Result)
    ccall((:LLVMOrcJITTargetMachineBuilderDetectHost, libllvm), LLVMErrorRef, (Ptr{LLVMOrcJITTargetMachineBuilderRef},), Result)
end

"""
    LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine(TM)

Create a JITTargetMachineBuilder from the given TargetMachine template.

This operation takes ownership of the given TargetMachine and destroys it before returing. The resulting JITTargetMachineBuilder is owned by the client and must be passed to a consuming operation (e.g. [`LLVMOrcLLJITBuilderSetJITTargetMachineBuilder`](@ref)) or disposed of by calling [`LLVMOrcDisposeJITTargetMachineBuilder`](@ref).
"""
function LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine(TM)
    ccall((:LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine, libllvm), LLVMOrcJITTargetMachineBuilderRef, (LLVMTargetMachineRef,), TM)
end

"""
    LLVMOrcDisposeJITTargetMachineBuilder(JTMB)

Dispose of a JITTargetMachineBuilder.
"""
function LLVMOrcDisposeJITTargetMachineBuilder(JTMB)
    ccall((:LLVMOrcDisposeJITTargetMachineBuilder, libllvm), Cvoid, (LLVMOrcJITTargetMachineBuilderRef,), JTMB)
end

"""
    LLVMOrcJITTargetMachineBuilderGetTargetTriple(JTMB)

Returns the target triple for the given JITTargetMachineBuilder as a string.

The caller owns the resulting string as must dispose of it by calling [`LLVMDisposeMessage`](@ref)
"""
function LLVMOrcJITTargetMachineBuilderGetTargetTriple(JTMB)
    ccall((:LLVMOrcJITTargetMachineBuilderGetTargetTriple, libllvm), Cstring, (LLVMOrcJITTargetMachineBuilderRef,), JTMB)
end

"""
    LLVMOrcJITTargetMachineBuilderSetTargetTriple(JTMB, TargetTriple)

Sets the target triple for the given JITTargetMachineBuilder to the given string.
"""
function LLVMOrcJITTargetMachineBuilderSetTargetTriple(JTMB, TargetTriple)
    ccall((:LLVMOrcJITTargetMachineBuilderSetTargetTriple, libllvm), Cvoid, (LLVMOrcJITTargetMachineBuilderRef, Cstring), JTMB, TargetTriple)
end

"""
    LLVMOrcObjectLayerAddObjectFile(ObjLayer, JD, ObjBuffer)

Add an object to an ObjectLayer to the given JITDylib.

Adds a buffer representing an object file to the given JITDylib using the given ObjectLayer instance. This operation transfers ownership of the buffer to the ObjectLayer instance. The buffer should not be disposed of or referenced once this function returns.

Resources associated with the given object will be tracked by the given JITDylib's default ResourceTracker.
"""
function LLVMOrcObjectLayerAddObjectFile(ObjLayer, JD, ObjBuffer)
    ccall((:LLVMOrcObjectLayerAddObjectFile, libllvm), LLVMErrorRef, (LLVMOrcObjectLayerRef, LLVMOrcJITDylibRef, LLVMMemoryBufferRef), ObjLayer, JD, ObjBuffer)
end

"""
    LLVMOrcObjectLayerAddObjectFileWithRT(ObjLayer, RT, ObjBuffer)

Add an object to an ObjectLayer using the given ResourceTracker.

Adds a buffer representing an object file to the given ResourceTracker's JITDylib using the given ObjectLayer instance. This operation transfers ownership of the buffer to the ObjectLayer instance. The buffer should not be disposed of or referenced once this function returns.

Resources associated with the given object will be tracked by ResourceTracker RT.
"""
function LLVMOrcObjectLayerAddObjectFileWithRT(ObjLayer, RT, ObjBuffer)
    ccall((:LLVMOrcObjectLayerAddObjectFileWithRT, libllvm), LLVMErrorRef, (LLVMOrcObjectLayerRef, LLVMOrcResourceTrackerRef, LLVMMemoryBufferRef), ObjLayer, RT, ObjBuffer)
end

"""
    LLVMOrcObjectLayerEmit(ObjLayer, R, ObjBuffer)

Emit an object buffer to an ObjectLayer.

Ownership of the responsibility object and object buffer pass to this function. The client is not responsible for cleanup.
"""
function LLVMOrcObjectLayerEmit(ObjLayer, R, ObjBuffer)
    ccall((:LLVMOrcObjectLayerEmit, libllvm), Cvoid, (LLVMOrcObjectLayerRef, LLVMOrcMaterializationResponsibilityRef, LLVMMemoryBufferRef), ObjLayer, R, ObjBuffer)
end

"""
    LLVMOrcDisposeObjectLayer(ObjLayer)

Dispose of an ObjectLayer.
"""
function LLVMOrcDisposeObjectLayer(ObjLayer)
    ccall((:LLVMOrcDisposeObjectLayer, libllvm), Cvoid, (LLVMOrcObjectLayerRef,), ObjLayer)
end

function LLVMOrcIRTransformLayerEmit(IRTransformLayer, MR, TSM)
    ccall((:LLVMOrcIRTransformLayerEmit, libllvm), Cvoid, (LLVMOrcIRTransformLayerRef, LLVMOrcMaterializationResponsibilityRef, LLVMOrcThreadSafeModuleRef), IRTransformLayer, MR, TSM)
end

"""
    LLVMOrcIRTransformLayerSetTransform(IRTransformLayer, TransformFunction, Ctx)

Set the transform function of the provided transform layer, passing through a pointer to user provided context.
"""
function LLVMOrcIRTransformLayerSetTransform(IRTransformLayer, TransformFunction, Ctx)
    ccall((:LLVMOrcIRTransformLayerSetTransform, libllvm), Cvoid, (LLVMOrcIRTransformLayerRef, LLVMOrcIRTransformLayerTransformFunction, Ptr{Cvoid}), IRTransformLayer, TransformFunction, Ctx)
end

"""
    LLVMOrcObjectTransformLayerSetTransform(ObjTransformLayer, TransformFunction, Ctx)

Set the transform function on an LLVMOrcObjectTransformLayer.
"""
function LLVMOrcObjectTransformLayerSetTransform(ObjTransformLayer, TransformFunction, Ctx)
    ccall((:LLVMOrcObjectTransformLayerSetTransform, libllvm), Cvoid, (LLVMOrcObjectTransformLayerRef, LLVMOrcObjectTransformLayerTransformFunction, Ptr{Cvoid}), ObjTransformLayer, TransformFunction, Ctx)
end

"""
    LLVMOrcCreateLocalIndirectStubsManager(TargetTriple)

Create a LocalIndirectStubsManager from the given target triple.

The resulting IndirectStubsManager is owned by the client and must be disposed of by calling LLVMOrcDisposeDisposeIndirectStubsManager.
"""
function LLVMOrcCreateLocalIndirectStubsManager(TargetTriple)
    ccall((:LLVMOrcCreateLocalIndirectStubsManager, libllvm), LLVMOrcIndirectStubsManagerRef, (Cstring,), TargetTriple)
end

"""
    LLVMOrcDisposeIndirectStubsManager(ISM)

Dispose of an IndirectStubsManager.
"""
function LLVMOrcDisposeIndirectStubsManager(ISM)
    ccall((:LLVMOrcDisposeIndirectStubsManager, libllvm), Cvoid, (LLVMOrcIndirectStubsManagerRef,), ISM)
end

function LLVMOrcCreateLocalLazyCallThroughManager(TargetTriple, ES, ErrorHandlerAddr, LCTM)
    ccall((:LLVMOrcCreateLocalLazyCallThroughManager, libllvm), LLVMErrorRef, (Cstring, LLVMOrcExecutionSessionRef, LLVMOrcJITTargetAddress, Ptr{LLVMOrcLazyCallThroughManagerRef}), TargetTriple, ES, ErrorHandlerAddr, LCTM)
end

"""
    LLVMOrcDisposeLazyCallThroughManager(LCTM)

Dispose of an LazyCallThroughManager.
"""
function LLVMOrcDisposeLazyCallThroughManager(LCTM)
    ccall((:LLVMOrcDisposeLazyCallThroughManager, libllvm), Cvoid, (LLVMOrcLazyCallThroughManagerRef,), LCTM)
end

"""
    LLVMOrcCreateDumpObjects(DumpDir, IdentifierOverride)

Create a DumpObjects instance.

DumpDir specifies the path to write dumped objects to. DumpDir may be empty in which case files will be dumped to the working directory.

IdentifierOverride specifies a file name stem to use when dumping objects. If empty then each MemoryBuffer's identifier will be used (with a .o suffix added if not already present). If an identifier override is supplied it will be used instead, along with an incrementing counter (since all buffers will use the same identifier, the resulting files will be named <ident>.o, <ident>.2.o, <ident>.3.o, and so on). IdentifierOverride should not contain an extension, as a .o suffix will be added by DumpObjects.
"""
function LLVMOrcCreateDumpObjects(DumpDir, IdentifierOverride)
    ccall((:LLVMOrcCreateDumpObjects, libllvm), LLVMOrcDumpObjectsRef, (Cstring, Cstring), DumpDir, IdentifierOverride)
end

"""
    LLVMOrcDisposeDumpObjects(DumpObjects)

Dispose of a DumpObjects instance.
"""
function LLVMOrcDisposeDumpObjects(DumpObjects)
    ccall((:LLVMOrcDisposeDumpObjects, libllvm), Cvoid, (LLVMOrcDumpObjectsRef,), DumpObjects)
end

"""
    LLVMOrcDumpObjects_CallOperator(DumpObjects, ObjBuffer)

Dump the contents of the given MemoryBuffer.
"""
function LLVMOrcDumpObjects_CallOperator(DumpObjects, ObjBuffer)
    ccall((:LLVMOrcDumpObjects_CallOperator, libllvm), LLVMErrorRef, (LLVMOrcDumpObjectsRef, Ptr{LLVMMemoryBufferRef}), DumpObjects, ObjBuffer)
end

"""
    LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(ES)

Create a RTDyldObjectLinkingLayer instance using the standard SectionMemoryManager for memory management.
"""
function LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(ES)
    ccall((:LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager, libllvm), LLVMOrcObjectLayerRef, (LLVMOrcExecutionSessionRef,), ES)
end

"""
    LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener(RTDyldObjLinkingLayer, Listener)

Add the given listener to the given RTDyldObjectLinkingLayer.

Note: Layer must be an RTDyldObjectLinkingLayer instance or behavior is undefined.
"""
function LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener(RTDyldObjLinkingLayer, Listener)
    ccall((:LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener, libllvm), Cvoid, (LLVMOrcObjectLayerRef, LLVMJITEventListenerRef), RTDyldObjLinkingLayer, Listener)
end

"""
    LLVMRemarkType

The type of the emitted remark.
"""
@cenum LLVMRemarkType::UInt32 begin
    LLVMRemarkTypeUnknown = 0
    LLVMRemarkTypePassed = 1
    LLVMRemarkTypeMissed = 2
    LLVMRemarkTypeAnalysis = 3
    LLVMRemarkTypeAnalysisFPCommute = 4
    LLVMRemarkTypeAnalysisAliasing = 5
    LLVMRemarkTypeFailure = 6
end

mutable struct LLVMRemarkOpaqueString end

"""
String containing a buffer and a length. The buffer is not guaranteed to be zero-terminated.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
const LLVMRemarkStringRef = Ptr{LLVMRemarkOpaqueString}

"""
    LLVMRemarkStringGetData(String)

Returns the buffer holding the string.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkStringGetData(String)
    ccall((:LLVMRemarkStringGetData, libllvm), Cstring, (LLVMRemarkStringRef,), String)
end

"""
    LLVMRemarkStringGetLen(String)

Returns the size of the string.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkStringGetLen(String)
    ccall((:LLVMRemarkStringGetLen, libllvm), UInt32, (LLVMRemarkStringRef,), String)
end

mutable struct LLVMRemarkOpaqueDebugLoc end

"""
DebugLoc containing File, Line and Column.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
const LLVMRemarkDebugLocRef = Ptr{LLVMRemarkOpaqueDebugLoc}

"""
    LLVMRemarkDebugLocGetSourceFilePath(DL)

Return the path to the source file for a debug location.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkDebugLocGetSourceFilePath(DL)
    ccall((:LLVMRemarkDebugLocGetSourceFilePath, libllvm), LLVMRemarkStringRef, (LLVMRemarkDebugLocRef,), DL)
end

"""
    LLVMRemarkDebugLocGetSourceLine(DL)

Return the line in the source file for a debug location.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkDebugLocGetSourceLine(DL)
    ccall((:LLVMRemarkDebugLocGetSourceLine, libllvm), UInt32, (LLVMRemarkDebugLocRef,), DL)
end

"""
    LLVMRemarkDebugLocGetSourceColumn(DL)

Return the column in the source file for a debug location.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkDebugLocGetSourceColumn(DL)
    ccall((:LLVMRemarkDebugLocGetSourceColumn, libllvm), UInt32, (LLVMRemarkDebugLocRef,), DL)
end

mutable struct LLVMRemarkOpaqueArg end

"""
Element of the "Args" list. The key might give more information about what the semantics of the value are, e.g. "Callee" will tell you that the value is a symbol that names a function.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
const LLVMRemarkArgRef = Ptr{LLVMRemarkOpaqueArg}

"""
    LLVMRemarkArgGetKey(Arg)

Returns the key of an argument. The key defines what the value is, and the same key can appear multiple times in the list of arguments.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkArgGetKey(Arg)
    ccall((:LLVMRemarkArgGetKey, libllvm), LLVMRemarkStringRef, (LLVMRemarkArgRef,), Arg)
end

"""
    LLVMRemarkArgGetValue(Arg)

Returns the value of an argument. This is a string that can contain newlines.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkArgGetValue(Arg)
    ccall((:LLVMRemarkArgGetValue, libllvm), LLVMRemarkStringRef, (LLVMRemarkArgRef,), Arg)
end

"""
    LLVMRemarkArgGetDebugLoc(Arg)

Returns the debug location that is attached to the value of this argument.

If there is no debug location, the return value will be `NULL`.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkArgGetDebugLoc(Arg)
    ccall((:LLVMRemarkArgGetDebugLoc, libllvm), LLVMRemarkDebugLocRef, (LLVMRemarkArgRef,), Arg)
end

mutable struct LLVMRemarkOpaqueEntry end

"""
A remark emitted by the compiler.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
const LLVMRemarkEntryRef = Ptr{LLVMRemarkOpaqueEntry}

"""
    LLVMRemarkEntryDispose(Remark)

Free the resources used by the remark entry.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryDispose(Remark)
    ccall((:LLVMRemarkEntryDispose, libllvm), Cvoid, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetType(Remark)

The type of the remark. For example, it can allow users to only keep the missed optimizations from the compiler.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetType(Remark)
    ccall((:LLVMRemarkEntryGetType, libllvm), LLVMRemarkType, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetPassName(Remark)

Get the name of the pass that emitted this remark.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetPassName(Remark)
    ccall((:LLVMRemarkEntryGetPassName, libllvm), LLVMRemarkStringRef, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetRemarkName(Remark)

Get an identifier of the remark.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetRemarkName(Remark)
    ccall((:LLVMRemarkEntryGetRemarkName, libllvm), LLVMRemarkStringRef, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetFunctionName(Remark)

Get the name of the function being processed when the remark was emitted.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetFunctionName(Remark)
    ccall((:LLVMRemarkEntryGetFunctionName, libllvm), LLVMRemarkStringRef, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetDebugLoc(Remark)

Returns the debug location that is attached to this remark.

If there is no debug location, the return value will be `NULL`.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetDebugLoc(Remark)
    ccall((:LLVMRemarkEntryGetDebugLoc, libllvm), LLVMRemarkDebugLocRef, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetHotness(Remark)

Return the hotness of the remark.

A hotness of `0` means this value is not set.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetHotness(Remark)
    ccall((:LLVMRemarkEntryGetHotness, libllvm), UInt64, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetNumArgs(Remark)

The number of arguments the remark holds.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetNumArgs(Remark)
    ccall((:LLVMRemarkEntryGetNumArgs, libllvm), UInt32, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetFirstArg(Remark)

Get a new iterator to iterate over a remark's argument.

If there are no arguments in `Remark`, the return value will be `NULL`.

The lifetime of the returned value is bound to the lifetime of `Remark`.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetFirstArg(Remark)
    ccall((:LLVMRemarkEntryGetFirstArg, libllvm), LLVMRemarkArgRef, (LLVMRemarkEntryRef,), Remark)
end

"""
    LLVMRemarkEntryGetNextArg(It, Remark)

Get the next argument in `Remark` from the position of `It`.

Returns `NULL` if there are no more arguments available.

The lifetime of the returned value is bound to the lifetime of `Remark`.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkEntryGetNextArg(It, Remark)
    ccall((:LLVMRemarkEntryGetNextArg, libllvm), LLVMRemarkArgRef, (LLVMRemarkArgRef, LLVMRemarkEntryRef), It, Remark)
end

mutable struct LLVMRemarkOpaqueParser end

const LLVMRemarkParserRef = Ptr{LLVMRemarkOpaqueParser}

"""
    LLVMRemarkParserCreateYAML(Buf, Size)

Creates a remark parser that can be used to parse the buffer located in `Buf` of size `Size` bytes.

`Buf` cannot be `NULL`.

This function should be paired with [`LLVMRemarkParserDispose`](@ref)() to avoid leaking resources.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkParserCreateYAML(Buf, Size)
    ccall((:LLVMRemarkParserCreateYAML, libllvm), LLVMRemarkParserRef, (Ptr{Cvoid}, UInt64), Buf, Size)
end

"""
    LLVMRemarkParserCreateBitstream(Buf, Size)

Creates a remark parser that can be used to parse the buffer located in `Buf` of size `Size` bytes.

`Buf` cannot be `NULL`.

This function should be paired with [`LLVMRemarkParserDispose`](@ref)() to avoid leaking resources.

\\since [`REMARKS_API_VERSION`](@ref)=1
"""
function LLVMRemarkParserCreateBitstream(Buf, Size)
    ccall((:LLVMRemarkParserCreateBitstream, libllvm), LLVMRemarkParserRef, (Ptr{Cvoid}, UInt64), Buf, Size)
end

"""
    LLVMRemarkParserGetNext(Parser)

Returns the next remark in the file.

The value pointed to by the return value needs to be disposed using a call to [`LLVMRemarkEntryDispose`](@ref)().

All the entries in the returned value that are of [`LLVMRemarkStringRef`](@ref) type will become invalidated once a call to [`LLVMRemarkParserDispose`](@ref) is made.

If the parser reaches the end of the buffer, the return value will be `NULL`.

In the case of an error, the return value will be `NULL`, and:

1) [`LLVMRemarkParserHasError`](@ref)() will return `1`.

2) [`LLVMRemarkParserGetErrorMessage`](@ref)() will return a descriptive error message.

An error may occur if:

1) An argument is invalid.

2) There is a parsing error. This can occur on things like malformed YAML.

3) There is a Remark semantic error. This can occur on well-formed files with missing or extra fields.

Here is a quick example of the usage:

``` [`LLVMRemarkParserRef`](@ref) Parser = [`LLVMRemarkParserCreateYAML`](@ref)(Buf, Size); [`LLVMRemarkEntryRef`](@ref) Remark = NULL; while ((Remark = [`LLVMRemarkParserGetNext`](@ref)(Parser))) { // use Remark [`LLVMRemarkEntryDispose`](@ref)(Remark); // Release memory. } bool HasError = [`LLVMRemarkParserHasError`](@ref)(Parser); [`LLVMRemarkParserDispose`](@ref)(Parser); ```

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkParserGetNext(Parser)
    ccall((:LLVMRemarkParserGetNext, libllvm), LLVMRemarkEntryRef, (LLVMRemarkParserRef,), Parser)
end

"""
    LLVMRemarkParserHasError(Parser)

Returns `1` if the parser encountered an error while parsing the buffer.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkParserHasError(Parser)
    ccall((:LLVMRemarkParserHasError, libllvm), LLVMBool, (LLVMRemarkParserRef,), Parser)
end

"""
    LLVMRemarkParserGetErrorMessage(Parser)

Returns a null-terminated string containing an error message.

In case of no error, the result is `NULL`.

The memory of the string is bound to the lifetime of `Parser`. If [`LLVMRemarkParserDispose`](@ref)() is called, the memory of the string will be released.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkParserGetErrorMessage(Parser)
    ccall((:LLVMRemarkParserGetErrorMessage, libllvm), Cstring, (LLVMRemarkParserRef,), Parser)
end

"""
    LLVMRemarkParserDispose(Parser)

Releases all the resources used by `Parser`.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkParserDispose(Parser)
    ccall((:LLVMRemarkParserDispose, libllvm), Cvoid, (LLVMRemarkParserRef,), Parser)
end

"""
    LLVMRemarkVersion()

Returns the version of the remarks library.

\\since [`REMARKS_API_VERSION`](@ref)=0
"""
function LLVMRemarkVersion()
    ccall((:LLVMRemarkVersion, libllvm), UInt32, ())
end

"""
    LLVMLoadLibraryPermanently(Filename)

This function permanently loads the dynamic library at the given path. It is safe to call this function multiple times for the same library.

# See also
sys::DynamicLibrary::LoadLibraryPermanently()
"""
function LLVMLoadLibraryPermanently(Filename)
    ccall((:LLVMLoadLibraryPermanently, libllvm), LLVMBool, (Cstring,), Filename)
end

"""
    LLVMParseCommandLineOptions(argc, argv, Overview)

This function parses the given arguments using the LLVM command line parser. Note that the only stable thing about this function is its signature; you cannot rely on any particular set of command line arguments being interpreted the same way across LLVM versions.

# See also
llvm::cl::ParseCommandLineOptions()
"""
function LLVMParseCommandLineOptions(argc, argv, Overview)
    ccall((:LLVMParseCommandLineOptions, libllvm), Cvoid, (Cint, Ptr{Cstring}, Cstring), argc, argv, Overview)
end

"""
    LLVMSearchForAddressOfSymbol(symbolName)

This function will search through all previously loaded dynamic libraries for the symbol `symbolName`. If it is found, the address of that symbol is returned. If not, null is returned.

# See also
sys::DynamicLibrary::SearchForAddressOfSymbol()
"""
function LLVMSearchForAddressOfSymbol(symbolName)
    ccall((:LLVMSearchForAddressOfSymbol, libllvm), Ptr{Cvoid}, (Cstring,), symbolName)
end

"""
    LLVMAddSymbol(symbolName, symbolValue)

This functions permanently adds the symbol `symbolName` with the value `symbolValue`. These symbols are searched before any libraries.

# See also
sys::DynamicLibrary::AddSymbol()
"""
function LLVMAddSymbol(symbolName, symbolValue)
    ccall((:LLVMAddSymbol, libllvm), Cvoid, (Cstring, Ptr{Cvoid}), symbolName, symbolValue)
end

"""
    LLVMByteOrdering

` LLVMCTarget Target information`

` LLVMC`

@{
"""
@cenum LLVMByteOrdering::UInt32 begin
    LLVMBigEndian = 0
    LLVMLittleEndian = 1
end

const LLVMTargetLibraryInfoRef = Ptr{LLVMOpaqueTargetLibraryInfotData}

"""
    LLVMGetModuleDataLayout(M)

Obtain the data layout for a module.

# See also
Module::getDataLayout()
"""
function LLVMGetModuleDataLayout(M)
    ccall((:LLVMGetModuleDataLayout, libllvm), LLVMTargetDataRef, (LLVMModuleRef,), M)
end

"""
    LLVMSetModuleDataLayout(M, DL)

Set the data layout for a module.

# See also
Module::setDataLayout()
"""
function LLVMSetModuleDataLayout(M, DL)
    ccall((:LLVMSetModuleDataLayout, libllvm), Cvoid, (LLVMModuleRef, LLVMTargetDataRef), M, DL)
end

"""
    LLVMCreateTargetData(StringRep)

Creates target data from a target layout string. See the constructor llvm::DataLayout::DataLayout.
"""
function LLVMCreateTargetData(StringRep)
    ccall((:LLVMCreateTargetData, libllvm), LLVMTargetDataRef, (Cstring,), StringRep)
end

"""
    LLVMDisposeTargetData(TD)

Deallocates a TargetData. See the destructor llvm::DataLayout::~DataLayout.
"""
function LLVMDisposeTargetData(TD)
    ccall((:LLVMDisposeTargetData, libllvm), Cvoid, (LLVMTargetDataRef,), TD)
end

"""
    LLVMAddTargetLibraryInfo(TLI, PM)

Adds target library information to a pass manager. This does not take ownership of the target library info. See the method llvm::PassManagerBase::add.
"""
function LLVMAddTargetLibraryInfo(TLI, PM)
    ccall((:LLVMAddTargetLibraryInfo, libllvm), Cvoid, (LLVMTargetLibraryInfoRef, LLVMPassManagerRef), TLI, PM)
end

"""
    LLVMCopyStringRepOfTargetData(TD)

Converts target data to a target layout string. The string must be disposed with [`LLVMDisposeMessage`](@ref). See the constructor llvm::DataLayout::DataLayout.
"""
function LLVMCopyStringRepOfTargetData(TD)
    ccall((:LLVMCopyStringRepOfTargetData, libllvm), Cstring, (LLVMTargetDataRef,), TD)
end

"""
    LLVMByteOrder(TD)

Returns the byte order of a target, either LLVMBigEndian or LLVMLittleEndian. See the method llvm::DataLayout::isLittleEndian.
"""
function LLVMByteOrder(TD)
    ccall((:LLVMByteOrder, libllvm), LLVMByteOrdering, (LLVMTargetDataRef,), TD)
end

"""
    LLVMPointerSize(TD)

Returns the pointer size in bytes for a target. See the method llvm::DataLayout::getPointerSize.
"""
function LLVMPointerSize(TD)
    ccall((:LLVMPointerSize, libllvm), Cuint, (LLVMTargetDataRef,), TD)
end

"""
    LLVMPointerSizeForAS(TD, AS)

Returns the pointer size in bytes for a target for a specified address space. See the method llvm::DataLayout::getPointerSize.
"""
function LLVMPointerSizeForAS(TD, AS)
    ccall((:LLVMPointerSizeForAS, libllvm), Cuint, (LLVMTargetDataRef, Cuint), TD, AS)
end

"""
    LLVMIntPtrType(TD)

Returns the integer type that is the same size as a pointer on a target. See the method llvm::DataLayout::getIntPtrType.
"""
function LLVMIntPtrType(TD)
    ccall((:LLVMIntPtrType, libllvm), LLVMTypeRef, (LLVMTargetDataRef,), TD)
end

"""
    LLVMIntPtrTypeForAS(TD, AS)

Returns the integer type that is the same size as a pointer on a target. This version allows the address space to be specified. See the method llvm::DataLayout::getIntPtrType.
"""
function LLVMIntPtrTypeForAS(TD, AS)
    ccall((:LLVMIntPtrTypeForAS, libllvm), LLVMTypeRef, (LLVMTargetDataRef, Cuint), TD, AS)
end

"""
    LLVMIntPtrTypeInContext(C, TD)

Returns the integer type that is the same size as a pointer on a target. See the method llvm::DataLayout::getIntPtrType.
"""
function LLVMIntPtrTypeInContext(C, TD)
    ccall((:LLVMIntPtrTypeInContext, libllvm), LLVMTypeRef, (LLVMContextRef, LLVMTargetDataRef), C, TD)
end

"""
    LLVMIntPtrTypeForASInContext(C, TD, AS)

Returns the integer type that is the same size as a pointer on a target. This version allows the address space to be specified. See the method llvm::DataLayout::getIntPtrType.
"""
function LLVMIntPtrTypeForASInContext(C, TD, AS)
    ccall((:LLVMIntPtrTypeForASInContext, libllvm), LLVMTypeRef, (LLVMContextRef, LLVMTargetDataRef, Cuint), C, TD, AS)
end

"""
    LLVMSizeOfTypeInBits(TD, Ty)

Computes the size of a type in bytes for a target. See the method llvm::DataLayout::getTypeSizeInBits.
"""
function LLVMSizeOfTypeInBits(TD, Ty)
    ccall((:LLVMSizeOfTypeInBits, libllvm), Culonglong, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMStoreSizeOfType(TD, Ty)

Computes the storage size of a type in bytes for a target. See the method llvm::DataLayout::getTypeStoreSize.
"""
function LLVMStoreSizeOfType(TD, Ty)
    ccall((:LLVMStoreSizeOfType, libllvm), Culonglong, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMABISizeOfType(TD, Ty)

Computes the ABI size of a type in bytes for a target. See the method llvm::DataLayout::getTypeAllocSize.
"""
function LLVMABISizeOfType(TD, Ty)
    ccall((:LLVMABISizeOfType, libllvm), Culonglong, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMABIAlignmentOfType(TD, Ty)

Computes the ABI alignment of a type in bytes for a target. See the method llvm::DataLayout::getTypeABISize.
"""
function LLVMABIAlignmentOfType(TD, Ty)
    ccall((:LLVMABIAlignmentOfType, libllvm), Cuint, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMCallFrameAlignmentOfType(TD, Ty)

Computes the call frame alignment of a type in bytes for a target. See the method llvm::DataLayout::getTypeABISize.
"""
function LLVMCallFrameAlignmentOfType(TD, Ty)
    ccall((:LLVMCallFrameAlignmentOfType, libllvm), Cuint, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMPreferredAlignmentOfType(TD, Ty)

Computes the preferred alignment of a type in bytes for a target. See the method llvm::DataLayout::getTypeABISize.
"""
function LLVMPreferredAlignmentOfType(TD, Ty)
    ccall((:LLVMPreferredAlignmentOfType, libllvm), Cuint, (LLVMTargetDataRef, LLVMTypeRef), TD, Ty)
end

"""
    LLVMPreferredAlignmentOfGlobal(TD, GlobalVar)

Computes the preferred alignment of a global variable in bytes for a target. See the method llvm::DataLayout::getPreferredAlignment.
"""
function LLVMPreferredAlignmentOfGlobal(TD, GlobalVar)
    ccall((:LLVMPreferredAlignmentOfGlobal, libllvm), Cuint, (LLVMTargetDataRef, LLVMValueRef), TD, GlobalVar)
end

"""
    LLVMElementAtOffset(TD, StructTy, Offset)

Computes the structure element that contains the byte offset for a target. See the method llvm::StructLayout::getElementContainingOffset.
"""
function LLVMElementAtOffset(TD, StructTy, Offset)
    ccall((:LLVMElementAtOffset, libllvm), Cuint, (LLVMTargetDataRef, LLVMTypeRef, Culonglong), TD, StructTy, Offset)
end

"""
    LLVMOffsetOfElement(TD, StructTy, Element)

Computes the byte offset of the indexed struct element for a target. See the method llvm::StructLayout::getElementContainingOffset.
"""
function LLVMOffsetOfElement(TD, StructTy, Element)
    ccall((:LLVMOffsetOfElement, libllvm), Culonglong, (LLVMTargetDataRef, LLVMTypeRef, Cuint), TD, StructTy, Element)
end

const LLVMTargetRef = Ptr{LLVMTarget}

@cenum LLVMCodeGenOptLevel::UInt32 begin
    LLVMCodeGenLevelNone = 0
    LLVMCodeGenLevelLess = 1
    LLVMCodeGenLevelDefault = 2
    LLVMCodeGenLevelAggressive = 3
end

@cenum LLVMRelocMode::UInt32 begin
    LLVMRelocDefault = 0
    LLVMRelocStatic = 1
    LLVMRelocPIC = 2
    LLVMRelocDynamicNoPic = 3
    LLVMRelocROPI = 4
    LLVMRelocRWPI = 5
    LLVMRelocROPI_RWPI = 6
end

@cenum LLVMCodeGenFileType::UInt32 begin
    LLVMAssemblyFile = 0
    LLVMObjectFile = 1
end

"""
    LLVMGetFirstTarget()

Returns the first llvm::Target in the registered targets list.
"""
function LLVMGetFirstTarget()
    ccall((:LLVMGetFirstTarget, libllvm), LLVMTargetRef, ())
end

"""
    LLVMGetNextTarget(T)

Returns the next llvm::Target given a previous one (or null if there's none)
"""
function LLVMGetNextTarget(T)
    ccall((:LLVMGetNextTarget, libllvm), LLVMTargetRef, (LLVMTargetRef,), T)
end

"""
    LLVMGetTargetFromName(Name)

Finds the target corresponding to the given name and stores it in `T`. Returns 0 on success.
"""
function LLVMGetTargetFromName(Name)
    ccall((:LLVMGetTargetFromName, libllvm), LLVMTargetRef, (Cstring,), Name)
end

"""
    LLVMGetTargetFromTriple(Triple, T, ErrorMessage)

Finds the target corresponding to the given triple and stores it in `T`. Returns 0 on success. Optionally returns any error in ErrorMessage. Use [`LLVMDisposeMessage`](@ref) to dispose the message.
"""
function LLVMGetTargetFromTriple(Triple, T, ErrorMessage)
    ccall((:LLVMGetTargetFromTriple, libllvm), LLVMBool, (Cstring, Ptr{LLVMTargetRef}, Ptr{Cstring}), Triple, T, ErrorMessage)
end

"""
    LLVMGetTargetName(T)

Returns the name of a target. See llvm::Target::getName
"""
function LLVMGetTargetName(T)
    ccall((:LLVMGetTargetName, libllvm), Cstring, (LLVMTargetRef,), T)
end

"""
    LLVMGetTargetDescription(T)

Returns the description of a target. See llvm::Target::getDescription
"""
function LLVMGetTargetDescription(T)
    ccall((:LLVMGetTargetDescription, libllvm), Cstring, (LLVMTargetRef,), T)
end

"""
    LLVMTargetHasJIT(T)

Returns if the target has a JIT
"""
function LLVMTargetHasJIT(T)
    ccall((:LLVMTargetHasJIT, libllvm), LLVMBool, (LLVMTargetRef,), T)
end

"""
    LLVMTargetHasTargetMachine(T)

Returns if the target has a TargetMachine associated
"""
function LLVMTargetHasTargetMachine(T)
    ccall((:LLVMTargetHasTargetMachine, libllvm), LLVMBool, (LLVMTargetRef,), T)
end

"""
    LLVMTargetHasAsmBackend(T)

Returns if the target as an ASM backend (required for emitting output)
"""
function LLVMTargetHasAsmBackend(T)
    ccall((:LLVMTargetHasAsmBackend, libllvm), LLVMBool, (LLVMTargetRef,), T)
end

"""
    LLVMCreateTargetMachine(T, Triple, CPU, Features, Level, Reloc, CodeModel)

Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine
"""
function LLVMCreateTargetMachine(T, Triple, CPU, Features, Level, Reloc, CodeModel)
    ccall((:LLVMCreateTargetMachine, libllvm), LLVMTargetMachineRef, (LLVMTargetRef, Cstring, Cstring, Cstring, LLVMCodeGenOptLevel, LLVMRelocMode, LLVMCodeModel), T, Triple, CPU, Features, Level, Reloc, CodeModel)
end

"""
    LLVMDisposeTargetMachine(T)

Dispose the [`LLVMTargetMachineRef`](@ref) instance generated by [`LLVMCreateTargetMachine`](@ref).
"""
function LLVMDisposeTargetMachine(T)
    ccall((:LLVMDisposeTargetMachine, libllvm), Cvoid, (LLVMTargetMachineRef,), T)
end

"""
    LLVMGetTargetMachineTarget(T)

Returns the Target used in a TargetMachine
"""
function LLVMGetTargetMachineTarget(T)
    ccall((:LLVMGetTargetMachineTarget, libllvm), LLVMTargetRef, (LLVMTargetMachineRef,), T)
end

"""
    LLVMGetTargetMachineTriple(T)

Returns the triple used creating this target machine. See llvm::TargetMachine::getTriple. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetTargetMachineTriple(T)
    ccall((:LLVMGetTargetMachineTriple, libllvm), Cstring, (LLVMTargetMachineRef,), T)
end

"""
    LLVMGetTargetMachineCPU(T)

Returns the cpu used creating this target machine. See llvm::TargetMachine::getCPU. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetTargetMachineCPU(T)
    ccall((:LLVMGetTargetMachineCPU, libllvm), Cstring, (LLVMTargetMachineRef,), T)
end

"""
    LLVMGetTargetMachineFeatureString(T)

Returns the feature string used creating this target machine. See llvm::TargetMachine::getFeatureString. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetTargetMachineFeatureString(T)
    ccall((:LLVMGetTargetMachineFeatureString, libllvm), Cstring, (LLVMTargetMachineRef,), T)
end

"""
    LLVMCreateTargetDataLayout(T)

Create a DataLayout based on the targetMachine.
"""
function LLVMCreateTargetDataLayout(T)
    ccall((:LLVMCreateTargetDataLayout, libllvm), LLVMTargetDataRef, (LLVMTargetMachineRef,), T)
end

"""
    LLVMSetTargetMachineAsmVerbosity(T, VerboseAsm)

Set the target machine's ASM verbosity.
"""
function LLVMSetTargetMachineAsmVerbosity(T, VerboseAsm)
    ccall((:LLVMSetTargetMachineAsmVerbosity, libllvm), Cvoid, (LLVMTargetMachineRef, LLVMBool), T, VerboseAsm)
end

"""
    LLVMTargetMachineEmitToFile(T, M, Filename, codegen, ErrorMessage)

Emits an asm or object file for the given module to the filename. This wraps several c++ only classes (among them a file stream). Returns any error in ErrorMessage. Use [`LLVMDisposeMessage`](@ref) to dispose the message.
"""
function LLVMTargetMachineEmitToFile(T, M, Filename, codegen, ErrorMessage)
    ccall((:LLVMTargetMachineEmitToFile, libllvm), LLVMBool, (LLVMTargetMachineRef, LLVMModuleRef, Cstring, LLVMCodeGenFileType, Ptr{Cstring}), T, M, Filename, codegen, ErrorMessage)
end

"""
    LLVMTargetMachineEmitToMemoryBuffer(T, M, codegen, ErrorMessage, OutMemBuf)

Compile the LLVM IR stored in `M` and store the result in `OutMemBuf`.
"""
function LLVMTargetMachineEmitToMemoryBuffer(T, M, codegen, ErrorMessage, OutMemBuf)
    ccall((:LLVMTargetMachineEmitToMemoryBuffer, libllvm), LLVMBool, (LLVMTargetMachineRef, LLVMModuleRef, LLVMCodeGenFileType, Ptr{Cstring}, Ptr{LLVMMemoryBufferRef}), T, M, codegen, ErrorMessage, OutMemBuf)
end

"""
    LLVMGetDefaultTargetTriple()

Get a triple for the host machine as a string. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetDefaultTargetTriple()
    ccall((:LLVMGetDefaultTargetTriple, libllvm), Cstring, ())
end

"""
    LLVMNormalizeTargetTriple(triple)

Normalize a target triple. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMNormalizeTargetTriple(triple)
    ccall((:LLVMNormalizeTargetTriple, libllvm), Cstring, (Cstring,), triple)
end

"""
    LLVMGetHostCPUName()

Get the host CPU as a string. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetHostCPUName()
    ccall((:LLVMGetHostCPUName, libllvm), Cstring, ())
end

"""
    LLVMGetHostCPUFeatures()

Get the host CPU's features as a string. The result needs to be disposed with [`LLVMDisposeMessage`](@ref).
"""
function LLVMGetHostCPUFeatures()
    ccall((:LLVMGetHostCPUFeatures, libllvm), Cstring, ())
end

"""
    LLVMAddAnalysisPasses(T, PM)

Adds the target-specific analysis passes to the pass manager.
"""
function LLVMAddAnalysisPasses(T, PM)
    ccall((:LLVMAddAnalysisPasses, libllvm), Cvoid, (LLVMTargetMachineRef, LLVMPassManagerRef), T, PM)
end

"""
    LLVMAddAggressiveInstCombinerPass(PM)

See llvm::createAggressiveInstCombinerPass function.
"""
function LLVMAddAggressiveInstCombinerPass(PM)
    ccall((:LLVMAddAggressiveInstCombinerPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddConstantMergePass(PM)

See llvm::createConstantMergePass function.
"""
function LLVMAddConstantMergePass(PM)
    ccall((:LLVMAddConstantMergePass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddMergeFunctionsPass(PM)

See llvm::createMergeFunctionsPass function.
"""
function LLVMAddMergeFunctionsPass(PM)
    ccall((:LLVMAddMergeFunctionsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddCalledValuePropagationPass(PM)

See llvm::createCalledValuePropagationPass function.
"""
function LLVMAddCalledValuePropagationPass(PM)
    ccall((:LLVMAddCalledValuePropagationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddDeadArgEliminationPass(PM)

See llvm::createDeadArgEliminationPass function.
"""
function LLVMAddDeadArgEliminationPass(PM)
    ccall((:LLVMAddDeadArgEliminationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddFunctionAttrsPass(PM)

See llvm::createFunctionAttrsPass function.
"""
function LLVMAddFunctionAttrsPass(PM)
    ccall((:LLVMAddFunctionAttrsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddFunctionInliningPass(PM)

See llvm::createFunctionInliningPass function.
"""
function LLVMAddFunctionInliningPass(PM)
    ccall((:LLVMAddFunctionInliningPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddAlwaysInlinerPass(PM)

See llvm::createAlwaysInlinerPass function.
"""
function LLVMAddAlwaysInlinerPass(PM)
    ccall((:LLVMAddAlwaysInlinerPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddGlobalDCEPass(PM)

See llvm::createGlobalDCEPass function.
"""
function LLVMAddGlobalDCEPass(PM)
    ccall((:LLVMAddGlobalDCEPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddGlobalOptimizerPass(PM)

See llvm::createGlobalOptimizerPass function.
"""
function LLVMAddGlobalOptimizerPass(PM)
    ccall((:LLVMAddGlobalOptimizerPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddPruneEHPass(PM)

See llvm::createPruneEHPass function.
"""
function LLVMAddPruneEHPass(PM)
    ccall((:LLVMAddPruneEHPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddIPSCCPPass(PM)

See llvm::createIPSCCPPass function.
"""
function LLVMAddIPSCCPPass(PM)
    ccall((:LLVMAddIPSCCPPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddInternalizePass(arg1, AllButMain)

See llvm::createInternalizePass function.
"""
function LLVMAddInternalizePass(arg1, AllButMain)
    ccall((:LLVMAddInternalizePass, libllvm), Cvoid, (LLVMPassManagerRef, Cuint), arg1, AllButMain)
end

"""
    LLVMAddInternalizePassWithMustPreservePredicate(PM, Context, MustPreserve)

Create and add the internalize pass to the given pass manager with the provided preservation callback.

The context parameter is forwarded to the callback on each invocation. As such, it is the responsibility of the caller to extend its lifetime until execution of this pass has finished.

# See also
llvm::createInternalizePass function.
"""
function LLVMAddInternalizePassWithMustPreservePredicate(PM, Context, MustPreserve)
    ccall((:LLVMAddInternalizePassWithMustPreservePredicate, libllvm), Cvoid, (LLVMPassManagerRef, Ptr{Cvoid}, Ptr{Cvoid}), PM, Context, MustPreserve)
end

"""
    LLVMAddStripDeadPrototypesPass(PM)

See llvm::createStripDeadPrototypesPass function.
"""
function LLVMAddStripDeadPrototypesPass(PM)
    ccall((:LLVMAddStripDeadPrototypesPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddStripSymbolsPass(PM)

See llvm::createStripSymbolsPass function.
"""
function LLVMAddStripSymbolsPass(PM)
    ccall((:LLVMAddStripSymbolsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddInstructionCombiningPass(PM)

See llvm::createInstructionCombiningPass function.
"""
function LLVMAddInstructionCombiningPass(PM)
    ccall((:LLVMAddInstructionCombiningPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

mutable struct LLVMOpaquePassBuilderOptions end

"""
A set of options passed which are attached to the Pass Manager upon run.

This corresponds to an llvm::LLVMPassBuilderOptions instance

The details for how the different properties of this structure are used can be found in the source for [`LLVMRunPasses`](@ref)
"""
const LLVMPassBuilderOptionsRef = Ptr{LLVMOpaquePassBuilderOptions}

"""
    LLVMRunPasses(M, Passes, TM, Options)

Construct and run a set of passes over a module

This function takes a string with the passes that should be used. The format of this string is the same as opt's -passes argument for the new pass manager. Individual passes may be specified, separated by commas. Full pipelines may also be invoked using `default<O3>` and friends. See opt for full reference of the Passes format.
"""
function LLVMRunPasses(M, Passes, TM, Options)
    ccall((:LLVMRunPasses, libllvm), LLVMErrorRef, (LLVMModuleRef, Cstring, LLVMTargetMachineRef, LLVMPassBuilderOptionsRef), M, Passes, TM, Options)
end

"""
    LLVMCreatePassBuilderOptions()

Create a new set of options for a PassBuilder

Ownership of the returned instance is given to the client, and they are responsible for it. The client should call [`LLVMDisposePassBuilderOptions`](@ref) to free the pass builder options.
"""
function LLVMCreatePassBuilderOptions()
    ccall((:LLVMCreatePassBuilderOptions, libllvm), LLVMPassBuilderOptionsRef, ())
end

"""
    LLVMPassBuilderOptionsSetVerifyEach(Options, VerifyEach)

Toggle adding the VerifierPass for the PassBuilder, ensuring all functions inside the module is valid.
"""
function LLVMPassBuilderOptionsSetVerifyEach(Options, VerifyEach)
    ccall((:LLVMPassBuilderOptionsSetVerifyEach, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, VerifyEach)
end

"""
    LLVMPassBuilderOptionsSetDebugLogging(Options, DebugLogging)

Toggle debug logging when running the PassBuilder
"""
function LLVMPassBuilderOptionsSetDebugLogging(Options, DebugLogging)
    ccall((:LLVMPassBuilderOptionsSetDebugLogging, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, DebugLogging)
end

function LLVMPassBuilderOptionsSetLoopInterleaving(Options, LoopInterleaving)
    ccall((:LLVMPassBuilderOptionsSetLoopInterleaving, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, LoopInterleaving)
end

function LLVMPassBuilderOptionsSetLoopVectorization(Options, LoopVectorization)
    ccall((:LLVMPassBuilderOptionsSetLoopVectorization, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, LoopVectorization)
end

function LLVMPassBuilderOptionsSetSLPVectorization(Options, SLPVectorization)
    ccall((:LLVMPassBuilderOptionsSetSLPVectorization, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, SLPVectorization)
end

function LLVMPassBuilderOptionsSetLoopUnrolling(Options, LoopUnrolling)
    ccall((:LLVMPassBuilderOptionsSetLoopUnrolling, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, LoopUnrolling)
end

function LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll(Options, ForgetAllSCEVInLoopUnroll)
    ccall((:LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, ForgetAllSCEVInLoopUnroll)
end

function LLVMPassBuilderOptionsSetLicmMssaOptCap(Options, LicmMssaOptCap)
    ccall((:LLVMPassBuilderOptionsSetLicmMssaOptCap, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, Cuint), Options, LicmMssaOptCap)
end

function LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap(Options, LicmMssaNoAccForPromotionCap)
    ccall((:LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, Cuint), Options, LicmMssaNoAccForPromotionCap)
end

function LLVMPassBuilderOptionsSetCallGraphProfile(Options, CallGraphProfile)
    ccall((:LLVMPassBuilderOptionsSetCallGraphProfile, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, CallGraphProfile)
end

function LLVMPassBuilderOptionsSetMergeFunctions(Options, MergeFunctions)
    ccall((:LLVMPassBuilderOptionsSetMergeFunctions, libllvm), Cvoid, (LLVMPassBuilderOptionsRef, LLVMBool), Options, MergeFunctions)
end

"""
    LLVMDisposePassBuilderOptions(Options)

Dispose of a heap-allocated PassBuilderOptions instance
"""
function LLVMDisposePassBuilderOptions(Options)
    ccall((:LLVMDisposePassBuilderOptions, libllvm), Cvoid, (LLVMPassBuilderOptionsRef,), Options)
end

mutable struct LLVMOpaquePassManagerBuilder end

const LLVMPassManagerBuilderRef = Ptr{LLVMOpaquePassManagerBuilder}

"""
    LLVMPassManagerBuilderCreate()

See llvm::PassManagerBuilder.
"""
function LLVMPassManagerBuilderCreate()
    ccall((:LLVMPassManagerBuilderCreate, libllvm), LLVMPassManagerBuilderRef, ())
end

function LLVMPassManagerBuilderDispose(PMB)
    ccall((:LLVMPassManagerBuilderDispose, libllvm), Cvoid, (LLVMPassManagerBuilderRef,), PMB)
end

"""
    LLVMPassManagerBuilderSetOptLevel(PMB, OptLevel)

See llvm::PassManagerBuilder::OptLevel.
"""
function LLVMPassManagerBuilderSetOptLevel(PMB, OptLevel)
    ccall((:LLVMPassManagerBuilderSetOptLevel, libllvm), Cvoid, (LLVMPassManagerBuilderRef, Cuint), PMB, OptLevel)
end

"""
    LLVMPassManagerBuilderSetSizeLevel(PMB, SizeLevel)

See llvm::PassManagerBuilder::SizeLevel.
"""
function LLVMPassManagerBuilderSetSizeLevel(PMB, SizeLevel)
    ccall((:LLVMPassManagerBuilderSetSizeLevel, libllvm), Cvoid, (LLVMPassManagerBuilderRef, Cuint), PMB, SizeLevel)
end

"""
    LLVMPassManagerBuilderSetDisableUnitAtATime(PMB, Value)

See llvm::PassManagerBuilder::DisableUnitAtATime.
"""
function LLVMPassManagerBuilderSetDisableUnitAtATime(PMB, Value)
    ccall((:LLVMPassManagerBuilderSetDisableUnitAtATime, libllvm), Cvoid, (LLVMPassManagerBuilderRef, LLVMBool), PMB, Value)
end

"""
    LLVMPassManagerBuilderSetDisableUnrollLoops(PMB, Value)

See llvm::PassManagerBuilder::DisableUnrollLoops.
"""
function LLVMPassManagerBuilderSetDisableUnrollLoops(PMB, Value)
    ccall((:LLVMPassManagerBuilderSetDisableUnrollLoops, libllvm), Cvoid, (LLVMPassManagerBuilderRef, LLVMBool), PMB, Value)
end

"""
    LLVMPassManagerBuilderSetDisableSimplifyLibCalls(PMB, Value)

See llvm::PassManagerBuilder::DisableSimplifyLibCalls
"""
function LLVMPassManagerBuilderSetDisableSimplifyLibCalls(PMB, Value)
    ccall((:LLVMPassManagerBuilderSetDisableSimplifyLibCalls, libllvm), Cvoid, (LLVMPassManagerBuilderRef, LLVMBool), PMB, Value)
end

"""
    LLVMPassManagerBuilderUseInlinerWithThreshold(PMB, Threshold)

See llvm::PassManagerBuilder::Inliner.
"""
function LLVMPassManagerBuilderUseInlinerWithThreshold(PMB, Threshold)
    ccall((:LLVMPassManagerBuilderUseInlinerWithThreshold, libllvm), Cvoid, (LLVMPassManagerBuilderRef, Cuint), PMB, Threshold)
end

"""
    LLVMPassManagerBuilderPopulateFunctionPassManager(PMB, PM)

See llvm::PassManagerBuilder::populateFunctionPassManager.
"""
function LLVMPassManagerBuilderPopulateFunctionPassManager(PMB, PM)
    ccall((:LLVMPassManagerBuilderPopulateFunctionPassManager, libllvm), Cvoid, (LLVMPassManagerBuilderRef, LLVMPassManagerRef), PMB, PM)
end

"""
    LLVMPassManagerBuilderPopulateModulePassManager(PMB, PM)

See llvm::PassManagerBuilder::populateModulePassManager.
"""
function LLVMPassManagerBuilderPopulateModulePassManager(PMB, PM)
    ccall((:LLVMPassManagerBuilderPopulateModulePassManager, libllvm), Cvoid, (LLVMPassManagerBuilderRef, LLVMPassManagerRef), PMB, PM)
end

"""
    LLVMAddAggressiveDCEPass(PM)

See llvm::createAggressiveDCEPass function.
"""
function LLVMAddAggressiveDCEPass(PM)
    ccall((:LLVMAddAggressiveDCEPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddDCEPass(PM)

See llvm::createDeadCodeEliminationPass function.
"""
function LLVMAddDCEPass(PM)
    ccall((:LLVMAddDCEPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddBitTrackingDCEPass(PM)

See llvm::createBitTrackingDCEPass function.
"""
function LLVMAddBitTrackingDCEPass(PM)
    ccall((:LLVMAddBitTrackingDCEPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddAlignmentFromAssumptionsPass(PM)

See llvm::createAlignmentFromAssumptionsPass function.
"""
function LLVMAddAlignmentFromAssumptionsPass(PM)
    ccall((:LLVMAddAlignmentFromAssumptionsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddCFGSimplificationPass(PM)

See llvm::createCFGSimplificationPass function.
"""
function LLVMAddCFGSimplificationPass(PM)
    ccall((:LLVMAddCFGSimplificationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddDeadStoreEliminationPass(PM)

See llvm::createDeadStoreEliminationPass function.
"""
function LLVMAddDeadStoreEliminationPass(PM)
    ccall((:LLVMAddDeadStoreEliminationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddScalarizerPass(PM)

See llvm::createScalarizerPass function.
"""
function LLVMAddScalarizerPass(PM)
    ccall((:LLVMAddScalarizerPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddMergedLoadStoreMotionPass(PM)

See llvm::createMergedLoadStoreMotionPass function.
"""
function LLVMAddMergedLoadStoreMotionPass(PM)
    ccall((:LLVMAddMergedLoadStoreMotionPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddGVNPass(PM)

See llvm::createGVNPass function.
"""
function LLVMAddGVNPass(PM)
    ccall((:LLVMAddGVNPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddNewGVNPass(PM)

See llvm::createGVNPass function.
"""
function LLVMAddNewGVNPass(PM)
    ccall((:LLVMAddNewGVNPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddIndVarSimplifyPass(PM)

See llvm::createIndVarSimplifyPass function.
"""
function LLVMAddIndVarSimplifyPass(PM)
    ccall((:LLVMAddIndVarSimplifyPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddInstructionSimplifyPass(PM)

See llvm::createInstSimplifyLegacyPass function.
"""
function LLVMAddInstructionSimplifyPass(PM)
    ccall((:LLVMAddInstructionSimplifyPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddJumpThreadingPass(PM)

See llvm::createJumpThreadingPass function.
"""
function LLVMAddJumpThreadingPass(PM)
    ccall((:LLVMAddJumpThreadingPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLICMPass(PM)

See llvm::createLICMPass function.
"""
function LLVMAddLICMPass(PM)
    ccall((:LLVMAddLICMPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopDeletionPass(PM)

See llvm::createLoopDeletionPass function.
"""
function LLVMAddLoopDeletionPass(PM)
    ccall((:LLVMAddLoopDeletionPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopIdiomPass(PM)

See llvm::createLoopIdiomPass function
"""
function LLVMAddLoopIdiomPass(PM)
    ccall((:LLVMAddLoopIdiomPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopRotatePass(PM)

See llvm::createLoopRotatePass function.
"""
function LLVMAddLoopRotatePass(PM)
    ccall((:LLVMAddLoopRotatePass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopRerollPass(PM)

See llvm::createLoopRerollPass function.
"""
function LLVMAddLoopRerollPass(PM)
    ccall((:LLVMAddLoopRerollPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopUnrollPass(PM)

See llvm::createLoopUnrollPass function.
"""
function LLVMAddLoopUnrollPass(PM)
    ccall((:LLVMAddLoopUnrollPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopUnrollAndJamPass(PM)

See llvm::createLoopUnrollAndJamPass function.
"""
function LLVMAddLoopUnrollAndJamPass(PM)
    ccall((:LLVMAddLoopUnrollAndJamPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLowerAtomicPass(PM)

See llvm::createLowerAtomicPass function.
"""
function LLVMAddLowerAtomicPass(PM)
    ccall((:LLVMAddLowerAtomicPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddMemCpyOptPass(PM)

See llvm::createMemCpyOptPass function.
"""
function LLVMAddMemCpyOptPass(PM)
    ccall((:LLVMAddMemCpyOptPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddPartiallyInlineLibCallsPass(PM)

See llvm::createPartiallyInlineLibCallsPass function.
"""
function LLVMAddPartiallyInlineLibCallsPass(PM)
    ccall((:LLVMAddPartiallyInlineLibCallsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddReassociatePass(PM)

See llvm::createReassociatePass function.
"""
function LLVMAddReassociatePass(PM)
    ccall((:LLVMAddReassociatePass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddSCCPPass(PM)

See llvm::createSCCPPass function.
"""
function LLVMAddSCCPPass(PM)
    ccall((:LLVMAddSCCPPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddScalarReplAggregatesPass(PM)

See llvm::createSROAPass function.
"""
function LLVMAddScalarReplAggregatesPass(PM)
    ccall((:LLVMAddScalarReplAggregatesPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddScalarReplAggregatesPassSSA(PM)

See llvm::createSROAPass function.
"""
function LLVMAddScalarReplAggregatesPassSSA(PM)
    ccall((:LLVMAddScalarReplAggregatesPassSSA, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddScalarReplAggregatesPassWithThreshold(PM, Threshold)

See llvm::createSROAPass function.
"""
function LLVMAddScalarReplAggregatesPassWithThreshold(PM, Threshold)
    ccall((:LLVMAddScalarReplAggregatesPassWithThreshold, libllvm), Cvoid, (LLVMPassManagerRef, Cint), PM, Threshold)
end

"""
    LLVMAddSimplifyLibCallsPass(PM)

See llvm::createSimplifyLibCallsPass function.
"""
function LLVMAddSimplifyLibCallsPass(PM)
    ccall((:LLVMAddSimplifyLibCallsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddTailCallEliminationPass(PM)

See llvm::createTailCallEliminationPass function.
"""
function LLVMAddTailCallEliminationPass(PM)
    ccall((:LLVMAddTailCallEliminationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddDemoteMemoryToRegisterPass(PM)

See llvm::demotePromoteMemoryToRegisterPass function.
"""
function LLVMAddDemoteMemoryToRegisterPass(PM)
    ccall((:LLVMAddDemoteMemoryToRegisterPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddVerifierPass(PM)

See llvm::createVerifierPass function.
"""
function LLVMAddVerifierPass(PM)
    ccall((:LLVMAddVerifierPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddCorrelatedValuePropagationPass(PM)

See llvm::createCorrelatedValuePropagationPass function
"""
function LLVMAddCorrelatedValuePropagationPass(PM)
    ccall((:LLVMAddCorrelatedValuePropagationPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddEarlyCSEPass(PM)

See llvm::createEarlyCSEPass function
"""
function LLVMAddEarlyCSEPass(PM)
    ccall((:LLVMAddEarlyCSEPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddEarlyCSEMemSSAPass(PM)

See llvm::createEarlyCSEPass function
"""
function LLVMAddEarlyCSEMemSSAPass(PM)
    ccall((:LLVMAddEarlyCSEMemSSAPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLowerExpectIntrinsicPass(PM)

See llvm::createLowerExpectIntrinsicPass function
"""
function LLVMAddLowerExpectIntrinsicPass(PM)
    ccall((:LLVMAddLowerExpectIntrinsicPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLowerConstantIntrinsicsPass(PM)

See llvm::createLowerConstantIntrinsicsPass function
"""
function LLVMAddLowerConstantIntrinsicsPass(PM)
    ccall((:LLVMAddLowerConstantIntrinsicsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddTypeBasedAliasAnalysisPass(PM)

See llvm::createTypeBasedAliasAnalysisPass function
"""
function LLVMAddTypeBasedAliasAnalysisPass(PM)
    ccall((:LLVMAddTypeBasedAliasAnalysisPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddScopedNoAliasAAPass(PM)

See llvm::createScopedNoAliasAAPass function
"""
function LLVMAddScopedNoAliasAAPass(PM)
    ccall((:LLVMAddScopedNoAliasAAPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddBasicAliasAnalysisPass(PM)

See llvm::createBasicAliasAnalysisPass function
"""
function LLVMAddBasicAliasAnalysisPass(PM)
    ccall((:LLVMAddBasicAliasAnalysisPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddUnifyFunctionExitNodesPass(PM)

See llvm::createUnifyFunctionExitNodesPass function
"""
function LLVMAddUnifyFunctionExitNodesPass(PM)
    ccall((:LLVMAddUnifyFunctionExitNodesPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLowerSwitchPass(PM)

See llvm::createLowerSwitchPass function.
"""
function LLVMAddLowerSwitchPass(PM)
    ccall((:LLVMAddLowerSwitchPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddPromoteMemoryToRegisterPass(PM)

See llvm::createPromoteMemoryToRegisterPass function.
"""
function LLVMAddPromoteMemoryToRegisterPass(PM)
    ccall((:LLVMAddPromoteMemoryToRegisterPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddAddDiscriminatorsPass(PM)

See llvm::createAddDiscriminatorsPass function.
"""
function LLVMAddAddDiscriminatorsPass(PM)
    ccall((:LLVMAddAddDiscriminatorsPass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddLoopVectorizePass(PM)

See llvm::createLoopVectorizePass function.
"""
function LLVMAddLoopVectorizePass(PM)
    ccall((:LLVMAddLoopVectorizePass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

"""
    LLVMAddSLPVectorizePass(PM)

See llvm::createSLPVectorizerPass function.
"""
function LLVMAddSLPVectorizePass(PM)
    ccall((:LLVMAddSLPVectorizePass, libllvm), Cvoid, (LLVMPassManagerRef,), PM)
end

struct llvm_blake3_chunk_state
    cv::NTuple{8, UInt32}
    chunk_counter::UInt64
    buf::NTuple{64, UInt8}
    buf_len::UInt8
    blocks_compressed::UInt8
    flags::UInt8
end

struct llvm_blake3_hasher
    key::NTuple{8, UInt32}
    chunk::llvm_blake3_chunk_state
    cv_stack_len::UInt8
    cv_stack::NTuple{1760, UInt8}
end

function llvm_blake3_version()
    ccall((:llvm_blake3_version, libllvm), Cstring, ())
end

function llvm_blake3_hasher_init(self)
    ccall((:llvm_blake3_hasher_init, libllvm), Cvoid, (Ptr{llvm_blake3_hasher},), self)
end

function llvm_blake3_hasher_init_keyed(self, key)
    ccall((:llvm_blake3_hasher_init_keyed, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, Ptr{UInt8}), self, key)
end

function llvm_blake3_hasher_init_derive_key(self, context)
    ccall((:llvm_blake3_hasher_init_derive_key, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, Cstring), self, context)
end

function llvm_blake3_hasher_init_derive_key_raw(self, context, context_len)
    ccall((:llvm_blake3_hasher_init_derive_key_raw, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, Ptr{Cvoid}, Csize_t), self, context, context_len)
end

function llvm_blake3_hasher_update(self, input, input_len)
    ccall((:llvm_blake3_hasher_update, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, Ptr{Cvoid}, Csize_t), self, input, input_len)
end

function llvm_blake3_hasher_finalize(self, out, out_len)
    ccall((:llvm_blake3_hasher_finalize, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, Ptr{UInt8}, Csize_t), self, out, out_len)
end

function llvm_blake3_hasher_finalize_seek(self, seek, out, out_len)
    ccall((:llvm_blake3_hasher_finalize_seek, libllvm), Cvoid, (Ptr{llvm_blake3_hasher}, UInt64, Ptr{UInt8}, Csize_t), self, seek, out, out_len)
end

function llvm_blake3_hasher_reset(self)
    ccall((:llvm_blake3_hasher_reset, libllvm), Cvoid, (Ptr{llvm_blake3_hasher},), self)
end

const lto_bool_t = Bool

"""
    lto_symbol_attributes

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
@cenum lto_symbol_attributes::UInt32 begin
    LTO_SYMBOL_ALIGNMENT_MASK = 31
    LTO_SYMBOL_PERMISSIONS_MASK = 224
    LTO_SYMBOL_PERMISSIONS_CODE = 160
    LTO_SYMBOL_PERMISSIONS_DATA = 192
    LTO_SYMBOL_PERMISSIONS_RODATA = 128
    LTO_SYMBOL_DEFINITION_MASK = 1792
    LTO_SYMBOL_DEFINITION_REGULAR = 256
    LTO_SYMBOL_DEFINITION_TENTATIVE = 512
    LTO_SYMBOL_DEFINITION_WEAK = 768
    LTO_SYMBOL_DEFINITION_UNDEFINED = 1024
    LTO_SYMBOL_DEFINITION_WEAKUNDEF = 1280
    LTO_SYMBOL_SCOPE_MASK = 14336
    LTO_SYMBOL_SCOPE_INTERNAL = 2048
    LTO_SYMBOL_SCOPE_HIDDEN = 4096
    LTO_SYMBOL_SCOPE_PROTECTED = 8192
    LTO_SYMBOL_SCOPE_DEFAULT = 6144
    LTO_SYMBOL_SCOPE_DEFAULT_CAN_BE_HIDDEN = 10240
    LTO_SYMBOL_COMDAT = 16384
    LTO_SYMBOL_ALIAS = 32768
end

"""
    lto_debug_model

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
@cenum lto_debug_model::UInt32 begin
    LTO_DEBUG_MODEL_NONE = 0
    LTO_DEBUG_MODEL_DWARF = 1
end

"""
    lto_codegen_model

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
@cenum lto_codegen_model::UInt32 begin
    LTO_CODEGEN_PIC_MODEL_STATIC = 0
    LTO_CODEGEN_PIC_MODEL_DYNAMIC = 1
    LTO_CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC = 2
    LTO_CODEGEN_PIC_MODEL_DEFAULT = 3
end

mutable struct LLVMOpaqueLTOModule end

"""
opaque reference to a loaded object module
"""
const lto_module_t = Ptr{LLVMOpaqueLTOModule}

mutable struct LLVMOpaqueLTOCodeGenerator end

"""
opaque reference to a code generator
"""
const lto_code_gen_t = Ptr{LLVMOpaqueLTOCodeGenerator}

mutable struct LLVMOpaqueThinLTOCodeGenerator end

"""
opaque reference to a thin code generator
"""
const thinlto_code_gen_t = Ptr{LLVMOpaqueThinLTOCodeGenerator}

"""
    lto_get_version()

Returns a printable string.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_get_version()
    ccall((:lto_get_version, libllvm), Cstring, ())
end

"""
    lto_get_error_message()

Returns the last error string or NULL if last operation was successful.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_get_error_message()
    ccall((:lto_get_error_message, libllvm), Cstring, ())
end

"""
    lto_module_is_object_file(path)

Checks if a file is a loadable object file.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_is_object_file(path)
    ccall((:lto_module_is_object_file, libllvm), lto_bool_t, (Cstring,), path)
end

"""
    lto_module_is_object_file_for_target(path, target_triple_prefix)

Checks if a file is a loadable object compiled for requested target.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_is_object_file_for_target(path, target_triple_prefix)
    ccall((:lto_module_is_object_file_for_target, libllvm), lto_bool_t, (Cstring, Cstring), path, target_triple_prefix)
end

"""
    lto_module_has_objc_category(mem, length)

Return true if `Buffer` contains a bitcode file with ObjC code (category or class) in it.

\\since [`LTO_API_VERSION`](@ref)=20
"""
function lto_module_has_objc_category(mem, length)
    ccall((:lto_module_has_objc_category, libllvm), lto_bool_t, (Ptr{Cvoid}, Csize_t), mem, length)
end

"""
    lto_module_is_object_file_in_memory(mem, length)

Checks if a buffer is a loadable object file.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_is_object_file_in_memory(mem, length)
    ccall((:lto_module_is_object_file_in_memory, libllvm), lto_bool_t, (Ptr{Cvoid}, Csize_t), mem, length)
end

"""
    lto_module_is_object_file_in_memory_for_target(mem, length, target_triple_prefix)

Checks if a buffer is a loadable object compiled for requested target.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_is_object_file_in_memory_for_target(mem, length, target_triple_prefix)
    ccall((:lto_module_is_object_file_in_memory_for_target, libllvm), lto_bool_t, (Ptr{Cvoid}, Csize_t, Cstring), mem, length, target_triple_prefix)
end

"""
    lto_module_create(path)

Loads an object file from disk. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_create(path)
    ccall((:lto_module_create, libllvm), lto_module_t, (Cstring,), path)
end

"""
    lto_module_create_from_memory(mem, length)

Loads an object file from memory. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_create_from_memory(mem, length)
    ccall((:lto_module_create_from_memory, libllvm), lto_module_t, (Ptr{Cvoid}, Csize_t), mem, length)
end

"""
    lto_module_create_from_memory_with_path(mem, length, path)

Loads an object file from memory with an extra path argument. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=9
"""
function lto_module_create_from_memory_with_path(mem, length, path)
    ccall((:lto_module_create_from_memory_with_path, libllvm), lto_module_t, (Ptr{Cvoid}, Csize_t, Cstring), mem, length, path)
end

"""
    lto_module_create_in_local_context(mem, length, path)

Loads an object file in its own context.

Loads an object file in its own LLVMContext. This function call is thread-safe. However, modules created this way should not be merged into an [`lto_code_gen_t`](@ref) using [`lto_codegen_add_module`](@ref)().

Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=11
"""
function lto_module_create_in_local_context(mem, length, path)
    ccall((:lto_module_create_in_local_context, libllvm), lto_module_t, (Ptr{Cvoid}, Csize_t, Cstring), mem, length, path)
end

"""
    lto_module_create_in_codegen_context(mem, length, path, cg)

Loads an object file in the codegen context.

Loads an object file into the same context as `cg`. The module is safe to add using [`lto_codegen_add_module`](@ref)().

Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=11
"""
function lto_module_create_in_codegen_context(mem, length, path, cg)
    ccall((:lto_module_create_in_codegen_context, libllvm), lto_module_t, (Ptr{Cvoid}, Csize_t, Cstring, lto_code_gen_t), mem, length, path, cg)
end

"""
    lto_module_create_from_fd(fd, path, file_size)

Loads an object file from disk. The seek point of fd is not preserved. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=5
"""
function lto_module_create_from_fd(fd, path, file_size)
    ccall((:lto_module_create_from_fd, libllvm), lto_module_t, (Cint, Cstring, Csize_t), fd, path, file_size)
end

"""
    lto_module_create_from_fd_at_offset(fd, path, file_size, map_size, offset)

Loads an object file from disk. The seek point of fd is not preserved. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=5
"""
function lto_module_create_from_fd_at_offset(fd, path, file_size, map_size, offset)
    ccall((:lto_module_create_from_fd_at_offset, libllvm), lto_module_t, (Cint, Cstring, Csize_t, Csize_t, off_t), fd, path, file_size, map_size, offset)
end

"""
    lto_module_dispose(mod)

Frees all memory internally allocated by the module. Upon return the [`lto_module_t`](@ref) is no longer valid.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_dispose(mod)
    ccall((:lto_module_dispose, libllvm), Cvoid, (lto_module_t,), mod)
end

"""
    lto_module_get_target_triple(mod)

Returns triple string which the object module was compiled under.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_get_target_triple(mod)
    ccall((:lto_module_get_target_triple, libllvm), Cstring, (lto_module_t,), mod)
end

"""
    lto_module_set_target_triple(mod, triple)

Sets triple string with which the object will be codegened.

\\since [`LTO_API_VERSION`](@ref)=4
"""
function lto_module_set_target_triple(mod, triple)
    ccall((:lto_module_set_target_triple, libllvm), Cvoid, (lto_module_t, Cstring), mod, triple)
end

"""
    lto_module_get_num_symbols(mod)

Returns the number of symbols in the object module.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_get_num_symbols(mod)
    ccall((:lto_module_get_num_symbols, libllvm), Cuint, (lto_module_t,), mod)
end

"""
    lto_module_get_symbol_name(mod, index)

Returns the name of the ith symbol in the object module.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_get_symbol_name(mod, index)
    ccall((:lto_module_get_symbol_name, libllvm), Cstring, (lto_module_t, Cuint), mod, index)
end

"""
    lto_module_get_symbol_attribute(mod, index)

Returns the attributes of the ith symbol in the object module.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_module_get_symbol_attribute(mod, index)
    ccall((:lto_module_get_symbol_attribute, libllvm), lto_symbol_attributes, (lto_module_t, Cuint), mod, index)
end

"""
    lto_module_get_linkeropts(mod)

Returns the module's linker options.

The linker options may consist of multiple flags. It is the linker's responsibility to split the flags using a platform-specific mechanism.

\\since [`LTO_API_VERSION`](@ref)=16
"""
function lto_module_get_linkeropts(mod)
    ccall((:lto_module_get_linkeropts, libllvm), Cstring, (lto_module_t,), mod)
end

"""
    lto_module_get_macho_cputype(mod, out_cputype, out_cpusubtype)

If targeting mach-o on darwin, this function gets the CPU type and subtype that will end up being encoded in the mach-o header. These are the values that can be found in mach/machine.h.

`out_cputype` and `out_cpusubtype` must be non-NULL.

Returns true on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=27
"""
function lto_module_get_macho_cputype(mod, out_cputype, out_cpusubtype)
    ccall((:lto_module_get_macho_cputype, libllvm), lto_bool_t, (lto_module_t, Ptr{Cuint}, Ptr{Cuint}), mod, out_cputype, out_cpusubtype)
end

"""
    lto_module_has_ctor_dtor(mod)

This function can be used by the linker to check if a given module has any constructor or destructor functions.

Returns true if the module has either the .global\\_ctors or the .global\\_dtors symbol. Otherwise returns false.

\\since [`LTO_API_VERSION`](@ref)=29
"""
function lto_module_has_ctor_dtor(mod)
    ccall((:lto_module_has_ctor_dtor, libllvm), lto_bool_t, (lto_module_t,), mod)
end

"""
    lto_codegen_diagnostic_severity_t

Diagnostic severity.

\\since [`LTO_API_VERSION`](@ref)=7
"""
@cenum lto_codegen_diagnostic_severity_t::UInt32 begin
    LTO_DS_ERROR = 0
    LTO_DS_WARNING = 1
    LTO_DS_REMARK = 3
    LTO_DS_NOTE = 2
end

# typedef void ( * lto_diagnostic_handler_t ) ( lto_codegen_diagnostic_severity_t severity , const char * diag , void * ctxt )
"""
Diagnostic handler type. `severity` defines the severity. `diag` is the actual diagnostic. The diagnostic is not prefixed by any of severity keyword, e.g., 'error: '. `ctxt` is used to pass the context set with the diagnostic handler.

\\since [`LTO_API_VERSION`](@ref)=7
"""
const lto_diagnostic_handler_t = Ptr{Cvoid}

"""
    lto_codegen_set_diagnostic_handler(arg1, arg2, arg3)

Set a diagnostic handler and the related context (void *). This is more general than [`lto_get_error_message`](@ref), as the diagnostic handler can be called at anytime within lto.

\\since [`LTO_API_VERSION`](@ref)=7
"""
function lto_codegen_set_diagnostic_handler(arg1, arg2, arg3)
    ccall((:lto_codegen_set_diagnostic_handler, libllvm), Cvoid, (lto_code_gen_t, lto_diagnostic_handler_t, Ptr{Cvoid}), arg1, arg2, arg3)
end

"""
    lto_codegen_create()

Instantiates a code generator. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

All modules added using [`lto_codegen_add_module`](@ref)() must have been created in the same context as the codegen.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_create()
    ccall((:lto_codegen_create, libllvm), lto_code_gen_t, ())
end

"""
    lto_codegen_create_in_local_context()

Instantiate a code generator in its own context.

Instantiates a code generator in its own context. Modules added via [`lto_codegen_add_module`](@ref)() must have all been created in the same context, using [`lto_module_create_in_codegen_context`](@ref)().

\\since [`LTO_API_VERSION`](@ref)=11
"""
function lto_codegen_create_in_local_context()
    ccall((:lto_codegen_create_in_local_context, libllvm), lto_code_gen_t, ())
end

"""
    lto_codegen_dispose(arg1)

Frees all code generator and all memory it internally allocated. Upon return the [`lto_code_gen_t`](@ref) is no longer valid.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_dispose(arg1)
    ccall((:lto_codegen_dispose, libllvm), Cvoid, (lto_code_gen_t,), arg1)
end

"""
    lto_codegen_add_module(cg, mod)

Add an object module to the set of modules for which code will be generated. Returns true on error (check [`lto_get_error_message`](@ref)() for details).

`cg` and `mod` must both be in the same context. See [`lto_codegen_create_in_local_context`](@ref)() and [`lto_module_create_in_codegen_context`](@ref)().

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_add_module(cg, mod)
    ccall((:lto_codegen_add_module, libllvm), lto_bool_t, (lto_code_gen_t, lto_module_t), cg, mod)
end

"""
    lto_codegen_set_module(cg, mod)

Sets the object module for code generation. This will transfer the ownership of the module to the code generator.

`cg` and `mod` must both be in the same context.

\\since [`LTO_API_VERSION`](@ref)=13
"""
function lto_codegen_set_module(cg, mod)
    ccall((:lto_codegen_set_module, libllvm), Cvoid, (lto_code_gen_t, lto_module_t), cg, mod)
end

"""
    lto_codegen_set_debug_model(cg, arg2)

Sets if debug info should be generated. Returns true on error (check [`lto_get_error_message`](@ref)() for details).

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_set_debug_model(cg, arg2)
    ccall((:lto_codegen_set_debug_model, libllvm), lto_bool_t, (lto_code_gen_t, lto_debug_model), cg, arg2)
end

"""
    lto_codegen_set_pic_model(cg, arg2)

Sets which PIC code model to generated. Returns true on error (check [`lto_get_error_message`](@ref)() for details).

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_set_pic_model(cg, arg2)
    ccall((:lto_codegen_set_pic_model, libllvm), lto_bool_t, (lto_code_gen_t, lto_codegen_model), cg, arg2)
end

"""
    lto_codegen_set_cpu(cg, cpu)

Sets the cpu to generate code for.

\\since [`LTO_API_VERSION`](@ref)=4
"""
function lto_codegen_set_cpu(cg, cpu)
    ccall((:lto_codegen_set_cpu, libllvm), Cvoid, (lto_code_gen_t, Cstring), cg, cpu)
end

"""
    lto_codegen_set_assembler_path(cg, path)

Sets the location of the assembler tool to run. If not set, libLTO will use gcc to invoke the assembler.

\\since [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_set_assembler_path(cg, path)
    ccall((:lto_codegen_set_assembler_path, libllvm), Cvoid, (lto_code_gen_t, Cstring), cg, path)
end

"""
    lto_codegen_set_assembler_args(cg, args, nargs)

Sets extra arguments that libLTO should pass to the assembler.

\\since [`LTO_API_VERSION`](@ref)=4
"""
function lto_codegen_set_assembler_args(cg, args, nargs)
    ccall((:lto_codegen_set_assembler_args, libllvm), Cvoid, (lto_code_gen_t, Ptr{Cstring}, Cint), cg, args, nargs)
end

"""
    lto_codegen_add_must_preserve_symbol(cg, symbol)

Adds to a list of all global symbols that must exist in the final generated code. If a function is not listed there, it might be inlined into every usage and optimized away.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_add_must_preserve_symbol(cg, symbol)
    ccall((:lto_codegen_add_must_preserve_symbol, libllvm), Cvoid, (lto_code_gen_t, Cstring), cg, symbol)
end

"""
    lto_codegen_write_merged_modules(cg, path)

Writes a new object file at the specified path that contains the merged contents of all modules added so far. Returns true on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=5
"""
function lto_codegen_write_merged_modules(cg, path)
    ccall((:lto_codegen_write_merged_modules, libllvm), lto_bool_t, (lto_code_gen_t, Cstring), cg, path)
end

"""
    lto_codegen_compile(cg, length)

Generates code for all added modules into one native object file. This calls [`lto_codegen_optimize`](@ref) then [`lto_codegen_compile_optimized`](@ref).

On success returns a pointer to a generated mach-o/ELF buffer and length set to the buffer size. The buffer is owned by the [`lto_code_gen_t`](@ref) and will be freed when [`lto_codegen_dispose`](@ref)() is called, or [`lto_codegen_compile`](@ref)() is called again. On failure, returns NULL (check [`lto_get_error_message`](@ref)() for details).

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_compile(cg, length)
    ccall((:lto_codegen_compile, libllvm), Ptr{Cvoid}, (lto_code_gen_t, Ptr{Csize_t}), cg, length)
end

"""
    lto_codegen_compile_to_file(cg, name)

Generates code for all added modules into one native object file. This calls [`lto_codegen_optimize`](@ref) then [`lto_codegen_compile_optimized`](@ref) (instead of returning a generated mach-o/ELF buffer, it writes to a file).

The name of the file is written to name. Returns true on error.

\\since [`LTO_API_VERSION`](@ref)=5
"""
function lto_codegen_compile_to_file(cg, name)
    ccall((:lto_codegen_compile_to_file, libllvm), lto_bool_t, (lto_code_gen_t, Ptr{Cstring}), cg, name)
end

"""
    lto_codegen_optimize(cg)

Runs optimization for the merged module. Returns true on error.

\\since [`LTO_API_VERSION`](@ref)=12
"""
function lto_codegen_optimize(cg)
    ccall((:lto_codegen_optimize, libllvm), lto_bool_t, (lto_code_gen_t,), cg)
end

"""
    lto_codegen_compile_optimized(cg, length)

Generates code for the optimized merged module into one native object file. It will not run any IR optimizations on the merged module.

On success returns a pointer to a generated mach-o/ELF buffer and length set to the buffer size. The buffer is owned by the [`lto_code_gen_t`](@ref) and will be freed when [`lto_codegen_dispose`](@ref)() is called, or [`lto_codegen_compile_optimized`](@ref)() is called again. On failure, returns NULL (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=12
"""
function lto_codegen_compile_optimized(cg, length)
    ccall((:lto_codegen_compile_optimized, libllvm), Ptr{Cvoid}, (lto_code_gen_t, Ptr{Csize_t}), cg, length)
end

"""
    lto_api_version()

Returns the runtime API version.

\\since [`LTO_API_VERSION`](@ref)=12
"""
function lto_api_version()
    ccall((:lto_api_version, libllvm), Cuint, ())
end

"""
    lto_set_debug_options(options, number)

Parses options immediately, making them available as early as possible. For example during executing codegen::InitTargetOptionsFromCodeGenFlags. Since parsing shud only happen once, only one of [`lto_codegen_debug_options`](@ref) or [`lto_set_debug_options`](@ref) should be called.

This function takes one or more options separated by spaces. Warning: passing file paths through this function may confuse the argument parser if the paths contain spaces.

\\since [`LTO_API_VERSION`](@ref)=28
"""
function lto_set_debug_options(options, number)
    ccall((:lto_set_debug_options, libllvm), Cvoid, (Ptr{Cstring}, Cint), options, number)
end

"""
    lto_codegen_debug_options(cg, arg2)

Sets options to help debug codegen bugs. Since parsing shud only happen once, only one of [`lto_codegen_debug_options`](@ref) or [`lto_set_debug_options`](@ref) should be called.

This function takes one or more options separated by spaces. Warning: passing file paths through this function may confuse the argument parser if the paths contain spaces.

\\since prior to [`LTO_API_VERSION`](@ref)=3
"""
function lto_codegen_debug_options(cg, arg2)
    ccall((:lto_codegen_debug_options, libllvm), Cvoid, (lto_code_gen_t, Cstring), cg, arg2)
end

"""
    lto_codegen_debug_options_array(cg, arg2, number)

Same as the previous function, but takes every option separately through an array.

\\since prior to [`LTO_API_VERSION`](@ref)=26
"""
function lto_codegen_debug_options_array(cg, arg2, number)
    ccall((:lto_codegen_debug_options_array, libllvm), Cvoid, (lto_code_gen_t, Ptr{Cstring}, Cint), cg, arg2, number)
end

"""
    lto_initialize_disassembler()

Initializes LLVM disassemblers. FIXME: This doesn't really belong here.

\\since [`LTO_API_VERSION`](@ref)=5
"""
function lto_initialize_disassembler()
    ccall((:lto_initialize_disassembler, libllvm), Cvoid, ())
end

"""
    lto_codegen_set_should_internalize(cg, ShouldInternalize)

Sets if we should run internalize pass during optimization and code generation.

\\since [`LTO_API_VERSION`](@ref)=14
"""
function lto_codegen_set_should_internalize(cg, ShouldInternalize)
    ccall((:lto_codegen_set_should_internalize, libllvm), Cvoid, (lto_code_gen_t, lto_bool_t), cg, ShouldInternalize)
end

"""
    lto_codegen_set_should_embed_uselists(cg, ShouldEmbedUselists)

Set whether to embed uselists in bitcode.

Sets whether [`lto_codegen_write_merged_modules`](@ref)() should embed uselists in output bitcode. This should be turned on for all -save-temps output.

\\since [`LTO_API_VERSION`](@ref)=15
"""
function lto_codegen_set_should_embed_uselists(cg, ShouldEmbedUselists)
    ccall((:lto_codegen_set_should_embed_uselists, libllvm), Cvoid, (lto_code_gen_t, lto_bool_t), cg, ShouldEmbedUselists)
end

mutable struct LLVMOpaqueLTOInput end

"""
Opaque reference to an LTO input file
"""
const lto_input_t = Ptr{LLVMOpaqueLTOInput}

"""
    lto_input_create(buffer, buffer_size, path)

Creates an LTO input file from a buffer. The path argument is used for diagnotics as this function otherwise does not know which file the given buffer is associated with.

\\since [`LTO_API_VERSION`](@ref)=24
"""
function lto_input_create(buffer, buffer_size, path)
    ccall((:lto_input_create, libllvm), lto_input_t, (Ptr{Cvoid}, Csize_t, Cstring), buffer, buffer_size, path)
end

"""
    lto_input_dispose(input)

Frees all memory internally allocated by the LTO input file. Upon return the [`lto_module_t`](@ref) is no longer valid.

\\since [`LTO_API_VERSION`](@ref)=24
"""
function lto_input_dispose(input)
    ccall((:lto_input_dispose, libllvm), Cvoid, (lto_input_t,), input)
end

"""
    lto_input_get_num_dependent_libraries(input)

Returns the number of dependent library specifiers for the given LTO input file.

\\since [`LTO_API_VERSION`](@ref)=24
"""
function lto_input_get_num_dependent_libraries(input)
    ccall((:lto_input_get_num_dependent_libraries, libllvm), Cuint, (lto_input_t,), input)
end

"""
    lto_input_get_dependent_library(input, index, size)

Returns the ith dependent library specifier for the given LTO input file. The returned string is not null-terminated.

\\since [`LTO_API_VERSION`](@ref)=24
"""
function lto_input_get_dependent_library(input, index, size)
    ccall((:lto_input_get_dependent_library, libllvm), Cstring, (lto_input_t, Csize_t, Ptr{Csize_t}), input, index, size)
end

"""
    lto_runtime_lib_symbols_list(size)

Returns the list of libcall symbols that can be generated by LTO that might not be visible from the symbol table of bitcode files.

\\since prior to [`LTO_API_VERSION`](@ref)=25
"""
function lto_runtime_lib_symbols_list(size)
    ccall((:lto_runtime_lib_symbols_list, libllvm), Ptr{Cstring}, (Ptr{Csize_t},), size)
end

"""
    LTOObjectBuffer

Type to wrap a single object returned by ThinLTO.

\\since [`LTO_API_VERSION`](@ref)=18
"""
struct LTOObjectBuffer
    Buffer::Cstring
    Size::Csize_t
end

"""
    thinlto_create_codegen()

Instantiates a ThinLTO code generator. Returns NULL on error (check [`lto_get_error_message`](@ref)() for details).

The ThinLTOCodeGenerator is not intended to be reuse for multiple compilation: the model is that the client adds modules to the generator and ask to perform the ThinLTO optimizations / codegen, and finally destroys the codegenerator.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_create_codegen()
    ccall((:thinlto_create_codegen, libllvm), thinlto_code_gen_t, ())
end

"""
    thinlto_codegen_dispose(cg)

Frees the generator and all memory it internally allocated. Upon return the [`thinlto_code_gen_t`](@ref) is no longer valid.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_dispose(cg)
    ccall((:thinlto_codegen_dispose, libllvm), Cvoid, (thinlto_code_gen_t,), cg)
end

"""
    thinlto_codegen_add_module(cg, identifier, data, length)

Add a module to a ThinLTO code generator. Identifier has to be unique among all the modules in a code generator. The data buffer stays owned by the client, and is expected to be available for the entire lifetime of the [`thinlto_code_gen_t`](@ref) it is added to.

On failure, returns NULL (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_add_module(cg, identifier, data, length)
    ccall((:thinlto_codegen_add_module, libllvm), Cvoid, (thinlto_code_gen_t, Cstring, Cstring, Cint), cg, identifier, data, length)
end

"""
    thinlto_codegen_process(cg)

Optimize and codegen all the modules added to the codegenerator using ThinLTO. Resulting objects are accessible using [`thinlto_module_get_object`](@ref)().

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_process(cg)
    ccall((:thinlto_codegen_process, libllvm), Cvoid, (thinlto_code_gen_t,), cg)
end

"""
    thinlto_module_get_num_objects(cg)

Returns the number of object files produced by the ThinLTO CodeGenerator.

It usually matches the number of input files, but this is not a guarantee of the API and may change in future implementation, so the client should not assume it.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_module_get_num_objects(cg)
    ccall((:thinlto_module_get_num_objects, libllvm), Cuint, (thinlto_code_gen_t,), cg)
end

"""
    thinlto_module_get_object(cg, index)

Returns a reference to the ith object file produced by the ThinLTO CodeGenerator.

Client should use [`thinlto_module_get_num_objects`](@ref)() to get the number of available objects.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_module_get_object(cg, index)
    ccall((:thinlto_module_get_object, libllvm), LTOObjectBuffer, (thinlto_code_gen_t, Cuint), cg, index)
end

"""
    thinlto_module_get_num_object_files(cg)

Returns the number of object files produced by the ThinLTO CodeGenerator.

It usually matches the number of input files, but this is not a guarantee of the API and may change in future implementation, so the client should not assume it.

\\since [`LTO_API_VERSION`](@ref)=21
"""
function thinlto_module_get_num_object_files(cg)
    ccall((:thinlto_module_get_num_object_files, libllvm), Cuint, (thinlto_code_gen_t,), cg)
end

"""
    thinlto_module_get_object_file(cg, index)

Returns the path to the ith object file produced by the ThinLTO CodeGenerator.

Client should use [`thinlto_module_get_num_object_files`](@ref)() to get the number of available objects.

\\since [`LTO_API_VERSION`](@ref)=21
"""
function thinlto_module_get_object_file(cg, index)
    ccall((:thinlto_module_get_object_file, libllvm), Cstring, (thinlto_code_gen_t, Cuint), cg, index)
end

"""
    thinlto_codegen_set_pic_model(cg, arg2)

Sets which PIC code model to generate. Returns true on error (check [`lto_get_error_message`](@ref)() for details).

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_pic_model(cg, arg2)
    ccall((:thinlto_codegen_set_pic_model, libllvm), lto_bool_t, (thinlto_code_gen_t, lto_codegen_model), cg, arg2)
end

"""
    thinlto_codegen_set_savetemps_dir(cg, save_temps_dir)

Sets the path to a directory to use as a storage for temporary bitcode files. The intention is to make the bitcode files available for debugging at various stage of the pipeline.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_savetemps_dir(cg, save_temps_dir)
    ccall((:thinlto_codegen_set_savetemps_dir, libllvm), Cvoid, (thinlto_code_gen_t, Cstring), cg, save_temps_dir)
end

"""
    thinlto_set_generated_objects_dir(cg, save_temps_dir)

Set the path to a directory where to save generated object files. This path can be used by a linker to request on-disk files instead of in-memory buffers. When set, results are available through [`thinlto_module_get_object_file`](@ref)() instead of [`thinlto_module_get_object`](@ref)().

\\since [`LTO_API_VERSION`](@ref)=21
"""
function thinlto_set_generated_objects_dir(cg, save_temps_dir)
    ccall((:thinlto_set_generated_objects_dir, libllvm), Cvoid, (thinlto_code_gen_t, Cstring), cg, save_temps_dir)
end

"""
    thinlto_codegen_set_cpu(cg, cpu)

Sets the cpu to generate code for.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_cpu(cg, cpu)
    ccall((:thinlto_codegen_set_cpu, libllvm), Cvoid, (thinlto_code_gen_t, Cstring), cg, cpu)
end

"""
    thinlto_codegen_disable_codegen(cg, disable)

Disable CodeGen, only run the stages till codegen and stop. The output will be bitcode.

\\since [`LTO_API_VERSION`](@ref)=19
"""
function thinlto_codegen_disable_codegen(cg, disable)
    ccall((:thinlto_codegen_disable_codegen, libllvm), Cvoid, (thinlto_code_gen_t, lto_bool_t), cg, disable)
end

"""
    thinlto_codegen_set_codegen_only(cg, codegen_only)

Perform CodeGen only: disable all other stages.

\\since [`LTO_API_VERSION`](@ref)=19
"""
function thinlto_codegen_set_codegen_only(cg, codegen_only)
    ccall((:thinlto_codegen_set_codegen_only, libllvm), Cvoid, (thinlto_code_gen_t, lto_bool_t), cg, codegen_only)
end

"""
    thinlto_debug_options(options, number)

Parse -mllvm style debug options.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_debug_options(options, number)
    ccall((:thinlto_debug_options, libllvm), Cvoid, (Ptr{Cstring}, Cint), options, number)
end

"""
    lto_module_is_thinlto(mod)

Test if a module has support for ThinLTO linking.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function lto_module_is_thinlto(mod)
    ccall((:lto_module_is_thinlto, libllvm), lto_bool_t, (lto_module_t,), mod)
end

"""
    thinlto_codegen_add_must_preserve_symbol(cg, name, length)

Adds a symbol to the list of global symbols that must exist in the final generated code. If a function is not listed there, it might be inlined into every usage and optimized away. For every single module, the functions referenced from code outside of the ThinLTO modules need to be added here.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_add_must_preserve_symbol(cg, name, length)
    ccall((:thinlto_codegen_add_must_preserve_symbol, libllvm), Cvoid, (thinlto_code_gen_t, Cstring, Cint), cg, name, length)
end

"""
    thinlto_codegen_add_cross_referenced_symbol(cg, name, length)

Adds a symbol to the list of global symbols that are cross-referenced between ThinLTO files. If the ThinLTO CodeGenerator can ensure that every references from a ThinLTO module to this symbol is optimized away, then the symbol can be discarded.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_add_cross_referenced_symbol(cg, name, length)
    ccall((:thinlto_codegen_add_cross_referenced_symbol, libllvm), Cvoid, (thinlto_code_gen_t, Cstring, Cint), cg, name, length)
end

"""
    thinlto_codegen_set_cache_dir(cg, cache_dir)

Sets the path to a directory to use as a cache storage for incremental build. Setting this activates caching.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_cache_dir(cg, cache_dir)
    ccall((:thinlto_codegen_set_cache_dir, libllvm), Cvoid, (thinlto_code_gen_t, Cstring), cg, cache_dir)
end

"""
    thinlto_codegen_set_cache_pruning_interval(cg, interval)

Sets the cache pruning interval (in seconds). A negative value disables the pruning. An unspecified default value will be applied, and a value of 0 will force prunning to occur.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_cache_pruning_interval(cg, interval)
    ccall((:thinlto_codegen_set_cache_pruning_interval, libllvm), Cvoid, (thinlto_code_gen_t, Cint), cg, interval)
end

"""
    thinlto_codegen_set_final_cache_size_relative_to_available_space(cg, percentage)

Sets the maximum cache size that can be persistent across build, in terms of percentage of the available space on the disk. Set to 100 to indicate no limit, 50 to indicate that the cache size will not be left over half the available space. A value over 100 will be reduced to 100, a value of 0 will be ignored. An unspecified default value will be applied.

The formula looks like: AvailableSpace = FreeSpace + ExistingCacheSize NewCacheSize = AvailableSpace * P/100

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_final_cache_size_relative_to_available_space(cg, percentage)
    ccall((:thinlto_codegen_set_final_cache_size_relative_to_available_space, libllvm), Cvoid, (thinlto_code_gen_t, Cuint), cg, percentage)
end

"""
    thinlto_codegen_set_cache_entry_expiration(cg, expiration)

Sets the expiration (in seconds) for an entry in the cache. An unspecified default value will be applied. A value of 0 will be ignored.

\\since [`LTO_API_VERSION`](@ref)=18
"""
function thinlto_codegen_set_cache_entry_expiration(cg, expiration)
    ccall((:thinlto_codegen_set_cache_entry_expiration, libllvm), Cvoid, (thinlto_code_gen_t, Cuint), cg, expiration)
end

"""
    thinlto_codegen_set_cache_size_bytes(cg, max_size_bytes)

Sets the maximum size of the cache directory (in bytes). A value over the amount of available space on the disk will be reduced to the amount of available space. An unspecified default value will be applied. A value of 0 will be ignored.

\\since [`LTO_API_VERSION`](@ref)=22
"""
function thinlto_codegen_set_cache_size_bytes(cg, max_size_bytes)
    ccall((:thinlto_codegen_set_cache_size_bytes, libllvm), Cvoid, (thinlto_code_gen_t, Cuint), cg, max_size_bytes)
end

"""
    thinlto_codegen_set_cache_size_megabytes(cg, max_size_megabytes)

Same as [`thinlto_codegen_set_cache_size_bytes`](@ref), except the maximum size is in megabytes (2^20 bytes).

\\since [`LTO_API_VERSION`](@ref)=23
"""
function thinlto_codegen_set_cache_size_megabytes(cg, max_size_megabytes)
    ccall((:thinlto_codegen_set_cache_size_megabytes, libllvm), Cvoid, (thinlto_code_gen_t, Cuint), cg, max_size_megabytes)
end

"""
    thinlto_codegen_set_cache_size_files(cg, max_size_files)

Sets the maximum number of files in the cache directory. An unspecified default value will be applied. A value of 0 will be ignored.

\\since [`LTO_API_VERSION`](@ref)=22
"""
function thinlto_codegen_set_cache_size_files(cg, max_size_files)
    ccall((:thinlto_codegen_set_cache_size_files, libllvm), Cvoid, (thinlto_code_gen_t, Cuint), cg, max_size_files)
end

const LLVMDisassembler_Option_UseMarkup = 1

const LLVMDisassembler_Option_PrintImmHex = 2

const LLVMDisassembler_Option_AsmPrinterVariant = 4

const LLVMDisassembler_Option_SetInstrComments = 8

const LLVMDisassembler_Option_PrintLatency = 16

const LLVMDisassembler_VariantKind_None = 0

const LLVMDisassembler_VariantKind_ARM_HI16 = 1

const LLVMDisassembler_VariantKind_ARM_LO16 = 2

const LLVMDisassembler_VariantKind_ARM64_PAGE = 1

const LLVMDisassembler_VariantKind_ARM64_PAGEOFF = 2

const LLVMDisassembler_VariantKind_ARM64_GOTPAGE = 3

const LLVMDisassembler_VariantKind_ARM64_GOTPAGEOFF = 4

const LLVMDisassembler_VariantKind_ARM64_TLVP = 5

const LLVMDisassembler_VariantKind_ARM64_TLVOFF = 6

const LLVMDisassembler_ReferenceType_InOut_None = 0

const LLVMDisassembler_ReferenceType_In_Branch = 1

const LLVMDisassembler_ReferenceType_In_PCrel_Load = 2

const LLVMDisassembler_ReferenceType_In_ARM64_ADRP = 0x0000000100000001

const LLVMDisassembler_ReferenceType_In_ARM64_ADDXri = 0x0000000100000002

const LLVMDisassembler_ReferenceType_In_ARM64_LDRXui = 0x0000000100000003

const LLVMDisassembler_ReferenceType_In_ARM64_LDRXl = 0x0000000100000004

const LLVMDisassembler_ReferenceType_In_ARM64_ADR = 0x0000000100000005

const LLVMDisassembler_ReferenceType_Out_SymbolStub = 1

const LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr = 2

const LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr = 3

const LLVMDisassembler_ReferenceType_Out_Objc_CFString_Ref = 4

const LLVMDisassembler_ReferenceType_Out_Objc_Message = 5

const LLVMDisassembler_ReferenceType_Out_Objc_Message_Ref = 6

const LLVMDisassembler_ReferenceType_Out_Objc_Selector_Ref = 7

const LLVMDisassembler_ReferenceType_Out_Objc_Class_Ref = 8

const LLVMDisassembler_ReferenceType_DeMangled_Name = 9

const LLVMErrorSuccess = 0

const REMARKS_API_VERSION = 1

const LLVM_BLAKE3_VERSION_STRING = "1.3.1"

const LLVM_BLAKE3_KEY_LEN = 32

const LLVM_BLAKE3_OUT_LEN = 32

const LLVM_BLAKE3_BLOCK_LEN = 64

const LLVM_BLAKE3_CHUNK_LEN = 1024

const LLVM_BLAKE3_MAX_DEPTH = 54

const LTO_API_VERSION = 29

