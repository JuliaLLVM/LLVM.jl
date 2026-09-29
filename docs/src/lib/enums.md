# Enumerations

The enumerations of the C API are available using scoped names, e.g.,
`LLVM.Linkage.Internal` for `LLVM.API.LLVMInternalLinkage`. The values that are available
depend on the version of LLVM, so they may differ from the ones listed here.

```@docs
LLVM.Linkage
LLVM.Visibility
LLVM.DLLStorageClass
LLVM.UnnamedAddr
LLVM.ThreadLocalMode
LLVM.CallConv
LLVM.IntPredicate
LLVM.RealPredicate
LLVM.Opcode
LLVM.TypeKind
LLVM.ValueKind
LLVM.AtomicOrdering
LLVM.AtomicRMWBinOp
LLVM.TailCallKind
LLVM.InlineAsmDialect
LLVM.ModuleFlagBehavior
LLVM.CloneFunctionChangeType
LLVM.DWARFSourceLanguage
LLVM.DWARFEmissionKind
LLVM.DebugEmissionKind
LLVM.CodeGenOptLevel
LLVM.CodeGenFileType
LLVM.RelocMode
LLVM.CodeModel
LLVM.ByteOrdering
LLVM.LookupKind
LLVM.JITDylibLookupFlags
LLVM.SymbolLookupFlags
```
