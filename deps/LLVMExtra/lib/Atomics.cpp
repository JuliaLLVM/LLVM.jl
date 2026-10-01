// Utilities for expanding atomic operations.
//
// The computation of atomic operations uses LLVM's own utilities
// (llvm/Transforms/Utils/LowerAtomic.h). The expansions are copied from
// llvm/lib/CodeGen/AtomicExpandPass.cpp and llvm/lib/Transforms/Utils/LowerAtomic.cpp of
// LLVM 22.1.8, where they are private or tied to a TargetLowering, with the target's
// minimum cmpxchg size as a parameter, and adapted to older LLVM versions where noted.
//
// AtomicExpandPass runs during code generation, so its expansions don't need to be valid
// for the IR optimizer. These copies are also used from IR passes, so they differ in that
// the loops start with an atomic (monotonic) load instead of a plain one (like LLVM 23
// does), of an integer for floating-point and vector values, and that they preserve the
// volatility, alignment and metadata of the original instruction.

#include "LLVMExtra.h"

#include <llvm/ADT/APInt.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/Utils/LowerAtomic.h>

using namespace llvm;

namespace {

// the values of LLVMAtomicRMWBinOp in the most recent C API, for every operation the
// running LLVM supports (the C API of some versions lacks some of these operations)
AtomicRMWInst::BinOp unwrapRMWBinOp(unsigned Op) {
  switch (Op) {
  case 0: return AtomicRMWInst::Xchg;
  case 1: return AtomicRMWInst::Add;
  case 2: return AtomicRMWInst::Sub;
  case 3: return AtomicRMWInst::And;
  case 4: return AtomicRMWInst::Nand;
  case 5: return AtomicRMWInst::Or;
  case 6: return AtomicRMWInst::Xor;
  case 7: return AtomicRMWInst::Max;
  case 8: return AtomicRMWInst::Min;
  case 9: return AtomicRMWInst::UMax;
  case 10: return AtomicRMWInst::UMin;
  case 11: return AtomicRMWInst::FAdd;
  case 12: return AtomicRMWInst::FSub;
  case 13: return AtomicRMWInst::FMax;
  case 14: return AtomicRMWInst::FMin;
#if LLVM_VERSION_MAJOR >= 16
  case 15: return AtomicRMWInst::UIncWrap;
  case 16: return AtomicRMWInst::UDecWrap;
#endif
#if LLVM_VERSION_MAJOR >= 20
  case 17: return AtomicRMWInst::USubCond;
  case 18: return AtomicRMWInst::USubSat;
#endif
#if LLVM_VERSION_MAJOR >= 21
  case 19: return AtomicRMWInst::FMaximum;
  case 20: return AtomicRMWInst::FMinimum;
#endif
  }
  llvm_unreachable("Invalid LLVMAtomicRMWBinOp value!");
}

// copied from AtomicExpandPass.cpp
void copyMetadataForAtomic(Instruction &Dest, const Instruction &Source) {
  SmallVector<std::pair<unsigned, MDNode *>, 8> MD;
  Source.getAllMetadata(MD);
  LLVMContext &Ctx = Dest.getContext();

  for (auto [ID, N] : MD) {
    switch (ID) {
    case LLVMContext::MD_dbg:
    case LLVMContext::MD_tbaa:
    case LLVMContext::MD_tbaa_struct:
    case LLVMContext::MD_alias_scope:
    case LLVMContext::MD_noalias:
#if LLVM_VERSION_MAJOR >= 20
    case LLVMContext::MD_noalias_addrspace:
#endif
    case LLVMContext::MD_access_group:
#if LLVM_VERSION_MAJOR >= 19
    case LLVMContext::MD_mmra:
#endif
      Dest.setMetadata(ID, N);
      break;
    default:
      if (ID == Ctx.getMDKindID("amdgpu.no.remote.memory"))
        Dest.setMetadata(ID, N);
      else if (ID == Ctx.getMDKindID("amdgpu.no.fine.grained.memory"))
        Dest.setMetadata(ID, N);
      break;
    }
  }
}

// copied from AtomicExpandPass.cpp, and extended to handle typed pointers (like LLVM 15),
// pointer values (which cmpxchg supports) and volatility
void createCmpXchgInstFun(IRBuilderBase &Builder, Value *Addr, Value *Loaded,
                          Value *NewVal, Align AddrAlign, AtomicOrdering MemOpOrder,
                          SyncScope::ID SSID, bool IsVolatile, Value *&Success,
                          Value *&NewLoaded, Instruction *MetadataSrc) {
  Type *OrigTy = NewVal->getType();

  bool NeedBitcast = OrigTy->isFloatingPointTy() || OrigTy->isVectorTy();
  if (NeedBitcast) {
    IntegerType *IntTy = Builder.getIntNTy(OrigTy->getPrimitiveSizeInBits());
#if LLVM_VERSION_MAJOR < 17
    if (!cast<PointerType>(Addr->getType())->isOpaque())
      Addr = Builder.CreateBitCast(
          Addr, IntTy->getPointerTo(Addr->getType()->getPointerAddressSpace()));
#endif
    NewVal = Builder.CreateBitCast(NewVal, IntTy);
    Loaded = Builder.CreateBitCast(Loaded, IntTy);
  }

  AtomicCmpXchgInst *Pair = Builder.CreateAtomicCmpXchg(
      Addr, Loaded, NewVal, AddrAlign, MemOpOrder,
      AtomicCmpXchgInst::getStrongestFailureOrdering(MemOpOrder), SSID);
  Pair->setVolatile(IsVolatile);
  if (MetadataSrc)
    copyMetadataForAtomic(*Pair, *MetadataSrc);

  Success = Builder.CreateExtractValue(Pair, 1, "success");
  NewLoaded = Builder.CreateExtractValue(Pair, 0, "newloaded");

  if (NeedBitcast)
    NewLoaded = Builder.CreateBitCast(NewLoaded, OrigTy);
}

// the initial load of a cmpxchg loop, which races with other accesses so it has to be
// atomic. floating-point and vector values are loaded as an integer of the same size, like
// they are compared: not every target supports atomic loads of those types (before LLVM 22,
// atomic loads of vectors aren't even valid IR), and AtomicExpandPass of LLVM 23 also casts
// floating-point ones to integers (by default, with shouldCastAtomicLoadInIR).
Value *createInitialLoad(IRBuilderBase &Builder, Type *Ty, Value *Addr, Align AddrAlign,
                         SyncScope::ID SSID, bool IsVolatile) {
  Type *LoadTy = Ty;
  if (Ty->isFloatingPointTy() || Ty->isVectorTy()) {
    LoadTy = Builder.getIntNTy(Ty->getPrimitiveSizeInBits());
#if LLVM_VERSION_MAJOR < 17
    if (!cast<PointerType>(Addr->getType())->isOpaque())
      Addr = Builder.CreateBitCast(
          Addr, LoadTy->getPointerTo(Addr->getType()->getPointerAddressSpace()));
#endif
  }
  LoadInst *Load = Builder.CreateAlignedLoad(LoadTy, Addr, AddrAlign);
  Load->setAtomic(AtomicOrdering::Monotonic, SSID);
  Load->setVolatile(IsVolatile);
  return LoadTy == Ty ? Load : Builder.CreateBitCast(Load, Ty);
}

// copied from AtomicExpandImpl::insertRMWCmpXchgLoop, always using createCmpXchgInstFun
Value *insertRMWCmpXchgLoop(IRBuilderBase &Builder, Type *ResultTy, Value *Addr,
                            Align AddrAlign, AtomicOrdering MemOpOrder,
                            SyncScope::ID SSID, bool IsVolatile,
                            function_ref<Value *(IRBuilderBase &, Value *)> PerformOp,
                            Instruction *MetadataSrc) {
  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *F = BB->getParent();

  BasicBlock *ExitBB = BB->splitBasicBlock(Builder.GetInsertPoint(), "atomicrmw.end");
  BasicBlock *LoopBB = BasicBlock::Create(Ctx, "atomicrmw.start", F, ExitBB);

  // The split call above "helpfully" added a branch at the end of BB (to the
  // wrong place), but we want a load. It's easiest to just remove
  // the branch entirely.
  std::prev(BB->end())->eraseFromParent();
  Builder.SetInsertPoint(BB);
  Value *InitLoaded = createInitialLoad(Builder, ResultTy, Addr, AddrAlign, SSID, IsVolatile);
  Builder.CreateBr(LoopBB);

  // Start the main loop block now that we've taken care of the preliminaries.
  Builder.SetInsertPoint(LoopBB);
  PHINode *Loaded = Builder.CreatePHI(ResultTy, 2, "loaded");
  Loaded->addIncoming(InitLoaded, BB);

  Value *NewVal = PerformOp(Builder, Loaded);

  Value *NewLoaded = nullptr;
  Value *Success = nullptr;

  createCmpXchgInstFun(Builder, Addr, Loaded, NewVal, AddrAlign,
                       MemOpOrder == AtomicOrdering::Unordered ? AtomicOrdering::Monotonic
                                                               : MemOpOrder,
                       SSID, IsVolatile, Success, NewLoaded, MetadataSrc);
  assert(Success && NewLoaded);

  Loaded->addIncoming(NewLoaded, LoopBB);

  Builder.CreateCondBr(Success, ExitBB, LoopBB);

  Builder.SetInsertPoint(ExitBB, ExitBB->begin());
  return NewLoaded;
}

struct PartwordMaskValues {
  // These three fields are guaranteed to be set by createMaskInstrs.
  Type *WordType = nullptr;
  Type *ValueType = nullptr;
  Type *IntValueType = nullptr;
  Value *AlignedAddr = nullptr;
  Align AlignedAddrAlignment;
  // The remaining fields can be null.
  Value *ShiftAmt = nullptr;
  Value *Mask = nullptr;
  Value *Inv_Mask = nullptr;
};

// copied from AtomicExpandPass.cpp, taking the module instead of an instruction, and
// computing the address of the word by subtracting the offset of the value from its
// address, instead of with `llvm.ptrmask` (LLVM 16+) or `inttoptr(and(ptrtoint))` (LLVM 15),
// which respectively aren't supported by every back-end and lose the provenance of the
// pointer. When no partword access is needed, the shift amount and mask use the integer
// type of the value, so that this works for floating-point values too.
PartwordMaskValues createMaskInstrs(IRBuilderBase &Builder, Module *M, Type *ValueType,
                                    Value *Addr, Align AddrAlign, unsigned MinWordSize) {
  PartwordMaskValues PMV;

  LLVMContext &Ctx = M->getContext();
  const DataLayout &DL = M->getDataLayout();
  unsigned ValueSize = DL.getTypeStoreSize(ValueType);

  PMV.ValueType = PMV.IntValueType = ValueType;
  if (PMV.ValueType->isFloatingPointTy() || PMV.ValueType->isVectorTy())
    PMV.IntValueType = Type::getIntNTy(Ctx, ValueType->getPrimitiveSizeInBits());

  PMV.WordType =
      MinWordSize > ValueSize ? Type::getIntNTy(Ctx, MinWordSize * 8) : ValueType;
  if (PMV.ValueType == PMV.WordType) {
    PMV.AlignedAddr = Addr;
    PMV.AlignedAddrAlignment = AddrAlign;
    PMV.ShiftAmt = ConstantInt::get(PMV.IntValueType, 0);
    PMV.Mask = ConstantInt::get(PMV.IntValueType, ~0, /*isSigned*/ true);
    return PMV;
  }

  PMV.AlignedAddrAlignment = Align(MinWordSize);

  assert(ValueSize < MinWordSize);

  PointerType *PtrTy = cast<PointerType>(Addr->getType());
  IntegerType *IntTy = cast<IntegerType>(DL.getIndexType(PtrTy));
  Value *PtrLSB;

  if (AddrAlign < MinWordSize) {
    Value *AddrInt = Builder.CreatePtrToInt(Addr, IntTy);
    PtrLSB = Builder.CreateAnd(AddrInt, MinWordSize - 1, "PtrLSB");
    Value *BytePtr = Addr;
#if LLVM_VERSION_MAJOR < 17
    if (!PtrTy->isOpaque())
      BytePtr = Builder.CreateBitCast(Addr, Builder.getInt8PtrTy(PtrTy->getAddressSpace()));
#endif
    PMV.AlignedAddr = Builder.CreateGEP(Builder.getInt8Ty(), BytePtr,
                                        Builder.CreateNeg(PtrLSB), "AlignedAddr");
  } else {
    // If the alignment is high enough, the LSB are known 0.
    PMV.AlignedAddr = Addr;
    PtrLSB = ConstantInt::getNullValue(IntTy);
  }
#if LLVM_VERSION_MAJOR < 17
  if (!PtrTy->isOpaque())
    PMV.AlignedAddr = Builder.CreateBitCast(
        PMV.AlignedAddr, PMV.WordType->getPointerTo(PtrTy->getAddressSpace()));
#endif

  if (DL.isLittleEndian()) {
    // turn bytes into bits
    PMV.ShiftAmt = Builder.CreateShl(PtrLSB, 3);
  } else {
    // turn bytes into bits, and count from the other side.
    PMV.ShiftAmt = Builder.CreateShl(Builder.CreateXor(PtrLSB, MinWordSize - ValueSize), 3);
  }

  // (AtomicExpandPass truncates, which fails for words that are wider than the index type)
  PMV.ShiftAmt = Builder.CreateZExtOrTrunc(PMV.ShiftAmt, PMV.WordType, "ShiftAmt");
  // (AtomicExpandPass uses `(1 << (ValueSize * 8)) - 1`, which overflows for 4-byte values)
  PMV.Mask = Builder.CreateShl(
      ConstantInt::get(PMV.WordType, APInt::getLowBitsSet(MinWordSize * 8, ValueSize * 8)),
      PMV.ShiftAmt, "Mask");

  PMV.Inv_Mask = Builder.CreateNot(PMV.Mask, "Inv_Mask");

  return PMV;
}

// copied from AtomicExpandPass.cpp
Value *extractMaskedValue(IRBuilderBase &Builder, Value *WideWord,
                          const PartwordMaskValues &PMV) {
  assert(WideWord->getType() == PMV.WordType && "Widened type mismatch");
  if (PMV.WordType == PMV.ValueType)
    return WideWord;

  Value *Shift = Builder.CreateLShr(WideWord, PMV.ShiftAmt, "shifted");
  Value *Trunc = Builder.CreateTrunc(Shift, PMV.IntValueType, "extracted");
  return Builder.CreateBitCast(Trunc, PMV.ValueType);
}

// copied from AtomicExpandPass.cpp
Value *insertMaskedValue(IRBuilderBase &Builder, Value *WideWord, Value *Updated,
                         const PartwordMaskValues &PMV) {
  assert(WideWord->getType() == PMV.WordType && "Widened type mismatch");
  assert(Updated->getType() == PMV.ValueType && "Value type mismatch");
  if (PMV.WordType == PMV.ValueType)
    return Updated;

  Updated = Builder.CreateBitCast(Updated, PMV.IntValueType);

  Value *ZExt = Builder.CreateZExt(Updated, PMV.WordType, "extended");
  Value *Shift = Builder.CreateShl(ZExt, PMV.ShiftAmt, "shifted", /*HasNUW*/ true);
  Value *And = Builder.CreateAnd(WideWord, PMV.Inv_Mask, "unmasked");
  Value *Or = Builder.CreateOr(And, Shift, "inserted");
  return Or;
}

// copied from AtomicExpandPass.cpp; the operations that depend on the LLVM version are
// all handled by the default case
Value *performMaskedAtomicOp(AtomicRMWInst::BinOp Op, IRBuilderBase &Builder, Value *Loaded,
                             Value *Shifted_Inc, Value *Inc,
                             const PartwordMaskValues &PMV) {
  switch (Op) {
  case AtomicRMWInst::Xchg: {
    Value *Loaded_MaskOut = Builder.CreateAnd(Loaded, PMV.Inv_Mask);
    Value *FinalVal = Builder.CreateOr(Loaded_MaskOut, Shifted_Inc);
    return FinalVal;
  }
  case AtomicRMWInst::Or:
  case AtomicRMWInst::Xor:
  case AtomicRMWInst::And:
    llvm_unreachable("Or/Xor/And handled by widenPartwordAtomicRMW");
  case AtomicRMWInst::Add:
  case AtomicRMWInst::Sub:
  case AtomicRMWInst::Nand: {
    // The other arithmetic ops need to be masked into place.
    Value *NewVal = buildAtomicRMWValue(Op, Builder, Loaded, Shifted_Inc);
    Value *NewVal_Masked = Builder.CreateAnd(NewVal, PMV.Mask);
    Value *Loaded_MaskOut = Builder.CreateAnd(Loaded, PMV.Inv_Mask);
    Value *FinalVal = Builder.CreateOr(Loaded_MaskOut, NewVal_Masked);
    return FinalVal;
  }
  default: {
    // Finally, other ops will operate on the full value, so truncate down to
    // the original size, and expand out again after doing the
    // operation. Bitcasts will be inserted for FP values.
    Value *Loaded_Extract = extractMaskedValue(Builder, Loaded, PMV);
    Value *NewVal = buildAtomicRMWValue(Op, Builder, Loaded_Extract, Inc);
    Value *FinalVal = insertMaskedValue(Builder, Loaded, NewVal, PMV);
    return FinalVal;
  }
  }
}

// copied from AtomicExpandImpl::widenPartwordAtomicRMW
AtomicRMWInst *widenPartwordAtomicRMW(AtomicRMWInst *AI, unsigned MinWordSize) {
  IRBuilder<> Builder(AI);
  AtomicRMWInst::BinOp Op = AI->getOperation();

  assert((Op == AtomicRMWInst::Or || Op == AtomicRMWInst::Xor ||
          Op == AtomicRMWInst::And) &&
         "Unable to widen operation");

  PartwordMaskValues PMV =
      createMaskInstrs(Builder, AI->getModule(), AI->getType(), AI->getPointerOperand(),
                       AI->getAlign(), MinWordSize);

  Value *ValOperand_Shifted = Builder.CreateShl(
      Builder.CreateZExt(AI->getValOperand(), PMV.WordType), PMV.ShiftAmt,
      "ValOperand_Shifted");

  Value *NewOperand;

  if (Op == AtomicRMWInst::And)
    NewOperand = Builder.CreateOr(ValOperand_Shifted, PMV.Inv_Mask, "AndOperand");
  else
    NewOperand = ValOperand_Shifted;

  AtomicRMWInst *NewAI =
      Builder.CreateAtomicRMW(Op, PMV.AlignedAddr, NewOperand, PMV.AlignedAddrAlignment,
                              AI->getOrdering(), AI->getSyncScopeID());
  NewAI->setVolatile(AI->isVolatile());

  copyMetadataForAtomic(*NewAI, *AI);

  Value *FinalOldResult = extractMaskedValue(Builder, NewAI, PMV);
  AI->replaceAllUsesWith(FinalOldResult);
  AI->eraseFromParent();
  return NewAI;
}

void wrapPartwordMaskValues(const PartwordMaskValues &PMV, LLVMExtraPartwordMaskValues *Out) {
  Out->WordType = wrap(PMV.WordType);
  Out->ValueType = wrap(PMV.ValueType);
  Out->IntValueType = wrap(PMV.IntValueType);
  Out->AlignedAddr = wrap(PMV.AlignedAddr);
  Out->AlignedAddrAlignment = PMV.AlignedAddrAlignment.value();
  Out->ShiftAmt = wrap(PMV.ShiftAmt);
  Out->Mask = wrap(PMV.Mask);
  Out->InvMask = wrap(PMV.Inv_Mask);
}

PartwordMaskValues unwrapPartwordMaskValues(const LLVMExtraPartwordMaskValues *In) {
  PartwordMaskValues PMV;
  PMV.WordType = unwrap(In->WordType);
  PMV.ValueType = unwrap(In->ValueType);
  PMV.IntValueType = unwrap(In->IntValueType);
  PMV.AlignedAddr = unwrap(In->AlignedAddr);
  PMV.AlignedAddrAlignment = Align(In->AlignedAddrAlignment);
  PMV.ShiftAmt = unwrap(In->ShiftAmt);
  PMV.Mask = unwrap(In->Mask);
  PMV.Inv_Mask = In->InvMask ? unwrap(In->InvMask) : nullptr;
  return PMV;
}

} // namespace

LLVMValueRef LLVMExtraBuildAtomicRMWValue(LLVMBuilderRef B, unsigned Op, LLVMValueRef Loaded,
                                          LLVMValueRef Val) {
  return wrap(
      buildAtomicRMWValue(unwrapRMWBinOp(Op), *unwrap(B), unwrap(Loaded), unwrap(Val)));
}

LLVMValueRef LLVMExtraBuildCmpXchgValue(LLVMBuilderRef B, LLVMValueRef PointerVal, LLVMValueRef Cmp,
                                        LLVMValueRef Val, unsigned Alignment,
                                        LLVMValueRef *Success) {
#if LLVM_VERSION_MAJOR >= 20
  auto [Orig, Equal] =
      buildCmpXchgValue(*unwrap(B), unwrap(PointerVal), unwrap(Cmp), unwrap(Val), Align(Alignment));
#else
  // copied from llvm/lib/Transforms/Utils/LowerAtomic.cpp of LLVM 20
  IRBuilder<> &Builder = *unwrap(B);
  Value *V = unwrap(Val);
  LoadInst *Orig = Builder.CreateAlignedLoad(V->getType(), unwrap(PointerVal), Align(Alignment));
  Value *Equal = Builder.CreateICmpEQ(Orig, unwrap(Cmp));
  Value *Res = Builder.CreateSelect(Equal, V, Orig);
  Builder.CreateAlignedStore(Res, unwrap(PointerVal), Align(Alignment));
#endif
  *Success = wrap(Equal);
  return wrap(Orig);
}

// based on lowerAtomicRMWInst from LowerAtomic.cpp, which uses the ABI alignment of the
// type and drops volatility
LLVMBool LLVMExtraLowerAtomicRMWInst(LLVMValueRef Inst) {
  AtomicRMWInst *RMWI = unwrap<AtomicRMWInst>(Inst);
  IRBuilder<> Builder(RMWI);
  Value *Ptr = RMWI->getPointerOperand();
  Value *Val = RMWI->getValOperand();

  LoadInst *Orig = Builder.CreateAlignedLoad(Val->getType(), Ptr, RMWI->getAlign());
  Orig->setVolatile(RMWI->isVolatile());
  Value *Res = buildAtomicRMWValue(RMWI->getOperation(), Builder, Orig, Val);
  StoreInst *Store = Builder.CreateAlignedStore(Res, Ptr, RMWI->getAlign());
  Store->setVolatile(RMWI->isVolatile());

  RMWI->replaceAllUsesWith(Orig);
  RMWI->eraseFromParent();
  return true;
}

// based on lowerAtomicCmpXchgInst from LowerAtomic.cpp, which drops volatility. A failed
// cmpxchg doesn't store, so a volatile one is lowered to a conditional store.
LLVMBool LLVMExtraLowerAtomicCmpXchgInst(LLVMValueRef Inst) {
  AtomicCmpXchgInst *CXI = unwrap<AtomicCmpXchgInst>(Inst);
  IRBuilder<> Builder(CXI);
  Value *Ptr = CXI->getPointerOperand();
  Value *Cmp = CXI->getCompareOperand();
  Value *Val = CXI->getNewValOperand();

  LoadInst *Orig = Builder.CreateAlignedLoad(Val->getType(), Ptr, CXI->getAlign());
  Orig->setVolatile(CXI->isVolatile());
  Value *Equal = Builder.CreateICmpEQ(Orig, Cmp);
  if (CXI->isVolatile()) {
    BasicBlock *BB = CXI->getParent();
    BasicBlock *EndBB = BB->splitBasicBlock(CXI->getIterator(), "cmpxchg.end");
    BasicBlock *StoreBB =
        BasicBlock::Create(Builder.getContext(), "cmpxchg.store", BB->getParent(), EndBB);
    // replace the unconditional branch that splitBasicBlock added
    std::prev(BB->end())->eraseFromParent();
    Builder.SetInsertPoint(BB);
    Builder.CreateCondBr(Equal, StoreBB, EndBB);
    Builder.SetInsertPoint(StoreBB);
    Builder.CreateAlignedStore(Val, Ptr, CXI->getAlign())->setVolatile(true);
    Builder.CreateBr(EndBB);
    Builder.SetInsertPoint(CXI);
  } else {
    Value *Res = Builder.CreateSelect(Equal, Val, Orig);
    Builder.CreateAlignedStore(Res, Ptr, CXI->getAlign());
  }

  Value *Result = Builder.CreateInsertValue(PoisonValue::get(CXI->getType()), Orig, 0);
  Result = Builder.CreateInsertValue(Result, Equal, 1);
  CXI->replaceAllUsesWith(Result);
  CXI->eraseFromParent();
  return true;
}

// based on llvm::expandAtomicRMWToCmpXchg from AtomicExpandPass.cpp
LLVMBool LLVMExtraExpandAtomicRMWToCmpXchg(LLVMValueRef Inst) {
  AtomicRMWInst *AI = unwrap<AtomicRMWInst>(Inst);
  IRBuilder<> Builder(AI);
  Value *Loaded = insertRMWCmpXchgLoop(
      Builder, AI->getType(), AI->getPointerOperand(), AI->getAlign(), AI->getOrdering(),
      AI->getSyncScopeID(), AI->isVolatile(),
      [&](IRBuilderBase &Builder, Value *Loaded) {
        return buildAtomicRMWValue(AI->getOperation(), Builder, Loaded, AI->getValOperand());
      },
      AI);

  AI->replaceAllUsesWith(Loaded);
  AI->eraseFromParent();
  return true;
}

// copied from AtomicExpandImpl::convertAtomic{Load,Store,Xchg}ToIntegerType, using the data
// layout instead of the target to determine the integer type, and handling typed pointers
// (like LLVM 15)
namespace {
Value *castPointerOperand(IRBuilderBase &Builder, Value *Addr, Type *NewTy) {
#if LLVM_VERSION_MAJOR < 17
  if (!cast<PointerType>(Addr->getType())->isOpaque())
    return Builder.CreateBitCast(
        Addr, NewTy->getPointerTo(Addr->getType()->getPointerAddressSpace()));
#endif
  return Addr;
}
} // namespace

LLVMValueRef LLVMExtraCastAtomicToInteger(LLVMValueRef Inst) {
  Instruction *I = unwrap<Instruction>(Inst);
  const DataLayout &DL = I->getModule()->getDataLayout();
  IRBuilder<> Builder(I);

  if (auto *LI = dyn_cast<LoadInst>(I)) {
    if (LI->getType()->isIntegerTy())
      return Inst;
    Type *NewTy = Builder.getIntNTy(DL.getTypeStoreSizeInBits(LI->getType()));
    Value *Addr = castPointerOperand(Builder, LI->getPointerOperand(), NewTy);
    LoadInst *NewLI = Builder.CreateLoad(NewTy, Addr);
    NewLI->setAlignment(LI->getAlign());
    NewLI->setVolatile(LI->isVolatile());
    NewLI->setAtomic(LI->getOrdering(), LI->getSyncScopeID());
    copyMetadataForAtomic(*NewLI, *LI);
    Value *NewVal = LI->getType()->isPointerTy() ? Builder.CreateIntToPtr(NewLI, LI->getType())
                                                 : Builder.CreateBitCast(NewLI, LI->getType());
    LI->replaceAllUsesWith(NewVal);
    LI->eraseFromParent();
    return wrap(NewLI);
  }

  if (auto *SI = dyn_cast<StoreInst>(I)) {
    Value *Val = SI->getValueOperand();
    if (Val->getType()->isIntegerTy())
      return Inst;
    Type *NewTy = Builder.getIntNTy(DL.getTypeStoreSizeInBits(Val->getType()));
    Value *NewVal = Val->getType()->isPointerTy() ? Builder.CreatePtrToInt(Val, NewTy)
                                                  : Builder.CreateBitCast(Val, NewTy);
    Value *Addr = castPointerOperand(Builder, SI->getPointerOperand(), NewTy);
    StoreInst *NewSI = Builder.CreateStore(NewVal, Addr);
    NewSI->setAlignment(SI->getAlign());
    NewSI->setVolatile(SI->isVolatile());
    NewSI->setAtomic(SI->getOrdering(), SI->getSyncScopeID());
    copyMetadataForAtomic(*NewSI, *SI);
    SI->eraseFromParent();
    return wrap(NewSI);
  }

  auto *RMWI = cast<AtomicRMWInst>(I);
  assert(RMWI->getOperation() == AtomicRMWInst::Xchg);
  if (RMWI->getType()->isIntegerTy())
    return Inst;
  Type *NewTy = Builder.getIntNTy(DL.getTypeStoreSizeInBits(RMWI->getType()));
  Value *Val = RMWI->getValOperand();
  Value *NewVal = Val->getType()->isPointerTy() ? Builder.CreatePtrToInt(Val, NewTy)
                                                : Builder.CreateBitCast(Val, NewTy);
  Value *Addr = castPointerOperand(Builder, RMWI->getPointerOperand(), NewTy);
  AtomicRMWInst *NewRMWI =
      Builder.CreateAtomicRMW(AtomicRMWInst::Xchg, Addr, NewVal, RMWI->getAlign(),
                              RMWI->getOrdering(), RMWI->getSyncScopeID());
  NewRMWI->setVolatile(RMWI->isVolatile());
  copyMetadataForAtomic(*NewRMWI, *RMWI);
  Value *NewRVal = RMWI->getType()->isPointerTy()
                       ? Builder.CreateIntToPtr(NewRMWI, RMWI->getType())
                       : Builder.CreateBitCast(NewRMWI, RMWI->getType());
  RMWI->replaceAllUsesWith(NewRVal);
  RMWI->eraseFromParent();
  return wrap(NewRMWI);
}

void LLVMExtraCreatePartwordMaskValues(LLVMBuilderRef B, LLVMTypeRef ValueType,
                                       LLVMValueRef Addr, unsigned AddrAlign,
                                       unsigned MinWordSize, LLVMExtraPartwordMaskValues *PMV) {
  IRBuilder<> &Builder = *unwrap(B);
  Module *M = Builder.GetInsertBlock()->getModule();
  wrapPartwordMaskValues(createMaskInstrs(Builder, M, unwrap(ValueType), unwrap(Addr),
                                          Align(AddrAlign), MinWordSize),
                         PMV);
}

LLVMValueRef LLVMExtraExtractMaskedValue(LLVMBuilderRef B, LLVMValueRef WideWord,
                                         const LLVMExtraPartwordMaskValues *PMV) {
  return wrap(extractMaskedValue(*unwrap(B), unwrap(WideWord), unwrapPartwordMaskValues(PMV)));
}

LLVMValueRef LLVMExtraInsertMaskedValue(LLVMBuilderRef B, LLVMValueRef WideWord,
                                        LLVMValueRef Updated,
                                        const LLVMExtraPartwordMaskValues *PMV) {
  return wrap(insertMaskedValue(*unwrap(B), unwrap(WideWord), unwrap(Updated),
                                unwrapPartwordMaskValues(PMV)));
}

// copied from AtomicExpandImpl::expandPartwordAtomicRMW, for the cmpxchg expansion kind
LLVMBool LLVMExtraExpandPartwordAtomicRMW(LLVMValueRef RMWI, unsigned MinWordSize) {
  AtomicRMWInst *AI = unwrap<AtomicRMWInst>(RMWI);
  const DataLayout &DL = AI->getModule()->getDataLayout();
  if (DL.getTypeStoreSize(AI->getType()) >= MinWordSize)
    return false;

  // Widen And/Or/Xor, which don't need a loop
  AtomicRMWInst::BinOp Op = AI->getOperation();
  if (Op == AtomicRMWInst::Or || Op == AtomicRMWInst::Xor || Op == AtomicRMWInst::And) {
    widenPartwordAtomicRMW(AI, MinWordSize);
    return true;
  }
  AtomicOrdering MemOpOrder = AI->getOrdering();
  SyncScope::ID SSID = AI->getSyncScopeID();

  IRBuilder<> Builder(AI);

  PartwordMaskValues PMV =
      createMaskInstrs(Builder, AI->getModule(), AI->getType(), AI->getPointerOperand(),
                       AI->getAlign(), MinWordSize);

  Value *ValOperand_Shifted = nullptr;
  if (Op == AtomicRMWInst::Xchg || Op == AtomicRMWInst::Add || Op == AtomicRMWInst::Sub ||
      Op == AtomicRMWInst::Nand) {
    Value *ValOp = Builder.CreateBitCast(AI->getValOperand(), PMV.IntValueType);
    ValOperand_Shifted = Builder.CreateShl(Builder.CreateZExt(ValOp, PMV.WordType),
                                           PMV.ShiftAmt, "ValOperand_Shifted");
  }

  auto PerformPartwordOp = [&](IRBuilderBase &Builder, Value *Loaded) {
    return performMaskedAtomicOp(Op, Builder, Loaded, ValOperand_Shifted,
                                 AI->getValOperand(), PMV);
  };

  Value *OldResult =
      insertRMWCmpXchgLoop(Builder, PMV.WordType, PMV.AlignedAddr, PMV.AlignedAddrAlignment,
                           MemOpOrder, SSID, AI->isVolatile(), PerformPartwordOp, AI);

  Value *FinalOldResult = extractMaskedValue(Builder, OldResult, PMV);
  AI->replaceAllUsesWith(FinalOldResult);
  AI->eraseFromParent();
  return true;
}

// copied from AtomicExpandImpl::expandPartwordCmpXchg, additionally copying the metadata of
// the original cmpxchg (AtomicExpand only propagates !mmra through its IRBuilder)
LLVMBool LLVMExtraExpandPartwordCmpXchg(LLVMValueRef CXI, unsigned MinWordSize) {
  AtomicCmpXchgInst *CI = unwrap<AtomicCmpXchgInst>(CXI);
  const DataLayout &DL = CI->getModule()->getDataLayout();
  if (DL.getTypeStoreSize(CI->getCompareOperand()->getType()) >= MinWordSize)
    return false;

  Value *Addr = CI->getPointerOperand();
  Value *Cmp = CI->getCompareOperand();
  Value *NewVal = CI->getNewValOperand();

  BasicBlock *BB = CI->getParent();
  Function *F = BB->getParent();
  IRBuilder<> Builder(CI);
  LLVMContext &Ctx = Builder.getContext();

  BasicBlock *EndBB = BB->splitBasicBlock(CI->getIterator(), "partword.cmpxchg.end");
  auto FailureBB = BasicBlock::Create(Ctx, "partword.cmpxchg.failure", F, EndBB);
  auto LoopBB = BasicBlock::Create(Ctx, "partword.cmpxchg.loop", F, FailureBB);

  // The split call above "helpfully" added a branch at the end of BB
  // (to the wrong place).
  std::prev(BB->end())->eraseFromParent();
  Builder.SetInsertPoint(BB);

  PartwordMaskValues PMV = createMaskInstrs(Builder, CI->getModule(), Cmp->getType(), Addr,
                                            CI->getAlign(), MinWordSize);

  // Shift the incoming values over, into the right location in the word.
  Value *NewVal_Shifted =
      Builder.CreateShl(Builder.CreateZExt(NewVal, PMV.WordType), PMV.ShiftAmt);
  Value *Cmp_Shifted = Builder.CreateShl(Builder.CreateZExt(Cmp, PMV.WordType), PMV.ShiftAmt);

  // Load the entire current word, and mask into place the expected and new
  // values
  Value *InitLoaded =
      createInitialLoad(Builder, PMV.WordType, PMV.AlignedAddr, PMV.AlignedAddrAlignment,
                        CI->getSyncScopeID(), CI->isVolatile());
  Value *InitLoaded_MaskOut = Builder.CreateAnd(InitLoaded, PMV.Inv_Mask);
  Builder.CreateBr(LoopBB);

  // partword.cmpxchg.loop:
  Builder.SetInsertPoint(LoopBB);
  PHINode *Loaded_MaskOut = Builder.CreatePHI(PMV.WordType, 2);
  Loaded_MaskOut->addIncoming(InitLoaded_MaskOut, BB);

  // Mask/Or the expected and new values into place in the loaded word.
  Value *FullWord_NewVal = Builder.CreateOr(Loaded_MaskOut, NewVal_Shifted);
  Value *FullWord_Cmp = Builder.CreateOr(Loaded_MaskOut, Cmp_Shifted);
  AtomicCmpXchgInst *NewCI = Builder.CreateAtomicCmpXchg(
      PMV.AlignedAddr, FullWord_Cmp, FullWord_NewVal, PMV.AlignedAddrAlignment,
      CI->getSuccessOrdering(), CI->getFailureOrdering(), CI->getSyncScopeID());
  NewCI->setVolatile(CI->isVolatile());
  // When we're building a strong cmpxchg, we need a loop, so you
  // might think we could use a weak cmpxchg inside. But, using strong
  // allows the below comparison for ShouldContinue, and we're
  // expecting the underlying cmpxchg to be a machine instruction,
  // which is strong anyways.
  NewCI->setWeak(CI->isWeak());
  copyMetadataForAtomic(*NewCI, *CI);

  Value *OldVal = Builder.CreateExtractValue(NewCI, 0);
  Value *Success = Builder.CreateExtractValue(NewCI, 1);

  if (CI->isWeak())
    Builder.CreateBr(EndBB);
  else
    Builder.CreateCondBr(Success, EndBB, FailureBB);

  // partword.cmpxchg.failure:
  Builder.SetInsertPoint(FailureBB);
  // Upon failure, verify that the masked-out part of the loaded value
  // has been modified.  If it didn't, abort the cmpxchg, since the
  // masked-in part must've.
  Value *OldVal_MaskOut = Builder.CreateAnd(OldVal, PMV.Inv_Mask);
  Value *ShouldContinue = Builder.CreateICmpNE(Loaded_MaskOut, OldVal_MaskOut);
  Builder.CreateCondBr(ShouldContinue, LoopBB, EndBB);

  // Add the second value to the phi from above
  Loaded_MaskOut->addIncoming(OldVal_MaskOut, FailureBB);

  // partword.cmpxchg.end:
  Builder.SetInsertPoint(CI);

  Value *FinalOldVal = extractMaskedValue(Builder, OldVal, PMV);
  Value *Res = PoisonValue::get(CI->getType());
  Res = Builder.CreateInsertValue(Res, FinalOldVal, 0);
  Res = Builder.CreateInsertValue(Res, Success, 1);

  CI->replaceAllUsesWith(Res);
  CI->eraseFromParent();
  return true;
}
