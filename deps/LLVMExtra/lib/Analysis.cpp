#include "LLVMExtra.h"

#include <llvm/ADT/APInt.h>
#include <llvm/Analysis/AssumptionCache.h>
#include <llvm/Analysis/LazyValueInfo.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/ConstantRange.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Operator.h>
#include <llvm/Support/KnownBits.h>

using namespace llvm;

DEFINE_STDCXX_CONVERSION_FUNCTIONS(AssumptionCache, LLVMAssumptionCacheRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(LazyValueInfo, LLVMLazyValueInfoRef)
// defined in Core.cpp
DEFINE_STDCXX_CONVERSION_FUNCTIONS(DominatorTree, LLVMDominatorTreeRef)

// APInt values are passed as an array of 64-bit words (least significant first), of which
// there are as many as needed for the bit width
static APInt readAPInt(unsigned NumBits, const uint64_t *Words) {
  return APInt(NumBits, ArrayRef<uint64_t>(Words, divideCeil(NumBits, 64)));
}

static void writeAPInt(const APInt &Value, uint64_t *Words) {
  const uint64_t *Raw = Value.getRawData();
  std::copy(Raw, Raw + Value.getNumWords(), Words);
}

static ConstantRange readRange(unsigned NumBits, const uint64_t *Lower,
                               const uint64_t *Upper) {
  return ConstantRange(readAPInt(NumBits, Lower), readAPInt(NumBits, Upper));
}

static void writeRange(const ConstantRange &CR, uint64_t *Lower, uint64_t *Upper) {
  writeAPInt(CR.getLower(), Lower);
  writeAPInt(CR.getUpper(), Upper);
}

static unsigned mapFromLLVMOpcode(LLVMOpcode Code) {
  switch (Code) {
#define HANDLE_INST(num, opc, clas)                                                        \
  case LLVM##opc:                                                                          \
    return num;
#include <llvm/IR/Instruction.def>
#undef HANDLE_INST
  }
  llvm_unreachable("unknown opcode");
}


// ConstantRange

LLVMBool LLVMExtraConstantRangeBinaryOp(LLVMOpcode Opcode, unsigned NoWrapKind,
                                        unsigned NumBits, const uint64_t *LowerA,
                                        const uint64_t *UpperA, const uint64_t *LowerB,
                                        const uint64_t *UpperB, uint64_t *LowerOut,
                                        uint64_t *UpperOut) {
  unsigned Op = mapFromLLVMOpcode(Opcode);
  if (!Instruction::isBinaryOp(Op))
    return false;
  auto A = readRange(NumBits, LowerA, UpperA);
  auto B = readRange(NumBits, LowerB, UpperB);
  unsigned Flags = 0;
  if (NoWrapKind & LLVMExtraNoUnsignedWrap)
    Flags |= OverflowingBinaryOperator::NoUnsignedWrap;
  if (NoWrapKind & LLVMExtraNoSignedWrap)
    Flags |= OverflowingBinaryOperator::NoSignedWrap;
  // operations that have no special handling of no-wrap flags (e.g., multiplication before
  // LLVM 19, or left shifts before LLVM 20) conservatively ignore them
  auto R = Flags ? A.overflowingBinaryOp(static_cast<Instruction::BinaryOps>(Op), B, Flags)
                 : A.binaryOp(static_cast<Instruction::BinaryOps>(Op), B);
  writeRange(R, LowerOut, UpperOut);
  return true;
}

LLVMBool LLVMExtraConstantRangeCastOp(LLVMOpcode Opcode, unsigned NumBits,
                                      const uint64_t *Lower, const uint64_t *Upper,
                                      unsigned ResultBits, uint64_t *LowerOut,
                                      uint64_t *UpperOut) {
  auto CR = readRange(NumBits, Lower, Upper);
  ConstantRange R(ResultBits, true);
  switch (mapFromLLVMOpcode(Opcode)) {
  case Instruction::Trunc:
    if (ResultBits >= NumBits)
      return false;
    R = CR.truncate(ResultBits);
    break;
  case Instruction::ZExt:
    if (ResultBits <= NumBits)
      return false;
    R = CR.zeroExtend(ResultBits);
    break;
  case Instruction::SExt:
    if (ResultBits <= NumBits)
      return false;
    R = CR.signExtend(ResultBits);
    break;
  default:
    return false;
  }
  writeRange(R, LowerOut, UpperOut);
  return true;
}

static ConstantRange::PreferredRangeType unwrapRangeType(LLVMExtraPreferredRangeType Type) {
  switch (Type) {
  case LLVMExtraSmallestRange:
    return ConstantRange::Smallest;
  case LLVMExtraUnsignedRange:
    return ConstantRange::Unsigned;
  case LLVMExtraSignedRange:
    return ConstantRange::Signed;
  }
  llvm_unreachable("unknown preferred range type");
}

void LLVMExtraConstantRangeIntersectWith(unsigned NumBits, const uint64_t *LowerA,
                                         const uint64_t *UpperA, const uint64_t *LowerB,
                                         const uint64_t *UpperB,
                                         LLVMExtraPreferredRangeType Type, uint64_t *LowerOut,
                                         uint64_t *UpperOut) {
  auto A = readRange(NumBits, LowerA, UpperA);
  auto B = readRange(NumBits, LowerB, UpperB);
  writeRange(A.intersectWith(B, unwrapRangeType(Type)), LowerOut, UpperOut);
}

void LLVMExtraConstantRangeUnionWith(unsigned NumBits, const uint64_t *LowerA,
                                     const uint64_t *UpperA, const uint64_t *LowerB,
                                     const uint64_t *UpperB, LLVMExtraPreferredRangeType Type,
                                     uint64_t *LowerOut, uint64_t *UpperOut) {
  auto A = readRange(NumBits, LowerA, UpperA);
  auto B = readRange(NumBits, LowerB, UpperB);
  writeRange(A.unionWith(B, unwrapRangeType(Type)), LowerOut, UpperOut);
}

void LLVMExtraConstantRangeMakeICmpRegion(LLVMIntPredicate Predicate, LLVMBool Satisfying,
                                          unsigned NumBits, const uint64_t *Lower,
                                          const uint64_t *Upper, uint64_t *LowerOut,
                                          uint64_t *UpperOut) {
  auto CR = readRange(NumBits, Lower, Upper);
  auto Pred = static_cast<CmpInst::Predicate>(Predicate);
  auto R = Satisfying ? ConstantRange::makeSatisfyingICmpRegion(Pred, CR)
                      : ConstantRange::makeAllowedICmpRegion(Pred, CR);
  writeRange(R, LowerOut, UpperOut);
}

void LLVMExtraConstantRangeFromKnownBits(unsigned NumBits, const uint64_t *Zero,
                                         const uint64_t *One, LLVMBool Signed,
                                         uint64_t *LowerOut, uint64_t *UpperOut) {
  KnownBits Known(NumBits);
  Known.Zero = readAPInt(NumBits, Zero);
  Known.One = readAPInt(NumBits, One);
  writeRange(ConstantRange::fromKnownBits(Known, Signed), LowerOut, UpperOut);
}

void LLVMExtraConstantRangeToKnownBits(unsigned NumBits, const uint64_t *Lower,
                                       const uint64_t *Upper, uint64_t *ZeroOut,
                                       uint64_t *OneOut) {
  KnownBits Known = readRange(NumBits, Lower, Upper).toKnownBits();
  writeAPInt(Known.Zero, ZeroOut);
  writeAPInt(Known.One, OneOut);
}

#if LLVM_VERSION_MAJOR >= 19
unsigned LLVMExtraGetConstantRangeAttributeValue(LLVMAttributeRef A, uint64_t *Lower,
                                                 uint64_t *Upper) {
  const ConstantRange &CR = unwrap(A).getRange();
  if (Lower && Upper)
    writeRange(CR, Lower, Upper);
  return CR.getBitWidth();
}
#endif


// AssumptionCache

// the assumption of an element of the cache (LLVM 22 stores the handles directly)
static Value *assumeOf(const AssumptionCache::ResultElem &Elem) { return Elem.Assume; }
static Value *assumeOf(const WeakVH &Handle) { return Handle; }

unsigned LLVMExtraAssumptionCacheGetAssumptions(LLVMAssumptionCacheRef AC,
                                                LLVMValueRef *Assumes) {
  unsigned N = 0;
  for (auto &Elem : unwrap(AC)->assumptions()) {
    Value *Assume = assumeOf(Elem);
    if (!Assume)
      continue;
    if (Assumes)
      Assumes[N] = wrap(Assume);
    N++;
  }
  return N;
}

unsigned LLVMExtraAssumptionCacheGetAssumptionsFor(LLVMAssumptionCacheRef AC, LLVMValueRef V,
                                                   LLVMValueRef *Assumes, int *Indices) {
  unsigned N = 0;
  for (auto &Elem : unwrap(AC)->assumptionsFor(unwrap(V))) {
    Value *Assume = Elem.Assume;
    if (!Assume)
      continue;
    if (Assumes) {
      Assumes[N] = wrap(Assume);
      Indices[N] = Elem.Index == AssumptionCache::ExprResultIdx ? -1 : (int)Elem.Index;
    }
    N++;
  }
  return N;
}

LLVMBool LLVMExtraAssumptionCacheRegisterAssumption(LLVMAssumptionCacheRef AC,
                                                    LLVMValueRef Assume) {
  auto *CI = dyn_cast<AssumeInst>(unwrap(Assume));
  if (!CI)
    return false;
  unwrap(AC)->registerAssumption(CI);
  return true;
}

void LLVMExtraAssumptionCacheClear(LLVMAssumptionCacheRef AC) { unwrap(AC)->clear(); }


// ValueTracking

// the data layout to use for a query: the given one, or that of the module containing the
// context instruction or the value
static const DataLayout *queryDataLayout(LLVMTargetDataRef DL, Value *V, Instruction *CxtI) {
  if (DL)
    return unwrap(DL);
  for (Value *X : {static_cast<Value *>(CxtI), V}) {
    if (!X)
      continue;
    const Module *M = nullptr;
    if (auto *I = dyn_cast<Instruction>(X))
      M = I->getModule();
    else if (auto *A = dyn_cast<Argument>(X))
      M = A->getParent() ? A->getParent()->getParent() : nullptr;
    else if (auto *G = dyn_cast<GlobalValue>(X))
      M = G->getParent();
    if (M)
      return &M->getDataLayout();
  }
  return nullptr;
}

LLVMBool LLVMExtraComputeConstantRange(LLVMValueRef V, LLVMBool ForSigned,
                                       LLVMBool UseInstrInfo, LLVMAssumptionCacheRef AC,
                                       LLVMValueRef CxtI, LLVMDominatorTreeRef DT,
                                       LLVMTargetDataRef DL, uint64_t *Lower,
                                       uint64_t *Upper) {
  Value *Val = unwrap(V);
  if (!Val->getType()->isIntOrIntVectorTy())
    return false;
  auto *Ctx = CxtI ? unwrap<Instruction>(CxtI) : nullptr;
#if LLVM_VERSION_MAJOR >= 23
  const DataLayout *Layout = queryDataLayout(DL, Val, Ctx);
  if (!Layout)
    return false;
  SimplifyQuery Q(*Layout, DT ? unwrap(DT) : nullptr, AC ? unwrap(AC) : nullptr, Ctx,
                  UseInstrInfo);
  auto CR = computeConstantRange(Val, ForSigned, Q);
#else
  auto CR = computeConstantRange(Val, ForSigned, UseInstrInfo, AC ? unwrap(AC) : nullptr,
                                 Ctx, DT ? unwrap(DT) : nullptr);
#endif
  writeRange(CR, Lower, Upper);
  return true;
}

LLVMBool LLVMExtraComputeKnownBits(LLVMValueRef V, LLVMBool UseInstrInfo,
                                   LLVMAssumptionCacheRef AC, LLVMValueRef CxtI,
                                   LLVMDominatorTreeRef DT, LLVMTargetDataRef DL,
                                   uint64_t *Zero, uint64_t *One) {
  Value *Val = unwrap(V);
  if (!Val->getType()->isIntOrIntVectorTy())
    return false;
  auto *Ctx = CxtI ? unwrap<Instruction>(CxtI) : nullptr;
  const DataLayout *Layout = queryDataLayout(DL, Val, Ctx);
  if (!Layout)
    return false;
#if LLVM_VERSION_MAJOR >= 21
  KnownBits Known = computeKnownBits(Val, *Layout, AC ? unwrap(AC) : nullptr, Ctx,
                                     DT ? unwrap(DT) : nullptr, UseInstrInfo);
#elif LLVM_VERSION_MAJOR >= 17
  KnownBits Known = computeKnownBits(Val, *Layout, /*Depth=*/0, AC ? unwrap(AC) : nullptr,
                                     Ctx, DT ? unwrap(DT) : nullptr, UseInstrInfo);
#else
  KnownBits Known = computeKnownBits(Val, *Layout, /*Depth=*/0, AC ? unwrap(AC) : nullptr,
                                     Ctx, DT ? unwrap(DT) : nullptr, /*ORE=*/nullptr,
                                     UseInstrInfo);
#endif
  writeAPInt(Known.Zero, Zero);
  writeAPInt(Known.One, One);
  return true;
}

LLVMBool LLVMExtraIsValidAssumeForContext(LLVMValueRef Assume, LLVMValueRef CxtI,
                                          LLVMDominatorTreeRef DT) {
  return isValidAssumeForContext(unwrap<Instruction>(Assume), unwrap<Instruction>(CxtI),
                                 DT ? unwrap(DT) : nullptr);
}

LLVMBool LLVMExtraIsGuaranteedNotToBePoison(LLVMValueRef V, LLVMAssumptionCacheRef AC,
                                            LLVMValueRef CxtI, LLVMDominatorTreeRef DT) {
  return isGuaranteedNotToBePoison(unwrap(V), AC ? unwrap(AC) : nullptr,
                                   CxtI ? unwrap<Instruction>(CxtI) : nullptr,
                                   DT ? unwrap(DT) : nullptr);
}

LLVMBool LLVMExtraProgramUndefinedIfPoison(LLVMValueRef Inst) {
  return programUndefinedIfPoison(unwrap<Instruction>(Inst));
}


// LazyValueInfo

LLVMBool LLVMExtraLazyValueInfoGetConstantRange(LLVMLazyValueInfoRef LVI, LLVMValueRef V,
                                                LLVMValueRef CxtI, LLVMBool UndefAllowed,
                                                uint64_t *Lower, uint64_t *Upper) {
  Value *Val = unwrap(V);
  if (!Val->getType()->isIntOrIntVectorTy())
    return false;
  writeRange(unwrap(LVI)->getConstantRange(Val, unwrap<Instruction>(CxtI), UndefAllowed),
             Lower, Upper);
  return true;
}

LLVMBool LLVMExtraLazyValueInfoGetConstantRangeOnEdge(LLVMLazyValueInfoRef LVI,
                                                      LLVMValueRef V, LLVMBasicBlockRef From,
                                                      LLVMBasicBlockRef To,
                                                      LLVMValueRef CxtI, uint64_t *Lower,
                                                      uint64_t *Upper) {
  Value *Val = unwrap(V);
  if (!Val->getType()->isIntOrIntVectorTy())
    return false;
  writeRange(unwrap(LVI)->getConstantRangeOnEdge(Val, unwrap(From), unwrap(To),
                                                 CxtI ? unwrap<Instruction>(CxtI) : nullptr),
             Lower, Upper);
  return true;
}

#if LLVM_VERSION_MAJOR >= 16
LLVMBool LLVMExtraLazyValueInfoGetConstantRangeAtUse(LLVMLazyValueInfoRef LVI, LLVMUseRef U,
                                                     LLVMBool UndefAllowed, uint64_t *Lower,
                                                     uint64_t *Upper) {
  Use *TheUse = unwrap(U);
  if (!TheUse->get()->getType()->isIntOrIntVectorTy())
    return false;
  writeRange(unwrap(LVI)->getConstantRangeAtUse(*TheUse, UndefAllowed), Lower, Upper);
  return true;
}
#endif
