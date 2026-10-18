#include "LLVMExtra.h"

#include <llvm/ADT/APInt.h>
#include <llvm/Analysis/AssumptionCache.h>
#include <llvm/Analysis/ConstantFolding.h>
#include <llvm/Analysis/DomTreeUpdater.h>
#include <llvm/Analysis/InlineCost.h>
#include <llvm/Analysis/InstructionSimplify.h>
#include <llvm/Analysis/LazyValueInfo.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/LoopIterator.h>
#include <llvm/Analysis/MemorySSA.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>
#include <llvm/Analysis/ValueTracking.h>
#if LLVM_VERSION_MAJOR >= 18
#include <llvm/Analysis/SimplifyQuery.h>
#endif
#include <llvm/IR/Attributes.h>
#include <llvm/IR/ConstantRange.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Operator.h>
#include <llvm/Support/KnownBits.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/PromoteMemToReg.h>

#include <optional>

using namespace llvm;

DEFINE_STDCXX_CONVERSION_FUNCTIONS(AssumptionCache, LLVMAssumptionCacheRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(LazyValueInfo, LLVMLazyValueInfoRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(ScalarEvolution, LLVMScalarEvolutionRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(LoopInfo, LLVMLoopInfoRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(Loop, LLVMLoopRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(MemorySSA, LLVMMemorySSARef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(MemoryAccess, LLVMMemoryAccessRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(MemorySSAUpdater, LLVMMemorySSAUpdaterRef)

static const SCEV *unwrap(LLVMSCEVRef S) { return reinterpret_cast<const SCEV *>(S); }
static LLVMSCEVRef wrap(const SCEV *S) {
  return reinterpret_cast<LLVMSCEVRef>(const_cast<SCEV *>(S));
}
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

LLVMBool LLVMExtraConstantRangeContains(unsigned NumBits, const uint64_t *LowerA,
                                        const uint64_t *UpperA, const uint64_t *LowerB,
                                        const uint64_t *UpperB) {
  return readRange(NumBits, LowerA, UpperA).contains(readRange(NumBits, LowerB, UpperB));
}

unsigned LLVMExtraGetConstantRangeFromMetadata(LLVMMetadataRef MD, uint64_t *Lower,
                                               uint64_t *Upper) {
  // getConstantRangeFromMetadata asserts that the metadata is valid, so check it first
  auto *Node = dyn_cast<MDNode>(unwrap(MD));
  if (!Node || Node->getNumOperands() == 0 || Node->getNumOperands() % 2 != 0)
    return 0;
  unsigned NumBits = 0;
  for (unsigned I = 0; I < Node->getNumOperands(); I += 2) {
    auto *Lo = mdconst::dyn_extract_or_null<ConstantInt>(Node->getOperand(I));
    auto *Hi = mdconst::dyn_extract_or_null<ConstantInt>(Node->getOperand(I + 1));
    if (!Lo || !Hi || Lo->getBitWidth() != Hi->getBitWidth() ||
        (NumBits && Lo->getBitWidth() != NumBits))
      return 0;
    NumBits = Lo->getBitWidth();
    if (Lo->getValue() == Hi->getValue() && !Lo->getValue().isMaxValue() &&
        !Lo->getValue().isMinValue())
      return 0;
  }
  if (Lower && Upper)
    writeRange(getConstantRangeFromMetadata(*Node), Lower, Upper);
  return NumBits;
}

LLVMMetadataRef LLVMExtraCreateRangeMetadata(LLVMContextRef C, unsigned NumBits,
                                             const uint64_t *Lower, const uint64_t *Upper) {
  MDBuilder Builder(*unwrap(C));
  return wrap(Builder.createRange(readAPInt(NumBits, Lower), readAPInt(NumBits, Upper)));
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
  if (auto *CI = dyn_cast<ConstantInt>(Val)) {
    writeRange(ConstantRange(CI->getValue()), Lower, Upper);
    return true;
  }
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

LLVMBool LLVMExtraIsGuaranteedNotToBeUndefOrPoison(LLVMValueRef V,
                                                   LLVMAssumptionCacheRef AC,
                                                   LLVMValueRef CxtI,
                                                   LLVMDominatorTreeRef DT) {
  return isGuaranteedNotToBeUndefOrPoison(unwrap(V), AC ? unwrap(AC) : nullptr,
                                          CxtI ? unwrap<Instruction>(CxtI) : nullptr,
                                          DT ? unwrap(DT) : nullptr);
}

LLVMBool LLVMExtraIsKnown(LLVMExtraKnownFact Fact, LLVMValueRef V, LLVMBool UseInstrInfo,
                          LLVMAssumptionCacheRef AC, LLVMValueRef CxtI,
                          LLVMDominatorTreeRef DT, LLVMTargetDataRef DL, LLVMBool *Known) {
  Value *Val = unwrap(V);
  auto *Ctx = CxtI ? unwrap<Instruction>(CxtI) : nullptr;
  const DataLayout *Layout = queryDataLayout(DL, Val, Ctx);
  if (!Layout)
    return false;
  AssumptionCache *Cache = AC ? unwrap(AC) : nullptr;
  DominatorTree *Tree = DT ? unwrap(DT) : nullptr;
#if LLVM_VERSION_MAJOR >= 18
  SimplifyQuery Q(*Layout, Tree, Cache, Ctx, UseInstrInfo);
#endif
  bool Result = false;
  switch (Fact) {
  case LLVMExtraKnownNonNegative:
#if LLVM_VERSION_MAJOR >= 18
    Result = isKnownNonNegative(Val, Q);
#else
    Result = isKnownNonNegative(Val, *Layout, /*Depth=*/0, Cache, Ctx, Tree, UseInstrInfo);
#endif
    break;
  case LLVMExtraKnownPositive:
#if LLVM_VERSION_MAJOR >= 18
    Result = isKnownPositive(Val, Q);
#else
    Result = isKnownPositive(Val, *Layout, /*Depth=*/0, Cache, Ctx, Tree, UseInstrInfo);
#endif
    break;
  case LLVMExtraKnownNonZero:
#if LLVM_VERSION_MAJOR >= 19
    Result = isKnownNonZero(Val, Q);
#else
    Result = isKnownNonZero(Val, *Layout, /*Depth=*/0, Cache, Ctx, Tree, UseInstrInfo);
#endif
    break;
  }
  *Known = Result;
  return true;
}

unsigned LLVMExtraComputeNumSignBits(LLVMValueRef V, LLVMBool UseInstrInfo,
                                     LLVMAssumptionCacheRef AC, LLVMValueRef CxtI,
                                     LLVMDominatorTreeRef DT, LLVMTargetDataRef DL) {
  Value *Val = unwrap(V);
  auto *Ctx = CxtI ? unwrap<Instruction>(CxtI) : nullptr;
  const DataLayout *Layout = queryDataLayout(DL, Val, Ctx);
  if (!Layout)
    return 0;
  // the depth argument moved to the end in LLVM 21
  return ComputeNumSignBits(Val, *Layout,
#if LLVM_VERSION_MAJOR < 21
                            /*Depth=*/0,
#endif
                            AC ? unwrap(AC) : nullptr, Ctx, DT ? unwrap(DT) : nullptr,
                            UseInstrInfo);
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


// ScalarEvolution

LLVMBool LLVMExtraScalarEvolutionIsSCEVable(LLVMScalarEvolutionRef SE, LLVMTypeRef Ty) {
  return unwrap(SE)->isSCEVable(unwrap(Ty));
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetSCEV(LLVMScalarEvolutionRef SE, LLVMValueRef V) {
  return wrap(unwrap(SE)->getSCEV(unwrap(V)));
}

// whether expressions can be added: they need to have the same effective type (with
// pointers treated as integers of their index width), and at most one can be a pointer
static bool areCompatibleOperands(ScalarEvolution &SE, ArrayRef<const SCEV *> Ops,
                                  unsigned MaxPointers) {
  unsigned NumPointers = 0;
  Type *Ty = nullptr;
  for (const SCEV *S : Ops) {
    if (isa<SCEVCouldNotCompute>(S))
      return false;
    Type *ETy = SE.getEffectiveSCEVType(S->getType());
    if (Ty && ETy != Ty)
      return false;
    Ty = ETy;
    if (S->getType()->isPointerTy())
      NumPointers++;
  }
  return NumPointers <= MaxPointers;
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetAddExpr(LLVMScalarEvolutionRef SE, LLVMSCEVRef *Ops,
                                               unsigned NumOps) {
  SmallVector<const SCEV *, 4> Operands;
  for (unsigned I = 0; I < NumOps; ++I)
    Operands.push_back(unwrap(Ops[I]));
  if (Operands.empty() || !areCompatibleOperands(*unwrap(SE), Operands, 1))
    return nullptr;
#if LLVM_VERSION_MAJOR >= 23
  SmallVector<SCEVUse, 4> Uses(Operands.begin(), Operands.end());
  return wrap(unwrap(SE)->getAddExpr(Uses));
#else
  return wrap(unwrap(SE)->getAddExpr(Operands));
#endif
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetMinusSCEV(LLVMScalarEvolutionRef SE, LLVMSCEVRef LHS,
                                                 LLVMSCEVRef RHS) {
  // the difference of two pointers is an integer (or could-not-compute if they have
  // different bases), so they can both be pointers
  if (!areCompatibleOperands(*unwrap(SE), {unwrap(LHS), unwrap(RHS)}, 2))
    return nullptr;
  return wrap(unwrap(SE)->getMinusSCEV(unwrap(LHS), unwrap(RHS)));
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetMulExpr(LLVMScalarEvolutionRef SE, LLVMSCEVRef *Ops,
                                               unsigned NumOps) {
  SmallVector<const SCEV *, 4> Operands;
  for (unsigned I = 0; I < NumOps; ++I)
    Operands.push_back(unwrap(Ops[I]));
  // pointers cannot be multiplied
  if (Operands.empty() || !areCompatibleOperands(*unwrap(SE), Operands, 0))
    return nullptr;
#if LLVM_VERSION_MAJOR >= 23
  SmallVector<SCEVUse, 4> Uses(Operands.begin(), Operands.end());
  return wrap(unwrap(SE)->getMulExpr(Uses));
#else
  return wrap(unwrap(SE)->getMulExpr(Operands));
#endif
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetCastExpr(LLVMScalarEvolutionRef SE, LLVMOpcode Opcode,
                                                LLVMSCEVRef S, LLVMTypeRef Ty) {
  ScalarEvolution &Evolution = *unwrap(SE);
  const SCEV *Op = unwrap(S);
  Type *DestTy = unwrap(Ty);
  // the builders assert that the types are valid for the cast
  if (isa<SCEVCouldNotCompute>(Op) || !Op->getType()->isIntegerTy() ||
      !DestTy->isIntegerTy())
    return nullptr;
  unsigned SrcBits = Op->getType()->getIntegerBitWidth();
  unsigned DestBits = DestTy->getIntegerBitWidth();
  switch (Opcode) {
  case LLVMTrunc:
    return SrcBits > DestBits ? wrap(Evolution.getTruncateExpr(Op, DestTy)) : nullptr;
  case LLVMZExt:
    return SrcBits < DestBits ? wrap(Evolution.getZeroExtendExpr(Op, DestTy)) : nullptr;
  case LLVMSExt:
    return SrcBits < DestBits ? wrap(Evolution.getSignExtendExpr(Op, DestTy)) : nullptr;
  default:
    return nullptr;
  }
}

LLVMBool LLVMExtraScalarEvolutionIsLoopInvariant(LLVMScalarEvolutionRef SE, LLVMSCEVRef S,
                                                 LLVMLoopRef L) {
  return unwrap(SE)->isLoopInvariant(unwrap(S), unwrap(L));
}

LLVMSCEVRef LLVMExtraScalarEvolutionGetBackedgeTakenCount(LLVMScalarEvolutionRef SE,
                                                          LLVMLoopRef L,
                                                          LLVMExtraBackedgeTakenCountKind Kind) {
  ScalarEvolution::ExitCountKind CountKind;
  switch (Kind) {
  case LLVMExtraExactBackedgeTakenCount:
    CountKind = ScalarEvolution::Exact;
    break;
  case LLVMExtraConstantMaxBackedgeTakenCount:
    CountKind = ScalarEvolution::ConstantMaximum;
    break;
  case LLVMExtraSymbolicMaxBackedgeTakenCount:
    CountKind = ScalarEvolution::SymbolicMaximum;
    break;
  default:
    llvm_unreachable("unknown backedge-taken count kind");
  }
  return wrap(unwrap(SE)->getBackedgeTakenCount(unwrap(L), CountKind));
}

LLVMSCEVRef LLVMExtraSCEVAddRecGetStepRecurrence(LLVMScalarEvolutionRef SE, LLVMSCEVRef S) {
  if (auto *AR = dyn_cast<SCEVAddRecExpr>(unwrap(S)))
    return wrap(AR->getStepRecurrence(*unwrap(SE)));
  return nullptr;
}

unsigned LLVMExtraSCEVGetNoWrapFlags(LLVMSCEVRef S) {
  if (auto *NAry = dyn_cast<SCEVNAryExpr>(unwrap(S)))
    return static_cast<unsigned>(NAry->getNoWrapFlags());
  return 0;
}

unsigned LLVMExtraScalarEvolutionGetRange(LLVMScalarEvolutionRef SE, LLVMSCEVRef S,
                                          LLVMBool Signed, uint64_t *Lower,
                                          uint64_t *Upper) {
  // could-not-compute expressions have no type
  if (isa<SCEVCouldNotCompute>(unwrap(S)))
    return 0;
  if (!Lower || !Upper)
    return unwrap(SE)->getTypeSizeInBits(unwrap(S)->getType());
  const ConstantRange &CR =
      Signed ? unwrap(SE)->getSignedRange(unwrap(S)) : unwrap(SE)->getUnsignedRange(unwrap(S));
  writeRange(CR, Lower, Upper);
  return CR.getBitWidth();
}

LLVMExtraSCEVKind LLVMExtraSCEVGetKind(LLVMSCEVRef S) {
  switch (unwrap(S)->getSCEVType()) {
  case scConstant:
    return LLVMExtraSCEVConstantKind;
  case scTruncate:
    return LLVMExtraSCEVTruncateKind;
  case scZeroExtend:
    return LLVMExtraSCEVZeroExtendKind;
  case scSignExtend:
    return LLVMExtraSCEVSignExtendKind;
  case scAddExpr:
    return LLVMExtraSCEVAddKind;
  case scMulExpr:
    return LLVMExtraSCEVMulKind;
  case scUDivExpr:
    return LLVMExtraSCEVUDivKind;
  case scAddRecExpr:
    return LLVMExtraSCEVAddRecKind;
  case scUMaxExpr:
    return LLVMExtraSCEVUMaxKind;
  case scSMaxExpr:
    return LLVMExtraSCEVSMaxKind;
  case scUMinExpr:
    return LLVMExtraSCEVUMinKind;
  case scSMinExpr:
    return LLVMExtraSCEVSMinKind;
  case scSequentialUMinExpr:
    return LLVMExtraSCEVSequentialUMinKind;
  case scUnknown:
    return LLVMExtraSCEVUnknownKind;
  case scCouldNotCompute:
    return LLVMExtraSCEVCouldNotComputeKind;
#if LLVM_VERSION_MAJOR >= 17
  case scVScale:
    return LLVMExtraSCEVVScaleKind;
#endif
  case scPtrToInt:
    return LLVMExtraSCEVPtrToIntKind;
  default:
    return LLVMExtraSCEVOtherKind;
  }
}

LLVMTypeRef LLVMExtraSCEVGetType(LLVMSCEVRef S) {
  if (isa<SCEVCouldNotCompute>(unwrap(S)))
    return nullptr;
  return wrap(unwrap(S)->getType());
}

// the operands of an expression (SCEV::operands() only exists since LLVM 16)
static SmallVector<const SCEV *, 4> getOperands(const SCEV *S) {
  SmallVector<const SCEV *, 4> Ops;
  if (auto *Cast = dyn_cast<SCEVCastExpr>(S))
    Ops.append(Cast->operands().begin(), Cast->operands().end());
  else if (auto *NAry = dyn_cast<SCEVNAryExpr>(S))
    Ops.append(NAry->operands().begin(), NAry->operands().end());
  else if (auto *UDiv = dyn_cast<SCEVUDivExpr>(S))
    Ops.append({UDiv->getLHS(), UDiv->getRHS()});
  return Ops;
}

unsigned LLVMExtraSCEVGetOperands(LLVMSCEVRef S, LLVMSCEVRef *Ops) {
  auto Operands = getOperands(unwrap(S));
  if (Ops)
    for (unsigned I = 0; I < Operands.size(); ++I)
      Ops[I] = wrap(Operands[I]);
  return Operands.size();
}

LLVMValueRef LLVMExtraSCEVGetValue(LLVMSCEVRef S) {
  if (auto *C = dyn_cast<SCEVConstant>(unwrap(S)))
    return wrap(C->getValue());
  if (auto *U = dyn_cast<SCEVUnknown>(unwrap(S)))
    return wrap(U->getValue());
  return nullptr;
}

LLVMLoopRef LLVMExtraSCEVAddRecGetLoop(LLVMSCEVRef S) {
  if (auto *AR = dyn_cast<SCEVAddRecExpr>(unwrap(S)))
    return wrap(const_cast<Loop *>(AR->getLoop()));
  return nullptr;
}

LLVMBool LLVMExtraSCEVContains(LLVMSCEVRef S, LLVMExtraSCEVKind Kind) {
  return SCEVExprContains(unwrap(S), [Kind](const SCEV *X) {
    return LLVMExtraSCEVGetKind(wrap(X)) == Kind;
  });
}

char *LLVMExtraPrintSCEVToString(LLVMSCEVRef S) {
  std::string Buf;
  raw_string_ostream OS(Buf);
  unwrap(S)->print(OS);
  OS.flush();
  return strdup(Buf.c_str());
}


// LoopInfo

LLVMLoopRef LLVMExtraLoopInfoGetLoopFor(LLVMLoopInfoRef LI, LLVMBasicBlockRef BB) {
  return wrap(unwrap(LI)->getLoopFor(unwrap(BB)));
}

LLVMBasicBlockRef LLVMExtraLoopGetHeader(LLVMLoopRef L) { return wrap(unwrap(L)->getHeader()); }

LLVMLoopRef LLVMExtraLoopGetParent(LLVMLoopRef L) { return wrap(unwrap(L)->getParentLoop()); }

unsigned LLVMExtraLoopGetDepth(LLVMLoopRef L) { return unwrap(L)->getLoopDepth(); }

LLVMBool LLVMExtraLoopContains(LLVMLoopRef L, LLVMBasicBlockRef BB) {
  return unwrap(L)->contains(unwrap(BB));
}

// copy a list of objects to an array of their references, if it is not NULL
template <typename Ref, typename Range> static unsigned writeRefs(const Range &R, Ref *Out) {
  unsigned N = 0;
  for (auto *X : R) {
    if (Out)
      Out[N] = wrap(X);
    N++;
  }
  return N;
}

unsigned LLVMExtraLoopGetNumBlocks(LLVMLoopRef L) { return unwrap(L)->getNumBlocks(); }

LLVMBasicBlockRef LLVMExtraLoopGetBlock(LLVMLoopRef L, unsigned Index) {
  return wrap(unwrap(L)->getBlocks()[Index]);
}

unsigned LLVMExtraLoopGetNumSubLoops(LLVMLoopRef L) { return unwrap(L)->getSubLoops().size(); }

LLVMLoopRef LLVMExtraLoopGetSubLoop(LLVMLoopRef L, unsigned Index) {
  return wrap(unwrap(L)->getSubLoops()[Index]);
}

unsigned LLVMExtraLoopGetUniqueExitBlocks(LLVMLoopRef L, LLVMBasicBlockRef *Blocks) {
  SmallVector<BasicBlock *, 4> Exits;
  unwrap(L)->getUniqueExitBlocks(Exits);
  return writeRefs(Exits, Blocks);
}

unsigned LLVMExtraLoopInfoGetTopLevelLoops(LLVMLoopInfoRef LI, LLVMLoopRef *Loops) {
  return writeRefs(unwrap(LI)->getTopLevelLoops(), Loops);
}

LLVMBasicBlockRef LLVMExtraLoopGetLatch(LLVMLoopRef L) {
  return wrap(unwrap(L)->getLoopLatch());
}

LLVMBasicBlockRef LLVMExtraLoopGetPreheader(LLVMLoopRef L) {
  return wrap(unwrap(L)->getLoopPreheader());
}

LLVMBool LLVMExtraLoopIsLoopInvariant(LLVMLoopRef L, LLVMValueRef V) {
  return unwrap(L)->isLoopInvariant(unwrap(V));
}


// Dominance

LLVMBool LLVMExtraDominatorTreeInstructionDominatesUse(LLVMDominatorTreeRef Tree,
                                                       LLVMValueRef Inst, LLVMUseRef U) {
  return unwrap(Tree)->dominates(unwrap<Instruction>(Inst), *unwrap(U));
}

LLVMBool LLVMExtraDominatorTreeBlockDominates(LLVMDominatorTreeRef Tree, LLVMBasicBlockRef A,
                                              LLVMBasicBlockRef B) {
  return unwrap(Tree)->dominates(unwrap(A), unwrap(B));
}

void LLVMExtraDominatorTreeRecalculate(LLVMDominatorTreeRef Tree, LLVMValueRef F) {
  // the root of the tree can't be used to find the function, as it may have been deleted
  unwrap(Tree)->recalculate(*unwrap<Function>(F));
}

LLVMValueRef LLVMExtraSplitBlockAndInsertIfThen(LLVMValueRef Cond, LLVMValueRef SplitBefore,
                                                LLVMBool Unreachable,
                                                LLVMDominatorTreeRef Tree) {
  Instruction *I = unwrap<Instruction>(SplitBefore);
  std::optional<DomTreeUpdater> DTU;
  if (Tree) {
    // the root of a tree is the entry block of its function (compared without dereferencing
    // it, as the tree may be stale), and LLVM asserts that blocks are of that function
    if (unwrap(Tree)->getRoot() != &I->getFunction()->getEntryBlock())
      return nullptr;
    DTU.emplace(*unwrap(Tree), DomTreeUpdater::UpdateStrategy::Eager);
  }
  // the overload taking an instruction and a DomTreeUpdater exists in every version
  DomTreeUpdater *Updater = DTU ? &*DTU : nullptr;
  return wrap(SplitBlockAndInsertIfThen(unwrap(Cond), I, Unreachable,
                                        /*BranchWeights=*/nullptr, Updater));
}


// Dead code

LLVMBool LLVMExtraIsInstructionTriviallyDead(LLVMValueRef Inst) {
  return isInstructionTriviallyDead(unwrap<Instruction>(Inst));
}

LLVMBool LLVMExtraRecursivelyDeleteTriviallyDeadInstructions(LLVMValueRef Inst) {
  return RecursivelyDeleteTriviallyDeadInstructions(unwrap(Inst));
}

LLVMBool LLVMExtraRecursivelyDeleteTriviallyDeadInstructionsPermissive(LLVMValueRef *Insts,
                                                                       unsigned NumInsts) {
  // the handles become null when their instruction is deleted, which happens when an
  // instruction is also the operand of another one that is deleted first
  SmallVector<WeakTrackingVH, 16> DeadInsts;
  for (unsigned I = 0; I < NumInsts; ++I)
    DeadInsts.emplace_back(unwrap(Insts[I]));
  return RecursivelyDeleteTriviallyDeadInstructionsPermissive(DeadInsts);
}

LLVMBool LLVMExtraIsAllocaPromotable(LLVMValueRef Alloca) {
  return isAllocaPromotable(unwrap<AllocaInst>(Alloca));
}

LLVMBool LLVMExtraPromoteMemToReg(LLVMValueRef *Allocas, unsigned NumAllocas,
                                  LLVMDominatorTreeRef Tree, LLVMAssumptionCacheRef AC) {
  if (NumAllocas == 0)
    return true;
  SmallVector<AllocaInst *, 4> List;
  for (unsigned I = 0; I < NumAllocas; ++I)
    List.push_back(unwrap<AllocaInst>(Allocas[I]));
  if (unwrap(Tree)->getRoot() != &List.front()->getFunction()->getEntryBlock())
    return false;
  PromoteMemToReg(List, *unwrap(Tree), unwrap(AC));
  return true;
}

LLVMBool LLVMExtraConstantFoldCastOperand(LLVMOpcode Opcode, LLVMValueRef C,
                                          LLVMTypeRef DestTy, LLVMTargetDataRef TD,
                                          LLVMValueRef *Result) {
  Instruction::CastOps Op;
  switch (Opcode) {
#define CAST(Name)                                                                         \
  case LLVM##Name:                                                                         \
    Op = Instruction::Name;                                                                \
    break;
    CAST(Trunc) CAST(ZExt) CAST(SExt) CAST(FPToUI) CAST(FPToSI) CAST(UIToFP) CAST(SIToFP)
    CAST(FPTrunc) CAST(FPExt) CAST(PtrToInt) CAST(IntToPtr) CAST(BitCast) CAST(AddrSpaceCast)
#undef CAST
  default:
    return false;
  }
  Constant *Const = unwrap<Constant>(C);
  Type *Ty = unwrap(DestTy);
  // the constant folder asserts that the cast is valid
  if (!CastInst::castIsValid(Op, Const, Ty))
    return false;
  *Result = wrap(ConstantFoldCastOperand(Op, Const, Ty, *unwrap(TD)));
  return true;
}

void LLVMExtraScalarEvolutionForgetValue(LLVMScalarEvolutionRef SE, LLVMValueRef V) {
  unwrap(SE)->forgetValue(unwrap(V));
}

void LLVMExtraScalarEvolutionForgetLoop(LLVMScalarEvolutionRef SE, LLVMLoopRef L) {
  unwrap(SE)->forgetLoop(unwrap(L));
}

void LLVMExtraScalarEvolutionForgetLoopDispositions(LLVMScalarEvolutionRef SE) {
#if LLVM_VERSION_MAJOR >= 16
  unwrap(SE)->forgetLoopDispositions();
#else
  // the loop argument was unused, as dispositions are always forgotten for all loops
  unwrap(SE)->forgetLoopDispositions(nullptr);
#endif
}

LLVMBool LLVMExtraIsSafeToSpeculativelyExecute(LLVMValueRef Inst, LLVMValueRef CxtI,
                                               LLVMAssumptionCacheRef AC,
                                               LLVMDominatorTreeRef DT) {
  Instruction *I = unwrap<Instruction>(Inst);
  Instruction *Ctx = CxtI ? unwrap<Instruction>(CxtI) : nullptr;
#if LLVM_VERSION_MAJOR >= 16
  return isSafeToSpeculativelyExecute(I, Ctx, unwrap(AC), unwrap(DT));
#else
  // the assumption cache is only used since LLVM 16
  return isSafeToSpeculativelyExecute(I, Ctx, unwrap(DT));
#endif
}

void LLVMExtraMemorySSAVerify(LLVMMemorySSARef MSSA) { unwrap(MSSA)->verifyMemorySSA(); }

LLVMMemoryAccessRef LLVMExtraMemorySSAGetInstructionAccess(LLVMMemorySSARef MSSA,
                                                           LLVMValueRef Inst) {
  return wrap(unwrap(MSSA)->getMemoryAccess(unwrap<Instruction>(Inst)));
}

LLVMMemoryAccessRef LLVMExtraMemorySSAGetBlockPhi(LLVMMemorySSARef MSSA,
                                                  LLVMBasicBlockRef BB) {
  return wrap(unwrap(MSSA)->getMemoryAccess(unwrap(BB)));
}

LLVMMemoryAccessRef LLVMExtraMemorySSAGetLiveOnEntryDef(LLVMMemorySSARef MSSA) {
  return wrap(unwrap(MSSA)->getLiveOnEntryDef());
}

unsigned LLVMExtraMemorySSAGetBlockAccesses(LLVMMemorySSARef MSSA, LLVMBasicBlockRef BB,
                                            LLVMMemoryAccessRef *Accesses) {
  auto *List = unwrap(MSSA)->getBlockAccesses(unwrap(BB));
  if (!List)
    return 0;
  unsigned N = 0;
  for (const MemoryAccess &MA : *List) {
    if (Accesses)
      Accesses[N] = wrap(const_cast<MemoryAccess *>(&MA));
    N++;
  }
  return N;
}

LLVMExtraMemoryAccessKind LLVMExtraMemoryAccessGetKind(LLVMMemoryAccessRef MA) {
  MemoryAccess *Access = unwrap(MA);
  if (isa<MemoryUse>(Access))
    return LLVMExtraMemoryUse;
  if (isa<MemoryDef>(Access))
    return LLVMExtraMemoryDef;
  return LLVMExtraMemoryPhi;
}

LLVMBasicBlockRef LLVMExtraMemoryAccessGetBlock(LLVMMemoryAccessRef MA) {
  return wrap(unwrap(MA)->getBlock());
}

LLVMValueRef LLVMExtraMemoryUseOrDefGetInstruction(LLVMMemoryAccessRef MA) {
  return wrap(cast<MemoryUseOrDef>(unwrap(MA))->getMemoryInst());
}

LLVMMemoryAccessRef LLVMExtraMemoryUseOrDefGetDefiningAccess(LLVMMemoryAccessRef MA) {
  return wrap(cast<MemoryUseOrDef>(unwrap(MA))->getDefiningAccess());
}

unsigned LLVMExtraMemoryPhiGetNumIncoming(LLVMMemoryAccessRef MA) {
  return cast<MemoryPhi>(unwrap(MA))->getNumIncomingValues();
}

LLVMMemoryAccessRef LLVMExtraMemoryPhiGetIncomingAccess(LLVMMemoryAccessRef MA,
                                                       unsigned Index) {
  return wrap(cast<MemoryPhi>(unwrap(MA))->getIncomingValue(Index));
}

LLVMBasicBlockRef LLVMExtraMemoryPhiGetIncomingBlock(LLVMMemoryAccessRef MA, unsigned Index) {
  return wrap(cast<MemoryPhi>(unwrap(MA))->getIncomingBlock(Index));
}

char *LLVMExtraPrintMemoryAccessToString(LLVMMemoryAccessRef MA) {
  std::string Buf;
  raw_string_ostream OS(Buf);
  unwrap(MA)->print(OS);
  OS.flush();
  return strdup(Buf.c_str());
}

LLVMMemorySSAUpdaterRef LLVMExtraCreateMemorySSAUpdater(LLVMMemorySSARef MSSA) {
  return wrap(new MemorySSAUpdater(unwrap(MSSA)));
}

void LLVMExtraDisposeMemorySSAUpdater(LLVMMemorySSAUpdaterRef U) { delete unwrap(U); }

static MemorySSA::InsertionPlace unwrap(LLVMExtraMemorySSAInsertionPlace Place) {
  switch (Place) {
  case LLVMExtraMemorySSABeginning:
    return MemorySSA::Beginning;
  case LLVMExtraMemorySSAEnd:
    return MemorySSA::End;
  case LLVMExtraMemorySSABeforeTerminator:
    return MemorySSA::BeforeTerminator;
  }
  llvm_unreachable("unknown MemorySSA insertion place");
}

void LLVMExtraMemorySSAUpdaterRemoveMemoryAccess(LLVMMemorySSAUpdaterRef U,
                                                 LLVMMemoryAccessRef MA,
                                                 LLVMBool OptimizePhis) {
  unwrap(U)->removeMemoryAccess(unwrap(MA), OptimizePhis);
}

void LLVMExtraMemorySSAUpdaterMoveToPlace(LLVMMemorySSAUpdaterRef U, LLVMMemoryAccessRef What,
                                          LLVMBasicBlockRef BB,
                                          LLVMExtraMemorySSAInsertionPlace Place) {
  unwrap(U)->moveToPlace(cast<MemoryUseOrDef>(unwrap(What)), unwrap(BB), unwrap(Place));
}

void LLVMExtraMemorySSAUpdaterMoveBefore(LLVMMemorySSAUpdaterRef U, LLVMMemoryAccessRef What,
                                         LLVMMemoryAccessRef Where) {
  unwrap(U)->moveBefore(cast<MemoryUseOrDef>(unwrap(What)),
                        cast<MemoryUseOrDef>(unwrap(Where)));
}

void LLVMExtraMemorySSAUpdaterMoveAfter(LLVMMemorySSAUpdaterRef U, LLVMMemoryAccessRef What,
                                        LLVMMemoryAccessRef Where) {
  unwrap(U)->moveAfter(cast<MemoryUseOrDef>(unwrap(What)),
                       cast<MemoryUseOrDef>(unwrap(Where)));
}

// whether MemorySSA may model an instruction as accessing memory, i.e., whether creating
// an access for it can succeed: `MemorySSA::createNewAccess` also skips instructions that
// alias analysis finds not to access memory, which this does not check. Creating an access
// that is not created fails an assertion, or crashes, except in `createMemoryAccessInBB`
// since LLVM 19.
static bool mayHaveMemoryAccess(Instruction *I) {
  if (auto *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default:
      break;
#if LLVM_VERSION_MAJOR >= 19
    case Intrinsic::allow_runtime_check:
    case Intrinsic::allow_ubsan_check:
#endif
    case Intrinsic::assume:
    case Intrinsic::experimental_noalias_scope_decl:
    case Intrinsic::pseudoprobe:
      return false;
    }
  }
  return I->mayReadFromMemory() || I->mayWriteToMemory();
}

LLVMMemoryAccessRef LLVMExtraMemorySSAUpdaterCreateMemoryAccessInBB(
    LLVMMemorySSAUpdaterRef U, LLVMValueRef Inst, LLVMMemoryAccessRef Definition,
    LLVMBasicBlockRef BB, LLVMExtraMemorySSAInsertionPlace Place) {
  Instruction *I = unwrap<Instruction>(Inst);
  if (!mayHaveMemoryAccess(I))
    return nullptr;
#if LLVM_VERSION_MAJOR >= 19
  return wrap(unwrap(U)->createMemoryAccessInBB(I, unwrap(Definition), unwrap(BB),
                                                unwrap(Place),
                                                /*CreationMustSucceed=*/false));
#else
  return wrap(unwrap(U)->createMemoryAccessInBB(I, unwrap(Definition), unwrap(BB),
                                                unwrap(Place)));
#endif
}

LLVMMemoryAccessRef LLVMExtraMemorySSAUpdaterCreateMemoryAccessBefore(
    LLVMMemorySSAUpdaterRef U, LLVMValueRef Inst, LLVMMemoryAccessRef Definition,
    LLVMMemoryAccessRef InsertPt) {
  Instruction *I = unwrap<Instruction>(Inst);
  if (!mayHaveMemoryAccess(I))
    return nullptr;
  return wrap(unwrap(U)->createMemoryAccessBefore(I, unwrap(Definition),
                                                  cast<MemoryUseOrDef>(unwrap(InsertPt))));
}

LLVMMemoryAccessRef LLVMExtraMemorySSAUpdaterCreateMemoryAccessAfter(
    LLVMMemorySSAUpdaterRef U, LLVMValueRef Inst, LLVMMemoryAccessRef Definition,
    LLVMMemoryAccessRef InsertPt) {
  Instruction *I = unwrap<Instruction>(Inst);
  if (!mayHaveMemoryAccess(I))
    return nullptr;
  return wrap(unwrap(U)->createMemoryAccessAfter(I, unwrap(Definition), unwrap(InsertPt)));
}

void LLVMExtraMemorySSAUpdaterInsertDef(LLVMMemorySSAUpdaterRef U, LLVMMemoryAccessRef Def,
                                        LLVMBool RenameUses) {
  unwrap(U)->insertDef(cast<MemoryDef>(unwrap(Def)), RenameUses);
}

void LLVMExtraMemorySSAUpdaterInsertUse(LLVMMemorySSAUpdaterRef U, LLVMMemoryAccessRef Use,
                                        LLVMBool RenameUses) {
  unwrap(U)->insertUse(cast<MemoryUse>(unwrap(Use)), RenameUses);
}

unsigned LLVMExtraLoopGetBlocksRPO(LLVMLoopRef L, LLVMLoopInfoRef LI,
                                   LLVMBasicBlockRef *Blocks) {
  LoopBlocksRPO RPO(unwrap(L));
  RPO.perform(unwrap(LI));
  return writeRefs(RPO, Blocks);
}

LLVMValueRef LLVMExtraSimplifyInstruction(LLVMValueRef Inst) {
  Instruction *I = unwrap<Instruction>(Inst);
  return wrap(simplifyInstruction(I, SimplifyQuery(I->getModule()->getDataLayout(), I)));
}


// Control flow

LLVMBasicBlockRef LLVMExtraSplitBasicBlock(LLVMBasicBlockRef BB, LLVMValueRef Before,
                                           LLVMBool Head, const char *Name) {
  BasicBlock::iterator It = unwrap<Instruction>(Before)->getIterator();
#if LLVM_VERSION_MAJOR >= 19
  It.setHeadBit(Head);
#endif
  return wrap(unwrap(BB)->splitBasicBlock(It, Name));
}


// Inlining

LLVMBool LLVMExtraInlineFunction(LLVMValueRef Call, LLVMBool MergeAttributes,
                                 LLVMBool InsertLifetime, char **FailureReason) {
  CallBase *CB = unwrap<CallBase>(Call);
  Function *Caller = CB->getCaller();
  InlineFunctionInfo IFI;
#if LLVM_VERSION_MAJOR < 22
  // before checking the personality functions, InlineFunction gives a caller without a GC
  // strategy the one of the callee, which it doesn't undo when it then fails. it changes
  // nothing else before failing (it only sets the personality function of a caller that
  // has none, after which it can't fail anymore), so undo that to leave the IR unchanged.
  bool CallerHadGC = Caller->hasGC();
#endif
#if LLVM_VERSION_MAJOR >= 16
  InlineResult Result =
      InlineFunction(*CB, IFI, MergeAttributes, /*CalleeAAR=*/nullptr, InsertLifetime);
#else
  // InlineFunction doesn't merge the attributes yet, which its callers did instead
  Function *Callee = CB->getCalledFunction();
  InlineResult Result = InlineFunction(*CB, IFI, /*CalleeAAR=*/nullptr, InsertLifetime);
  if (Result.isSuccess() && MergeAttributes)
    AttributeFuncs::mergeAttributesForInlining(*Caller, *Callee);
#endif
#if LLVM_VERSION_MAJOR < 22
  if (!Result.isSuccess() && !CallerHadGC)
    Caller->clearGC();
#endif
  if (FailureReason)
    *FailureReason = Result.isSuccess() ? nullptr : strdup(Result.getFailureReason());
  return Result.isSuccess();
}

LLVMBool LLVMExtraIsInlineViable(LLVMValueRef Fn, char **FailureReason) {
  InlineResult Result = isInlineViable(*unwrap<Function>(Fn));
  if (FailureReason)
    *FailureReason = Result.isSuccess() ? nullptr : strdup(Result.getFailureReason());
  return Result.isSuccess();
}
