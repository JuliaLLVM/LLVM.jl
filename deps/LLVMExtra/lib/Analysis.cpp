#include "LLVMExtra.h"

#include <llvm/ADT/APInt.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/ConstantRange.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Operator.h>
#include <llvm/Support/KnownBits.h>

using namespace llvm;

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
