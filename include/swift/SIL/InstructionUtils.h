//===--- InstructionUtils.h - Utilities for SIL instructions ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_INSTRUCTIONUTILS_H
#define SWIFT_SIL_INSTRUCTIONUTILS_H

#include "swift/SIL/RuntimeEffect.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                         SSA Use-Def Helpers
//===----------------------------------------------------------------------===//

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

SILValue stripSinglePredecessorArgs(SILValue V);

/// Return the underlying SILValue after stripping off all casts from the
/// current SILValue.
SILValue stripCasts(SILValue V);

/// Return the underlying SILValue after stripping off all casts (but
/// mark_dependence) from the current SILValue.
SILValue stripCastsWithoutMarkDependence(SILValue V);

/// Return the underlying SILValue after looking through all copy_value and
/// begin_borrow instructions.
SILValue lookThroughOwnershipInsts(SILValue v);

/// Reverse of lookThroughOwnershipInsts.
///
/// Return true if \p visitor returned true for all uses.
bool visitNonOwnershipUses(SILValue value,
                           function_ref<bool(Operand *)> visitor);

/// Return the underlying SILValue after looking through all copy_value
/// instructions.
SILValue lookThroughCopyValueInsts(SILValue v);

/// Return the underlying SILValue after stripping off all upcasts from the
/// current SILValue.
SILValue stripUpCasts(SILValue V);

/// Return the underlying SILValue after stripping off all
/// upcasts and downcasts.
SILValue stripClassCasts(SILValue V);

/// Return the underlying SILValue after stripping off all address projection
/// instructions.
///
/// FIXME: Today address projections are referring to the result of the
/// projection and doesn't consider the operand. Should we change this?
SILValue stripAddressProjections(SILValue V);

/// Look through any projections that transform an address -> an address.
SILValue lookThroughAddressToAddressProjections(SILValue v);

/// Return the underlying SILValue after stripping off all aggregate projection
/// instructions.
///
/// An aggregate projection instruction is either a struct_extract or a
/// tuple_extract instruction.
SILValue stripValueProjections(SILValue V);

/// Return the underlying SILValue after stripping off all indexing
/// instructions.
///
/// An indexing inst is either index_addr or index_raw_pointer.
SILValue stripIndexingInsts(SILValue V);

/// Returns the underlying value after stripping off a builtin expect
/// intrinsic call.
SILValue stripExpectIntrinsic(SILValue V);

/// If V is a begin_borrow, strip off the begin_borrow and return. Otherwise,
/// ust return V.
SILValue stripBorrow(SILValue V);

//===----------------------------------------------------------------------===//
//                         Instruction Properties
//===----------------------------------------------------------------------===//

/// Return a non-null SingleValueInstruction if the given instruction merely
/// copies or moves the value of its first operand, possibly changing its type
/// or ownership state, but otherwise having no effect.
///
/// The returned instruction may have additional "incidental" operands;
/// mark_dependence for example.
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping". These are
/// instructions that the use-visitor can recurse into. Note that the value's
/// type may be changed by a cast.
SingleValueInstruction *getSingleValueCopyOrCast(SILInstruction *I);

/// Return true if this instruction terminates a SIL-level scope. Scope end
/// instructions do not produce a result.
bool isEndOfScopeMarker(SILInstruction *user);

/// Return true if the given instruction has no effect on it's operand values
/// and produces no result. These are typically end-of scope markers.
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping".
bool isIncidentalUse(SILInstruction *user);

/// Return true if the given `user` instruction modifies the value's refcount
/// without propagating the value or having any other effect aside from
/// potentially destroying the value itself (and executing associated cleanups).
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping".
bool onlyAffectsRefCount(SILInstruction *user);

/// Returns true if the given user instruction checks the ref count of a
/// pointer.
bool mayCheckRefCount(SILInstruction *User);

/// Return true when the instruction represents added instrumentation for
/// run-time sanitizers.
bool isSanitizerInstrumentation(SILInstruction *Instruction);

/// Return true when the instruction represents added instrumentation for
/// run-time sanitizers or code coverage.
bool isInstrumentation(SILInstruction *Instruction);

/// Check that this is a partial apply of a reabstraction thunk and return the
/// argument of the partial apply if it is.
SILValue isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI);

/// Returns true if \p PAI is only used by an assign_by_wrapper instruction as
/// init or set function.
bool onlyUsedByAssignByWrapper(PartialApplyInst *PAI);

/// Returns true if \p PAI is only used by an \c assign_or_init
/// instruction as init or set function.
bool onlyUsedByAssignOrInit(PartialApplyInst *PAI);

/// Returns the runtime effects of \p inst.
///
/// Predicts which runtime calls are called in the generated code for `inst`.
/// This is sometimes a conservative approximation, i.e. more runtime effects
/// are reported than actually happen.
/// If the runtime effects can be associated with a type, this type is returned
/// in `impactType`. That's useful for diagnostics.
RuntimeEffect getRuntimeEffect(SILInstruction *inst, SILType &impactType);

/// If V is a function closure, return the reaching set of partial_apply's.
void findClosuresForFunctionValue(SILValue V,
                                  TinyPtrVector<PartialApplyInst *> &results);

/// An abstraction over LoadInst/LoadBorrowInst so one can handle both types of
/// load using common code.
struct LoadOperation {
  llvm::PointerUnion<LoadInst *, LoadBorrowInst *> value;

  LoadOperation() : value() {}
  LoadOperation(SILInstruction *input) : value(nullptr) {
    if (auto *li = dyn_cast<LoadInst>(input)) {
      value = li;
      return;
    }

    if (auto *lbi = dyn_cast<LoadBorrowInst>(input)) {
      value = lbi;
      return;
    }
  }

  explicit operator bool() const { return !value.isNull(); }

  SingleValueInstruction *getLoadInst() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>();
    return value.get<LoadBorrowInst *>();
  }

  SingleValueInstruction *operator*() const { return getLoadInst(); }

  const SingleValueInstruction *operator->() const { return getLoadInst(); }

  SingleValueInstruction *operator->() { return getLoadInst(); }

  SILValue getOperand() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>()->getOperand();
    return value.get<LoadBorrowInst *>()->getOperand();
  }

  /// Return the ownership qualifier of the underlying load if we have a load or
  /// None if we have a load_borrow.
  ///
  /// TODO: Rather than use an optional here, we should include an invalid
  /// representation in LoadOwnershipQualifier.
  Optional<LoadOwnershipQualifier> getOwnershipQualifier() const {
    if (auto *lbi = value.dyn_cast<LoadBorrowInst *>()) {
      return None;
    }

    return value.get<LoadInst *>()->getOwnershipQualifier();
  }
};

/// Given a polymorphic builtin \p bi that may be generic and thus have in/out
/// params, stash all of the information needed for either specializing while
/// inlining or propagating the type in constant propagation.
///
/// NOTE: If we perform this transformation, our builtin will no longer have any
/// substitutions since we only substitute to concrete static overloads.
struct PolymorphicBuiltinSpecializedOverloadInfo {
  const BuiltinInfo *builtinInfo;
  Identifier staticOverloadIdentifier;
  SmallVector<SILType, 8> argTypes;
  SILType resultType;
  bool hasOutParam;

private:
  bool isInitialized;

public:
  PolymorphicBuiltinSpecializedOverloadInfo()
      : builtinInfo(nullptr), staticOverloadIdentifier(), argTypes(),
        resultType(), hasOutParam(false), isInitialized(false) {}

  /// Returns true if we were able to map the polymorphic builtin to a static
  /// overload. False otherwise.
  ///
  /// NOTE: This does not mean that the static overload actually exists.
  bool init(BuiltinInst *bi);

  bool doesOverloadExist() const {
    CanBuiltinType builtinType = argTypes.front().getAs<BuiltinType>();
    return canBuiltinBeOverloadedForType(builtinInfo->ID, builtinType);
  }

private:
  bool init(SILFunction *fn, BuiltinValueKind builtinKind,
            ArrayRef<SILType> oldOperandTypes, SILType oldResultType);
};

/// Given a polymorphic builtin \p bi, analyze its types and create a builtin
/// for the static overload that the builtin corresponds to. If \p bi is not a
/// polymorphic builtin or does not have any available overload for these types,
/// return SILValue().
SILValue getStaticOverloadForSpecializedPolymorphicBuiltin(BuiltinInst *bi);

/// An ADT for writing generic code against conversion instructions.
struct ConversionOperation {
  SingleValueInstruction *inst = nullptr;

  ConversionOperation() = default;

  explicit ConversionOperation(SILInstruction *inst) {
    auto *svi = dyn_cast<SingleValueInstruction>(inst);
    if (!svi) {
      return;
    }
    if (!ConversionOperation::isa(svi)) {
      return;
    }
    this->inst = svi;
  }

  explicit ConversionOperation(SILValue value) {
    auto *inst = value->getDefiningInstruction();
    if (!inst) {
      return;
    }
    auto *svi = dyn_cast<SingleValueInstruction>(inst);
    if (!svi) {
      return;
    }
    if (!ConversionOperation::isa(svi)) {
      return;
    }
    this->inst = svi;
  }

  operator bool() const { return inst != nullptr; }

  SingleValueInstruction *operator->() { return inst; }
  SingleValueInstruction *operator->() const { return inst; }
  SingleValueInstruction *operator*() { return inst; }
  SingleValueInstruction *operator*() const { return inst; }

  static bool isa(SILInstruction *inst) {
    switch (inst->getKind()) {
    case SILInstructionKind::ConvertFunctionInst:
    case SILInstructionKind::UpcastInst:
    case SILInstructionKind::AddressToPointerInst:
    case SILInstructionKind::UncheckedTrivialBitCastInst:
    case SILInstructionKind::UncheckedAddrCastInst:
    case SILInstructionKind::UncheckedBitwiseCastInst:
    case SILInstructionKind::RefToRawPointerInst:
    case SILInstructionKind::RawPointerToRefInst:
    case SILInstructionKind::ConvertEscapeToNoEscapeInst:
    case SILInstructionKind::RefToBridgeObjectInst:
    case SILInstructionKind::BridgeObjectToRefInst:
    case SILInstructionKind::BridgeObjectToWordInst:
    case SILInstructionKind::ThinToThickFunctionInst:
    case SILInstructionKind::ThickToObjCMetatypeInst:
    case SILInstructionKind::ObjCToThickMetatypeInst:
    case SILInstructionKind::ObjCMetatypeToObjectInst:
    case SILInstructionKind::ObjCExistentialMetatypeToObjectInst:
    case SILInstructionKind::UnconditionalCheckedCastInst:
    case SILInstructionKind::UncheckedRefCastInst:
    case SILInstructionKind::UncheckedValueCastInst:
    case SILInstructionKind::RefToUnmanagedInst:
    case SILInstructionKind::RefToUnownedInst:
    case SILInstructionKind::UnmanagedToRefInst:
    case SILInstructionKind::UnownedToRefInst:
      return true;
    default:
      return false;
    }
  }

  SILValue getConverted() { return inst->getOperand(0); }
};

/// An ADT for writing generic code against SelectEnumAddrInst and
/// SelectEnumInst.
///
/// We use this instead of SelectEnumInstBase for this purpose in order to avoid
/// the need for templating SelectEnumInstBase from causing this ADT type of
/// usage to require templates.
class SelectEnumOperation {
  PointerUnion<SelectEnumAddrInst *, SelectEnumInst *> value;

public:
  SelectEnumOperation(SelectEnumAddrInst *seai) : value(seai) {}
  SelectEnumOperation(SelectEnumInst *seai) : value(seai) {}
  SelectEnumOperation(SILInstruction *i) : value(nullptr) {
    if (auto *seai = dyn_cast<SelectEnumAddrInst>(i)) {
      value = seai;
      return;
    }

    if (auto *sei = dyn_cast<SelectEnumInst>(i)) {
      value = sei;
      return;
    }
  }

  SelectEnumOperation(const SILInstruction *i)
      : SelectEnumOperation(const_cast<SILInstruction *>(i)) {}

  operator SingleValueInstruction *() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  SingleValueInstruction *operator*() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  SingleValueInstruction *operator->() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  operator bool() const { return bool(value); }

  SILValue getOperand() {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getOperand();
    return value.get<SelectEnumAddrInst *>()->getOperand();
  }

  SILValue getEnumOperand() { return getOperand(); }

  const Operand &getEnumOperandRef() {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getEnumOperandRef();
    return value.get<SelectEnumAddrInst *>()->getEnumOperandRef();
  }

  unsigned getNumCases() {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getNumCases();
    return value.get<SelectEnumAddrInst *>()->getNumCases();
  }

  std::pair<EnumElementDecl *, SILValue> getCase(unsigned i) const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getCase(i);
    return value.get<SelectEnumAddrInst *>()->getCase(i);
  }
  /// Return the value that will be used as the result for the specified enum
  /// case.
  SILValue getCaseResult(EnumElementDecl *D) {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getCaseResult(D);
    return value.get<SelectEnumAddrInst *>()->getCaseResult(D);
  }

  /// If the default refers to exactly one case decl, return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDefault();

  bool hasDefault() const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->hasDefault();
    return value.get<SelectEnumAddrInst *>()->hasDefault();
  }

  SILValue getDefaultResult() const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getDefaultResult();
    return value.get<SelectEnumAddrInst *>()->getDefaultResult();
  }
};

} // end namespace swift

#endif
