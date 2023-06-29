//===--- ForwardingUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_FORWARDINGUTILS_H
#define SWIFT_SIL_FORWARDINGUTILS_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"

namespace swift {
class SILBasicBlock;
class SILInstruction;
class SILModule;
class SILValue;

class ForwardingOperation {
  SILInstruction *forwardingInst = nullptr;

public:
  explicit ForwardingOperation(SILInstruction *inst);

  operator bool() const { return bool(forwardingInst); }
  const SILInstruction *operator->() const { return forwardingInst; }
  SILInstruction *operator->() { return forwardingInst; }
  const SILInstruction *operator*() const { return forwardingInst; }
  SILInstruction *operator*() { return forwardingInst; }

  ValueOwnershipKind getForwardingOwnershipKind();
  bool preservesOwnership();

  // FIXME: Find a better name. Even unary instructions like struct_extract
  // forward "all" operands.
  bool canForwardAllOperands() const {
    switch (forwardingInst->getKind()) {
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleInst:
    case SILInstructionKind::LinearFunctionInst:
    case SILInstructionKind::DifferentiableFunctionInst:
      return true;
    default:
      return false;
    }
  }

  // FIXME: Find a better name. Even instructions that forward all operands can
  // forward the first operand.
  bool canForwardFirstOperandOnly() const {
    return !canForwardAllOperands() && forwardingInst->getNumRealOperands() > 0;
  }

  ArrayRef<Operand> getForwardedOperands() const {
    if (canForwardAllOperands()) {
      return forwardingInst->getAllOperands();
    }
    if (canForwardFirstOperandOnly()) {
      return forwardingInst->getOperandRef(0);
    }
    return {};
  }

  MutableArrayRef<Operand> getForwardedOperands() {
    if (canForwardAllOperands()) {
      return forwardingInst->getAllOperands();
    }
    if (canForwardFirstOperandOnly()) {
      return forwardingInst->getOperandRef(0);
    }
    return {};
  }

  bool canForwardOwnedCompatibleValuesOnly() {
    switch (forwardingInst->getKind()) {
    case SILInstructionKind::MarkUninitializedInst:
      return true;
    default:
      return false;
    }
  }

  bool canForwardGuaranteedCompatibleValuesOnly() {
    switch (forwardingInst->getKind()) {
    case SILInstructionKind::TupleExtractInst:
    case SILInstructionKind::StructExtractInst:
    case SILInstructionKind::DifferentiableFunctionExtractInst:
    case SILInstructionKind::LinearFunctionExtractInst:
      return true;
    default:
      return false;
    }
  }

  /// Return true if the forwarded value has the same representation. If true,
  /// then the result can be mapped to the same storage without a move or copy.
  bool hasSameRepresentation() const;

  /// Return true if the forwarded value is address-only either before or after
  /// forwarding.
  bool isAddressOnly() const;
};
} // namespace swift

#endif