//===--- ForwardingUtils.cpp
//-----------------------------------------------===//
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

#include "swift/SIL/ForwardingUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBuilder.h"

using namespace swift;

ForwardingValue::ForwardingValue(SILValue value) {
  if (auto *inst = value->getDefiningInstructionOrTerminator()) {
    if (auto *mixin = OwnershipForwardingMixin::get(inst)) {
      this->forwardingValue = value;
      return;
    }
  }
}

bool ForwardingValue::visitDefs(function_ref<bool(SILValue)> visitor) {
  ValueWorklist worklist(forwardingValue);
  if (auto *allArgFwd =
          dyn_cast<AllArgOwnershipForwardingSingleValueInst>(forwardingValue)) {
    for (auto opValue : allArgFwd->getOperandValues()) {
      worklist.pushIfNotVisited(opValue);
    }
  } else {
    auto *inst = forwardingValue->getDefiningInstructionOrTerminator();
    auto opValue = inst->getOperand(0);
    worklist.pushIfNotVisited(opValue);
  }

  while (auto value = worklist.pop()) {
    if (!visitor(value)) {
      return false;
    }
    if (ForwardingValue(value)) {
      if (auto *allArgFwd =
              dyn_cast<AllArgOwnershipForwardingSingleValueInst>(value)) {
        for (auto opValue : allArgFwd->getOperandValues()) {
          worklist.pushIfNotVisited(opValue);
        }
      } else {
        auto *inst = value->getDefiningInstructionOrTerminator();
        if (inst->getNumRealOperands() > 0) {
          auto opValue = inst->getOperand(0);
          worklist.pushIfNotVisited(opValue);
        }
      }
    }
    if (auto *borrow = dyn_cast<BeginBorrowInst>(value)) {
      worklist.pushIfNotVisited(borrow->getOperand());
    }
    if (auto *move = dyn_cast<MoveValueInst>(value)) {
      worklist.pushIfNotVisited(move->getOperand());
    }
    if (auto *phi = SILArgument::asPhi(value)) {
      phi->visitTransitiveIncomingPhiOperands([&](auto *phi, auto *operand) {
        worklist.pushIfNotVisited(operand->get());
        return true;
      });
    }
  }
  return true;
}

bool ForwardingValue::visitIntroducers(
    function_ref<bool(SILValue)> visitor, bool createIntroducers) {
  ValueWorklist worklist(forwardingValue);
  if (auto *allArgFwd =
          dyn_cast<AllArgOwnershipForwardingSingleValueInst>(forwardingValue)) {
    for (auto opValue : allArgFwd->getOperandValues()) {
      worklist.pushIfNotVisited(opValue);
    }
  } else {
    auto *inst = forwardingValue->getDefiningInstructionOrTerminator();
    auto opValue = inst->getOperand(0);
    worklist.pushIfNotVisited(opValue);
  }

  while (auto value = worklist.pop()) {
    if (ForwardingValue(value)) {
      if (auto *allArgFwd =
              dyn_cast<AllArgOwnershipForwardingSingleValueInst>(value)) {
        for (auto opValue : allArgFwd->getOperandValues()) {
          worklist.pushIfNotVisited(opValue);
        }
      } else {
        auto *inst = value->getDefiningInstructionOrTerminator();
        if (inst->getNumRealOperands() > 0) {
          auto opValue = inst->getOperand(0);
          worklist.pushIfNotVisited(opValue);
        }
      }
      continue;
    }
    if (isa<BeginBorrowInst>(value) || isa<MoveValueInst>(value) ||
        isa<AllocBoxInst>(value)) {
      if (!visitor(value)) {
        return false;
      }
      continue;
    }
    if (auto *arg = dyn_cast<SILArgument>(value)) {
      if (arg->isTerminatorResult()) {
        assert(isa<TryApplyInst>(arg->getTerminatorForResult()));
        if (!visitor(arg)) {
          return false;
        }
        continue;
      }
      if (!visitor(value)) {
        return false;
      }
      continue;
    }
    if (!createIntroducers) {
      continue;
    }
    assert(!isCandidateIntroducer(value));
    if (value->getOwnershipKind() == OwnershipKind::Owned) {
      auto *move = SILBuilderWithScope(
                       value->getDefiningInsertionPoint()->getNextInstruction())
                       .createMoveValue(value.getLoc(), value);
      value->replaceAllUsesWith(move);
      move->setOperand(value);
      if (!visitor(move)) {
        return false;
      }
      continue;
    }
    assert(value->getOwnershipKind() == OwnershipKind::Guaranteed);
    auto *borrow = SILBuilderWithScope(
                       value->getDefiningInsertionPoint()->getNextInstruction())
                       .createBeginBorrow(value.getLoc(), value);
    value->replaceAllUsesWith(borrow);
    borrow->setOperand(value);
    if (!visitor(borrow)) {
      return false;
    }
  }
  return true;
}
