//===--- ScopedAddressUtils.cpp -------------------------------------------===//
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

#include "swift/SIL/ScopedAddressUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

void ScopedAddressOperandKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case Kind::Invalid:
    llvm_unreachable("Using an unreachable?!");
  case Kind::StoreBorrow:
    os << "StoreBorrow";
    return;
  case Kind::BeginAccess:
    os << "BeginAccess";
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ScopedAddressOperandKind kind) {
  kind.print(os);
  return os;
}

void ScopedAddressOperand::print(llvm::raw_ostream &os) const {
  os << "ScopedAddressOperand:\n"
        "Kind: "
     << kind
     << "\n"
        "Value: "
     << op->get() << "User: " << *op->getUser();
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     const ScopedAddressOperand &operand) {
  operand.print(os);
  return os;
}

void ScopedAddressValueKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case ScopedAddressValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case ScopedAddressValueKind::StoreBorrow:
    os << "StoreBorrow";
    return;
  case ScopedAddressValueKind::BeginAccess:
    os << "BeginAccess";
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ScopedAddressValueKind kind) {
  kind.print(os);
  return os;
}

bool ScopedAddressOperand::visitScopeEndingUses(
    function_ref<bool(Operand *)> func) const {
  switch (kind) {
  case ScopedAddressOperandKind::Invalid:
    llvm_unreachable("Using invalid case");
  case ScopedAddressOperandKind::StoreBorrow: {
    bool deadBorrow = true;
    for (auto *use : cast<StoreBorrowInst>(op->getUser())->getUses()) {
      if (isa<EndBorrowInst>(use->getUser())) {
        deadBorrow = false;
        if (!func(use))
          return false;
      }
    }
    return !deadBorrow;
  }
  case ScopedAddressOperandKind::BeginAccess: {
    bool deadAccess = true;
    auto *user = cast<BeginAccessInst>(op->getUser());
    for (auto *use : user->getUses()) {
      if (isa<EndAccessInst>(use->getUser())) {
        deadAccess = false;
        if (!func(use))
          return false;
      }
    }
    return !deadAccess;
  }
  }
  llvm_unreachable("Covered switch isn't covered");
}

void ScopedAddressOperand::getImplicitUses(
    SmallVectorImpl<Operand *> &foundUses) const {
  // FIXME: this visitScopeEndingUses should never return false once dead
  // borrows are disallowed.
  if (!visitScopeEndingUses([&](Operand *endOp) {
        foundUses.push_back(endOp);
        return true;
      })) {
    // Special-case for dead borrows.
    foundUses.push_back(op);
  }
}

bool ScopedAddressValue::isScopeEndingUse(Operand *op) const {
  switch (kind) {
  case ScopedAddressValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case ScopedAddressValueKind::StoreBorrow: {
    if (auto *endBorrow = dyn_cast<EndBorrowInst>(op->getUser())) {
      return endBorrow->getOperand() == value;
    }
    return false;
  }
  case ScopedAddressValueKind::BeginAccess: {
    if (auto *endAccess = dyn_cast<EndAccessInst>(op->getUser())) {
      return endAccess->getOperand() == value;
    }
    return false;
  }
  }
}

bool ScopedAddressValue::visitScopeEndingUses(
    function_ref<bool(Operand *)> visitor) const {
  switch (kind) {
  case ScopedAddressValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case ScopedAddressValueKind::StoreBorrow: {
    for (auto *use : value->getUses()) {
      if (isa<EndBorrowInst>(use->getUser())) {
        if (!visitor(use))
          return false;
      }
    }
    return true;
  }
  case ScopedAddressValueKind::BeginAccess: {
    for (auto *use : value->getUses()) {
      if (isa<EndAccessInst>(use->getUser())) {
        if (!visitor(use))
          return false;
      }
    }
    return true;
  }
  }
}

void ScopedAddressValue::computeLiveness(PrunedLiveness &liveness) const {
  liveness.initializeDefBlock(value->getParentBlock());

  visitScopeEndingUses([&](Operand *endOp) {
    liveness.updateForUse(endOp->getUser(), /* isLifetimeEnding */ true);
    return true;
  });
}

bool swift::hasOtherStoreBorrowsInLifetime(StoreBorrowInst *storeBorrow,
                                           PrunedLiveness *liveness,
                                           DeadEndBlocks *deadEndBlocks) {
  SmallVector<StoreBorrowInst *, 4> otherStoreBorrows;
  // Collect all other store_borrows to the destination of \p storeBorrow
  for (auto *destUse : storeBorrow->getDest()->getUses()) {
    if (auto *user = dyn_cast<StoreBorrowInst>(destUse->getUser())) {
      if (user == storeBorrow) {
        continue;
      }
      otherStoreBorrows.push_back(user);
    }
  }

  for (auto *otherStoreBorrow : otherStoreBorrows) {
    // Return true, if otherStoreBorrow was in \p storeBorrow's scope
    if (liveness->isWithinBoundaryOfDef(otherStoreBorrow, storeBorrow)) {
      return true;
    }
  }
  return false;
}

void ScopedAddressValue::print(llvm::raw_ostream &os) const {
  os << "ScopedAddressIntroducingValue:\n"
        "Kind: "
     << kind
     << "\n"
        "Value: "
     << value;
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     const ScopedAddressValue &value) {
  value.print(os);
  return os;
}