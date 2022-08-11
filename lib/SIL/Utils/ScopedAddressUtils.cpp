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

void ScopedAddressValue::createScopeEnd(SILBasicBlock::iterator insertPt,
                                        SILLocation loc) const {
  switch (kind) {
  case ScopedAddressValueKind::StoreBorrow: {
    SILBuilderWithScope(insertPt).createEndBorrow(loc, value);
    return;
  }
  case ScopedAddressValueKind::BeginAccess: {
    SILBuilderWithScope(insertPt).createEndAccess(loc, value, false);
    return;
  }
  case ScopedAddressValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  }
}

void ScopedAddressValue::endScopeAtUseBoundary() const {
  SmallVector<SILBasicBlock *, 4> discoveredBlocks;
  PrunedLiveness liveness(&discoveredBlocks);
  SmallVector<Operand *, 4> uses;

  // Collect all uses that need to be enclosed by the scope.
  findTransitiveUsesForAddress(value, &uses);

  for (auto *use : uses) {
    // Update all collected uses as non-lifetime ending.
    liveness.updateForUse(use->getUser(), /* lifetimeEnding */ false);
  }

  // Create scope ending instructions at the liveness boundary.
  endScopeAtLivenessBoundary(&liveness);
}

void ScopedAddressValue::endScopeAtLivenessBoundary(
    PrunedLiveness *liveness) const {
  // If no users exist, create scope ending instruction immediately after the
  // scoped address value.
  if (liveness->empty()) {
    createScopeEnd(value->getNextInstruction()->getIterator(),
                   RegularLocation::getAutoGeneratedLocation());
    return;
  }

  PrunedLivenessBoundary scopedAddressBoundary;
  scopedAddressBoundary.compute(*liveness);
  // Go over the boundary and create scope ending instructions.
  scopedAddressBoundary.visitInsertionPoints(
      [&](SILBasicBlock::iterator insertPt) {
        createScopeEnd(insertPt, RegularLocation::getAutoGeneratedLocation());
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

bool swift::needStoreBorrowExtenstionForNewUsers(
    StoreBorrowInst *sbi, SmallVectorImpl<SILInstruction *> &newUsers,
    DeadEndBlocks *deadEndBlocks) {
  PrunedLiveness liveness;
  ScopedAddressValue(sbi).visitScopeEndingUses([&](Operand *op) {
    liveness.updateForUse(op->getUser(), /* lifetimeEnding */ true);
    return true;
  });
  // store_borrow extension is necessary only when newUsers are not within its
  // scope.
  return !liveness.areUsersWithinBoundary(newUsers, deadEndBlocks);
}

bool swift::canExtendStoreBorrowToNewUsers(
    StoreBorrowInst *sbi, SmallVectorImpl<SILInstruction *> &newUsers,
    DeadEndBlocks *deadEndBlocks) {
  assert(needStoreBorrowExtenstionForNewUsers(sbi, newUsers, deadEndBlocks));

  PrunedLiveness newLiveness;
  ScopedAddressValue(sbi).visitScopeEndingUses([&](Operand *op) {
    // Update current scope ending uses as non-lifetime ending
    newLiveness.updateForUse(op->getUser(), /* lifetimeEnding */ false);
    return true;
  });

  for (auto *newUser : newUsers) {
    // Update newUsers as lifetime ending
    newLiveness.updateForUse(newUser, /* lifetimeEnding */ true);
  }

  // store_borrow extension is possible only when there are no other
  // store_borrows to the same destination within the store_borrow's lifetime
  // built from newUsers
  return !hasOtherStoreBorrowsInLifetime(sbi, &newLiveness, deadEndBlocks);
}

void swift::extendStoreBorrowToNewUsers(
    StoreBorrowInst *sbi, SmallVectorImpl<SILInstruction *> &newUsers,
    DeadEndBlocks *deadEndBlocks, InstModCallbacks callbacks) {
  assert(canExtendStoreBorrowToNewUsers(sbi, newUsers, deadEndBlocks));

  ScopedAddressValue scopedAddress(sbi);
  SmallVector<SILBasicBlock *, 4> discoveredBlocks;
  PrunedLiveness newLiveness(&discoveredBlocks);
  SmallVector<Operand *, 4> endBorrowUses;

  scopedAddress.visitScopeEndingUses([&](Operand *op) {
    endBorrowUses.push_back(op);
    // Update current scope ending uses as non-lifetime ending
    newLiveness.updateForUse(op->getUser(), /* lifetimeEnding */ false);
    return true;
  });
  for (auto *newUser : newUsers) {
    // Update newUsers as non-lifetime ending
    newLiveness.updateForUse(newUser, /* lifetimeEnding */ false);
  }
  // Add new scope-ending instructions
  scopedAddress.endScopeAtLivenessBoundary(&newLiveness);
  // Remove old scope-ending instructions
  for (auto *endBorrowUse : endBorrowUses) {
    callbacks.deleteInst(endBorrowUse->getUser());
  }
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