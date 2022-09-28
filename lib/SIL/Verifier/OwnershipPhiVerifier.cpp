//===--- OwnershipPhiVerifier.cpp
//---------------------------------------------===//
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

#define DEBUG_TYPE "sil-guaranteed-phi-verifier"

#include "OwnershipPhiVerifierPrivate.h"

using namespace swift;

bool OwnershipPhiVerifier::verifyDependentPhiLifetime(SILPhiArgument *phiArg,
                                                      SILValue baseVal) {
  SmallVector<Operand *, 4> baseValConsumingUses(baseVal->getConsumingUses());
  // If the baseValue has no consuming uses, there is nothing more to verify
  if (baseValConsumingUses.empty())
    return false;

  SmallVector<Operand *, 4> phiArgUses(phiArg->getUses());
  LinearLifetimeChecker checker(deadEndBlocks);
  // newErrorBuilder is consumed at the end of the checkValue function.
  // Copy initial state from errorBuilder everytime
  LinearLifetimeChecker::ErrorBuilder newErrorBuilder = errorBuilder;
  // Verify whether the guaranteed phi arg lies within the lifetime of the base
  // value.
  auto linearLifetimeResult = checker.checkValue(baseVal, baseValConsumingUses,
                                                 phiArgUses, newErrorBuilder);
  return linearLifetimeResult.getFoundError();
}

void OwnershipPhiVerifier::verifyOwnershipPhis(SingleValueInstruction *borrow,
                                               SILValue baseValue) {
  assert(isa<BeginBorrowInst>(borrow) || isa<LoadBorrowInst>(borrow));
  auto visitDependentPhiBaseValuePair = [&](SILPhiArgument *phiArg,
                                            SILValue baseValue) {
    // If the (phiArg, baseValue) pair was not visited before, verify the
    // lifetime.
    auto it = dependentPhiToBaseValueMap.find(phiArg);
    if (it == dependentPhiToBaseValueMap.end() ||
        it->second.find(baseValue) == it->second.end()) {
      dependentPhiToBaseValueMap[phiArg].insert(baseValue);
      verifyDependentPhiLifetime(phiArg, baseValue);
    }
  };

  findTransitiveDependentPhiBaseValuePairs(borrow, baseValue,
                                           visitDependentPhiBaseValuePair);
}
