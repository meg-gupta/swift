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

class ForwardingValue {
  SILValue forwardingValue = nullptr;

public:
  explicit ForwardingValue(SILValue value);

  bool visitDefs(function_ref<bool(SILValue)> visitor);

  operator bool() const { return bool(forwardingValue); }
  const SILValue operator->() const {
    assert(forwardingValue);
    return forwardingValue;
  }
  SILValue operator->() {
    assert(forwardingValue);
    return forwardingValue;
  }
};
} // namespace swift

#endif