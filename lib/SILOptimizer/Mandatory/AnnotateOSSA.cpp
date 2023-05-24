//===------- AnnotateOSSA.cpp - optimize hop_to_executor ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "annotate-ossa"
#include "swift/SIL/ForwardingUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {
class AnnotateOSSAPass : public SILFunctionTransform {
public:
  void run() override {
    auto *func = getFunction();
    if (!func->hasOwnership())
      return;

    annotateEscaping();
  }

  void annotateEscaping() {
    auto *func = getFunction();

    for (auto &bb : *func) {
      for (auto *arg : bb.getArguments()) {
        if (findPointerEscape(arg)) {
          arg->setEscaping(true);
        }
      }

      for (auto &inst : bb) {
        for (auto res : inst.getResults()) {
          if (auto forwardedValue = ForwardingValue(res)) {
            if (findPointerEscape(res)) {
              forwardedValue.visitIntroducers(
                  [&](SILValue introducer) {
                    setEscaping(introducer);
                    return true;
                  },
                  /*createIntroducers*/ true);
            }
          }
        }
      }
    }

    for (auto &bb : *func) {
      for (auto *arg : bb.getArguments()) {
        bool compute = findPointerEscape(arg);
        assert(!(compute ^ arg->isEscaping()));
      }

      for (auto &inst : bb) {
        for (auto res : inst.getResults()) {
          if (auto forwardedValue = ForwardingValue(res)) {
            bool compute = findPointerEscape(res);
            assert(!(compute ^ hasPointerEscape(res)));
          }
        }
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAnnotateOSSA() { return new AnnotateOSSAPass(); }
