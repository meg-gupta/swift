//===------ TypeFullyInhabited.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Semantic analysis for ConvertibleToBytes and ConvertibleFromBytes.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckFullyInhabited.h"
#include "TypeCheckInvertible.h" // StorageVisitor
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"

using namespace swift;

bool checkFullyInhabitedSubConformance(ProtocolConformance *conformance,
                                       KnownProtocolKind kind) {
  assert(conformance->getProtocol() ==
         conformance->getDeclContext()
             ->getParentModule()
             ->getASTContext()
             .getProtocol(kind));
  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return false;

  // If this is an always-unavailable conformance, there's nothing to check.
  if (auto ext = dyn_cast<ExtensionDecl>(conformanceDC)) {
    if (ext->isUnavailable())
      return false;
  }

  // ConvertibleToBytes must be added in the same module or its overlay.
  auto conformanceDecl = conformanceDC->getAsDecl();
  if (!conformanceDecl->getModuleContext()->isSameModuleLookingThroughOverlays(
          nominal->getModuleContext())) {
    conformanceDecl->diagnose(diag::convertible_to_bytes_outside_module, nominal);
    return true;
  }

  if (conformanceDecl->getDeclContext()->getParentModule()->isStdlibModule()) {
    return true;
  }

  conformanceDecl->diagnose(diag::convertible_to_bytes_outside_module, nominal);
  return false;
}

bool swift::checkConvertibleToBytesConformance(ProtocolConformance *conformance) {
  return checkFullyInhabitedSubConformance(conformance,
                                           KnownProtocolKind::ConvertibleToBytes);
}

bool swift::checkConvertibleFromBytesConformance(ProtocolConformance *conformance) {
  return checkFullyInhabitedSubConformance(conformance,
                                           KnownProtocolKind::ConvertibleFromBytes);
}
