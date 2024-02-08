//===--- LifetimeDependenceSpecifiers.h ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines types and utilities related to Lifetime Dependence
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LIFETIMEDEPENDENCE_H
#define SWIFT_AST_LIFETIMEDEPENDENCE_H

#include "swift/AST/Identifier.h"
#include "swift/AST/IndexSubset.h"
#include "swift/AST/Ownership.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/SourceLoc.h"

#include "llvm/ADT/ArrayRef.h"

namespace swift {

class AbstractFunctionDecl;
class LifetimeDependentReturnTypeRepr;

enum class ParseableLifetimeDependenceKind : uint8_t {
  Copy = 0,
  Consume,
  Borrow,
  Mutate
};

enum class LifetimeDependenceKind : uint8_t { Inherit = 0, Scope };

class LifetimeDependenceSpecifier {
public:
  enum class SpecifierKind { Named, Ordered, Self };

private:
  SourceLoc loc;
  SpecifierKind specifierKind;
  ParseableLifetimeDependenceKind lifetimeDependenceKind;
  union Value {
    struct {
      Identifier name;
    } Named;
    struct {
      unsigned index;
    } Ordered;
    struct {
    } self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() {}
  } value;

  LifetimeDependenceSpecifier(
      SourceLoc loc, SpecifierKind specifierKind,
      ParseableLifetimeDependenceKind lifetimeDependenceKind, Value value)
      : loc(loc), specifierKind(specifierKind),
        lifetimeDependenceKind(lifetimeDependenceKind), value(value) {}

public:
  static LifetimeDependenceSpecifier getNamedLifetimeDependenceSpecifier(
      SourceLoc loc, ParseableLifetimeDependenceKind kind, Identifier name) {
    return {loc, SpecifierKind::Named, kind, name};
  }

  static LifetimeDependenceSpecifier getOrderedLifetimeDependenceSpecifier(
      SourceLoc loc, ParseableLifetimeDependenceKind kind, unsigned index) {
    return {loc, SpecifierKind::Ordered, kind, index};
  }

  static LifetimeDependenceSpecifier
  getSelfLifetimeDependenceSpecifier(SourceLoc loc,
                                     ParseableLifetimeDependenceKind kind) {
    return {loc, SpecifierKind::Self, kind, {}};
  }

  SourceLoc getLoc() const { return loc; }

  SpecifierKind getSpecifierKind() const { return specifierKind; }

  ParseableLifetimeDependenceKind getLifetimeDependenceKind() const {
    return lifetimeDependenceKind;
  }

  Identifier getName() const {
    assert(specifierKind == SpecifierKind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    assert(specifierKind == SpecifierKind::Ordered);
    return value.Ordered.index;
  }

  std::string getParamString() const {
    switch (specifierKind) {
    case SpecifierKind::Named:
      return value.Named.name.str().str();
    case SpecifierKind::Self:
      return "self";
    case SpecifierKind::Ordered:
      return std::to_string(value.Ordered.index);
    }
    llvm_unreachable("Invalid LifetimeDependenceSpecifier::SpecifierKind");
  }

  StringRef getLifetimeDependenceKindString() const {
    switch (lifetimeDependenceKind) {
    case ParseableLifetimeDependenceKind::Borrow:
      return "_borrow";
    case ParseableLifetimeDependenceKind::Consume:
      return "_consume";
    case ParseableLifetimeDependenceKind::Copy:
      return "_copy";
    case ParseableLifetimeDependenceKind::Mutate:
      return "_mutate";
    }
    llvm_unreachable(
        "Invalid LifetimeDependenceSpecifier::ParseableLifetimeDependenceKind");
  }
};

class LifetimeDependenceInfo {
  IndexSubset *inheritLifetimeParamIndices;
  IndexSubset *scopeLifetimeParamIndices;

  static LifetimeDependenceInfo getForParamIndex(AbstractFunctionDecl *afd,
                                                 unsigned index,
                                                 ValueOwnership ownership);

  static llvm::Optional<LifetimeDependenceInfo>
  fromTypeRepr(AbstractFunctionDecl *afd, Type resultType, bool allowIndex);

  static llvm::Optional<LifetimeDependenceInfo> infer(AbstractFunctionDecl *afd,
                                                      Type resultType);

public:
  LifetimeDependenceInfo()
      : inheritLifetimeParamIndices(nullptr),
        scopeLifetimeParamIndices(nullptr) {}
  LifetimeDependenceInfo(IndexSubset *inheritLifetimeParamIndices,
                         IndexSubset *scopeLifetimeParamIndices)
      : inheritLifetimeParamIndices(inheritLifetimeParamIndices),
        scopeLifetimeParamIndices(scopeLifetimeParamIndices) {}

  operator bool() const { return !empty(); }

  bool empty() const {
    return inheritLifetimeParamIndices == nullptr &&
           scopeLifetimeParamIndices == nullptr;
  }

  bool hasInheritLifetimeParamIndices() const {
    return inheritLifetimeParamIndices != nullptr;
  }
  bool hasBorrowLifetimeParamIndices() const {
    return scopeLifetimeParamIndices != nullptr;
  }
  llvm::Optional<LifetimeDependenceKind>
  getLifetimeDependenceFor(unsigned paramIndex);

  std::string getString() const;
  void Profile(llvm::FoldingSetNodeID &ID) const;
  void getConcatenatedData(SmallVectorImpl<bool> &concatenatedData) const;

  static llvm::Optional<LifetimeDependenceInfo>
  get(AbstractFunctionDecl *decl, Type resultType, bool allowIndex = false);
};

} // namespace swift

#endif