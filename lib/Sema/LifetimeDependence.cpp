//===--- LifetimeDependence.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LifetimeDependence.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Defer.h"

namespace swift {
std::string LifetimeDependenceInfo::getString() const {
  std::string lifetimeDependenceString;
  auto getOnIndices = [](IndexSubset *bitvector) {
    std::string result;
    bool isFirstSetBit = true;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSetBit) {
          result += ", ";
        }
        result += std::to_string(i);
        isFirstSetBit = false;
      }
    }
    return result;
  };
  if (inheritLifetimeParamIndices && !inheritLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString =
        "_inherit(" + getOnIndices(inheritLifetimeParamIndices) + ") ";
  }
  if (scopeLifetimeParamIndices && !scopeLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_scope(" + getOnIndices(scopeLifetimeParamIndices) + ") ";
  }
  return lifetimeDependenceString;
}

void LifetimeDependenceInfo::Profile(llvm::FoldingSetNodeID &ID) const {
  if (inheritLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Inherit);
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (scopeLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Scope);
    scopeLifetimeParamIndices->Profile(ID);
  }
}

static LifetimeDependenceKind
getLifetimeDependenceKindFromSpecifier(Type sourceType) {
  if (sourceType->isEscapable()) {
    return LifetimeDependenceKind::Scope;
  }
  return LifetimeDependenceKind::Inherit;
}

static ValueOwnership getLoweredOwnership(AbstractFunctionDecl *afd) {
  if (isa<ConstructorDecl>(afd)) {
    return ValueOwnership::Owned;
  }
  if (auto *ad = dyn_cast<AccessorDecl>(afd)) {
    if (ad->getAccessorKind() == AccessorKind::Set ||
        ad->getAccessorKind() == AccessorKind::Modify) {
      return ValueOwnership::InOut;
    }
  }
  return ValueOwnership::Shared;
}

static bool isLifetimeDependenceCompatibleWithOwnership(
    LifetimeDependenceKind lifetimeDependenceKind,
    ValueOwnership loweredOwnership) {
  assert(loweredOwnership != ValueOwnership::Default);

  switch (lifetimeDependenceKind) {
  case LifetimeDependenceKind::Inherit:
    return true;

  case LifetimeDependenceKind::Scope:
    return (loweredOwnership == ValueOwnership::InOut ||
            loweredOwnership == ValueOwnership::Shared)
               ? true
               : false;
  }
}

LifetimeDependenceInfo LifetimeDependenceInfo::getForParamIndex(
    AbstractFunctionDecl *afd, unsigned index, LifetimeDependenceKind kind) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  unsigned capacity =
      afd->hasImplicitSelfDecl() ? (afd->getParameters()->size() + 1) : afd->getParameters()->size();
  auto indexSubset = IndexSubset::get(ctx, capacity, {index});
  if (kind == LifetimeDependenceKind::Scope) {
    return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ nullptr,
                                  /*scopeLifetimeParamIndices*/ indexSubset};
  }
  return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ indexSubset,
                                /*scopeLifetimeParamIndices*/ nullptr};
}

void LifetimeDependenceInfo::getConcatenatedData(
    SmallVectorImpl<bool> &concatenatedData) const {
  auto pushData = [&](IndexSubset *paramIndices) {
    if (paramIndices == nullptr) {
      return;
    }
    assert(!paramIndices->isEmpty());

    for (unsigned i = 0; i < paramIndices->getCapacity(); i++) {
      if (paramIndices->contains(i)) {
        concatenatedData.push_back(true);
        continue;
      }
      concatenatedData.push_back(false);
    }
  };
  if (hasInheritLifetimeParamIndices()) {
    pushData(inheritLifetimeParamIndices);
  }
  if (hasScopeLifetimeParamIndices()) {
    pushData(scopeLifetimeParamIndices);
  }
}

static bool hasEscapableResultOrYield(AbstractFunctionDecl *afd,
                                      Type resultType) {
  Type resultTypeInContext = afd->mapTypeIntoContext(resultType);
  std::optional<Type> yieldTyInContext;

  if (auto *accessor = dyn_cast<AccessorDecl>(afd)) {
    if (accessor->isCoroutine()) {
      yieldTyInContext = accessor->getStorage()->getValueInterfaceType();
      yieldTyInContext = accessor->mapTypeIntoContext(*yieldTyInContext);
    }
  }
  if (resultTypeInContext->isEscapable() &&
      (!yieldTyInContext.has_value() || (*yieldTyInContext)->isEscapable())) {
    return true;
  }
  return false;
}

static LifetimeDependenceKind getLifetimeDependenceKindFromSpecifier(
    LifetimeDependenceSpecifier specifier, Type sourceType) {
  switch (specifier.getParsedLifetimeDependenceKind()) {
  case ParsedLifetimeDependenceKind::Scope:
    return LifetimeDependenceKind::Scope;

  case ParsedLifetimeDependenceKind::Inherit:
    // TODO: assert that this can happen only on deserialized decls
    return LifetimeDependenceKind::Inherit;

  case ParsedLifetimeDependenceKind::Default:
    return sourceType->isEscapable() ? LifetimeDependenceKind::Scope
                                     : LifetimeDependenceKind::Inherit;
  }
}

static std::optional<LifetimeDependenceKind> tryGetLifetimeDependenceKind(
    LifetimeDependenceSpecifier specifier, unsigned sourceIndex,
    ValueOwnership sourceOwnership, Type sourceType, DeclContext *dc) {
  auto loc = specifier.getLoc();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto *mod = dc->getParentModule();

  // Diagnose when we have lifetime dependence on a type that is
  // BitwiseCopyable & Escapable.
  // ~Escapable types are non-trivial in SIL and we should not raise this
  // error.
  // TODO: Diagnose ~Escapable types are always non-trivial in SIL.
  if (sourceType->isEscapable()) {
    if (ctx.LangOpts.hasFeature(Feature::BitwiseCopyable)) {
      auto *bitwiseCopyableProtocol =
          ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
      if (bitwiseCopyableProtocol &&
          mod->checkConformance(sourceType, bitwiseCopyableProtocol)) {
        diags.diagnose(loc, diag::lifetime_dependence_on_bitwise_copyable);
        return std::nullopt;
      }
    }
  }

  auto lifetimeDependenceKind =
      getLifetimeDependenceKindFromSpecifier(specifier, sourceType);
  bool isCompatible = isLifetimeDependenceCompatibleWithOwnership(
      lifetimeDependenceKind, sourceOwnership);

  if (!isCompatible) {
    assert(lifetimeDependenceKind == LifetimeDependenceKind::Scope);

    if (specifier.getParsedLifetimeDependenceKind() ==
        ParsedLifetimeDependenceKind::Scope) {
      diags.diagnose(
          loc, diag::lifetime_dependence_cannot_use_parsed_scoped_consuming);
      return std::nullopt;;
    }
    diags.diagnose(
        loc, diag::lifetime_dependence_cannot_use_inferred_scoped_consuming);
    return std::nullopt;;
  }

  return lifetimeDependenceKind;
}

std::optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::fromTypeRepr(LifetimeDependentReturnTypeRepr *lifetimeRepr,
                                     ArrayRef<AnyFunctionType::Param> params,
                                     Type outputType, bool hasSelf, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto *mod = dc->getParentModule();
  auto &diags = ctx.Diags;
  auto capacity = hasSelf ? (params.size() + 1) : params.size();

  if (outputType->isEscapable()) {
    diags.diagnose(lifetimeRepr->getLoc(),
                   diag::lifetime_dependence_invalid_return_type);
    return std::nullopt;
  }
  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  for (auto specifier : lifetimeRepr->getLifetimeDependencies()) {
    switch (specifier.getSpecifierKind()) {
    case LifetimeDependenceSpecifier::SpecifierKind::Named: {
      bool foundParamName = false;
      unsigned paramIndex = 0;
      for (auto param : *params) {
        if (param.getParameterName() == specifier.getName()) {
          if (tryGetLifetimeDependenceKind(
                  specifier, paramIndex)) {
            return std::nullopt;
          }
          foundParamName = true;
          break;
        }
        paramIndex++;
      }
      if (!foundParamName) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_name,
                       specifier.getName());
        return std::nullopt;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Ordered: {
      auto index = specifier.getIndex();
      if (index >= capacity) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      if (tryGetLifetimeDependenceKind(specifier, index)) {
        return std::nullopt;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Self: {
      if (!afd->hasImplicitSelfDecl()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self_in_static);
        return std::nullopt;
      }
      if (isa<ConstructorDecl>(afd)) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self_in_init);
        return std::nullopt;
      }
      if (tryGetLifetimeDependenceKind(
              specifier, /* selfIndex */ afd->getParameters()->size(),
              afd->getImplicitSelfDecl()->getTypeInContext(),
              afd->getImplicitSelfDecl()->getValueOwnership())) {
        return std::nullopt;
      }
      break;
    }
    }
  }

  return LifetimeDependenceInfo(
      inheritLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
          : nullptr,
      scopeLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
          : nullptr,
      /*isExplicit*/ true);
}

std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromTypeRepr(
    LifetimeDependentReturnTypeRepr *lifetimeDependentRepr,
    ArrayRef<AnyFunctionType::Param> params, Type resultType,
    ParamDecl *selfDecl, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto *mod = dc->getParentModule();
  auto &diags = ctx.Diags;
  auto capacity = selfDecl ? (params.size() + 1) : params.size();

  if (resultType->isEscapable()) {
    diags.diagnose(lifetimeDependentRepr->getLoc(),
                   diag::lifetime_dependence_invalid_return_type);
    return std::nullopt;
  }

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  std::optional<LifetimeDependenceKind> lifetimeDependenceKind;
  unsigned paramOrSelfIndex;

  for (auto specifier : lifetimeDependentRepr->getLifetimeDependencies()) {
    switch (specifier.getSpecifierKind()) {
    case LifetimeDependenceSpecifier::SpecifierKind::Named: {
      auto index =
          getParamIndex(params, specifier.getName());
      if (!index.has_value()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_name,
                       specifier.getName());
        return std::nullopt;
      }
      paramOrSelfIndex = *index;
      auto param = params[paramOrSelfIndex];
      lifetimeDependenceKind = tryGetLifetimeDependenceKind(
          specifier, paramOrSelfIndex, param.getValueOwnership(),
          param.getParameterType(), dc);
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Ordered: {
      paramOrSelfIndex = specifier.getIndex();
      if (paramOrSelfIndex >= capacity) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index,
                       paramOrSelfIndex);
        return std::nullopt;
      }
      auto isSelfDependence = paramOrSelfIndex == params.size();
      lifetimeDependenceKind = tryGetLifetimeDependenceKind(
          specifier, paramOrSelfIndex,
          isSelfDependence ? selfDecl->getValueOwnership()
                           : params[paramOrSelfIndex].getValueOwnership(),
          isSelfDependence ? selfDecl->getTypeInContext()
                           : params[paramOrSelfIndex].getParameterType(),
          dc);
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Self: {
      if (!selfDecl) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self_in_static);
        return std::nullopt;
      }
      paramOrSelfIndex = /* selfIndex */ params.size();
      lifetimeDependenceKind = tryGetLifetimeDependenceKind(
          specifier, paramOrSelfIndex,
          selfDecl->getValueOwnership(),
          selfDecl->getTypeInContext(), dc);

      if (!lifetimeDependenceKind.has_value()) {
        return std::nullopt;
      }
      break;
    }
    }

    if (*lifetimeDependenceKind == LifetimeDependenceKind::Inherit) {
      if (inheritLifetimeParamIndices.test(paramOrSelfIndex)) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_duplicate_param_id);
        return std::nullopt;
      }
      inheritLifetimeParamIndices.set(paramOrSelfIndex);
    }
    if (*lifetimeDependenceKind == LifetimeDependenceKind::Scope) {
      if (scopeLifetimeParamIndices.test(paramOrSelfIndex)) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_duplicate_param_id);
        return std::nullopt;
      }
      scopeLifetimeParamIndices.set(paramOrSelfIndex);
    }
  }

  return LifetimeDependenceInfo(
      inheritLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
          : nullptr,
      scopeLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
          : nullptr,
      /*isExplicit*/ true);
}

  // This utility is similar to its overloaded version that builds the
  // LifetimeDependenceInfo from the swift decl. Reason for duplicated code is
  // the apis on type and ownership is different in SIL compared to Sema.
  std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromTypeRepr(
      LifetimeDependentReturnTypeRepr * lifetimeDependentRepr,
      SmallVectorImpl<SILParameterInfo> & params, DeclContext * dc) {
    auto &ctx = dc->getASTContext();
    auto &diags = ctx.Diags;
    auto capacity = params.size(); // SIL parameters include self

    SmallBitVector inheritLifetimeParamIndices(capacity);
    SmallBitVector scopeLifetimeParamIndices(capacity);

    auto tryGetLifetimeDependenceKind =
        [&](LifetimeDependenceSpecifier specifier, unsigned paramIndexToSet,
            ParameterConvention paramConvention) {
          auto loc = specifier.getLoc();
          auto kind = specifier.getParsedLifetimeDependenceKind();

          if (kind == ParsedLifetimeDependenceKind::Scope &&
              (!isGuaranteedParameter(paramConvention) &&
               !isMutatingParameter(paramConvention))) {
            diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind,
                           "_scope",
                           getStringForParameterConvention(paramConvention));
            return true;
          }

          if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
              scopeLifetimeParamIndices.test(paramIndexToSet)) {
            diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
            return true;
          }
          if (kind == ParsedLifetimeDependenceKind::Inherit) {
            inheritLifetimeParamIndices.set(paramIndexToSet);
          } else {
            assert(kind == ParsedLifetimeDependenceKind::Scope);
            scopeLifetimeParamIndices.set(paramIndexToSet);
          }
          return false;
        };

    for (auto specifier : lifetimeDependentRepr->getLifetimeDependencies()) {
      assert(specifier.getSpecifierKind() ==
             LifetimeDependenceSpecifier::SpecifierKind::Ordered);
      auto index = specifier.getIndex();
      if (index > capacity) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      auto param = params[index];
      auto paramConvention = param.getConvention();
      if (tryGetLifetimeDependenceKind(specifier, index, paramConvention)) {
        return std::nullopt;
      }
    }

    return LifetimeDependenceInfo(
        inheritLifetimeParamIndices.any()
            ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
            : nullptr,
        scopeLifetimeParamIndices.any()
            ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
            : nullptr,
        /*isExplicit*/ true);
  }

  std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::infer(
      AbstractFunctionDecl * afd, Type resultType) {
    auto *dc = afd->getDeclContext();
    auto &ctx = dc->getASTContext();

    if (!ctx.LangOpts.hasFeature(Feature::NonescapableTypes)) {
      return std::nullopt;
    }

    // Disable inference if requested.
    if (!ctx.LangOpts.EnableExperimentalLifetimeDependenceInference) {
      return std::nullopt;
    }

    if (hasEscapableResultOrYield(afd, resultType)) {
      return std::nullopt;
    }

    if (afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {
      return std::nullopt;
    }

    auto &diags = ctx.Diags;
    auto returnTypeRepr = afd->getResultTypeRepr();
    // Calling AbstractFunctionDecl::getLoc() during
    // InterfaceTypeRequest::evaluate() can cause circular reference errors.
    // Using getLoc(/* SerializedOK*/ false) is a workaround for this. Filed
    // rdar://125597747 for underlying issue.
    auto returnLoc = returnTypeRepr ? returnTypeRepr->getLoc()
                                    : afd->getLoc(/* SerializedOK */ false);

    auto *cd = dyn_cast<ConstructorDecl>(afd);
    if (cd && cd->isImplicit()) {
      if (cd->getParameters()->size() == 0) {
        return std::nullopt;
      }
    }

    // For methods, infer lifetime dependence based on self ownership and type.
    if (afd->getKind() != DeclKind::Constructor && afd->hasImplicitSelfDecl()) {
      Type selfTypeInContext = dc->getSelfTypeInContext();
      auto kind = getLifetimeDependenceKindFromSpecifier(selfTypeInContext);
      auto selfOwnership = afd->getImplicitSelfDecl()->getValueOwnership();
      if (!isLifetimeDependenceCompatibleWithOwnership(kind, selfOwnership,
                                                       afd)) {
        diags.diagnose(returnLoc,
                       diag::lifetime_dependence_invalid_self_ownership);
        return std::nullopt;
      }
      return LifetimeDependenceInfo::getForParamIndex(
          afd, /*selfIndex*/ afd->getParameters()->size(), kind);
    }

    LifetimeDependenceInfo lifetimeDependenceInfo;
    ParamDecl *candidateParam = nullptr;
    unsigned paramIndex = 0;
    bool hasParamError = false;

    // For non-methods, we infer lifetime dependence if we can find a single
    // param that is: ~Escapable or ~Copyable or has ownership modifier.
    for (auto *param : *afd->getParameters()) {
      SWIFT_DEFER { paramIndex++; };
      Type paramTypeInContext =
          afd->mapTypeIntoContext(param->getInterfaceType());
      if (paramTypeInContext->hasError()) {
        hasParamError = true;
        continue;
      }
      auto paramOwnership = param->getValueOwnership();
      if (paramTypeInContext->isEscapable() &&
          !paramTypeInContext->isNoncopyable() &&
          paramOwnership == ValueOwnership::Default) {
        continue;
      }

      auto lifetimeDependenceKind =
          getLifetimeDependenceKindFromSpecifier(paramTypeInContext);
      if (!isLifetimeDependenceCompatibleWithOwnership(lifetimeDependenceKind,
                                                       paramOwnership, afd)) {
        continue;
      }
      if (candidateParam) {
        diags.diagnose(
            returnLoc,
            diag::lifetime_dependence_cannot_infer_ambiguous_candidate,
            afd->getKind() == DeclKind::Constructor && afd->isImplicit()
                ? "on implicit initializer"
                : "");
        return std::nullopt;
      }
      candidateParam = param;
      lifetimeDependenceInfo = LifetimeDependenceInfo::getForParamIndex(
          afd, paramIndex, lifetimeDependenceKind);
    }

    if (!candidateParam && !hasParamError) {
      diags.diagnose(
          returnLoc, diag::lifetime_dependence_cannot_infer_no_candidates,
          afd->getKind() == DeclKind::Constructor && afd->isImplicit()
              ? "on implicit initializer"
              : "");
      return std::nullopt;
    }
    return lifetimeDependenceInfo;
  }

  std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::get(
      AbstractFunctionDecl * afd, Type resultType, bool allowIndex) {
    auto *returnTypeRepr = afd->getResultTypeRepr();
    if (isa_and_nonnull<LifetimeDependentReturnTypeRepr>(returnTypeRepr)) {
      return LifetimeDependenceInfo::fromTypeRepr(afd, resultType, allowIndex);
    }
    return LifetimeDependenceInfo::infer(afd, resultType);
  }

  LifetimeDependenceInfo LifetimeDependenceInfo::get(
      ASTContext & ctx, const SmallBitVector &inheritLifetimeIndices,
      const SmallBitVector &scopeLifetimeIndices) {
    return LifetimeDependenceInfo{
        inheritLifetimeIndices.any()
            ? IndexSubset::get(ctx, inheritLifetimeIndices)
            : nullptr,
        scopeLifetimeIndices.any() ? IndexSubset::get(ctx, scopeLifetimeIndices)
                                   : nullptr};
  }

  std::optional<LifetimeDependenceKind>
  LifetimeDependenceInfo::getLifetimeDependenceOnParam(unsigned paramIndex) {
    if (inheritLifetimeParamIndices) {
      if (inheritLifetimeParamIndices->contains(paramIndex)) {
        return LifetimeDependenceKind::Inherit;
      }
    }
    if (scopeLifetimeParamIndices) {
      if (scopeLifetimeParamIndices->contains(paramIndex)) {
        return LifetimeDependenceKind::Scope;
      }
    }
    return {};
  }

} // namespace swift
