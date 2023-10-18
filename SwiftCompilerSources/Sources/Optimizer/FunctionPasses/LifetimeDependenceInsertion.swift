//===--- LifetimeDependenceInsertion.swift - insert lifetime dependence ---===//
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
///
/// Insert mark_dependence [nonescaping] markers on the return value
/// (or yielded value) of a call whose return type is non-escaping.
///
/// Pass dependencies: This must run as a SILGen cleanup pass before
/// any lifetime canonicalization or optimization can be performed.
///
//===----------------------------------------------------------------------===//

import SIL

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

let lifetimeDependenceInsertionPass = FunctionPass(
  name: "lifetime-dependence-insertion")
{ (function: Function, context: FunctionPassContext) in
  log("Inserting lifetime dependence markers in \(function.name)")

  for instruction in function.instructions {
    if let dependentApply = LifetimeDependentApply(instruction) {
      insertDependencies(for: dependentApply, context)
    }
  }
}

/// An apply that produces a non-escapable value, linking it to a parent value.
private struct LifetimeDependentApply {
  let applySite: FullApplySite

  init?(_ instruction: Instruction) {
    guard let apply = instruction as? FullApplySite else { return nil }
    self.applySite = apply
    // At least one non-escapable result means this is a
    // lifetime-dependent instruction.
    if dependentValues.contains(where: { $0.type.isEscapable }) { return }
    // TODO: Check the @_resultDependsOn attribute.
    return nil
  }

  init?(withResult value: Value) {
    switch value {
    case let apply as ApplyInst:
      if let dependentApply = LifetimeDependentApply(apply) {
        self = dependentApply
      }
    case let arg as Argument:
      guard let termResult = TerminatorResult(arg) else { return nil }
      switch termResult.terminator {
      case let ta as TryApplyInst:
        if termResult.successor == ta.errorBlock {
          if let dependentApply = LifetimeDependentApply(ta) {
            self = dependentApply
          }
        }
      default:
        break
      }
    default:
      break
    }
    return nil
  }
  
  /// The immediate uses that depend on this instruction's lifetime.
  var dependentValues: [Value] {
    if let beginApply = applySite as? BeginApplyInst {
      return Array(beginApply.yieldedValues)
    }
    return [applySite.singleDirectResult!]
  }
}

extension LifetimeDependentApply {
  /// A lifetime argument copies, borrows, or mutatably borrows the lifetime of the argument value.
  ///
  /// 'new' implies that the dependent values are not enclosed by a
  /// non-escaping access scope within this function. In this case,
  /// copies of the original value and copies of any forwarded value
  /// must not live beyond the original forward-extended
  /// lifetime. This happens when 'self' is a call to a
  /// @_unsafeNonEscapableResult function.
  enum LifetimeArgument {
    case copy(Value)
    case borrow(Value)
    case mutate(Value)
    case new
  }

  func getLifetimeArguments() -> [LifetimeArgument] {
    /// TODO: when function parameters support a @_resultDependsOn
    /// attribute, add each annotated argument. Only rely on
    /// inferrence rules if no arguments are annotated.
    return inferDependencies()
  }

  func inferDependencies() -> [LifetimeArgument] {
    // A method always infers dependence on self.
    //!!!

    // Infer dependence on the single argument of a function or initializer.
    //!!!

    // Issue a diagnostic if no inferrence is possible.
    //!!!
    return []
  }
}

/// Replace the each dependent apply result with a chain of
/// mark_dependence [nonescaping] instructions; one for each base.
private func insertDependencies(for apply: LifetimeDependentApply,
  _ context: FunctionPassContext ) {
  precondition(apply.applySite.results.count > 0,
    "a lifetime-dependent instruction must have at least one result")

  let callResultBuilder = Builder(after: apply.applySite, context)
  for dependentValue in apply.dependentValues {
    var currentValue = dependentValue
    for base in recursivelyFindDependenceBases(
      of: apply, dependentValue: dependentValue, context) {

      let markDep = callResultBuilder.createMarkDependence(
        value: currentValue, base: base, .nonEscaping)

      currentValue.uses.lazy.filter { $0.instruction != markDep }
      .replaceAll(with: markDep, context)
      
      currentValue = markDep
    }
  }
}

/// Return base values that this return value depends on.
///
/// For lifetime copies, walk up the dependence chain to find the
/// dependence roots, inserting dependencies for any
/// LifetimeDependentApply.
private func recursivelyFindDependenceBases(of apply: LifetimeDependentApply,
  dependentValue: Value, _ context: FunctionPassContext) -> [Value] {
  log("Creating dependencies for \(apply.applySite)")
  var bases: [Value] = []
  for lifetimeArg in apply.getLifetimeArguments() {
    switch lifetimeArg {
    case let .copy(baseValue):
      // Inherit the argument's lifetime dependence by finding the
      // roots.  This requires that a mark_dependence [nonescaping]
      // already be created for any earlier LifetimeDependentApply.
      _ = LifetimeDependence.visitDependenceRoots(enclosing: baseValue,
        context)
      { (scope: LifetimeDependence.Scope) in
        if let updatedScope = recursivelyUpdate(scope: scope, context) {
          log("Copied lifetime from \(baseValue)")
          log("  depends on: \(updatedScope)")
          bases.append(updatedScope.parentValue)
        }
        return .continueWalk
      }
    case let .borrow(baseValue), let .mutate(baseValue):
      // Create a new dependence on the apply's access to the argument.
      if let scope = LifetimeDependence.Scope(base: baseValue, context) {
        log("Borrowed lifetime from \(baseValue)")
        log("  scope: \(scope)")
        bases.append(scope.parentValue)
      }
    case .new:
      // Create a self dependence on the return value.
      bases.append(dependentValue)
    }
  }
  return bases
}

// Recursively insert dependencies, assuming no cycle of dependent applies.
//
// TODO: needs unit test.
private func recursivelyUpdate(scope: LifetimeDependence.Scope,
  _ context: FunctionPassContext) -> LifetimeDependence.Scope? {
  if let dependentApply =
    LifetimeDependentApply(withResult: scope.parentValue) {
    insertDependencies(for: dependentApply, context)
    // If a mark_dependence [nonescaping] was created for this apply,
    // then return it as the updated dependence. Otherwise, return the
    // original dependence.
    if let markDep = scope.parentValue.uses.singleUse?.instruction
      as? MarkDependenceInst {
      return LifetimeDependence(markDependenceInst: markDep, context)?.scope
    }
  }
  return scope
}
