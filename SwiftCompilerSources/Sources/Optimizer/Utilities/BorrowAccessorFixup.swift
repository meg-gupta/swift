//===--- BorrowAccessorFixup.swift ----------------------------------------===//
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

import SIL
import OptimizerBridging

/// SILGen may produce a borrow accessor result from within a local borrow scope. Such as:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %ex = struct_extract %fwd, #Struct.storedProperty
///   end_borrow %ld
///   return %ex
/// ```
/// This is illegal OSSA, since the return uses a value outside its borrow scope. Transform this into valid OSSA:
/// ```
///   %ld = load_borrow %self
///   %ex = struct_extract %ld, #Struct.storedProperty
///   return_borrow %ex from_scopes (%ld)
/// ```
/// The local borrow scopes which enclose the returned value are found transitively with `getEnclosingValues`.
/// This includes the scopes of the borrowed arguments of a borrow accessor result.
///
/// A borrow accessor result has no `end_borrow` in SILGen output. It is added to the `return_borrow`'s scopes
/// later, by `fixupBorrowAccessorResultScopes`, since its liveness ends at the `return_borrow`:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %r = apply %otherBorrowAccessor(%fwd)
///   end_borrow %ld
///   return %r
/// ```
/// ->
/// ```
///   %ld = load_borrow %self
///   %r = apply %otherBorrowAccessor(%ld)
///   return_borrow %r from_scopes (%r, %ld)
/// ```
///
/// Returns true if the function was changed.
func fixupBorrowAccessors(in function: Function, _ context: FunctionPassContext) -> Bool {
  guard function.convention.hasGuaranteedResult,
        let returnInst = function.returnInstruction as? ReturnInst,
        returnInst.returnedValue.ownership == .guaranteed
  else {
    return false
  }

  var enclosingScopes = [Value]()
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(returnInst.returnedValue)
  while let value = worklist.pop() {
    if let beginBorrow = BeginBorrowValue(value) {
      guard beginBorrow.hasLocalScope else {
        continue
      }
      enclosingScopes.append(value)
    }
    worklist.pushIfNotVisited(contentsOf: value.getEnclosingValues(context))
  }

  // For all the local borrow scopes that enclose the return value, delete their end_borrow instructions and use
  // them as an enclosing value of the return_borrow instruction.
  var returnBorrowScopes = [Value]()
  var hasGuaranteedApplyResult = false
  for scope in enclosingScopes {
    let endBorrows = Array(scope.uses.users(ofType: EndBorrowInst.self))
    if !endBorrows.isEmpty {
      returnBorrowScopes.append(scope)
    }
    for endBorrow in endBorrows {
      context.erase(instruction: endBorrow)
    }
    // A borrow accessor result has no end_borrow yet. It is added to the return_borrow's scopes when its lifetime is
    // completed.
    if case .borrowAccessorResult = BeginBorrowValue(scope)! {
      hasGuaranteedApplyResult = true
    }
    for uncheckedOwnership in Array(scope.uses.users(ofType: UncheckedOwnershipInst.self)) {
      uncheckedOwnership.replace(with: scope, context)
    }
  }

  if returnBorrowScopes.isEmpty && !hasGuaranteedApplyResult {
    return false
  }

  let builder = Builder(before: returnInst, location: returnInst.location, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, enclosingValues: returnBorrowScopes)
  context.erase(instruction: returnInst)
  return true
}

/// Complete the borrow scopes of borrow accessor results.
///
/// SILGen does not emit `end_borrow`s for the borrowed `@guaranteed` result of a borrow accessor apply. Instead, it
/// routes the result's uses through an `unchecked_ownership` instruction. Remove the `unchecked_ownership`s and end
/// each borrow scope at the boundary of its liveness:
/// ```
///   %r = apply %borrowAccessor(%x)
///   %u = unchecked_ownership %r
///   %c = copy_value %u
/// ```
/// ->
/// ```
///   %r = apply %borrowAccessor(%x)
///   %c = copy_value %r
///   end_borrow %r
/// ```
/// If the boundary is a `return_borrow`, the borrow accessor result is added to its enclosing values instead:
/// ```
///   return_borrow %v from_scopes (%r)
/// ```
///
/// Returns true if the function was changed.
func fixupBorrowAccessorResultScopes(in function: Function, _ context: FunctionPassContext) -> Bool {
  guard function.hasOwnership else {
    return false
  }

  let borrowAccessorApplies = function.instructions.compactMap {
    ($0 as? ApplyInst).flatMap { $0.hasBorrowedResult ? $0 : nil }
  }
  if borrowAccessorApplies.isEmpty {
    return false
  }

  for apply in borrowAccessorApplies {
    for uncheckedOwnership in Array(apply.uses.users(ofType: UncheckedOwnershipInst.self)) {
      uncheckedOwnership.replace(with: apply, context)
    }
  }

  var returnBorrow: ReturnBorrowInst? = nil
  var returnBorrowScopes: [Value] = []

  // Process inner scopes, such as nested borrow accessor results, first. The liveness of an outer scope includes the
  // inner uses, so its end_borrows are inserted after the inner ones.
  for apply in borrowAccessorApplies.reversed() {
    var range = computeKnownLiveness(for: apply, visitInnerUses: true, context)
    defer { range.deinitialize() }

    for end in range.ends {
      if let rbi = end as? ReturnBorrowInst {
        assert(returnBorrow == nil || returnBorrow == rbi, "expected a single return_borrow")
        returnBorrow = rbi
        returnBorrowScopes.append(apply)
        continue
      }
      if end.operands.contains(where: { $0.value == apply && $0.endsLifetime }) {
        // The borrow scope already ends here, e.g. at a reborrow.
        continue
      }
      Builder.insert(after: end, location: end.location.asAutoGenerated, context) {
        $0.createEndBorrow(of: apply)
      }
    }
    for exit in range.exits {
      Builder(before: exit, location: exit.location.asAutoGenerated, context).createEndBorrow(of: apply)
    }
  }

  if let returnBorrow {
    var enclosingValues = returnBorrowScopes
    for enclosingValue in returnBorrow.enclosingValues where !enclosingValues.contains(where: { $0 == enclosingValue }) {
      enclosingValues.append(enclosingValue)
    }
    let builder = Builder(before: returnBorrow, location: returnBorrow.location, context)
    builder.createReturnBorrow(of: returnBorrow.returnedValue, enclosingValues: enclosingValues)
    context.erase(instruction: returnBorrow)
  }
  return true
}

func registerBorrowAccessorFixup() {
  BridgedOptimizerUtilities.registerBorrowAccessorFixup(
    // fixupBorrowAccessors
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) -> Bool in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      return fixupBorrowAccessors(in: bridgedFunction.function, context)
    },
    // fixupBorrowAccessorResultScopes
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) -> Bool in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      return fixupBorrowAccessorResultScopes(in: bridgedFunction.function, context)
    }
  )
}
