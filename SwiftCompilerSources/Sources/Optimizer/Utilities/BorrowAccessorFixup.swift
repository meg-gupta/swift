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

/// SILGen produces the result of a borrow accessor from within local borrow scopes, and uses
/// `unchecked_ownership` to silence the ownership verifier:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %ex = struct_extract %fwd, #Struct.storedProperty
///   end_borrow %ld
///   return %ex
/// ```
/// Transform this into valid OSSA:
/// ```
///   %ld = load_borrow %self
///   %ex = struct_extract %ld, #Struct.storedProperty
///   return_borrow %ex from_scopes (%ld)
/// ```
/// A trivial result has no enclosing scope, so only the `unchecked_ownership` instructions are removed.
///
/// A `@guaranteed_address` result that is an interior pointer of, or depends on, a borrowed object is handled the same
/// way:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %addr = apply %borrowAccessor(%fwd)
///   end_borrow %ld
///   return %addr
/// ```
/// ->
/// ```
///   %ld = load_borrow %self
///   %addr = apply %borrowAccessor(%ld)
///   return_borrow %addr from_scopes (%ld)
/// ```
///
/// Returns true if the function changed.
func fixupBorrowAccessors(in function: Function, _ context: FunctionPassContext) -> Bool {
  guard function.convention.hasGuaranteedResult || function.convention.hasGuaranteedAddressResult else {
    return false
  }

  var changed = false
  for inst in function.instructions {
    if let uncheckedOwnership = inst as? UncheckedOwnershipInst {
      uncheckedOwnership.replace(with: uncheckedOwnership.operand.value, context)
      changed = true
    }
  }

  // A function that never returns has no result to enclose.
  guard let returnInst = function.returnInstruction as? ReturnInst else {
    return changed
  }

  let enclosingScopes = getEnclosingLocalScopes(of: returnInst.returnedValue, context)
  if enclosingScopes.isEmpty {
    return changed
  }
  for scope in enclosingScopes {
    assert(!scope.scopeEndingOperands.isEmpty)
    for scopeEnd in scope.scopeEndingOperands {
      // SILGen does not emit reborrows, so every scope ends with an end_borrow.
      assert(scopeEnd.instruction is EndBorrowInst)
      context.erase(instruction: scopeEnd.instruction)
    }
  }

  // The scopes are ordered from outer to inner.
  let builder = Builder(before: returnInst, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: enclosingScopes.map { $0.value })
  context.erase(instruction: returnInst)
  return true
}

/// Returns the local borrow scopes that enclose `value`, ordered from outer to inner.
private func getEnclosingLocalScopes(of value: Value, _ context: FunctionPassContext) -> [BeginBorrowValue] {
  if value.type.isAddress {
    var scopes = [BeginBorrowValue]()
    for object in enclosingObjects(of: value) {
      for scope in getEnclosingLocalScopes(of: object, context)
        where !scopes.contains(where: { $0.value == scope.value }) {
        scopes.append(scope)
      }
    }
    return scopes
  }
  var scopes = [BeginBorrowValue]()
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(value)
  while let current = worklist.pop() {
    // A trivial value, such as the dereference_borrow of a trivial type, has no scope.
    if current.ownership == .guaranteed, let scope = BeginBorrowValue(current), scope.hasLocalScope {
      scopes.append(scope)
    }
    worklist.pushIfNotVisited(contentsOf: current.getEnclosingValues(context))
  }
  // Walking up visits inner scopes before outer scopes.
  return scopes.reversed()
}

/// The objects which `address` is only valid within, ordered from outer to inner: the object which it is an interior
/// pointer of, and the bases of its mark_dependence scopes, e.g. the self of an unsafe addressor:
/// ```
///   %ld = load_borrow %selfField
///   %ptr = apply %unsafeAddressor(%ld)
///   %addr = pointer_to_address %ptr
///   %md = mark_dependence [unresolved] %addr on %ld
/// ```
private func enclosingObjects(of address: Value) -> [Value] {
  let accessBaseAndScopes = address.accessBaseWithScopes
  var objects = [Value]()
  if case let .borrowMutateAccessor(apply) = accessBaseAndScopes.base {
    if let selfArgument = apply.selfArgument {
      objects.append(selfArgument)
    }
  } else if let reference = accessBaseAndScopes.base.reference {
    objects.append(reference)
  }
  // The scopes are ordered from inner to outer.
  for case let .dependence(markDep) in accessBaseAndScopes.scopes.reversed() {
    if markDep.base.type.isAddress {
      objects.append(contentsOf: enclosingObjects(of: markDep.base))
    } else {
      objects.append(markDep.base)
    }
  }
  return objects
}

func registerBorrowAccessorFixup() {
  BridgedOptimizerUtilities.registerBorrowAccessorFixup(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) -> Bool in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      return fixupBorrowAccessors(in: bridgedFunction.function, context)
    }
  )
}
