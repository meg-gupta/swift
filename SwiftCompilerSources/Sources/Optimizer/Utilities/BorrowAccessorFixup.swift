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
/// The address result of an accessor with a borrowed object self is handled the same way. The address result is a
/// scope within the borrow scope of self:
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
///   return_borrow %addr from_scopes (%ld, %addr)
/// ```
///
/// Returns true if the function changed.
func fixupBorrowAccessors(in function: Function, _ context: FunctionPassContext) -> Bool {
  guard function.convention.hasGuaranteedResult || function.convention.hasAddressResult else {
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
    let scopeEnds = scopeEndingInstructions(of: scope)
    assert(!scopeEnds.isEmpty)
    for scopeEnd in scopeEnds {
      // SILGen does not emit reborrows, so every scope ends with an end_borrow.
      assert(scopeEnd is EndBorrowInst)
      context.erase(instruction: scopeEnd)
    }
  }

  // The scopes are ordered from outer to inner.
  let builder = Builder(before: returnInst, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: enclosingScopes)
  context.erase(instruction: returnInst)
  return true
}

/// The scope ending instructions of a borrow scope or of the address result of an accessor.
private func scopeEndingInstructions(of scope: Value) -> [Instruction] {
  if let apply = scope as? ApplyInst, apply.functionConvention.hasAddressResult {
    return apply.addressResultScopeEndingOperands.map { $0.instruction }
  }
  return BeginBorrowValue(scope)!.scopeEndingOperands.map { $0.instruction }
}

/// Returns the local scopes that enclose `value`, ordered from outer to inner: borrow scopes and the
/// address results of borrow and mutate accessors.
private func getEnclosingLocalScopes(of value: Value, _ context: FunctionPassContext) -> [Value] {
  var scopes = [Value]()
  collectEnclosingLocalScopes(of: value, &scopes, context)
  // Walking up visits inner scopes before outer scopes.
  return scopes.reversed()
}

private func collectEnclosingLocalScopes(of value: Value, _ scopes: inout [Value],
                                         _ context: FunctionPassContext) {
  if value.type.isAddress {
    collectEnclosingLocalScopes(ofAddress: value, &scopes, context)
    return
  }
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(value)
  while let current = worklist.pop() {
    // With opaque values, the address result of an accessor is an object until AddressLowering.
    if isOpaqueAddressResult(current) {
      scopes.append(current)
      continue
    }
    // A trivial value, such as the dereference_borrow of a trivial type, has no scope.
    if current.ownership == .guaranteed, let scope = BeginBorrowValue(current), scope.hasLocalScope {
      scopes.append(current)
      if let loadBorrow = current as? LoadBorrowInst {
        collectEnclosingLocalScopes(ofAddress: loadBorrow.address, &scopes, context)
      }
      if let apply = current as? ApplyInst, let selfAddress = apply.selfAddressOfGuaranteedBorrowAccessorResult {
        collectEnclosingLocalScopes(ofAddress: selfAddress, &scopes, context)
      }
    }
    worklist.pushIfNotVisited(contentsOf: current.getEnclosingValues(context))
  }
}

/// Collects the local scopes that `address` is only valid within: the accessor address results that it is
/// derived from, and the local scopes of the objects that it depends on, e.g. the self of an unsafe addressor:
/// ```
///   %ld = load_borrow %selfField
///   %ptr = apply %unsafeAddressor(%ld)
///   %addr = pointer_to_address %ptr
///   %md = mark_dependence [unresolved] %addr on %ld
/// ```
private func collectEnclosingLocalScopes(ofAddress address: Value, _ scopes: inout [Value],
                                         _ context: FunctionPassContext) {
  let accessBaseAndScopes = address.accessBaseWithScopes
  // The scopes are ordered from inner to outer.
  for case let .dependence(markDep) in accessBaseAndScopes.scopes {
    collectEnclosingLocalScopes(of: markDep.base, &scopes, context)
  }
  switch accessBaseAndScopes.base {
  case let .borrowMutateAccessor(apply):
    scopes.append(apply)
    if let selfArgument = apply.selfArgument {
      collectEnclosingLocalScopes(of: selfArgument, &scopes, context)
    }
  case let .box(projectBox):
    collectEnclosingLocalScopes(of: projectBox.box, &scopes, context)
  case let .class(refElementAddr):
    collectEnclosingLocalScopes(of: refElementAddr.instance, &scopes, context)
  case let .tail(refTailAddr):
    collectEnclosingLocalScopes(of: refTailAddr.instance, &scopes, context)
  case .stack, .global, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
    break
  }
}

/// With opaque values, the @guaranteed_address result of an accessor is an object.
private func isOpaqueAddressResult(_ value: Value) -> Bool {
  guard let apply = value as? ApplyInst else {
    return false
  }
  return apply.functionConvention.hasAddressResult && !apply.type.isAddress
}

func registerBorrowAccessorFixup() {
  BridgedOptimizerUtilities.registerBorrowAccessorFixup(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) -> Bool in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      return fixupBorrowAccessors(in: bridgedFunction.function, context)
    }
  )
}
