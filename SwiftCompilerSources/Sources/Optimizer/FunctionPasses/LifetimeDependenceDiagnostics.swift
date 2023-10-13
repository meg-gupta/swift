//===--- LifetimeDependenceDiagnostics.swift - Lifetime dependence --------===//
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

import SIL

// DiagnosticEngine already declares DiagID. Why isn't it visible?
//!!!import ASTBridging
//!!!public typealias DiagID = BridgedDiagID

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

/// Diagnostic pass.
///
/// Find the roots of all non-escapable values in this function. All
/// non-escapable values either depend on a NonEscapingScope, or they
/// are produced by a LifetimeDependentInstruction that has no
/// dependence on a parent value (@_unsafeNonEscapableResult).
let lifetimeDependenceDiagnosticsPass = FunctionPass(
  name: "lifetime-dependence-diagnostics")
{ (function: Function, context: FunctionPassContext) in
  log("Diagnosing lifetime dependence in \(function.name)")
  
  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInst else { continue }
    if let lifetimeDep =
      LifetimeDependence(markDependenceInst: markDep, context) {
      analyze(dependence: lifetimeDep, context)
    }
  }
}

/// Analyze a single Lifetime dependence and trigger diagnostics.
///
/// 1. Compute the LifetimeDependence scope.
///
/// 2. Walk down all dependent values checking that they are within range.
private func analyze(dependence: LifetimeDependence,
  _ context: FunctionPassContext) {
  log("Diagnosing lifetime dependence: \(dependence)")
    
  // Compute this dependence scope.
  guard var range = dependence.scope.computeRange(context) else { return }
  defer { range.deinitialize() }

  let diagnostics =
  DiagnoseDependence(dependence: dependence, range: range, context: context)

  // Check each lifetime-dependent use via a def-use visitor
  var walker = DiagnoseDependenceWalker(diagnostics, context)
  defer { walker.deinitialize() }
  let result = walker.walkDown(root: dependence.parentValue)
  // TODO: print SIL-level diagnostics wherever we do .abortWalk
  assert(result == .continueWalk,
    "unimplemented lifetime dependence diagnostic")
}

/// Analyze and diagnose a single LifetimeDependence.
private struct DiagnoseDependence {
  let dependence: LifetimeDependence
  let range: InstructionRange
  let context: FunctionPassContext

  func check(operand: Operand) {
    // First, check that this use is inside the dependence scope.
    if !range.inclusiveRangeContains(operand.instruction) {
      reportError(operand: operand, diagID: .lifetime_outside_scope_use)
      return
    }
    log("Range contains: \(operand.instruction)")
    // Then, check that the use cannot propagate the value. The user
    // must have a known effect on the operand's lifetime.
    if !DiagnoseDependence.hasKnownLifetime(operand) {
      reportError(operand: operand, diagID: .lifetime_outside_scope_escape)
    }
  }

  static func hasKnownLifetime(_ operand: Operand) -> Bool {
    switch operand.ownership {
    case .nonUse, .borrow, .reborrow, .guaranteedForwarding:
      fatalError("DiagnoseDependenceWalker should bypass: \(operand)")
    case .trivialUse, .instantaneousUse, .unownedInstantaneousUse, .endBorrow:
      return true
    case .forwardingUnowned, .pointerEscape, .bitwiseEscape, .forwardingConsume,
      .interiorPointer:
      // TODO: handle all interiorPointers with a standard ownership
      // def-use walker so we never reach here.
      //
      // TODO: fix forwardingConsume to only refer to instructions
      // that produce forwarded value in the current scope so we never
      // reach here (ForwardingDefUseWalker will bypass them).
      if let returnInst = operand.instruction as? ReturnInst {
        return returnInst.parentFunction.hasUnsafeNonEscapableResult
      }
      return false
    case .destroyingConsume:
      switch operand.instruction {
      case is DeallocBoxInst, is DeallocExistentialBoxInst, is DeallocRefInst,
        is DestroyValueInst, is EndLifetimeInst:
        return true
      default:
        return false
      }
    }
  }

  func reportError(operand: Operand, diagID: DiagID) {
    // Identify the escaping variable.
    let escapingVar = LifetimeVariable(dependent: operand.value, context)
    let varName = escapingVar.name
    if let varName {
      context.diagnosticEngine.diagnose(escapingVar.sourceLoc,
        .lifetime_variable_outside_scope,
        varName)
    } else {
      context.diagnosticEngine.diagnose(escapingVar.sourceLoc,
        .lifetime_value_outside_scope)
    }
    // Identify the dependence scope.
    //
    // TODO: add bridging for function argument locations
    // [SILArgument.getDecl().getLoc()]
    //
    // TODO: For clear diagnostics: switch on dependence.scope.
    // For an access, report both the accessed variable, and the access.
    let parentSourceLoc =
      dependence.parentValue.definingInstruction?.location.sourceLoc
    context.diagnosticEngine.diagnose(parentSourceLoc,
      .lifetime_outside_scope_parent)
    
    // Identify the use point.
    let userSourceLoc = operand.instruction.location.sourceLoc
    context.diagnosticEngine.diagnose(userSourceLoc, diagID)
  }
}

private extension Instruction {
  func findVarDecl() -> VarDecl? {
    if let varDeclInst = self as? VarDeclInstruction {
      return varDeclInst.varDecl
    }
    for result in results {
      for use in result.uses {
        if let debugVal = use.instruction as? DebugValueInst {
          return debugVal.varDecl
        }
      }
    }
    return nil
  }
}

// Identify a best-effort variable declaration based on a defining SIL
// value or any lifetime dependent use of that SIL value.
private struct LifetimeVariable {
  var varDecl: VarDecl?
  var sourceLoc: SourceLoc?
  
  var name: String? {
    return varDecl?.userFacingName
  }

  init(introducer: Value) {
    if introducer.type.isAddress {
      switch introducer.enclosingAccessScope {
      case let .scope(beginAccess):
        // TODO: report both the access point and original variable.
        self = LifetimeVariable(introducer: beginAccess.operand.value)
        return
      case .base(_):
        // TODO: use an address walker to get the allocation point.
        break
      }
    }
    // TODO: handle function arguments
    self.sourceLoc = introducer.definingInstruction?.location.sourceLoc
    self.varDecl = introducer.definingInstruction?.findVarDecl()
    if let varDecl {
      sourceLoc = varDecl.sourceLoc
    }
  }

  init(dependent value: Value, _ context: Context) {
    // TODO: consider diagnosing multiple variable introducers. It's
    // unclear how more than one can happen.
    var introducers = Stack<Value>(context)
    gather(borrowIntroducers: &introducers, for: value, context)
    if let firstIntroducer = introducers.pop() {
      self = LifetimeVariable(introducer: firstIntroducer)
      return
    }
    self.varDecl = nil
    self.sourceLoc = nil
  }
}

/// Extends LifetimeWalker for dependent values and
/// AddressDefUseWalker for dependent addresses. The walk starts with
/// add address for .access dependencies. The walk can transition from
/// an address to a value at a load. The walk can transition from a
/// value to an address as follows:
///
///     %dependent_addr = mark_dependence [nonescaping] %base_addr on %value
///
/// TODO: handle stores to singly initialized temporaries like copies using a standard reaching-def analysis.
private struct DiagnoseDependenceWalker : LifetimeDependenceDefUseWalker,
  AddressDefUseWalker {
  let diagnostics: DiagnoseDependence
  let context: Context
  var visitedValues: ValueSet

  var function: Function { diagnostics.dependence.function }
  
  init(_ diagnostics: DiagnoseDependence, _ context: Context) {
    self.diagnostics = diagnostics
    self.context = context
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    visitedValues.deinitialize()
  }

  // Returns .abortWalk if either the closure aborts or the address walker fails to recognize an address use. In the future, the address walker will be guaranteed to succeed, and this confusion will be avoided.
  mutating func walkDownUses(of value: Value, using operand: Operand?)
  -> WalkResult {
    if value.type.isTrivial(in: function) {
      return .continueWalk
    }
    if value.type.isAddress {
      // Delegate to AddressDefUseWalker.
      return walkDownUses(ofAddress: value, path: UnusedWalkingPath())
    }
    // Delegate to ForwardingDefUseWalker.
    return walkDownUsesDefault(forwarding: value, using: operand)
  }

  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
  -> WalkResult {
    // Ignore a dead root value. It never escapes.
    if let operand {
      diagnostics.check(operand: operand)
    }
    return .continueWalk
  }

  mutating func leafUse(_ operand: Operand) -> WalkResult {
    diagnostics.check(operand: operand)
    return .continueWalk
  }
}

extension DiagnoseDependenceWalker {
  // Callback from AddressDefUseWalker. Walk down interior pointers.
  //
  // TODO: This must be complete for correctness. Never abort the
  // walk. Replace this with a standard address def-use walker that
  // exhaustively recognizes all uses that could propagate a value out
  // of the addressible memory vs. instantaneous uses. This is needed
  // for correctness. In C++, this is the TransitiveAddressWalker. The
  // only custom logic here should be the mark_dependence and
  // store/copy_addr checks.
  mutating func walkDown(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    // Lifetime dependence ignores trivial types.
    if address.value.type.objectType.isTrivial(in: function) {
      return .continueWalk
    }
    switch address.instruction {
    // TODO: the real address def-use walker will handle all loads
    // as a single callback.
    case let load as LoadInst:
      return walkDownUses(of: load, using: address)

    case let load as LoadBorrowInst:
      return walkDownUses(of: load, using: address)

    case let markDep as MarkDependenceInst:
      if markDep.baseOperand == address
      && LifetimeDependence(markDependenceInst: markDep, context) != nil {
        return walkDownUses(of: markDep, using: address)
      }
    default:
      break
    }
    return walkDownDefault(address: address, path: path)
  }

  // Callback from AddressDefUseWalker. Walk down interior pointers.
  mutating func leafUse(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    // stores delegate to the implementation's general leafUse().
    switch address.instruction {
    case let copy as CopyAddrInst:
      // Ignore copies to this address, just like stores.
      //
      // TODO: handle copies from this address by performing a
      // standard reaching-def analysis on the destination address and
      // continuing the walk at the loads for which this is the
      // reaching-def.
      return address == copy.sourceOperand ? .abortWalk : leafUse(address)

      // TODO: the real address def-use walker will handle all kinds
      // of stores as a single callback.
    case is StoreInst:
      return leafUse(address)

    case let apply as FullApplySite:
      if let callerArgIdx = apply.argumentIndex(of: address) {
        let calleeArgIdx = apply.calleeArgIndex(callerArgIndex: callerArgIdx)
        let convention = apply.getArgumentConvention(calleeArgIndex: calleeArgIdx)
        if convention.isIndirectOut {
          return leafUse(address)
        }
      }
      return .abortWalk

    default:
      return .abortWalk
    }
  }
}
