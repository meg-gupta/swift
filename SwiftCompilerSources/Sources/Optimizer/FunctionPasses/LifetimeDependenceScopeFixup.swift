//===--- LifetimeDependenceScopeFixup.swift ----------------------------===//
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


private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

let lifetimeDependenceScopeFixupPass = FunctionPass(
  name: "lifetime-dependence-scope-fixup")
{ (function: Function, context: FunctionPassContext) in
  log("Scope fixup for lifetime dependence in \(function.name)")
  
  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInst else { continue }
    if let lifetimeDep =
      LifetimeDependence(markDependenceInst: markDep, context) {
      fixup(dependence: lifetimeDep, context)
    }
  }
}

private func fixup(dependence: LifetimeDependence,
  _ context: FunctionPassContext) {
  log("Scope fixup for lifetime dependent instructions: \(dependence)")
  guard case .access(let bai) = dependence.scope else {return}

  var range = InstructionRange(begin: bai, context)
  var walker = LifetimeDependenceScopeFixupWalker(bai.parentFunction, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
    
  defer {walker.deinitialize()}
  _ = walker.walkDown(root: dependence.parentValue)
  defer {range.deinitialize()}

  let endAcceses = bai.endInstructions
  for eai in endAcceses {
    context.erase(instruction: eai)
  }
    
  for end in range.ends {
    let endBuilder = Builder(after: end, context)
    _ = endBuilder.createEndAccess(beginAccess: bai)
  }
}

private struct LifetimeDependenceScopeFixupWalker : LifetimeDependenceDefUseWalker,
  AddressDefUseWalker {
  let function: Function
  let context: Context
  let visitor: (Operand) -> WalkResult
  var visitedValues: ValueSet
  
  init(_ function: Function, _ context: Context, visitor: @escaping (Operand) -> WalkResult) {
    self.function = function
    self.context = context
    self.visitor = visitor
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    visitedValues.deinitialize()
  }

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
      return visitor(operand)
    }
    return .continueWalk
  }

  mutating func leafUse(_ operand: Operand) -> WalkResult {
    return visitor(operand)
  }
}

extension LifetimeDependenceScopeFixupWalker {
  mutating func walkDown(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    switch address.instruction {
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
    switch address.instruction {
    case let copy as CopyAddrInst:
      return address == copy.sourceOperand ? .abortWalk : leafUse(address)

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
