//===--- OpaqueBorrowInsertion.swift --------------------------------------===//
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

/// Introduces a `begin_opaque_borrow` scope for the `@in_guaranteed` self argument of calls with a guaranteed result,
/// e.g. borrow accessors:
///
/// ```
///   %r = apply %accessor(%self_addr) : $(@in_guaranteed Container) -> @guaranteed Klass
///   // uses of %r
/// ```
/// ->
/// ```
///   %b = begin_opaque_borrow %self_addr
///   %r = apply %accessor(%b) : $(@in_guaranteed Container) -> @guaranteed Klass
///   // uses of %r
///   end_borrow %b
/// ```
///
/// Without this scope, the guaranteed result has no OSSA borrow introducer because self is an address.
///
/// The scope ends at the end of the result's liverange. If the result flows into guaranteed phis, the opaque borrow is
/// reborrowed into the phi blocks. If the result is returned, the `return` becomes a `return_borrow` with the opaque
/// borrow as enclosing value.
///
/// Calls whose scope cannot be constructed are left unchanged.
let opaqueBorrowInsertion = FunctionPass(name: "opaque-borrow-insertion") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }
  let candidates = Array(function.instructions.compactMap { inst -> ApplyInst? in
    guard let apply = inst as? ApplyInst, apply.needsOpaqueBorrow else {
      return nil
    }
    return apply
  })
  if candidates.isEmpty {
    return
  }

  var changed = false
  for group in OpaqueBorrowGroup.computeGroups(of: candidates, context) {
    if group.insertOpaqueBorrows(context) {
      changed = true
    }
  }
  if changed {
    updateGuaranteedPhis(in: function, context)
  }
}

private extension ApplyInst {
  var needsOpaqueBorrow: Bool {
    guard hasGuaranteedResult, ownership == .guaranteed,
          let selfOperand = selfOperand,
          selfOperand.value.type.isAddress,
          convention(of: selfOperand) == .indirectInGuaranteed else {
      return false
    }
    return true
  }

  var selfOperand: Operand? {
    functionConvention.hasSelfParameter ? argumentOperands.last : nil
  }
}

/// The transitive uses of a guaranteed value within its borrow scope.
private struct ScopeUses {
  /// All users which need to be covered by the borrow scope.
  var users: [Instruction] = []
  /// The value itself and all values which are transitively forwarded from it, excluding phis.
  var forwardedValues: [Value] = []
  /// Guaranteed phis which the value is forwarded to.
  var phis: [Phi] = []
  /// False if the uses cannot be handled.
  var isValid = true

  init(of root: Value, _ context: FunctionPassContext) {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(root)

    while let value = worklist.pop() {
      forwardedValues.append(value)
      for use in value.uses {
        visit(use: use, &worklist, context)
      }
    }
  }

  private mutating func visit(use: Operand, _ worklist: inout ValueWorklist, _ context: FunctionPassContext) {
    let inst = use.instruction
    if let bfi = inst as? BorrowedFromInst {
      // The borrowed-from of a guaranteed phi forwards the phi.
      if use.index == 0 {
        worklist.pushIfNotVisited(bfi)
      }
      return
    }
    if inst is ReturnInstruction {
      users.append(inst)
      return
    }
    switch use.ownership {
    case .nonUse:
      return
    case .guaranteedForwarding:
      users.append(inst)
      if let phi = Phi(using: use) {
        if !phis.contains(where: { $0.value == phi.value }) {
          phis.append(phi)
        }
      } else if let forwardingInst = inst as? ForwardingInstruction {
        for result in forwardingInst.forwardedResults where result.ownership == .guaranteed {
          worklist.pushIfNotVisited(result)
        }
      } else if let apply = inst as? ApplyInst, apply.ownership == .guaranteed {
        // A call with a guaranteed result forwards its guaranteed self argument.
        worklist.pushIfNotVisited(apply)
      } else {
        isValid = false
      }
    case .borrow:
      users.append(inst)
      guard let borrowingInst = BorrowingInstruction(inst) else {
        isValid = false
        return
      }
      var scopeEnds = Stack<Instruction>(context)
      defer { scopeEnds.deinitialize() }
      let result = borrowingInst.visitScopeEndingOperands(context) { (scopeEnd: Operand) in
        if scopeEnd.instruction is BranchInst {
          // Inner reborrows are not supported.
          return .abortWalk
        }
        scopeEnds.push(scopeEnd.instruction)
        return .continueWalk
      }
      if result == .abortWalk {
        isValid = false
      }
      users.append(contentsOf: scopeEnds)
    case .endBorrow, .reborrow, .destroyingConsume, .forwardingConsume:
      isValid = false
    default:
      users.append(inst)
    }
  }
}

/// The scope of a guaranteed value: either the result of a call or a guaranteed phi.
private struct ScopeRoot {
  let value: Value
  let uses: ScopeUses

  /// The first instruction of the scope.
  var begin: Instruction {
    if let inst = value.definingInstruction {
      return inst
    }
    return value.parentBlock.instructions.first!
  }

  func computeRange(_ context: FunctionPassContext) -> InstructionRange {
    var range = InstructionRange(begin: begin, context)
    range.insert(begin)
    range.insert(contentsOf: uses.users)
    return range
  }
}

/// Calls whose results are forwarded to the same guaranteed phis. Their opaque borrows must be reborrowed together.
private struct OpaqueBorrowGroup {
  var applies: [ApplyInst] = []
  var roots: [ScopeRoot] = []

  static func computeGroups(of candidates: [ApplyInst], _ context: FunctionPassContext) -> [OpaqueBorrowGroup] {
    // For each candidate, compute the scope roots: its result and all transitively reached guaranteed phis.
    var rootsOfCandidate = [[ScopeRoot]]()
    for apply in candidates {
      var roots = [ScopeRoot(value: apply, uses: ScopeUses(of: apply, context))]
      var idx = 0
      while idx < roots.count {
        for phi in roots[idx].uses.phis where !roots.contains(where: { $0.value == phi.value }) {
          roots.append(ScopeRoot(value: phi.value, uses: ScopeUses(of: phi.value, context)))
        }
        idx += 1
      }
      rootsOfCandidate.append(roots)
    }

    // Merge candidates which share a phi.
    var groupOf = Array(0..<candidates.count)
    func find(_ i: Int) -> Int {
      var i = i
      while groupOf[i] != i {
        i = groupOf[i]
      }
      return i
    }
    for i in 0..<candidates.count {
      for j in (i + 1)..<candidates.count where find(i) != find(j) {
        let sharesPhi = rootsOfCandidate[i].dropFirst().contains { rootI in
          rootsOfCandidate[j].dropFirst().contains { $0.value == rootI.value }
        }
        if sharesPhi {
          groupOf[find(j)] = find(i)
        }
      }
    }

    var groups = [Int: OpaqueBorrowGroup]()
    var order = [Int]()
    for (i, apply) in candidates.enumerated() {
      let g = find(i)
      if groups[g] == nil {
        groups[g] = OpaqueBorrowGroup()
        order.append(g)
      }
      groups[g]!.applies.append(apply)
      for root in rootsOfCandidate[i] where !groups[g]!.roots.contains(where: { $0.value == root.value }) {
        groups[g]!.roots.append(root)
      }
    }
    return order.map { groups[$0]! }
  }

  var hasPhis: Bool { roots.count > applies.count }

  /// Returns false if the opaque borrow scopes of this group cannot be constructed.
  func isSupported(_ context: FunctionPassContext) -> Bool {
    if roots.contains(where: { !$0.uses.isValid }) {
      return false
    }
    if !hasPhis {
      return true
    }
    // All opaque borrows are merged by the same reborrow phis.
    let selfType = applies[0].selfOperand!.value.type
    if applies.contains(where: { $0.selfOperand!.value.type != selfType }) {
      return false
    }
    // There is a single available opaque borrow per block.
    var blocks = BasicBlockSet(context)
    defer { blocks.deinitialize() }
    for apply in applies where !blocks.insert(apply.parentBlock) {
      return false
    }
    // All incoming values of the phis are forwarded from the group's calls, so that the phis can be enclosed by
    // reborrows of the opaque borrows.
    var forwardedValues = ValueSet(context)
    defer { forwardedValues.deinitialize() }
    for root in roots {
      for value in root.uses.forwardedValues {
        forwardedValues.insert(value)
      }
    }
    for root in roots.dropFirst(applies.count) {
      let phi = Phi(root.value)!
      if phi.incomingValues.contains(where: { !forwardedValues.contains($0) }) {
        return false
      }
    }
    // The scopes of the roots don't overlap with each other.
    for root in roots {
      var range = root.computeRange(context)
      defer { range.deinitialize() }
      for other in roots where other.value != root.value {
        if range.inclusiveRangeContains(other.begin) ||
           other.uses.users.contains(where: { range.inclusiveRangeContains($0) && !($0 is BranchInst) }) {
          return false
        }
      }
    }
    return true
  }

  /// Returns true if the opaque borrows were inserted.
  func insertOpaqueBorrows(_ context: FunctionPassContext) -> Bool {
    if !isSupported(context) {
      return false
    }
    // Check that all scope ends can be created before changing anything.
    for root in roots {
      var range = root.computeRange(context)
      defer { range.deinitialize() }
      if range.exitBlocks.contains(where: { $0.singlePredecessor == nil }) {
        return false
      }
      if range.ends.contains(where: { $0 is TermInst && !($0 is BranchInst) && !($0 is ReturnInstruction) }) {
        return false
      }
    }

    let selfType = applies[0].selfOperand!.value.type.objectType
    var ssaUpdater = SSAUpdater(type: selfType, ownership: .guaranteed, context)
    defer { ssaUpdater.deinitialize() }

    var opaqueBorrows = [BeginOpaqueBorrowInst]()
    for apply in applies {
      let selfOperand = apply.selfOperand!
      let builder = Builder(before: apply, context)
      let opaqueBorrow = builder.createBeginOpaqueBorrow(fromAddress: selfOperand.value)
      selfOperand.set(to: opaqueBorrow, context)
      ssaUpdater.addAvailableValue(opaqueBorrow, in: apply.parentBlock)
      opaqueBorrows.append(opaqueBorrow)
    }

    /// The opaque borrow, or a reborrow of it, which is live at `inst`.
    func scope(at inst: Instruction) -> Value {
      if let opaqueBorrow = opaqueBorrows.first(where: { $0.parentBlock == inst.parentBlock }) {
        return opaqueBorrow
      }
      return ssaUpdater.getValue(atBeginOf: inst.parentBlock)
    }

    let phiValues = roots.dropFirst(applies.count).map { $0.value }
    for root in roots {
      var range = root.computeRange(context)
      defer { range.deinitialize() }

      func isForwarded(_ value: Value) -> Bool {
        root.uses.forwardedValues.contains(where: { $0 == value })
      }
      for end in Array(range.ends) {
        switch end {
        case let branch as BranchInst:
          // The scope continues in the reborrow phi of the successor.
          let reachesPhi = branch.operands.contains { operand in
            let arg = branch.getArgument(for: operand)
            return phiValues.contains(where: { $0 == arg })
          }
          if !reachesPhi {
            Builder(before: branch, context).createEndBorrow(of: scope(at: branch))
          }
        case let ret as ReturnInst where isForwarded(ret.returnedValue):
          // The scope ends when the caller ends the borrow of the returned value.
          let builder = Builder(before: ret, context)
          builder.createReturnBorrow(of: ret.returnedValue, enclosingValues: [scope(at: ret)])
          context.erase(instruction: ret)
        case let returnBorrow as ReturnBorrowInst where isForwarded(returnBorrow.returnedValue):
          let builder = Builder(before: returnBorrow, context)
          builder.createReturnBorrow(of: returnBorrow.returnedValue,
                                     enclosingValues: Array(returnBorrow.enclosingValues) + [scope(at: returnBorrow)])
          context.erase(instruction: returnBorrow)
        case is TermInst:
          Builder(before: end, context).createEndBorrow(of: scope(at: end))
        default:
          Builder(after: end, context).createEndBorrow(of: scope(at: end))
        }
      }
      for exit in Array(range.exits) {
        Builder(before: exit, context).createEndBorrow(of: scope(at: exit))
      }
    }
    return true
  }
}
