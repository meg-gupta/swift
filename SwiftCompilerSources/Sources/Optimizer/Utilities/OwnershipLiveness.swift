//===--- OwnershipLiveness.swift - Utilities for ownership liveness -------===//
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
//
// TODO: Implement ExtendedLinearLiveness. This requires
// MultiDefPrunedLiveness, which is not supported by InstructionRange.
//
// TODO: Move this all into SIL, along with DominatorTree. OSSA
// lifetimes and dominance are part of SIL semantics, and need to be
// verified. Remove uses of FunctionPassContext.
//
//===----------------------------------------------------------------------===//

import SIL

/// Compute liveness and return a range, which the caller must deinitialize.
///
/// `definingValue` must introduce an OSSA lifetime. It may be either
/// an owned value or introduce a borrowed value (BeginBorrowValue),
/// including:
///
/// 1. Owned non-phi values
/// 2. Owned phi values
/// 3. Borrow scope introducers: begin_borrow/load_borrow
/// 4. Reborrows: guaranteed phi that ends its operand's borrow scope and
///    requires post-dominating scope-ending uses (end_borrow or reborrow)
///
/// `definingValue`'s lifetime must already complete on all paths
/// (a.k.a linear). Only lifetime-ending operations generate liveness.
///
/// `definingValue` dominates the range. Forwarding and phi uses do
/// not extend the lifetime.
///
/// This is the simplest OSSA liveness analysis. It assumes that OSSA
/// lifetime completion has already run on `definingValue`, and it
/// cannot fix OSSA lifetimes after a transformation.
func computeLinearLiveness(for definingValue: Value, _ context: Context)
  -> InstructionRange {

  assert(definingValue.ownership == .owned
    || BeginBorrowValue(definingValue) != nil,
    "value must define an OSSA lifetime")

  // InstructionRange cannot directly represent the beginning of the block
  // so we fake it with getRepresentativeInstruction().
  let defInst =
    InstructionRange.getRepresentativeInstruction(for: definingValue)
  var range = InstructionRange(begin: defInst, context)

  // Compute liveness.
  definingValue.lifetimeEndingUses.forEach { range.insert($0.instruction) }

  return range
}

/// Indicate whether OwnershipUseVisitor is visiting a use of the
/// outer OSSA lifetime or within an inner borrow scope (reborrow).
enum IsInnerLifetime {
case outerLifetime
case innerLifetime
}

typealias InnerScopeHandler = (Value) -> WalkResult

/// Compute liveness and return a range, which rhe caller must deinitialize.
///
/// An OSSA lifetime begins with a single "defining" value, which
/// must be owned, or must begin a borrow scope.
///
/// - Liveness does not extend beyond lifetime-ending operations
/// (a.k.a. affine lifetimes).
///
/// - The definition dominates all use points.
///
/// - Does not assume the current lifetime is complete, but does
/// assume any inner scopes are complete. Use `innerScopeHandler` to
/// complete them or bail-out.
func computeInteriorLiveness(for definingValue: Value,
  _ context: FunctionPassContext,
  _ innerScopeHandler: InnerScopeHandler? = nil) -> InstructionRange {

  assert(definingValue.ownership == .owned
    || BeginBorrowValue(definingValue) != nil,
    "value must define an OSSA lifetime")

  // InstructionRange cannot directly represent the beginning of the block
  // so we fake it with getRepresentativeInstruction().
  let defInst =
    InstructionRange.getRepresentativeInstruction(for: definingValue)
  var range = InstructionRange(begin: defInst, context)

  var visitor = InteriorUseVisitor(definingValue: definingValue, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
  defer { visitor.deinitialize() }
  visitor.innerScopeHandler = innerScopeHandler
  let success = visitor.visitUses()
  assert(success == .continueWalk, "our visitor never fails")
  assert(visitor.unenclosedPhis.isEmpty, "missing adjacent phis")
  return range
}

/// Visit all interior uses of on OSSA lifetime.
///
/// - `definingValue` dominates all uses. Only dominated phis extend
/// the lifetime. All other phis must have a lifetime-ending outer
/// adjacent phi; otherwise they will be recorded as `unenclosedPhis`.
///
/// - Does not assume the current lifetime is linear. Transitively
/// follows guaranteed forwarding and address uses within the current
/// scope. Phis that are not dominanted by definingValue or an outer
/// adjacent phi are marked "unenclosed" to signal an incomplete
/// lifetime.
///
/// - Assumes inner scopes *are* linear, including borrow and address
/// scopes (e.g. begin_borrow, load_borrow, begin_apply, store_borrow,
/// begin_access) A `innerScopeHandler` callback may be used to
/// complete inner scopes before updating liveness.
///
/// InteriorUseVisitor can be used to complete (linearize) an OSSA
/// lifetime after transformation that invalidates OSSA.
///
/// Example:
///
///     %struct = struct ...
///     %f = struct_extract %s     // defines a guaranteed value (%f)
///     %b = begin_borrow %field
///     %a = ref_element_addr %b
///     _  = address_to_pointer %a
///     end_borrow %b              // the only interior use of %f
///
/// When computing interior liveness for %f, %b is an inner
/// scope. Because inner scopes are complete, the only relevant use is
/// end_borrow %b. Despite the address_to_pointer instruction, %f does
/// not escape any dependent address. 
///
/// TODO: Implement the hasPointerEscape flags on BeginBorrowInst,
/// MoveValueInst, and Allocation. Then this visitor should assert
/// that the forward-extended lifetime introducer has no pointer
/// escaping uses.
///
/// TODO: Change the operandOwnership of MarkDependenceInst base operand.
/// It should be a borrowing operand, not a pointer escape.
struct InteriorUseVisitor: OwnershipUseVisitor {
  let context: FunctionPassContext
  var _context: Context { context }

  let function: Function
  let definingValue: Value
  let useVisitor: (Operand) -> WalkResult

  var innerScopeHandler: InnerScopeHandler? = nil

  var unenclosedPhis: [Phi] = []

  private var visited: ValueSet

  mutating func deinitialize() {
    visited.deinitialize()
  }

  init(definingValue: Value, _ context: FunctionPassContext,
    visitor: @escaping (Operand) -> WalkResult) {
    assert(!definingValue.type.isAddress, "address values have no ownership")
    self.context = context
    self.function = definingValue.parentFunction
    self.definingValue = definingValue
    self.useVisitor = visitor
    self.visited = ValueSet(context)
  }

  mutating func visitUses() -> WalkResult {
    visitUsesOfOuter(value: definingValue)
  }

  mutating func visit(use: Operand, _ isInnerLifetime: IsInnerLifetime)
  -> WalkResult {
    useVisitor(use)
  }

  // Visit owned and guaranteed forwarding operands.
  //
  // Guaranteed forwarding operands extend the outer lifetime.
  //
  // Owned forwarding operands end the outer lifetime but extend the
  // inner lifetime (e.g. from a PartialApply or MarkDependence).
  mutating func visitForwarding(operand: Operand,
    _ isInnerLifetime: IsInnerLifetime) -> WalkResult {
    switch operand.value.ownership {
    case .guaranteed:
      assert(isInnerLifetime == .outerLifetime,
        "inner guaranteed forwards are not walked")
      return walkDown(operand: operand)
    case .owned:
      switch isInnerLifetime {
      case .outerLifetime:
        return visit(use: operand, .outerLifetime)
      case .innerLifetime:
        return walkDown(operand: operand)
      }
    default:
      fatalError("forwarded values must have a lifetime")
    }
  }

  // Visit a reborrow operand. This ends an outer lifetime and extends
  // an inner lifetime.
  mutating func visitReborrow(operand: Operand,
    _ isInnerLifetime: IsInnerLifetime) -> WalkResult {
    switch isInnerLifetime {
    case .outerLifetime:
      return visit(use: operand, .outerLifetime)
    case .innerLifetime:
      return walkDown(operand: operand)
    }
  }

  func visitPointerEscape(use: Operand) -> WalkResult {
    fatalError("client should check for pointer escapes first")
  }

  mutating func visitInteriorPointer(use: Operand) -> WalkResult {
    return walkDown(address: use, path: UnusedWalkingPath())
  }

  func handleInner(borrow: BeginBorrowValue) -> WalkResult  {
    guard let innerScopeHandler else { return .continueWalk }
    return innerScopeHandler(borrow.value)
  }

  func handleAccess(address: any BeginAccessInst) -> WalkResult {
    guard let innerScopeHandler else { return .continueWalk }
    return innerScopeHandler(address)
  }
}

// Helpers to walk down forwarding operations.
extension InteriorUseVisitor {
  // Walk down forwarding operands
  private mutating func walkDown(operand: Operand) -> WalkResult {
    // Record all uses
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    if let inst = operand.instruction as? ForwardingInstruction {
      return inst.forwardedResults.walk { walkDownUses(of: $0) }
    }
    if let phi = Phi(using: operand) {
      if phi.value.ownership == .guaranteed {
        return walkDown(guaranteedPhi: phi)
      }
      return walkDownUses(of: phi.value)
    }
    // TODO: verify that ForwardInstruction handles all .forward
    // operand ownership and change this to a fatalError.
    return .continueWalk
  }

  private mutating func walkDownUses(of value: Value) -> WalkResult {
    guard value.ownership.hasLifetime else { return .continueWalk }

    guard visited.insert(value) else { return .continueWalk }

    switch value.ownership {
    case .owned:
      return visitUsesOfInner(value: value)
    case .guaranteed:
      return visitUsesOfOuter(value: value)
    default:
      fatalError("ownership requires a lifetime")
    }
  }

  // Dominating definingValue example: walkDown must continue visiting
  // uses of a reborrow in the inner borrow scope:
  //
  // bb0:
  //  d1 = ...
  //  cond_br bb1, bb2
  // bb1:
  //   b1 = borrow d1
  //   br bb3(b1)
  // bb2:
  //   b2 = borrow d1
  //   br bb3(b2)
  // bb3(reborrow):
  //   u1 = d1
  //   u2 = reborrow
  //   // can't move destroy above u2
  //   destroy_value d1
  //
  // Dominating definingValue example: walkDown must continue visiting
  // uses of a guaranteed phi in the outer lifetime:
  //
  // bb0:
  //  b1 = borrow d1
  //  cond_br bb1, bb2
  // bb1:
  //   p1 = projection b1
  //   br bb3(p1)
  // bb2:
  //   p1 = projection b1
  //   br bb3(p2)
  // bb3(forwardingPhi):
  //   u1 = b1
  //   u2 = forwardingPhi
  //   // can't move end_borrow above u2
  //   end_borrow b1
  private mutating func walkDown(guaranteedPhi: Phi) -> WalkResult {
    guard visited.insert(guaranteedPhi.value) else {
      return .continueWalk
    }
    var enclosingValues = Stack<Value>(context)
    defer { enclosingValues.deinitialize() }
    gather(enclosingValues: &enclosingValues, for: guaranteedPhi.value, context)
    guard enclosingValues.contains(definingValue) else {
      // Since definingValue is not an enclosing value, it must be
      // consumed or reborrowed by some outer adjacent phi in this
      // block. An outer adjacent phi's uses do not contribute to the
      // outer liveness. Instead, guaranteedPhi will be recorded as a
      // regular lifetime-ending use by the visitor.
      return .continueWalk
    }
    // definingValue is not consumed or reborrowed by an outer
    // adjacent phi in guaranteedPhi's block. Therefore this
    // guaranteedPhi's uses contribute to the liveness of
    // definingValue.
    //
    // TODO: instead of relying on Dominance, we can reformulate
    // this algorithm to detect redundant phis, similar to the
    // SSAUpdater.
    if !definingValue.parentBlock.dominates(guaranteedPhi.successor,
      context.dominatorTree) {
      // definingValue does not dominate guaranteedPhi. Record this
      // unenclosed phi so the liveness client can insert the missing
      // outer adjacent phi.
      unenclosedPhis.append(guaranteedPhi);
      return .continueWalk
    }
    // Since definingValue dominates guaranteedPhi, this is a well-formed linear
    // lifetime, and liveness can proceed.
    if guaranteedPhi.isReborrow {
      return visitUsesOfInner(value: guaranteedPhi.value)
    } else {
      return visitUsesOfOuter(value: guaranteedPhi.value)
    }
  }
}

extension InteriorUseVisitor: AddressDefUseWalker {
  mutating func walkDown(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    // OSS lifetime ignores trivial types.
    if address.value.type.objectType.isTrivial(in: function) {
      return .continueWalk
    }
    switch address.instruction {
    case let load as LoadBorrowInst:
      return visitUsesOfInner(value: load)

    case let markDep as MarkDependenceInst:
      if markDep.baseOperand == address,
        LifetimeDependence(markDependenceInst: markDep, context) != nil {
        return walkDownUses(of: markDep)
      }
    default:
      break
    }
    return walkDownDefault(address: address, path: path)
  }

  // Callback from AddressDefUseWalker. Walk down interior pointers.
  //
  // TODO: This must be complete for correctness. Verify that this
  // exhaustively recognizes all uses that could propagate a value out
  // of the addressible memory vs. instantaneous uses.
  mutating func leafUse(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    return visit(use: address, .outerLifetime)
  }
}

/// Enumerate all special cases of ownership uses in a visitor
/// API. This encourages anyone who needs to analyze ownership uses to
/// think about all of the special cases, many of which result
/// from phis of borrowed values. Relying on linear lifetimes, which
/// results from running "lifetime completion", allows you to ignore
/// those cases.
///
/// To visit a value's uses:
///
/// - visitUsesOfOuter(value:)
/// - visitUsesOfInner(value:)
///
/// Visitors need to implement:
///
/// - visit(use:_),
/// - visitForwarding(operand:_)
/// - visitReborrow(operand:_)
/// - visitPointerEscape(use:)
///
/// This only visits the first level of uses. The implementation may
/// transitively visit forwarding operations in its implementation of
/// `visitForwarding(operand:_)` and `visitReborrow(operand:_)`,
/// calling back to `visitUsesOfOuter(value:)` or
/// `visitUsesOfInner(value:)`.
///
/// For uses that begin a borrow or access scope, this skips ahead to
/// the end of the scope. To record incomplete or dead inner scopes
/// (no scope-ending use on some path), the implementation must
/// override handleInner(borrow:).
protocol OwnershipUseVisitor {
  var _context: Context { get }

  /// Visit a non-forwarding use. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// IsInnerLifetime indicates whether `operand` uses the original
  /// OSSA lifetime. This use ends the original lifetime if
  /// (IsInnerLifetime == .outerLifetime && use.endsLifetime).
  mutating func visit(use: Operand, _: IsInnerLifetime) -> WalkResult

  /// Visit a forwarding operand. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// Use ForwardingInstruction or ForwardingDefUseWalker to handle
  /// downstream uses.
  mutating func visitForwarding(operand: Operand, _: IsInnerLifetime)
  -> WalkResult

  /// Visit a reborrow operand. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// Use visitUsesOfInner to visit downstream borrow scopes.
  mutating func visitReborrow(operand: Operand, _: IsInnerLifetime)
  -> WalkResult

  /// Visit a use that propagates its operand to some trivial
  /// value such as an address that depends on the operand's value.
  mutating func visitInteriorPointer(use: Operand) -> WalkResult

  /// Visit a use that escapes information from its operand's value.
  ///
  /// Note: visitPointerEscape may not find all relevant pointer
  /// escapes, such as from owned forwarded values. Clients should
  /// generally check findPointerEscape() before relying on a liveness
  /// result and implement this as a fatalError.
  mutating func visitPointerEscape(use: Operand) -> WalkResult

  /// Handle begin_borrow, load_borrow, store_borrow, begin_apply.
  ///
  /// Handle an inner adjacent phi where the original OSSA def is a
  /// phi in the same block
  ///
  /// Handle a reborrow of an inner borrow scope or inner adjacent phi
  ///
  /// If this returns .continueWalk, then visit(use:) will be called
  /// on the scope ending operands.
  ///
  /// Allows the implementation to complete inner scopes before considering
  /// their scope ending operations as uses of the outer scope.
  mutating func handleInner(borrow: BeginBorrowValue) -> WalkResult

  /// Handle begin_access.
  ///
  /// If this returns .continueWalk, then visit(use:) will be called
  /// on the scope ending operands.
  ///
  /// Allows the implementation to complete inner scopes before considering
  /// their scope ending operations as uses of the outer scope.
  ///
  /// This may add uses to the inner scope, but it may not modify the use-list
  /// containing \p scopedAddress or in any outer scopes.
  mutating func handleAccess(address: any BeginAccessInst) -> WalkResult
}

// Default requirements: Visit everything as a regular use except escapes.
extension OwnershipUseVisitor {
  mutating func handleInner(borrow: BeginBorrowValue) -> WalkResult {
    return .continueWalk
  }

  mutating func handleAccess(address: any BeginAccessInst) -> WalkResult {
    return .continueWalk
  }
}

extension OwnershipUseVisitor {
  /// Visit all uses that contribute to the ownership live
  /// range of `value`. This does not assume that `value` has a
  /// complete lifetime, and non-lifetime-ending uses are visited.
  ///
  /// If `value` is a phi (owned or reborrowed), then find its inner
  /// adjacent phis and treat them like inner borrows.
  ///
  /// This is only called for uses in the outer lifetime.
  mutating func visitUsesOfOuter(value: Value) -> WalkResult {
    // If the outer value is an owned phi or reborrow, consider inner
    // adjacent phis part of its lifetime.
    if let phi = Phi(value), phi.endsLifetime {
      var innerPhis = Stack<Phi>(_context)
      defer { innerPhis.deinitialize() }
      gather(innerAdjacentPhis: &innerPhis, for: phi, _context)
      // Inner adjacent reborrows are considered inner borrow scopes.
      // Inner adjacent guaranteed phis are consider part of the outer lifetime.
      return innerPhis.walk { innerPhi in
        innerPhi.isReborrow ? visitUsesOfInner(value: innerPhi.value)
        : innerPhi.value.uses.walk { visitGuaranteed(use: $0) }
      }
    }
    switch value.ownership {
    case .owned:
      return value.uses.walk { visitOwned(use: $0) }
    case .guaranteed:
      return value.uses.walk { visitGuaranteed(use: $0) }
    case .none, .unowned:
      return .continueWalk
    }
  }

  /// Visit only those uses of an value within an inner borrow scope
  /// that may affect the outer lifetime. An inner borrow scope is one
  /// in which the borrowing operand is itself a use of the outer
  /// lifetime, including: begin_borrow, reborrow, partial_apply,
  /// mark_dependence, or an inner adjacent phi (where original SSA
  /// def is a phi in the same block).
  mutating func visitUsesOfInner(value: Value) -> WalkResult {
    if let beginBorrow = BeginBorrowValue(value) {
      if handleInner(borrow: beginBorrow) == .abortWalk {
        return .abortWalk
      }
      return beginBorrow.scopeEndingOperands.walk {
        visitInnerBorrowScopeEnd(operand: $0);
      }
    }
    // When a borrow introduces an owned value, each OSSA lifetime is
    // effectively a separate borrow scope. A destroy ends the borrow
    // scope, while a forwarding consume effectively "reborrows".
    assert(value.ownership == .owned,
      "inner value must be a reborrow or owned forward")
    return value.lifetimeEndingUses.walk {
      visitInnerBorrowScopeEnd(operand: $0)
    }
  }
}

extension OwnershipUseVisitor {
  // Visit a lifetime-ending use of an inner borrow scope.
  private mutating func visitInnerBorrowScopeEnd(operand: Operand)
  -> WalkResult {
    switch operand.ownership {
    case .endBorrow:
      return visit(use: operand, .innerLifetime)

    case .reborrow:
      if handleInner(borrow: BeginBorrowValue(using: operand)!) == .abortWalk {
        return .abortWalk
      }
      return visitReborrow(operand: operand, .innerLifetime)

    case .forwardingConsume:
      return visitForwarding(operand: operand, .innerLifetime)

    case .destroyingConsume:
      return visit(use: operand, .innerLifetime)

    default:
      fatalError("expected borrow scope end")
    }
  }

  // Visit a borrowing operand (operandOwnerhip == .borrow).
  private mutating func visitInnerBorrow(operand: Operand) -> WalkResult {
    // If a borrowed value is introduced, then handle the inner scope.
    if let beginBorrow = BeginBorrowValue(using: operand) {
      return visitUsesOfInner(value: beginBorrow.value)
    }
    // Otherwise, directly visit the scope ending uses.
    let borrowInst = BorrowingInstruction(operand.instruction)!
    return borrowInst.scopeEndingOperands.walk {
      visit(use: $0, .innerLifetime)
    }
  }

  // This is only called for uses in the outer lifetime.
  private mutating func visitOwned(use: Operand) -> WalkResult {
    switch use.ownership {
    case .nonUse:
      return .continueWalk

    case .destroyingConsume:
      return visit(use: use, .outerLifetime)

    case .forwardingConsume:
      return visitForwarding(operand: use, .outerLifetime)

    case .pointerEscape:
      return visitPointerEscape(use: use)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse,
      .bitwiseEscape:
      return visit(use: use, .outerLifetime)

    case .borrow:
      return visitInnerBorrow(operand: use)

    // TODO: Eventually, visit owned InteriorPointers as implicit borrows.
    case .interiorPointer, .trivialUse, .endBorrow, .reborrow,
      .guaranteedForwarding:
      fatalError("ownership incompatible with an owned value");
    }
  }

  // This is only called for uses in the outer lifetime.
  private mutating func visitGuaranteed(use: Operand)
  -> WalkResult {
    switch use.ownership {
    case .nonUse:
      return .continueWalk

    case .pointerEscape:
      return visitPointerEscape(use: use)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse,
      .bitwiseEscape:
      return visit(use: use, .outerLifetime)

    case .endBorrow:
      return visit(use: use, .outerLifetime)

    case .reborrow:
      return visitReborrow(operand: use, .outerLifetime)

    case .guaranteedForwarding:
      return visitForwarding(operand: use, .outerLifetime)

    case .borrow:
      return visitInnerBorrow(operand: use)

    case .interiorPointer:
      return visitInteriorPointer(use: use)

    case .trivialUse, .forwardingConsume, .destroyingConsume:
      fatalError("ownership incompatible with a guaranteed value")
    }
  }
}

/// Cache the liveness boundary by taking a snapshot of its InstructionRange.
struct LivenessBoundary {
  var lastUsers : Stack<Instruction>
  var boundaryEdges : Stack<BasicBlock>
  var deadDefs : Stack<Instruction>

  // Compute the boundary of a singly-defined range.
  init(range: InstructionRange, _ context: Context) {
    assert(range.isValid)

    lastUsers = Stack<Instruction>(context)
    boundaryEdges = Stack<BasicBlock>(context)
    deadDefs = Stack<Instruction>(context)

    lastUsers.append(contentsOf: range.ends)
    boundaryEdges.append(contentsOf: range.exitBlocks)
    // If range had no users, then it's `begin` instruction is
    // recorded as the single `end` instruction.
    if lastUsers.first == range.begin {
      deadDefs.push(lastUsers.pop()!)
      assert(lastUsers.isEmpty && boundaryEdges.isEmpty)
    }
  }

  var description: String {
    (lastUsers.map { "last user: \($0.description)" }
    + boundaryEdges.map { "boundary edge: \($0.description)" }
    + deadDefs.map { "dead def: \($0.description)" }).joined(separator: "\n")
  }

  mutating func deinitialize() {
    lastUsers.deinitialize()
    boundaryEdges.deinitialize()
    deadDefs.deinitialize()
  }
}

let linearLivenessTest = FunctionTest("linear_liveness_swift") {
  function, arguments, context in
  let value = arguments.takeValue()
  print(function)
  print("Linear liveness: \(value)")
  let range = computeLinearLiveness(for: value, context)
  print(range)
  print(LivenessBoundary(range: range, context))
}

let interiorLivenessTest = FunctionTest("interior_liveness_swift") {
  function, arguments, context in
  let value = arguments.takeValue()
  print(function)
  print("Interior liveness: \(value)")
  let range = computeInteriorLiveness(for: value, context)
  print(range)
  print(LivenessBoundary(range: range, context))
}
