//===--- BorrowUtils.swift - Utilities for borrow scopes ------------------===//
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

/// A scoped instruction that borrows one or more operands.
///
/// If this instruction produces a borrowed value, then
/// BeginBorrowValue(resultOf: self) != nil.
///
/// This does not include instructions like Apply and TryApply that
/// instantaneously borrow a value from the caller.
///
/// Note: This must handle all instructions with a .borrow operand ownership.
///
/// TODO: replace BorrowIntroducingInstruction
///
/// TODO: Add non-escaping MarkDependence.
struct BorrowingInstruction {
  let instruction: Instruction

  init?(_ inst: Instruction) {
    switch inst {
    case is BeginBorrowInst, is StoreBorrowInst, is BeginApplyInst:
      self.instruction = inst
    case let pai as PartialApplyInst where pai.isOnStack:
      self.instruction = inst
    case let builtin as BuiltinInst
      where builtin.id == .StartAsyncLetWithLocalBuffer:
      self.instruction = inst
    default:
      return nil
    }
  } 
  
  public typealias ScopeEndingOperands =
    LazyMapSequence<LazyFilterSequence<LazyMapSequence<UseList, Operand?>>,
    Operand>

  /// The operands that end the local borrow scope.
  ///
  /// Note: When this instruction's result is BeginBorrowValue the
  /// scopeEndingOperand may include reborrows. To find all uses that
  /// contribute to liveness, the caller needs to determine whether an
  /// incoming value dominates or is consumed by an outer adjacent
  /// phi. See InteriorLiveness.
  ///
  /// TODO: to hande reborrow-extended uses migrate ExtendedLiveness
  /// to SwiftCompilerSources.
  ///
  /// TODO: For instructions that are not a BeginBorrowValue, verify
  /// that scope ending instructions exist on all paths. These
  /// instructions should be complete after SILGen and never cloned to
  /// produce phis.
  var scopeEndingOperands: ScopeEndingOperands {
    if let value = instruction as? SingleValueInstruction,
      let beginBorrow = BeginBorrowValue(value) {
      return beginBorrow.scopeEndingOperands
    }
    switch instruction {
    case let sbi as StoreBorrowInst:
      return sbi.uses.lazy.compactMap {
        $0.instruction is EndBorrowInst ? $0 : nil
      }
    case let bai as BeginApplyInst:
      return bai.token.uses.lazy.compactMap {
        $0.instruction is EndApplyInst ? $0 : nil
      }
    case let builtin as BuiltinInst:
      return builtin.uses.lazy.compactMap {
        if let builtinUser = $0.instruction as? BuiltinInst,
          builtinUser.id == .EndAsyncLetLifetime {
          return $0
        }
        return nil
      }
    default:
      fatalError("unknown BorrowingInstruction")
    }
  }
}

/// A value that introduces a borrow scope:
/// begin_borrow, load_borrow, reborrow, guaranteed function argument
///
/// If the value introduces a local scope, then that scope is
/// terminated by end_borrow instructions that directly use this
/// value. Function arguments do not introduce a local scope because
/// the caller owns the scope.
struct BeginBorrowValue {
  let value: Value

  init?(_ value: Value) {
    switch value {
    case is BeginBorrowInst, is LoadBorrowInst:
      break
    case let pai as PartialApplyInst where pai.isOnStack:
      break
    case is FunctionArgument:
      break
    case let arg as Argument where arg.isReborrow:
      break
    default:
      return nil
    }
    self.value = value
  }

  init?(using operand: Operand) {
    switch operand.instruction {
    case is BeginBorrowInst, is LoadBorrowInst:
      self.value = operand.instruction as! SingleValueInstruction
    case is BranchInst:
      guard let phi = Phi(using: operand) else { return nil }
      guard phi.isReborrow else { return nil }
      self.value = phi.value
    default:
      return nil
    }
  }

  init?(resultOf borrowInstruction: BorrowingInstruction) {
    if let value = borrowInstruction.instruction as? SingleValueInstruction,
    let borrow = BeginBorrowValue(value) {
      self = borrow
    }
    return nil
  }

  var hasLocalScope: Bool {
    return !(value is FunctionArgument)
  }

  // non-nil if hasLocalScope is true
  var baseOperand: Operand? {
    switch value {
    case let beginBorrow as BeginBorrowInst:
      return beginBorrow.operand
    case let loadBorrow as LoadBorrowInst:
      return loadBorrow.operand
    default:
      return nil
    }
  }

  public typealias ScopeEndingOperands =
    LazyMapSequence<LazyFilterSequence<LazyMapSequence<UseList, Operand?>>,
    Operand>

  /// The EndBorrows, reborrows (phis), and consumes (of closures)
  /// that end the local borrow scope. Empty if hasLocalScope is false.
  var scopeEndingOperands: ScopeEndingOperands {
    value.uses.lazy.compactMap { $0.endsLifetime ? $0 : nil }
  }
}

/// Find the enclosing borrow introducers for `value`. If `value` is
/// owned, or introduces a borrow scope, then it is the single
/// introducer for itself. Duplicate introducers may be pushed.
func gather(borrowIntroducers: inout Stack<Value>, for value: Value,
  _ context: Context) {

  // Cache introducers across multiple instances of BorrowIntroducers.
  var cache = BorrowIntroducers.Cache()
  BorrowIntroducers.gather(introducers: &borrowIntroducers,
    forValue: value, &cache, context)
}

private struct BorrowIntroducers {
  typealias Cache = Dictionary<HashableValue, Value>
  
  let context: Context
  // BorrowIntroducers instances are recursively nested in order to
  // find outer adjacent phis. Each instance populates a separate
  // 'introducers' set. The same value may occur in 'introducers' at
  // multiple levels. Each instance, therefore, needs a separate
  // introducer set to avoid adding duplicates.
  var visitedIntroducers: Set<HashableValue> = Set()

  static func gather(introducers: inout Stack<Value>, forValue value: Value,
    _ cache: inout Cache, _ context: Context) {
    var borrowIntroducers = BorrowIntroducers(context: context)
    borrowIntroducers.gather(introducers: &introducers, forValue: value, &cache)
  }

  private mutating func push(_ introducer: Value,
    introducers: inout Stack<Value>) {
    if visitedIntroducers.insert(introducer.hashable).inserted {
      introducers.push(introducer)
    }
  }

  private mutating func push<S: Sequence>(contentsOf other: S,
    introducers: inout Stack<Value>) where S.Element == Value {
    for elem in other {
      push(elem, introducers: &introducers)
    }
  }

  // This is the identity function (i.e. just adds `value` to `introducers`)
  // when:
  // - `value` is owned
  // - `value` introduces a borrow scope (begin_borrow, load_borrow, reborrow)
  //
  // Otherwise recurse up the use-def chain to find all introducers.
  private mutating func gather(introducers: inout Stack<Value>,
    forValue value: Value, _ cache: inout Cache) {
    // Check if this value's introducers have already been added to
    // 'introducers' to avoid duplicates and avoid exponential
    // recursion on aggregates.
    if let cachedIntroducer = cache[value.hashable] {
      push(cachedIntroducer, introducers: &introducers)
      return
    }
    switch value.ownership {
    case .none, .unowned:
      return

    case .owned:
      introducers.push(value);
      return

    case .guaranteed:
      break
    }
    // BorrowedValue handles the initial scope introducers: begin_borrow,
    // load_borrow, & reborrow.
    if BeginBorrowValue(value) != nil {
      introducers.push(value)
      return
    }
    // Handle guaranteed forwarding phis
    if let phi = Phi(value) {
      gather(introducers: &introducers, forPhi: phi, &cache)
      return
    }
    // Recurse through guaranteed forwarding non-phi instructions.
    guard let forwardingInst = value.forwardingInstruction else {
      fatalError("guaranteed value must be forwarding")
    }
    for operand in forwardingInst.forwardedOperands {
      if operand.value.ownership == .guaranteed {
        gather(introducers: &introducers, forValue: operand.value, &cache);
      }
    }
  }

  // Find the introducers of a guaranteed forwarding phi's borrow
  // scope. The introducers are either dominating values or reborrows
  // in the same block as the forwarding phi.
  //
  // Recurse along the use-def phi web until a begin_borrow is reached. At each
  // level, find the outer-adjacent phi, if one exists, otherwise return the
  // dominating definition.
  //
  // Example:
  //
  //     one(%reborrow_1 : @guaranteed)
  //         %field = struct_extract %reborrow_1
  //         br two(%reborrow_1, %field)
  //     two(%reborrow_2 : @guaranteed, %forward_2 : @guaranteed)
  //         end_borrow %reborrow_2
  //
  // Calling `recursivelyFindForwardingPhiIntroducers(%forward_2)`
  // recursively computes these introducers:
  //
  //    %field is the only value incoming to %forward_2.
  //
  //    %field is introduced by %reborrow_1 via
  //    recursivelyFindBorrowIntroducers(%field).
  //
  //    %reborrow_1 is introduced by %reborrow_2 in block "two" via
  //    findSuccessorDefsFromPredDefs(%reborrow_1)).
  //
  //    %reborrow_2 is returned.
  //
  private mutating func gather(introducers: inout Stack<Value>, forPhi phi: Phi,
    _ cache: inout Cache) {
    // Phi cycles are skipped. They cannot contribute any new enclosing defs.
    if !visitedIntroducers.insert(phi.value.hashable).inserted {
      return
    }
    for (pred, value) in zip(phi.predecessors, phi.incomingValues) {
      // Each phi operand requires a new introducer list and visited
      // values set. These values will be remapped to successor phis
      // before adding them to the caller's introducer list. It may be
      // necessary to revisit a value that was already visited by the
      // caller before remapping to phis.
      var incomingIntroducers = Stack<Value>(context)
      defer {
        incomingIntroducers.deinitialize()
      }
      BorrowIntroducers.gather(introducers: &incomingIntroducers,
        forValue: value, &cache, context)
      // Map the incoming introducers to an outer-adjacent phi if one exists.
      introducers.append(contentsOf: mapToPhi(predecessor: pred,
          incomingValues: incomingIntroducers))
    }
  }
}

// Given incoming values on a predecessor path, return the
// corresponding values on the successor block. Each incoming value is
// either used by a phi in the successor block, or it must dominate
// the successor block.
private func mapToPhi<PredecessorSequence: Sequence<Value>> (
  predecessor: BasicBlock, incomingValues: PredecessorSequence)
-> LazyMapSequence<PredecessorSequence, Value> {

  guard let successor = predecessor.singleSuccessor else {
    fatalError("phi predecessor must have a single successor in OSSA");
  }
  // Gather the new introducers for the successor block.
  return incomingValues.lazy.map { incomingValue in
    // Find an outer adjacent phi in the successor block.
    for candidatePhi in successor.arguments {
      let candidateValue =
        Phi(candidatePhi)!.incomingOperand(predecessor: predecessor).value
      if candidateValue == incomingValue {
        return candidatePhi
      }
    }
    // No candidates phi are outer-adjacent phis. The incoming
    // `predDef` must dominate the current guaranteed phi.
    return incomingValue
  }
}

/// Find each "enclosing value" whose OSSA lifetime immediately
/// encloses a guaranteed value. Duplicate enclosing values may be
/// pushed.
///
/// The guaranteed `value` being enclosed effectively keeps these
/// enclosing values alive. The meaning of "enclosing" differs
/// depending on whether `value` is itself a borrow introducer vs. a
/// forwarded guaranteed value.  If `value` is a forwarded guaranteed
/// value, then this finds the introducers of the current borrow
/// scope, which is never an empty set. If `value` introduces a borrow
/// scope, then this finds the introducers of the outer enclosing
/// borrow scope that containes this inner scope. This is an empty set
/// if, for example, `value` is owned, is a function argument, or is a
/// load_borrow.
///
/// This handles all combinations of aggregation, guaranteed
/// forwarding phis, and reborrows.
///
/// TODO: for efficiency, turn this into an iterator so we can stop
/// visiting early.
func gather(enclosingValues: inout Stack<Value>,
  for value: Value, _ context: some Context) {

  var gatherValues = EnclosingValues(context)
  defer { gatherValues.deinitialize() }
  var cache = BorrowIntroducers.Cache()
  gatherValues.gather(enclosingValues: &enclosingValues, forValue: value,
    &cache)
}

/// Find inner adjacent phis in the same block as `enclosingPhi`.
/// These keep the enclosing (outer adjacent) phi alive.
func gather(innerAdjacentPhis: inout Stack<Phi>, for enclosingPhi: Phi,
  _ context: Context) {
  for candidatePhi in enclosingPhi.successor.arguments {
    var enclosingValues = Stack<Value>(context)
    defer { enclosingValues.deinitialize() }
    gather(enclosingValues: &enclosingValues, for: candidatePhi, context)
    if enclosingValues.contains(where: { $0 == enclosingPhi.value}) {
      innerAdjacentPhis.push(Phi(candidatePhi)!)
    }
  }
}

// Find the enclosing values for any value, including reborrows.
private struct EnclosingValues {
  var context: Context
  var visitedReborrows : ValueSet

  init(_ context: Context) {
    self.context = context
    self.visitedReborrows = ValueSet(context)
  }

  mutating func deinitialize() {
    visitedReborrows.deinitialize()
  }

  mutating func gather(enclosingValues: inout Stack<Value>,
    forValue value: Value, _ cache: inout BorrowIntroducers.Cache) {
    if value is Undef || value.ownership != .guaranteed {
      return
    }
    var cache = BorrowIntroducers.Cache()
    if BeginBorrowValue(value) != nil {
      switch value {
      case let beginBorrow as BeginBorrowInst:
        // Gather the outer enclosing borrow scope.
        BorrowIntroducers.gather(introducers: &enclosingValues,
          forValue: beginBorrow.operand.value, &cache, context)
      case is LoadBorrowInst, is FunctionArgument:
        // There is no enclosing value on this path.
        break
      default:
        gather(enclosingValues: &enclosingValues, forReborrow: Phi(value)!,
          &cache)
      }
    } else {
      // Handle forwarded guaranteed values.
      BorrowIntroducers.gather(introducers: &enclosingValues, forValue: value,
        &cache, context)
    }
  }
  
  // Given a reborrow, find the enclosing values. Each enclosing value
  // is represented by one of the following cases, which refer to the
  // example below:
  //
  // dominating owned value -> %value encloses %reborrow_1
  // owned outer-adjacent phi -> %phi_3 encloses %reborrow_3
  // dominating outer borrow introducer -> %outerBorrowB encloses %reborrow
  // outer-adjacent reborrow -> %outerReborrow encloses %reborrow
  //
  // Recurse along the use-def phi web until a begin_borrow is
  // reached. Then find all introducers of the begin_borrow's
  // operand. At each level, find the outer adjacent phi, if one
  // exists, otherwise return the most recently found dominating
  // definition.
  //
  // If `reborrow` was already encountered because of a phi cycle,
  // then no enclosingDefs are added.
  //
  // Example:
  //
  //         %value = ...
  //         %borrow = begin_borrow %value
  //         br one(%borrow)
  //     one(%reborrow_1 : @guaranteed)
  //         br two(%value, %reborrow_1)
  //     two(%phi_2 : @owned, %reborrow_2 : @guaranteed)
  //         br three(%value, %reborrow_1)
  //     three(%phi_3 : @owned, %reborrow_3 : @guaranteed)
  //         end_borrow %reborrow_3
  //         destroy_value %phi_3
  //
  // gather(%reborrow_3) finds %phi_3 by computing enclosing defs in this order
  // (inner -> outer):
  //
  //     %reborrow_1 -> %value
  //     %reborrow_2 -> %phi_2
  //     %reborrow_3 -> %phi_3
  //
  // Example:
  //
  //         %outerBorrowA = begin_borrow
  //         %outerBorrowB = begin_borrow
  //         %struct = struct (%outerBorrowA, outerBorrowB)
  //         %borrow = begin_borrow %struct
  //         br one(%outerBorrowA, %borrow)
  //     one(%outerReborrow : @guaranteed, %reborrow : @guaranteed)
  //
  // gather(%reborrow) finds (%outerReborrow, %outerBorrowB).
  //
  private mutating func gather(enclosingValues: inout Stack<Value>,
    forReborrow reborrow: Phi, _ cache: inout BorrowIntroducers.Cache) {

    guard visitedReborrows.insert(reborrow.value) else { return }

    // Find the enclosing introducer for each reborrow operand, and
    // remap it to the enclosing introducer for the successor block.
    for (pred, incomingValue)
    in zip(reborrow.predecessors, reborrow.incomingValues) {
      var incomingEnclosingValues = Stack<Value>(context)
      defer {
        incomingEnclosingValues.deinitialize()
      }
      gather(enclosingValues: &incomingEnclosingValues, forValue: incomingValue,
        &cache)
      enclosingValues.append(contentsOf: mapToPhi(predecessor: pred,
          incomingValues: incomingEnclosingValues))
    }
  }
}

let borrowIntroducersTest = FunctionTest("borrow_introducers") {
  function, arguments, context in
  let value = arguments.takeValue()
  print(function)
  print("Borrow introducers for: \(value)")
  var introducers = Stack<Value>(context)
  defer {
    introducers.deinitialize()
  }
  gather(borrowIntroducers: &introducers, for: value, context)
  introducers.forEach { print($0) }
}

let enclosingValuesTest = FunctionTest("enclosing_values") {
  function, arguments, context in
  let value = arguments.takeValue()
  print(function)
  print("Enclosing values for: \(value)")
  var enclosing = Stack<Value>(context)
  defer {
    enclosing.deinitialize()
  }
  gather(enclosingValues: &enclosing, for: value, context)
  enclosing.forEach { print($0) }
}
