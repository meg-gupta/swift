// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature BuiltinModule -enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

/* -Xllvm -enable-lifetime-dependence-diagnostics */

import Builtin

let c = 10

struct A: ~Copyable {
  let p = UnsafeMutablePointer<Int>.allocate(capacity: c)

  init() {
    let b = UnsafeMutableBufferPointer(start: p, count: c)
    let i = b.initialize(fromContentsOf: 0..<c)
    assert(i == c)
  }

  deinit {
    p.deinitialize(count: c)
    p.deallocate()
  }
}

extension A: ContiguousStorageSpan {
  typealias Element = Int

  var storage: Span<Int> {
    borrowing get {
      .init(unsafePointer: p, count: c, owner: self)
    }
  }
}



































































//===--- Span.swift ------------------------------------------------===//
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

#if hasFeature(NonescapableTypes)

// A Span<Element> represents a span of memory which
// contains initialized instances of `Element`.
@frozen
public struct Span<Element: ~Copyable & ~Escapable>: Copyable, ~Escapable {
  @usableFromInline let _start: Index
  @usableFromInline let _count: Int

  @inlinable @inline(__always)
  internal init<Owner: ~Copyable & ~Escapable>(
    _unchecked start: Index,
    count: Int,
    owner: borrowing Owner
  ) -> _borrow(owner) Self {
    self._start = start
    self._count = count
    return self
  }
}

@available(*, unavailable)
extension Span: Sendable {}

extension Span where Element: ~Copyable & ~Escapable {

  @inlinable @inline(__always)
  internal init<Owner: ~Copyable & ~Escapable>(
    start: Index,
    count: Int,
    owner: borrowing Owner
  ) -> _borrow(owner) Self {
    precondition(count >= 0, "Count must not be negative")
    precondition(
      start.isAligned,
      "baseAddress must be properly aligned for accessing \(Element.self)"
    )
    self.init(_unchecked: start, count: count, owner: owner)
    return self
  }

  public init<Owner: ~Copyable & ~Escapable>(
    unsafeBufferPointer buffer: UnsafeBufferPointer<Element>,
    owner: borrowing Owner
  ) -> _borrow(owner) Self {
    guard let baseAddress = buffer.baseAddress else {
      fatalError("Span requires a non-nil base address")
    }
    self.init(unsafePointer: baseAddress, count: buffer.count, owner: owner)
    return self
  }

  public init<Owner: ~Copyable & ~Escapable>(
    unsafePointer: UnsafePointer<Element>,
    count: Int,
    owner: borrowing Owner
  ) -> _borrow(owner) Self {
    let start = Index(_rawStart: unsafePointer)
    self.init(start: start, count: count, owner: owner)
    return self
  }
}

#if hasFeature(BitwiseCopyable)
extension Span where Element: _BitwiseCopyable {

  @inlinable
  internal init<Owner: ~Copyable & ~Escapable>(
    start index: Index,
    count: Int,
    owner: borrowing Owner
  ) /* -> borrow(owner) Self */ {
    precondition(count >= 0, "Count must not be negative")
    self._start = index
    self._count = count
  }

  public init<Owner: ~Copyable & ~Escapable>(
    unsafeBytes buffer: UnsafeRawBufferPointer,
    as type: Element.Type,
    owner: borrowing Owner
  ) /* -> borrow(owner) Self */ {
    guard let baseAddress = buffer.baseAddress else {
      fatalError("Span requires a non-nil base address")
    }
    let (c, s) = (buffer.count, MemoryLayout<Element>.stride)
    let (q, r) = c.quotientAndRemainder(dividingBy: s)
    precondition(r == 0)
    self.init(
      unsafeRawPointer: baseAddress, as: Element.self, count: q, owner: owner
    )
  }

  public init<Owner: ~Copyable & ~Escapable>(
    unsafeRawPointer: UnsafeRawPointer,
    as type: Element.Type,
    count: Int,
    owner: borrowing Owner
  ) /* -> borrow(owner) Self */ {
    let start = Index(_rawStart: unsafeRawPointer)
    self.init(start: start, count: count, owner: owner)
  }
}
#endif

//MARK: Sequence

extension Span/*: Sequence*/ where Element: Copyable & Escapable {

  public var iterator: Iterator {
    borrowing _read { yield .init(owner: self) }
  }
}

extension Span where Element: Equatable {

  public func elementsEqual(_ other: Self) -> Bool {
    guard count == other.count else { return false }
    if count == 0 { return true }
    if startIndex == other.startIndex { return true }

    //FIXME: This could be short-cut
    //       with a layout constraint where stride equals size,
    //       as long as there is at most 1 unused bit pattern.
    // if Element is BitwiseRepresentable {
    // return _swift_stdlib_memcmp(lhs.baseAddress, rhs.baseAddress, count) == 0
    // }
    for o in 0..<count {
      if self[uncheckedOffset: o] != other[uncheckedOffset: o] { return false }
    }
    return true
  }

  @inlinable
  public func elementsEqual(_ other: some Collection<Element>) -> Bool {
    guard count == other.count else { return false }
    if count == 0 { return true }

    return elementsEqual(AnySequence(other))
  }

  @inlinable
  public func elementsEqual(_ other: some Sequence<Element>) -> Bool {
    for (index, otherElement) in zip(indices, other) {
      if self[unchecked: index] != otherElement { return false }
    }
    return true
  }
}

//MARK: Index Validation, Bounds Checking
extension Span where Element: ~Copyable & ~Escapable {

  @inlinable @inline(__always)
  func boundsCheckPrecondition(_ position: Index) {
    precondition(
      position.isAligned,
      "Index is not properly aligned for accessing Element"
    )
    precondition(
      startIndex._allocation == position._allocation &&
      distance(from: startIndex, to: position) >= 0 &&
      distance(from: position, to: endIndex) > 0,
      "Index out of bounds"
    )
  }

  @inlinable @inline(__always)
  func boundsCheckPrecondition(_ bounds: Range<Index>) {
    precondition(
      bounds.lowerBound.isAligned && bounds.upperBound.isAligned,
      "Range of indices is not properly aligned for accessing Element"
    )
    precondition(
      startIndex._allocation == bounds.lowerBound._allocation &&
      startIndex._allocation == bounds.upperBound._allocation &&
      distance(from: startIndex, to: bounds.lowerBound) >= 0 &&
      distance(from: bounds.lowerBound, to: bounds.upperBound) >= 0 &&
      distance(from: bounds.upperBound, to: endIndex) >= 0,
      "Range of indices out of bounds"
    )
  }
}

#if hasFeature(BitwiseCopyable)
extension Span where Element: _BitwiseCopyable {
  @inlinable @inline(__always)
  func boundsCheckPrecondition(_ position: Index) {
    precondition(
      startIndex._allocation == position._allocation &&
      distance(from: startIndex, to: position) >= 0 &&
      distance(from: position, to: endIndex) > 0,
      "Index out of bounds"
    )
  }

  @inlinable @inline(__always)
  func boundsCheckPrecondition(_ bounds: Range<Index>) {
    precondition(
      startIndex._allocation == bounds.lowerBound._allocation &&
      startIndex._allocation == bounds.upperBound._allocation &&
      startIndex._rawValue.distance(to: bounds.lowerBound._rawValue) >= 0 &&
      bounds.lowerBound._rawValue.distance(to: bounds.upperBound._rawValue) >= 0 &&
      bounds.upperBound._rawValue.distance(to: endIndex._rawValue) >= 0,
      "Range of indices out of bounds"
    )
  }
}
#endif

//MARK: Collection typealiases
extension Span where Element: ~Copyable & ~Escapable {
  public typealias Element = Element
  public typealias SubSequence = Self
}

//MARK: Index Manipulation
extension Span where Element: ~Copyable & ~Escapable {
  //  Collection,
  //  BidirectionalCollection,
  //  RandomAccessCollection

  @inlinable @inline(__always)
  public var startIndex: Index { _start }

  @inlinable @inline(__always)
  public var endIndex: Index { _start.advanced(by: _count) }

  @inlinable @inline(__always)
  public var count: Int {
    borrowing get { self._count }
  }

  @inlinable @inline(__always)
  public var indices: Range<Index> {
    .init(uncheckedBounds: (startIndex, endIndex))
  }

  @inlinable @inline(__always)
  public var isEmpty: Bool { count == 0 }

  @inlinable @inline(__always)
  public func index(after i: Index) -> Index {
    i.advanced(by: +1)
  }

  @inlinable @inline(__always)
  public func index(before i: Index) -> Index {
    i.advanced(by: -1)
  }

  @inlinable @inline(__always)
  public func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable @inline(__always)
  public func formIndex(before i: inout Index) {
    i = index(before: i)
  }

  @inlinable @inline(__always)
  public func index(_ i: Index, offsetBy distance: Int) -> Index {
    i.advanced(by: distance)
  }

  @inlinable @inline(__always)
  public func formIndex(_ i: inout Index, offsetBy distance: Int) {
    i = index(i, offsetBy: distance)
  }

  @inlinable @inline(__always)
  public func distance(from start: Index, to end: Index) -> Int {
    start.distance(to: end)
  }
}

//MARK: Index-based Subscripts
extension Span where Element: ~Copyable {
  //  Collection,
  //  BidirectionalCollection,
  //  RandomAccessCollection

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(position: Index) -> Element {
    _read {
      boundsCheckPrecondition(position)
      yield self[unchecked: position]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(unchecked position: Index) -> Element {
    _read {
      let binding = Builtin.bindMemory(
        position._rawValue._rawValue, 1._builtinWordValue, Element.self
      )
      defer { Builtin.rebindMemory(position._rawValue._rawValue, binding) }
      yield UnsafePointer<Element>(position._rawValue._rawValue).pointee
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(bounds: Range<Index>) -> Self {
    get {
      boundsCheckPrecondition(bounds)
      return self[unchecked: bounds]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(unchecked bounds: Range<Index>) -> Self {
    _read {
      yield Span(
        start: bounds.lowerBound,
        count: bounds.count,
        owner: self
      )
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(bounds: some RangeExpression<Index>) -> Self {
    _read {
      yield self[bounds.relative(to: indices)]
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(unchecked bounds: some RangeExpression<Index>) -> Self {
    _read {
      yield self[unchecked: bounds.relative(to: indices)]
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(x: UnboundedRange) -> Self {
    _read {
      yield self[unchecked: indices]
    }
  }
}

#if hasFeature(BitwiseCopyable)
extension Span where Element: _BitwiseCopyable {

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(position: Index) -> Element {
    get {
      boundsCheckPrecondition(position)
      return self[unchecked: position]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(unchecked position: Index) -> Element {
    get {
      position._rawValue.loadUnaligned(as: Element.self)
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(bounds: Range<Index>) -> Self {
    _read {
      boundsCheckPrecondition(bounds)
      yield self[unchecked: bounds]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(unchecked bounds: Range<Index>) -> Self {
    _read {
      yield Span(
        start: bounds.lowerBound,
        count: bounds.count,
        owner: self
      )
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(bounds: some RangeExpression<Index>) -> Self {
    _read {
      yield self[bounds.relative(to: indices)]
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(unchecked bounds: some RangeExpression<Index>) -> Self {
    _read {
      yield self[unchecked: bounds.relative(to: indices)]
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(x: UnboundedRange) -> Self {
    _read {
      yield self[unchecked: indices]
    }
  }
}
#endif

//MARK: integer offset subscripts
extension Span where Element: ~Copyable {

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(offset offset: Int) -> Element {
    _read {
      precondition(0 <= offset && offset < count)
      yield self[uncheckedOffset: offset]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(uncheckedOffset offset: Int) -> Element {
    _read {
      yield self[unchecked: index(startIndex, offsetBy: offset)]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(offsets: Range<Int>) -> Self {
    _read {
      precondition(0 <= offsets.lowerBound && offsets.upperBound <= count)
      yield self[uncheckedOffsets: offsets]
    }
  }

  //FIXME: lifetime-dependent on self
  @inlinable @inline(__always)
  public subscript(uncheckedOffsets offsets: Range<Int>) -> Self {
    _read {
      yield Span(
        start: index(startIndex, offsetBy: offsets.lowerBound),
        count: offsets.count,
        owner: self
      )
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(offsets: some RangeExpression<Int>) -> Self {
    _read {
      yield self[offsets.relative(to: 0..<count)]
    }
  }

  //FIXME: lifetime-dependent on self
  @_alwaysEmitIntoClient
  public subscript(uncheckedOffsets offsets: some RangeExpression<Int>) -> Self {
    _read {
      yield self[uncheckedOffsets: offsets.relative(to: 0..<count)]
    }
  }
}

//MARK: withUnsafeRaw...
#if hasFeature(BitwiseCopyable)
extension Span where Element: _BitwiseCopyable {

  //FIXME: mark closure parameter as non-escaping
  public func withUnsafeBytes<R>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let rawBuffer = UnsafeRawBufferPointer(
      start: count==0 ? nil : _start._rawValue,
      count: count*MemoryLayout<Element>.stride
    )
    return try body(rawBuffer)
  }
}
#endif

//MARK: withUnsafePointer, etc.
extension Span where Element: ~Copyable {

  //FIXME: mark closure parameter as non-escaping
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    try _start._rawValue.withMemoryRebound(to: Element.self, capacity: count) {
      try body(.init(start: $0, count: count))
    }
  }
// This should work:
//  borrowing public func withUnsafeBufferPointer<
//    Result: ~Copyable /*& ~Escapable*/,
//    E: Error
//  >(
//    _ body: (UnsafeBufferPointer<Element>) throws(E) -> /*_borrow(0)*/ Result
//  ) throws(E) -> /*_borrow(self)*/ Result {
//    try _start._rawValue.withMemoryRebound(to: Element.self, capacity: count) {
//      (pointer: UnsafePointer<Element>) throws(E) -> Result in
//      try body(.init(start: pointer, count: count))
//    }
//  }

  //FIXME: mark closure parameter as non-escaping
  public func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    try withUnsafeBufferPointer(body)
  }
}

//MARK: load
#if hasFeature(BitwiseCopyable)
extension Span where Element: _BitwiseCopyable {

  public func load<T>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    boundsCheckPrecondition(
      Range(uncheckedBounds: (
        .init(
          allocation: startIndex._allocation,
          rawValue: _start._rawValue.advanced(by: offset)
        ),
        .init(
          allocation: startIndex._allocation,
          rawValue: _start._rawValue.advanced(by: offset+MemoryLayout<T>.size)
        )
      ))
    )
    return _start._rawValue.load(fromByteOffset: offset, as: T.self)
  }

  public func load<T>(from index: Index, as: T.Type) -> T {
    let o = distance(from: startIndex, to: index)*MemoryLayout<Element>.stride
    return load(fromByteOffset: o, as: T.self)
  }

  public func loadUnaligned<T: _BitwiseCopyable>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    boundsCheckPrecondition(
      Range(uncheckedBounds: (
        .init(
          allocation: startIndex._allocation,
          rawValue: _start._rawValue.advanced(by: offset)
        ),
        .init(
          allocation: startIndex._allocation,
          rawValue: _start._rawValue.advanced(by: offset+MemoryLayout<T>.size)
        )
      ))
    )
    return _start._rawValue.loadUnaligned(fromByteOffset: offset, as: T.self)
  }

  public func loadUnaligned<T: _BitwiseCopyable>(
    from index: Index, as: T.Type
  ) -> T {
    let o = distance(from: startIndex, to: index)*MemoryLayout<Element>.stride
    return loadUnaligned(fromByteOffset: o, as: T.self)
  }

  //FIXME: lifetime-dependent on self
  /// View the memory span represented by this view as a different type
  ///
  /// The memory must be laid out identically to the in-memory representation
  /// of `T`. The memory span must be over a whole number of instances of `T`.
  ///
  /// - Parameters:
  ///   - type: The type you wish to view the memory as
  /// - Returns: A new `Span` over elements of type `T`
  borrowing public func view<T: _BitwiseCopyable>(
    as: T.Type
  ) -> _borrow(self) Span<T> {
    let bc = count*MemoryLayout<Element>.stride
    let (nc, rem) = bc.quotientAndRemainder(dividingBy: MemoryLayout<T>.stride)
    precondition(rem == 0)
    let start = Span<T>.Index(
      allocation: startIndex._allocation,
      rawValue: startIndex._rawValue
    )
    return Span<T>(_unchecked: start, count: nc, owner: self)
  }
}
#endif

extension Span where Element: Copyable {
  @inlinable
  public var first: Element? {
    isEmpty ? nil : self[unchecked: startIndex]
  }

  @inlinable
  public var last: Element? {
    isEmpty ? nil : self[unchecked: index(startIndex, offsetBy: count &- 1)]
  }
}

//MARK: one-sided slicing operations
extension Span where Element: ~Copyable {

  borrowing public func prefix(upTo index: Index) -> _borrow(self) Self {
    index == startIndex
    ? Self(_unchecked: _start, count: 0, owner: self)
    : prefix(through: index.advanced(by: -1))
  }

  borrowing public func prefix(through index: Index) -> _borrow(self) Self {
    boundsCheckPrecondition(index)
    let nc = distance(from: startIndex, to: index) &+ 1
    return Self(_unchecked: _start, count: nc, owner: self)
  }

  borrowing public func prefix(_ maxLength: Int) -> _borrow(self) Self {
    precondition(maxLength >= 0, "Can't have a prefix of negative length.")
    let nc = maxLength < count ? maxLength : count
    return Self(_unchecked: _start, count: nc, owner: self)
  }

  borrowing public func dropLast(_ k: Int = 1) -> _borrow(self) Self {
    precondition(k >= 0, "Can't drop a negative number of elements.")
    let nc = k < count ? count&-k : 0
    return Self(_unchecked: _start, count: nc, owner: self)
  }

  borrowing public func suffix(from index: Index) -> _borrow(self) Self {
    if index == endIndex {
      return Self(_unchecked: index, count: 0, owner: self )
    }
    boundsCheckPrecondition(index)
    let nc = distance(from: index, to: endIndex)
    return Self(_unchecked: index, count: nc, owner: self)
  }

  borrowing public func suffix(_ maxLength: Int) -> _borrow(self) Self {
    precondition(maxLength >= 0, "Can't have a suffix of negative length.")
    let nc = maxLength < count ? maxLength : count
    let newStart = _start.advanced(by: count&-nc)
    return Self(_unchecked: newStart, count: nc, owner: self)
  }

  borrowing public func dropFirst(_ k: Int = 1) -> _borrow(self) Self {
    precondition(k >= 0, "Can't drop a negative number of elements.")
    let dc = k < count ? k : count
    let newStart = _start.advanced(by: dc)
    return Self(_unchecked: newStart, count: count&-dc, owner: self)
  }
}

#endif
























//===--- StorageViewIndex.swift -------------------------------------------===//
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

#if hasFeature(NonescapableTypes)

extension Span where Element: ~Copyable & ~Escapable {
  @frozen
  public struct Index {
    @usableFromInline let _allocation: UnsafeRawPointer
    @usableFromInline let _rawValue: UnsafeRawPointer

    @inlinable @inline(__always)
    internal init(allocation: UnsafeRawPointer, rawValue: UnsafeRawPointer) {
      (_allocation, _rawValue) = (allocation, rawValue)
    }

    @inlinable @inline(__always)
    internal init(_rawStart: UnsafeRawPointer) {
      self.init(allocation: _rawStart, rawValue: _rawStart)
    }
  }
}

extension Span.Index where Element: ~Copyable & ~Escapable {

  @inlinable @inline(__always)
  var isAligned: Bool {
    (Int(bitPattern: _rawValue) & (MemoryLayout<Element>.alignment-1)) == 0
  }
}

@available(*, unavailable)
extension Span.Index: Sendable {}

extension Span.Index: Equatable where Element: ~Copyable & ~Escapable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    // note: if we don't define this function, then `Strideable` will define it.
    (lhs._allocation == rhs._allocation) && (lhs._rawValue == rhs._rawValue)
  }
}

extension Span.Index: Hashable where Element: ~Copyable & ~Escapable {}

extension Span.Index: Strideable where Element: ~Copyable & ~Escapable {
  public typealias Stride = Int

  @inlinable @inline(__always)
  public func distance(to other: Self) -> Int {
    precondition(_allocation == other._allocation)
    let bytes = _rawValue.distance(to: other._rawValue)
    let (q, r) = bytes.quotientAndRemainder(dividingBy: MemoryLayout<Element>.stride)
    precondition(r == 0)
    return q
  }

  @inlinable @inline(__always)
  public func advanced(by n: Int) -> Self {
    .init(
      allocation: _allocation,
      rawValue: _rawValue.advanced(by: n &* MemoryLayout<Element>.stride)
    )
  }
}

extension Span.Index: Comparable where Element: ~Copyable & ~Escapable {
  @inlinable @inline(__always)
  public static func <(lhs: Self, rhs: Self) -> Bool {
    return lhs._rawValue < rhs._rawValue
  }
}

#endif



















//===--- StorageViewIterator.swift ----------------------------------------===//
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

#if hasFeature(NonescapableTypes)

extension Span where Element: ~Copyable & ~Escapable {
  @frozen
  public struct Iterator: Copyable, ~Escapable {
    var curPointer: UnsafeRawPointer
    let endPointer: UnsafeRawPointer

    init(owner: borrowing Span<Element>) -> _borrow(owner) Self {
      self.curPointer = owner.startIndex._rawValue
      self.endPointer = owner.endIndex._rawValue
      return self
    }
  }
}

extension Span.Iterator where Element: Copyable & Escapable {

  // This is the `IteratorProtocol` requirement, except that
  // Span.Iterator does not conform to `Escapable`
  public mutating func next() -> Element? {
    guard curPointer < endPointer else { return nil }
    defer {
      curPointer = curPointer.advanced(by: MemoryLayout<Element>.stride)
    }
    if _isPOD(Element.self) {
      return curPointer.loadUnaligned(as: Element.self)
    }
    return curPointer.load(as: Element.self)
  }
}

extension Span.Iterator where Element: _BitwiseCopyable {

  // This is the `IteratorProtocol` requirement, except that
  // Span.Iterator does not conform to `Escapable`
  public mutating func next() -> Element? {
    guard curPointer < endPointer else { return nil }
    defer {
      curPointer = curPointer.advanced(by: MemoryLayout<Element>.stride)
    }
    return curPointer.loadUnaligned(as: Element.self)
  }
}

#endif










































//===--- ContiguousStorageSpan.swift ------------------------------------------===//
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

#if hasFeature(NonescapableTypes)

public protocol ContiguousStorageSpan<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable & ~Escapable

  var storage: Span<Element> { borrowing get }
}

extension Span: ContiguousStorageSpan where Element: ~Copyable & ~Escapable {
  public var storage: Self { self }
}

extension Array: ContiguousStorageSpan {
  public var storage: Span<Element> {
    borrowing _read {
      if let a = _baseAddressIfContiguous {
        yield Span(
          unsafePointer: a, count: count, owner: self
        )
      }
      else {
        let a = ContiguousArray(copy self)
        yield a.storage
      }
    }
  }
}

extension ContiguousArray: ContiguousStorageSpan {
  public var storage: Span<Element> {
    borrowing _read {
      yield Span(
        unsafePointer: _baseAddressIfContiguous!, count: count, owner: self
      )
    }
  }
}

#endif
