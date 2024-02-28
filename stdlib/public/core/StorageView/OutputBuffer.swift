#if hasFeature(NonescapableTypes)

public struct OutputBuffer<Element: ~Copyable/* & ~Escapable*/>: ~Copyable, ~Escapable {
  let start: UnsafeMutablePointer<Element>
  public let capacity: Int
  public internal(set) var initialized: Int = 0

  deinit {
    // `self` always borrows memory, and it shouldn't have gotten here.
    // Failing to use `relinquishBorrowedMemory()` is an error.
    if initialized > 0 {
      fatalError()
    }
  }

  // precondition: pointer points to uninitialized memory for count elements
  public init<Owner: ~Copyable & ~Escapable>(
    initializing: UnsafeMutablePointer<Element>, capacity: Int, owner: inout Owner
  ) -> _mutate(owner) Self {
    start = initializing
    self.capacity = capacity
    return self
  }
}

extension OutputBuffer where Element: ~Copyable {
  public consuming func relinquishBorrowedMemory() -> (UnsafeMutablePointer<Element>, Int) {
    let start = self.start
    let initialized = self.initialized
    discard self
    return (start, initialized)
  }
}

extension OutputBuffer {

  public consuming func relinquishBorrowedMemory() -> UnsafeMutableBufferPointer<Element> {
    let (start, initialized) = relinquishBorrowedMemory()
    return .init(start: start, count: initialized)
  }
}

extension OutputBuffer: ContiguousStorage where Element: ~Copyable /*& ~Escapable*/ {
  public var storage: StorageView<Element> {
    borrowing _read {
      yield StorageView(unsafePointer: start, count: initialized, owner: self)
    }
  }
}

extension OutputBuffer where Element: ~Copyable {
  public mutating func appendElement(_ value: consuming Element) {
    precondition(initialized < capacity, "Output buffer overflow")
    start.advanced(by: initialized).initialize(to: value)
    initialized &+= 1
  }

  public mutating func deinitializeLastElement() {
    if initialized == 0 { return }
    initialized &-= 1
    start.advanced(by: initialized).deinitialize(count: 1)
  }
}

extension OutputBuffer {

  @discardableResult
  public mutating func deinitializeLastElement() -> Element? {
    guard initialized > 0 else { return nil }
    initialized &-= 1
    return start.advanced(by: initialized).move()
  }
}

extension OutputBuffer where Element: ~Copyable {
  public mutating func deinitialize() {
    start.deinitialize(count: initialized)
    initialized = 0
  }
}

//extension OutputBuffer {
//  public mutating func append<S>(
//    from elements: S
//  ) -> S.Iterator where S: Sequence, S.Element == Element {
//    var iterator = elements.makeIterator()
//    append(from: &iterator)
//    return iterator
//  }
//
//  public mutating func append(
//    from elements: inout some IteratorProtocol<Element>
//  ) {
//    while initialized < capacity {
//      guard let element = elements.next() else { break }
//      start.advanced(by: initialized).initialize(to: element)
//      initialized &+= 1
//    }
//  }
//
//  public mutating func append(
//    fromContentsOf source: some Collection<Element>
//  ) {
//    let count = source.withContiguousStorageIfAvailable {
//      guard let sourceAddress = $0.baseAddress, !$0.isEmpty else {
//        return 0
//      }
//      let available = capacity &- initialized
//      precondition(
//        $0.count <= available,
//        "buffer cannot contain every element from source."
//      )
//      let tail = start.advanced(by: initialized)
//      tail.initialize(from: sourceAddress, count: $0.count)
//      return $0.count
//    }
//    if let count {
//      initialized &+= count
//      return
//    }
//
//    let available = capacity &- initialized
//    let tail = start.advanced(by: initialized)
//    let suffix = UnsafeMutableBufferPointer(start: tail, count: available)
//    var (iterator, copied) = source._copyContents(initializing: suffix)
//    precondition(
//      iterator.next() == nil,
//      "buffer cannot contain every element from source."
//    )
//    assert(initialized + copied <= capacity)
//    initialized &+= copied
//  }

//  public mutating func moveAppend(
//    fromContentsOf source: UnsafeMutableBufferPointer<Element>
//  ) {
//    guard let sourceAddress = source.baseAddress, !source.isEmpty else {
//      return
//    }
//    let available = capacity &- initialized
//    precondition(
//      source.count <= available,
//      "buffer cannot contain every element from source."
//    )
//    let tail = start.advanced(by: initialized)
//    tail.moveInitialize(from: sourceAddress, count: source.count)
//    initialized &+= source.count
//  }
//
//  public mutating func moveAppend(
//    fromContentsOf source: Slice<UnsafeMutableBufferPointer<Element>>
//  ) {
//    moveAppend(fromContentsOf: UnsafeMutableBufferPointer(rebasing: source))
//  }
//}

//extension OutputBuffer {
//  public mutating func initializeOutOfOrder<R>(
//    _ count: Int,
//    body: (inout RandomAccessOutputBuffer<Element>) throws -> R
//  ) rethrows -> R {
//    precondition(initialized + count < capacity)
//    var out = RandomAccessOutputBuffer(
//      initializing: start.advanced(by: initialized), count: count
//    )
//    let result = try body(&out)
//    let buffer = out.relinquishBorrowedMemory()
//    assert(
//      buffer.baseAddress == start.advanced(by: initialized) &&
//      initialized + buffer.count < capacity
//    )
//    initialized &+= buffer.count
//    return result
//  }
//}

extension Array {

  public init(
    capacity: Int,
    initializingWith initializer: (inout OutputBuffer<Element>) throws -> Void
  ) rethrows {
    try self.init(
      unsafeUninitializedCapacity: capacity,
      initializingWith: { (buffer, count) in
        var buffer = buffer
        var output = OutputBuffer(
          initializing: buffer.baseAddress.unsafelyUnwrapped,
          capacity: buffer.count,
          owner: &buffer
        )
        try initializer(&output)
        let initialized = output.relinquishBorrowedMemory()
        assert(initialized.baseAddress == buffer.baseAddress)
        count = initialized.count
      }
    )
  }
}

extension String {

  // also see https://github.com/apple/swift/pull/23050
  // and `final class __SharedStringStorage`

  @available(macOS 11, *)
  public init(
    utf8Capacity capacity: Int,
    initializingWith initializer: (inout OutputBuffer<UInt8>) throws -> Void
  ) rethrows {
    try self.init(
      unsafeUninitializedCapacity: capacity,
      initializingUTF8With: { buffer in
        var buffer = buffer
        var output = OutputBuffer(
          initializing: buffer.baseAddress.unsafelyUnwrapped,
          capacity: capacity,
          owner: &buffer
        )
        try initializer(&output)
        let initialized = output.relinquishBorrowedMemory()
        assert(initialized.baseAddress == buffer.baseAddress)
        return initialized.count
      }
    )
  }
}

#endif
