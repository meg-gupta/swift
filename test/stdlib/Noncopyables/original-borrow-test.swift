// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

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

extension A: ContiguousStorage {

  var storage: StorageView<Int> {
    borrowing get {
      .init(unsafePointer: p, count: c, owner: self)
    }
  }
}

