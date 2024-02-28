// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

func test() {
  let c = 10
  let p = UnsafeMutablePointer<Int>.allocate(c)

  var o = OutputBuffer<Int>(initializing: p, capacity: c, owner: p)

  let s = o.storage
  assert(s.isEmpty)
}

test()
