// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

func borrow(a: borrowing [Int]) {
  let s = a.storage
  print(s.count)
//  print(s[offset: 0])
}

func test() {
  var a = Array(0..<9)
  borrow(a: a)
  a.append(a.count)
}
