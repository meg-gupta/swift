// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

struct A: ~Copyable {
  var value: Int

  init(_ value: Int) { self.value = value }
}

struct B: ~Escapable {
  let value: Int

  init(_ borrowed: borrowing A) -> _borrow(borrowed) Self {
    self.value = borrowed.value
    return self
  }
}

extension A {
  var b: B {
    borrowing _read {
      yield B(self)
    }
  }
}

func test(a: inout A) {
  let b = a.b   // borrows `a` (non-mutably)
  let c = b.value + 1
  _ = consume b // should this end the borrow of `a`?
  a.value = 1
  print(c)
}

//var a = A(42)
//test(a: &a)
