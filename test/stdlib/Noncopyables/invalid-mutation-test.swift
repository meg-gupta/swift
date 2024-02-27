// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes -Xfrontend -enable-experimental-associated-type-inference -Xfrontend -disable-experimental-parser-round-trip -Xllvm -enable-lifetime-dependence-diagnostics)
// REQUIRES: executable_test

struct A: ~Copyable {
  var value: Int

  init(_ value: Int) { self.value = value }
}

struct B: ~Escapable {
  let p: UnsafeMutablePointer<Int>
  var value: Int {
    get { p.pointee }
    _modify { yield &p.pointee }
  }

  init(_ a: inout A) -> _mutate(a) Self {
    p = withUnsafeMutablePointer(to: &a.value) { $0 }
    return self
  }
}

extension A {
  var b: B {
    mutating _read {
      yield B(&self)
    }
    _modify {
      var b = B(&self)
      yield &b
    }
  }
}

func test1(a: inout A) {
  var b = a.b
  b.value += 1
}

var a = A(42)
test1(a: &a)
assert(a.value == 43)

func test2(a: inout A) -> _mutate(a) B {
  a.b
}
var b = test2(a: &a)
b.value += 1


//func overTest() {
//  var a = A(42)
//  var b = test(a: &a)
//  //print(a.value) // 43
//}
//
//overTest()
