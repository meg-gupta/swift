// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature BorrowAndMutateAccessors) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature BorrowAndMutateAccessors) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_BorrowAndMutateAccessors

// A write through a pointer to a mutate accessor's address result must be
// visible to the caller.

final class X {}

struct S {
  var _x: X
  var _i: Int

  var i: Int {
    borrow { return _i }
    @inline(never)
    mutate { return &_i }
  }
}

struct ModifyS {
  var _x: X
  var _i: Int

  var i: Int {
    get { return _i }
    @inline(never)
    _modify { yield &_i }
  }
}

@inline(never)
func writeThroughPointer(_ p: UnsafeMutablePointer<Int>) {
  p.pointee = 42
}

@inline(never)
func escapeViaAccessor(_ s: inout S) {
  withUnsafeMutablePointer(to: &s.i) { writeThroughPointer($0) }
}

@inline(never)
func escapeViaModify(_ s: inout ModifyS) {
  withUnsafeMutablePointer(to: &s.i) { writeThroughPointer($0) }
}

@inline(never)
func caller() -> Int {
  var s = S(_x: X(), _i: 1)
  escapeViaAccessor(&s)
  return s._i
}

@inline(never)
func modifyCaller() -> Int {
  var s = ModifyS(_x: X(), _i: 1)
  escapeViaModify(&s)
  return s._i
}

// CHECK: mutate: 42
print("mutate: \(caller())")
// CHECK: modify: 42
print("modify: \(modifyCaller())")
