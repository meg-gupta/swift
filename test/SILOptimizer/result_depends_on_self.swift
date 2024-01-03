// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip
// REQUIRES: asserts

import Builtin

final class Klass {
    _resultDependsOnSelf func getDependentResult() -> Object {
      return Object(self)
    }
}

struct Object : ~Escapable {
  let ptr: Builtin.NativeObject
  init(_ k: Klass) {
    ptr = Builtin.unsafeCastToNativeObject(k)
  }
}

func test() {
  var x = Klass()
  let obj = x.getDependentResult()
  use(obj)
}

func use(_ o : Object) {}

