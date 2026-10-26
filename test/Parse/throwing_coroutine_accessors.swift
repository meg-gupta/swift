// RUN: %target-typecheck-verify-swift -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors

// REQUIRES: swift_feature_ThrowingCoroutineAccessors

struct MyError: Error {}

struct S {
  var x: Int

  var plain: Int {
    yielding borrow throws { yield x }
  }

  subscript(i: Int) -> Int {
    yielding borrow throws { yield x }
  }

  var typed: Int {
    yielding borrow throws(MyError) { yield x } // expected-error {{typed throws is not supported on a 'yielding borrow' accessor}}
  }

  var asyncBorrow: Int {
    yielding borrow async { yield x } // expected-error {{'yielding_borrow' accessor cannot have specifier 'async'}}
  }

  var asyncThrowsBorrow: Int {
    yielding borrow async throws { yield x } // expected-error {{'yielding_borrow' accessor cannot have specifier 'async'}}
  }

  var mutateThrows: Int {
    yielding borrow { yield x }
    yielding mutate throws { yield &x } // expected-error {{'yielding_mutate' accessor cannot have specifier 'throws'}}
  }

  var readThrows: Int {
    _read throws { yield x } // expected-error {{'_read' accessor cannot have specifier 'throws'}}
  }

  var modifyThrows: Int {
    get { x }
    _modify throws { yield &x } // expected-error {{'_modify' accessor cannot have specifier 'throws'}}
  }

  var withMutateAfter: Int {
    yielding borrow throws { yield x }
    yielding mutate { yield &x } // expected-error {{'yielding_mutate' accessor is not allowed on property with 'yielding borrow' accessor that is 'throws'}}
  }

  var withSetBefore: Int {
    set {} // expected-error {{'set' accessor is not allowed on property with 'yielding borrow' accessor that is 'throws'}}
    yielding borrow throws { yield x }
  }

  var withEffectfulGet: Int {
    get throws { x } // expected-note {{getter defined here}}
    yielding borrow throws { yield x } // expected-error {{'yielding_borrow' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}} expected-error {{variable cannot provide both a 'yielding borrow' accessor and a getter}}
  }
}

protocol P {
  var p: Int { yielding borrow throws }
  subscript(i: Int) -> Int { yielding borrow throws }
}
