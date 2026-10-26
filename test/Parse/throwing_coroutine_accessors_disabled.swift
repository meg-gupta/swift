// RUN: %target-typecheck-verify-swift -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_CoroutineAccessors

struct S {
  var x: Int

  var p: Int {
    yielding borrow throws { yield x } // expected-error {{'yielding_borrow' accessor cannot have specifier 'throws'}}
  }
}
