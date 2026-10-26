// RUN: %target-typecheck-verify-swift -enable-library-evolution \
// RUN:     -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN:     -verify-additional-prefix old-
// RUN: %target-typecheck-verify-swift -enable-library-evolution \
// RUN:     -target %target-future-triple \
// RUN:     -enable-experimental-feature ThrowingCoroutineAccessors
// RUN: %target-typecheck-verify-swift -disable-callee-allocated-coro-abi \
// RUN:     -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN:     -verify-additional-prefix noabi-

// REQUIRES: swift_feature_ThrowingCoroutineAccessors
// REQUIRES: swift_stable_abi

public struct S {
  public var x: Int

  public var publicProp: Int {
    yielding borrow throws { yield x } // expected-old-error {{throwing 'yielding borrow' accessor of property 'publicProp' must not be available before the yield_once_2 coroutine ABI}} expected-noabi-error {{throwing 'yielding borrow' accessor requires the callee-allocated coroutine ABI}}
  }

  @available(macOS 99.99, iOS 99.99, watchOS 99.99, tvOS 99.99, visionOS 99.99, *)
  public var newEnough: Int {
    yielding borrow throws { yield x } // expected-noabi-error {{throwing 'yielding borrow' accessor requires the callee-allocated coroutine ABI}}
  }

  internal var internalProp: Int {
    yielding borrow throws { yield x } // expected-noabi-error {{throwing 'yielding borrow' accessor requires the callee-allocated coroutine ABI}}
  }

  internal var transparentProp: Int {
    @_transparent yielding borrow throws { yield x } // expected-error {{'@_transparent' is not supported on a throwing 'yielding borrow' accessor}} expected-noabi-error {{throwing 'yielding borrow' accessor requires the callee-allocated coroutine ABI}}
  }

  @available(macOS 99.99, iOS 99.99, watchOS 99.99, tvOS 99.99, visionOS 99.99, *)
  @backDeployed(before: macOS 100, iOS 100, watchOS 100, tvOS 100, visionOS 100)
  public var backDeployedProp: Int {
    yielding borrow throws { yield x } // expected-error {{'@backDeployed' is not supported on a throwing 'yielding borrow' accessor}} expected-noabi-error {{throwing 'yielding borrow' accessor requires the callee-allocated coroutine ABI}}
  }
}
