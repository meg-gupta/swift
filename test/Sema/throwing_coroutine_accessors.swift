// RUN: %target-typecheck-verify-swift -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors

// REQUIRES: swift_feature_ThrowingCoroutineAccessors

struct E: Error {}
func mayThrow() throws {}

struct S {
  var x: Int

  // expected-note@+1 {{property declared here}}
  var p: Int {
    yielding borrow throws {
      try mayThrow()
      if x < 0 {
        throw E()
      }
      yield x
    }
  }

  var missingTryInBody: Int {
    yielding borrow throws {
      mayThrow() // expected-error {{call can throw but is not marked with 'try'}}
      // expected-note@-1 {{did you mean to use 'try'?}}
      // expected-note@-2 {{did you mean to handle error as optional value?}}
      // expected-note@-3 {{did you mean to disable error propagation?}}
      yield x
    }
  }

  subscript(i: Int) -> Int {
    yielding borrow throws { yield x }
  }
}

func reads(_ s: S) throws -> Int {
  let a = try s.p
  let b = try s[0]
  return a + b
}

func missingTry(_ s: S) throws {
  _ = s.p // expected-error {{property access can throw but is not marked with 'try'}}
  // expected-note@-1 {{did you mean to use 'try'?}}
  // expected-note@-2 {{did you mean to handle error as optional value?}}
  // expected-note@-3 {{did you mean to disable error propagation?}}
  _ = s[0] // expected-error {{subscript access can throw but is not marked with 'try'}}
  // expected-note@-1 {{did you mean to use 'try'?}}
  // expected-note@-2 {{did you mean to handle error as optional value?}}
  // expected-note@-3 {{did you mean to disable error propagation?}}
}

func unhandled(_ s: S) -> Int {
  return s.p // expected-error {{property access can throw, but it is not marked with 'try' and the error is not handled}}
}

func keyPath() {
  _ = \S.p // expected-error {{cannot form key path to property with 'throws' or 'async'}}
}

struct NC: ~Copyable {
  var v: Int
}

struct Holder: ~Copyable {
  var nc: NC

  var p: NC {
    yielding borrow throws {
      try mayThrow()
      yield nc
    }
  }
}

func readNoncopyable(_ h: borrowing Holder) throws -> Int {
  return try h.p.v
}
