// RUN: %target-swift-emit-sil -verify %s -o /dev/null \
// RUN:     -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors

// REQUIRES: swift_feature_ThrowingCoroutineAccessors

struct E: Error {}
func mayThrow() throws {}

struct NC: ~Copyable {
  var v: Int
}

func makeNC() throws -> NC { NC(v: 0) }

struct S: ~Copyable {
  var nc: NC
  var flag: Bool

  var beforeYield: NC {
    yielding borrow throws {
      try mayThrow()
      if flag {
        throw E()
      }
      yield nc
    }
  }

  var yieldTry: NC {
    yielding borrow throws {
      yield try makeNC()
    }
  }

  var handledAfter: NC {
    yielding borrow throws {
      yield nc
      do {
        try mayThrow()
      } catch {}
    }
  }

  var throwAfter: NC {
    yielding borrow throws {
      yield nc // expected-note {{accessor yields here}}
      if flag {
        throw E() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
      }
    }
  }

  var tryAfter: NC {
    yielding borrow throws {
      yield nc // expected-note {{accessor yields here}}
      try mayThrow() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
    }
  }

  var beforeAndAfter: NC {
    yielding borrow throws {
      try mayThrow()
      yield nc // expected-note {{accessor yields here}}
      try mayThrow() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
    }
  }

  var rethrowFromCatch: NC {
    yielding borrow throws {
      do {
        try mayThrow()
        yield nc // expected-note {{accessor yields here}}
        try mayThrow()
      } catch {
        throw error // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
      }
    }
  }

  var caughtThenRethrow: NC {
    yielding borrow throws {
      yield nc // expected-note {{accessor yields here}}
      do {
        if flag {
          throw E()
        }
      } catch {
        throw error // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
      }
    }
  }

  var throwBeforeTryAfter: NC {
    yielding borrow throws {
      if flag {
        throw E()
      }
      yield nc // expected-note {{accessor yields here}}
      try mayThrow() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
    }
  }

  var throwAndTryAfter: NC {
    yielding borrow throws {
      yield nc // expected-note 2 {{accessor yields here}}
      if flag {
        throw E() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
      }
      try mayThrow() // expected-error {{'yielding borrow' accessor cannot throw after it yields}}
    }
  }

  var deferBeforeYield: NC {
    yielding borrow throws {
      defer { try? mayThrow() }
      if flag {
        throw E()
      }
      yield nc
    }
  }
}
