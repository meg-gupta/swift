// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip
// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s --check-prefix=NOFLAG

// REQUIRES: swift_feature_SingleYieldFunctions

// NOFLAG: error: '-> yield' requires '-enable-experimental-feature SingleYieldFunctions'

struct Counter {
  var value: Int = 0

  func owned() -> yield Int { yield value }
  func borrowed() -> yield borrow Int { yield value }
  mutating func mutable() -> yield inout Int { yield &value }
  static func staticOwned() -> yield Int { yield 0 }

  func borrowedNoAmpersand() -> yield borrow Int { yield value }
  mutating func needsAmpersand() -> yield inout Int {
    yield value // expected-error {{yielding mutable value of type 'Int' requires explicit '&'}}
  }
}

func globalOwned() -> yield String { yield "" }

func outer() {
  func localBorrowed() -> yield borrow Int { yield 1 }
}

// `yield` is still a type name when it isn't followed by a type.
struct yield {}
func returnsYieldType() -> yield { return yield() }

struct Invalid {
  init() -> yield Int {} // expected-error {{initializers cannot be single-yield functions}}
  static func +(a: Invalid, b: Invalid) -> yield Int { fatalError() } // expected-error {{operator functions cannot be single-yield functions}}
}

func takesFunction(_ f: () -> yield Int) {} // expected-error {{single-yield function types are not yet supported}}
