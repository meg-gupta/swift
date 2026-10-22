// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: concurrency

struct Effects {
  var x = 0
  func asyncYield() async -> yield Int { yield x } // expected-error {{single-yield functions cannot be 'async'}}
  func throwingYield() throws -> yield Int { yield x } // expected-error {{single-yield functions cannot be 'throws'}}
  @yield_once func attributed() -> yield Int { yield x } // expected-error {{'@yield_once' cannot be applied to a single-yield function}}
}

func generic<T>(_ t: T) -> yield T { yield t } // expected-error {{single-yield functions in generic contexts are not yet supported}}

struct G<T> {
  var t: T
  func get() -> yield T { yield t } // expected-error {{single-yield functions in generic contexts are not yet supported}}
}

protocol P {
  func requirement() -> yield Int // expected-error {{single-yield functions as protocol requirements are not yet supported}}
}

extension P {
  func extensionMethod() -> yield Int { yield 0 } // expected-error {{single-yield functions in generic contexts are not yet supported}}
}

class C {
  var x = 0
  func nonFinal() -> yield Int { yield x } // expected-error {{single-yield functions as non-final class methods are not yet supported}}
  class func classMethod() -> yield Int { yield 0 } // expected-error {{single-yield functions as non-final class methods are not yet supported}}
  final func finalMethod() -> yield borrow Int { yield x }
  static func staticMethod() -> yield Int { yield 0 }
}

final class FC {
  var x = 0
  func method() -> yield borrow Int { yield x }
}

actor A {
  func method() -> yield Int { yield 0 } // expected-error {{single-yield functions as actor methods are not yet supported}}
}

struct Dynamic {
  dynamic func d() -> yield Int { yield 0 } // expected-error {{single-yield functions marked 'dynamic' are not yet supported}}
}

enum NE: ~Escapable {}
// Lifetime dependence also rejects the yield until it learns about single-yield
// functions.
func yieldsNonEscapable() -> yield NE { fatalError() } // expected-error {{single-yield functions yielding a non-Escapable type are not yet supported}} expected-error {{a function cannot return a ~Escapable result}}

struct Overloads {
  var x = 0
  func f() -> Int { x } // expected-note {{'f()' previously declared here}}
  func f() -> yield Int { yield x } // expected-error {{invalid redeclaration of 'f()'}}

  func g() -> yield borrow Int { yield x } // expected-note {{'g()' previously declared here}}
  mutating func g() -> yield inout Int { yield &x } // expected-error {{invalid redeclaration of 'g()'}}

  func h() -> Int { x }
  func h() -> yield String { yield "" }
}
