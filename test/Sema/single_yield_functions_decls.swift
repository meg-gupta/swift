// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: concurrency

func generic<T>(_ t: T) -> yield T { yield t }
func genericBorrow<T: ~Copyable>(_ t: borrowing T) -> yield borrow T { yield t }
func genericInout<T>(_ t: inout T) -> yield inout T { yield &t }
func constrained<T: Equatable>(_ t: T) -> yield Bool { yield t == t }

struct G<T> {
  var t: T
  func get() -> yield T { yield t }
  func read() -> yield borrow T { yield t }
  mutating func modify() -> yield inout T { yield &t }
  func genericMethod<U>(_ u: U) -> yield [U] { yield [u] }
}

extension G where T == Int {
  func doubled() -> yield Int { yield t * 2 }
}

struct NCBox<T: ~Copyable>: ~Copyable {
  var value: T
  borrowing func read() -> yield borrow T { yield value }
  mutating func modify() -> yield inout T { yield &value }
}

final class GenericFinalClass<T> {
  var t: T
  init(t: T) { self.t = t }
  func read() -> yield borrow T { yield t }
}

class GenericClass<T> {
  var t: T
  init(t: T) { self.t = t }
  func read() -> yield borrow T { yield t }
}

protocol P {
  func requirement() -> yield Int
}

protocol GenericRequirement {
  func requirement<T>(_ t: T) -> yield T
}

extension P {
  func extensionMethod() -> yield Int { yield 0 }
  func yieldsSelf() -> yield Self { yield self }
}

class C {
  var x = 0
  func nonFinal() -> yield Int { yield x }
  class func classMethod() -> yield Int { yield 0 }
  final func finalMethod() -> yield borrow Int { yield x }
  static func staticMethod() -> yield Int { yield 0 }
}

final class FC {
  var x = 0
  func method() -> yield borrow Int { yield x }
}

actor A {
  func method() -> yield Int { yield 0 }
}

struct Dynamic {
  dynamic func d() -> yield Int { yield 0 }
}

struct View: ~Escapable {
  var count: Int
  @_lifetime(immortal) init() { count = 0 }
}

func yieldsGenericNonEscapable<T: ~Escapable>(_ t: borrowing T) -> yield borrow T { yield t }
func yieldsOwnedView(_ v: consuming View) -> yield View { yield v }
func yieldsBorrowedView(_ v: borrowing View) -> yield borrow View { yield v }
func yieldsViewOfArray(_ a: [Int]) -> yield View { yield View() }
@_lifetime(immortal) func yieldsImmortalView() -> yield View { yield View() }
func yieldsViewWithoutParam() -> yield View { yield View() } // expected-error {{a function with a non-'Escapable' result needs a parameter to depend on}} expected-note {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}

struct ViewStorage {
  var a: [Int]
  func view() -> yield View { yield View() }
  mutating func mutableView() -> yield inout View { fatalError() }
}

struct TrivialViewStorage {
  var x: Int
  func view() -> yield View { yield View() } // expected-error {{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable, specify '@_lifetime(borrow self)'}}
  @_lifetime(borrow self) func annotatedView() -> yield View { yield View() }
}

struct ViewHolder: ~Escapable {
  var view: View
  @_lifetime(copy self) func read() -> yield borrow View { yield view }
}

struct Overloads {
  var x = 0
  func h() -> Int { x }
  func h() -> yield String { yield "" }
}
