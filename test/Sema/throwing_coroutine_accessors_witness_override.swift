// RUN: %target-typecheck-verify-swift -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors

// REQUIRES: swift_feature_ThrowingCoroutineAccessors

protocol ThrowingBorrow {
  // expected-note@+1 3 {{protocol requires property 'p' with type 'Int'}}
  var p: Int { yielding borrow throws }
}

protocol ThrowingGet {
  // expected-note@+1 {{protocol requires property 'p' with type 'Int'}}
  var p: Int { get throws }
}

struct Exact: ThrowingBorrow {
  var p: Int { yielding borrow throws { yield 0 } }
}

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate and protocol requirement must both or neither use a throwing 'yielding borrow' accessor}}
// expected-error@+1 {{type 'NonThrowingBorrow' does not conform to protocol 'ThrowingBorrow'}}
struct NonThrowingBorrow: ThrowingBorrow { var p: Int { yielding borrow { yield 0 } } }

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate and protocol requirement must both or neither use a throwing 'yielding borrow' accessor}}
// expected-error@+1 {{type 'ThrowingGetWitness' does not conform to protocol 'ThrowingBorrow'}}
struct ThrowingGetWitness: ThrowingBorrow { var p: Int { get throws { 0 } } }

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate and protocol requirement must both or neither use a throwing 'yielding borrow' accessor}}
// expected-error@+1 {{type 'StoredWitness' does not conform to protocol 'ThrowingBorrow'}}
struct StoredWitness: ThrowingBorrow { var p: Int = 0 }

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate and protocol requirement must both or neither use a throwing 'yielding borrow' accessor}}
// expected-error@+1 {{type 'BorrowForGet' does not conform to protocol 'ThrowingGet'}}
struct BorrowForGet: ThrowingGet { var p: Int { yielding borrow throws { yield 0 } } }

class Base {
  var p: Int { yielding borrow throws { yield 0 } }
  var q: Int { yielding borrow { yield 0 } }
  var r: Int { get throws { 0 } }
}

class DerivedExact: Base {
  override var p: Int { yielding borrow throws { yield 1 } }
}

class DerivedMismatch: Base {
  override var p: Int { yielding borrow { yield 1 } } // expected-error {{overriding property and the overridden property must both or neither use a throwing 'yielding borrow' accessor}}
  override var q: Int { yielding borrow throws { yield 1 } } // expected-error {{overriding property and the overridden property must both or neither use a throwing 'yielding borrow' accessor}}
  override var r: Int { yielding borrow throws { yield 1 } } // expected-error {{overriding property and the overridden property must both or neither use a throwing 'yielding borrow' accessor}}
}
