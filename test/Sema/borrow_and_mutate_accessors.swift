// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors

class Klass {}

struct Struct {}

extension Klass {
  var i: Int {
    borrow { // expected-error{{a borrow accessor is supported only on a struct or enum}}
      return 0
    }
    mutate { // expected-error{{a mutate accessor is supported only on a struct or enum}}
      fatalError()
    }
  }
}

extension Struct {
  var i: Int {
    borrow {
      return 0
    }
    mutate {
      fatalError()
    }
  }
}

enum E {}

extension E {
  var i: Int {
    borrow {
      return 0
    }
    mutate {
      fatalError()
    }
  }
}

// TODO: borrow and mutate protocol requirements
protocol P {
  var name: String { borrow } // expected-error{{property in protocol must have explicit { get } or { get set } specifier}} // expected-error{{expected get, read, or set in a protocol property}}
  var phone: String { mutate } // expected-error{{property in protocol must have explicit { get } or { get set } specifier}} // expected-error{{expected get, read, or set in a protocol property}}
}

