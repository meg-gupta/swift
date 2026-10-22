// RUN: %target-swift-frontend -emit-sil -verify -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip %s -o /dev/null

// REQUIRES: swift_feature_SingleYieldFunctions

struct Row: ~Copyable { var count = 0 }

struct Table: ~Copyable {
  var storage = Row()
  borrowing func row() -> yield borrow Row { yield storage }
  mutating func mutableRow() -> yield inout Row { yield &storage }
}

func takeRow(_ r: consuming Row) {}
func borrowRow(_ r: borrowing Row) {}

func consumeBorrowed(_ t: borrowing Table) {
  takeRow(t.row()) // expected-error {{cannot be consumed}} expected-note {{consumed here}}
}

func borrowBorrowed(_ t: borrowing Table) {
  borrowRow(t.row())
  _ = t.row().count
}

func mutateInPlace(_ t: inout Table) {
  t.mutableRow().count += 1
}

struct Counter {
  var value = 0
  mutating func mutable() -> yield inout Int { yield &value }
}

// The access to `c` lasts until the coroutine resumes.
func exclusivity(_ c: inout Counter) {
  swap(&c.mutable(), &c.value) // expected-error {{overlapping accesses to 'c'}} expected-note {{conflicting access is here}}
}
