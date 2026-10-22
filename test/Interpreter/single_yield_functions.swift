// RUN: %target-run-simple-swift(-enable-experimental-feature SingleYieldFunctions -Xfrontend -disable-experimental-parser-round-trip) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_SingleYieldFunctions

struct Counter {
  var value = 1

  func owned() -> yield Int {
    print("owned: start")
    yield value
    print("owned: end")
  }

  func borrowed() -> yield borrow Int {
    print("borrowed: start")
    yield value
    print("borrowed: end")
  }

  mutating func mutable() -> yield inout Int {
    print("mutable: start \(value)")
    yield &value
    print("mutable: end \(value)")
  }
}

func show(_ x: Int) { print("show: \(x)") }

struct E: Error {}

struct Row: ~Copyable {
  var name: String
  var count: Int
}

func showRow(_ r: borrowing Row) { print("showRow: \(r.name) \(r.count)") }

func failingRow(_ r: borrowing Row) throws {
  print("failingRow: \(r.count)")
  throw E()
}

struct Table: ~Copyable {
  var storage = Row(name: "widgets", count: 0)

  borrowing func row(named name: String) -> yield borrow Row {
    print("row: start \(name)")
    yield storage
    print("row: end")
  }

  mutating func mutableRow(named name: String) -> yield inout Row {
    print("mutableRow: start \(name)")
    yield &storage
    print("mutableRow: end \(storage.count)")
  }

  consuming func take() -> yield Row {
    print("take: start")
    yield storage
    print("take: end")
  }
}

func main() {
  var c = Counter()

  // CHECK: owned: start
  // CHECK-NEXT: owned: end
  // CHECK-NEXT: x = 1
  let x = c.owned()
  print("x = \(x)")

  // A copyable borrowed value is copied into the argument, so the coroutine
  // resumes before `show` runs.
  // CHECK-NEXT: borrowed: start
  // CHECK-NEXT: borrowed: end
  // CHECK-NEXT: show: 1
  show(c.borrowed())

  // CHECK-NEXT: mutable: start 1
  // CHECK-NEXT: mutable: end 2
  c.mutable() += 1

  // CHECK-NEXT: value = 2
  print("value = \(c.value)")

  var t = Table()

  // CHECK-NEXT: row: start widgets
  // CHECK-NEXT: row: end
  // CHECK-NEXT: count = 0
  print("count = \(t.row(named: "widgets").count)")

  // A noncopyable borrowed value stays borrowed until `showRow` returns.
  // CHECK-NEXT: row: start widgets
  // CHECK-NEXT: showRow: widgets 0
  // CHECK-NEXT: row: end
  showRow(t.row(named: "widgets"))

  // CHECK-NEXT: mutableRow: start widgets
  // CHECK-NEXT: mutableRow: end 1
  t.mutableRow(named: "widgets").count += 1

  // The coroutine still runs to completion when the caller throws.
  // CHECK-NEXT: row: start widgets
  // CHECK-NEXT: failingRow: 1
  // CHECK-NEXT: row: end
  // CHECK-NEXT: caught
  do {
    try failingRow(t.row(named: "widgets"))
  } catch {
    print("caught")
  }

  // CHECK-NEXT: take: start
  // CHECK-NEXT: take: end
  // CHECK-NEXT: taken widgets 1
  let r = t.take()
  print("taken \(r.name) \(r.count)")
}

main()
