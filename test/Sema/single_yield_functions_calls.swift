// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: swift_feature_Lifetimes

struct Counter {
  var value = 0
  func owned() -> yield Int { yield value }
  func borrowed() -> yield borrow Int { yield value }
  mutating func mutable() -> yield inout Int { yield &value }
  static func make() -> yield Counter { yield Counter() }
}

func increment(_ x: inout Int) { x += 1 }
func show(_ x: Int) {}

func testCopyable() {
  var c = Counter()
  var x = c.owned()
  x += 1
  let y: Int = c.borrowed()
  show(c.borrowed())
  c.mutable() += 1
  c.mutable() = 5
  increment(&c.mutable())
  let z = c.mutable()
  _ = Counter.make().value
  let s: String = c.owned() // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
  _ = (x, y, z, s)
}

func testImmutable() {
  let c = Counter() // expected-note {{change 'let' to 'var' to make it mutable}}
  c.mutable() += 1 // expected-error {{'c' is a 'let' constant}}
}

struct Row: ~Copyable {
  var name: String
  var count: Int
}

struct Table: ~Copyable {
  var storage: Row

  borrowing func row(named name: String) -> yield borrow Row { yield storage }
  mutating func mutableRow(named name: String) -> yield inout Row { yield &storage }
}

func useRow(_ r: borrowing Row) {}

func testNoncopyable(_ t: inout Table) {
  print(t.row(named: "widgets").count)
  useRow(t.row(named: "widgets"))
  t.mutableRow(named: "widgets").count += 1
  t.mutableRow(named: "widgets").name = "gadgets"
}

struct Box<T> {
  var value: T
  func owned() -> yield T { yield value }
  func borrowed() -> yield borrow T { yield value }
  mutating func mutable() -> yield inout T { yield &value }
}

func identity<T>(_ t: T) -> yield T { yield t }
func access<T>(_ t: inout T) -> yield inout T { yield &t }

func testGeneric() {
  var b = Box(value: 1)
  let x: Int = b.owned()
  show(b.borrowed())
  b.mutable() += 1
  increment(&b.mutable())
  show(identity(3))
  let s: String = identity("a")
  var n = 0
  access(&n) += 1
  increment(&access(&n))
  let bad: String = b.owned() // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
  _ = (x, s, bad)
}

func testGenericContext<T>(_ b: inout Box<T>, _ t: T) -> T {
  b.mutable() = t
  return b.owned()
}

struct NCBox<T: ~Copyable>: ~Copyable {
  var value: T
  borrowing func read() -> yield borrow T { yield value }
  mutating func modify() -> yield inout T { yield &value }
}

func testGenericNoncopyable(_ b: inout NCBox<Row>) {
  useRow(b.read())
  b.modify().count += 1
}

protocol Shape {
  var area: Int { get }
}

extension Shape {
  func measured() -> yield Int { yield area }
  func copied() -> yield Self { yield self }
}

struct Square: Shape {
  var area: Int
}

func testProtocolExtension(_ s: Square, _ e: any Shape) {
  show(s.measured())
  show(e.measured())
  let c: Square = s.copied()
  let ec: any Shape = e.copied()
  _ = (c, ec)
}

struct View: ~Escapable {
  var count: Int
  @_lifetime(immortal) init() { count = 0 }
}

struct ViewStorage {
  var a: [Int]
  func view() -> yield View { yield View() }
  mutating func mutableView() -> yield inout View { fatalError() }
}

func useView(_ v: borrowing View) {}
func yieldsGeneric<T: ~Escapable>(_ t: borrowing T) -> yield borrow T { yield t }

func testNonEscapable(_ s: inout ViewStorage, _ v: borrowing View) {
  useView(s.view())
  show(s.view().count)
  s.mutableView().count += 1
  s.mutableView() = View()
  useView(yieldsGeneric(v))
  let _: String = s.view().count // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
}
