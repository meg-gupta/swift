// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -enable-experimental-feature CoroutineFunctions -disable-experimental-parser-round-trip

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: swift_feature_CoroutineFunctions

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

func testReferences() {
  let c = Counter()
  let f = c.owned // expected-error {{using single-yield function 'owned()' other than by calling it is not yet supported}}
  _ = Counter.owned // expected-error {{using single-yield function 'owned()' other than by calling it is not yet supported}}
  _ = f
}

@yield_once func oldStyle(_ x: inout Float) yields (inout Float) -> () { yield &x }

func testOldStyle() {
  var f: Float = 0
  oldStyle(&f) // expected-error {{calling a '@yield_once' function is not supported}}
  let fn = oldStyle
  fn(&f) // expected-error {{calling a '@yield_once' function is not supported}}
}
