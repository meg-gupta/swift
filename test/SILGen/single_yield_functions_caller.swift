// RUN: %target-swift-emit-silgen -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip -module-name M %s | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions

class Box {}

struct Counter {
  var value = 0
  var box = Box()
  func owned() -> yield Box { yield box }
  func borrowed() -> yield borrow Int { yield value }
  mutating func mutable() -> yield inout Int { yield &value }
}

func use(_ x: Int) {}
func consume(_ b: __owned Box) {}

// Like a `yielding borrow` accessor, a copyable borrowed value is copied into
// an argument, so the coroutine resumes before the callee runs.
// CHECK-LABEL: sil hidden [ossa] @{{.*}}12testBorrowed{{.*}} : $@convention(thin) (@guaranteed Counter) -> () {
// CHECK:         [[FN:%.*]] = function_ref @{{.*}}7CounterV8borrowed{{.*}}
// CHECK:         ([[VALUE:%.*]], [[TOKEN:%.*]], [[ALLOC:%.*]]) = begin_apply [[FN]](
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[USE:%.*]] = function_ref @{{.*}}3useyySiF
// CHECK:         apply [[USE]]([[VALUE]])
// CHECK:         dealloc_stack [[ALLOC]]
func testBorrowed(_ c: Counter) {
  use(c.borrowed())
}

// A noncopyable borrowed value stays borrowed across the call it's passed to.
struct Row: ~Copyable { var count = 0 }
struct Rows: ~Copyable {
  var storage = Row()
  borrowing func row() -> yield borrow Row { yield storage }
}
func useRow(_ r: borrowing Row) {}
// CHECK-LABEL: sil hidden [ossa] @{{.*}}15testNoncopyable{{.*}} :
// CHECK:         ([[ROW:%.*]], [[TOKEN:%.*]], [[ALLOC:%.*]]) = begin_apply
// CHECK:         [[USE:%.*]] = function_ref @{{.*}}6useRow{{.*}}
// CHECK:         apply [[USE]](
// CHECK:         end_apply [[TOKEN]]
func testNoncopyable(_ r: borrowing Rows) {
  useRow(r.row())
}

// An owned yield is forwarded without a copy. The caller owns it, so the
// coroutine resumes when the argument has been evaluated.
// CHECK-LABEL: sil hidden [ossa] @{{.*}}9testOwned{{.*}} :
// CHECK:         ([[BOX:%.*]], [[TOKEN:%.*]], [[ALLOC:%.*]]) = begin_apply
// CHECK-NOT:     copy_value [[BOX]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         apply {{%.*}}([[BOX]])
func testOwned(_ c: Counter) {
  consume(c.owned())
}

// The access to `c` covers the whole coroutine.
// CHECK-LABEL: sil hidden [ossa] @{{.*}}11testMutable{{.*}} : $@convention(thin) (@inout Counter) -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// CHECK:         ([[ADDR:%.*]], [[TOKEN:%.*]], [[ALLOC:%.*]]) = begin_apply {{%.*}}([[ACCESS]])
// CHECK:         assign {{%.*}} to [[ADDR]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOC]]
// CHECK:         end_access [[ACCESS]]
func testMutable(_ c: inout Counter) {
  c.mutable() = 1
}

// Arguments are evaluated before the coroutine starts.
func makeName() -> String { "" }
struct Table {
  var rows: [String: Int] = [:]
  mutating func row(named name: String) -> yield inout Int { yield &rows[name, default: 0] }
}
// CHECK-LABEL: sil hidden [ossa] @{{.*}}12testArgOrder{{.*}} :
// CHECK:         function_ref @{{.*}}8makeName{{.*}}
// CHECK:         begin_access [modify]
// CHECK:         begin_apply
func testArgOrder(_ t: inout Table) {
  t.row(named: makeName()) += 1
}

// Global functions have no base.
func globalYield() -> yield borrow Int { yield 0 }
// CHECK-LABEL: sil hidden [ossa] @{{.*}}10testGlobal{{.*}} :
// CHECK:         ([[V:%.*]], [[TOKEN:%.*]], [[ALLOC:%.*]]) = begin_apply {{%.*}}() : $@yield_once_2 @convention(thin) () -> @yields Int
// CHECK:         end_apply [[TOKEN]]
func testGlobal() {
  use(globalYield())
}

// Local functions pass their captures.
// CHECK-LABEL: sil hidden [ossa] @{{.*}}9testLocal{{.*}} :
// CHECK:         begin_apply {{%.*}}(%0)
func testLocal(_ x: Int) {
  func local() -> yield borrow Int { yield x }
  use(local())
}
