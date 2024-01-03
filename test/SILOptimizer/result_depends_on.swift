// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip
// REQUIRES: asserts

import Builtin

final class Container {
}

final class GenericContainer<T> {
}

struct MoveOnlyStruct : ~Copyable {
}

struct View : ~Escapable {
  let ptr: Builtin.RawPointer?
  init(_ k: Container) {
    ptr = Builtin.bridgeToRawPointer(k)
  }
  init<T>(_ k: GenericContainer<T>) {
    ptr = Builtin.bridgeToRawPointer(k)
  }
  init(_ k: borrowing MoveOnlyStruct) {
    ptr = nil
  }
  init(_ ptr: Builtin.RawPointer?) {
    self.ptr = ptr
  }
}

struct WrapperStruct : ~Escapable{
  let o: View
  init(_ o: View) {
    self.o = o
  }
}

func use(_ o : View) {}

func usetuple(_ o : (View, View)) {}

func usestruct(_ w : WrapperStruct) {}

func blackhole<T : ~Escapable>(_ t : T) {}

func getView(_ x: _resultDependsOn Container) -> View {
  return View(x)
}

func identity(_ x: _resultDependsOn View) -> View {
  return x
}

func derive(_ x: _resultDependsOn View) -> View {
  return View(x.ptr)
}

func consume(_ view: consuming View) {}

func borrowAndCreate(_ view: _resultDependsOn borrowing View) -> View {
  return View(view.ptr)
}

func consumeAndCreate(_ view: _resultDependsOn consuming View) -> View {
  return View(view.ptr)
}

func consumeAndModify(_ view: consuming View, _ container: inout Container) {}

func accessLexicalView(_ container: inout Container) {
  let initialView = getView(container)
  let derivedView = derive(initialView)
// TODO: hasKnownLifetime returns false due to the forwardingConsume use here, and diagnostics throw error
//  consume(derivedView)
}

func accessLexicalViewMaybeConflict(_ container: inout Container) {
  let initialView = getView(container)
  let borrowedView = borrowAndCreate(initialView)
// TODO: confirm this is an error for all lifetime dependence specifiers on the view argument 
// consumeAndModify(borrowedView, &container)
}

func accessViewNoConflict(container: inout Container) {
  consumeAndModify(consumeAndCreate(getView(container)), &container)
}

func getGenericDependentResult<T>(_ x: _resultDependsOn GenericContainer<T>) -> View {
  return View(x)
}

func getMoveOnlyDependentResult(_ x: _resultDependsOn borrowing MoveOnlyStruct) -> View {
  return View(x)
}

var global_k = Container()
var global_generic_k = GenericContainer<Int>()
var global_moveonly_k = MoveOnlyStruct()

// Tests with permutations of let/var/argument/global/generic type/move only type/transitive lifetime dependence/store uses/ownership modifiers
func testBasicBorrowLifetime1() {
  var x = Container()
  let view = getView(x)
  use(view)
}

func testBasicBorrowLifetime2() {
  let x = Container()
  let view = getView(x)
  use(view)
}

func testBasicBorrowLifetime3(_ x: Container) {
  let view = getView(x)
  use(view)
}

func testBasicBorrowLifetime4() {
  let view = getView(global_k)
  use(view)
}

func testBasicBorrowLifetime5() {
  var x = Container()
  let view1 = getView(x)
  let view2 = identity(view1)
  use(view2)
}

/*
// TODO : Improve diagnostics for store uses
// newview = view raises an error
func testBasicBorrowLifetime6() {
  var x = Container()
  let view = getView(x)
  var newview = view
}
*/

func testBasicBorrowLifetime7() {
  var x = Container()
  let view = getView(x)
  usetuple((view, view))
}

/*
// TODO: Confirm if this is invalid
// hasKnownLifetime returns false due to forwardingConsume at the WrapperStruct's init
func testBasicBorrowLifetime8() {
  var x = Container()
  let view = getView(x)
  usestruct(WrapperStruct(view))
}
*/

func testGenericBorrowLifetime1() {
  var x = GenericContainer<Int>()
  let view = getGenericDependentResult(x)
  use(view)
}

func testGenericBorrowLifetime2() {
  let x = GenericContainer<Int>()
  let view = getGenericDependentResult(x)
  use(view)
}

func testGenericBorrowLifetime3<T>(_ x: GenericContainer<T>) {
  let view = getGenericDependentResult(x)
  use(view)
}

func testGenericBorrowLifetime4() {
  var x = GenericContainer<Int>()
  let view1 = getGenericDependentResult(x)
  let view2 = identity(view1)
  use(view2)
}

func testGenericBorrowLifetime5() {
  let view = getGenericDependentResult(global_generic_k)
  use(view)
}

func testMoveOnlyBorrowLifetime1() {
  var x = MoveOnlyStruct()
  let view = getMoveOnlyDependentResult(x)
  use(view)
}

/*
// TODO: computeIndirectResultRange is unimplemented
// Here base is a project_box instruction
func testMoveOnlyBorrowLifetime2() {
  let x = MoveOnlyStruct()
  let view = getMoveOnlyDependentResult(x)
  use(view)
}
*/

func testMoveOnlyBorrowLifetime3(_ x: borrowing MoveOnlyStruct) {
  let view = getMoveOnlyDependentResult(x)
  use(view)
}

func testMoveOnlyBorrowLifetime4() {
  var x = MoveOnlyStruct()
  let view1 = getMoveOnlyDependentResult(x)
  let view2 = identity(view1)
  use(view2)
}

func testMoveOnlyBorrowLifetime5() {
  let view = getMoveOnlyDependentResult(global_moveonly_k)
  use(view)
}

