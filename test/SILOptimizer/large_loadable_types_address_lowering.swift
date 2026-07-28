// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-large-loadable-types-address-lowering %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Test that the large-loadable-types address lowering pass correctly handles
// mutually-recursive functions and callers of large-loadable functions.

struct LargeTrivial {
  var a: Int = 0
  var b: Int = 0
  var c: Int = 0
  var d: Int = 0
  var e: Int = 0
  var f: Int = 0
  var g: Int = 0
  var h: Int = 0
}

// --- Circular reference: foo and call reference each other ---

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}3foo{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial {
// CHECK:       bb0(%0 : $*LargeTrivial, %1 : $*LargeTrivial):
// CHECK:         function_ref @$s{{.*}}4call{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial
// CHECK:         apply {{%[0-9]+}}(%0, %1) : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial
// CHECK:       } // end sil function
@inline(never)
func foo(_ g: LargeTrivial) -> LargeTrivial {
  return call(g)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}4call{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial {
// CHECK:       bb0(%0 : $*LargeTrivial, %1 : $*LargeTrivial):
// CHECK:         function_ref @$s{{.*}}3foo{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial
// CHECK:         apply {{%[0-9]+}}(%0, %1) : $@convention(thin) (@in_guaranteed LargeTrivial) -> @out LargeTrivial
// CHECK:       } // end sil function
@inline(never)
func call(_ g: LargeTrivial) -> LargeTrivial {
  foo(g)
}

// --- Caller whose own type doesn't change but calls a lowered function ---

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}17printFirstElement{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> () {
@inline(never)
func printFirstElement(_ large: LargeTrivial) {
    print(large.a)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}5test1{{.*}} : $@convention(thin) () -> () {
// CHECK:         function_ref @$s{{.*}}17printFirstElement{{.*}} : $@convention(thin) (@in_guaranteed LargeTrivial) -> ()
// CHECK:       } // end sil function
@inline(never)
func test1() {
  let large = LargeTrivial()
  printFirstElement(large)
}

// --- Large non-trivial type ---

struct LargeNonTrivial {
  var a: String = ""
  var b: String = ""
  var c: String = ""
  var d: String = ""
  var e: String = ""
  var f: String = ""
  var g: String = ""
  var h: String = ""
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}13nontrivialFoo{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial {
// CHECK:       bb0(%0 : $*LargeNonTrivial, %1 : $*LargeNonTrivial):
// CHECK:         function_ref @$s{{.*}}14nontrivialCall{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial
// CHECK:         apply {{%[0-9]+}}(%0, %1) : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial
// CHECK:       } // end sil function
@inline(never)
func nontrivialFoo(_ g: LargeNonTrivial) -> LargeNonTrivial {
  return nontrivialCall(g)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}14nontrivialCall{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial {
// CHECK:       bb0(%0 : $*LargeNonTrivial, %1 : $*LargeNonTrivial):
// CHECK:         function_ref @$s{{.*}}13nontrivialFoo{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial
// CHECK:         apply {{%[0-9]+}}(%0, %1) : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial
// CHECK:       } // end sil function
@inline(never)
func nontrivialCall(_ g: LargeNonTrivial) -> LargeNonTrivial {
  nontrivialFoo(g)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}27printFirstNonTrivialElement{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> () {
@inline(never)
func printFirstNonTrivialElement(_ large: LargeNonTrivial) {
    print(large.a)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}5test2{{.*}} : $@convention(thin) () -> () {
// CHECK:         function_ref @$s{{.*}}27printFirstNonTrivialElement{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> ()
// CHECK:       } // end sil function
@inline(never)
func test2() {
  let large = LargeNonTrivial()
  printFirstNonTrivialElement(large)
}

// --- Large non-trivial type with class references ---

class Klass {
  var x: Int = 0
}

struct LargeNonTrivialWithRef {
  var a: Klass = Klass()
  var b: Klass = Klass()
  var c: Klass = Klass()
  var d: Klass = Klass()
  var e: Klass = Klass()
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}11refIdentity{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivialWithRef) -> @out LargeNonTrivialWithRef {
// CHECK:       bb0(%0 : $*LargeNonTrivialWithRef, %1 : $*LargeNonTrivialWithRef):
// CHECK:       } // end sil function
@inline(never)
func refIdentity(_ g: LargeNonTrivialWithRef) -> LargeNonTrivialWithRef {
  return g
}

// --- Modifying a large non-trivial type (inout) ---

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}13modifyInPlace{{.*}} : $@convention(thin) (@inout LargeNonTrivial) -> () {
@inline(never)
func modifyInPlace(_ x: inout LargeNonTrivial) {
  x.a = "modified"
}

// --- Chained function calls with large non-trivial types ---

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}11chainedCall{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @out LargeNonTrivial {
@inline(never)
func chainedCall(_ x: LargeNonTrivial) -> LargeNonTrivial {
  return nontrivialFoo(nontrivialCall(x))
}

// --- Array of large non-trivial types ---

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}15arrayOfLargeVal{{.*}} : $@convention(thin) (@in_guaranteed LargeNonTrivial) -> @owned Array<LargeNonTrivial> {
@inline(never)
func arrayOfLargeVal(_ x: LargeNonTrivial) -> [LargeNonTrivial] {
  return [x, x, x]
}
