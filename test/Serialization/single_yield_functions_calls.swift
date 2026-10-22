// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/single_yield_functions_other.swift \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -enable-experimental-feature CoroutineFunctions \
// RUN:   -disable-experimental-parser-round-trip -experimental-skip-all-function-bodies
// RUN: %target-typecheck-verify-swift -I %t \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -enable-experimental-feature CoroutineFunctions
// RUN: %target-swift-emit-silgen -I %t %s -D SILGEN \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -enable-experimental-feature CoroutineFunctions | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: swift_feature_CoroutineFunctions

import single_yield_functions_other

func use(_ x: Int) {}
func consume(_ b: __owned Box) {}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}9testOwned{{.*}} :
// CHECK:         [[FN:%.*]] = function_ref @{{.*}}5owned{{.*}} : $@yield_once_2 @convention(thin) (@guaranteed S) -> @yields @owned Box
// CHECK:         begin_apply [[FN]](
func testOwned(_ s: S) {
  consume(owned(s))
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}12testBorrowed{{.*}} :
// CHECK:         [[FN:%.*]] = function_ref @{{.*}}8borrowed{{.*}} : $@yield_once_2 @convention(thin) (@guaranteed S) -> @yields Int
// CHECK:         begin_apply [[FN]](
func testBorrowed(_ s: S) {
  use(borrowed(s))
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}11testMutable{{.*}} :
// CHECK:         [[FN:%.*]] = function_ref @{{.*}}7mutable{{.*}} : $@yield_once_2 @convention(thin) (@inout S) -> @yields @inout Int
// CHECK:         begin_apply [[FN]](
func testMutable(_ s: inout S) {
  mutable(&s) += 1
}

#if !SILGEN
// A '@yield_once' function keeps its kind across modules: it isn't a
// single-yield function, so calling it is still unsupported.
func testYieldOnce(_ x: inout Int) {
  _ = coro(&x) // expected-error {{calling a '@yield_once' function is not supported}}
}
#endif
