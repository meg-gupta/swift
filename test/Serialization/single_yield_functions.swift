// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/single_yield_functions_other.swift \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -enable-experimental-feature CoroutineFunctions \
// RUN:   -disable-experimental-parser-round-trip -experimental-skip-all-function-bodies
// RUN: %target-swift-ide-test -print-module -module-to-print=single_yield_functions_other \
// RUN:   -I %t -source-filename=%s \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -enable-experimental-feature CoroutineFunctions | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions
// REQUIRES: swift_feature_CoroutineFunctions

// A module records whether each coroutine is single-yield or '@yield_once'.
// CHECK: func borrowed(_ s: S) -> yield borrow Int
// CHECK: @yield_once func coro(_ x: inout Int) yields (inout Int) -> Int
// CHECK: func mutable(_ s: inout S) -> yield inout Int
// CHECK: func owned(_ s: S) -> yield Box
