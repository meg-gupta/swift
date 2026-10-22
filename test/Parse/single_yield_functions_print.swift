// RUN: %target-swift-frontend -print-ast -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip %s | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions

struct S {
  var x: Int = 0

  // CHECK: func owned() -> yield Int
  func owned() -> yield Int { yield x }
  // CHECK: func borrowed() -> yield borrow Int
  func borrowed() -> yield borrow Int { yield x }
  // CHECK: mutating func mutable() -> yield inout Int
  mutating func mutable() -> yield inout Int { yield &x }
}
