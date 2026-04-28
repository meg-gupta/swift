// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/ResilientLib.swiftmodule -module-name=ResilientLib %S/Inputs/resilient_consuming_getter_nonescapable.swift -enable-experimental-feature Lifetimes -enable-experimental-feature UnderscoreOwned
// RUN: %target-swift-emit-silgen %s -I %t -enable-experimental-feature Lifetimes -enable-experimental-feature UnderscoreOwned | %FileCheck %s
// RUN: %target-swift-emit-sil %s -I %t -enable-experimental-feature Lifetimes -enable-experimental-feature UnderscoreOwned -verify

import ResilientLib

// Verify that calling a consuming getter on a noncopyable l-value from a resilient library
// gets a `mark_unresolved_non_copyable_value` and compiles.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}9takeThing{{.*}} : $@convention(thin) (@in Thing) -> ()
func takeThing(out: consuming NCNE) {
  // CHECK: [[ACCESS:%.*]] = begin_access [read]
  // CHECK: [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
  // CHECK: [[TEMP:%.*]] = alloc_stack $Thing
  // CHECK: copy_addr [[MARKED]] to [init] [[TEMP]]
  // CHECK: [[GETTER:%.*]] = function_ref @$s12ResilientLib5ThingV5bytess11MutableSpanVySiGvg
  // CHECK: apply [[GETTER]]([[TEMP]])
  let bytes = out.bytes
  _ = consume bytes
}
