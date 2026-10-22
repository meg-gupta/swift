// RUN: %target-swift-emit-silgen -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip -module-name M %s | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions

class Box {}

struct S {
  var box: Box
  var n: Int

  // CHECK-LABEL: sil hidden [ossa] @{{.*}}1SV5owned{{.*}} : $@yield_once_2 @convention(method) (@guaranteed S) -> @yields @owned Box {
  // CHECK:         [[COPY:%.*]] = copy_value
  // CHECK:         yield [[COPY]], resume [[RESUME:bb[0-9]+]], unwind [[UNWIND:bb[0-9]+]]
  // CHECK:       [[RESUME]]:
  // CHECK-NOT:     destroy_value [[COPY]]
  // CHECK:         return
  func owned() -> yield Box { yield box }

  // CHECK-LABEL: sil hidden [ossa] @{{.*}}1SV8borrowed{{.*}} : $@yield_once_2 @convention(method) (@guaranteed S) -> @yields @guaranteed Box {
  // CHECK:         yield {{%.*}}, resume
  func borrowed() -> yield borrow Box { yield box }

  // CHECK-LABEL: sil hidden [ossa] @{{.*}}1SV7mutable{{.*}} : $@yield_once_2 @convention(method) (@inout S) -> @yields @inout Int {
  // CHECK:         [[ACCESS:%.*]] = begin_access [modify]
  // CHECK:         [[ADDR:%.*]] = struct_element_addr [[ACCESS]], #S.n
  // CHECK:         yield [[ADDR]], resume
  mutating func mutable() -> yield inout Int { yield &n }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}6global{{.*}} : $@yield_once_2 @convention(thin) () -> @yields @owned Box {
func global() -> yield Box { yield Box() }

// Address-only (existential) yields are indirect.
protocol P {}
// CHECK-LABEL: sil hidden [ossa] @{{.*}}11existential{{.*}} : $@yield_once_2 @convention(thin) (@in_guaranteed any P) -> @yields @in_guaranteed any P {
func existential(_ p: any P) -> yield borrow any P { yield p }
