// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-sil-opaque-values -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A function whose only lowering work is a "nonopaque" checked cast that must
// be rewritten to its address form (checked_cast_br -> checked_cast_addr_br)
// must not be skipped. runAddressLoweringOnFunction bailed out early when the
// value-storage map, indirect applies, and indirect results were all empty,
// which dropped the pending nonopaqueResultUCCs / nonopaqueResultCCBs rewrites
// and left an invalid scalar checked cast that the verifier rejected.

// CHECK-LABEL: sil hidden [noinline] @$s1m1f{{.*}} : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK:         checked_cast_addr_br {{.*}}AnyObject in {{.*}} to any AnyObject.Type in
// CHECK-NOT:     checked_cast_br
// CHECK:       } // end sil function
@inline(never) func f(_ x: AnyObject) -> Bool {
  if let _ = x as? AnyObject.Type { return true }
  return false
}

func sink(_ x: AnyObject) { _ = f(x) }
