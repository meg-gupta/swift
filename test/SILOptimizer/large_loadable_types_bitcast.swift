// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A bit cast (e.g. unsafeBitCast, or a C union field reinterpret) between two
// large-loadable types must share storage with its operand: the cast is a pure
// reinterpret of the in-memory value. Previously getReusedStorageOperand only
// shared storage when the cast source was address-only (and did not handle
// unchecked_trivial_bit_cast at all), so a large-loadable source got its own
// storage and the cast crashed the pass with "Unimplemented opaque value def" /
// a markRewritten storage conflict.

struct A {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}
struct B {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

// The cast reinterprets the source storage in place with unchecked_addr_cast and
// copies the result into the @out argument.
//
// CHECK-LABEL: sil hidden [noinline] @$s1m6recast{{.*}} : $@convention(thin) (@in_guaranteed A) -> @out B {
// CHECK:         [[VIEW:%[0-9]+]] = unchecked_addr_cast {{%[0-9]+}} to $*B
// CHECK:         copy_addr [[VIEW]] to [init] %0
// CHECK:       } // end sil function
@inline(never) func recast(_ x: A) -> B {
  unsafeBitCast(x, to: B.self)
}

func sink(_ x: A) { _ = recast(x) }
