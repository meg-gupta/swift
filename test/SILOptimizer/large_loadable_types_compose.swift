// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Composing an aggregate (struct/tuple/enum) whose field is a large-loadable
// value taken from a borrowed source must not consume the source.
// AddressMaterialization::initializeComposingUse always used copy_addr [take]
// when moving a composed operand into the aggregate storage; when the operand's
// storage was a borrowed @in_guaranteed argument, that was an illegal consuming
// use. It now takes only when the operand is owned.

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

struct Wrap {
  var n: Int
  var big: Large
}

// The large field is copied (not taken) from the @in_guaranteed argument into
// the @out result's field storage.
//
// CHECK-LABEL: sil hidden @$s1m4WrapV1n3big{{.*}} : $@convention(method) (Int, @in_guaranteed Large, @thin Wrap.Type) -> @out Wrap {
// CHECK:         [[FIELD:%[0-9]+]] = struct_element_addr %0, #Wrap.big
// CHECK:         copy_addr {{%[0-9]+}} to [init] [[FIELD]]
// CHECK-NOT:     copy_addr [take] {{%[0-9]+}} to [init] [[FIELD]]
// CHECK:       } // end sil function
@inline(never) func make(_ x: Large) -> Wrap { Wrap(n: 0, big: x) }

func sink(_ x: Large) { _ = make(x) }
