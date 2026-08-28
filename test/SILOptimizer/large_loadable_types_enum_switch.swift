// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Switching over an owned enum with a large-loadable payload must be lowered by
// the large-loadable-types address lowering pass. Previously the switch_enum
// case payload was mishandled by UseRewriter::visitSwitchEnumInst, which
// checked isAddressOnly instead of needsLowering: a trivial-but-large payload
// was not marked rewritten even though it was in the value storage map, leaving
// a dangling storage entry that was later dereferenced (SIGSEGV).

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

enum E {
  case big(Large)
  case small(Int)
}

// The enum is lowered to memory and switched with switch_enum_addr; the big
// case's large payload is projected with unchecked_take_enum_data_addr and its
// field read through the projected address (not loaded out as a value).
//
// CHECK-LABEL: sil hidden [noinline] @$s1m3sum{{.*}} : $@convention(thin) (@in_guaranteed E) -> Int {
// CHECK:         switch_enum_addr {{%[0-9]+}}, case #E.big!enumelt: {{bb[0-9]+}}, case #E.small!enumelt: {{bb[0-9]+}}
// CHECK:         [[PAYLOAD:%[0-9]+]] = unchecked_take_enum_data_addr {{%[0-9]+}}, #E.big!enumelt
// CHECK:         struct_element_addr [[PAYLOAD]], #Large.a
// CHECK:       } // end sil function
@inline(never) func sum(_ e: consuming E) -> Int {
  switch consume e {
  case .big(let l): return l.a
  case .small(let i): return i
  }
}

func sink(_ e: consuming E) { _ = sum(e) }
