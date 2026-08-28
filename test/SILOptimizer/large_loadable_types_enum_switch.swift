// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Switching over an owned enum with a large-loadable payload must be lowered by
// the large-loadable-types address lowering pass. Previously the switch_enum
// case payload was mishandled by UseRewriter::visitSwitchEnumInst, which
// checked isAddressOnly instead of needsLowering: a large payload was not
// marked rewritten even though it was in the value storage map, leaving a
// dangling storage entry that was later dereferenced (SIGSEGV).

class Box { var n = 0 }

struct Large {
  var a = Box(), b = Box(), c = Box(), d = Box(), e = Box()
}

enum E {
  case big(Large)
  case small(Int)
}

// The owned enum is switched with switch_enum_addr; the big case's large
// payload is taken with unchecked_take_enum_data_addr and its field is read
// through the projected address.
//
// CHECK-LABEL: sil hidden [noinline] @$s1m4pick{{.*}} : $@convention(thin) (@in E) -> @owned Box {
// CHECK:         switch_enum_addr {{%[0-9]+}}, case #E.big!enumelt: {{bb[0-9]+}}, case #E.small!enumelt: {{bb[0-9]+}}
// CHECK:         [[PAYLOAD:%[0-9]+]] = unchecked_take_enum_data_addr {{%[0-9]+}}, #E.big!enumelt
// CHECK:         struct_element_addr [[PAYLOAD]], #Large.a
// CHECK:       } // end sil function
@inline(never) func pick(_ e: consuming E) -> Box {
  switch consume e {
  case .big(let l): return l.a
  case .small: return Box()
  }
}

func sink(_ e: consuming E) { _ = pick(e) }
