// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Projecting a large-loadable field out of a large-loadable aggregate with
// struct_extract must be lowered by the large-loadable-types address lowering
// pass. Previously the struct_extract (a def-projection) was mishandled by
// UseRewriter::emitExtract, which checked isAddressOnly instead of
// needsLowering: a trivial-but-large field took the "load the value" path and
// was never marked rewritten, so it later crashed in DefRewriter with
// "Unimplemented opaque value def".

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

struct Pair {
  var x = Large()
  var y = Large()
}

@inline(never) func makePair() -> Pair { Pair() }

// The field is projected with struct_element_addr and copied into the @out
// result. It must NOT be loaded out as a value.
//
// CHECK-LABEL: sil hidden [noinline] @$s1m9firstHalfAA5LargeVyF : $@convention(thin) () -> @out Large {
// CHECK:       bb0([[OUT:%[0-9]+]] : $*Large):
// CHECK:         [[PAIR:%[0-9]+]] = alloc_stack $Pair
// CHECK:         apply {{%[0-9]+}}([[PAIR]]) : $@convention(thin) () -> @out Pair
// CHECK:         [[FIELD:%[0-9]+]] = struct_element_addr [[PAIR]], #Pair.x
// CHECK:         copy_addr [[FIELD]] to [init] [[OUT]]
// CHECK-NOT:     struct_extract
// CHECK:       } // end sil function '$s1m9firstHalfAA5LargeVyF'
@inline(never) func firstHalf() -> Large {
  return makePair().x
}

// Projecting a large field out of a borrowing (@in_guaranteed) argument must
// copy, never take, so the borrowed parameter is not consumed.
//
// CHECK-LABEL: sil hidden [noinline] @$s1m10firstFieldyAA5LargeVAA4PairVF : $@convention(thin) (@in_guaranteed Pair) -> @out Large {
// CHECK:       bb0([[OUT:%[0-9]+]] : $*Large, [[P:%[0-9]+]] : $*Pair):
// CHECK:         [[FIELD:%[0-9]+]] = struct_element_addr [[P]], #Pair.x
// CHECK:         copy_addr [[FIELD]] to [init] [[OUT]]
// CHECK-NOT:     copy_addr [take] [[FIELD]]
// CHECK:       } // end sil function '$s1m10firstFieldyAA5LargeVAA4PairVF'
@inline(never) func firstField(_ p: Pair) -> Large { return p.x }

func sink(_ p: Pair) {
  _ = firstHalf()
  _ = firstField(p)
}
