// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A multi-result call whose large-loadable results are consumed by
// tuple_extract (rather than a destructure_tuple) must be lowered by the
// large-loadable-types address lowering pass. Previously the tuple_extract def
// hit "Unimplemented opaque value def"; DefRewriter::visitTupleExtractInst now
// normalizes it to the destructure_tuple form and rewrites the call.

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

@inline(never) func pair() -> (Large, Large) { (Large(), Large()) }

// The call is lowered to two indirect (@out) results, written into stack slots,
// and each element is read back through an address projection.
//
// CHECK-LABEL: sil hidden [noinline] @$s1m3useSiyF : $@convention(thin) () -> Int {
// CHECK:         [[X:%[0-9]+]] = alloc_stack $Large
// CHECK:         [[Y:%[0-9]+]] = alloc_stack $Large
// CHECK:         [[F:%[0-9]+]] = function_ref @$s1m4pairAA5LargeV_ADtyF : $@convention(thin) () -> (@out Large, @out Large)
// CHECK:         apply [[F]]([[X]], [[Y]]) : $@convention(thin) () -> (@out Large, @out Large)
// CHECK-NOT:     tuple_extract {{%[0-9]+}} : $(Large, Large)
// CHECK:       } // end sil function '$s1m3useSiyF'
@inline(never) func use() -> Int {
  let (x, y) = pair()
  return x.a &+ y.a
}

func sink() { _ = use() }
