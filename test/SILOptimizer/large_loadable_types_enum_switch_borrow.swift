// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s | %FileCheck %s
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Switching over a borrowing (@in_guaranteed) enum with a large-loadable
// payload must not consume the borrowed enum. Previously the payload was
// projected with a destructive unchecked_take_enum_data_addr, which the
// verifier rejected as a consuming use of an @in_guaranteed parameter. The
// pass now projects the payload non-destructively: through a scratch buffer
// (unchecked_borrow_enum_data_addr) for destructive enum layouts, and a
// non-trivial small payload is read with a borrow rather than a take.

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

enum Trivial {
  case big(Large)
  case small(Int)
}

// The borrowed enum is switched with switch_enum_addr and its large payload is
// projected non-destructively into a scratch buffer, then read through the
// address without taking (no unchecked_take_enum_data_addr).
//
// CHECK-LABEL: sil hidden [noinline] @$s1m4read{{.*}} : $@convention(thin) (@in_guaranteed Trivial) -> Int {
// CHECK:         [[SCRATCH:%[0-9]+]] = alloc_stack $Trivial
// CHECK:         switch_enum_addr %0, case #Trivial.big!enumelt: {{bb[0-9]+}}, case #Trivial.small!enumelt: {{bb[0-9]+}}
// CHECK:         [[PAYLOAD:%[0-9]+]] = unchecked_borrow_enum_data_addr %0, #Trivial.big!enumelt in [[SCRATCH]]
// CHECK:         struct_element_addr [[PAYLOAD]], #Large.a
// CHECK-NOT:     unchecked_take_enum_data_addr
// CHECK:       } // end sil function
@inline(never) func read(_ e: borrowing Trivial) -> Int {
  switch e {
  case .big(let l): return l.a
  case .small(let i): return i
  }
}

// A borrowed enum with a non-trivial small payload: the large payload still
// uses the borrow projection, and the non-trivial payload is read with a
// load_borrow rather than a consuming load [take].
enum WithRef {
  case big(Large)
  case ref(AnyObject)
}

// CHECK-LABEL: sil hidden [noinline] @$s1m7extract{{.*}} : $@convention(thin) (@in_guaranteed WithRef) -> Int {
// CHECK:         unchecked_borrow_enum_data_addr %0, #WithRef.big!enumelt
// CHECK-NOT:     unchecked_take_enum_data_addr
// CHECK:       } // end sil function
@inline(never) func extract(_ e: borrowing WithRef) -> Int {
  switch e {
  case .big(let l): return l.a
  case .ref: return 0
  }
}

func sink(_ t: borrowing Trivial, _ w: borrowing WithRef) {
  _ = read(t)
  _ = extract(w)
}
