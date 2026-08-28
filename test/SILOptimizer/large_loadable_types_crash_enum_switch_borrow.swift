// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s -o /dev/null
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Remaining reproducer: switching over a *borrowing* (@in_guaranteed) enum with
// a large-loadable payload. The owned case is handled (see
// large_loadable_types_enum_switch.swift), but the borrowed case still projects
// the payload with a destructive unchecked_take_enum_data_addr, which is an
// illegal consuming use of the borrowed enum:
//
//   SIL verification failed: Found mutating or consuming use of an
//   in_guaranteed parameter?!
//
// The fix needs the non-destructive projection (unchecked_borrow_enum_data_addr
// / load_borrow with matching end_borrows) when the switched enum is borrowed.
//
// Compiles cleanly when the pass is disabled
// (-Xllvm -sil-disable-pass=large-loadable-address-lowering).
//
// XFAIL: *

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

enum E {
  case big(Large)
  case small(Int)
}

@inline(never) func read(_ e: borrowing E) -> Int {
  switch e {
  case .big(let l): return l.a
  case .small(let i): return i
  }
}

func sink(_ e: borrowing E) { _ = read(e) }
