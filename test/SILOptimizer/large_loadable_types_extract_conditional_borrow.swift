// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s -o /dev/null
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// Extracting a loadable (non-trivial) field from a large-loadable value that is
// passed @guaranteed produces a load_borrow of that field. When the extract
// occurs in a conditionally executed block (so it does not dominate the
// function's common exit), the borrow of the loaded field must be ended at the
// field value's own liveness boundary -- not at the enclosing guaranteed
// value's scope-ending uses.
//
// emitExtract ended the load_borrow at the enclosing guaranteed value's
// boundary via emitEndBorrowsAtEnclosingGuaranteedBoundary. For a @guaranteed
// function argument that boundary is every function exit; when the extract sat
// in a conditional block, the resulting end_borrow was not dominated by its
// load_borrow ("instruction isn't dominated by its operand"). It now falls back
// to the liveness-based boundary when the load_borrow does not dominate the
// enclosing scope-ending uses.
//
// This is a reduction of _DebuggerSupport.printForDebuggerImpl (Mirror is
// large-loadable; mirror.children is a loadable AnyCollection extracted inside
// a guard) which broke the stdlib build.

final class Ref { var n = 0 }

// Large-loadable (8 words) with a loadable, non-trivial field.
struct Big {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0
  var ref = Ref()
}

@inline(never) func use(_ r: Ref) {}

// `big` is @guaranteed; `big.ref` is extracted (load_borrow) only in the
// `cond` branch, which does not dominate the merged return block.
@inline(never) func take(_ big: Big, _ cond: Bool) {
  if cond {
    use(big.ref)
  }
}

func sink(_ big: Big, _ cond: Bool) { take(big, cond) }
