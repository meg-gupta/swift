// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s -o /dev/null
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A protocol witness thunk for a requirement returning an associated type has a
// pre-existing indirect (@out) result. When the witness also refers to a
// large-loadable value, the large-loadable address lowering pass runs
// ReturnRewriter on the thunk. ReturnRewriter paired the lowered function
// type's results (which include the pre-existing @out result) against the
// direct SIL results only, so the two lists had different sizes
// (Assertion: c1.size() == c2.size() in for_each). It now pairs each lowered
// result with its opaque counterpart and skips pre-existing indirect results.

struct Large {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

protocol P {
  associatedtype A
  func get() -> A
}

struct S: P {
  @inline(never) func get() -> Large { Large() }
}

@inline(never) func useP<T: P>(_ t: T) -> T.A { t.get() }

@inline(never) func caller(_ s: S) -> Large { useP(s) }

func sink() { _ = caller(S()) }
