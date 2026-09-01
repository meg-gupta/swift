// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s -o /dev/null
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A generic function whose result type depends on its generic parameters (and
// is large when substituted with concrete types) is passed via the generic
// calling convention, not the large-loadable indirect convention. The
// large-loadable address lowering pass must not remap such generic-dependent
// results/parameters: the callee's abstract signature is not remapped, so
// remapping the concrete substituted convention at the call site would produce
// a call whose argument count disagrees with the callee (crashing in
// makeIndirectArgs / apply verification). Generic-dependent types (containing
// archetypes) are now left unlowered.
//
// This is a reduction of the standard library's Set.hash(into:) ->
// Set.Iterator.init(_cocoa:) pattern that broke the stdlib build.

struct BigIter<T> {
  var a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0
}

@inline(never) func makeIter<T>(_: T.Type) -> BigIter<T> { BigIter<T>() }

@inline(never) func caller<U>(_: U.Type) -> Int { return makeIter(U.self).a }

func sink<U>(_ t: U.Type) { _ = caller(t) }
