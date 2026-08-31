// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-large-loadable-types-address-lowering -module-name m %s -o /dev/null
//
// REQUIRES: CPU=arm64 || CPU=x86_64
//
// A multi-result function may return a tuple that is not a `tuple` instruction
// feeding the return -- e.g. it directly returns another multi-result call.
// OpaqueValueVisitor::canonicalizeReturnValues asserted that any single-use
// owned returned tuple was already the pseudo-return value
// (isPseudoReturnValue), which is false for a call-result tuple. It now
// canonicalizes any non-pseudo returned tuple by destructuring and rebuilding
// it, instead of asserting.

final class C { var n = 0 }

@inline(never) func pair() -> (C, C) { (C(), C()) }

@inline(never) func f() -> (C, C) { return pair() }

func sink() { _ = f() }
