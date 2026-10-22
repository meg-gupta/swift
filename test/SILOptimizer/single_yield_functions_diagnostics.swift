// RUN: %target-swift-frontend -emit-sil -verify -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip %s -o /dev/null

// REQUIRES: swift_feature_SingleYieldFunctions

func noYield(_ b: Bool) -> yield Int {
  if b { // expected-note {{missing yield when the condition is false}}
    yield 0
  }
  return // expected-error {{function must yield on all paths before returning}}
}

func twoYields() -> yield Int {
  yield 0 // expected-note {{previous yield was here}}
  yield 1 // expected-error {{function must not yield more than once}}
}

func returnFirst() -> yield Int {
  return // expected-error {{function must yield before returning}}
}

struct Accessors {
  var x: Int {
    _read {
      return // expected-error {{accessor must yield before returning}}
    }
  }
}
