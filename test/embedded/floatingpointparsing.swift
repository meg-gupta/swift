// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Check that initializing a Double with a string literal doesn't result in unresolved symbols
@inline(never)
func testLiteral() -> Double? {
  Double("1.5")
}

@main
struct Main {
  static func main() {
    print(testLiteral() == 1.5)
    // CHECK: true
  }
}
