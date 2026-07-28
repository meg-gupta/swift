// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-large-loadable-types-address-lowering -target %target-cpu-apple-macosx26.0 %s | %FileCheck %s
//
// REQUIRES: OS=macosx
//
// Test that the large-loadable-types address lowering pass correctly handles
// InlineArray (BuiltinFixedArray) types: borrowing parameters must not generate
// copy_addr [take] on the @in_guaranteed argument, and applies with
// pre-existing @out results must not be re-processed.

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}4sum1{{.*}} : $@convention(thin) (@in_guaranteed InlineArray<512, Int>) -> Int {
// CHECK:       bb0(%0 : @noImplicitCopy $*InlineArray<512, Int>):
// CHECK-NOT:     copy_addr [take] %0
// CHECK:       } // end sil function
@inline(never)
func sum1(_ value: borrowing [512 of Int]) -> Int {
    var sum = 0
    for i in value.indices {
        sum &+= value[i]
    }
    return sum
}

// CHECK-LABEL: sil hidden {{.*}} @$s{{.*}}4sum2{{.*}} : $@convention(thin) () -> Int {
// CHECK-NOT:     copy_addr
// CHECK:       } // end sil function
func sum2() -> Int {
    var value = [512 of Int](repeating: 0)
    var sum = 0
    for i in value.indices {
        value[i] = i
    }
    for i in value.indices {
        sum &+= value[i]
    }
    return sum
}

@inline(never)
func sum3() -> Int {
    var value = [512 of Int](repeating: 0)
    for i in value.indices {
        value[i] = i
    }
    return sum1(value)
}

// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}15innefficientSum{{.*}} : $@convention(thin) (@in_guaranteed InlineArray<512, Int>, @inout InlineArray<512, Int>) -> Int {
// CHECK:       bb0(%0 : $*InlineArray<512, Int>, %1 : $*InlineArray<512, Int>):
// CHECK:       } // end sil function
@inline(never)
func innefficientSum(_ value1: [512 of Int], _ value2: inout [512 of Int]) -> Int {
    let count = value1.count
    precondition(count == value2.count)
    var sum = 0
    for i in value1.indices {
        sum &+= value1[i] + value2[i]
    }
    return sum
}

// CHECK-LABEL: sil hidden {{.*}} @$s{{.*}}4sum4{{.*}} : $@convention(thin) () -> Int {
// CHECK:         copy_addr
// CHECK:       } // end sil function
func sum4() -> Int {
    var value = [512 of Int](repeating: 0)
    for i in value.indices {
        value[i] = i
    }
    return innefficientSum(value, &value)
}
