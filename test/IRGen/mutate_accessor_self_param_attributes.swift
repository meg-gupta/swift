// RUN: %target-swift-frontend -emit-irgen -module-name test %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

// The address result of a mutate accessor points into its 'self', so 'self' must not be 'captures(none)'.

public final class X {}

public struct S {
  var _x: X
  var _i: Int

  public var i: Int {
    borrow { return _i }
    mutate { return &_i }
  }
}

public struct AddressOnly {
  let _storage: UnsafeMutablePointer<Int>
  var _extra: Any

  public var i: Int {
    @_unsafeSelfDependentResult
    borrow { return _storage.pointee }
    @_unsafeSelfDependentResult
    nonmutating mutate { return &_storage.pointee }
  }
}

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s4test1SV1iSivz"(ptr noalias swiftself align 8 dereferenceable(16) %0)
// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s4test11AddressOnlyV1iSivz"(ptr noalias swiftself align 8 dereferenceable(40) %0)
