// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-silgen %t/callee.swift -module-name Lib \
// RUN:     -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN: | %FileCheck %t/callee.swift

// RUN: %target-swift-emit-silgen %t/caller.swift -module-name Lib -verify \
// RUN:     -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN: | %FileCheck %t/caller.swift

// RUN: %target-swift-emit-silgen %t/getter.swift -module-name Lib -verify \
// RUN:     -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN: | %FileCheck %t/getter.swift

// RUN: %target-swift-emit-silgen %t/protocol.swift -module-name Lib \
// RUN:     -enable-library-evolution -target %target-future-triple \
// RUN:     -enable-callee-allocated-coro-abi -enable-experimental-feature ThrowingCoroutineAccessors \
// RUN: | %FileCheck %t/protocol.swift

// REQUIRES: swift_feature_ThrowingCoroutineAccessors

//--- callee.swift
func mayThrow() throws {}

struct NC: ~Copyable {
  var v: Int
}

struct S: ~Copyable {
  var nc: NC

  // CHECK-LABEL: sil hidden [ossa] @$s3Lib1SV1pAA2NCVvy : $@yield_once_2 @convention(method) ({{.*}}) -> (@yields {{.*}}NC, @error any Error) {
  // CHECK:         try_apply {{%[0-9]+}}() : $@convention(thin) () -> @error any Error, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
  // CHECK-DAG:     yield {{%[0-9]+}}, resume {{bb[0-9]+}}, unwind {{bb[0-9]+}}
  // CHECK-DAG:   [[ERROR]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:         throw {{%[0-9]+}}
  // CHECK-LABEL: } // end sil function '$s3Lib1SV1pAA2NCVvy'
  var p: NC {
    yielding borrow throws {
      try mayThrow()
      yield nc
    }
  }
}

//--- caller.swift
func mayThrow() throws {}

struct NC: ~Copyable {
  var v: Int
}

struct S: ~Copyable {
  var nc: NC

  var p: NC {
    yielding borrow throws {
      try mayThrow()
      yield nc
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s3Lib4read{{.*}} :
// CHECK:         begin_apply [nothrow]
// CHECK-LABEL: } // end sil function '$s3Lib4read{{.*}}'
func read(_ s: borrowing S) throws -> Int {
  return try s.p.v // expected-error {{INTERNAL ERROR: feature not implemented: calling a throwing 'yielding borrow' accessor}}
}

//--- getter.swift
public struct S {
  public var x: Int

  // CHECK-LABEL: sil {{.*}}@$s3Lib1SV1pSivg : $@convention(method) (S) -> (Int, @error any Error) {
  // CHECK:         begin_apply [nothrow]
  // CHECK-LABEL: } // end sil function '$s3Lib1SV1pSivg'
  public var p: Int { // expected-error {{INTERNAL ERROR: feature not implemented: calling a throwing 'yielding borrow' accessor}}
    yielding borrow throws { yield x }
  }
}

//--- protocol.swift
// CHECK-NOT:   #P.p!read
// CHECK-LABEL: sil_default_witness_table P {
// CHECK-NEXT:    no_default
// CHECK-NEXT:  }
public protocol P {
  var p: Int { yielding borrow throws }
}
