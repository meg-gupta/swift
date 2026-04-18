// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default

import VirtualMethods

let _ = Base() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}
let _ = DerivedInt()

func callVirtualRenamedMethod(_ b: Base) {
  b.virtualRename() // expected-error {{has no member 'virtualRename'}}
  b.swiftVirtualRename()
}

func callVirtualRenamedMethod(_ d: DerivedInt) {
  d.virtualRename() // expected-error {{has no member 'virtualRename'}}
  d.swiftVirtualRename()
}

let _ = Base2() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}

let _ = Derived2()
let _ = Derived3()
let _ = Derived4()
let _ = DerivedFromDerived2()

VirtualNonAbstractBase().nonAbstractMethod()
