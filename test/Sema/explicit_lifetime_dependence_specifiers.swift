// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip   -enable-experimental-feature NoncopyableGenerics -enable-builtin-module
import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

func incorrectSelfInvalidLifetimeDependence(_ x: borrowing BufferView) -> _borrow(self) BufferView { //expected-error{{Invalid lifetime dependence specifier, self is valid in non-static methods only}}
  return BufferView(x.ptr)
}

func incorrectParamNameInvalidLifetimeDependence(_ x: borrowing BufferView) -> _borrow(y) BufferView { // expected-error{{Invalid parameter name specified 'y'}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence1(_ x: borrowing BufferView) -> _borrow(x, x) BufferView { // expected-error{{Duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence2(_ x: borrowing BufferView) -> _borrow(x) _borrow(x) BufferView { // expected-error{{Duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence3(_ x: borrowing BufferView) -> _borrow(x) _copy(x) BufferView { // expected-error{{Duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func consumingParamInvalidLifetimeDependence1(_ x: consuming BufferView) -> _borrow(x) BufferView { //expected-error{{Invalid use of borrow lifetime dependence for owned ownership}}
  return BufferView(x.ptr)
}

func consumingParamInvalidLifetimeDependence2(_ x: consuming BufferView) -> _mutate(x) BufferView { //expected-error{{Invalid use of mutate lifetime dependence for owned ownership}}
  return BufferView(x.ptr)
}

func borrowingParamInvalidLifetimeDependence1(_ x: borrowing BufferView) -> _consume(x) BufferView { //expected-error{{Invalid use of consume lifetime dependence for guaranteed ownership}}
  return BufferView(x.ptr)
}

func borrowingParamInvalidLifetimeDependence2(_ x: borrowing BufferView) -> _mutate(x) BufferView { //expected-error{{Invalid use of mutate lifetime dependence for guaranteed ownership}}
  return BufferView(x.ptr)
}

// Sema cannot diagnose this
func implicitBorrowingParamInvalidLifetimeDependence1(_ x: BufferView) -> _consume(x) BufferView {
  return BufferView(x.ptr)
}

// Sema cannot diagnose this
func implicitBorrowingParamInvalidLifetimeDependence2(_ x: BufferView) -> _mutate(x) BufferView {
  return BufferView(x.ptr)
}

func inoutParamInvalidLifetimeDependence1(_ x: inout BufferView) -> _consume(x) BufferView { //expected-error{{Invalid use of consume lifetime dependence for inout ownership}}
  return BufferView(x.ptr)
}

func inoutParamInvalidLifetimeDependence2(_ x: inout BufferView) -> _borrow(x) BufferView { //expected-error{{Invalid use of borrow lifetime dependence for inout ownership}}
  return BufferView(x.ptr)
}

struct Wrapper : ~Escapable {
  let view: BufferView

  borrowing func getView1() -> _borrow(self) BufferView {
    return view
  }
  
  consuming func getView2() -> _consume(self) BufferView {
    return view
  }
  
  mutating func getView3() -> _copy(self) BufferView {
    return view
  }

  borrowing func getView4() -> _copy(self) BufferView {
    return view
  }
  
  borrowing func borrowingMethodInvalidLifetimeDependence1() -> _consume(self) BufferView { // expected-error{{Invalid use of consume lifetime dependence for guaranteed ownership}}
    return view
  }

  borrowing func borrowingMethodInvalidLifetimeDependence2() -> _mutate(self) BufferView { // expected-error{{Invalid use of mutate lifetime dependence for guaranteed ownership}}
    return view
  }

  consuming func consumingMethodInvalidLifetimeDependence1() -> _borrow(self) BufferView { // expected-error{{Invalid use of borrow lifetime dependence for owned ownership}}
    return view
  }

  consuming func consumingMethodInvalidLifetimeDependence2() -> _mutate(self) BufferView { // expected-error{{Invalid use of mutate lifetime dependence for owned ownership}}
    return view
  }

  mutating func mutatingMethodInvalidLifetimeDependence1() -> _borrow(self) BufferView { // expected-error{{Invalid use of borrow lifetime dependence for inout ownership}}
    return view
  }

  mutating func mutatingMethodInvalidLifetimeDependence2() -> _consume(self) BufferView { // expected-error{{Invalid use of consume lifetime dependence for inout ownership}}
    return view
  } 

}

