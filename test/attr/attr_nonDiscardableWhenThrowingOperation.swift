// RUN: %target-typecheck-verify-swift

// @_nonDiscardableWhenThrowingOperation requires @discardableResult

@_nonDiscardableWhenThrowingOperation // expected-error {{'@_nonDiscardableWhenThrowingOperation' must be combined with '@discardableResult'}}
func missingDiscardableResult(_ op: () throws -> Void) -> Int { return 0 }

// @_nonDiscardableWhenThrowingOperation requires a closure parameter that uses typed throws

@discardableResult
@_nonDiscardableWhenThrowingOperation // expected-error {{'@_nonDiscardableWhenThrowingOperation' requires the function to have exactly one closure parameter that uses typed throws}}
func noClosureParam() -> Int { return 0 }

@discardableResult
@_nonDiscardableWhenThrowingOperation // expected-error {{'@_nonDiscardableWhenThrowingOperation' requires the function to have exactly one closure parameter that uses typed throws}}
func nonThrowingClosure(_ op: () -> Void) -> Int { return 0 }

// Valid: has @discardableResult and a throwing closure parameter

@discardableResult
@_nonDiscardableWhenThrowingOperation
func validThrowingClosure(_ op: () throws -> Void) -> Int { return 0 }

@discardableResult
@_nonDiscardableWhenThrowingOperation
func validTypedThrowingClosure<E: Error>(_ op: () throws(E) -> Void) -> Int { return 0 }

// Test the behavior: throwing closure warns, non-throwing closure does not

struct MyError: Error {}

func testNonDiscardableWhenThrowingOperation() {
  validThrowingClosure { throw MyError() }
  // expected-warning @-1 {{result of 'validThrowingClosure' is not used, which may accidentally ignore errors thrown by passed-in closure}}
  validThrowingClosure { } // no warning - closure does not throw
  _ = validThrowingClosure { throw MyError() } // no warning - explicitly discarded

  validTypedThrowingClosure { () throws(MyError) in throw MyError() }
  // expected-warning @-1 {{result of 'validTypedThrowingClosure' is not used, which may accidentally ignore errors thrown by passed-in closure}}
  validTypedThrowingClosure { () throws(Never) in } // no warning - throws(Never)
}
