public struct NCNE: ~Escapable, ~Copyable {
  var _bytes: UnsafeMutableBufferPointer<Int>

  @_owned public var bytes: MutableSpan<Int> {
    @_lifetime(copy self)
    consuming get {
      _overrideLifetime(MutableSpan(_unsafeElements: _bytes), copying: self)
    }
  }
}
