
extension FloatingPoint {
  public static var expOverflowThreshold: Self {
    return Self(Self.greatestFiniteMagnitude.exponent)
  }
}

public func foo() -> Float {
  return Float.expOverflowThreshold
}

public func bar() -> Float {
  return Float(Float.greatestFiniteMagnitude.exponent)
}

