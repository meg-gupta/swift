//===--- Span.swift -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct Span<Element: ~Copyable>: ~Escapable {
  @usableFromInline internal let _pointer: UnsafeRawPointer?
  @usableFromInline internal let _count: Int

  @lifetime(borrow pointer)
  public init(_unchecked pointer: borrowing UnsafeRawPointer?, count: Int) {
    _pointer = copy pointer
    _count = count
  }
}
