//===----------------------------------------------------------------------===//
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

import Builtin

// MARK: - Memory layout

/// A 128-bit signed integer type.
@frozen
public struct Int128 {
  //  On 64-bit platforms (including arm64_32 and any similar targets with
  //  32b pointers but HW-backed 64b integers), the layout is simply that
  //  of `Builtin.Int128`.
#if _pointerBitWidth(_64) || arch(arm64_32)
  public var _value: Builtin.Int128
  
  @_transparent
  public init(_ _value: Builtin.Int128) {
    self._value = _value
  }
  
  @usableFromInline @_transparent
  package var low: UInt64 {
    UInt64(truncatingIfNeeded: self)
  }
  
  @usableFromInline @_transparent
  package var high: Int64 {
    Int64(truncatingIfNeeded: self &>> 64)
  }
  
  @usableFromInline @_transparent
  package init(low: UInt64, high: Int64) {
#if _endian(little)
    self = unsafeBitCast((low, high), to: Int128.self)
#else
    self = unsafeBitCast((high, low), to: Int128.self)
#endif
  }

#else
  //  On 32-bit platforms, we don't want to use Builtin.Int128 for layout
  //  because it would be 16B aligned, which is excessive for such targets
  //  (and generally incompatible with C's `_BitInt(128)`). Instead we lay
  //  out the type as two `UInt64` fields--note that we have to be careful
  //  about endianness in this case.
#if _endian(little)
  @usableFromInline package var low: UInt64
  @usableFromInline package var high: Int64
#else
  @usableFromInline package var high: Int64
  @usableFromInline package var low: UInt64
#endif
  
  @usableFromInline @_transparent
  package init(low: UInt64, high: Int64) {
    self.low = low
    self.high = high
  }
  
  public var _value: Builtin.Int128 {
    @_transparent get { unsafeBitCast(self, to: Builtin.Int128.self) }
    @_transparent set { self = Self(newValue) }
  }
  
  @_transparent
  public init(_ _value: Builtin.Int128) {
    self = unsafeBitCast(_value, to: Self.self)
  }
#endif
  
  @_transparent
  public init(bitPattern: UInt128) {
    self.init(bitPattern._value)
  }
}

// MARK: - Constants

extension Int128 {
  @_transparent
  public static var zero: Self {
    Self(Builtin.zeroInitializer())
  }
  
  @_transparent
  public static var min: Self {
    Self(low: .zero, high: .min)
  }
  
  @_transparent
  public static var max: Self {
    Self(low: .max, high: .max)
  }
}

// MARK: - Conversions from other integers

extension Int128: ExpressibleByIntegerLiteral,
                  _ExpressibleByBuiltinIntegerLiteral {
  
  public typealias IntegerLiteralType = Self
  
  @_transparent
  public init(_builtinIntegerLiteral x: Builtin.IntLiteral) {
    self.init(Builtin.s_to_s_checked_trunc_IntLiteral_Int128(x).0)
  }
  
  @inlinable
  public init?<T>(exactly source: T) where T: BinaryInteger {
    guard let high = Int64(exactly: source >> 64) else { return nil }
    let low = UInt64(truncatingIfNeeded: source)
    self.init(low: low, high: high)
  }
  
  @inlinable
  public init<T>(_ source: T) where T: BinaryInteger {
    guard let value = Self(exactly: source) else {
      fatalError("value cannot be converted to Self because it is outside the representable range")
    }
    self = value
  }
  
  @inlinable
  public init<T>(clamping source: T) where T: BinaryInteger {
    guard let value = Self(exactly: source) else {
      self = source < .zero ? .min : .max
      return
    }
    self = value
  }
  
  @inlinable
  public init<T>(truncatingIfNeeded source: T) where T: BinaryInteger {
    let high = Int64(truncatingIfNeeded: source >> 64)
    let low = UInt64(truncatingIfNeeded: source)
    self.init(low: low, high: high)
  }
  
  @inlinable
  public init<T>(_truncatingBits source: T) where T: BinaryInteger {
    self.init(T(truncatingIfNeeded: source))
  }
}

// MARK: - Conversions from Binary floating-point
extension Int128 {
  @inlinable
  public init?<T>(exactly source: T) where T: BinaryFloatingPoint {
    let highAsFloat = (source * 0x1.0p-64).rounded(.towardZero)
    guard let high = Int64(exactly: highAsFloat) else { return nil }
    guard let low = UInt64(
      exactly: high == 0 ? source : source - 0x1.0p64*highAsFloat
    ) else { return nil }
    self.init(low: low, high: high)
  }
  
  @inlinable
  public init<T>(_ source: T) where T: BinaryFloatingPoint {
    guard let value = Self(exactly: source.rounded(.towardZero)) else {
      fatalError("value cannot be converted to Self because it is outside the representable range")
    }
    self = value
  }
}

// MARK: - Non-arithmetic utility conformances
extension Int128: Equatable {
  @inlinable
  public static func ==(a: Self, b: Self) -> Bool {
    // This "should" use the unlabeled init, but that's stdlib internal.
    // _builtinBooleanLiteral is identical and public.
    Bool(_builtinBooleanLiteral: Builtin.cmp_eq_Int128(a._value, b._value))
  }
}

extension Int128: Comparable {
  @inlinable
  public static func <(a: Self, b: Self) -> Bool {
    // This "should" use the unlabeled init, but that's stdlib internal.
    // _builtinBooleanLiteral is identical and public.
    Bool(_builtinBooleanLiteral: Builtin.cmp_slt_Int128(a._value, b._value))
  }
}

extension Int128: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(low)
    hasher.combine(high)
  }
}

extension Int128: Codable {
  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(low)
    try container.encode(high)
  }
  
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let low = try container.decode(UInt64.self)
    let high = try container.decode(Int64.self)
    self.init(low: low, high: high)
  }
}

// MARK: - Overflow-reporting arithmetic
extension Int128 {
  @inlinable
  public func addingReportingOverflow(
    _ other: Self
  ) -> (partialValue: Self, overflow: Bool) {
    let (newValue, overflow) = Builtin.sadd_with_overflow_Int128(
      self._value, other._value, Builtin.zeroInitializer()
    )
    return (
      partialValue: Self(newValue),
      overflow: Bool(_builtinBooleanLiteral: overflow)
    )
  }
  
  @inlinable
  public func subtractingReportingOverflow(
    _ other: Self
  ) -> (partialValue: Self, overflow: Bool) {
    let (newValue, overflow) = Builtin.ssub_with_overflow_Int128(
      self._value, other._value, Builtin.zeroInitializer()
    )
    return (
      partialValue: Self(newValue),
      overflow: Bool(_builtinBooleanLiteral: overflow)
    )
  }
  
  public func multipliedReportingOverflow(
    by other: Self
  ) -> (partialValue: Self, overflow: Bool) {
    let (newValue, overflow) = Builtin.smul_with_overflow_Int128(
      self._value, other._value, Builtin.zeroInitializer()
    )
    return (
      partialValue: Self(newValue),
      overflow: Bool(_builtinBooleanLiteral: overflow)
    )
  }
  
  public func dividedReportingOverflow(
    by other: Self
  ) -> (partialValue: Self, overflow: Bool) {
    precondition(other != .zero, "Division by zero")
    if self == .min && other == -1 { return (.min, true) }
    return (Self(Builtin.sdiv_Int128(self._value, other._value)), false)
  }
  
  
  public func remainderReportingOverflow(
    dividingBy other: Self
  ) -> (partialValue: Self, overflow: Bool) {
    precondition(other != .zero, "Remainder dividing by zero.")
    if self == .min && other == -1 { return (0, true) }
    return (Self(Builtin.srem_Int128(self._value, other._value)), false)
  }
}

// MARK: - AdditiveArithmetic conformance

extension Int128: AdditiveArithmetic {
  @inlinable
  public static func +(a: Self, b: Self) -> Self {
    let (result, overflow) = a.addingReportingOverflow(b)
    // On arm64, this check materializes the carryout in register, then does
    // a TBNZ, where we should get a b.cs instead. I filed rdar://115387277
    // to track this, but it only costs us one extra instruction, so we'll
    // keep it as is for now.
    precondition(!overflow)
    return result
  }
  
  @inlinable
  public static func -(a: Self, b: Self) -> Self {
    let (result, overflow) = a.subtractingReportingOverflow(b)
    precondition(!overflow)
    return result
  }
  
  @inlinable
  public static func &+(a: Self, b: Self) -> Self {
    a.addingReportingOverflow(b).partialValue
  }
  
  @inlinable
  public static func &-(a: Self, b: Self) -> Self {
    a.subtractingReportingOverflow(b).partialValue
  }
  
  @inlinable
  public static func &+=(a: inout Self, b: Self) { a = a &+ b }
  
  @inlinable
  public static func &-=(a: inout Self, b: Self) { a = a &- b }
}

// MARK: - Multiplication and division

extension Int128 {
  public static func *(a: Self, b: Self) -> Self {
    let (result, overflow) = a.multipliedReportingOverflow(by: b)
    precondition(!overflow)
    return result
  }
  
  @inlinable
  public static func *=(a: inout Self, b: Self) { a = a * b }
  
  public static func /(a: Self, b: Self) -> Self {
    return a.dividedReportingOverflow(by: b).partialValue
  }
  
  public static func /=(a: inout Self, b: Self) { a = a / b }
  
  public static func %(a: Self, b: Self) -> Self {
    return a.remainderReportingOverflow(dividingBy: b).partialValue
  }
  
  public static func %=(a: inout Self, b: Self) { a = a % b }
}

// MARK: - Numeric conformance

extension Int128: Numeric {
  public typealias Magnitude = UInt128
  
  @inlinable
  public var magnitude: Magnitude {
    let unsignedSelf = UInt128(truncatingIfNeeded: self)
    return self < 0 ? 0 &- unsignedSelf : unsignedSelf
  }
}

// MARK: - BinaryInteger conformance

extension Int128: BinaryInteger {
  
  @inlinable
  public var words: UInt128.Words {
    Words(_value: UInt128(_value))
  }
  
  public static prefix func ~(a: Self) -> Self {
    return Self(low: ~a.low, high: ~a.high)
  }
  
  public static func &=(a: inout Self, b: Self) {
    a._value = Builtin.and_Int128(a._value, b._value)
  }
  
  public static func |=(a: inout Self, b: Self) {
    a._value = Builtin.or_Int128(a._value, b._value)
  }
  
  public static func ^=(a: inout Self, b: Self) {
    a._value = Builtin.xor_Int128(a._value, b._value)
  }
  
  public static func &>>=(a: inout Self, b: Self) {
    let masked = b & 127
    a._value = Builtin.ashr_Int128(a._value, masked._value)
  }
  
  public static func &<<=(a: inout Self, b: Self) {
    let masked = b & 127
    a._value = Builtin.shl_Int128(a._value, masked._value)
  }
  
  public var trailingZeroBitCount: Int {
    low == 0 ? 64 + high.trailingZeroBitCount : low.trailingZeroBitCount
  }
}

// MARK: - FixedWidthInteger conformance

extension Int128: FixedWidthInteger, SignedInteger {
  
  @_transparent
  static public var bitWidth: Int { 128 }
  
  public var nonzeroBitCount: Int {
    high.nonzeroBitCount &+ low.nonzeroBitCount
  }
  
  public var leadingZeroBitCount: Int {
    high == 0 ? 64 + low.leadingZeroBitCount : high.leadingZeroBitCount
  }
  
  public var byteSwapped: Self {
    return Self(low: UInt64(truncatingIfNeeded: high.byteSwapped),
                high: Int64(truncatingIfNeeded: low.byteSwapped))
  }
}
