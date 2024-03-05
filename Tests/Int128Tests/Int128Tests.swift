import Int128Demo
import _TestSupport
import XCTest

final class Int128Tests: XCTestCase {
  
  func testLayout() {
    XCTAssertEqual(MemoryLayout<Int128>.size, 16)
    XCTAssertEqual(MemoryLayout<Int128>.stride, 16)
#if _pointerBitWidth(_64) || arch(arm64_32)
    XCTAssertEqual(MemoryLayout<Int128>.alignment, 16)
#else
    XCTAssertEqual(
      MemoryLayout<Int128>.alignment,
      MemoryLayout<UInt64>.alignment
    )
#endif
  }
  
  func testLiterals() {
    XCTAssertEqual(0, Int128.zero)
    XCTAssertEqual(0x7fffffffffffffff_ffffffffffffffff, Int128.max)
    XCTAssertEqual( 170141183460469231731687303715884105727, Int128.max)
    XCTAssertEqual(-170141183460469231731687303715884105728, Int128.min)
    func tenToThe(_ n: Int) -> Int128 {
      let tens: [UInt64] = [
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000,
        10000000000000000000
      ]
      if n <= 19 { return Int128(tens[n]) }
      if n <= 38 {
        let (high, low) = tens[19].multipliedFullWidth(by: tens[n - 19])
        return Int128(low: low, high: Int64(high))
      }
      fatalError()
    }
    
    XCTAssertEqual(1, tenToThe(0))
    XCTAssertEqual(10, tenToThe(1))
    XCTAssertEqual(100, tenToThe(2))
    XCTAssertEqual(1000, tenToThe(3))
    XCTAssertEqual(10000, tenToThe(4))
    XCTAssertEqual(100000, tenToThe(5))
    XCTAssertEqual(1000000, tenToThe(6))
    XCTAssertEqual(10000000, tenToThe(7))
    XCTAssertEqual(100000000, tenToThe(8))
    XCTAssertEqual(1000000000, tenToThe(9))
    XCTAssertEqual(10000000000, tenToThe(10))
    XCTAssertEqual(100000000000, tenToThe(11))
    XCTAssertEqual(1000000000000, tenToThe(12))
    XCTAssertEqual(10000000000000, tenToThe(13))
    XCTAssertEqual(100000000000000, tenToThe(14))
    XCTAssertEqual(1000000000000000, tenToThe(15))
    XCTAssertEqual(10000000000000000, tenToThe(16))
    XCTAssertEqual(100000000000000000, tenToThe(17))
    XCTAssertEqual(1000000000000000000, tenToThe(18))
    XCTAssertEqual(10000000000000000000, tenToThe(19))
    XCTAssertEqual(100000000000000000000, tenToThe(20))
    XCTAssertEqual(1000000000000000000000, tenToThe(21))
    XCTAssertEqual(10000000000000000000000, tenToThe(22))
    XCTAssertEqual(100000000000000000000000, tenToThe(23))
    XCTAssertEqual(1000000000000000000000000, tenToThe(24))
    XCTAssertEqual(10000000000000000000000000, tenToThe(25))
    XCTAssertEqual(100000000000000000000000000, tenToThe(26))
    XCTAssertEqual(1000000000000000000000000000, tenToThe(27))
    XCTAssertEqual(10000000000000000000000000000, tenToThe(28))
    XCTAssertEqual(100000000000000000000000000000, tenToThe(29))
    XCTAssertEqual(1000000000000000000000000000000, tenToThe(30))
    XCTAssertEqual(10000000000000000000000000000000, tenToThe(31))
    XCTAssertEqual(100000000000000000000000000000000, tenToThe(32))
    XCTAssertEqual(1000000000000000000000000000000000, tenToThe(33))
    XCTAssertEqual(10000000000000000000000000000000000, tenToThe(34))
    XCTAssertEqual(100000000000000000000000000000000000, tenToThe(35))
    XCTAssertEqual(1000000000000000000000000000000000000, tenToThe(36))
    XCTAssertEqual(10000000000000000000000000000000000000, tenToThe(37))
    XCTAssertEqual(100000000000000000000000000000000000000, tenToThe(38))
  }
  
  func testConversionsFromBinaryFloatingPoint() {
    XCTAssertEqual(Int128(-1.0.nextDown), .zero)
    XCTAssertEqual(Int128(-Double.leastNonzeroMagnitude), .zero)
    XCTAssertEqual(Int128(Double.leastNonzeroMagnitude), .zero)
    XCTAssertEqual(Int128(1.0.nextDown), .zero)
    XCTAssertEqual(Int128(1.0), 1)
    XCTAssertEqual(Int128(1.0.nextUp), 1)
    XCTAssertEqual(Int128(0x1.0p64.nextDown).high, 0)
    XCTAssertEqual(Int128(0x1.0p64.nextDown).low, 0xffff_ffff_ffff_f800)
    XCTAssertEqual(Int128(0x1.0p64).high, 1)
    XCTAssertEqual(Int128(0x1.0p64).low, 0)
    XCTAssertEqual(Int128(0x1.0p64.nextUp).high, 1)
    XCTAssertEqual(Int128(0x1.0p64.nextUp).low, 0x0000_0000_0000_1000)
    XCTAssertEqual(Int128(0x1.0p116.nextDown).high, 0x000f_ffff_ffff_ffff)
    XCTAssertEqual(Int128(0x1.0p116.nextDown).low, 0x8000_0000_0000_0000)
    XCTAssertEqual(Int128(0x1.0p116).high, 0x0010_0000_0000_0000)
    XCTAssertEqual(Int128(0x1.0p116).low, 0)
    XCTAssertEqual(Int128(0x1.0p116.nextUp).high, 0x0010_0000_0000_0001)
    XCTAssertEqual(Int128(0x1.0p116.nextUp).low, 0)
    XCTAssertEqual(Int128(0x1.0p127.nextDown).high, 0x7fff_ffff_ffff_fc00)
    XCTAssertEqual(Int128(0x1.0p127.nextDown).low, 0)
    
    XCTAssertEqual(Int128(Float.greatestFiniteMagnitude/2).high, 0x7fff_ff80_0000_0000)
    XCTAssertEqual(Int128(Float.greatestFiniteMagnitude/2).low, 0)
    
#if swift(>=5.4) && !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
    XCTAssertEqual(Int128(Float16.greatestFiniteMagnitude).high, 0)
    XCTAssertEqual(Int128(Float16.greatestFiniteMagnitude).low, 65504)
#endif
  }
  
  func testConversionsFromIntegers() {
    XCTAssertEqual(Int128(truncatingIfNeeded: -1), -1)
    XCTAssertEqual(Int128(truncatingIfNeeded: Int8.min), -0x80)
    XCTAssertEqual(Int128(truncatingIfNeeded: Int8.max), 0x0000000000000000_000000000000007f)
    XCTAssertEqual(Int128(truncatingIfNeeded: UInt8.max), 0x0000000000000000_00000000000000ff)
    XCTAssertEqual(Int128(truncatingIfNeeded: Int.min), -0x8000000000000000)
    XCTAssertEqual(Int128(truncatingIfNeeded: Int.max), 0x0000000000000000_7fffffffffffffff)
    XCTAssertEqual(Int128(truncatingIfNeeded: UInt.max), 0x0000000000000000_ffffffffffffffff)
    
    XCTAssertEqual(Int128(exactly: -1), -1)
    XCTAssertEqual(Int128(exactly: Int8.min), -0x80)
    XCTAssertEqual(Int128(exactly: Int8.max),  0x7f)
    XCTAssertEqual(Int128(exactly: UInt8.max), 0xff)
    XCTAssertEqual(Int128(exactly: Int.min), -0x8000000000000000)
    XCTAssertEqual(Int128(exactly: Int.max),  0x7fffffffffffffff)
    XCTAssertEqual(Int128(exactly: UInt.max), 0xffffffffffffffff)
    
    XCTAssertEqual(Int128(clamping: -1), -1)
    XCTAssertEqual(Int128(clamping: Int8.min), -0x80)
    XCTAssertEqual(Int128(clamping: Int8.max),  0x7f)
    XCTAssertEqual(Int128(clamping: UInt8.max), 0xff)
    XCTAssertEqual(Int128(clamping: Int.min), -0x8000000000000000)
    XCTAssertEqual(Int128(clamping: Int.max),  0x7fffffffffffffff)
    XCTAssertEqual(Int128(clamping: UInt.max), 0xffffffffffffffff)
  }
  
  func testConversionsToIntegers() {
    let half1 = Int128(0xffffffffffffffff)
    let two64 = Int128(0x1_0000000000000000)
    let bytes = Int128(0x000102030405060708090a0b0c0d0e0f)
    XCTAssertEqual(Int(truncatingIfNeeded: Int128.zero), 0)
    XCTAssertEqual(Int(truncatingIfNeeded: half1), -1)
    XCTAssertEqual(Int(truncatingIfNeeded: two64), 0)
    XCTAssertEqual(Int(truncatingIfNeeded: bytes), 0x08090a0b0c0d0e0f)
    XCTAssertEqual(Int(truncatingIfNeeded: Int128.max), -1)
    XCTAssertEqual(Int8(truncatingIfNeeded: Int128.zero), 0)
    XCTAssertEqual(Int8(truncatingIfNeeded: half1), -1)
    XCTAssertEqual(Int8(truncatingIfNeeded: two64), 0)
    XCTAssertEqual(Int8(truncatingIfNeeded: bytes), 0x0f)
    XCTAssertEqual(Int8(truncatingIfNeeded: Int128.max), -1)
    XCTAssertEqual(UInt(truncatingIfNeeded: Int128.zero), 0)
    XCTAssertEqual(UInt(truncatingIfNeeded: half1), ~0)
    XCTAssertEqual(UInt(truncatingIfNeeded: two64), 0)
    XCTAssertEqual(UInt(truncatingIfNeeded: bytes), 0x08090a0b0c0d0e0f)
    XCTAssertEqual(UInt(truncatingIfNeeded: Int128.max), ~0)
    XCTAssertEqual(UInt8(truncatingIfNeeded: Int128.zero), 0)
    XCTAssertEqual(UInt8(truncatingIfNeeded: half1), ~0)
    XCTAssertEqual(UInt8(truncatingIfNeeded: two64), 0)
    XCTAssertEqual(UInt8(truncatingIfNeeded: bytes), 0x0f)
    XCTAssertEqual(UInt8(truncatingIfNeeded: Int128.max), ~0)
    
    XCTAssertEqual(Int(exactly: Int128.zero), 0)
    XCTAssertEqual(Int(exactly: half1), .none)
    XCTAssertEqual(Int(exactly: two64), .none)
    XCTAssertEqual(Int(exactly: bytes), .none)
    XCTAssertEqual(Int(exactly: Int128.max), .none)
    XCTAssertEqual(Int8(exactly: Int128.zero), 0)
    XCTAssertEqual(Int8(exactly: half1), .none)
    XCTAssertEqual(Int8(exactly: two64), .none)
    XCTAssertEqual(Int8(exactly: bytes), .none)
    XCTAssertEqual(Int8(exactly: Int128.max), .none)
    XCTAssertEqual(UInt(exactly: Int128.zero), 0)
    XCTAssertEqual(UInt(exactly: half1), ~0)
    XCTAssertEqual(UInt(exactly: two64), .none)
    XCTAssertEqual(UInt(exactly: bytes), .none)
    XCTAssertEqual(UInt(exactly: Int128.max), .none)
    XCTAssertEqual(UInt8(exactly: Int128.zero), 0)
    XCTAssertEqual(UInt8(exactly: half1), .none)
    XCTAssertEqual(UInt8(exactly: two64), .none)
    XCTAssertEqual(UInt8(exactly: bytes), .none)
    XCTAssertEqual(UInt8(exactly: Int128.max), .none)
    
    XCTAssertEqual(Int(clamping: Int128.zero), 0)
    XCTAssertEqual(Int(clamping: half1), .max)
    XCTAssertEqual(Int(clamping: two64), .max)
    XCTAssertEqual(Int(clamping: bytes), .max)
    XCTAssertEqual(Int(clamping: Int128.max), .max)
    XCTAssertEqual(Int8(clamping: Int128.zero), 0)
    XCTAssertEqual(Int8(clamping: half1), .max)
    XCTAssertEqual(Int8(clamping: two64), .max)
    XCTAssertEqual(Int8(clamping: bytes), .max)
    XCTAssertEqual(Int8(clamping: Int128.max), .max)
    XCTAssertEqual(UInt(clamping: Int128.zero), 0)
    XCTAssertEqual(UInt(clamping: half1), .max)
    XCTAssertEqual(UInt(clamping: two64), .max)
    XCTAssertEqual(UInt(clamping: bytes), .max)
    XCTAssertEqual(UInt(clamping: Int128.max), .max)
    XCTAssertEqual(UInt8(clamping: Int128.zero), 0)
    XCTAssertEqual(UInt8(clamping: half1), .max)
    XCTAssertEqual(UInt8(clamping: two64), .max)
    XCTAssertEqual(UInt8(clamping: bytes), .max)
    XCTAssertEqual(UInt8(clamping: Int128.max), .max)
  }
  
  func testConversionsFromWideIntegers() {
    XCTAssertEqual(Int128(truncatingIfNeeded: -1 as S128), -1)
    XCTAssertEqual(Int128(truncatingIfNeeded: S128.min), .min)
    XCTAssertEqual(Int128(truncatingIfNeeded: S128.max), .max)
    XCTAssertEqual(Int128(truncatingIfNeeded: U128.max),   -1)
    XCTAssertEqual(Int128(truncatingIfNeeded: S256.min),    0)
    XCTAssertEqual(Int128(truncatingIfNeeded: S256.max),   -1)
    XCTAssertEqual(Int128(truncatingIfNeeded: U256.max),   -1)
    
    XCTAssertEqual(Int128(exactly: -1 as S128), -1)
    XCTAssertEqual(Int128(exactly: S128.min), .min)
    XCTAssertEqual(Int128(exactly: S128.max), .max)
    XCTAssertEqual(Int128(exactly: U128.max), .none)
    XCTAssertEqual(Int128(exactly: S256.min), .none)
    XCTAssertEqual(Int128(exactly: S256.max), .none)
    XCTAssertEqual(Int128(exactly: U256.max), .none)
    
    XCTAssertEqual(Int128(clamping: -1 as S128), -1)
    XCTAssertEqual(Int128(clamping: S128.min), .min)
    XCTAssertEqual(Int128(clamping: S128.max), .max)
    XCTAssertEqual(Int128(clamping: U128.max), .max)
    XCTAssertEqual(Int128(clamping: S256.min), .min)
    XCTAssertEqual(Int128(clamping: S256.max), .max)
    XCTAssertEqual(Int128(clamping: U256.max), .max)
    
    let a = S256((high: S128((high: 0x0001020304050607,
                              low:  0x08090a0b0c0d0e0f)),
                  low:  U128((high: 0x1011121314151617,
                              low:  0x18191a1b1c1d1e1f))))
    let b = -a
    let c = U256(truncatingIfNeeded: a)
    XCTAssertEqual(Int128(truncatingIfNeeded: a),  0x101112131415161718191a1b1c1d1e1f)
    XCTAssertEqual(Int128(truncatingIfNeeded: b), ~0x101112131415161718191a1b1c1d1e1f + 1)
    XCTAssertEqual(Int128(truncatingIfNeeded: c),  0x101112131415161718191a1b1c1d1e1f)
    XCTAssertEqual(Int128(exactly: a), .none)
    XCTAssertEqual(Int128(exactly: b), .none)
    XCTAssertEqual(Int128(exactly: c), .none)
    XCTAssertEqual(Int128(clamping: a), .max)
    XCTAssertEqual(Int128(clamping: b), .min)
    XCTAssertEqual(Int128(clamping: c), .max)
  }
  
  let values: [Int128] = [
    0xc1e957535e430264_0000000000000000 as UInt128,
    0xc1e957535e430264_2f71ea0f2c5d8988,
    0xc1e957535e430264_c1e957535e430264,
    0xc1e957535e430264_ffffffffffffffff,
    0xffffffffffffffff_0000000000000000,
    0xffffffffffffffff_2f71ea0f2c5d8988,
    0xffffffffffffffff_c1e957535e430264,
    0xffffffffffffffff_ffffffffffffffff,
    0x0000000000000000_0000000000000000,
    0x0000000000000000_2f71ea0f2c5d8988,
    0x0000000000000000_c1e957535e430264,
    0x0000000000000000_ffffffffffffffff,
    0x0000000000000001_0000000000000000,
    0x0000000000000001_2f71ea0f2c5d8988,
    0x0000000000000001_c1e957535e430264,
    0x0000000000000001_ffffffffffffffff,
    0x2f71ea0f2c5d8988_0000000000000000,
    0x2f71ea0f2c5d8988_2f71ea0f2c5d8988,
    0x2f71ea0f2c5d8988_c1e957535e430264,
    0x2f71ea0f2c5d8988_ffffffffffffffff,
  ].map { Int128(truncatingIfNeeded: $0) }
  
  func testComparisons() {
    for i in 0 ..< values.count {
      let a = values[i]
      for b in values[0..<i] {
        XCTAssertLessThan(b, a)
      }
      XCTAssertEqual(a, a)
    }
  }
  
  func testBitwiseOperations() {
    let a = Int128(truncatingIfNeeded: 0xffff0000ffff0000_ffff0000ffff0000 as UInt128)
    let b = Int128(truncatingIfNeeded: 0xf0e0d0c0b0a09080_7060504030201000 as UInt128)
    XCTAssertEqual(~a   , 0x0000ffff0000ffff_0000ffff0000ffff)
    XCTAssertEqual(~b   , 0x0f1f2f3f4f5f6f7f_8f9fafbfcfdfefff)
    XCTAssertEqual(a & b, Int128(truncatingIfNeeded: 0xf0e00000b0a00000_7060000030200000 as UInt128))
    XCTAssertEqual(a | b, Int128(truncatingIfNeeded: 0xffffd0c0ffff9080_ffff5040ffff1000 as UInt128))
    XCTAssertEqual(a ^ b, Int128(truncatingIfNeeded: 0x0f1fd0c04f5f9080_8f9f5040cfdf1000 as UInt128))
    
    XCTAssertEqual(a &>>   0, a)
    XCTAssertEqual(a &>>   1, Int128(truncatingIfNeeded: 0xffff80007fff8000_7fff80007fff8000 as UInt128))
    XCTAssertEqual(a &>>   2, Int128(truncatingIfNeeded: 0xffffc0003fffc000_3fffc0003fffc000 as UInt128))
    XCTAssertEqual(a &>>   3, Int128(truncatingIfNeeded: 0xffffe0001fffe000_1fffe0001fffe000 as UInt128))
    XCTAssertEqual(a &>>   4, Int128(truncatingIfNeeded: 0xfffff0000ffff000_0ffff0000ffff000 as UInt128))
    XCTAssertEqual(b &>>   8, Int128(truncatingIfNeeded: 0xfff0e0d0c0b0a090_8070605040302010 as UInt128))
    XCTAssertEqual(b &>>  16, Int128(truncatingIfNeeded: 0xfffff0e0d0c0b0a0_9080706050403020 as UInt128))
    XCTAssertEqual(b &>>  32, Int128(truncatingIfNeeded: 0xfffffffff0e0d0c0_b0a0908070605040 as UInt128))
    XCTAssertEqual(b &>>  64, Int128(truncatingIfNeeded: 0xffffffffffffffff_f0e0d0c0b0a09080 as UInt128))
    XCTAssertEqual(b &>> 127, Int128(truncatingIfNeeded: 0xffffffffffffffff_ffffffffffffffff as UInt128))
    XCTAssertEqual(b &>> 128, b)
    XCTAssertEqual(b  >> 128, -1)
    
    XCTAssertEqual(a &<< 0, a)
    XCTAssertEqual(a &<< 1, Int128(truncatingIfNeeded: 0xfffe0001fffe0001_fffe0001fffe0000 as UInt128))
    XCTAssertEqual(a &<< 2, Int128(truncatingIfNeeded: 0xfffc0003fffc0003_fffc0003fffc0000 as UInt128))
    XCTAssertEqual(a &<< 3, Int128(truncatingIfNeeded: 0xfff80007fff80007_fff80007fff80000 as UInt128))
    XCTAssertEqual(a &<< 4, Int128(truncatingIfNeeded: 0xfff0000ffff0000f_fff0000ffff00000 as UInt128))
    XCTAssertEqual(b &<<   8, Int128(truncatingIfNeeded: 0xe0d0c0b0a0908070_6050403020100000 as UInt128))
    XCTAssertEqual(b &<<  16, Int128(truncatingIfNeeded: 0xd0c0b0a090807060_5040302010000000 as UInt128))
    XCTAssertEqual(b &<<  32, Int128(truncatingIfNeeded: 0xb0a0908070605040_3020100000000000 as UInt128))
    XCTAssertEqual(b &<<  64, Int128(truncatingIfNeeded: 0x7060504030201000_0000000000000000 as UInt128))
    XCTAssertEqual(b &<< 127, Int128(truncatingIfNeeded: 0x0000000000000000_0000000000000000 as UInt128))
    XCTAssertEqual(b &<< 128, b)
    XCTAssertEqual(b  << 128, 0)
    
    XCTAssertEqual(a.nonzeroBitCount, 64)
    XCTAssertEqual(b.nonzeroBitCount, 32)
    for i in 0 ..< 128 {
      XCTAssertEqual((a &>> i).leadingZeroBitCount, 0)
      XCTAssertEqual((~a &<< i).trailingZeroBitCount, i)
    }
    XCTAssertEqual(a.byteSwapped, ~a)
    XCTAssertEqual(b.byteSwapped, 0x0010203040506070_8090a0b0c0d0e0f0)
  }
  
  func testArithmetic() {
    let a: Int128  = 0x0000ffff0000ffff_0000ffff0000ffff
    let b: Int128  = 0x00f0e0d0c0b0a090_8070605040302010
    let c: Int128  = 0x0000000000000000_ffffffffffffffff
    XCTAssertEqual(a + b, 0x00f1e0cfc0b1a08f_8071604f4031200f)
    XCTAssertEqual(b + c, 0x00f0e0d0c0b0a091_807060504030200f)
    XCTAssertEqual(b - a, 0x00efe0d1c0afa091_806f6051402f2011)
    XCTAssertEqual(b - c, 0x00f0e0d0c0b0a08f_8070605040302011)
  }
  
  func testWrapping() {
    let a: Int128  = .min + 0x0000ffff0000ffff_0000ffff0000ffff
    let b: Int128  = .min + 0x00f0e0d0c0b0a090_8070605040302010
    let c: Int128  = .min + 0x0000000000000000_ffffffffffffffff
    XCTAssertEqual(a &+ b, 0x00f1e0cfc0b1a08f_8071604f4031200f)
    XCTAssertEqual(b &+ c, 0x00f0e0d0c0b0a091_807060504030200f)
    XCTAssertEqual(b &- a, 0x00efe0d1c0afa091_806f6051402f2011)
    XCTAssertEqual(b &- c, 0x00f0e0d0c0b0a08f_8070605040302011)
    XCTAssertEqual(a &* b, Int128(truncatingIfNeeded: 0xff7f7f8f9f9f9faf_bfbfbfcfdfdfdff0 as UInt128))
  }
  
  func testString() {
    for a in [
      Int128.min + 0x0000ffff0000ffff_0000ffff0000ffff,
      Int128.min + 0x00f0e0d0c0b0a090_8070605040302010,
      0x0000000000000000_ffffffffffffffff,
      -1
    ] {
      XCTAssertEqual(a, Int128(String(a)))
    }
  }
}
