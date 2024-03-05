#if _hasAtomicBitWidth(_128)
import Synchronization
import Builtin

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UInt128: AtomicRepresentable {
  public typealias AtomicRepresentation = _Atomic128BitStorage
  
  public static func encodeAtomicRepresentation(
    _ value: consuming UInt128
  ) -> _Atomic128BitStorage {
    return _Atomic128BitStorage(value._value)
  }
  
  public static func decodeAtomicRepresentation(
    _ storage: consuming _Atomic128BitStorage
  ) -> UInt128 {
    UInt128(storage._storage)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Int128: AtomicRepresentable {
  public typealias AtomicRepresentation = _Atomic128BitStorage
  
  public static func encodeAtomicRepresentation(
    _ value: consuming Int128
  ) -> _Atomic128BitStorage {
    return _Atomic128BitStorage(value._value)
  }
  
  public static func decodeAtomicRepresentation(
    _ storage: consuming _Atomic128BitStorage
  ) -> Int128 {
    Int128(storage._storage)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Atomic where Value == UInt128 {
  @_transparent @usableFromInline
  var rawAddress: Builtin.RawPointer {
    Builtin.unprotectedAddressOfBorrow(self)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func wrappingAdd(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_add_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_add_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_add_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_add_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_add_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: UInt128(original) &- operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func wrappingSubtract(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_sub_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_sub_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_sub_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_sub_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_sub_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: UInt128(original) &- operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseAnd(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_and_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_and_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_and_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_and_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_and_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: UInt128(original) & operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseOr(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_or_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_or_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_or_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_or_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_or_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: UInt128(original) | operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseXor(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_xor_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_xor_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_xor_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_xor_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_xor_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: UInt128(original) ^ operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseMin(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_umin_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_umin_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_umin_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_umin_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_umin_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: Swift.min(UInt128(original), operand))
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseMax(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_umax_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_umax_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_umax_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_umax_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_umax_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: UInt128(original), newValue: Swift.max(UInt128(original), operand))
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func add(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    var result = (
      exchanged: false,
      original: load(ordering: .relaxed)
    )
    var new: UInt128
    
    repeat {
      new = result.original + operand
      
      result = weakCompareExchange(
        expected: result.original,
        desired: new,
        ordering: ordering
      )
    } while !result.exchanged
    
    return (oldValue: result.original, newValue: new)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func subtract(
    _ operand: UInt128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt128, newValue: UInt128) {
    var result = (
      exchanged: false,
      original: load(ordering: .relaxed)
    )
    var new: UInt128
    
    repeat {
      new = result.original - operand
      
      result = weakCompareExchange(
        expected: result.original,
        desired: new,
        ordering: ordering
      )
    } while !result.exchanged
    
    return (oldValue: result.original, newValue: new)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Atomic where Value == Int128 {
  @_transparent @usableFromInline
  var rawAddress: Builtin.RawPointer {
    Builtin.unprotectedAddressOfBorrow(self)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func wrappingAdd(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_add_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_add_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_add_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_add_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_add_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Int128(original) &- operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func wrappingSubtract(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_sub_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_sub_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_sub_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_sub_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_sub_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Int128(original) &- operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseAnd(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_and_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_and_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_and_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_and_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_and_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Int128(original) & operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseOr(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_or_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_or_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_or_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_or_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_or_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Int128(original) | operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func bitwiseXor(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_xor_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_xor_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_xor_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_xor_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_xor_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Int128(original) ^ operand)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func min(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_min_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_min_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_min_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_min_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_min_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Swift.min(Int128(original), operand))
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func max(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    let original = switch ordering {
    case .relaxed: Builtin.atomicrmw_max_monotonic_Int128(rawAddress, operand._value)
    case .acquiring: Builtin.atomicrmw_max_acquire_Int128(rawAddress, operand._value)
    case .releasing: Builtin.atomicrmw_max_release_Int128(rawAddress, operand._value)
    case .acquiringAndReleasing: Builtin.atomicrmw_max_acqrel_Int128(rawAddress, operand._value)
    case .sequentiallyConsistent: Builtin.atomicrmw_max_seqcst_Int128(rawAddress, operand._value)
    default: Builtin.unreachable()
    }
    return (oldValue: Int128(original), newValue: Swift.max(Int128(original), operand))
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func add(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    var result = (
      exchanged: false,
      original: load(ordering: .relaxed)
    )
    var new: Int128
    
    repeat {
      new = result.original + operand
      
      result = weakCompareExchange(
        expected: result.original,
        desired: new,
        ordering: ordering
      )
    } while !result.exchanged
    
    return (oldValue: result.original, newValue: new)
  }
  
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func subtract(
    _ operand: Int128,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Int128, newValue: Int128) {
    var result = (
      exchanged: false,
      original: load(ordering: .relaxed)
    )
    var new: Int128
    
    repeat {
      new = result.original - operand
      
      result = weakCompareExchange(
        expected: result.original,
        desired: new,
        ordering: ordering
      )
    } while !result.exchanged
    
    return (oldValue: result.original, newValue: new)
  }
}
#endif
