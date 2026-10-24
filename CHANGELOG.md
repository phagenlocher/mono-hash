# Revision history for mono-hash

## Unreleased

* Export `HasFNVBasis` so the supported FNV result widths appear in the
  generated documentation.
* Expose the element byte-encoding class as
  `Data.Hash.Mono.Internal.ByteFoldable.ByteFoldable` (method `foldlBytes`),
  so containers of user-defined element types can be hashed. The class was
  previously unreachable as `ToWords` in the hidden internal module.

## 0.1.0.0 -- 2026-10-21

* First release.
* FNV-1 and FNV-1a for monomorphic containers.
* SipHash-c-d and HalfSipHash-c-d for monomorphic containers.
* SipHash and HalfSipHash use the reference implementation's output-word
  byte order; keys are the reference key bytes in big-endian order.
