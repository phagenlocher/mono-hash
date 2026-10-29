# Revision history for mono-hash

## 0.1.1.0 -- 2026-10-29

* Add anti-instances for `ByteFoldable` w.r.t. `Int`, `Word`, `Integer`, and `Natural`.
* Add `ByteFoldable` instances for `Int128` and `Int256`.

## 0.1.0.2 -- 2026-10-27

* Internal: replace the hand-rolled `byteSwap32`/`byteSwap64` from `Data.Hash.Mono.Internal` with the equivalents already provided by base in `Data.Word`.
* Performance: use unchecked shifts (`unsafeShiftL`/`unsafeShiftR`) instead of the bounds-checked `shiftL`/`shiftR` in the byte encoders and in the SipHash/HalfSipHash inner loop.

## 0.1.0.1 -- 2026-10-25

* Documentation: credit the authors of FNV and SipHash/HalfSipHash.
* Add the "Hash" category to the package's cabal file

## 0.1.0.0 -- 2026-10-25

* First release.
* FNV-1 and FNV-1a for monomorphic containers.
* SipHash-c-d and HalfSipHash-c-d for monomorphic containers.
