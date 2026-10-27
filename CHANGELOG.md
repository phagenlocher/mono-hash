# Revision history for mono-hash

## 0.1.0.2 -- 2026-10-27

* Internal: replace the hand-rolled `byteSwap32`/`byteSwap66` from `Data.Hash.Mono.Internal` with the equivalents already provided by base in `Data.Word`.

## 0.1.0.1 -- 2026-10-25

* Documentation: credit the authors of FNV and SipHash/HalfSipHash.
* Add the "Hash" category to the package's cabal file

## 0.1.0.0 -- 2026-10-25

* First release.
* FNV-1 and FNV-1a for monomorphic containers.
* SipHash-c-d and HalfSipHash-c-d for monomorphic containers.
