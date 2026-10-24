{-# LANGUAGE BangPatterns #-}

-- | FNV-1 and FNV-1a hashes for any 'MonoFoldable' container.
module Data.Hash.Mono.FNV (
    FNVData,
    FNVResult,
    HasFNVBasis,
    fnv1,
    fnv1a,
)
where

import Data.Bits (Bits (xor))
import Data.Hash.Mono.Internal.ByteFoldable (ByteFoldable (..))
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word32, Word64, Word8)

-- | Offset basis and prime for an FNV result width.
class HasFNVBasis a where
    -- | Offset basis used to seed the hash.
    fnvOffsetBasis :: a

    -- | Prime applied once per input byte.
    fnvPrime :: a

instance HasFNVBasis Word32 where
    fnvOffsetBasis :: Word32
    fnvOffsetBasis = 2166136261

    fnvPrime :: Word32
    fnvPrime = 16777619

instance HasFNVBasis Word64 where
    fnvOffsetBasis :: Word64
    fnvOffsetBasis = 14695981039346656037

    fnvPrime :: Word64
    fnvPrime = 1099511628211

instance HasFNVBasis Word128 where
    fnvOffsetBasis :: Word128
    fnvOffsetBasis = 144066263297769815596495629667062367629

    fnvPrime :: Word128
    fnvPrime = 309485009821345068724781371

instance HasFNVBasis Word256 where
    fnvOffsetBasis :: Word256
    fnvOffsetBasis = 100029257958052580907070968620625704837092796014241193945225284501741471925557

    fnvPrime :: Word256
    fnvPrime = 374144419156711147060143317175368453031918731002211

-- | Constraint for types that can be used as an FNV result.
type FNVResult a = (HasFNVBasis a, Bits a, Num a)

-- | Constraint for containers hashed with FNV.
type FNVData a = (MonoFoldable a, ByteFoldable (Element a))

fnvGeneric :: forall result mono. (FNVData mono, FNVResult result) => (result -> Word8 -> result) -> mono -> result
fnvGeneric fnvRound = ofoldl' (foldlBytes fnvRound) fnvOffsetBasis
{-# INLINE fnvGeneric #-}

{- | FNV-1 hash of a monomorphic container.

Example:
@
fnv1 \@'Word64' \"hello\"
@
-}
fnv1 :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1 = fnvGeneric fnvRound
  where
    fnvRound :: result -> Word8 -> result
    fnvRound !acc !octet = (acc * fnvPrime) `xor` fromIntegral octet
{-# INLINEABLE fnv1 #-}

{- | FNV-1a hash of a monomorphic container.

Example:
@
fnv1a \@'Word64' \"hello\"
@
-}
fnv1a :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1a = fnvGeneric fnvRound
  where
    fnvRound :: result -> Word8 -> result
    fnvRound !acc !octet = (acc `xor` fromIntegral octet) * fnvPrime
{-# INLINEABLE fnv1a #-}
