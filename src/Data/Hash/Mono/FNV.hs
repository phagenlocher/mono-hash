{-# LANGUAGE BangPatterns #-}

-- | FNV-1 and FNV-1a hashes for any 'MonoFoldable' container.
--
-- The output width is chosen by the type application; see 'fnv1' and
-- 'fnv1a'.
module Data.Hash.Mono.FNV
  ( FNVData,
    FNVResult,
    fnv1,
    fnv1a,
  )
where

import Data.Bits (Bits (xor))
import Data.Hash.Mono.Internal (ToWords (..))
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word32, Word64, Word8)

-- | Offset basis and prime for a particular FNV output width.
class HasFNVBasis a where
  -- | The FNV offset basis for this output width.
  fnvOffsetBasis :: a

  -- | The FNV prime for this output width.
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
--
-- Instances are provided for 'Word32', 'Word64', 'Word128', and 'Word256'.
type FNVResult a = (HasFNVBasis a, Bits a, Num a)

-- | Constraint for containers that can be hashed with FNV.
--
-- The container's element type must have a ToWords instance.
type FNVData a = (MonoFoldable a, ToWords (Element a))

-- | Shared implementation of 'fnv1' and 'fnv1a'.
fnvGeneric :: forall result mono. (FNVData mono, FNVResult result) => (result -> Word8 -> result) -> mono -> result
fnvGeneric fnvRound = ofoldl' (foldlWords fnvRound) fnvOffsetBasis
{-# INLINE fnvGeneric #-}

-- | FNV-1 hash of a monomorphic container.
--
-- The result width is selected by type application:
--
-- @
-- fnv1 \@'Word64' \"hello\"
-- @
fnv1 :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1 = fnvGeneric fnvRound
  where
    fnvRound :: result -> Word8 -> result
    fnvRound !acc !octet = (acc * fnvPrime) `xor` fromIntegral octet
{-# INLINEABLE fnv1 #-}

-- | FNV-1a hash of a monomorphic container.
--
-- The result width is selected by type application:
--
-- @
-- fnv1a \@'Word64' \"hello\"
-- @
fnv1a :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1a = fnvGeneric fnvRound
  where
    fnvRound :: result -> Word8 -> result
    fnvRound !acc !octet = (acc `xor` fromIntegral octet) * fnvPrime
{-# INLINEABLE fnv1a #-}
