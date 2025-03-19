{-# LANGUAGE BangPatterns #-}

module Data.Hash.Mono.FNV
  ( FNVData,
    FNVResult,
    fnv1,
    fnv1a,
  )
where

import Data.Bits (Bits (xor))
import Data.Hash.Mono.Internal (ToWords (..))
import Data.List qualified as List
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word32, Word64)

class HasFNVBasis a where
  fnvOffsetBasis :: a
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

type FNVResult a = (HasFNVBasis a, Bits a, Num a)

type FNVData a = (MonoFoldable a, ToWords (Element a))

fnvGeneric :: forall result mono. (FNVData mono, FNVResult result) => (result -> result -> result) -> mono -> result
fnvGeneric fnvRound = ofoldl' f fnvOffsetBasis
  where
    f :: result -> Element mono -> result
    f !acc = List.foldl' fnvRound acc . List.map fromIntegral . toWords
{-# INLINE fnvGeneric #-}

fnv1 :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1 = fnvGeneric fnvRound
  where
    fnvRound :: result -> result -> result
    fnvRound !acc !octet = (acc * fnvPrime) `xor` octet
{-# INLINEABLE fnv1 #-}

fnv1a :: forall result mono. (FNVData mono, FNVResult result) => mono -> result
fnv1a = fnvGeneric fnvRound
  where
    fnvRound :: result -> result -> result
    fnvRound !acc !octet = (acc `xor` octet) * fnvPrime
{-# INLINEABLE fnv1a #-}
