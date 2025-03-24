{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Hash.Mono.HalfSipHash
  ( HalfSipHashData,
    HalfSipHashResult,
    halfSipHash,
  )
where

import Data.Bits (Bits (rotateL, shiftR, xor, (.&.)))
import Data.Foldable qualified as Foldable
import Data.Hash.Mono.Internal
  ( ToWords (..),
    mkWord16,
    mkWord32,
    mkWord64,
    word32ToLE,
  )
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.Sequence qualified as Seq
import Data.Word (Word32, Word64, Word8)
import Numeric (showHex)

class HalfSipHashResult a where
  initialize :: HalfSipData -> HalfSipData
  finalization :: Word -> HalfSipData -> a

instance HalfSipHashResult Word32 where
  initialize :: HalfSipData -> HalfSipData
  initialize = id

  finalization :: Word -> HalfSipData -> Word32
  finalization d (HalfSipData !v0 !v1 !v2 !v3) =
    let v2' = v2 `xor` 0xff
        (HalfSipData !_ v1' _ !v3') = halfSipRoundN d (HalfSipData v0 v1 v2' v3)
     in word32ToLE $ v1' `xor` v3'

instance HalfSipHashResult Word64 where
  initialize :: HalfSipData -> HalfSipData
  initialize (HalfSipData !v0 !v1 !v2 !v3) = HalfSipData v0 (v1 `xor` 0xee) v2 v3

  finalization :: Word -> HalfSipData -> Word64
  finalization d (HalfSipData !v0 !v1 !v2 !v3) =
    let v2' = v2 `xor` 0xee
        (HalfSipData !v0' !v1' !v2'' !v3') = halfSipRoundN d (HalfSipData v0 v1 v2' v3)
        hi = word32ToLE $ v1' `xor` v3'
        v1'' = v1' `xor` 0xdd
        (HalfSipData _ !v1''' _ !v3'') = halfSipRoundN d (HalfSipData v0' v1'' v2'' v3')
        lo = word32ToLE $ v1''' `xor` v3''
     in mkWord64 hi lo

type HalfSipHashData a = (MonoFoldable a, ToWords (Element a))

data HalfSipData = HalfSipData
  { v0 :: {-# UNPACK #-} !Word32,
    v1 :: {-# UNPACK #-} !Word32,
    v2 :: {-# UNPACK #-} !Word32,
    v3 :: {-# UNPACK #-} !Word32
  }

instance Show HalfSipData where
  show :: HalfSipData -> String
  show (HalfSipData !v0 !v1 !v2 !v3) =
    unwords
      [ "v0",
        showHex v0 "",
        "v1",
        showHex v1 "",
        "v2",
        showHex v2 "",
        "v3",
        showHex v3 ""
      ]

halfSipHash :: forall result mono. (HalfSipHashData mono, HalfSipHashResult result) => Word -> Word -> Word64 -> mono -> result
halfSipHash c d key = finalization d . Foldable.foldl' compression (initialize @result initData) . chunkHalfSipHashData
  where
    compression :: HalfSipData -> Word32 -> HalfSipData
    compression (HalfSipData !v0 !v1 !v2 !v3) m =
      let v3' = v3 `xor` m
          (HalfSipData !v0' !v1' !v2' !v3'') = halfSipRoundN c (HalfSipData v0 v1 v2 v3')
          v0'' = v0' `xor` m
       in HalfSipData v0'' v1' v2' v3''

    initData :: HalfSipData
    initData =
      HalfSipData
        { v0 = k0,
          v1 = k1,
          v2 = k0 `xor` 0x6c796765,
          v3 = k1 `xor` 0x74656462
        }
      where
        k0 :: Word32
        k0 = word32ToLE . fromIntegral $ key `shiftR` 32

        k1 :: Word32
        k1 = word32ToLE . fromIntegral $ key .&. 0xffffffff

halfSipRound :: HalfSipData -> HalfSipData
halfSipRound (HalfSipData !v0 !v1 !v2 !v3) =
  let v0' = v0 + v1
      v1' = v1 `rotateL` 5
      v1'' = v1' `xor` v0'
      v0'' = v0' `rotateL` 16
      v2' = v2 + v3
      v3' = v3 `rotateL` 8
      v3'' = v3' `xor` v2'
      v0''' = v0'' + v3''
      v3''' = v3'' `rotateL` 7
      v3'''' = v3''' `xor` v0'''
      v2'' = v2' + v1''
      v1''' = v1'' `rotateL` 13
      v1'''' = v1''' `xor` v2''
      v2''' = v2'' `rotateL` 16
   in HalfSipData v0''' v1'''' v2''' v3''''

halfSipRoundN :: Word -> HalfSipData -> HalfSipData
halfSipRoundN n !x
  | n <= 0 = x
  | otherwise = halfSipRoundN (n - 1) $! halfSipRound x

chunkHalfSipHashData :: forall a. (HalfSipHashData a) => a -> [Word32]
chunkHalfSipHashData = chunkWords . collectWords
  where
    collectWords :: a -> (Int, Seq.Seq Word8)
    collectWords = ofoldl' f (0, Seq.empty)
      where
        f :: (ToWords c) => (Int, Seq.Seq Word8) -> c -> (Int, Seq.Seq Word8)
        f (!len, !word8s) c =
          let !word8s' = Seq.fromList $ toWords c
           in (len + Seq.length word8s', (Seq.><) word8s word8s')

    chunkWords :: (Int, Seq.Seq Word8) -> [Word32]
    chunkWords (len, word8s) = go $ Foldable.toList word8s
      where
        lenWord :: Word8
        lenWord = fromIntegral $ len `mod` 256

        go :: [Word8] -> [Word32]
        go (c0 : c1 : c2 : c3 : xs) =
          mkWord c3 c2 c1 c0 : go xs
        go [c0, c1, c2] = [mkWord lenWord c2 c1 c0]
        go [c0, c1] = [mkWord lenWord 0 c1 c0]
        go [c0] = [mkWord lenWord 0 0 c0]
        go [] = [mkWord lenWord 0 0 0]

    mkWord :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
    mkWord !c0 !c1 !c2 !c3 =
      mkWord32 (mkWord16 c0 c1) (mkWord16 c2 c3)
    {-# INLINE mkWord #-}