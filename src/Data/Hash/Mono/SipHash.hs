{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Hash.Mono.SipHash
  ( SipHashData,
    SipHashResult,
    sipHash,
  )
where

import Data.Bits (Bits (rotateL, xor))
import Data.Foldable qualified as Foldable
import Data.Hash.Mono.Internal
  ( ToWords (..),
    mkWord16,
    mkWord32,
    mkWord64,
    word64ToLE,
  )
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.Sequence qualified as Seq
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64, Word8)
import Numeric (showHex)

class SipHashResult a where
  initialize :: SipData -> SipData
  finalization :: Word -> SipData -> a

instance SipHashResult Word64 where
  initialize :: SipData -> SipData
  initialize = id

  finalization :: Word -> SipData -> Word64
  finalization d (SipData !v0 !v1 !v2 !v3) =
    let v2' = v2 `xor` 0xff
        (SipData !v0' !v1' !v2'' !v3') = sipRoundN d (SipData v0 v1 v2' v3)
     in word64ToLE $ v0' `xor` v1' `xor` v2'' `xor` v3'

instance SipHashResult Word128 where
  initialize :: SipData -> SipData
  initialize (SipData !v0 !v1 !v2 !v3) = SipData v0 (v1 `xor` 0xee) v2 v3

  finalization :: Word -> SipData -> Word128
  finalization d (SipData !v0 !v1 !v2 !v3) =
    let v2' = v2 `xor` 0xee
        (SipData !v0' !v1' !v2'' !v3') = sipRoundN d (SipData v0 v1 v2' v3)
        hi = word64ToLE $ v0' `xor` v1' `xor` v2'' `xor` v3'
        v1'' = v1' `xor` 0xdd
        (SipData !v0'' !v1''' !v2''' !v3'') = sipRoundN d (SipData v0' v1'' v2'' v3')
        lo = word64ToLE $ v0'' `xor` v1''' `xor` v2''' `xor` v3''
     in Word128 hi lo

type SipHashData a = (MonoFoldable a, ToWords (Element a))

data SipData = SipData
  { v0 :: {-# UNPACK #-} !Word64,
    v1 :: {-# UNPACK #-} !Word64,
    v2 :: {-# UNPACK #-} !Word64,
    v3 :: {-# UNPACK #-} !Word64
  }

instance Show SipData where
  show (SipData !v0 !v1 !v2 !v3) =
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

sipHash :: forall result mono. (SipHashData mono, SipHashResult result) => Word -> Word -> Word128 -> mono -> result
sipHash c d key = finalization d . Foldable.foldl' compression (initialize @result initData) . chunkSipHashData
  where
    compression :: SipData -> Word64 -> SipData
    compression (SipData !v0 !v1 !v2 !v3) m =
      let v3' = v3 `xor` m
          (SipData !v0' !v1' !v2' !v3'') = sipRoundN c (SipData v0 v1 v2 v3')
          v0'' = v0' `xor` m
       in SipData v0'' v1' v2' v3''

    initData :: SipData
    initData =
      SipData
        { v0 = k0 `xor` 0x736f6d6570736575,
          v1 = k1 `xor` 0x646f72616e646f6d,
          v2 = k0 `xor` 0x6c7967656e657261,
          v3 = k1 `xor` 0x7465646279746573
        }
      where
        k0 :: Word64
        k0 = word64ToLE $ word128Hi64 key

        k1 :: Word64
        k1 = word64ToLE $ word128Lo64 key

sipRound :: SipData -> SipData
sipRound (SipData !v0 !v1 !v2 !v3) =
  let v0' = v0 + v1
      v1' = v1 `rotateL` 13
      v1'' = v1' `xor` v0'
      v0'' = v0' `rotateL` 32
      v2' = v2 + v3
      v3' = v3 `rotateL` 16
      v3'' = v3' `xor` v2'
      v0''' = v0'' + v3''
      v3''' = v3'' `rotateL` 21
      v3'''' = v3''' `xor` v0'''
      v2'' = v2' + v1''
      v1''' = v1'' `rotateL` 17
      v1'''' = v1''' `xor` v2''
      v2''' = v2'' `rotateL` 32
   in SipData v0''' v1'''' v2''' v3''''

sipRoundN :: Word -> SipData -> SipData
sipRoundN n !x
  | n <= 0 = x
  | otherwise = sipRoundN (n - 1) $! sipRound x

chunkSipHashData :: forall a. (SipHashData a) => a -> [Word64]
chunkSipHashData = chunkWords . collectWords
  where
    collectWords :: a -> (Int, Seq.Seq Word8)
    collectWords = ofoldl' f (0, Seq.empty)
      where
        f :: (ToWords c) => (Int, Seq.Seq Word8) -> c -> (Int, Seq.Seq Word8)
        f (!len, !word8s) c =
          let !word8s' = Seq.fromList $ toWords c
           in (len + Seq.length word8s', (Seq.><) word8s word8s')

    chunkWords :: (Int, Seq.Seq Word8) -> [Word64]
    chunkWords (len, word8s) = go $ Foldable.toList word8s
      where
        lenWord :: Word8
        lenWord = fromIntegral $ len `mod` 256

        go :: [Word8] -> [Word64]
        go (c0 : c1 : c2 : c3 : c4 : c5 : c6 : c7 : xs) =
          mkWord c7 c6 c5 c4 c3 c2 c1 c0 : go xs
        go [c0, c1, c2, c3, c4, c5, c6] = [mkWord lenWord c6 c5 c4 c3 c2 c1 c0]
        go [c0, c1, c2, c3, c4, c5] = [mkWord lenWord 0 c5 c4 c3 c2 c1 c0]
        go [c0, c1, c2, c3, c4] = [mkWord lenWord 0 0 c4 c3 c2 c1 c0]
        go [c0, c1, c2, c3] = [mkWord lenWord 0 0 0 c3 c2 c1 c0]
        go [c0, c1, c2] = [mkWord lenWord 0 0 0 0 c2 c1 c0]
        go [c0, c1] = [mkWord lenWord 0 0 0 0 0 c1 c0]
        go [c0] = [mkWord lenWord 0 0 0 0 0 0 c0]
        go [] = [mkWord lenWord 0 0 0 0 0 0 0]

    mkWord :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
    mkWord !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 =
      mkWord64 (mkWord32 (mkWord16 c0 c1) (mkWord16 c2 c3)) (mkWord32 (mkWord16 c4 c5) (mkWord16 c6 c7))
    {-# INLINE mkWord #-}