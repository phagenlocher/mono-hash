{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module Data.Hash.Mono.SipHash
  ( SipHashData,
    SipHashResult,
    sipHash,
  )
where

import Data.Bits (Bits (rotateL, shiftL, xor, (.|.)))
import Data.Hash.Mono.Internal
  ( ToWords (..),
    word64ToLE,
  )
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
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
  { sv0 :: {-# UNPACK #-} !Word64,
    sv1 :: {-# UNPACK #-} !Word64,
    sv2 :: {-# UNPACK #-} !Word64,
    sv3 :: {-# UNPACK #-} !Word64
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

data SipStream = SipStream
  { ssData :: {-# UNPACK #-} !SipData,
    ssWord :: {-# UNPACK #-} !Word64,
    ssCount :: {-# UNPACK #-} !Int,
    ssLen :: {-# UNPACK #-} !Word8
  }

sipHash :: forall result mono. (SipHashData mono, SipHashResult result) => Word -> Word -> Word128 -> mono -> result
sipHash c d key mono =
  case ofoldl' (foldlWords (step c)) initStream mono of
    SipStream {ssData = state, ssWord = word, ssLen = len} ->
      finalization d (compress c state (word .|. (fromIntegral len `shiftL` 56)))
  where
    initData :: SipData
    initData =
      SipData
        { sv0 = k0 `xor` 0x736f6d6570736575,
          sv1 = k1 `xor` 0x646f72616e646f6d,
          sv2 = k0 `xor` 0x6c7967656e657261,
          sv3 = k1 `xor` 0x7465646279746573
        }
      where
        k0 :: Word64
        k0 = word64ToLE $ word128Hi64 key

        k1 :: Word64
        k1 = word64ToLE $ word128Lo64 key

    initStream :: SipStream
    initStream =
      SipStream
        { ssData = initialize @result initData,
          ssWord = 0,
          ssCount = 0,
          ssLen = 0
        }
{-# INLINABLE sipHash #-}

step :: Word -> SipStream -> Word8 -> SipStream
step c SipStream {ssData = d, ssWord = word, ssCount = n, ssLen = len} !b =
  let word' = word .|. (fromIntegral b `shiftL` (8 * n))
      len' = len + 1
   in if n == 7
        then SipStream {ssData = compress c d word', ssWord = 0, ssCount = 0, ssLen = len'}
        else SipStream {ssData = d, ssWord = word', ssCount = n + 1, ssLen = len'}
{-# INLINE step #-}

compress :: Word -> SipData -> Word64 -> SipData
compress c (SipData !v0 !v1 !v2 !v3) m =
  let v3' = v3 `xor` m
      (SipData !v0' !v1' !v2' !v3'') = sipRoundN c (SipData v0 v1 v2 v3')
      v0'' = v0' `xor` m
   in SipData v0'' v1' v2' v3''
{-# INLINE compress #-}

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
{-# INLINE sipRound #-}

sipRoundN :: Word -> SipData -> SipData
sipRoundN n !x
  | n <= 0 = x
  | otherwise = sipRoundN (n - 1) $! sipRound x
{-# INLINE sipRoundN #-}
