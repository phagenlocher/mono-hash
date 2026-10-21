{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | HalfSipHash-@c@-@d@ for any 'MonoFoldable' container.
--
-- Input is consumed one byte at a time in constant space. The result width is
-- chosen by the type application; see 'halfSipHash'.
module Data.Hash.Mono.HalfSipHash
  ( HalfSipHashData,
    HalfSipHashResult,
    halfSipHash,
  )
where

import Data.Bits (Bits (rotateL, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.Hash.Mono.Internal
  ( ToWords (..),
    word32ToLE,
  )
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.Word (Word32, Word64, Word8)
import Numeric (showHex)

-- | Output widths that 'halfSipHash' can produce.
class HalfSipHashResult a where
  -- | Adjust the initial state for the selected output width. The 64-bit
  -- output mixes a domain separator into the second state word; the 32-bit
  -- output does not.
  initialize :: HalfSipData -> HalfSipData

  -- | Run the finalization rounds and return the hash.
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
     in (fromIntegral hi `shiftL` 32) + fromIntegral lo

-- | Constraint for containers that can be hashed with HalfSipHash.
--
-- Requires @ToWords@ for the element type.
type HalfSipHashData a = (MonoFoldable a, ToWords (Element a))

-- | HalfSipHash internal state, consisting of four 32-bit words.
data HalfSipData = HalfSipData
  { hv0 :: {-# UNPACK #-} !Word32,
    hv1 :: {-# UNPACK #-} !Word32,
    hv2 :: {-# UNPACK #-} !Word32,
    hv3 :: {-# UNPACK #-} !Word32
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

-- | Streaming HalfSipHash state.
--
-- The input is consumed one byte at a time, so memory use is constant
-- regardless of input size.
data HalfSipStream = HalfSipStream
  { hsData :: {-# UNPACK #-} !HalfSipData,
    -- ^ Current hash state.
    hsWord :: {-# UNPACK #-} !Word32,
    -- ^ Input bytes buffered so far, in little-endian order.
    hsCount :: {-# UNPACK #-} !Int,
    -- ^ Number of bytes buffered in 'hsWord' (0-3).
    hsLen :: {-# UNPACK #-} !Word8
    -- ^ Total number of input bytes, modulo 256.
  }

-- | Compute HalfSipHash-@c@-@d@ of a monomorphic container.
--
-- @c@ and @d@ are the compression and finalization round counts;
-- HalfSipHash-2-4 is the standard variant. The 64-bit key is split into two
-- 32-bit halves.
--
-- For example:
--
-- @
-- halfSipHash \@'Word32' 2 4 key input
-- @
halfSipHash :: forall result mono. (HalfSipHashData mono, HalfSipHashResult result) => Word -> Word -> Word64 -> mono -> result
halfSipHash c d key mono =
  case ofoldl' (foldlWords (step c)) initStream mono of
    HalfSipStream {hsData = state, hsWord = word, hsLen = len} ->
      finalization d (compress c state (word .|. (fromIntegral len `shiftL` 24)))
  where
    initData :: HalfSipData
    initData =
      HalfSipData
        { hv0 = k0,
          hv1 = k1,
          hv2 = k0 `xor` 0x6c796765,
          hv3 = k1 `xor` 0x74656462
        }
      where
        k0 :: Word32
        k0 = word32ToLE . fromIntegral $ key `shiftR` 32

        k1 :: Word32
        k1 = word32ToLE . fromIntegral $ key .&. 0xffffffff

    initStream :: HalfSipStream
    initStream =
      HalfSipStream
        { hsData = initialize @result initData,
          hsWord = 0,
          hsCount = 0,
          hsLen = 0
        }
{-# INLINABLE halfSipHash #-}

-- | Feed one input byte into the stream, compressing every full 32-bit word.
step :: Word -> HalfSipStream -> Word8 -> HalfSipStream
step c HalfSipStream {hsData = d, hsWord = word, hsCount = n, hsLen = len} !b =
  let word' = word .|. (fromIntegral b `shiftL` (8 * n))
      len' = len + 1
   in if n == 3
        then HalfSipStream {hsData = compress c d word', hsWord = 0, hsCount = 0, hsLen = len'}
        else HalfSipStream {hsData = d, hsWord = word', hsCount = n + 1, hsLen = len'}
{-# INLINE step #-}

-- | Compress one 32-bit input word into the hash state.
compress :: Word -> HalfSipData -> Word32 -> HalfSipData
compress c (HalfSipData !v0 !v1 !v2 !v3) m =
  let v3' = v3 `xor` m
      (HalfSipData !v0' !v1' !v2' !v3'') = halfSipRoundN c (HalfSipData v0 v1 v2 v3')
      v0'' = v0' `xor` m
   in HalfSipData v0'' v1' v2' v3''
{-# INLINE compress #-}

-- | One full HalfSipHash round (two half-rounds).
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
{-# INLINE halfSipRound #-}

-- | Apply 'halfSipRound' @n@ times.
halfSipRoundN :: Word -> HalfSipData -> HalfSipData
halfSipRoundN n !x
  | n <= 0 = x
  | otherwise = halfSipRoundN (n - 1) $! halfSipRound x
