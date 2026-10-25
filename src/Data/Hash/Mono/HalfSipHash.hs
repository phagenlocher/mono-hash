{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- | HalfSipHash-@c@-@d@ for any 'MonoFoldable' container.

HalfSipHash is the 32-bit-word variant of SipHash, a PRF family designed by
Jean-Philippe Aumasson and Daniel J. Bernstein. HalfSipHash itself is not
described in the SipHash paper; see
<https://eprint.iacr.org/2012/351 SipHash: a fast short-input PRF> for the
design and the <https://github.com/veorq/SipHash reference implementation>
for the variants.
-}
module Data.Hash.Mono.HalfSipHash (
    HalfSipHashData,
    HalfSipHashResult,
    halfSipHash,
)
where

import Data.Bits (Bits (rotateL, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.Hash.Mono.Internal (byteSwap32)
import Data.Hash.Mono.Internal.ByteFoldable (ByteFoldable (..))
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.Word (Word32, Word64, Word8)

-- | Result types that 'halfSipHash' can produce.
class HalfSipHashResult a where
    -- | Adjust the initial state for the selected result type.
    initialize :: HalfSipData -> HalfSipData

    -- | Run the finalization rounds and return the hash.
    finalization :: Word -> HalfSipData -> a

instance HalfSipHashResult Word32 where
    initialize :: HalfSipData -> HalfSipData
    initialize = id

    finalization :: Word -> HalfSipData -> Word32
    finalization d (HalfSipData !v0 !v1 !v2 !v3) =
        let v2' = v2 `xor` 0xff
            (HalfSipData !_ v1' _ !v3') = halfSipRounds d (HalfSipData v0 v1 v2' v3)
         in v1' `xor` v3'

instance HalfSipHashResult Word64 where
    initialize :: HalfSipData -> HalfSipData
    initialize (HalfSipData !v0 !v1 !v2 !v3) = HalfSipData v0 (v1 `xor` 0xee) v2 v3

    finalization :: Word -> HalfSipData -> Word64
    finalization d (HalfSipData !v0 !v1 !v2 !v3) =
        let v2' = v2 `xor` 0xee
            (HalfSipData !v0' !v1' !v2'' !v3') = halfSipRounds d (HalfSipData v0 v1 v2' v3)
            word0 = v1' `xor` v3'
            v1'' = v1' `xor` 0xdd
            (HalfSipData _ !v1''' _ !v3'') = halfSipRounds d (HalfSipData v0' v1'' v2'' v3')
            word1 = v1''' `xor` v3''
         in (fromIntegral word1 `shiftL` 32) + fromIntegral word0

-- | Constraint for containers hashed with HalfSipHash.
type HalfSipHashData a = (MonoFoldable a, ByteFoldable (Element a))

-- | HalfSipHash internal state, consisting of four 32-bit words.
data HalfSipData = HalfSipData
    { hv0 :: {-# UNPACK #-} !Word32
    , hv1 :: {-# UNPACK #-} !Word32
    , hv2 :: {-# UNPACK #-} !Word32
    , hv3 :: {-# UNPACK #-} !Word32
    }

-- | Internal HalfSipHash accumulator.
data HalfSipStream = HalfSipStream
    { hsData :: {-# UNPACK #-} !HalfSipData
    -- ^ HalfSipHash state.
    , hsWord :: {-# UNPACK #-} !Word32
    -- ^ Input bytes buffered so far, in little-endian order.
    , hsCount :: {-# UNPACK #-} !Int
    -- ^ Number of bytes buffered in 'hsWord' (0-3).
    , hsLen :: {-# UNPACK #-} !Word8
    -- ^ Total number of input bytes, modulo 256.
    }

{- | Compute HalfSipHash-@c@-@d@ of a monomorphic container.

@c@ and @d@ are the compression and finalization round counts;
HalfSipHash-2-4 is the standard variant.

Byte order: the 'Word64' key holds the 8 reference key bytes in
big-endian order, and each 32-bit half is then read as a little-endian
word. The result is the reference implementation's output word: a
'Word32' result is the 32-bit tag. A 'Word64' result packs the first
32-bit tag word in the low half and the second in the high half; its
little-endian serialization is the 8-byte reference digest.

Example:
@
halfSipHash \@'Word32' 2 4 0xc0ffee \"hello\"
@
-}
halfSipHash :: forall result mono. (HalfSipHashData mono, HalfSipHashResult result) => Word -> Word -> Word64 -> mono -> result
halfSipHash c d key mono =
    case ofoldl' (foldlBytes (step c)) initStream mono of
        HalfSipStream{hsData = state, hsWord = word, hsLen = len} ->
            finalization d (compress c state (word .|. (fromIntegral len `shiftL` 24)))
  where
    initData :: HalfSipData
    initData =
        HalfSipData
            { hv0 = k0
            , hv1 = k1
            , hv2 = k0 `xor` 0x6c796765
            , hv3 = k1 `xor` 0x74656462
            }
      where
        k0 :: Word32
        k0 = byteSwap32 . fromIntegral $ key `shiftR` 32

        k1 :: Word32
        k1 = byteSwap32 . fromIntegral $ key .&. 0xffffffff

    initStream :: HalfSipStream
    initStream =
        HalfSipStream
            { hsData = initialize @result initData
            , hsWord = 0
            , hsCount = 0
            , hsLen = 0
            }
{-# INLINEABLE halfSipHash #-}

step :: Word -> HalfSipStream -> Word8 -> HalfSipStream
step c HalfSipStream{hsData = d, hsWord = word, hsCount = n, hsLen = len} !b =
    let word' = word .|. (fromIntegral b `shiftL` (8 * n))
        len' = len + 1
     in if n == 3
            then HalfSipStream{hsData = compress c d word', hsWord = 0, hsCount = 0, hsLen = len'}
            else HalfSipStream{hsData = d, hsWord = word', hsCount = n + 1, hsLen = len'}
{-# INLINE step #-}

compress :: Word -> HalfSipData -> Word32 -> HalfSipData
compress c (HalfSipData !v0 !v1 !v2 !v3) m =
    let v3' = v3 `xor` m
        (HalfSipData !v0' !v1' !v2' !v3'') = halfSipRounds c (HalfSipData v0 v1 v2 v3')
        v0'' = v0' `xor` m
     in HalfSipData v0'' v1' v2' v3''
{-# INLINE compress #-}

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

halfSipRoundN :: Word -> HalfSipData -> HalfSipData
halfSipRoundN 0 !x = x
halfSipRoundN n !x = halfSipRoundN (n - 1) $! halfSipRound x

-- | Unrolled variant of 'halfSipRoundN' for the small counts used in practice.
halfSipRounds :: Word -> HalfSipData -> HalfSipData
halfSipRounds 1 !x = halfSipRound x
halfSipRounds 2 !x = halfSipRound (halfSipRound x)
halfSipRounds 3 !x = halfSipRound (halfSipRound (halfSipRound x))
halfSipRounds 4 !x = halfSipRound (halfSipRound (halfSipRound (halfSipRound x)))
halfSipRounds n !x = halfSipRoundN n x
{-# INLINE halfSipRounds #-}
