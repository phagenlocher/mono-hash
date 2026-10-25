{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- | SipHash-@c@-@d@ for any 'MonoFoldable' container.

Designed by Jean-Philippe Aumasson and Daniel J. Bernstein. See
<https://eprint.iacr.org/2012/351 SipHash: a fast short-input PRF> and the
<https://github.com/veorq/SipHash reference implementation>.
-}
module Data.Hash.Mono.SipHash (
    SipHashData,
    SipHashResult,
    sipHash,
)
where

import Data.Bits (Bits (rotateL, shiftL, xor, (.|.)))
import Data.Hash.Mono.Internal (byteSwap64)
import Data.Hash.Mono.Internal.ByteFoldable (ByteFoldable (..))
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64, Word8)

-- | Result types that 'sipHash' can produce.
class SipHashResult a where
    -- | Adjust the initial state for the selected result type.
    initialize :: SipData -> SipData

    -- | Run the finalization rounds and return the hash.
    finalization :: Word -> SipData -> a

instance SipHashResult Word64 where
    initialize :: SipData -> SipData
    initialize = id

    finalization :: Word -> SipData -> Word64
    finalization d (SipData !v0 !v1 !v2 !v3) =
        let v2' = v2 `xor` 0xff
            (SipData !v0' !v1' !v2'' !v3') = sipRounds d (SipData v0 v1 v2' v3)
         in v0' `xor` v1' `xor` v2'' `xor` v3'

instance SipHashResult Word128 where
    initialize :: SipData -> SipData
    initialize (SipData !v0 !v1 !v2 !v3) = SipData v0 (v1 `xor` 0xee) v2 v3

    finalization :: Word -> SipData -> Word128
    finalization d (SipData !v0 !v1 !v2 !v3) =
        let v2' = v2 `xor` 0xee
            (SipData !v0' !v1' !v2'' !v3') = sipRounds d (SipData v0 v1 v2' v3)
            word0 = v0' `xor` v1' `xor` v2'' `xor` v3'
            v1'' = v1' `xor` 0xdd
            (SipData !v0'' !v1''' !v2''' !v3'') = sipRounds d (SipData v0' v1'' v2'' v3')
            word1 = v0'' `xor` v1''' `xor` v2''' `xor` v3''
         in Word128 word1 word0

-- | Constraint for containers hashable with SipHash.
type SipHashData a = (MonoFoldable a, ByteFoldable (Element a))

-- | SipHash internal state.
data SipData = SipData
    { sv0 :: {-# UNPACK #-} !Word64
    , sv1 :: {-# UNPACK #-} !Word64
    , sv2 :: {-# UNPACK #-} !Word64
    , sv3 :: {-# UNPACK #-} !Word64
    }

-- | Internal SipHash accumulator.
data SipStream = SipStream
    { ssData :: {-# UNPACK #-} !SipData
    -- ^ SipHash state.
    , ssWord :: {-# UNPACK #-} !Word64
    -- ^ Input bytes buffered so far, in little-endian order.
    , ssCount :: {-# UNPACK #-} !Int
    -- ^ Number of bytes buffered in 'ssWord'.
    , ssLen :: {-# UNPACK #-} !Word8
    -- ^ Total number of input bytes, modulo 256.
    }

{- | Compute SipHash-@c@-@d@ of a monomorphic container.

@c@ and @d@ are the compression and finalization round counts;
SipHash-2-4 is the standard variant.

Byte order: the @Word128@ key holds the 16 reference key bytes in
big-endian order, and each 64-bit half is then read as a little-endian
word. The result is the reference implementation's output word: a
'Word64' result is the 64-bit tag. A @Word128@ result packs the first
64-bit tag word in the low half and the second in the high half; its
little-endian serialization is the 16-byte reference digest.

Example:
@
sipHash \@'Word64' 2 4 0xc0ffee \"hello\"
@
-}
sipHash :: forall result mono. (SipHashData mono, SipHashResult result) => Word -> Word -> Word128 -> mono -> result
sipHash c d key mono =
    case ofoldl' (foldlBytes (step c)) initStream mono of
        SipStream{ssData = state, ssWord = word, ssLen = len} ->
            finalization d (compress c state (word .|. (fromIntegral len `shiftL` 56)))
  where
    initData :: SipData
    initData =
        SipData
            { sv0 = k0 `xor` 0x736f6d6570736575
            , sv1 = k1 `xor` 0x646f72616e646f6d
            , sv2 = k0 `xor` 0x6c7967656e657261
            , sv3 = k1 `xor` 0x7465646279746573
            }
      where
        k0 :: Word64
        k0 = byteSwap64 $ word128Hi64 key

        k1 :: Word64
        k1 = byteSwap64 $ word128Lo64 key

    initStream :: SipStream
    initStream =
        SipStream
            { ssData = initialize @result initData
            , ssWord = 0
            , ssCount = 0
            , ssLen = 0
            }
{-# INLINEABLE sipHash #-}

step :: Word -> SipStream -> Word8 -> SipStream
step c SipStream{ssData = d, ssWord = word, ssCount = n, ssLen = len} !b =
    let word' = word .|. (fromIntegral b `shiftL` (8 * n))
        len' = len + 1
     in if n == 7
            then SipStream{ssData = compress c d word', ssWord = 0, ssCount = 0, ssLen = len'}
            else SipStream{ssData = d, ssWord = word', ssCount = n + 1, ssLen = len'}
{-# INLINE step #-}

compress :: Word -> SipData -> Word64 -> SipData
compress c (SipData !v0 !v1 !v2 !v3) m =
    let v3' = v3 `xor` m
        (SipData !v0' !v1' !v2' !v3'') = sipRounds c (SipData v0 v1 v2 v3')
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
sipRoundN 0 !x = x
sipRoundN n !x = sipRoundN (n - 1) $! sipRound x

-- | Unrolled variant of 'sipRoundN' for the small counts used in practice.
sipRounds :: Word -> SipData -> SipData
sipRounds 1 !x = sipRound x
sipRounds 2 !x = sipRound (sipRound x)
sipRounds 3 !x = sipRound (sipRound (sipRound x))
sipRounds 4 !x = sipRound (sipRound (sipRound (sipRound x)))
sipRounds n !x = sipRoundN n x
{-# INLINE sipRounds #-}
