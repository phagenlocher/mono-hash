{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- | SipHash-@c@-@d@ for any 'MonoFoldable' container.

Input is consumed one byte at a time in constant space. The result width is
chosen by the type application; see 'sipHash'.
-}
module Data.Hash.Mono.SipHash (
    SipHashData,
    SipHashResult,
    sipHash,
)
where

import Data.Bits (Bits (rotateL, shiftL, xor, (.|.)))
import Data.Hash.Mono.Internal (
    ToWords (..),
    word64ToLE,
 )
import Data.MonoTraversable (Element, MonoFoldable (ofoldl'))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64, Word8)

-- | Output widths that 'sipHash' can produce.
class SipHashResult a where
    {- | Adjust the initial state for the selected output width. The 128-bit
    output mixes a domain separator into the second state word; the 64-bit
    output does not.
    -}
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
         in word64ToLE $ v0' `xor` v1' `xor` v2'' `xor` v3'

instance SipHashResult Word128 where
    initialize :: SipData -> SipData
    initialize (SipData !v0 !v1 !v2 !v3) = SipData v0 (v1 `xor` 0xee) v2 v3

    finalization :: Word -> SipData -> Word128
    finalization d (SipData !v0 !v1 !v2 !v3) =
        let v2' = v2 `xor` 0xee
            (SipData !v0' !v1' !v2'' !v3') = sipRounds d (SipData v0 v1 v2' v3)
            hi = word64ToLE $ v0' `xor` v1' `xor` v2'' `xor` v3'
            v1'' = v1' `xor` 0xdd
            (SipData !v0'' !v1''' !v2''' !v3'') = sipRounds d (SipData v0' v1'' v2'' v3')
            lo = word64ToLE $ v0'' `xor` v1''' `xor` v2''' `xor` v3''
         in Word128 hi lo

{- | Constraint for containers that can be hashed with SipHash.

Requires @ToWords@ for the element type.
-}
type SipHashData a = (MonoFoldable a, ToWords (Element a))

-- | SipHash internal state, consisting of four 64-bit words.
data SipData = SipData
    { sv0 :: {-# UNPACK #-} !Word64
    , sv1 :: {-# UNPACK #-} !Word64
    , sv2 :: {-# UNPACK #-} !Word64
    , sv3 :: {-# UNPACK #-} !Word64
    }

{- | Streaming SipHash state.

The input is consumed one byte at a time, so memory use is constant
regardless of input size.
-}
data SipStream = SipStream
    { ssData :: {-# UNPACK #-} !SipData
    -- ^ Current hash state.
    , ssWord :: {-# UNPACK #-} !Word64
    -- ^ Input bytes buffered so far, in little-endian order.
    , ssCount :: {-# UNPACK #-} !Int
    -- ^ Number of bytes buffered in 'ssWord' (0-7).
    , ssLen :: {-# UNPACK #-} !Word8
    -- ^ Total number of input bytes, modulo 256.
    }

{- | Compute SipHash-@c@-@d@ of a monomorphic container.

@c@ and @d@ are the compression and finalization round counts;
SipHash-2-4 is the standard variant. The 128-bit key is split into two
64-bit halves.

For example:

@
sipHash \@'Word64' 2 4 key input
@
-}
sipHash :: forall result mono. (SipHashData mono, SipHashResult result) => Word -> Word -> Word128 -> mono -> result
sipHash c d key mono =
    case ofoldl' (foldlWords (step c)) initStream mono of
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
        k0 = word64ToLE $ word128Hi64 key

        k1 :: Word64
        k1 = word64ToLE $ word128Lo64 key

    initStream :: SipStream
    initStream =
        SipStream
            { ssData = initialize @result initData
            , ssWord = 0
            , ssCount = 0
            , ssLen = 0
            }
{-# INLINEABLE sipHash #-}

-- | Feed one input byte into the stream, compressing every full 64-bit word.
step :: Word -> SipStream -> Word8 -> SipStream
step c SipStream{ssData = d, ssWord = word, ssCount = n, ssLen = len} !b =
    let word' = word .|. (fromIntegral b `shiftL` (8 * n))
        len' = len + 1
     in if n == 7
            then SipStream{ssData = compress c d word', ssWord = 0, ssCount = 0, ssLen = len'}
            else SipStream{ssData = d, ssWord = word', ssCount = n + 1, ssLen = len'}
{-# INLINE step #-}

-- | Compress one 64-bit input word into the hash state.
compress :: Word -> SipData -> Word64 -> SipData
compress c (SipData !v0 !v1 !v2 !v3) m =
    let v3' = v3 `xor` m
        (SipData !v0' !v1' !v2' !v3'') = sipRounds c (SipData v0 v1 v2 v3')
        v0'' = v0' `xor` m
     in SipData v0'' v1' v2' v3''
{-# INLINE compress #-}

-- | One full SipHash round (two half-rounds).
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

{- | Apply 'sipRound' @n@ times.

This is the general fallback. It is self-recursive, so GHC treats it as a
loop breaker and never inlines or unrolls it, even when @n@ is a literal.
-}
sipRoundN :: Word -> SipData -> SipData
sipRoundN 0 !x = x
sipRoundN n !x = sipRoundN (n - 1) $! sipRound x

{- | Apply 'sipRound' @n@ times, with unrolled cases for the small counts used
in practice (SipHash-2-4 uses 2 and 4).

Unlike 'sipRoundN' this function is not recursive, so the 'INLINE' pragma is
honoured: calls with a small @n@ become straight-line code instead of a call
to a loop. (When @n@ is a known literal the case can fold away as well.)
-}
sipRounds :: Word -> SipData -> SipData
sipRounds 1 !x = sipRound x
sipRounds 2 !x = sipRound (sipRound x)
sipRounds 3 !x = sipRound (sipRound (sipRound x))
sipRounds 4 !x = sipRound (sipRound (sipRound (sipRound x)))
sipRounds n !x = sipRoundN n x
{-# INLINE sipRounds #-}
