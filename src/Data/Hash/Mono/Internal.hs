-- | Internal helpers shared by the hash implementations.
module Data.Hash.Mono.Internal (
    byteSwap64,
    byteSwap32,
)
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.Word (Word32, Word64)

{- | Byte-swap a 'Word64'.

The public API takes keys as big-endian byte strings; the reference
implementation reads each 64-bit key half as a little-endian word.
-}
byteSwap64 :: Word64 -> Word64
byteSwap64 w =
    let c0 = (w `shiftR` 56) .&. 0xff
        c1 = (w `shiftR` 48) .&. 0xff
        c2 = (w `shiftR` 40) .&. 0xff
        c3 = (w `shiftR` 32) .&. 0xff
        c4 = (w `shiftR` 24) .&. 0xff
        c5 = (w `shiftR` 16) .&. 0xff
        c6 = (w `shiftR` 8) .&. 0xff
        c7 = w .&. 0xff
     in c0
            + (c1 `shiftL` 8)
            + (c2 `shiftL` 16)
            + (c3 `shiftL` 24)
            + (c4 `shiftL` 32)
            + (c5 `shiftL` 40)
            + (c6 `shiftL` 48)
            + (c7 `shiftL` 56)
{-# INLINEABLE byteSwap64 #-}

{- | Byte-swap a 'Word32'.

The public API takes keys as big-endian byte strings; the reference
implementation reads each 32-bit key half as a little-endian word.
-}
byteSwap32 :: Word32 -> Word32
byteSwap32 w =
    let c0 = (w `shiftR` 24) .&. 0xff
        c1 = (w `shiftR` 16) .&. 0xff
        c2 = (w `shiftR` 8) .&. 0xff
        c3 = w .&. 0xff
     in c0
            + (c1 `shiftL` 8)
            + (c2 `shiftL` 16)
            + (c3 `shiftL` 24)
{-# INLINEABLE byteSwap32 #-}
