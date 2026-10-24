{-# LANGUAGE InstanceSigs #-}

-- | Internal helpers shared by the hash implementations.
module Data.Hash.Mono.Internal (
    ToWords (..),
    byteSwap64,
    byteSwap32,
)
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.Char qualified as Char
import Data.Int (Int16, Int32, Int64, Int8)
import Data.WideWord.Word128 (Word128 (..))
import Data.WideWord.Word256 (Word256 (..))
import Data.Word (Word16, Word32, Word64, Word8)

{- | Types with a byte encoding for hashing.

Fixed-width integers are encoded most significant byte first; signed
integers as their two's-complement bit pattern. For example, 'Int8' has
the same bytes as the corresponding 'Word8'. 'Char' is encoded as UTF-8,
so 'String' and 'Text' hash the same bytes as their UTF-8 encoding. Lone
surrogate code points, which 'Char' can represent, use the 3-byte
<https://simonsapin.github.io/wtf-8/ WTF-8> form; that keeps the encoding
injective and total, but note that 'Data.Text.Text' replaces lone
surrogates with @U+FFFD@, so a 'String' and a 'Text' containing one encode
differently. 'Word8' contributes its byte unchanged, so byte strings are
hashed as their raw bytes.
-}
class ToWords a where
    {- | Fold over the bytes of a value in the order the hash consumes them.

    Multi-byte integers are emitted most significant byte first. The
    accumulator is not forced; use a strict left fold when hashing.
    -}
    foldlWords :: (b -> Word8 -> b) -> b -> a -> b

instance ToWords Char where
    foldlWords f z c
        | n < 0x80 = f z (fromIntegral n)
        | n < 0x800 =
            f
                (f z (fromIntegral (0xc0 + (n `shiftR` 6))))
                (fromIntegral (0x80 + (n .&. 0x3f)))
        | n < 0x10000 =
            f
                ( f
                    (f z (fromIntegral (0xe0 + (n `shiftR` 12))))
                    (fromIntegral (0x80 + ((n `shiftR` 6) .&. 0x3f)))
                )
                (fromIntegral (0x80 + (n .&. 0x3f)))
        | otherwise =
            f
                ( f
                    ( f
                        (f z (fromIntegral (0xf0 + (n `shiftR` 18))))
                        (fromIntegral (0x80 + ((n `shiftR` 12) .&. 0x3f)))
                    )
                    (fromIntegral (0x80 + ((n `shiftR` 6) .&. 0x3f)))
                )
                (fromIntegral (0x80 + (n .&. 0x3f)))
      where
        n :: Int
        n = Char.ord c
    {-# INLINE foldlWords #-}

instance ToWords Word8 where
    foldlWords f = f
    {-# INLINE foldlWords #-}

instance ToWords Int8 where
    foldlWords f z i = foldlWords f z (fromIntegral i :: Word8)
    {-# INLINE foldlWords #-}

instance ToWords Word16 where
    foldlWords f z w =
        f (f z (fromIntegral (w `shiftR` 8))) (fromIntegral w)
    {-# INLINE foldlWords #-}

instance ToWords Int16 where
    foldlWords f z i = foldlWords f z (fromIntegral i :: Word16)
    {-# INLINE foldlWords #-}

instance ToWords Word32 where
    foldlWords f z w =
        f
            ( f
                ( f
                    (f z (fromIntegral (w `shiftR` 24)))
                    (fromIntegral (w `shiftR` 16))
                )
                (fromIntegral (w `shiftR` 8))
            )
            (fromIntegral w)
    {-# INLINE foldlWords #-}

instance ToWords Int32 where
    foldlWords f z i = foldlWords f z (fromIntegral i :: Word32)
    {-# INLINE foldlWords #-}

instance ToWords Word64 where
    foldlWords f z w =
        f
            ( f
                ( f
                    ( f
                        ( f
                            ( f
                                ( f
                                    (f z (fromIntegral (w `shiftR` 56)))
                                    (fromIntegral (w `shiftR` 48))
                                )
                                (fromIntegral (w `shiftR` 40))
                            )
                            (fromIntegral (w `shiftR` 32))
                        )
                        (fromIntegral (w `shiftR` 24))
                    )
                    (fromIntegral (w `shiftR` 16))
                )
                (fromIntegral (w `shiftR` 8))
            )
            (fromIntegral w)
    {-# INLINE foldlWords #-}

instance ToWords Int64 where
    foldlWords f z i = foldlWords f z (fromIntegral i :: Word64)
    {-# INLINE foldlWords #-}

instance ToWords Word128 where
    foldlWords f z w =
        foldlWords f (foldlWords f z (word128Hi64 w)) (word128Lo64 w)
    {-# INLINE foldlWords #-}

instance ToWords Word256 where
    foldlWords f z w =
        foldlWords
            f
            ( foldlWords
                f
                ( foldlWords
                    f
                    (foldlWords f z (word256hi w))
                    (word256m1 w)
                )
                (word256m0 w)
            )
            (word256lo w)
    {-# INLINE foldlWords #-}

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
