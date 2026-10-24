{-# LANGUAGE InstanceSigs #-}

-- | Byte encoding for types.
module Data.Hash.Mono.Internal.ByteFoldable (
    ByteFoldable (..),
)
where

import Data.Bits (Bits (shiftR, (.&.)))
import Data.Char qualified as Char
import Data.Int (Int16, Int32, Int64, Int8)
import Data.WideWord.Word128 (Word128 (..))
import Data.WideWord.Word256 (Word256 (..))
import Data.Word (Word16, Word32, Word64, Word8)

{- | Types with a fixed byte encoding.

An instance must define a fixed order for the bytes to be folded.
-}
class ByteFoldable a where
    -- | Fold over the bytes of a value.
    foldlBytes :: (b -> Word8 -> b) -> b -> a -> b

{- | 'Char' is encoded as
<https://wtf-8.codeberg.page WTF-8>, the UTF-8 encoding extended to
lone surrogates. For all other code points the bytes are ordinary UTF-8.

'Char' can represent lone surrogates, which strict UTF-8 cannot encode.
Replacing them, as @Text@ does with @U+FFFD@, would be lossy and would let
distinct strings produce the same bytes. WTF-8 instead uses the ordinary
three-byte shape for them, so every 'Char' has an encoding, the encoding
stays injective, and valid text remains plain UTF-8. A 'String' and a
@Text@ containing a lone surrogate therefore encode differently.
-}
instance ByteFoldable Char where
    foldlBytes f z c
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
    {-# INLINE foldlBytes #-}

instance ByteFoldable Word8 where
    foldlBytes f = f
    {-# INLINE foldlBytes #-}

{- | Encoded as its two's-complement bit pattern, the same bytes as the
corresponding 'Word8'.
-}
instance ByteFoldable Int8 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word8)
    {-# INLINE foldlBytes #-}

-- | Encoded as two bytes, most significant first.
instance ByteFoldable Word16 where
    foldlBytes f z w =
        f (f z (fromIntegral (w `shiftR` 8))) (fromIntegral w)
    {-# INLINE foldlBytes #-}

{- | Encoded as its two's-complement bit pattern, the same bytes as the
corresponding 'Word16'.
-}
instance ByteFoldable Int16 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word16)
    {-# INLINE foldlBytes #-}

-- | Encoded as four bytes, most significant first.
instance ByteFoldable Word32 where
    foldlBytes f z w =
        f
            ( f
                ( f
                    (f z (fromIntegral (w `shiftR` 24)))
                    (fromIntegral (w `shiftR` 16))
                )
                (fromIntegral (w `shiftR` 8))
            )
            (fromIntegral w)
    {-# INLINE foldlBytes #-}

{- | Encoded as its two's-complement bit pattern, the same bytes as the
corresponding 'Word32'.
-}
instance ByteFoldable Int32 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word32)
    {-# INLINE foldlBytes #-}

-- | Encoded as eight bytes, most significant first.
instance ByteFoldable Word64 where
    foldlBytes f z w =
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
    {-# INLINE foldlBytes #-}

{- | Encoded as its two's-complement bit pattern, the same bytes as the
corresponding 'Word64'.
-}
instance ByteFoldable Int64 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word64)
    {-# INLINE foldlBytes #-}

-- | Encoded as sixteen bytes, most significant first.
instance ByteFoldable Word128 where
    foldlBytes f z w =
        foldlBytes f (foldlBytes f z (word128Hi64 w)) (word128Lo64 w)
    {-# INLINE foldlBytes #-}

-- | Encoded as thirty-two bytes, most significant first.
instance ByteFoldable Word256 where
    foldlBytes f z w =
        foldlBytes
            f
            ( foldlBytes
                f
                ( foldlBytes
                    f
                    (foldlBytes f z (word256hi w))
                    (word256m1 w)
                )
                (word256m0 w)
            )
            (word256lo w)
    {-# INLINE foldlBytes #-}
