{-# LANGUAGE InstanceSigs #-}

{- | Types with a byte encoding for hashing.

This module is exposed so that container elements of user-defined types can
be hashed by giving them a 'ByteFoldable' instance. It is named under
@Data.Hash.Mono.Internal@ because it describes the encoding layer rather than
a hash algorithm; the rest of "Data.Hash.Mono.Internal" is not exposed.
-}
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

{- | Types with a byte encoding for hashing.

Fixed-width integers are encoded most significant byte first; signed
integers as their two's-complement bit pattern. For example, 'Int8' has
the same bytes as the corresponding 'Word8'. 'Char' is encoded as UTF-8,
so 'String' and @Text@ hash the same bytes as their UTF-8 encoding. Lone
surrogate code points, which 'Char' can represent, use the 3-byte
<https://simonsapin.github.io/wtf-8/ WTF-8> form; that keeps the encoding
injective and total, but note that @Text@ replaces lone
surrogates with @U+FFFD@, so a 'String' and a @Text@ containing one encode
differently. 'Word8' contributes its byte unchanged, so byte strings are
hashed as their raw bytes.

An instance defines a fixed byte stream: the bytes must not depend on the
accumulation function or the initial accumulator, and they are the bytes
the hash is computed over. Changing an instance therefore changes every
hash of every container holding that element type, which matters if hash
values are persisted.
-}
class ByteFoldable a where
    {- | Fold over the bytes of a value in the order the hash consumes them.

    Multi-byte integers are emitted most significant byte first. The
    accumulator is not forced; use a strict left fold when hashing.
    -}
    foldlBytes :: (b -> Word8 -> b) -> b -> a -> b

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

instance ByteFoldable Int8 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word8)
    {-# INLINE foldlBytes #-}

instance ByteFoldable Word16 where
    foldlBytes f z w =
        f (f z (fromIntegral (w `shiftR` 8))) (fromIntegral w)
    {-# INLINE foldlBytes #-}

instance ByteFoldable Int16 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word16)
    {-# INLINE foldlBytes #-}

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

instance ByteFoldable Int32 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word32)
    {-# INLINE foldlBytes #-}

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

instance ByteFoldable Int64 where
    foldlBytes f z i = foldlBytes f z (fromIntegral i :: Word64)
    {-# INLINE foldlBytes #-}

instance ByteFoldable Word128 where
    foldlBytes f z w =
        foldlBytes f (foldlBytes f z (word128Hi64 w)) (word128Lo64 w)
    {-# INLINE foldlBytes #-}

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
