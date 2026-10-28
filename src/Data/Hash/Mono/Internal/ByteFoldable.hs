{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
#if !MIN_VERSION_base(4,19,0)
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

-- | Byte encoding for types.
module Data.Hash.Mono.Internal.ByteFoldable (
    ByteFoldable (..),
)
where

import Data.Bits (Bits (unsafeShiftR, (.&.)))
import Data.Char qualified as Char
import Data.Int (Int16, Int32, Int64, Int8)
#if !MIN_VERSION_base(4,19,0)
import Data.Kind (Constraint)
#endif
import Data.WideWord.Word128 (Word128 (..))
import Data.WideWord.Word256 (Word256 (..))
import Data.Word (Word16, Word32, Word64, Word8)
#if MIN_VERSION_base(4,19,0)
import GHC.TypeError (ErrorMessage (Text), Unsatisfiable)
#else
import GHC.TypeLits (ErrorMessage (Text), TypeError)
#endif
import Numeric.Natural (Natural)

#if MIN_VERSION_base(4,19,0)
type AntiInstance msg = Unsatisfiable msg
#else
-- A type synonym here would make older GHC report the TypeError eagerly at
-- the synonym declaration; a type family only expands it at the use site.
type family AntiInstance msg :: Constraint where
    AntiInstance msg = TypeError msg
#endif

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
                (f z (fromIntegral (0xc0 + (n `unsafeShiftR` 6))))
                (fromIntegral (0x80 + (n .&. 0x3f)))
        | n < 0x10000 =
            f
                ( f
                    (f z (fromIntegral (0xe0 + (n `unsafeShiftR` 12))))
                    (fromIntegral (0x80 + ((n `unsafeShiftR` 6) .&. 0x3f)))
                )
                (fromIntegral (0x80 + (n .&. 0x3f)))
        | otherwise =
            f
                ( f
                    ( f
                        (f z (fromIntegral (0xf0 + (n `unsafeShiftR` 18))))
                        (fromIntegral (0x80 + ((n `unsafeShiftR` 12) .&. 0x3f)))
                    )
                    (fromIntegral (0x80 + ((n `unsafeShiftR` 6) .&. 0x3f)))
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
        f (f z (fromIntegral (w `unsafeShiftR` 8))) (fromIntegral w)
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
                    (f z (fromIntegral (w `unsafeShiftR` 24)))
                    (fromIntegral (w `unsafeShiftR` 16))
                )
                (fromIntegral (w `unsafeShiftR` 8))
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
                                    (f z (fromIntegral (w `unsafeShiftR` 56)))
                                    (fromIntegral (w `unsafeShiftR` 48))
                                )
                                (fromIntegral (w `unsafeShiftR` 40))
                            )
                            (fromIntegral (w `unsafeShiftR` 32))
                        )
                        (fromIntegral (w `unsafeShiftR` 24))
                    )
                    (fromIntegral (w `unsafeShiftR` 16))
                )
                (fromIntegral (w `unsafeShiftR` 8))
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

{- | Anti-instance: 'Int' is as wide as a machine word, so its byte encoding
would differ between platforms. Use a fixed-width type such as 'Int32' or
'Int64' instead.
-}
instance
    ( AntiInstance
        ( 'Text
            "ByteFoldable for Int cannot exist: Int is platform-dependent; use Int32 or Int64 instead."
        )
    ) =>
    ByteFoldable Int
#if !MIN_VERSION_base(4,19,0)
    where
    foldlBytes _ _ _ = error "ByteFoldable Int: unreachable"
#endif

{- | Anti-instance: 'Word' is as wide as a machine word, so its byte encoding
would differ between platforms. Use a fixed-width type such as 'Word32' or
'Word64' instead.
-}
instance
    ( AntiInstance
        ( 'Text
            "ByteFoldable for Word cannot exist: Word is platform-dependent; use Word32 or Word64 instead."
        )
    ) =>
    ByteFoldable Word
#if !MIN_VERSION_base(4,19,0)
    where
    foldlBytes _ _ _ = error "ByteFoldable Word: unreachable"
#endif

{- | Anti-instance: 'Integer' is arbitrary-precision and has no fixed width,
so it has no fixed byte encoding. Use a fixed-width integer type instead.
-}
instance
    ( AntiInstance
        ( 'Text
            "ByteFoldable for Integer cannot exist: Integer is arbitrary-precision; use a fixed-width integer type instead."
        )
    ) =>
    ByteFoldable Integer
#if !MIN_VERSION_base(4,19,0)
    where
    foldlBytes _ _ _ = error "ByteFoldable Integer: unreachable"
#endif

{- | Anti-instance: 'Natural' is arbitrary-precision and has no fixed width,
so it has no fixed byte encoding. Use a fixed-width unsigned integer type
instead.
-}
instance
    ( AntiInstance
        ( 'Text
            "ByteFoldable for Natural cannot exist: Natural is arbitrary-precision; use a fixed-width unsigned integer type instead."
        )
    ) =>
    ByteFoldable Natural
#if !MIN_VERSION_base(4,19,0)
    where
    foldlBytes _ _ _ = error "ByteFoldable Natural: unreachable"
#endif
