{-# LANGUAGE InstanceSigs #-}

-- | Internal helpers shared by the hash implementations.
module Data.Hash.Mono.Internal
  ( ToWords (..),
    word64ToLE,
    word32ToLE,
  )
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.Char qualified as Char
import Data.Int (Int16, Int32, Int64, Int8)
import Data.WideWord.Word128 (Word128 (..))
import Data.WideWord.Word256 (Word256 (..))
import Data.Word (Word16, Word32, Word64, Word8)

-- | Types that can be viewed as a sequence of bytes.
--
-- The fold form is used instead of returning a list so that hashing does not
-- allocate a list cell (and often a boxed number) for every input byte.
--
-- Fixed-width signed integers are hashed as their two's-complement bit
-- pattern. For example, @Int8@ has the same bytes as the corresponding
-- @Word8@.
class ToWords a where
  -- | Strictly fold over the bytes of a value in hash-input order.
  --
  -- The accumulator is not forced by this function itself; callers that
  -- need strictness should use a strict left fold.
  foldlWords :: (b -> Word8 -> b) -> b -> a -> b

instance ToWords Char where
  foldlWords f z c
    | n < 0x100 = f z (fromIntegral n)
    | n < 0x10000 =
        f (f z (fromIntegral (n `shiftR` 8))) (fromIntegral n)
    | otherwise =
        f
          ( f
              (f z (fromIntegral (n `shiftR` 16)))
              (fromIntegral (n `shiftR` 8))
          )
          (fromIntegral n)
    where
      n :: Int
      n = Char.ord c
  {-# INLINE foldlWords #-}

instance ToWords Word8 where
  foldlWords f z w = f z w
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
    foldlWords f
      ( foldlWords f
          ( foldlWords f
              (foldlWords f z (word256hi w))
              (word256m1 w)
          )
          (word256m0 w)
      )
      (word256lo w)
  {-# INLINE foldlWords #-}

-- | Reverse the byte order of a 'Word64'.
word64ToLE :: Word64 -> Word64
word64ToLE w =
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
{-# INLINEABLE word64ToLE #-}

-- | Reverse the byte order of a 'Word32'.
word32ToLE :: Word32 -> Word32
word32ToLE w =
  let c0 = (w `shiftR` 24) .&. 0xff
      c1 = (w `shiftR` 16) .&. 0xff
      c2 = (w `shiftR` 8) .&. 0xff
      c3 = w .&. 0xff
   in c0
        + (c1 `shiftL` 8)
        + (c2 `shiftL` 16)
        + (c3 `shiftL` 24)
{-# INLINEABLE word32ToLE #-}
