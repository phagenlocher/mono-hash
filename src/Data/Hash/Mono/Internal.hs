{-# LANGUAGE InstanceSigs #-}

module Data.Hash.Mono.Internal
  ( ToWords (..),
    word64ToLE,
    word32ToLE,
    mkWord16,
    mkWord32,
    mkWord64,
  )
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.Char qualified as Char
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word16, Word32, Word64, Word8)

class ToWords a where
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

instance ToWords Word16 where
  foldlWords f z w =
    f (f z (fromIntegral (w `shiftR` 8))) (fromIntegral w)
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

instance ToWords Word128 where
  foldlWords f z w = go 15 z
    where
      go i acc
        | i < 0 = acc
        | otherwise =
            go (i - 1) $! f acc (fromIntegral (w `shiftR` (8 * i)))
  {-# INLINE foldlWords #-}

instance ToWords Word256 where
  foldlWords f z w = go 31 z
    where
      go i acc
        | i < 0 = acc
        | otherwise =
            go (i - 1) $! f acc (fromIntegral (w `shiftR` (8 * i)))
  {-# INLINE foldlWords #-}

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

mkWord16 :: Word8 -> Word8 -> Word16
mkWord16 hi lo = (fromIntegral hi `shiftL` 8) + fromIntegral lo
{-# INLINE mkWord16 #-}

mkWord32 :: Word16 -> Word16 -> Word32
mkWord32 hi lo = (fromIntegral hi `shiftL` 16) + fromIntegral lo
{-# INLINE mkWord32 #-}

mkWord64 :: Word32 -> Word32 -> Word64
mkWord64 hi lo = (fromIntegral hi `shiftL` 32) + fromIntegral lo
{-# INLINE mkWord64 #-}
