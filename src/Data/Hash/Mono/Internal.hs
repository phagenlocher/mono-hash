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
import Data.List qualified as List
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word16, Word32, Word64, Word8)

class ToWords a where
  toWords :: a -> [Word8]

instance ToWords Char where
  toWords :: Char -> [Word8]
  toWords c
    | c == '\x00' = [0]
    | otherwise = List.reverse $ go (Char.ord c)
    where
      go :: Int -> [Word8]
      go n
        | n <= 0 = []
        | otherwise =
            let octet = fromIntegral (n .&. 0xff)
             in octet : go (n `shiftR` 8)
  {-# INLINEABLE toWords #-}

instance ToWords Word8 where
  toWords :: Word8 -> [Word8]
  toWords w = [w]
  {-# INLINE toWords #-}

instance ToWords Word16 where
  toWords :: Word16 -> [Word8]
  toWords w =
    [ fromIntegral $ (w `shiftR` 8) .&. 0xff,
      fromIntegral $ w .&. 0xff
    ]
  {-# INLINE toWords #-}

instance ToWords Word32 where
  toWords :: Word32 -> [Word8]
  toWords w =
    [ fromIntegral $ (w `shiftR` 24) .&. 0xff,
      fromIntegral $ (w `shiftR` 16) .&. 0xff,
      fromIntegral $ (w `shiftR` 8) .&. 0xff,
      fromIntegral $ w .&. 0xff
    ]
  {-# INLINE toWords #-}

instance ToWords Word64 where
  toWords :: Word64 -> [Word8]
  toWords w =
    [ fromIntegral $ (w `shiftR` 56) .&. 0xff,
      fromIntegral $ (w `shiftR` 48) .&. 0xff,
      fromIntegral $ (w `shiftR` 40) .&. 0xff,
      fromIntegral $ (w `shiftR` 32) .&. 0xff,
      fromIntegral $ (w `shiftR` 24) .&. 0xff,
      fromIntegral $ (w `shiftR` 16) .&. 0xff,
      fromIntegral $ (w `shiftR` 8) .&. 0xff,
      fromIntegral $ w .&. 0xff
    ]
  {-# INLINE toWords #-}

instance ToWords Word128 where
  toWords :: Word128 -> [Word8]
  toWords w =
    [ fromIntegral $ (w `shiftR` 120) .&. 0xff,
      fromIntegral $ (w `shiftR` 112) .&. 0xff,
      fromIntegral $ (w `shiftR` 104) .&. 0xff,
      fromIntegral $ (w `shiftR` 96) .&. 0xff,
      fromIntegral $ (w `shiftR` 88) .&. 0xff,
      fromIntegral $ (w `shiftR` 80) .&. 0xff,
      fromIntegral $ (w `shiftR` 72) .&. 0xff,
      fromIntegral $ (w `shiftR` 64) .&. 0xff,
      fromIntegral $ (w `shiftR` 56) .&. 0xff,
      fromIntegral $ (w `shiftR` 48) .&. 0xff,
      fromIntegral $ (w `shiftR` 40) .&. 0xff,
      fromIntegral $ (w `shiftR` 32) .&. 0xff,
      fromIntegral $ (w `shiftR` 24) .&. 0xff,
      fromIntegral $ (w `shiftR` 16) .&. 0xff,
      fromIntegral $ (w `shiftR` 8) .&. 0xff,
      fromIntegral $ w .&. 0xff
    ]
  {-# INLINE toWords #-}

instance ToWords Word256 where
  toWords :: Word256 -> [Word8]
  toWords w =
    [ fromIntegral $ (w `shiftR` 248) .&. 0xff,
      fromIntegral $ (w `shiftR` 240) .&. 0xff,
      fromIntegral $ (w `shiftR` 232) .&. 0xff,
      fromIntegral $ (w `shiftR` 224) .&. 0xff,
      fromIntegral $ (w `shiftR` 216) .&. 0xff,
      fromIntegral $ (w `shiftR` 208) .&. 0xff,
      fromIntegral $ (w `shiftR` 200) .&. 0xff,
      fromIntegral $ (w `shiftR` 192) .&. 0xff,
      fromIntegral $ (w `shiftR` 184) .&. 0xff,
      fromIntegral $ (w `shiftR` 176) .&. 0xff,
      fromIntegral $ (w `shiftR` 168) .&. 0xff,
      fromIntegral $ (w `shiftR` 160) .&. 0xff,
      fromIntegral $ (w `shiftR` 152) .&. 0xff,
      fromIntegral $ (w `shiftR` 144) .&. 0xff,
      fromIntegral $ (w `shiftR` 136) .&. 0xff,
      fromIntegral $ (w `shiftR` 128) .&. 0xff,
      fromIntegral $ (w `shiftR` 120) .&. 0xff,
      fromIntegral $ (w `shiftR` 112) .&. 0xff,
      fromIntegral $ (w `shiftR` 104) .&. 0xff,
      fromIntegral $ (w `shiftR` 96) .&. 0xff,
      fromIntegral $ (w `shiftR` 88) .&. 0xff,
      fromIntegral $ (w `shiftR` 80) .&. 0xff,
      fromIntegral $ (w `shiftR` 72) .&. 0xff,
      fromIntegral $ (w `shiftR` 64) .&. 0xff,
      fromIntegral $ (w `shiftR` 56) .&. 0xff,
      fromIntegral $ (w `shiftR` 48) .&. 0xff,
      fromIntegral $ (w `shiftR` 40) .&. 0xff,
      fromIntegral $ (w `shiftR` 32) .&. 0xff,
      fromIntegral $ (w `shiftR` 24) .&. 0xff,
      fromIntegral $ (w `shiftR` 16) .&. 0xff,
      fromIntegral $ (w `shiftR` 8) .&. 0xff,
      fromIntegral $ w .&. 0xff
    ]
  {-# INLINE toWords #-}

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