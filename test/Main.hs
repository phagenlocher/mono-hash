{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Bits (Bits, shiftL, shiftR)
import Data.Hash.Mono.FNV (fnv1, fnv1a)
import Data.Hash.Mono.HalfSipHash
import Data.Hash.Mono.SipHash
import Data.Text (Text)
import qualified Data.Text as T
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Test.Data.FNV
import qualified Test.Data.HalfSipHash
import qualified Test.Data.SipHash
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

-- | A string containing ASCII, one-byte non-ASCII, and multi-byte code
-- points.  The library hashes the code point's big-endian bytes.
charEncodingInput :: String
charEncodingInput = "\x00\x7f\x80\xff\x100\x1234\x1F600"

charBytes :: Char -> [Word8]
charBytes c
  | n < 0x100 = [fromIntegral n]
  | n < 0x10000 = [fromIntegral (n `shiftR` 8), fromIntegral n]
  | otherwise =
      [ fromIntegral (n `shiftR` 16),
        fromIntegral (n `shiftR` 8),
        fromIntegral n
      ]
  where
    n = fromEnum c

-- | Big-endian bytes of a fixed-width word.
wordBytes :: (Bits a, Integral a) => Int -> a -> [Word8]
wordBytes width w =
  [fromIntegral (w `shiftR` (8 * i)) | i <- [width - 1, width - 2 .. 0]]

packWords :: (Bits a, Integral a) => Int -> [a] -> BS.ByteString
packWords width = BS.pack . concatMap (wordBytes width)

word16Samples :: [Word16]
word16Samples = [0x0102, 0x0304, 0xfffe]

word32Samples :: [Word32]
word32Samples = [0x01020304, 0x05060708, 0xdeadbeef]

word64Samples :: [Word64]
word64Samples = [0x0102030405060708, 0x1112131415161718, 0xdeadbeefcafed00d]

word128Sample :: Word128
word128Sample =
  (fromIntegral (0x0102030405060708 :: Word64) `shiftL` 64)
    + fromIntegral (0x1112131415161718 :: Word64)

word256Sample :: Word256
word256Sample =
  (fromIntegral (0x2122232425262728 :: Word64) `shiftL` 192)
    + (fromIntegral (0x3132333435363738 :: Word64) `shiftL` 128)
    + (fromIntegral (0x4142434445464748 :: Word64) `shiftL` 64)
    + fromIntegral (0x5152535455565758 :: Word64)

tests :: TestTree
tests =
  testGroup
    "Golden Samples"
    [ testGroup
        "FNV"
        [ testCase "FNV1  Word32 String" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @String) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 String" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @String) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 String" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @String) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 String" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @String) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Text" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @Text) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Text" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @Text) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Text" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @Text) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Text" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @Text) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 ByteString" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @BS.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 ByteString" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @BS.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 ByteString" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @BS.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 ByteString" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @BS.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Lazy ByteString" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @BSL.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Lazy ByteString" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @BSL.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Lazy ByteString" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @BSL.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Lazy ByteString" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @BSL.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 ByteString Char8" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @BSC.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 ByteString Char8" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @BSC.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 ByteString Char8" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @BSC.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 ByteString Char8" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @BSC.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Lazy ByteString Char8" $ fmap (fnv1 @Word32) (Test.Data.FNV.inputs @BSLC.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Lazy ByteString Char8" $ fmap (fnv1a @Word32) (Test.Data.FNV.inputs @BSLC.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Lazy ByteString Char8" $ fmap (fnv1 @Word64) (Test.Data.FNV.inputs @BSLC.ByteString) @?= Test.Data.FNV.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Lazy ByteString Char8" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @BSLC.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord64,
          testCase "FNV1a Word64 Char encoding matches bytes" $ do
            let bytes = BS.pack (concatMap charBytes charEncodingInput)
            fnv1a @Word64 charEncodingInput @?= fnv1a @Word64 bytes
            fnv1a @Word64 (T.pack charEncodingInput) @?= fnv1a @Word64 bytes,
          testCase "FNV1a Word64 word elements match bytes" $ do
            fnv1a @Word64 [word128Sample] @?= fnv1a @Word64 (BS.pack (wordBytes 16 word128Sample))
            fnv1a @Word64 [word256Sample] @?= fnv1a @Word64 (BS.pack (wordBytes 32 word256Sample)),
          testCase "FNV1 Word64 small word elements match bytes" $ do
            fnv1 @Word64 word16Samples @?= fnv1 @Word64 (packWords 2 word16Samples)
            fnv1 @Word64 word32Samples @?= fnv1 @Word64 (packWords 4 word32Samples)
            fnv1 @Word64 word64Samples @?= fnv1 @Word64 (packWords 8 word64Samples),
          testCase "FNV1a Word64 small word elements match bytes" $ do
            fnv1a @Word64 word16Samples @?= fnv1a @Word64 (packWords 2 word16Samples)
            fnv1a @Word64 word32Samples @?= fnv1a @Word64 (packWords 4 word32Samples)
            fnv1a @Word64 word64Samples @?= fnv1a @Word64 (packWords 8 word64Samples)
        ],
      testGroup
        "SipHash"
        [ testCase "SipHash-2-4 Word64 String" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @String) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Text" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @Text) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 ByteString" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BS.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Lazy ByteString" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSL.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 ByteString Char8" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSC.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Lazy ByteString Char8" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSLC.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word128 String" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @String) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Text" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @Text) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 ByteString" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BS.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Lazy ByteString" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSL.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 ByteString Char8" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSC.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Lazy ByteString Char8" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSLC.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word64 boundary lengths" $ fmap (sipHash @Word64 2 4 Test.Data.SipHash.key) Test.Data.SipHash.boundaryInputs @?= Test.Data.SipHash.boundaryVectorsForWord64,
          testCase "SipHash-2-4 Word64 Char encoding matches bytes" $ do
            let bytes = BS.pack (concatMap charBytes charEncodingInput)
            sipHash @Word64 2 4 Test.Data.SipHash.key charEncodingInput @?= sipHash @Word64 2 4 Test.Data.SipHash.key bytes
            sipHash @Word64 2 4 Test.Data.SipHash.key (T.pack charEncodingInput) @?= sipHash @Word64 2 4 Test.Data.SipHash.key bytes,
          testCase "SipHash-2-4 Word64 word elements match bytes" $ do
            sipHash @Word64 2 4 Test.Data.SipHash.key [word128Sample] @?= sipHash @Word64 2 4 Test.Data.SipHash.key (BS.pack (wordBytes 16 word128Sample))
            sipHash @Word64 2 4 Test.Data.SipHash.key [word256Sample] @?= sipHash @Word64 2 4 Test.Data.SipHash.key (BS.pack (wordBytes 32 word256Sample)),
          testCase "SipHash-2-4 Word64 small word elements match bytes" $ do
            sipHash @Word64 2 4 Test.Data.SipHash.key word16Samples @?= sipHash @Word64 2 4 Test.Data.SipHash.key (packWords 2 word16Samples)
            sipHash @Word64 2 4 Test.Data.SipHash.key word32Samples @?= sipHash @Word64 2 4 Test.Data.SipHash.key (packWords 4 word32Samples)
            sipHash @Word64 2 4 Test.Data.SipHash.key word64Samples @?= sipHash @Word64 2 4 Test.Data.SipHash.key (packWords 8 word64Samples)
        ],
      testGroup
        "HalfSipHash"
        [ testCase "HalfSipHash-2-4 Word32 String" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @String) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 Text" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @Text) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 ByteString" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BS.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 Lazy ByteString" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSL.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 ByteString Char8" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSC.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 Lazy ByteString Char8" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSLC.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord32,
          testCase "HalfSipHash-2-4 Word64 String" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @String) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word64 Text" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @Text) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word64 ByteString" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BS.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word64 Lazy ByteString" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSL.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word64 ByteString Char8" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSC.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word64 Lazy ByteString Char8" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSLC.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64,
          testCase "HalfSipHash-2-4 Word32 boundary lengths" $ fmap (halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key) Test.Data.HalfSipHash.boundaryInputs @?= Test.Data.HalfSipHash.boundaryVectorsForWord32,
          testCase "HalfSipHash-2-4 Word32 Char encoding matches bytes" $ do
            let bytes = BS.pack (concatMap charBytes charEncodingInput)
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key charEncodingInput @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key bytes
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (T.pack charEncodingInput) @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key bytes,
          testCase "HalfSipHash-2-4 Word32 word elements match bytes" $ do
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key [word128Sample] @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (BS.pack (wordBytes 16 word128Sample))
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key [word256Sample] @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (BS.pack (wordBytes 32 word256Sample)),
          testCase "HalfSipHash-2-4 Word32 small word elements match bytes" $ do
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key word16Samples @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (packWords 2 word16Samples)
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key word32Samples @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (packWords 4 word32Samples)
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key word64Samples @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key (packWords 8 word64Samples)
        ]
    ]
