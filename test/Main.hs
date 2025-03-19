{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Hash.Mono.FNV (fnv1, fnv1a)
import Data.Hash.Mono.SipHash
import Data.Text (Text)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32, Word64)
import qualified FNV.TestData
import qualified SipHash.TestData
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Golden Samples"
    [ testGroup
        "FNV"
        [ testCase "FNV1  Word32 String" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @String) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 String" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @String) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 String" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @String) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 String" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @String) @?= FNV.TestData.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Text" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @Text) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Text" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @Text) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Text" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @Text) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Text" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @Text) @?= FNV.TestData.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 ByteString" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @BS.ByteString) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 ByteString" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @BS.ByteString) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 ByteString" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @BS.ByteString) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 ByteString" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @BS.ByteString) @?= FNV.TestData.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Lazy ByteString" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @BSL.ByteString) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Lazy ByteString" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @BSL.ByteString) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Lazy ByteString" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @BSL.ByteString) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Lazy ByteString" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @BSL.ByteString) @?= FNV.TestData.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 ByteString Char8" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @BSC.ByteString) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 ByteString Char8" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @BSC.ByteString) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 ByteString Char8" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @BSC.ByteString) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 ByteString Char8" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @BSC.ByteString) @?= FNV.TestData.fnv1aVectorsForWord64,
          testCase "FNV1  Word32 Lazy ByteString Char8" $ fmap (fnv1 @Word32) (FNV.TestData.inputs @BSLC.ByteString) @?= FNV.TestData.fnv1VectorsForWord32,
          testCase "FNV1a Word32 Lazy ByteString Char8" $ fmap (fnv1a @Word32) (FNV.TestData.inputs @BSLC.ByteString) @?= FNV.TestData.fnv1aVectorsForWord32,
          testCase "FNV1  Word64 Lazy ByteString Char8" $ fmap (fnv1 @Word64) (FNV.TestData.inputs @BSLC.ByteString) @?= FNV.TestData.fnv1VectorsForWord64,
          testCase "FNV1a Word64 Lazy ByteString Char8" $ fmap (fnv1a @Word64) (FNV.TestData.inputs @BSLC.ByteString) @?= FNV.TestData.fnv1aVectorsForWord64
        ],
      testGroup
        "SipHash"
        [ testCase "SipHash-2-4 Word64 String" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @String) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Text" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @Text) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 ByteString" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BS.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Lazy ByteString" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSL.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 ByteString Char8" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSC.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word64 Lazy ByteString Char8" $ fmap (sipHash @Word64 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSLC.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord64,
          testCase "SipHash-2-4 Word128 String" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @String) @?= SipHash.TestData.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Text" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @Text) @?= SipHash.TestData.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 ByteString" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BS.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Lazy ByteString" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSL.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 ByteString Char8" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSC.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord128,
          testCase "SipHash-2-4 Word128 Lazy ByteString Char8" $ fmap (sipHash @Word128 2 4 SipHash.TestData.key) (SipHash.TestData.inputs @BSLC.ByteString) @?= SipHash.TestData.sipHash24VectorsForWord128
        ]
    ]