{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Hash.Mono.FNV (fnv1, fnv1a)
import Data.Hash.Mono.HalfSipHash
import Data.Hash.Mono.SipHash
import Data.Text (Text)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32, Word64)
import qualified Test.Data.FNV
import qualified Test.Data.HalfSipHash
import qualified Test.Data.SipHash
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
          testCase "FNV1a Word64 Lazy ByteString Char8" $ fmap (fnv1a @Word64) (Test.Data.FNV.inputs @BSLC.ByteString) @?= Test.Data.FNV.fnv1aVectorsForWord64
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
          testCase "SipHash-2-4 Word128 Lazy ByteString Char8" $ fmap (sipHash @Word128 2 4 Test.Data.SipHash.key) (Test.Data.SipHash.inputs @BSLC.ByteString) @?= Test.Data.SipHash.sipHash24VectorsForWord128
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
          testCase "HalfSipHash-2-4 Word64 Lazy ByteString Char8" $ fmap (halfSipHash @Word64 2 4 Test.Data.HalfSipHash.key) (Test.Data.HalfSipHash.inputs @BSLC.ByteString) @?= Test.Data.HalfSipHash.halfSipHash24VectorsForWord64
        ]
    ]