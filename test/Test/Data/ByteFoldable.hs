{-# LANGUAGE TypeApplications #-}

{- | Tests for the public 'ByteFoldable' extension point: an element type
defined outside the library must be hashable via its own instance, and must
produce exactly the hash of the bytes it emits.
-}
module Test.Data.ByteFoldable (tests) where

import qualified Data.ByteString as BS
import Data.Hash.Mono.FNV (fnv1a)
import Data.Hash.Mono.HalfSipHash (halfSipHash)
import Data.Hash.Mono.Internal.ByteFoldable (ByteFoldable (..))
import Data.Hash.Mono.SipHash (sipHash)
import Data.Word (Word32, Word64, Word8)
import qualified Test.Data.HalfSipHash
import qualified Test.Data.SipHash
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | A user-defined element with a two-byte encoding, high byte first.
data TwoBytes = TwoBytes !Word8 !Word8

instance ByteFoldable TwoBytes where
    foldlBytes f z (TwoBytes hi lo) = f (f z hi) lo

-- | The bytes the instance above emits.
sampleBytes :: BS.ByteString
sampleBytes = BS.pack [0x01, 0x02]

sample :: TwoBytes
sample = TwoBytes 0x01 0x02

samples :: [TwoBytes]
samples = [sample]

tests :: TestTree
tests =
    testGroup
        "ByteFoldable"
        [ testCase "custom instance matches raw bytes (FNV-1a)" $
            fnv1a @Word64 samples @?= fnv1a @Word64 sampleBytes
        , testCase "custom instance matches raw bytes (SipHash-2-4)" $
            sipHash @Word64 2 4 Test.Data.SipHash.key samples
                @?= sipHash @Word64 2 4 Test.Data.SipHash.key sampleBytes
        , testCase "custom instance matches raw bytes (HalfSipHash-2-4)" $
            halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key samples
                @?= halfSipHash @Word32 2 4 Test.Data.HalfSipHash.key sampleBytes
        ]
