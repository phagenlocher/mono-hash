{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Criterion (Benchmark, bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import qualified Data.ByteString as BS
import Data.Hash.Mono.FNV (fnv1, fnv1a)
import Data.Hash.Mono.HalfSipHash (halfSipHash)
import Data.Hash.Mono.SipHash (sipHash)
import qualified Data.Text as T
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word32, Word64)

data Sizes a = Sizes
  { sizeSmall :: a,
    sizeMedium :: a,
    sizeLarge :: a,
    sizeHuge :: a
  }

mkSizes :: (Int -> a) -> Sizes a
mkSizes f =
  Sizes
    { sizeSmall = f 10,
      sizeMedium = f 1000,
      sizeLarge = f 100000,
      sizeHuge = f 10000000
    }

forceSizes :: (NFData a) => Sizes a -> IO (Sizes a)
forceSizes sizes@(Sizes small medium large huge) = do
  _ <- evaluate $ force (small, medium, large, huge)
  pure sizes

mkBench :: (a -> b) -> Sizes a -> [Benchmark]
mkBench fun (Sizes small medium large huge) =
  [ bench "small input" $ whnf fun small,
    bench "medium input" $ whnf fun medium,
    bench "large input" $ whnf fun large,
    bench "huge input" $ whnf fun huge
  ]

mkBenchNoHuge :: (a -> b) -> Sizes a -> [Benchmark]
mkBenchNoHuge fun (Sizes small medium large _) =
  [ bench "small input" $ whnf fun small,
    bench "medium input" $ whnf fun medium,
    bench "large input" $ whnf fun large
  ]

main :: IO ()
main = do
  strings <- forceSizes $ mkSizes (\n -> replicate n 'a')
  bytes <- forceSizes $ mkSizes (\n -> BS.replicate n 97)
  texts <- forceSizes $ mkSizes (\n -> T.replicate n (T.singleton 'a'))

  boundaryInputs <-
    evaluate $
      force
        [ (n, BS.replicate n 97)
          | n <- [254, 255, 256, 257, 258, 511, 512, 513]
        ]

  defaultMain
    [ bgroup "FNV1 Word32" $ mkBench (fnv1 @Word32) strings,
      bgroup "FNV1a Word32" $ mkBench (fnv1a @Word32) strings,
      bgroup "FNV1 Word64" $ mkBench (fnv1 @Word64) strings,
      bgroup "FNV1a Word64" $ mkBench (fnv1a @Word64) strings,
      bgroup "FNV1 Word128" $ mkBench (fnv1 @Word128) strings,
      bgroup "FNV1a Word128" $ mkBench (fnv1a @Word128) strings,
      bgroup "FNV1 Word256" $ mkBench (fnv1 @Word256) strings,
      bgroup "FNV1a Word256" $ mkBench (fnv1a @Word256) strings,
      bgroup "SipHash-2-4 Word64" $ mkBench (sipHash @Word64 2 4 0xc0ffee) strings,
      bgroup "SipHash-2-4 Word128" $ mkBench (sipHash @Word128 2 4 0xc0ffee) strings,
      bgroup "HalfSipHash-2-4 Word32" $ mkBench (halfSipHash @Word32 2 4 0xc0ffee) strings,
      bgroup "HalfSipHash-2-4 Word64" $ mkBench (halfSipHash @Word64 2 4 0xc0ffee) strings,
      bgroup "FNV1a Word64/ByteString" $ mkBenchNoHuge (fnv1a @Word64) bytes,
      bgroup "FNV1a Word64/Text" $ mkBenchNoHuge (fnv1a @Word64) texts,
      bgroup "SipHash-2-4 Word64/ByteString" $ mkBenchNoHuge (sipHash @Word64 2 4 0xc0ffee) bytes,
      bgroup "SipHash-2-4 Word64/Text" $ mkBenchNoHuge (sipHash @Word64 2 4 0xc0ffee) texts,
      bgroup "HalfSipHash-2-4 Word32/ByteString" $ mkBenchNoHuge (halfSipHash @Word32 2 4 0xc0ffee) bytes,
      bgroup "HalfSipHash-2-4 Word32/Text" $ mkBenchNoHuge (halfSipHash @Word32 2 4 0xc0ffee) texts,
      bgroup
        "SipHash-2-4 Word64 length boundary"
        [ bench (show n) $ whnf (sipHash @Word64 2 4 0xc0ffee) input
          | (n, input) <- boundaryInputs
        ]
    ]
