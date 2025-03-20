{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion (Benchmark, bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import Data.Hash.Mono.FNV (fnv1, fnv1a)
import Data.Hash.Mono.HalfSipHash (halfSipHash)
import Data.Hash.Mono.SipHash (sipHash)
import Data.WideWord.Word128 (Word128)
import Data.WideWord.Word256 (Word256)
import Data.Word (Word32, Word64)

main :: IO ()
main = do
  (small :: String, medium :: String, large :: String, huge :: String) <-
    evaluate $
      force
        ( replicate 10 'a',
          replicate 1000 'a',
          replicate 100000 'a',
          replicate 10000000 'a'
        )

  let mkBench :: (String -> a) -> [Benchmark]
      mkBench fun =
        [ bench "small input" $ whnf fun small,
          bench "medium input" $ whnf fun medium,
          bench "large input" $ whnf fun large,
          bench "huge input" $ whnf fun huge
        ]

  defaultMain
    [ bgroup "FNV1 Word32" $ mkBench (fnv1 @Word32),
      bgroup "FNV1a Word32" $ mkBench (fnv1a @Word32),
      bgroup "FNV1 Word64" $ mkBench (fnv1 @Word64),
      bgroup "FNV1a Word64" $ mkBench (fnv1a @Word64),
      bgroup "FNV1 Word128" $ mkBench (fnv1 @Word128),
      bgroup "FNV1a Word128" $ mkBench (fnv1a @Word128),
      bgroup "FNV1 Word256" $ mkBench (fnv1 @Word256),
      bgroup "FNV1a Word256" $ mkBench (fnv1a @Word256),
      bgroup "SipHash-2-4 Word64" $ mkBench (sipHash @Word64 2 4 0xc0ffee),
      bgroup "SipHash-2-4 Word128" $ mkBench (sipHash @Word128 2 4 0xc0ffee),
      bgroup "HalfSipHash-2-4 Word32" $ mkBench (halfSipHash @Word32 2 4 0xc0ffee),
      bgroup "HalfSipHash-2-4 Word64" $ mkBench (halfSipHash @Word64 2 4 0xc0ffee)
    ]
