# mono-hash

[![Hackage](https://img.shields.io/hackage/v/mono-hash.svg)](https://hackage.haskell.org/package/mono-hash)
[![Haskell-CI](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml/badge.svg?branch=main)](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml)

Hashing for monomorphic containers: FNV-1/FNV-1a, SipHash, and HalfSipHash for
any [`MonoFoldable`](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoFoldable),
including `ByteString`, lazy `ByteString`, `Text`, `String`, and lists of
fixed-width words.

Input is folded over one element at a time in constant space. There is no
intermediate byte-string allocation: each element is encoded to its
big-endian bytes on the fly.

## Installation

```console
cabal install mono-hash
```

or add it to your `build-depends`:

```cabal
build-depends: mono-hash >=0.1 && <0.2
```

## Usage

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Hash.Mono.FNV (fnv1a)
import Data.Hash.Mono.HalfSipHash (halfSipHash)
import Data.Hash.Mono.SipHash (sipHash)
import Data.Text (Text)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32, Word64)

-- FNV-1a with a 64-bit result
fnv1a @Word64 ("hello" :: Text)

-- SipHash-2-4 with a 128-bit key and a 64-bit result
sipHash @Word64 2 4 (0x0706050403020100 :: Word128) ("hello" :: Text)

-- HalfSipHash-2-4 with a 64-bit key and a 32-bit result
halfSipHash @Word32 2 4 (0x0706050403020100 :: Word64) ("hello" :: Text)
```

The result width is selected with a type application:

| Function       | Result types                              |
| -------------- | ----------------------------------------- |
| `fnv1`/`fnv1a` | `Word32`, `Word64`, `Word128`, `Word256`  |
| `sipHash`      | `Word64`, `Word128`                       |
| `halfSipHash`  | `Word32`, `Word64`                        |

Hashable element types are `Word8`, `Word16`, `Word32`, `Word64`, `Word128`,
`Word256`, `Int8`, `Int16`, `Int32`, `Int64`, and `Char`. Signed integers are
encoded as their two's-complement bit pattern; `Char` is encoded as its code
point.

## Supported GHC versions

GHC 8.10.7 through 9.14.1.

## Formatting and linting

Sources are formatted with [fourmolu](https://github.com/fourmolu/fourmolu) (0.21.0.0)
and linted with [hlint](https://github.com/ndmitchell/hlint) (3.10). CI checks
both, so run them before opening a pull request:

```console
fourmolu --mode inplace $(git ls-files '*.hs')
hlint src test bench
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
