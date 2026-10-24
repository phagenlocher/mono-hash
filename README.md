# mono-hash

[![Hackage](https://img.shields.io/hackage/v/mono-hash.svg)](https://hackage.haskell.org/package/mono-hash)
[![Haskell-CI](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml/badge.svg?branch=main)](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml)

Hashing for monomorphic containers: FNV-1/FNV-1a, SipHash, and HalfSipHash for
any [`MonoFoldable`](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoFoldable),
including `ByteString`, lazy `ByteString`, `Text`, `String`, and lists of
fixed-width words.

## Motivation

This library allows to hash data in monomorphic containers irrespective of their structure. This is helpful when changing container types but keeping the exact same bytes in a structure like changing a `[Word8]` to a `ByteString` while using equal contents.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Hash.Mono.FNV (fnv1a)
import Data.Hash.Mono.HalfSipHash (halfSipHash)
import Data.Hash.Mono.SipHash (sipHash)
import Data.Text (Text)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32, Word64)

-- FNV-1a with a 64-bit result
fnv1a @Word64 ("hello" :: Text)

-- SipHash-2-4 with the reference 128-bit key and a 64-bit result
sipHash @Word64 2 4 (0x000102030405060708090a0b0c0d0e0f :: Word128) ("hello" :: Text)

-- HalfSipHash-2-4 with the reference 64-bit key and a 32-bit result
halfSipHash @Word32 2 4 (0x0001020304050607 :: Word64) ("hello" :: Text)
```

The result type is selected with a type application (or type inference of course):

| Function       | Result type                              |
| -------------- | ---------------------------------------- |
| `fnv1`/`fnv1a` | `Word32`, `Word64`, `Word128`, `Word256` |
| `sipHash`      | `Word64`, `Word128`                      |
| `halfSipHash`  | `Word32`, `Word64`                       |

## Encoding and byte order

Hashing is defined over the concatenation of the elements' byte encodings.
The encoding is not type-tagged, so `[0x01, 0x02 :: Word8]` and
`[0x0102 :: Word16]` hash the same bytes.

`Word8` and `Int8` contribute one byte. Wider integers contribute
their bit pattern most significant byte first; signed integers use two's
complement. `Char` contributes UTF-8; a lone surrogate, which `String` can
contain but UTF-8 cannot represent, uses the 3-byte
[WTF-8](https://wtf-8.codeberg.page) form. `String` and `Text` are
hashed character by character, while `ByteString` contributes its raw bytes.
The library provides instances for `Word8`, `Word16`, `Word32`, `Word64`,
`Word128`, `Word256`, `Int8`, `Int16`, `Int32`, `Int64`, and `Char`.

## Custom element types

Give the element type a `ByteFoldable` instance to hash containers of your
own type:

```haskell
import Data.Hash.Mono.FNV (fnv1a)
import Data.Hash.Mono.Internal.ByteFoldable (ByteFoldable (..))
import Data.Word (Word8, Word64)

data Rgb = Rgb Word8 Word8 Word8

instance ByteFoldable Rgb where
    foldlBytes f z (Rgb r g b) = f (f (f z r) g) b

-- Hashes [Rgb 0x12 0x34 0x56] as the bytes 0x12 0x34 0x56.
fnv1a @Word64 [Rgb 0x12 0x34 0x56]
```

The bytes an instance emits are the bytes hashed, so changing an instance
changes every hash of containers holding that element type.

## Formatting and linting

Sources are formatted with [fourmolu](https://github.com/fourmolu/fourmolu) and
linted with [hlint](https://github.com/ndmitchell/hlint). CI checks both, using
the versions pinned in `.github/workflows/ci.yml`, so run them before opening
a pull request:

```console
fourmolu --mode inplace $(git ls-files '*.hs')
hlint src test bench
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
