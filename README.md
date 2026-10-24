# mono-hash

[![Hackage](https://img.shields.io/hackage/v/mono-hash.svg)](https://hackage.haskell.org/package/mono-hash)
[![Haskell-CI](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml/badge.svg?branch=main)](https://github.com/phagenlocher/mono-hash/actions/workflows/haskell-ci.yml)

Hashing for monomorphic containers: FNV-1/FNV-1a, SipHash, and HalfSipHash for
any [`MonoFoldable`](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoFoldable),
including `ByteString`, lazy `ByteString`, `Text`, `String`, and lists of
fixed-width words.

Input is folded over one element at a time in constant space. There is no
intermediate byte-string allocation: fixed-width words are encoded to their
big-endian bytes, `Char` to UTF-8, and byte strings contribute their raw
bytes on the fly.

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

The result width is selected with a type application:

| Function       | Result types                              |
| -------------- | ----------------------------------------- |
| `fnv1`/`fnv1a` | `Word32`, `Word64`, `Word128`, `Word256`  |
| `sipHash`      | `Word64`, `Word128`                       |
| `halfSipHash`  | `Word32`, `Word64`                        |

Element types with a `ToWords` instance are `Word8`, `Word16`, `Word32`,
`Word64`, `Word128`, `Word256`, `Int8`, `Int16`, `Int32`, `Int64`, and
`Char`. Signed integers are encoded as their two's-complement bit pattern,
and multi-byte integers most significant byte first. `Char` is encoded as
UTF-8, so `String` and `Text` hash the same bytes as their UTF-8 encoding. A
lone surrogate `Char`, which `String` can contain, uses the 3-byte
[WTF-8](https://simonsapin.github.io/wtf-8/) form; `Text` replaces lone
surrogates with `U+FFFD`, so the two may differ for such input.

## Encoding and byte order

Hashing is defined over the byte stream obtained by concatenating each
element's encoding. The algorithms then consume those bytes exactly as the
reference implementations do.

**Input encoding.** `Word8` and `Int8` contribute one byte. Wider fixed-width
integers contribute their two's-complement bit pattern, most significant byte
first; this is a portable library convention, not part of the SipHash
specification. `Char` contributes UTF-8, and `ByteString` contributes its raw
bytes.

`Char` is UTF-8 because the previous fixed-width code-point encoding was not
prefix-free: distinct strings such as `U+0100` and `U+0001 U+0000` produced
the same input bytes. UTF-8 is prefix-free and matches implementations that
hash UTF-8. Lone surrogates, which `Char` can represent, use the three-byte
[WTF-8](https://simonsapin.github.io/wtf-8/) form so the encoding stays total
and injective; `Text` replaces them with `U+FFFD`, so a `String` and a `Text`
can differ for such input. ASCII and `ByteString` input are unaffected;
non-ASCII `String`/`Text` hashes now follow their UTF-8 encoding.

Because the encoding is not type-tagged, different element types can feed the
same byte stream (for example `[Word8 0x01, 0x02]` and `[Word16 0x0102]`), and
`String "ab"` matches the `ByteString` `"ab"`. That is intended: the hash is
defined on bytes.

**Keys.** SipHash and HalfSipHash keys are the reference key byte strings
written as integers: the big-endian bytes of the `Word128`/`Word64` key are the
16/8 reference key bytes. Internally each 64/32-bit half is read as a
little-endian word, matching the reference's `U8TO64_LE` and `U8TO32_LE`. For
example, the reference words `k0 = 0x0706050403020100` and
`k1 = 0x0f0e0d0c0b0a0908` correspond to the key
`0x000102030405060708090a0b0c0d0e0f`.

**Results.** The result is the reference implementation's output word, the
numeric value used by the reference C code, Rust's `siphasher`, and Go.
Serializing a result little-endian gives the reference digest bytes. For the
wider results, serialize the whole `Word128`/`Word64` value little-endian: the
first reference output word is the low half and the second is the high half.
`showHex` on a SipHash result prints the reversed digest, so serialize before
formatting.

For example, with key `0x000102030405060708090a0b0c0d0e0f` and the empty input,
`sipHash @Word64 2 4` returns `0x726fdb47dd0e0e31`. Its little-endian bytes
`31 0e 0e dd 47 db 6f 72` are the first entry of the reference `vectors_sip64`.

FNV results are plain integers and are not byte-swapped.

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
