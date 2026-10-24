{- | Reference HalfSipHash-2-4 vectors.

The values are the reference implementation's output words, i.e. the
little-endian reading of the byte arrays in
<https://github.com/veorq/SipHash/blob/master/vectors.h>. The key bytes
are @00 01 .. 07@.
-}
module Test.Data.HalfSipHash where

import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.String (IsString, fromString)
import Data.Word (Word32, Word64)

key :: Word64
key = 0x0001020304050607

inputs :: (IsString a, Monoid a) => [a]
inputs = fmap (\n -> fromString $ fmap chr [0 .. n - 1]) [0 .. 64 - 1]

-- | Inputs around the 256-byte length-byte wrap (254-513 bytes).
boundaryInputs :: [BS.ByteString]
boundaryInputs = [BS.replicate n 97 | n <- [254, 255, 256, 257, 258, 511, 512, 513]]

{- | HalfSipHash-2-4-32 reference output words for the empty input and
the prefixes of @00 01 ..@.
-}
halfSipHash24VectorsForWord32 :: [Word32]
halfSipHash24VectorsForWord32 =
    [ 0x5b9f35a9
    , 0xb85a4727
    , 0x03a662fa
    , 0x04e7fe8a
    , 0x89466e2a
    , 0x69b6fac5
    , 0x23fc6358
    , 0xc563cf8b
    , 0x8f84b8d0
    , 0x79e706f8
    , 0x3479b094
    , 0x50300808
    , 0x2f87f057
    , 0xff63e677
    , 0x7cf8ffd6
    , 0x972bfe74
    , 0x84acb5d9
    , 0x5b6474c4
    , 0x9b8d5b46
    , 0x87e3ef7b
    , 0x45104de3
    , 0xb3623f61
    , 0xfe67f370
    , 0xbdb8ade6
    , 0x630c4027
    , 0x75787826
    , 0x5f7b564f
    , 0x69e6b03a
    , 0x004064b0
    , 0xb40f67ff
    , 0x8b339e50
    , 0x1a9f585d
    , 0x1221e7fe
    , 0x59327533
    , 0x8c4f436a
    , 0x29b728fe
    , 0xecc65ce7
    , 0x548d7e69
    , 0x0f8b6863
    , 0xb4620b65
    , 0x4018bcb6
    , 0x0545075d
    , 0x2efd4224
    , 0x3a86b77b
    , 0x48d50577
    , 0xb10852d7
    , 0xc899d4b6
    , 0x2e209208
    , 0xe32ce169
    , 0xe580b58d
    , 0xc6649736
    , 0x04026e01
    , 0xd4f3853b
    , 0xbe66dbfe
    , 0x3a2a691e
    , 0xc08489c6
    , 0x40b9c5a5
    , 0x8ce8e99b
    , 0x4081bc7d
    , 0xc58e077c
    , 0x736ce7d4
    , 0xb9cb8f42
    , 0x7a9983bd
    , 0x744aea59
    ]

{- | HalfSipHash-2-4-64 reference output words. Each result is the
little-endian reading of an 8-byte @vectors_hsip64@ entry: the first
32-bit output word is the low half and the second is the high half.
-}
halfSipHash24VectorsForWord64 :: [Word64]
halfSipHash24VectorsForWord64 =
    [ 0xc83cb8b9591f8d21
    , 0x157338f8122455be
    , 0x57eb507cef394f06
    , 0x790606f7451a0fce
    , 0xa12ee55b178ae7d5
    , 0x80b53d2f3f7c9dcb
    , 0x25bca28a35913ece
    , 0x84c67bb0282720ff
    , 0x8c85e4bc20e8feed
    , 0x07838813cccc515b
    , 0xeef2a6069f46b095
    , 0x48cddd94393326ae
    , 0x99c7f5ae9f1fc77b
    , 0x44370c5ad752235a
    , 0x58e6e8ea70a8b13b
    , 0x02c9814ecb0b7d21
    , 0xb5f37b5fd2aa3673
    , 0x6a4f4c1c64c0ad37
    , 0xf9423e9a2bdbb2c9
    , 0x3c36ab2080e410f9
    , 0xdba7ee6f0a2bf51b
    , 0xefb3e869c21d7400
    , 0xef76a71bfa0301e2
    , 0x731d684be510224c
    , 0xf1a63fae45107470
    , 0x384071393740860c
    , 0xf0232911d89e890d
    , 0xb8e11eb8faf56b22
    , 0xb516001efb5f922d
    , 0xf110ee2cd5581936
    , 0x9d17984886af1a29
    , 0x7c11345c157f3c86
    , 0x6c6211d8469d7028
    , 0x9cf8281d68778424
    , 0x30988f52d7e42483
    , 0xd86bea3ae1d4eff9
    , 0xdc7642ec407ad686
    , 0x357ea9ccec92623f
    , 0x0921d424e72ed9cb
    , 0x793d408d80f68d36
    , 0x4caec8671cc8385b
    , 0xb3ac39d48971ab95
    , 0x24703225c0521aa9
    , 0xeaac2895c687005b
    , 0x5ab1dc27adf3301e
    , 0xd44e32909a5c7f69
    , 0x38dc5755990f5c49
    , 0x4df9293c2a202794
    , 0x3e3ea94bc0a8eaa9
    , 0x1812017d73c1a4ee
    , 0x495af6d88f562d91
    , 0x975cffb096959156
    , 0xe150f598795a4402
    , 0xb21f1de76c46ec86
    , 0xbce389d2e7699535
    , 0x967cbb62ca051b87
    , 0x1d5ff142f992a4a1
    , 0x6e5b09f67f26ec12
    , 0x9dd831b2a15e1b5d
    , 0x54ee923f45b4cfd8
    , 0x60e426bf902876d6
    , 0xf35cedb7a4633531
    , 0x9366d472b53a0bf9
    , 0x876032bf713ca62e
    ]

{- | Reference HalfSipHash-2-4-32 output words for 'boundaryInputs'.
The values use the same byte order as
'halfSipHash24VectorsForWord32'.
-}
boundaryVectorsForWord32 :: [Word32]
boundaryVectorsForWord32 =
    [ 0x8bafb24d
    , 0x80debc4f
    , 0xccc21763
    , 0x74aae63c
    , 0xc110387a
    , 0x8d238996
    , 0xea25986f
    , 0x79711faa
    ]
