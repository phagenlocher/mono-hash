{- | Reference SipHash-2-4 vectors.

The values are the reference implementation's output words, i.e. the
little-endian reading of the byte arrays in
<https://github.com/veorq/SipHash/blob/master/vectors.h>. The key bytes
are @00 01 .. 0f@.
-}
module Test.Data.SipHash where

import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.String (IsString, fromString)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word64)

key :: Word128
key = 0x000102030405060708090a0b0c0d0e0f

inputs :: (IsString a, Monoid a) => [a]
inputs = fmap (\n -> fromString $ fmap chr [0 .. n - 1]) [0 .. 64 - 1]

-- | Inputs around the 256-byte length-byte wrap (254-513 bytes).
boundaryInputs :: [BS.ByteString]
boundaryInputs = [BS.replicate n 97 | n <- [254, 255, 256, 257, 258, 511, 512, 513]]

{- | SipHash-2-4-64 reference output words for the empty input and the
prefixes of @00 01 ..@.
-}
sipHash24VectorsForWord64 :: [Word64]
sipHash24VectorsForWord64 =
    [ 0x726fdb47dd0e0e31
    , 0x74f839c593dc67fd
    , 0x0d6c8009d9a94f5a
    , 0x85676696d7fb7e2d
    , 0xcf2794e0277187b7
    , 0x18765564cd99a68d
    , 0xcbc9466e58fee3ce
    , 0xab0200f58b01d137
    , 0x93f5f5799a932462
    , 0x9e0082df0ba9e4b0
    , 0x7a5dbbc594ddb9f3
    , 0xf4b32f46226bada7
    , 0x751e8fbc860ee5fb
    , 0x14ea5627c0843d90
    , 0xf723ca908e7af2ee
    , 0xa129ca6149be45e5
    , 0x3f2acc7f57c29bdb
    , 0x699ae9f52cbe4794
    , 0x4bc1b3f0968dd39c
    , 0xbb6dc91da77961bd
    , 0xbed65cf21aa2ee98
    , 0xd0f2cbb02e3b67c7
    , 0x93536795e3a33e88
    , 0xa80c038ccd5ccec8
    , 0xb8ad50c6f649af94
    , 0xbce192de8a85b8ea
    , 0x17d835b85bbb15f3
    , 0x2f2e6163076bcfad
    , 0xde4daaaca71dc9a5
    , 0xa6a2506687956571
    , 0xad87a3535c49ef28
    , 0x32d892fad841c342
    , 0x7127512f72f27cce
    , 0xa7f32346f95978e3
    , 0x12e0b01abb051238
    , 0x15e034d40fa197ae
    , 0x314dffbe0815a3b4
    , 0x027990f029623981
    , 0xcadcd4e59ef40c4d
    , 0x9abfd8766a33735c
    , 0x0e3ea96b5304a7d0
    , 0xad0c42d6fc585992
    , 0x187306c89bc215a9
    , 0xd4a60abcf3792b95
    , 0xf935451de4f21df2
    , 0xa9538f0419755787
    , 0xdb9acddff56ca510
    , 0xd06c98cd5c0975eb
    , 0xe612a3cb9ecba951
    , 0xc766e62cfcadaf96
    , 0xee64435a9752fe72
    , 0xa192d576b245165a
    , 0x0a8787bf8ecb74b2
    , 0x81b3e73d20b49b6f
    , 0x7fa8220ba3b2ecea
    , 0x245731c13ca42499
    , 0xb78dbfaf3a8d83bd
    , 0xea1ad565322a1a0b
    , 0x60e61c23a3795013
    , 0x6606d7e446282b93
    , 0x6ca4ecb15c5f91e1
    , 0x9f626da15c9625f3
    , 0xe51b38608ef25f57
    , 0x958a324ceb064572
    ]

{- | SipHash-2-4-128 reference output words. Each result is the
little-endian reading of a 16-byte @vectors_sip128@ entry: the first
64-bit output word is the low half and the second is the high half.
-}
sipHash24VectorsForWord128 :: [Word128]
sipHash24VectorsForWord128 =
    [ 0x930255c71472f66de6a825ba047f81a3
    , 0x45fc229b1159763444af996bd8c187da
    , 0xe4ff0af6de8ba3fcc75da4a48d227781
    , 0x51ed8529b0b6335f4ea967520cb6709c
    , 0x7955cd7b7c6e0f7daf8f9c2dc16481f8
    , 0x27960e69077a5254886f778059876813
    , 0x5ea1d78f30a05e481386208b33caee14
    , 0x3982f01fa64ab8c053c1dbd8beebf1a1
    , 0xb49714f364e2830f61f55862baa9623b
    , 0xed716dbb028b7fc4abbad90a06994426
    , 0xbafbd0f3d34754c956691478c30d1100
    , 0x18dce5816fdcb4a277666b3868c55101
    , 0x25c13285f64d638258f35e9066b226d6
    , 0xf752b9c44f9329d0108bc0e947e26998
    , 0x024949e45f48c77e9cded766aceffc31
    , 0xd9c3cf970fec087e11a8b03399e99354
    , 0x77052385bf1533fdbb54b067caa4e26e
    , 0x4077e47ac466c05498b88d73e8063d47
    , 0x23f7aefe81a44d298548bf23e4e526a4
    , 0xb12e51528920d574b0fa65cf31770178
    , 0xeb3938e8a544933e7390223f83fc259e
    , 0x121d073ecd14228a215a52be5a498e56
    , 0xae0aff8e52109c469a6bd15245b5294a
    , 0x1c69bf9a9ae28ccfe0f5a9d5dd84d1c9
    , 0xad32618a178a2a88d850bd78ae79b42d
    , 0x6f8f8dcbeab951507b445e2d045fce8e
    , 0x661f147886e0ae7ee807c3b3b4530b9c
    , 0x94eb9e122febd3bfe4eaa669af48f2ab
    , 0xf4ae587302f335b9884b576816da6406
    , 0xb76a7c463cfdd40ce97d33bfc49d4baa
    , 0x87226d68d4d71a2bde6baf1f477f5cea
    , 0x353dc4524fde2317fcfa233218b03929
    , 0x68eb4665559d3e363efcea5eca56397c
    , 0xcfffa94e5f9db6b6321cf0467107c677
    , 0xde549b30f1f02509df7e84b86c98a637
    , 0xc88c3c922e1a2407f9a8a99de6f005a7
    , 0x11674f90ed769e1e4648c4291f7dc43d
    , 0x2b69d3c551473c0d1a0efce601bf620d
    , 0xb5e7be4b085efde49e667cca8b46038c
    , 0xd92bd2d0e5cc73449c2caf3bb95b8a52
    , 0xd83b91c6c80cae97ad5dc9951e306adf
    , 0xdbb6705e289135e7397f852c90891180
    , 0x5b0ccacc34ae5036bb31c2c96a3417e6
    , 0x89df5aecdc211840aa21b7ef3734d927
    , 0x4273cc66b1c9b1d8785e9ced9d7d2389
    , 0x4cb150a294fa8911657d5ebf91806d4a
    , 0x022949cf3d0efc3f89aee75560f9330e
    , 0x1b1563dc4bd8c88ed1190b722b431ce6
    , 0x169b2608a6559037cf82f749f5aee5f7
    , 0x03641a20adf237a84fa5b7d00f038d43
    , 0x3f4286f2270d7e24e304bf4feed390a5
    , 0x38f5f9ae7cd35cb1c493fe72a1c1e25f
    , 0x7c013a8bd03d13b26eb306bd5c32972c
    , 0x9ed32a009f65f09f94ca6b7a2214c892
    , 0x871d91d64108d5fb8c32d80b1150e8dc
    , 0xda832592b52be3481279dac78449f167
    , 0x362a1da96f16947ee94ed572cff23819
    , 0x8e6904163024620ffe49ed46961e4874
    , 0x1d8a3d58d0386400d8d6a998dea5fc57
    , 0x595357d9743676d4be1cdcef1cdeec9f
    , 0x40e772d8cb73ca6653f128eb000c04e3
    , 0x7a0f6793591ca9ccfe1d836a9a009776
    , 0xbd5947f0a447d505a067f52123545358
    , 0x7cbd3f979a063e504a83502f77d15051
    ]

{- | Reference SipHash-2-4-64 output words for 'boundaryInputs'. The
values use the same byte order as 'sipHash24VectorsForWord64'.
-}
boundaryVectorsForWord64 :: [Word64]
boundaryVectorsForWord64 =
    [ 0x0028c1ded1b851e5
    , 0xe987ca4ff9f8404d
    , 0x03413138de48340e
    , 0xa5d0d8d2d9378001
    , 0xde306f78d586b010
    , 0xb2b283a137aa8552
    , 0x9cb7322fb06c14a2
    , 0xbad61d3a1333d25b
    ]
