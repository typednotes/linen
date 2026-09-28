/-
  Tests for `Linen.Data.ByteString.Builder` — the difference-list byte builder.

  Builders are run with `toStrictByteString` and compared via `.unpack`.
-/
import Linen.Data.ByteString.Builder

open Data
open Data.ByteString

namespace Tests.Data.ByteString.Builder

/-! ### core / primitives -/

#guard Builder.empty.toStrictByteString.unpack == ([] : List UInt8)
#guard (Builder.singleton 65).toStrictByteString.unpack == [65]
#guard ((Builder.singleton 65) ++ (Builder.singleton 66)).toStrictByteString.unpack == [65, 66]
#guard (Builder.byteString (ByteString.pack [1, 2, 3])).toStrictByteString.unpack == [1, 2, 3]
#guard (Builder.byteString ByteString.empty).toStrictByteString.unpack == ([] : List UInt8)
#guard (Builder.shortByteString (ShortByteString.pack [7, 8])).toStrictByteString.unpack == [7, 8]
#guard (Builder.singleton 65).toLazyByteString.unpack == [65]

/-! ### word encodings (big- vs little-endian) -/

#guard (Builder.word8 200).toStrictByteString.unpack == [200]
#guard (Builder.word16BE 0x0102).toStrictByteString.unpack == [1, 2]
#guard (Builder.word16LE 0x0102).toStrictByteString.unpack == [2, 1]
#guard (Builder.word32BE 0x01020304).toStrictByteString.unpack == [1, 2, 3, 4]
#guard (Builder.word32LE 0x01020304).toStrictByteString.unpack == [4, 3, 2, 1]
#guard (Builder.word64BE 0x0102030405060708).toStrictByteString.unpack == [1, 2, 3, 4, 5, 6, 7, 8]
#guard (Builder.word64LE 0x0102030405060708).toStrictByteString.unpack == [8, 7, 6, 5, 4, 3, 2, 1]

/-! ### text encodings -/

#guard (Builder.char8 'A').toStrictByteString.unpack == [65]
#guard (Builder.charUtf8 'A').toStrictByteString.unpack == [65]
#guard (Builder.charUtf8 'é').toStrictByteString.unpack == [195, 169]      -- U+00E9 in UTF-8
#guard (Builder.stringUtf8 "AB").toStrictByteString.unpack == [65, 66]
#guard (Builder.stringUtf8 "café").toStrictByteString.unpack == [99, 97, 102, 195, 169]

/-! ### decimal / hex -/

#guard (Builder.intDec 42).toStrictByteString.unpack == [52, 50]          -- "42"
#guard (Builder.intDec (-5)).toStrictByteString.unpack == [45, 53]         -- "-5"
#guard (Builder.wordHex 0).toStrictByteString.unpack == [48]               -- "0"
#guard (Builder.wordHex 255).toStrictByteString.unpack == [102, 102]       -- "ff"
#guard (Builder.wordHex 16).toStrictByteString.unpack == [49, 48]          -- "10"

/-! ### composition + ToString -/

#guard ((Builder.stringUtf8 "x=") ++ (Builder.intDec 42)).toStrictByteString.unpack == [120, 61, 52, 50]
#guard toString (Builder.stringUtf8 "AB") == "[65, 66]"

/-! ### monoid laws (compile-time) -/

example (b : Builder) : Builder.empty ++ b = b := Builder.empty_append b
example (b : Builder) : b ++ Builder.empty = b := Builder.append_empty b
example (a b c : Builder) : (a ++ b) ++ c = a ++ (b ++ c) := Builder.append_assoc a b c

end Tests.Data.ByteString.Builder
