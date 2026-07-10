import Linen.Data.PDF.Content.TexGlyphList
import Linen.Data.Text.Encoding

open Data.PDF.Content.TexGlyphList
open Data.Text.Encoding (encodeUtf8)

-- ── A handful of representative lookups ──

#guard texGlyphList[encodeUtf8 "Delta"]? == some '∆'
#guard texGlyphList[encodeUtf8 "Ifractur"]? == some 'ℑ'
#guard texGlyphList[encodeUtf8 "upslope"]? == some (Char.ofNat 0x29F8)

-- ── Several distinct names can map to the same ligature code point ──

#guard texGlyphList[encodeUtf8 "FFsmall"]? == texGlyphList[encodeUtf8 "FFIsmall"]?
#guard texGlyphList[encodeUtf8 "FFsmall"]? == some (Char.ofNat 0xF766)

-- ── An unknown glyph name has no entry ──

#guard texGlyphList[encodeUtf8 "notARealGlyphName"]? == none

-- ── The table has exactly the upstream entry count ──

#guard texGlyphList.size == 285
