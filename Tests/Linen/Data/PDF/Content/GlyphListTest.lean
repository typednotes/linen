import Linen.Data.PDF.Content.GlyphList
import Linen.Data.Text.Encoding

open Data.PDF.Content.GlyphList
open Data.Text.Encoding (encodeUtf8)

-- ── A handful of representative lookups, spread across the table ──

#guard adobeGlyphList[encodeUtf8 "A"]? == some 'A'
#guard adobeGlyphList[encodeUtf8 "AE"]? == some 'Æ'
#guard adobeGlyphList[encodeUtf8 "Aacute"]? == some 'Á'
#guard adobeGlyphList[encodeUtf8 "zuhiragana"]? == some 'ず'
#guard adobeGlyphList[encodeUtf8 "zukatakana"]? == some 'ズ'

-- ── Private-use-area glyph names (e.g. "small caps" variants) round-trip too ──

#guard adobeGlyphList[encodeUtf8 "AEsmall"]? == some (Char.ofNat 0xF7E6)

-- ── An unknown glyph name has no entry ──

#guard adobeGlyphList[encodeUtf8 "notARealGlyphName"]? == none

-- ── The table has exactly the upstream entry count ──

#guard adobeGlyphList.size == 4281
