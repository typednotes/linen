import Linen.Data.PDF.Content.UnicodeCMap
import Linen.Data.Text.Encoding

open Data.PDF.Content.UnicodeCMap
open Std.Internal.Parsec.ByteArray (Parser)

private def bs (s : String) : Data.ByteString := Data.Text.Encoding.encodeUtf8 s

private def runP (p : Parser α) (s : String) : Except String α :=
  Parser.run p (bs s).data

private def runOk [BEq α] (p : Parser α) (s : String) (expected : α) : Bool :=
  match runP p s with
  | .ok v => v == expected
  | .error _ => false

private def runFails (p : Parser α) (s : String) : Bool :=
  match runP p s with
  | .ok _ => false
  | .error _ => true

-- ── `toCode` ──

#guard toCode (bs "\x00\x41") == 0x41
#guard toCode (bs "\x01\x00") == 0x100

-- ── `decodeUtf16BE` ──

#guard decodeUtf16BE (Data.ByteString.pack [0x00, 0x41]) == "A"
-- A surrogate pair for U+1F600 (😀): D83D DE00.
#guard decodeUtf16BE (Data.ByteString.pack [0xD8, 0x3D, 0xDE, 0x00]) == "😀"

-- ── `parseHex` ──

#guard runOk parseHex "<0041>" (Data.ByteString.pack [0x00, 0x41])
-- Embedded spaces are stripped before decoding.
#guard runOk parseHex "<00 41>" (Data.ByteString.pack [0x00, 0x41])
-- An odd digit count fails.
#guard runFails parseHex "<041>"

-- ── `parseHexArray` ──

#guard runOk parseHexArray "[<0041><0042>]"
  #[Data.ByteString.pack [0x00, 0x41], Data.ByteString.pack [0x00, 0x42]]

-- ── `codeRangesParser` ──

#guard runOk codeRangesParser
    "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange"
    [(Data.ByteString.pack [0x00, 0x00], Data.ByteString.pack [0xFF, 0xFF])]

-- ── `charsParser` ──

#guard match runP charsParser "1 beginbfchar\n<0041> <0042>\nendbfchar" with
  | .ok m => m[0x41]? == some "B"
  | .error _ => false

-- ── `rangesParser`, single-hex-string form ──

#guard match runP rangesParser "1 beginbfrange\n<0041> <0043> <0062>\nendbfrange" with
  | .ok (rs, _) => rs == [(0x41, 0x43, 'b')]
  | .error _ => false

-- ── `rangesParser`, array form ──

#guard match runP rangesParser "1 beginbfrange\n<0041> <0042> [<0062><0063>]\nendbfrange" with
  | .ok (_, cs) => cs[0x41]? == some "b" && cs[0x42]? == some "c"
  | .error _ => false

-- ── `parseUnicodeCMap` end-to-end ──

private def sample : String :=
  "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n" ++
  "1 beginbfchar\n<0041> <0042>\nendbfchar\n" ++
  "1 beginbfrange\n<0043> <0045> <0061>\nendbfrange"

#guard match parseUnicodeCMap (bs sample) with
  | .error _ => false
  | .ok cmap =>
    cmap.codeRanges == [(Data.ByteString.pack [0x00, 0x00], Data.ByteString.pack [0xFF, 0xFF])]
    && cmap.chars[0x41]? == some "B"
    && cmap.ranges == [(0x43, 0x45, 'a')]

-- ── `unicodeCMapNextGlyph`/`unicodeCMapDecodeGlyph` ──

private def sampleCMap : UnicodeCMap :=
  match parseUnicodeCMap (bs sample) with
  | .ok cmap => cmap
  | .error _ => ⟨[], {}, []⟩

-- Codespace declares 2-byte codes, so the next glyph consumes 2 bytes.
#guard unicodeCMapNextGlyph sampleCMap (Data.ByteString.pack [0x00, 0x41, 0x00, 0x43])
  == some (0x41, Data.ByteString.pack [0x00, 0x43])

#guard unicodeCMapDecodeGlyph sampleCMap 0x41 == some "B"
-- 0x44 falls inside the `<0043> <0045> <0061>` range, offset by 1 from 'a'.
#guard unicodeCMapDecodeGlyph sampleCMap 0x44 == some "b"
#guard unicodeCMapDecodeGlyph sampleCMap 0x99 == none
