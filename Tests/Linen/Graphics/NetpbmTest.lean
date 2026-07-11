/-
  Tests for `Linen.Graphics.Netpbm` — parses each of the six PBM/PGM/PPM
  format variants from hand-built `ByteArray`s and checks the decoded
  headers, pixel data, and leftover-byte handling.
-/
import Linen.Graphics.Netpbm

open Graphics.Netpbm

private def bytes (l : List UInt8) : ByteArray := ByteArray.mk l.toArray

private def str (s : String) : List UInt8 := s.toUTF8.toList

/-- Unwraps a successful parse into `(images, rest)`, `none` on failure. -/
private def ok? (r : PpmParseResult) : Option (List PPM × Option ByteArray) :=
  match r with
  | .ok v => some v
  | .error _ => none

-- ── ASCII PBM (P1): 2×2, checkerboard ──

private def p1 : ByteArray := bytes (str "P1\n2 2\n1 0\n0 1\n")

#guard ok? (parsePPM p1) == some
  ([⟨⟨.P1, 2, 2⟩, .pbm #[⟨false⟩, ⟨true⟩, ⟨true⟩, ⟨false⟩]⟩], none)

-- ── ASCII PGM (P2): 2×1, maxval 255 ──

private def p2 : ByteArray := bytes (str "P2\n2 1\n255\n10 20\n")

#guard ok? (parsePPM p2) == some ([⟨⟨.P2, 2, 1⟩, .grey8 #[⟨10⟩, ⟨20⟩]⟩], none)

-- ── ASCII PPM (P3): 1×1 red pixel, with a comment in the header ──

private def p3 : ByteArray := bytes (str "P3\n# a comment\n1 1\n255\n255 0 0\n")

#guard ok? (parsePPM p3) == some ([⟨⟨.P3, 1, 1⟩, .rgb8 #[⟨255, 0, 0⟩]⟩], none)

-- ── Binary PBM (P4): 2×2, packed into 1 byte per row (MSB-first) ──

private def p4 : ByteArray := bytes (str "P4\n2 2\n" ++ [(0x80 : UInt8), 0x00])

#guard ok? (parsePPM p4) == some
  ([⟨⟨.P4, 2, 2⟩, .pbm #[⟨false⟩, ⟨true⟩, ⟨true⟩, ⟨true⟩]⟩], none)

-- ── Binary PGM (P5): 2×1, maxval 255 ──

private def p5 : ByteArray := bytes (str "P5\n2 1\n255\n" ++ [(7 : UInt8), 8])

#guard ok? (parsePPM p5) == some ([⟨⟨.P5, 2, 1⟩, .grey8 #[⟨7⟩, ⟨8⟩]⟩], none)

-- ── Binary PPM (P6): 1×1, maxval 255 ──

private def p6 : ByteArray := bytes (str "P6\n1 1\n255\n" ++ [(1 : UInt8), 2, 3])

#guard ok? (parsePPM p6) == some ([⟨⟨.P6, 1, 1⟩, .rgb8 #[⟨1, 2, 3⟩]⟩], none)

-- ── Multiple images concatenated in a single file (binary formats have an
--    exact byte length per image, so — unlike ASCII — concatenation is
--    unambiguous and supported) ──

private def multi : ByteArray :=
  bytes (str "P5\n1 1\n255\n" ++ [(7 : UInt8)] ++ str "P5\n1 1\n255\n" ++ [(9 : UInt8)])

#guard (ok? (parsePPM multi)).map (fun (imgs, rest) => (imgs.map (·.ppmData), rest))
  == some ([.grey8 #[⟨7⟩], .grey8 #[⟨9⟩]], none)

-- ── Leftover bytes after a well-formed binary image are reported, not
--    consumed (ASCII bodies instead absorb trailing junk, matching
--    attoparsec's "any junk after the raster is allowed" semantics) ──

private def withJunk : ByteArray := bytes (str "P5\n1 1\n255\n" ++ [(7 : UInt8), 42])

#guard (ok? (parsePPM withJunk)).map (fun (_, rest) => rest) == some (some (bytes [42]))

-- ── `pixelDataToIntList` / `pixelVectorToList` ──

#guard pixelDataToIntList (.grey8 #[⟨1⟩, ⟨2⟩]) == [1, 2]
#guard pixelDataToIntList (.rgb8 #[⟨1, 2, 3⟩]) == [1, 2, 3]
#guard pixelVectorToList #[⟨(1 : UInt8)⟩, ⟨2⟩] == [PgmPixel8.mk 1, PgmPixel8.mk 2]

-- ── Malformed input fails ──

#guard ok? (parsePPM (bytes (str "not a ppm"))) == none
