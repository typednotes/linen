/-
  Data.PDF.Content.FontDescriptor — font metrics other than glyph widths
  (PDF32000-1:2008 §9.8)

  Ports `Pdf.Content.FontDescriptor` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox,
  `content/lib/Pdf/Content/FontDescriptor.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/FontDescriptor.hs`),
  module 3 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  ## Design

  - `fdFontWeight`/numeric fields keep upstream's `Maybe`/`Int`/`Double`
    shapes as `Option`/`Int`/`Float`, per the usual substitutions. `fdFlags`
    is upstream's `Int64` (documented there as needing to "hold at least 32
    bit unsigned integers"); ported as `UInt32` directly, which states that
    requirement as a type rather than a comment, and lets `flagSet` below
    use `Nat.testBit`/`UInt32.toNat` instead of hand-rolled `div`/`mod`
    bit-peeling recursion.

  - Upstream's `flagSet'` recurses by repeatedly halving `val` and
    incrementing a bit-position counter until the counter reaches the
    target bit or the value is exhausted — genuine termination (`val`
    strictly decreases towards `0`), but it is *exactly* what "is bit `pos`
    of `val` set" means, and Lean's standard library already provides that
    check directly as `Nat.testBit`. Using it is a faithful semantic port,
    not a weakening: the same bit is tested, no case is handled
    differently, and it sidesteps writing a termination proof for logic the
    stdlib already proves total.

  - The two upstream `-- FIXME` comments (`FontFile*` fields, CIDFont-specific
    fields) mark functionality upstream itself never implemented; this port
    carries the same gap forward rather than inventing fields upstream
    doesn't have.
-/
import Linen.Data.PDF.Core.Types
import Linen.Data.ByteString

namespace Data.PDF.Content.FontDescriptor

open Data.PDF.Core.Types (Rectangle)

/-! ── The `FontDescriptor` type ── -/

/-- A font descriptor: font-wide metrics other than a font's glyph widths
    (PDF32000-1:2008 §9.8, Table 122). Mirrors upstream's `FontDescriptor`
    record. -/
structure FontDescriptor where
  /-- The font's PostScript name. -/
  fontName : Data.ByteString
  /-- The font's family name, if specified. -/
  fontFamily : Option Data.ByteString
  /-- The font's stretch (width class), if specified. -/
  fontStretch : Option Data.ByteString
  /-- The font's weight, if specified. -/
  fontWeight : Option Int
  /-- Font flags (PDF32000-1:2008 §9.8.2, Table 123) — must hold at least a
      32-bit unsigned integer, hence `UInt32` rather than a bare `Nat`. -/
  flags : UInt32
  /-- The font's bounding box, if specified. -/
  fontBBox : Option (Rectangle Float)
  /-- The angle, in degrees counterclockwise from vertical, of the
      dominant vertical strokes of the font. -/
  italicAngle : Float
  /-- The maximum height above the baseline reached by glyphs, if specified. -/
  ascent : Option Float
  /-- The maximum depth below the baseline reached by glyphs, if specified. -/
  descent : Option Float
  /-- The spacing between baselines of consecutive lines of text, if specified. -/
  leading : Option Float
  /-- The height of flat capital letters above the baseline, if specified. -/
  capHeight : Option Float
  /-- The font's x-height, if specified. -/
  xHeight : Option Float
  /-- The thickness of dominant vertical stems, if specified. -/
  stemV : Option Float
  /-- The thickness of dominant horizontal stems, if specified. -/
  stemH : Option Float
  /-- The average width of glyphs in the font, if specified. -/
  avgWidth : Option Float
  /-- The maximum width of glyphs in the font, if specified. -/
  maxWidth : Option Float
  /-- The width to use for glyphs missing from the font's widths array, if
      specified. -/
  missingWidth : Option Float
  -- FIXME: add FontFile* (upstream doesn't implement these either)
  /-- The font's character set, if specified. -/
  charSet : Option Data.ByteString
  -- FIXME: add special fields for CIDFonts (upstream doesn't implement these either)
deriving Repr

/-! ── Flags ── -/

/-- A single font-descriptor flag bit (PDF32000-1:2008 §9.8.2, Table 123),
    named by its meaning rather than its bit position. -/
inductive FontDescriptorFlag where
  /-- All glyphs have the same width (bit 1). -/
  | fixedPitch
  /-- Glyphs have serifs (bit 2). -/
  | serif
  /-- The font contains glyphs outside the Adobe standard Latin character
      set (bit 3). -/
  | symbolic
  /-- Glyphs resemble cursive handwriting (bit 4). -/
  | script
  /-- The font uses the Adobe standard Latin character set (bit 6). -/
  | nonSymbolic
  /-- Glyphs are italic, slanted left for left-to-right writing (bit 7). -/
  | italic
  /-- The font contains no lowercase letters (bit 17). -/
  | allCap
  /-- The font contains both uppercase and lowercase, with lowercase drawn
      as small caps (bit 18). -/
  | smallCap
  /-- Bold glyphs should be painted even absent a bold-specific font
      (bit 19). -/
  | forceBold
deriving BEq, Repr

/-- The (1-based) bit position a `FontDescriptorFlag` occupies in
    `FontDescriptor.flags` (PDF32000-1:2008 §9.8.2, Table 123). -/
def FontDescriptorFlag.bitPosition : FontDescriptorFlag → Nat
  | .fixedPitch => 1
  | .serif => 2
  | .symbolic => 3
  | .script => 4
  | .nonSymbolic => 6
  | .italic => 7
  | .allCap => 17
  | .smallCap => 18
  | .forceBold => 19

/-- Is a given flag bit set in a font descriptor's `flags`? Mirrors
    upstream's `flagSet`/`flagSet'`, but tests the bit directly via
    `Nat.testBit` (see the module doc-comment) instead of hand-rolled
    `div`/`mod` recursion. -/
def flagSet (fd : FontDescriptor) (flag : FontDescriptorFlag) : Bool :=
  Nat.testBit fd.flags.toNat (flag.bitPosition - 1)

end Data.PDF.Content.FontDescriptor
