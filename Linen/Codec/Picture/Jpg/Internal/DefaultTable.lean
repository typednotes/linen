/-!
  Port of `Codec.Picture.Jpg.Internal.DefaultTable` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 21 of 29).

  This module is pure data: the standard JPEG luminance/chrominance
  quantization tables and the standard DC/AC Huffman code-length tables
  (and the Huffman trees they build into), exactly as specified by ITU-T.81
  Annex K. Per the dependency plan, module 21 is "data only, no other module
  dependency" — it depends on nothing beyond the Lean stdlib.

  ## Design and scope

  - `HuffmanTree` (`Branch`/`Leaf`/`Empty` upstream) is ported as the
    structurally-recursive inductive `HuffmanTree` (`branch`/`leaf`/`empty`).
  - A raw `HuffmanTable` (as parsed from/written to a JPEG `DHT` segment) is
    upstream's `[[Word8]]`: index `i` (0-based, code length `i + 1`) holds
    the symbols whose canonical Huffman code has that bit length. This is
    ported as `List (List UInt8)`.
  - `buildHuffmanTree` is ported faithfully, including its
    `insertHuffmanVal` helper, which upstream partially defines (its
    `Leaf`/depth-`0`-into-`Branch` cases call `error`, since a well-formed
    code-length table never reaches them). Since `partial`/`sorry`/`error`
    are unavailable here, those unreachable cases are total no-ops (returning
    the tree unchanged) instead — for the four default tables transcribed
    below (and any other well-formed code-length table), those branches are
    never taken, so this is behaviour-preserving, not a behaviour change.
    Termination is structural in the depth `Nat` argument, which strictly
    decreases on every recursive call.
  - Upstream's `QuantificationTable`/`MacroBlock a` is `SV.Vector a`
    (specialised to `Int16` for quantization tables, to leave headroom for
    signed DCT coefficients elsewhere in the codec). This module only needs
    to hold the JPEG-standard literal quantization bytes, all of which lie in
    `1..255`, so `MacroBlock`/`QuantificationTable` are ported as
    `Array UInt8` here; a later module that needs the wider signed range for
    scaled/negative coefficients can convert via `UInt8.toNat`/`Int.ofNat` as
    needed.
  - `makeMacroBlock` (upstream `SV.fromListN 64`) is ported as a plain
    `List.toArray`; the `64` is documentation of intended length upstream,
    not a runtime-checked invariant, so nothing is lost by dropping it (the
    two call sites below are both literal 64-element lists, checked by this
    module's tests).
  - `scaleQuantisationMatrix`, `packHuffmanTree`, `makeInverseTable`, and
    `huffmanPackedDecode` are **not** ported here. `scaleQuantisationMatrix`
    (quality-based table scaling) and `packHuffmanTree`/`makeInverseTable`
    (encoder-side table flattening) are genuine JPEG codec machinery, not
    "default table" data; `huffmanPackedDecode` additionally depends on
    `Codec.Picture.BitWriter`'s `BoolReader` monad, which would violate this
    module's "no other module dependency" placement in the import order.
    These belong with the modules that actually decode/encode JPEG scans
    (`Linen.Codec.Picture.Jpg.Internal.Types`/`Common`/`Jpg`, modules 22–27),
    which already depend on `BitWriter`.
-/

namespace Codec.Picture.Jpg.Internal

-- ── Huffman tree ──

/-- Tree storing the code used for Huffman encoding/decoding of one JPEG
    coefficient: at a `branch`, a `0` bit takes the left subtree and a `1`
    bit the right; a `leaf` is the decoded value; `empty` marks the absence
    of a code at that position. -/
inductive HuffmanTree where
  | branch (left right : HuffmanTree)
  | leaf (value : UInt8)
  | empty
  deriving Repr, DecidableEq

/-- A raw JPEG Huffman table, as parsed from (or written to) a `DHT`
    segment: index `i` (code length `i + 1`) holds the symbols whose
    canonical Huffman code has that bit length. -/
abbrev HuffmanTable := List (List UInt8)

-- ── Macroblocks and quantization tables ──

/-- A compact array of `8 * 8 = 64` values, as used for one JPEG
    coefficient macroblock. Size is a documentation convention here (as
    upstream), not a runtime-checked invariant. -/
abbrev MacroBlock (α : Type) := Array α

/-- A 64-entry JPEG quantization table. -/
abbrev QuantificationTable := MacroBlock UInt8

/-- Helper to build a macroblock from a literal list of values. -/
def makeMacroBlock (xs : List α) : MacroBlock α := xs.toArray

-- ── DCT component tag ──

/-- Which of the two coefficient categories a Huffman table encodes:
    the DC (predicted) coefficient, or the 63 AC coefficients. -/
inductive DctComponent where
  | dcComponent
  | acComponent
  deriving Repr, DecidableEq

-- ── Building a `HuffmanTree` from a `HuffmanTable` ──

/-- Is every leaf of this (sub)tree already assigned a value? -/
def isTreeFullyDefined : HuffmanTree → Bool
  | .empty => false
  | .leaf _ => true
  | .branch l r => isTreeFullyDefined l && isTreeFullyDefined r

/-- Insert a value at Huffman-code depth `d` (i.e. `d` bits from the root)
    into a `HuffmanTree`, growing branches as needed. The two "shouldn't
    happen" cases (matching upstream's `error` calls) are unreachable for
    any well-formed code-length table — such as the four default tables
    built below — and are total no-ops here instead of a partial `error`. -/
def insertHuffmanVal : HuffmanTree → Nat → UInt8 → HuffmanTree
  | .empty, 0, val => .leaf val
  | .empty, d + 1, val => .branch (insertHuffmanVal .empty d val) .empty
  | .branch l r, d + 1, val =>
      if isTreeFullyDefined l then
        .branch l (insertHuffmanVal r d val)
      else
        .branch (insertHuffmanVal l d val) r
  | .leaf v, _, _ => .leaf v
  | .branch l r, 0, _ => .branch l r

/-- Transform parsed Huffman code-length groups (from a JPEG header) into
    the tree used to decode data. -/
def buildHuffmanTree (table : HuffmanTable) : HuffmanTree :=
  let pairs : List (Nat × UInt8) :=
    ((List.range table.length).zip table).flatMap
      (fun (i, group) => group.map (fun val => (i + 1, val)))
  pairs.foldl (fun tree (d, val) => insertHuffmanVal tree d val) .empty

-- ── Default quantization tables ──

/-- Default luminance quantization table, as specified by the JPEG
    standard. -/
def defaultLumaQuantizationTable : QuantificationTable :=
  makeMacroBlock
    [16, 11, 10, 16,  24,  40,  51,  61,
     12, 12, 14, 19,  26,  58,  60,  55,
     14, 13, 16, 24,  40,  57,  69,  56,
     14, 17, 22, 29,  51,  87,  80,  62,
     18, 22, 37, 56,  68, 109, 103,  77,
     24, 35, 55, 64,  81, 104, 113,  92,
     49, 64, 78, 87, 103, 121, 120, 101,
     72, 92, 95, 98, 112, 100, 103,  99]

/-- Default chrominance quantization table, as specified by the JPEG
    standard. -/
def defaultChromaQuantizationTable : QuantificationTable :=
  makeMacroBlock
    [17, 18, 24, 47, 99, 99, 99, 99,
     18, 21, 26, 66, 99, 99, 99, 99,
     24, 26, 56, 99, 99, 99, 99, 99,
     47, 66, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99]

-- ── Default Huffman tables ──

/-- From Table K.3 of ITU-T.81 (p. 153): the default DC luminance
    Huffman code-length table. -/
def defaultDcLumaHuffmanTable : HuffmanTable :=
  [ []
  , [0]
  , [1, 2, 3, 4, 5]
  , [6]
  , [7]
  , [8]
  , [9]
  , [10]
  , [11]
  , []
  , []
  , []
  , []
  , []
  , []
  , []
  ]

/-- The default DC luminance Huffman tree, built from
    `defaultDcLumaHuffmanTable`. -/
def defaultDcLumaHuffmanTree : HuffmanTree :=
  buildHuffmanTree defaultDcLumaHuffmanTable

/-- From Table K.4 of ITU-T.81 (p. 153): the default DC chrominance
    Huffman code-length table. -/
def defaultDcChromaHuffmanTable : HuffmanTable :=
  [ []
  , [0, 1, 2]
  , [3]
  , [4]
  , [5]
  , [6]
  , [7]
  , [8]
  , [9]
  , [10]
  , [11]
  , []
  , []
  , []
  , []
  , []
  ]

/-- The default DC chrominance Huffman tree, built from
    `defaultDcChromaHuffmanTable`. -/
def defaultDcChromaHuffmanTree : HuffmanTree :=
  buildHuffmanTree defaultDcChromaHuffmanTable

/-- From Table K.5 of ITU-T.81 (p. 154): the default AC luminance
    Huffman code-length table. -/
def defaultAcLumaHuffmanTable : HuffmanTable :=
  [ []
  , [0x01, 0x02]
  , [0x03]
  , [0x00, 0x04, 0x11]
  , [0x05, 0x12, 0x21]
  , [0x31, 0x41]
  , [0x06, 0x13, 0x51, 0x61]
  , [0x07, 0x22, 0x71]
  , [0x14, 0x32, 0x81, 0x91, 0xA1]
  , [0x08, 0x23, 0x42, 0xB1, 0xC1]
  , [0x15, 0x52, 0xD1, 0xF0]
  , [0x24, 0x33, 0x62, 0x72]
  , []
  , []
  , [0x82]
  , [ 0x09, 0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x34, 0x35,
      0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54,
      0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73,
      0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A,
      0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
      0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4,
      0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
      0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5,
      0xF6, 0xF7, 0xF8, 0xF9, 0xFA]
  ]

/-- The default AC luminance Huffman tree, built from
    `defaultAcLumaHuffmanTable`. -/
def defaultAcLumaHuffmanTree : HuffmanTree :=
  buildHuffmanTree defaultAcLumaHuffmanTable

/-- From Table K.6 of ITU-T.81 (p. 155): the default AC chrominance
    Huffman code-length table. -/
def defaultAcChromaHuffmanTable : HuffmanTable :=
  [ []
  , [0x00, 0x01]
  , [0x02]
  , [0x03, 0x11]
  , [0x04, 0x05, 0x21, 0x31]
  , [0x06, 0x12, 0x41, 0x51]
  , [0x07, 0x61, 0x71]
  , [0x13, 0x22, 0x32, 0x81]
  , [0x08, 0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1]
  , [0x09, 0x23, 0x33, 0x52, 0xF0]
  , [0x15, 0x62, 0x72, 0xD1]
  , [0x0A, 0x16, 0x24, 0x34]
  , []
  , [0xE1]
  , [0x25, 0xF1]
  , [ 0x17, 0x18, 0x19, 0x1A, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x35,
      0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47,
      0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
      0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73,
      0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83, 0x84,
      0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95,
      0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,
      0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
      0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8,
      0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9,
      0xDA, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA,
      0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA]
  ]

/-- The default AC chrominance Huffman tree, built from
    `defaultAcChromaHuffmanTable`. -/
def defaultAcChromaHuffmanTree : HuffmanTree :=
  buildHuffmanTree defaultAcChromaHuffmanTable

end Codec.Picture.Jpg.Internal
