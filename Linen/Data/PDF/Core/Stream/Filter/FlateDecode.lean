/-
  Data.PDF.Core.Stream.Filter.FlateDecode — the `FlateDecode` filter

  Ports `Pdf.Core.Stream.Filter.FlateDecode` from Hackage's
  `pdf-toolbox-core`'s zlib-enabled variant
  (https://github.com/Yuras/pdf-toolbox,
  `core/zlib/Pdf/Core/Stream/Filter/FlateDecode.hs` — note the real,
  zlib-backed implementation lives under `core/zlib/`, not the `core/lib/`
  path shared with the no-op `core/no-zlib/` stub), module 12 of the
  `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  `FlateDecode` (PDF32000-1:2008 §7.4.4) inflates a zlib/RFC 1950-compressed
  stream, optionally followed by a PNG- or TIFF-style predictor reversal
  (§7.4.4.4) named by the `/DecodeParms` dictionary's `/Predictor` entry.
  Only predictor `1` (none) and `12` (PNG "Up") are handled — matching
  upstream exactly (its own comment: "Only PNG-UP prediction is
  implemented"); every other predictor value (including the other four PNG
  sub-filter codes 10/11/13/14, which a *decoder* can't distinguish upfront
  since a real PNG predictor chooses its sub-filter per row, and the TIFF
  predictor 2) is rejected the same way upstream rejects it: an
  `Unexpected` error naming the unsupported value, not a silent no-op.

  ## Design

  Decompression itself delegates to `Data.PDF.Stream.decompress`, the same
  zlib inflate wrapper (`Crypto.Zlib.decompress`) already used by
  `Data.PDF.Stream`'s own module.

  Upstream's PNG-Up reversal (`unpredict12`) is a cons-based, pointer-
  shuffling `step` function that upstream itself labels "Hacky solution,
  rewrite it" — traced by hand (see the accompanying dependency doc) to
  confirm it computes nothing more than the standard PNG "Up" filter:
  `outputRow[i] = (rawRow[i] + previousOutputRow[i]) mod 256`, with each
  row's leading filter-type tag byte (always assumed `2`/Up here, exactly
  as upstream assumes without checking it) discarded unconditionally and
  `previousOutputRow` initialised to all zeros before the first row.
  `pngUnfilterRows` below reimplements that *behaviour* directly against
  Lean's `List`, rather than transliterating upstream's cons-shuffling
  trick — a faithful-to-*behaviour* (not faithful-to-implementation-detail)
  port, consistent with upstream disclaiming that trick as non-canonical in
  its own comment. `UInt8`'s `Add` instance already wraps at 256 the same
  way upstream's `Word8` addition does, so no explicit `% 256` is needed. -/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Stream.Filter.Type
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.Stream.Filter.FlateDecode

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.Exception
open Data.PDF.Core.Stream.Filter.Type

/-- Build a `Name` from an internal ASCII literal known not to contain a
    `0x00` byte (e.g. `"FlateDecode"`, `"Predictor"`), sidestepping
    `Name.make`'s `Except` for these always-well-formed call sites. -/
private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- Reverse the PNG "Up" predictor (PDF32000-1:2008 §7.4.4.4, PNG predictor
    `12`) over the raw decompressed bytes: each row is `cols + 1` bytes (a
    leading filter-type tag byte, discarded, plus `cols` data bytes), and
    each data byte is the sum (mod 256) of the corresponding raw byte and
    the same-column byte of the *previous output row* (all zero before the
    first row). See the module doc-comment for why this is a direct
    reimplementation of upstream's `step`, not a transliteration of it.

    `prevRow` carries the previous row's *output* (length `cols`, or
    shorter only on a malformed/truncated trailing row, matching this
    function's lenient behaviour on such input — upstream is equally
    unspecified there). Recursion is on the remaining byte list, which
    strictly shrinks by at least one row (`cols + 1 ≥ 1` bytes) every
    non-empty step. -/
def pngUnfilterRows (cols : Nat) (prevRow : List UInt8) : List UInt8 → List UInt8
  | [] => []
  | bytes@(_ :: _) =>
    let row := bytes.take (cols + 1)
    let data := row.drop 1
    let out := List.zipWith (· + ·) data prevRow
    out ++ pngUnfilterRows cols out (bytes.drop (cols + 1))
termination_by bytes => bytes.length
decreasing_by simp_wf; simp_all; omega

/-- Reverse predictor `p` over an already-inflated `InputStream`, given the
    `/DecodeParms` dictionary (needed to find `/Columns` for predictor
    `12`). Mirrors upstream's `unpredict`. -/
def unpredict (dict : Dict) (p : Int) (is : Data.PDF.Stream.InputStream) :
    IO Data.PDF.Stream.InputStream :=
  if p == 1 then
    pure is
  else if p == 12 then
    message "unpredict" do
      match dict.get? (mkName "Columns") with
      | none => throw (corrupted "Column is missing")
      | some o =>
        match intValue o with
        | some colsI =>
          let cols := colsI.toNat
          let bytes ← Data.PDF.Stream.toList is
          let flat := (bytes.foldl (· ++ ·) ByteArray.empty).toList
          let unfiltered := pngUnfilterRows cols (List.replicate cols 0) flat
          Data.PDF.Stream.fromByteString (ByteArray.mk unfiltered.toArray)
        | none => throw (corrupted "Column should be an integer")
  else
    throw (unexpected s!"Unsupported predictor: {p}")

/-- `FlateDecode`'s decoder: inflate via zlib, then optionally reverse a
    predictor named by `/DecodeParms`'s `/Predictor` entry. Mirrors
    upstream's `decode`. -/
def decode (parms : Option Dict) (is : Data.PDF.Stream.InputStream) :
    IO Data.PDF.Stream.InputStream := do
  match parms with
  | none => Data.PDF.Stream.decompress is
  | some dict =>
    match dict.get? (mkName "Predictor") with
    | none => Data.PDF.Stream.decompress is
    | some o =>
      match intValue o with
      | some p => do
        let inflated ← Data.PDF.Stream.decompress is
        unpredict dict p inflated
      | none => throw (corrupted "Predictor should be an integer")

/-- The `FlateDecode` stream filter (PDF32000-1:2008 §7.4.4). Unlike
    upstream's `Maybe StreamFilter` (`Nothing` when the zlib cabal flag is
    disabled), this is always available: `linen` has no analogous
    build-time flag, and this port always uses real zlib inflate (see the
    module doc-comment). -/
def flateDecode : StreamFilter :=
  { filterName := mkName "FlateDecode"
    filterDecode := decode }

end Data.PDF.Core.Stream.Filter.FlateDecode
