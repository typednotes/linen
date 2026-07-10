/-
  Data.PDF.Document.Internal.Util — utilities for internal use

  Ports `Pdf.Document.Internal.Util` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/Internal/Util.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Internal/Util.hs`),
  module 3 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  ## Design

  - `ensureType`/`dictionaryType` are ported directly against
    `Data.PDF.Core.Object.Util.nameValue` and `Std.HashMap.get?`, mirroring
    upstream's `HashMap.lookup "Type" dict` pattern-match.

  - `decodeTextString` implements PDF32000-1:2008 §7.9.2.2's "text string"
    format: UTF-16BE (with a mandatory `\xFE\xFF` byte-order mark) if the
    byte string starts with that BOM, otherwise PDFDocEncoding. The UTF-16BE
    branch reuses `Data.PDF.Content.UnicodeCMap.decodeUtf16BE` — already
    the established big-endian UTF-16 decoder in this port (see that
    module's own doc-comment for its surrogate-pair handling) — rather than
    re-deriving a second one; the PDFDocEncoding branch reuses
    `Data.PDF.Content.Encoding.PdfDoc.pdfDocEncoding`, mirroring upstream's
    `Map.lookup c PdfDoc.encoding` byte-by-byte lookup.

  - `decodeTextStringThrow` is exactly `Data.PDF.Core.Exception.sure`
    applied to `decodeTextString`: upstream's own definition
    (`case decodeTextString bs of Left err -> throwIO (Corrupted err []);
    Right txt -> return txt`) is precisely what `sure` already does for any
    `Except String α`, so no separate helper logic is needed here beyond
    that reuse.
-/
import Linen.Data.ByteString
import Linen.Data.Text
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Content.UnicodeCMap
import Linen.Data.PDF.Content.Encoding.PdfDoc

namespace Data.PDF.Document.Internal.Util

open Data.PDF.Core.Object (Name Dict)
open Data.PDF.Core.Object.Util (nameValue)
open Data.PDF.Core.Exception (sure corrupted)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── Dictionary type checking ── -/

/-- Get a dictionary's `"Type"` entry as a `Name`. Mirrors upstream's
    `dictionaryType`. -/
def dictionaryType (dict : Dict) : Except String Name :=
  match dict.get? (mkName "Type") with
  | some o =>
    match nameValue o with
    | some n => .ok n
    | none => .error "Type should be a name"
  | none => .error "Type is missing"

/-- Check that the dictionary's `"Type"` entry is exactly `name`, throwing
    `corrupted` otherwise. Mirrors upstream's `ensureType`. -/
def ensureType (name : Name) (dict : Dict) : IO Unit := do
  let n ← sure (dictionaryType dict)
  unless n == name do
    throw (corrupted s!"Expected type: {reprStr name}, but found: {reprStr n}")

/-! ── Text-string decoding (PDF32000-1:2008 §7.9.2.2) ── -/

/-- Decode a PDF "text string": UTF-16BE (with a `\xFE\xFF` byte-order
    mark) if present, otherwise PDFDocEncoding. Mirrors upstream's
    `decodeTextString`. -/
def decodeTextString (bs : Data.ByteString) : Except String Data.Text :=
  let bom : Data.ByteString := Data.ByteString.pack [0xFE, 0xFF]
  if bom.isPrefixOf bs then
    .ok (Data.PDF.Content.UnicodeCMap.decodeUtf16BE (bs.drop 2))
  else
    match bs.unpack.mapM Data.PDF.Content.Encoding.PdfDoc.pdfDocEncoding.get? with
    | some chars => .ok (Data.Text.concat chars)
    | none => .error "Unknow symbol"

/-- `decodeTextString`, throwing `corrupted` instead of returning an
    `Except`. Mirrors upstream's `decodeTextStringThrow` (see the module
    doc-comment for why this is exactly `sure ∘ decodeTextString`). -/
def decodeTextStringThrow (bs : Data.ByteString) : IO Data.Text :=
  sure (decodeTextString bs)

end Data.PDF.Document.Internal.Util
