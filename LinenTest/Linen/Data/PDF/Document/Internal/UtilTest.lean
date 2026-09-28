/-
  Tests for `Linen.Data.PDF.Document.Internal.Util`.

  `dictionaryType`/`decodeTextString` are pure (`Except`-returning), so
  they're checked with `#guard`. `ensureType`/`decodeTextStringThrow` throw
  through `IO`, so they're checked with `#eval`.
-/
import Linen.Data.PDF.Document.Internal.Util

open Data.PDF.Core.Object
open Data.PDF.Document.Internal.Util

namespace Tests.Data.PDF.Document.Internal.Util

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ### `dictionaryType`/`ensureType` -/

-- A dictionary whose `"Type"` entry is a `Name` reports it.
#guard
  let dict : Dict := Std.HashMap.ofList [(mkName "Type", Object.name (mkName "Page"))]
  match dictionaryType dict with
  | .ok n => n == mkName "Page"
  | .error _ => false

-- A dictionary with no `"Type"` entry reports it's missing.
#guard
  match dictionaryType (∅ : Dict) with
  | .error _ => true
  | .ok _ => false

-- A dictionary whose `"Type"` entry isn't a `Name` is rejected.
#guard
  let dict : Dict := Std.HashMap.ofList [(mkName "Type", Object.bool true)]
  match dictionaryType dict with
  | .error _ => true
  | .ok _ => false

-- `ensureType` succeeds silently when the type matches.
#eval show IO Unit from do
  let dict : Dict := Std.HashMap.ofList [(mkName "Type", Object.name (mkName "Page"))]
  ensureType (mkName "Page") dict

-- `ensureType` throws (a `Corrupted`-tagged error) when the type doesn't
-- match.
#eval show IO Unit from do
  let dict : Dict := Std.HashMap.ofList [(mkName "Type", Object.name (mkName "Pages"))]
  MonadExcept.tryCatch
    (do
      ensureType (mkName "Page") dict
      throw (IO.userError "expected ensureType to fail on a type mismatch"))
    (fun e => match e with
      | .userError msg =>
        if msg.startsWith "Corrupted: " then pure ()
        else throw (IO.userError s!"unexpected error message: {msg}")
      | other => throw other)

/-! ### `decodeTextString`/`decodeTextStringThrow` -/

-- A UTF-16BE string (with its mandatory `\xFE\xFF` byte-order mark)
-- decodes to the expected text: `FE FF 00 41 00 42` is the BOM followed by
-- big-endian code units for `'A'` and `'B'`.
#guard
  match decodeTextString (Data.ByteString.pack [0xFE, 0xFF, 0x00, 0x41, 0x00, 0x42]) with
  | .ok t => t == "AB"
  | .error _ => false

-- A PDFDocEncoding string (no byte-order mark) decodes byte-by-byte.
#guard
  match decodeTextString (Data.ByteString.pack [0x41, 0x42]) with
  | .ok t => t == "AB"
  | .error _ => false

-- `decodeTextStringThrow` succeeds on a well-formed PDFDocEncoding string.
#eval show IO Unit from do
  let txt ← decodeTextStringThrow (Data.ByteString.pack [0x41, 0x42])
  unless txt == "AB" do
    throw (IO.userError s!"unexpected decoded text: {reprStr txt}")

end Tests.Data.PDF.Document.Internal.Util
