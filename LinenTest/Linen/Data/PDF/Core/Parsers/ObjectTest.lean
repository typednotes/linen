/-
  Tests for `Linen.Data.PDF.Core.Parsers.Object`.
-/
import Linen.Data.PDF.Core.Parsers.Object

open Data.PDF.Core.Object Data.PDF.Core.Parsers.Object
open Std.Internal.Parsec ByteArray

private def bytes (s : String) : ByteArray := String.toUTF8 s

private def name! (s : String) : Data.PDF.Core.Name.Name :=
  match Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList) with
  | .ok n => n
  | .error _ => Data.PDF.Core.Name.Name.empty

namespace Tests.Data.PDF.Core.Parsers.Object

-- `parseName` reads a `/Name` up to the next delimiter/whitespace byte.
#guard match Parser.run parseName (bytes "/Type ") with
  | .ok n => n == name! "Type"
  | .error _ => false

-- `parseNumber` parses plain integers, negative integers, and decimals.
#guard match Parser.run parseNumber (bytes "123") with
  | .ok n => n.toBoundedInteger == some 123
  | .error _ => false
#guard match Parser.run parseNumber (bytes "-17") with
  | .ok n => n.toBoundedInteger == some (-17)
  | .error _ => false
#guard match Parser.run parseNumber (bytes "3.25") with
  | .ok n => n.toRealFloat == 3.25
  | .error _ => false
-- A leading-dot number with no integer part.
#guard match Parser.run parseNumber (bytes ".5") with
  | .ok n => n.toRealFloat == 0.5
  | .error _ => false

-- `parseString` decodes a literal string, including a balanced nested
-- parenthesis pair and the standard named escapes.
#guard match Parser.run parseString (bytes "(hello (world) \\n\\t)") with
  | .ok s => Data.ByteString.unpack s == "hello (world) \n\t".toUTF8.toList
  | .error _ => false

-- `parseString` decodes a `\ddd` octal escape (`\101` = 'A' = 65).
#guard match Parser.run parseString (bytes "(\\101)") with
  | .ok s => Data.ByteString.unpack s == [65]
  | .error _ => false

-- `parseHexString` decodes hex-digit pairs, `<48656C6C6F>` = "Hello".
#guard match Parser.run parseHexString (bytes "<48656C6C6F>") with
  | .ok s => Data.ByteString.unpack s == "Hello".toUTF8.toList
  | .error _ => false

-- `parseBool` parses both boolean literals.
#guard match Parser.run parseBool (bytes "true") with | .ok b => b | .error _ => false
#guard match Parser.run parseBool (bytes "false") with | .ok b => !b | .error _ => false

-- `parseRef` parses an indirect reference `idx gen R`.
#guard match Parser.run parseRef (bytes "12 0 R") with
  | .ok r => r == (⟨12, 0⟩ : Ref)
  | .error _ => false

-- `parseObject` dispatches to `null`, dictionaries, and arrays.
#guard match Parser.run parseObject (bytes "null") with
  | .ok Object.null => true
  | _ => false

#guard match Parser.run parseObject (bytes "<< /Type /Page >>") with
  | .ok (.dictRaw entries) => entries.toList == [(name! "Type", Object.name (name! "Page"))]
  | _ => false

#guard match Parser.run parseObject (bytes "[1 2 3]") with
  | .ok (.array items) => items.toList.map (fun o => match o with
      | .number n => n.toBoundedInteger
      | _ => none) == [some 1, some 2, some 3]
  | _ => false

-- Nested dictionaries/arrays round-trip through the mutual recursion.
#guard match Parser.run parseObject (bytes "<< /Kids [1 0 R 2 0 R] >>") with
  | .ok (.dictRaw #[(k, .array items)]) =>
    k == name! "Kids" && items.size == 2
  | _ => false

-- `parseIndirectObject` parses a bare (non-stream) indirect object.
#guard match Parser.run parseIndirectObject (bytes "3 0 obj\n42\nendobj") with
  | .ok (r, .number n) => r == (⟨3, 0⟩ : Ref) && n.toBoundedInteger == some 42
  | _ => false

-- `parseIndirectObject` reinterprets a dict immediately followed by
-- `stream` as a `Stream` object.
#guard match Parser.run parseIndirectObject
    (bytes "5 0 obj\n<< /Length 4 >>\nstream\ndata") with
  | .ok (r, .stream s) => r == (⟨5, 0⟩ : Ref) && s.dict.toList == [(name! "Length", Object.number (Data.Scientific.fromInt 4))]
  | _ => false

end Tests.Data.PDF.Core.Parsers.Object
