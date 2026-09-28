/-
  Tests for `Linen.Data.PDF.Core.Object.Builder`.
-/
import Linen.Data.PDF.Core.Object.Builder

open Data.PDF.Core.Object
open Data.PDF.Core.Object.Builder

namespace Tests.Data.PDF.Core.Object.Builder

private def name! (s : String) : Name :=
  match Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList) with
  | .ok n => n
  | .error _ => Data.PDF.Core.Name.Name.empty

private def bytesToStr (bs : Data.ByteString) : String :=
  String.ofList (bs.unpack.map (fun b => Char.ofNat b.toNat))

/-- Render `o`, collapsing both the `Except`'s cases to a plain `String` (an
    error becomes `"<error: ...>"`) so tests can compare with plain `==`
    without needing a `BEq (Except String String)` instance. -/
private def render (o : Object) : String :=
  match buildObject o with
  | .ok b => bytesToStr b.toStrictByteString
  | .error e => s!"<error: {e}>"

-- `formatFixed` renders six fixed decimal digits, matching `printf "%f"`.
#guard formatFixed 3.14159 == "3.141590"
#guard formatFixed (-2.5) == "-2.500000"
#guard formatFixed 0.0 == "0.000000"

-- `hexByte` zero-pads to two digits.
#guard hexByte 0x0f == "0f"
#guard hexByte 0xab == "ab"
#guard hexByte 0x00 == "00"

-- `toExactInt` reconstructs the exact integer from an integral `Scientific`,
-- with no upper-bound cap (unlike `Data.PDF.Core.Object.Util.intValue`).
#guard toExactInt (Data.Scientific.mk 42 0) == 42
#guard toExactInt (Data.Scientific.mk 5 3) == 5000

-- An integral number renders as a plain decimal integer.
#guard render (.number (Data.Scientific.mk 42 0)) == "42"

-- A fractional number renders in fixed-point notation.
#guard render (.number (Data.Scientific.mk 314 (-2))) == "3.140000"

-- Booleans, names, `null`, strings, and refs render directly.
#guard render (.bool true) == "true"
#guard render (.bool false) == "false"
#guard render .null == "null"
#guard render (.name (name! "Type")) == "/Type"
#guard render (.ref ⟨3, 0⟩) == "3 0 R"

-- Printable strings render literally, with escaping.
#guard render (.string (Data.ByteString.pack "hi(there)".toUTF8.toList)) ==
  "(hi\\(there\\))"

-- Non-printable strings render as hex.
#guard render (.string (Data.ByteString.pack [0xff, 0x00])) == "<ff00>"

-- A dictionary renders as `<<key value ...>>`.
#guard render (.dictRaw #[(name! "Type", Object.name (name! "Page"))]) ==
  "<</Type /Page>>"

-- An empty dictionary renders as `<<>>`.
#guard render (Object.dict (Std.HashMap.ofList [])) == "<<>>"

-- An array renders as `[item item ...]`.
#guard render (.array #[.number (Data.Scientific.mk 1 0), .bool true]) ==
  "[1 true]"

-- An empty array renders as `[]`.
#guard render (.array #[]) == "[]"

-- `buildObject` on a `stream` is a total (caught) error, not a panic — the
-- exact upstream message, ported as an `Except` value (see the module
-- doc-comment).
#guard render (.stream (Stream.mk' (Std.HashMap.ofList []) 0)) ==
  "<error: buildObject: please don't pass streams to me>"

-- `buildStream` renders a dictionary, `stream\n`, the content, then
-- `\nendstream`.
#guard
  let dict : Dict := Std.HashMap.ofList [(name! "Length", Object.number (Data.Scientific.mk 5 0))]
  match buildStream dict (Data.ByteString.Lazy.LazyByteString.fromStrict
      (Data.ByteString.pack "hello".toUTF8.toList)) with
  | .ok b => bytesToStr b.toStrictByteString == "<</Length 5>>stream\nhello\nendstream"
  | .error _ => false

-- `buildIndirectObject` wraps the rendered object in `\nN G obj\n...\nendobj\n`.
#guard
  match buildIndirectObject ⟨3, 0⟩ .null with
  | .ok b => bytesToStr b.toStrictByteString == "\n3 0 obj\nnull\nendobj\n"
  | .error _ => false

-- `buildIndirectStream` wraps a built stream the same way.
#guard
  let dict : Dict := Std.HashMap.ofList []
  match buildIndirectStream ⟨4, 0⟩ dict (Data.ByteString.Lazy.LazyByteString.fromStrict
      (Data.ByteString.pack "x".toUTF8.toList)) with
  | .ok b => bytesToStr b.toStrictByteString == "\n4 0 obj\n<<>>stream\nx\nendstream\nendobj\n"
  | .error _ => false

end Tests.Data.PDF.Core.Object.Builder
