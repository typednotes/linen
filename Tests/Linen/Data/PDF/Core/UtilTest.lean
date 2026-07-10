/-
  Tests for `Linen.Data.PDF.Core.Util`.

  `readObjectAtOffset` and `readCompressedObject` are IO actions (they seek a
  `Buffer`/drain an `InputStream`), so they are checked with `#eval` — a
  thrown error fails the build, as in `Tests/Linen/Crypto/Zlib/FFITest.lean`.
  `notice` and `parseHeaderUpTo` are pure, so they get plain `#guard`s.
-/
import Linen.Data.PDF.Core.Util

open Data.PDF.Core.Object Data.PDF.Core.Util
open Std.Internal.Parsec ByteArray

private def bytes (s : String) : ByteArray := String.toUTF8 s

namespace Tests.Data.PDF.Core.Util

-- `notice` turns `some`/`none` into `Except`.
#guard match notice (some 3) "missing" with
  | .ok n => n == 3
  | .error _ => false
#guard match notice (none : Option Nat) "missing" with
  | .ok _ => false
  | .error e => e == "missing"

-- `parseHeaderUpTo 0` reads exactly one header pair. A trailing sentinel
-- byte (rather than ending the buffer right at the header's own trailing
-- whitespace) avoids `Std.Internal.Parsec.ByteArray.skipWhile`'s eof
-- behaviour (it fails outright when called with no input left, even with
-- zero required matches — the same quirk documented in `Parsers.XRef`).
#guard match Parser.run (parseHeaderUpTo 0) (bytes "5 10 )") with
  | .ok (n, off) => n == 5 && off == 10
  | .error _ => false

-- `parseHeaderUpTo n` reads `n + 1` pairs and returns only the last one.
#guard match Parser.run (parseHeaderUpTo 2) (bytes "1 100 2 200 3 300 )") with
  | .ok (n, off) => n == 3 && off == 300
  | .error _ => false

-- `readObjectAtOffset` reads a bare indirect object at a given byte offset.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes "garbage\n3 0 obj\n42\nendobj")
  let (r, o) ← readObjectAtOffset buf 8
  unless r == (⟨3, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected ref: {reprStr r}")
  match o with
  | .number n => unless n.toBoundedInteger == some 42 do
      throw (IO.userError s!"unexpected number: {reprStr n}")
  | other => throw (IO.userError s!"expected a number, got: {reprStr other}")

-- `readObjectAtOffset` on a stream object updates the stream's payload
-- offset to right after the `stream` keyword's end-of-line.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes
    (bytes "5 0 obj\n<< /Length 4 >>\nstream\ndataXXXX")
  let (r, o) ← readObjectAtOffset buf 0
  unless r == (⟨5, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected ref: {reprStr r}")
  match o with
  | .stream s =>
    -- The payload starts right at "data", i.e. byte offset 31.
    unless s.offset == 31 do
      throw (IO.userError s!"unexpected stream offset: {s.offset}")
  | other => throw (IO.userError s!"expected a stream, got: {reprStr other}")

-- `readCompressedObject` reads object `num` out of a decoded object stream's
-- header table plus data section (PDF32000-1:2008 §7.5.7): two objects,
-- header `"0 0 1 3"` (obj 0 at data-relative offset 0, obj 1 at offset 3),
-- `first = 8` (the header table's length), data `"42 17 "`.
#eval show IO Unit from do
  let is ← Data.PDF.Stream.fromByteString (bytes "0 0 1 3 42 17 ")
  let o ← readCompressedObject is 8 1
  match o with
  | .number n => unless n.toBoundedInteger == some 17 do
      throw (IO.userError s!"unexpected number: {reprStr n}")
  | other => throw (IO.userError s!"expected a number, got: {reprStr other}")

end Tests.Data.PDF.Core.Util
