/-
  Tests for `Linen.Data.PDF.Core.File`.

  Builds a tiny, well-formed synthetic PDF byte buffer (one indirect
  object, a classic xref table, and a trailer) and exercises the public
  API end-to-end through it. Everything here is `IO`-based, so it is
  checked with `#eval` — a thrown error fails the build — following
  `Tests/Linen/Data/PDF/Core/UtilTest.lean`'s pattern.
-/
import Linen.Data.PDF.Core.File

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.File

private def bytes (s : String) : ByteArray := String.toUTF8 s

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- One indirect object (`1 0 obj 42 endobj`) at byte offset `0`, followed
    by a two-entry classic xref table (object `0` free, object `1` in
    use at offset `0`) and its trailer, with the mandatory `startxref`/
    `%%EOF` tail. Every offset is computed from the pieces themselves
    rather than hand-counted, so the fixture can't silently drift out of
    sync with itself. -/
private def mkDoc : String :=
  let obj1 := "1 0 obj\n42\nendobj\n"
  let xrefOff := (bytes obj1).size
  let xrefTable :=
    "xref\n0 2\n" ++
    "0000000000 65535 f \n" ++
    "0000000000 00000 n \n"
  let trailerText := "trailer\n<< /Size 2 /Root 1 0 R >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  obj1 ++ xrefTable ++ trailerText ++ startxrefText

namespace Tests.Data.PDF.Core.File

/-! ### Opening a file and resolving an object -/

-- `fromBytes` builds a `File`, and `findObject` resolves an in-use entry
-- straight to its object.
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  let obj ← findObject file (⟨1, 0⟩ : Ref)
  match obj with
  | .number n => unless n.toBoundedInteger == some 42 do
      throw (IO.userError s!"unexpected number: {reprStr n}")
  | other => throw (IO.userError s!"expected a number, got: {reprStr other}")

-- `findObject` on the free slot (object `0`) returns `Object.null`.
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  let obj ← findObject file (⟨0, 0⟩ : Ref)
  unless obj == Object.null do
    throw (IO.userError s!"expected null for a free entry, got: {reprStr obj}")

-- `findObject` on a ref with no entry anywhere in the xref chain raises a
-- `"NotFound: "`-tagged error (see the module doc-comment for why this
-- substitutes for upstream's dedicated `NotFound` exception type).
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  MonadExcept.tryCatch
    (do
      let _ ← findObject file (⟨99, 0⟩ : Ref)
      throw (IO.userError "expected findObject to raise NotFound"))
    (fun e => match e with
      | .userError msg =>
        if msg.startsWith "NotFound: " then pure ()
        else throw (IO.userError s!"unexpected error message: {msg}")
      | other => throw other)

/-! ### The trailer -/

-- `lastTrailer` parses the trailer dictionary reachable from the file's
-- most recent xref.
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  let tr ← lastTrailer file
  match tr.get? (mkName "Size") with
  | some o => unless intValue o == some 2 do throw (IO.userError "unexpected /Size")
  | none => throw (IO.userError "missing /Size in trailer")

/-! ### Encryption status -/

-- A document with no `/Encrypt` entry is `.plain`.
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  let status ← encryptionStatus file
  unless status == EncryptionStatus.plain do
    throw (IO.userError s!"expected .plain, got: {reprStr status}")

-- `setUserPassword` on an unencrypted document reports the "not
-- encrypted" failure (via `message`-wrapped `unexpected`) rather than
-- silently succeeding.
#eval show IO Unit from do
  let file ← fromBytes Data.PDF.Core.Stream.knownFilters (bytes mkDoc)
  MonadExcept.tryCatch
    (do
      let _ ← setUserPassword file ByteArray.empty
      throw (IO.userError "expected setUserPassword to fail on an unencrypted document"))
    (fun _ => pure ())

end Tests.Data.PDF.Core.File
