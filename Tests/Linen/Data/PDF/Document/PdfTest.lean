/-
  Tests for `Linen.Data.PDF.Document.Pdf`.

  Builds a tiny, well-formed synthetic PDF byte buffer (a `/Catalog`
  dictionary object, a 4-byte unfiltered stream object, a classic xref
  table, and a trailer) and exercises the public API end-to-end through
  it. Everything here is `IO`-based, so it is checked with `#eval` — a
  thrown error fails the build — following
  `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf

private def obj1Text : String := "1 0 obj\n<< /Type /Catalog >>\nendobj\n"
private def obj2Text : String := "2 0 obj\n<< /Length 4 >>\nstream\ndataendstream\nendobj\n"

/-- Two indirect objects back-to-back (a `/Catalog` dictionary at object 1,
    a 4-byte unfiltered stream at object 2), followed by a three-entry
    classic xref table and its trailer. Every offset is computed from the
    pieces themselves, so the fixture can't silently drift out of sync. -/
private def mkDoc : String :=
  let obj2Off := (String.toUTF8 obj1Text).size
  let body := obj1Text ++ obj2Text
  let xrefOff := (String.toUTF8 body).size
  let pad (n : Nat) (off : Nat) : String :=
    let s := toString off
    String.ofList (List.replicate (n - s.length) '0') ++ s
  let xrefTable :=
    "xref\n0 3\n" ++
    "0000000000 65535 f \n" ++
    pad 10 0 ++ " 00000 n \n" ++
    pad 10 obj2Off ++ " 00000 n \n"
  let trailerText := "trailer\n<< /Size 3 /Root 1 0 R >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  body ++ xrefTable ++ trailerText ++ startxrefText

private def bytes : ByteArray := String.toUTF8 mkDoc

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

namespace Tests.Data.PDF.Document.Pdf

/-! ### Opening a `Pdf` and finding the document ── -/

-- `fromBytes` builds a `Pdf`, and `document` produces the trailer-backed
-- `Document` handle for an unencrypted file.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc ← document pdf
  match doc.dict.get? (mkName "Root") with
  | some (.ref r) => unless r == (⟨1, 0⟩ : Ref) do throw (IO.userError "unexpected /Root")
  | _ => throw (IO.userError "missing /Root in trailer")

/-! ### Object lookup and caching ── -/

-- `lookupObject` resolves an indirect reference to its object.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let obj ← lookupObject pdf (⟨1, 0⟩ : Ref)
  match dictValue obj with
  | some _ => pure ()
  | none => throw (IO.userError s!"expected a dictionary, got: {reprStr obj}")

-- With caching disabled (the default), repeated lookups still succeed
-- (each one re-resolves from the file, rather than the cache).
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let _ ← lookupObject pdf (⟨1, 0⟩ : Ref)
  let obj ← lookupObject pdf (⟨1, 0⟩ : Ref)
  match dictValue obj with
  | some _ => pure ()
  | none => throw (IO.userError "expected a dictionary on the second lookup too")

-- After `enableCache`, a lookup populates the cache; `disableCache` turns
-- population back off (already-cached entries stay cached, though — the
-- flag only controls future writes, mirroring upstream).
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  enableCache pdf
  let _ ← lookupObject pdf (⟨1, 0⟩ : Ref)
  let (_, cache) ← pdf.cache.get
  unless cache.contains (⟨1, 0⟩ : Ref) do
    throw (IO.userError "expected the object to be cached after enableCache")
  disableCache pdf
  let (useCache, _) ← pdf.cache.get
  unless useCache == false do
    throw (IO.userError "expected disableCache to turn caching off")

/-! ### `deref` ── -/

-- `deref` follows a `Ref` to its object, and passes through anything else
-- unchanged.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let derefed ← deref pdf (Object.ref (⟨1, 0⟩ : Ref))
  match dictValue derefed with
  | some _ => pure ()
  | none => throw (IO.userError "expected deref to resolve the reference")
  let same ← deref pdf (Object.number 7)
  unless same == Object.number 7 do
    throw (IO.userError "expected deref to pass non-refs through unchanged")

/-! ### Stream content ── -/

-- `streamContent` reads object 2's decoded (here: unfiltered) payload.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let obj ← lookupObject pdf (⟨2, 0⟩ : Ref)
  match obj with
  | .stream s => do
    let is ← streamContent pdf (⟨2, 0⟩ : Ref) s
    let chunks ← Data.PDF.Stream.toList is
    let out := chunks.foldl (· ++ ·) ByteArray.empty
    unless out == String.toUTF8 "data" do
      throw (IO.userError s!"unexpected stream content: {out.toList}")
  | other => throw (IO.userError s!"expected a stream, got: {reprStr other}")

/-! ### Encryption ── -/

-- An unencrypted document reports `isEncrypted == false`, and
-- `setUserPassword` fails on it (it isn't encrypted to begin with).
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  unless (← isEncrypted pdf) == false do
    throw (IO.userError "expected an unencrypted document")
  MonadExcept.tryCatch
    (do
      let _ ← setUserPassword pdf ByteArray.empty
      throw (IO.userError "expected setUserPassword to fail on an unencrypted document"))
    (fun _ => pure ())

-- `defaultUserPassword` is re-exported straight from `Data.PDF.Core`.
#guard defaultUserPassword == Data.PDF.Core.Encryption.defaultUserPassword

end Tests.Data.PDF.Document.Pdf
