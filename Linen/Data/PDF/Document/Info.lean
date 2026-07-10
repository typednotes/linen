/-
  Data.PDF.Document.Info — the document information dictionary

  Ports `Pdf.Document.Info` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox, `document/lib/Pdf/Document/Info.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Info.hs`),
  module 6 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  ## Design

  Upstream defines six accessors (`infoTitle`, `infoAuthor`, `infoSubject`,
  `infoKeywords`, `infoCreator`, `infoProducer`) into the information
  dictionary (PDF32000-1:2008 §14.3.3), each with an *identical* body up to
  the dictionary key and the field's own name in the error message:

  ```haskell
  infoTitle (Info pdf _ dict) =
    case HashMap.lookup "Title" dict of
      Nothing -> return Nothing
      Just o -> do
        o' <- deref pdf o
        mstr <- sure $ fmap Just (stringValue o') `notice` "Title should be a string"
        case mstr of
          Nothing -> return Nothing
          Just str -> Just <$> decodeTextStringThrow str
  ```

  Per `docs/imports/PdfToolboxDocument/dependencies.md`'s scope note, this
  six-way repetition is de-duplicated through one shared, key-parametrized
  helper (`infoTextField` below), called once per public accessor — a
  direct fold of upstream's own copy-pasted pattern into a single
  definition, not new abstraction beyond what the task needs. Each public
  accessor's doc-comment states the dictionary key it reads, exactly the
  information the six near-identical Haskell bodies otherwise conveyed only
  through their names.

  One more upstream quirk, preserved by neither the six copies nor the one
  shared helper: `mstr <- sure $ fmap Just (stringValue o') \`notice\`
  "... should be a string"` throws (via `sure`/`notice`) whenever `o'`
  isn't a string at all, so the subsequent `case mstr of Nothing -> return
  Nothing; Just str -> ...` can *never* actually observe `Nothing` — that
  branch is dead code (`fmap Just` on a `Maybe` can only ever produce
  `Nothing` when the inner value already was `Nothing`, which `notice`
  turns into a thrown error before `mstr` is even bound). `infoTextField`
  ports the function's real, observable behaviour directly: `none` only
  when the dictionary key itself is absent, and a thrown error (not a
  silent `none`) whenever the key is present but not a string — the same
  "don't preserve a checked-impossible branch" treatment
  `Data.PDF.Core.Util`'s module doc-comment already applies to
  `readCompressedObject`'s own dead branch, not a new abstraction. -/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Internal.Util
import Linen.Data.PDF.Document.Pdf

namespace Data.PDF.Document.Info

open Data.PDF.Core.Object (Name)
open Data.PDF.Core.Object.Util (stringValue)
open Data.PDF.Core.Exception (sure)
open Data.PDF.Core.Util (notice)
open Data.PDF.Document.Internal.Types (Info)

export Data.PDF.Document.Internal.Types (Info)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── The shared accessor (see the module doc-comment) ── -/

/-- Read `key` out of the information dictionary as a decoded text string,
    `none` if the key is absent or its value is PDF `null`. Fails (via
    `sure`) if the key is present but not a string. Shared by every public
    accessor below, one call per upstream `infoXxx` function, `field` being
    the human-readable name upstream's own error message names (e.g.
    `"Title"` for `infoTitle`). -/
private def infoTextField (info : Info) (key field : String) : IO (Option Data.Text) := do
  match info.dict.get? (mkName key) with
  | none => pure none
  | some o => do
    let o' ← Data.PDF.Document.Pdf.deref info.pdf o
    let str ← sure (notice (stringValue o') s!"{field} should be a string")
    some <$> Data.PDF.Document.Internal.Util.decodeTextStringThrow str

/-! ── Public accessors ── -/

/-- The document's title (dictionary key `"Title"`). Mirrors upstream's
    `infoTitle`. -/
def infoTitle (info : Info) : IO (Option Data.Text) := infoTextField info "Title" "Title"

/-- The name of the person who created the document (dictionary key
    `"Author"`). Mirrors upstream's `infoAuthor`. -/
def infoAuthor (info : Info) : IO (Option Data.Text) := infoTextField info "Author" "Author"

/-- The subject of the document (dictionary key `"Subject"`). Mirrors
    upstream's `infoSubject`. -/
def infoSubject (info : Info) : IO (Option Data.Text) := infoTextField info "Subject" "Subject"

/-- Keywords associated with the document (dictionary key `"Keywords"`).
    Mirrors upstream's `infoKeywords`. -/
def infoKeywords (info : Info) : IO (Option Data.Text) :=
  infoTextField info "Keywords" "Keywords"

/-- The name of the application that created the original document
    (dictionary key `"Creator"`). Mirrors upstream's `infoCreator`. -/
def infoCreator (info : Info) : IO (Option Data.Text) := infoTextField info "Creator" "Creator"

/-- The name of the application that converted the document to PDF format
    (dictionary key `"Producer"`). Mirrors upstream's `infoProducer`. -/
def infoProducer (info : Info) : IO (Option Data.Text) :=
  infoTextField info "Producer" "Producer"

end Data.PDF.Document.Info
