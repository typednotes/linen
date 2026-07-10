/-
  Data.PDF.Document.Pdf — the top-level PDF handle

  Ports `Pdf.Document.Pdf` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox, `document/lib/Pdf/Document/Pdf.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Pdf.hs`),
  module 4 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  Wraps `Data.PDF.Document.Internal.Types.Pdf` (a low-level `File` plus a
  mutable object cache) with the object-resolution/caching API that every
  higher-level document type (`Document`, `Catalog`, `Info`, and the next
  batch's `PageNode`/`Page`/`FontDict`) is built on: `lookupObject`
  transparently consults/populates the cache, `deref` follows one level of
  indirection, and `document` produces the top-level `Document` handle
  (checking encryption status first).

  ## Design

  - Upstream's `EncryptedError` is a dedicated `Exception` instance
    (`data EncryptedError = EncryptedError Text deriving Show; instance
    Exception EncryptedError`), thrown by `document` when the file is
    encrypted and no password has been set yet. Per
    `Data.PDF.Core.Exception`'s module doc-comment (Lean's `IO.Error` isn't
    an open, `Typeable`-indexed hierarchy the way `Control.Exception`'s is),
    this is rendered the same way every other exception in this port is: a
    plain `IO.Error.userError` tagged `"EncryptedError: "`, thrown directly
    by `document` rather than exposed as a separate type/constructor name.

  - `lookupObject`'s cache read-then-maybe-write is ported directly against
    `IO.Ref`'s `get`/`set` (mirroring upstream's `readIORef`/`writeIORef`),
    with no additional locking: upstream itself has none either (a single
    `Pdf` handle is not documented as thread-safe upstream), so this is a
    faithful port, not a simplification.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.File
import Linen.Data.PDF.Core.Stream
import Linen.Data.PDF.Core.Encryption
import Linen.Data.PDF.Stream
import Linen.Data.PDF.Document.Internal.Types
import Std.Data.HashMap

namespace Data.PDF.Document.Pdf

open Data.PDF.Core.Object (Object Ref Stream)
open Data.PDF.Document.Internal.Types (Pdf Document)

export Data.PDF.Document.Internal.Types (Pdf)
export Data.PDF.Core.Encryption (defaultUserPassword)

/-! ── Opening a `Pdf` handle ── -/

/-- Make a `Pdf` handle wrapping an already-open low-level `File`. Mirrors
    upstream's `fromFile`. -/
def fromFile (f : Data.PDF.Core.File.File) : IO Pdf := do
  let cache ← IO.mkRef (false, ({} : Std.HashMap Ref Object))
  pure { file := f, cache := cache }

/-- Make a `Pdf` handle from a seekable binary handle. Mirrors upstream's
    `fromHandle`. -/
def fromHandle (h : IO.FS.Handle) : IO Pdf := do
  let f ← Data.PDF.Core.File.fromHandle Data.PDF.Core.Stream.knownFilters h
  fromFile f

/-- Make a `Pdf` handle from an already-resident `ByteArray`. Mirrors
    upstream's `fromBytes`. -/
def fromBytes (bytes : ByteArray) : IO Pdf := do
  let f ← Data.PDF.Core.File.fromBytes Data.PDF.Core.Stream.knownFilters bytes
  fromFile f

/-- Open a PDF file at `path` and run `action` on the resulting `Pdf`
    handle. Mirrors upstream's `withPdfFile`. -/
def withPdfFile (path : System.FilePath) (action : Pdf → IO α) : IO α :=
  Data.PDF.Core.File.withPdfFile path fun f => do
    let pdf ← fromFile f
    action pdf

/-! ── The document handle ── -/

/-- Get the top-level PDF document, checking that the file isn't encrypted
    (or has already been decrypted) first. Mirrors upstream's `document`
    (see the module doc-comment for how `EncryptedError` is rendered). -/
def document (pdf : Pdf) : IO Document := do
  match ← Data.PDF.Core.File.encryptionStatus pdf.file with
  | .encrypted =>
    throw (IO.Error.userError "EncryptedError: File is encrypted, use 'setUserPassword'")
  | .decrypted => pure ()
  | .plain => pure ()
  let dict ← Data.PDF.Core.File.lastTrailer pdf.file
  pure { pdf := pdf, dict := dict }

/-! ── Object resolution and caching ── -/

/-- Find an object by its reference, consulting (and, if caching is
    enabled, populating) the object cache. Mirrors upstream's
    `lookupObject`. -/
def lookupObject (pdf : Pdf) (ref : Ref) : IO Object := do
  let (useCache, cache) ← pdf.cache.get
  match cache.get? ref with
  | some obj => pure obj
  | none => do
    let obj ← Data.PDF.Core.File.findObject pdf.file ref
    if useCache then
      pdf.cache.set (useCache, cache.insert ref obj)
    pure obj

/-- Enable caching for future `lookupObject` calls. Mirrors upstream's
    `enableCache`. -/
def enableCache (pdf : Pdf) : IO Unit := do
  let (_, cache) ← pdf.cache.get
  pdf.cache.set (true, cache)

/-- Disable caching for future `lookupObject` calls. Mirrors upstream's
    `disableCache`. -/
def disableCache (pdf : Pdf) : IO Unit := do
  let (_, cache) ← pdf.cache.get
  pdf.cache.set (false, cache)

/-- Get a stream's content, decoded and decrypted. Note: the content's
    length may differ from the raw one. Mirrors upstream's
    `streamContent`. -/
def streamContent (pdf : Pdf) (ref : Ref) (s : Stream) : IO Data.PDF.Stream.InputStream :=
  Data.PDF.Core.File.streamContent pdf.file ref s

/-- Get a stream's content without decoding it. Mirrors upstream's
    `rawStreamContent`. -/
def rawStreamContent (pdf : Pdf) (ref : Ref) (s : Stream) : IO Data.PDF.Stream.InputStream :=
  Data.PDF.Core.File.rawStreamContent pdf.file ref s

/-- Whether the PDF document is encrypted. Mirrors upstream's
    `isEncrypted`. -/
def isEncrypted (pdf : Pdf) : IO Bool := do
  match ← Data.PDF.Core.File.encryptionStatus pdf.file with
  | .encrypted => pure true
  | .decrypted => pure true
  | .plain => pure false

/-- Set the password to use for decryption. Returns `false` when the
    password is wrong. Mirrors upstream's `setUserPassword`. -/
def setUserPassword (pdf : Pdf) (password : ByteArray) : IO Bool :=
  Data.PDF.Core.File.setUserPassword pdf.file password

/-- Follow one level of indirection: resolve `o` if it's a `Ref`, otherwise
    return it unchanged. Mirrors upstream's `deref`. -/
def deref (pdf : Pdf) (o : Object) : IO Object :=
  match o with
  | .ref r => lookupObject pdf r
  | other => pure other

end Data.PDF.Document.Pdf
