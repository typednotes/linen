/-
  Data.PDF.Core.File — a PDF file as a set of objects

  Ports `Pdf.Core.File` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/File.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/core/lib/Pdf/Core/File.hs`),
  module 16 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  Ties `Data.PDF.Core.IO.Buffer`, `Data.PDF.Core.XRef` and
  `Data.PDF.Core.Encryption` together: `findObject` resolves an indirect
  reference to its `Object` by walking the xref chain (following `/Prev`),
  decoding object-stream entries, and decrypting as needed; `streamContent`/
  `rawStreamContent` read a stream's payload, resolving an indirect `/Length`
  reference if the dictionary has one.

  ## Design

  - Upstream's `findObject`/`readObjectForEntry`/`streamContent`/
    `rawStreamContent`/`lookupEntry`/`lookupEntryRec` form one genuine,
    unavoidable mutual recursion: resolving an object can require resolving
    its containing object stream (itself an object, `findObject`), which can
    require resolving *that* stream's `/Length` (`rawStreamContent`), which
    can itself be an indirect reference (back to `findObject`) — and
    `lookupEntry` on an xref *stream* needs that stream's decoded content
    (`streamContent`), which again bottoms out at `findObject`/
    `rawStreamContent`. On any well-formed, finite PDF file, this chain of
    indirection is finite (there are only finitely many objects), but
    nothing in the file format itself rules out a malicious, cyclic `/Prev`
    or `/Length`-refers-to-itself chain that would otherwise recurse forever.

    Per `AGENTS.md`'s ban on `partial def`, the whole group below
    (`resolveObject`, `resolveStreamLength`, `streamContentFueled`,
    `rawStreamContentFueled`, `lookupEntryFueled`, `lookupEntryRecFueled`) is
    a single `mutual` block, structurally recursive on one shared `fuel :
    Nat`, matched down by exactly one at *every* call from one member of the
    group to another (including a member calling itself, e.g.
    `lookupEntryRecFueled` walking the `/Prev` chain) — mirroring the fuel
    technique `Data.PDF.Core.XRef` already establishes for its own
    genuinely-unbounded-but-always-terminating-in-practice IO loops
    (`skipTableLoop`, `lookupTableEntryGo`), just spread across several
    mutually-recursive functions instead of one. `findObject`/
    `streamContent`/`rawStreamContent` (the public API mirroring upstream's
    functions of the same name) seed `fuel` from the file's buffer size plus
    one — a deliberately loose but always-sufficient bound, since the file
    cannot contain more distinct objects than it has bytes, and each hop
    through the mutual group corresponds to resolving one more object or
    xref entry. On exhaustion (a cyclic/malicious file), the group throws a
    `corrupted` error rather than looping forever — a real totality
    treatment (an always-terminating total function with an explicit
    "too deep" error case), not a panic.

  - Upstream's `UnknownXRefStreamEntryType`-typed `catch` around
    `lookupEntryRec` (treating that one specific exception as "object not
    found", everything else propagating) is handled the same way
    `Data.PDF.Core.XRef` already does: `lookupEntryFueled`/
    `lookupEntryRecFueled` return `Except Nat (Option Entry)`/`Except Nat
    Entry` respectively — `findObject` pattern-matches the `.error` case
    directly (returning `Object.null`, exactly upstream's `catch` behaviour)
    instead of catching by exception type.

  - Upstream's `NotFound` is a dedicated `Exception` instance, needed so
    upstream's `catches`-style handling can distinguish it from
    `UnknownXRefStreamEntryType`. Since Lean's `IO.Error` has no open
    exception hierarchy to hang a second exception type off of (see
    `Data.PDF.Core.Exception`'s module doc-comment), "ref not found" is
    rendered as an ordinary `IO.Error.userError` tagged with a `"NotFound: "`
    prefix — the same closed-`IO.Error`, tag-in-the-message substitute
    already used throughout this port.

  - `withPdfFile` opens the handle via `IO.FS.Handle.mk` without an explicit
    close, matching this project's established idiom for short-lived file
    handles (e.g. `Linen/Network/Sendfile.lean`, `Linen/System/Log/
    FastLogger.lean`): Lean's `IO.FS.Handle` is closed by its finalizer when
    it becomes unreachable, and (per `Data.PDF.Core.IO.Buffer`'s own module
    doc-comment) the whole file is read eagerly into memory by `fromHandle`
    immediately anyway, so there is no lazy reading left pending on the
    handle by the time `action` runs.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.XRef
import Linen.Data.PDF.Core.Stream
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Core.IO.Buffer
import Linen.Data.PDF.Core.Encryption
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.File

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.Exception
open Data.PDF.Core.Util (notice)
open Data.PDF.Core.IO.Buffer (Buffer)

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── The `File` type ── -/

/-- A PDF file: its most recent cross reference, the byte buffer it was
    read from, the stream filters it knows how to decode, and (once set,
    via `setUserPassword`/`setDecryptor`) a decryptor for encrypted
    documents. Mirrors upstream's `File` record. -/
structure File where
  /-- The most recent (last, i.e. most up-to-date) cross reference. -/
  lastXRef : Data.PDF.Core.XRef.XRef
  /-- The byte buffer this file was read from. -/
  buffer : Buffer
  /-- The stream filters this file knows how to decode. -/
  filters : List Data.PDF.Core.Stream.StreamFilter
  /-- The decryptor to use for encrypted documents, once set. -/
  decryptorRef : IO.Ref (Option Data.PDF.Core.Encryption.Decryptor)

/-! ── Decryption ── -/

/-- Decrypt `o` (the already-read object at `ref`) if a decryptor is set,
    otherwise return it unchanged. Mirrors upstream's `decrypt`. -/
def decrypt (file : File) (ref : Ref) (o : Object) : IO Object := do
  match ← file.decryptorRef.get with
  | none => pure o
  | some decryptor => Data.PDF.Core.Encryption.decryptObject decryptor ref o

/-! ── Resolving objects (see the module doc-comment for the mutual
    recursion and its `fuel` bound) ── -/

mutual
  /-- Resolve `ref` to its `Object`, following the xref chain, decoding
      object-stream entries, and decrypting as needed. Mirrors the
      combination of upstream's `findObject`/`readObjectForEntry`. -/
  def resolveObject (file : File) : Nat → Ref → IO Object
    | 0, ref =>
      throw (corrupted "findObject"
        [s!"exceeded maximum indirect-reference resolution depth resolving {reprStr ref}"])
    | fuel + 1, ref => do
      match ← lookupEntryRecFueled file fuel ref file.lastXRef with
      | .error _ => pure .null
      | .ok entry =>
        match entry with
        | .free .. => pure .null
        | .used off gen => do
          let (ref', obj) ← Data.PDF.Core.Util.readObjectAtOffset file.buffer off
          unless ref'.generation == gen do
            throw (corrupted "readObjectForEntry" ["object generation missmatch"])
          decrypt file ref' obj
        | .compressed index num => do
          let objRef : Ref := { index := Int.ofNat index, generation := 0 }
          let obj ← resolveObject file fuel objRef
          let objStream ← sure (notice (streamValue obj) "Compressed entry should be in stream")
          let content ← streamContentFueled file fuel objRef objStream
          let first ← sure (notice (objStream.dict.get? (mkName "First") >>= intValue)
            "First should be an integer")
          Data.PDF.Core.Util.readCompressedObject content first.toNat num

  /-- Resolve a stream dictionary's `/Length` entry to a byte count,
      following an indirect reference if necessary. Mirrors upstream's
      inline `len <-` block in `rawStreamContent`. -/
  def resolveStreamLength (file : File) : Nat → Ref → Dict → IO Nat
    | 0, ref, _ =>
      throw (corrupted "rawStreamContent"
        [s!"exceeded maximum indirect-reference resolution depth resolving Length for {reprStr ref}"])
    | fuel + 1, _, dict => do
      let obj ← sure (notice (dict.get? (mkName "Length")) "Length missing in stream")
      match obj with
      | .number _ =>
        Int.toNat <$> sure (notice (intValue obj) "Length should be an integer")
      | .ref r => do
        let o ← resolveObject file fuel r
        Int.toNat <$> sure (notice (intValue o) "Length should be an integer")
      | _ => throw (corrupted "Length should be an integer")

  /-- A stream's still-encoded (undecoded, but already-decrypted) content.
      Mirrors upstream's `rawStreamContent`. -/
  def rawStreamContentFueled (file : File) :
      Nat → Ref → Object.Stream → IO Data.PDF.Stream.InputStream
    | 0, ref, _ =>
      throw (corrupted "rawStreamContent"
        [s!"exceeded maximum indirect-reference resolution depth for {reprStr ref}"])
    | fuel + 1, ref, s => do
      let len ← resolveStreamLength file fuel ref s.dict
      let raw ← Data.PDF.Core.Stream.rawStreamContent file.buffer len s.offset
      match ← file.decryptorRef.get with
      | none => pure raw
      | some decryptor => decryptor ref .stream raw

  /-- A stream's decrypted and decoded content. Mirrors upstream's
      `streamContent`. -/
  def streamContentFueled (file : File) :
      Nat → Ref → Object.Stream → IO Data.PDF.Stream.InputStream
    | 0, ref, _ =>
      throw (corrupted "streamContent"
        [s!"exceeded maximum indirect-reference resolution depth for {reprStr ref}"])
    | fuel + 1, ref, s => do
      let raw ← rawStreamContentFueled file fuel ref s
      Data.PDF.Core.Stream.decodeStream file.filters s raw

  /-- Look up `ref`'s entry directly in one cross reference (table or
      stream), without following `/Prev`. Mirrors upstream's `lookupEntry`. -/
  def lookupEntryFueled (file : File) :
      Nat → Ref → Data.PDF.Core.XRef.XRef → IO (Except Nat (Option Data.PDF.Core.XRef.Entry))
    | 0, ref, _ =>
      throw (corrupted "lookupEntry"
        [s!"exceeded maximum indirect-reference resolution depth for {reprStr ref}"])
    | fuel + 1, ref, xref =>
      match xref with
      | .table _ => Except.ok <$> Data.PDF.Core.XRef.lookupTableEntry file.buffer xref ref
      | .stream _ s => do
        -- Per PDF32000-1:2008 §7.5.8, cross-reference streams are never
        -- encrypted, so their content must be read raw and decoded without
        -- going through `streamContentFueled`/`rawStreamContentFueled`,
        -- which would decrypt using `ref` — the object being looked up,
        -- not the (untracked) xref stream's own object number.
        let len ← resolveStreamLength file fuel ref s.dict
        let raw ← Data.PDF.Core.Stream.rawStreamContent file.buffer len s.offset
        let content ← Data.PDF.Core.Stream.decodeStream file.filters s raw
        Data.PDF.Core.XRef.lookupStreamEntry s.dict content ref

  /-- Look up `ref`'s entry, following `/Prev` until it's found or the
      chain is exhausted. Mirrors upstream's `lookupEntryRec`. -/
  def lookupEntryRecFueled (file : File) :
      Nat → Ref → Data.PDF.Core.XRef.XRef → IO (Except Nat Data.PDF.Core.XRef.Entry)
    | 0, ref, _ =>
      throw (IO.Error.userError s!"NotFound: The Ref not found: {reprStr ref}")
    | fuel + 1, ref, xref => do
      match ← lookupEntryFueled file fuel ref xref with
      | .error n => pure (.error n)
      | .ok (some e) => pure (.ok e)
      | .ok none =>
        match ← Data.PDF.Core.XRef.prevXRef file.buffer xref with
        | some p => lookupEntryRecFueled file fuel ref p
        | none => throw (IO.Error.userError s!"NotFound: The Ref not found: {reprStr ref}")
end

/-! ── Public API ── -/

/-- Get an object with the specified ref. Returns `Object.null` if the ref
    is not in the xref table (an `UnknownXRefStreamEntryType`-equivalent
    situation, see the module doc-comment) — but re-throws (as a
    `"NotFound: "`-tagged `IO.Error`) if the ref genuinely can't be located
    anywhere in the xref chain. Mirrors upstream's `findObject`. -/
def findObject (file : File) (ref : Ref) : IO Object := do
  let fuel ← file.buffer.size
  resolveObject file (fuel + 1) ref

/-- Get the still-encoded content of a stream (decrypted, but not yet
    decoded by its named filters). Mirrors upstream's `rawStreamContent`. -/
def rawStreamContent (file : File) (ref : Ref) (s : Object.Stream) :
    IO Data.PDF.Stream.InputStream := do
  let fuel ← file.buffer.size
  rawStreamContentFueled file (fuel + 1) ref s

/-- Get the content of a stream, decrypted and decoded using this file's
    registered filters. Mirrors upstream's `streamContent`. -/
def streamContent (file : File) (ref : Ref) (s : Object.Stream) :
    IO Data.PDF.Stream.InputStream := do
  let fuel ← file.buffer.size
  streamContentFueled file (fuel + 1) ref s

/-- The last trailer: the entry point into the PDF file's object graph.
    Mirrors upstream's `lastTrailer`. -/
def lastTrailer (file : File) : IO Dict :=
  Data.PDF.Core.XRef.trailer file.buffer file.lastXRef

/-- Whether a PDF file is encrypted, already decrypted, or doesn't need
    decryption at all. Mirrors upstream's `EncryptionStatus`. -/
inductive EncryptionStatus where
  /-- The file is encrypted and requires decryption. -/
  | encrypted
  /-- The file is encrypted, and has already been decrypted (a decryptor
      is set). -/
  | decrypted
  /-- The file isn't encrypted at all. -/
  | plain
deriving BEq, Repr

/-- Get the encryption status. If it's `.encrypted`, `setUserPassword` may
    be used to decrypt it. Mirrors upstream's `encryptionStatus`. -/
def encryptionStatus (file : File) : IO EncryptionStatus := do
  let tr ← lastTrailer file
  match tr.get? (mkName "Encrypt") with
  | none => pure .plain
  | some _ =>
    match ← file.decryptorRef.get with
    | none => pure .encrypted
    | some _ => pure .decrypted

/-- Decrypt the file using the specified decryptor directly. Mirrors
    upstream's `setDecryptor`. -/
def setDecryptor (file : File) (decryptor : Data.PDF.Core.Encryption.Decryptor) : IO Unit :=
  file.decryptorRef.set (some decryptor)

/-- Set the user password to decrypt this PDF file. Use an empty `ByteArray`
    for the default password. Returns `true` on success (the password
    verified); `false` if it didn't. Mirrors upstream's `setUserPassword`. -/
def setUserPassword (file : File) (password : ByteArray) : IO Bool :=
  message "setUserPassword" do
    let tr ← lastTrailer file
    let enc ← do
      match tr.get? (mkName "Encrypt") with
      | none => throw (unexpected "document is not encrypted")
      | some o => do
        let o' ← deref o
        match o' with
        | .dictRaw entries => pure (Std.HashMap.ofList entries.toList)
        | .null => throw (corrupted "encryption encryption dict is null")
        | _ => throw (corrupted "document Encrypt should be a dictionary")
    let pass := Data.PDF.Core.Encryption.takeBytes 32
      (password ++ Data.PDF.Core.Encryption.defaultUserPassword)
    match Data.PDF.Core.Encryption.mkStandardDecryptor tr enc pass with
    | .error err => throw (corrupted err)
    | .ok none => pure false
    | .ok (some decryptor) => do
      setDecryptor file decryptor
      pure true
where
  deref (o : Object) : IO Object :=
    match o with
    | .ref r => findObject file r
    | other => pure other

/-! ── Opening a file ── -/

/-- Build a `File` from an already-read `Buffer`. Mirrors upstream's
    `fromBuffer`. -/
def fromBuffer (filters : List Data.PDF.Core.Stream.StreamFilter) (buffer : Buffer) : IO File := do
  let xref ← Data.PDF.Core.XRef.lastXRef buffer
  let decryptorRef ← IO.mkRef none
  pure { lastXRef := xref, buffer := buffer, filters := filters, decryptorRef := decryptorRef }

/-- Build a `File` from a binary handle. You may use
    `Data.PDF.Core.Stream.knownFilters` as the first argument. Mirrors
    upstream's `fromHandle`. -/
def fromHandle (filters : List Data.PDF.Core.Stream.StreamFilter) (h : IO.FS.Handle) :
    IO File := do
  let buffer ← Data.PDF.Core.IO.Buffer.fromHandle h
  fromBuffer filters buffer

/-- Build a `File` from an already-resident `ByteArray`. You may use
    `Data.PDF.Core.Stream.knownFilters` as the first argument. Mirrors
    upstream's `fromBytes`. -/
def fromBytes (filters : List Data.PDF.Core.Stream.StreamFilter) (bytes : ByteArray) :
    IO File := do
  let buffer ← Data.PDF.Core.IO.Buffer.fromBytes bytes
  fromBuffer filters buffer

/-- Open a PDF file and run `action` on it. You may want to check
    `encryptionStatus`/`setUserPassword` if the file turns out to be
    encrypted. Mirrors upstream's `withPdfFile` (see the module doc-comment
    for why no handle is explicitly closed). -/
def withPdfFile (path : System.FilePath) (action : File → IO α) : IO α := do
  let h ← IO.FS.Handle.mk path .read
  let file ← fromHandle Data.PDF.Core.Stream.knownFilters h
  action file

end Data.PDF.Core.File
