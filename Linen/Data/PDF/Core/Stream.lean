/-
  Data.PDF.Core.Stream — stream-related tools

  Ports `Pdf.Core.Stream` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Stream.hs`),
  module 13 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  **Not to be confused with `Data.PDF.Stream` (`Linen/Data/PDF/Stream.lean`)**
  — that earlier module is `linen`'s port of the scoped `io-streams` slice
  (`InputStream`/`OutputStream`, `decompress`, `parseFromStream`, etc.) that
  *this* module builds on; the two live in deliberately distinct namespaces
  (`Data.PDF.Stream` vs. `Data.PDF.Core.Stream`) precisely so this port,
  and later readers, never conflate upstream's `io-streams`-facing
  `Pdf.Core.Stream` with the lower-level stream abstraction it's built on.

  This module ties a stream's dictionary, its raw (still-filtered) data, and
  the filters named by `/Filter`/`/DecodeParms` together: `readStream`
  parses a `Stream` object out of an already-positioned input; `knownFilters`
  lists every filter this port implements (today, just `FlateDecode`);
  `rawStreamContent` reads a stream's still-encoded bytes given its length
  and offset; `decodeStream`/`buildFilterList` apply the named filter chain;
  `decodedStreamContent` composes raw-read, decryption, and decoding into
  one convenience call. -/
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Parsers.Object
import Linen.Data.PDF.Core.Stream.Filter.Type
import Linen.Data.PDF.Core.Stream.Filter.FlateDecode
import Linen.Data.PDF.Core.IO.Buffer
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.Stream

open Data.PDF.Core.Object Data.PDF.Core.Exception
open Data.PDF.Core.Stream.Filter.Type

export Data.PDF.Core.Stream.Filter.Type (StreamFilter)

/-- Build a `Name` from an internal ASCII literal known not to contain a
    `0x00` byte (mirrors `Stream.Filter.FlateDecode`'s private helper of the
    same name — kept local rather than shared, since each module's set of
    literal names is small and unrelated). -/
private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- Read a `Stream` object from an already-positioned input, given the
    current absolute byte offset (`off`) so the stream's data offset can be
    computed as `off` plus however many bytes the dictionary/`stream`
    keyword itself occupied. Mirrors upstream's `readStream`. -/
def readStream (is : Data.PDF.Stream.InputStream) (off : Nat) : IO Object.Stream := do
  let (is', counter) ← Data.PDF.Stream.countInput is
  let (_, obj) ←
    MonadExcept.tryCatch
      (Data.PDF.Stream.parseFromStream Data.PDF.Core.Parsers.Object.parseIndirectObject is')
      (fun e => match e with
        | .userError s => throw (corrupted s)
        | other => throw other)
  match obj with
  | .stream (.mk entries _) =>
    let off' ← counter
    pure (Object.Stream.mk entries (off + off'))
  | other => throw (IO.userError s!"stream expected, but got: {reprStr other}")

/-- Every stream filter this port implements. Upstream: "Right now it
    contains only FlateDecode filter." -/
def knownFilters : List StreamFilter :=
  [Data.PDF.Core.Stream.Filter.FlateDecode.flateDecode]

/-- A stream's raw content, filters not yet applied. Valid only until the
    next `Buffer.seek` on `buf` (the returned `InputStream` reads directly
    from `buf`'s cursor). `len` (the stream's `/Length`) must be supplied by
    the caller rather than read from the dictionary here, since `/Length`
    may itself be an indirect reference this module deliberately avoids
    resolving. Mirrors upstream's `rawStreamContent`. -/
def rawStreamContent (buf : Data.PDF.Core.IO.Buffer.Buffer) (len : Nat) (off : Nat) :
    IO Data.PDF.Stream.InputStream := do
  buf.seek off
  Data.PDF.Stream.takeBytes len (Data.PDF.Core.IO.Buffer.toInputStream buf)

/-- Build the ordered `(filterName, decodeParms)` list named by a stream
    dictionary's `/Filter` and `/DecodeParms` entries, handling every shape
    upstream does: absent (`[]`), a single `Name` (with `Dict`, a
    one-element `Array`, or no parms), or an `Array` of `Name`s paired with
    a matching `Array` of `Dict`s (or no parms at all). Mirrors upstream's
    `buildFilterList`. -/
def buildFilterList (dict : Dict) :
    IO (List (Data.PDF.Core.Name.Name × Option Dict)) := do
  let f := dict.getD (mkName "Filter") Object.null
  let p := dict.getD (mkName "DecodeParms") Object.null
  match f, p with
  | .null, _ => pure []
  | .name fd, .null => pure [(fd, none)]
  | .name fd, .dictRaw pdEntries => pure [(fd, some (Std.HashMap.ofList pdEntries.toList))]
  | .name fd, .array arr =>
    match arr.toList with
    | [.dictRaw pdEntries] => pure [(fd, some (Std.HashMap.ofList pdEntries.toList))]
    | _ => throw (corrupted s!"Can't handle Filter and DecodeParams: ({reprStr f}, {reprStr p})")
  | .array fa, .null => do
    let names ← fa.toList.mapM fun o =>
      match o with
      | .name n => pure n
      | _ => throw (corrupted "Filter should be a Name")
    pure (names.map (·, none))
  | .array fa, .array pa =>
    if fa.size == pa.size then do
      let names ← fa.toList.mapM fun o =>
        match o with
        | .name n => pure n
        | _ => throw (corrupted "Filter should be a Name")
      let parms ← pa.toList.mapM fun o =>
        match o with
        | .dictRaw entries => pure (some (Std.HashMap.ofList entries.toList))
        | _ => throw (corrupted "DecodeParams should be a dictionary")
      pure (names.zip parms)
    else
      throw (corrupted s!"Can't handle Filter and DecodeParams: ({reprStr f}, {reprStr p})")
  | _, _ => throw (corrupted s!"Can't handle Filter and DecodeParams: ({reprStr f}, {reprStr p})")

/-- Apply a stream's named filter chain (in order) to its already-decrypted
    content, given the list of filters this port knows about. Mirrors
    upstream's `decodeStream`. -/
def decodeStream (filters : List StreamFilter) (s : Object.Stream)
    (istream : Data.PDF.Stream.InputStream) : IO Data.PDF.Stream.InputStream := do
  let entries ← buildFilterList s.dict
  entries.foldlM
    (fun is (name, parms) => do
      match filters.find? (·.filterName == name) with
      | none => throw (corrupted "Filter not found")
      | some f => f.filterDecode parms is)
    istream

/-- Read, decrypt, and decode a stream's content in one call. Mirrors
    upstream's `decodedStreamContent`; note (as upstream does) that
    `/Length` must be supplied by the caller for the same reason as
    `rawStreamContent`. -/
def decodedStreamContent (buf : Data.PDF.Core.IO.Buffer.Buffer) (filters : List StreamFilter)
    (decryptor : Data.PDF.Stream.InputStream → IO Data.PDF.Stream.InputStream) (len : Nat)
    (s : Object.Stream) : IO Data.PDF.Stream.InputStream := do
  let raw ← rawStreamContent buf len s.offset
  let decrypted ← decryptor raw
  decodeStream filters s decrypted

end Data.PDF.Core.Stream
