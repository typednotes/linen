/-
  Data.PDF.Core.XRef — the cross-reference index (PDF32000-1:2008 §7.5.4/§7.5.8)

  Ports `Pdf.Core.XRef` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/XRef.hs`),
  module 14 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  A PDF's cross-reference index maps indirect-object refs to where their
  object data lives: either the byte offset of an in-use object, a marker
  that the object number is currently free, or (for objects stored inside an
  object stream, §7.5.7) the object-stream's own ref and the object's index
  within it. That index is built either from a classic xref *table*
  (`XRef.table`, plain text rows) or an xref *stream* (`XRef.stream`, a
  compressed binary table stored as PDF stream data) — a PDF may chain
  several of either kind together via each trailer's `/Prev` entry (one per
  incremental update); `Data.PDF.Core.File` (a later port) walks that chain.

  ## Dropped debug residue

  Per the dependency doc's "Scope" note, upstream's `lookupTableEntry`
  contains a stray `print (index, gen, off, gen', free)` immediately before
  throwing on a generation mismatch — debug residue with no documented
  contract, not reproduced here (the throw itself is kept).

  ## The "impossible" `collect` branch

  Upstream's `lookupStreamEntry` decodes the three big-endian integer fields
  of one xref-stream row from a fixed-width byte string via a hand-rolled
  `collect`/`conv` pair, whose `collect` has a wildcard
  `error "readStreamEntry: collect: impossible"` case, reachable per GHC's
  checker but not per the surrounding invariant (`totalWidth = w1 + w2 + w3`
  and exactly `totalWidth` bytes were read, so the three widths always
  partition the byte string exactly). Rather than reproduce that
  materialize-then-assert-nonempty shape, this port slices the byte list
  directly with three `take`/`drop` calls (each total on any list, including
  a too-short one) and folds each slice into a big-endian `Nat` — the
  partition is total by construction, so there is no "this can't happen"
  case to (not) guard at all, following the precedent set by
  `Data.PDF.Core.Util.parseHeaderUpTo`.

  ## `UnknownXRefStreamEntryType` as a typed result, not a typed exception

  Upstream's `lookupStreamEntry` also `throwIO`s a dedicated
  `UnknownXRefStreamEntryType` exception on an unrecognized entry-type tag,
  which `Data.PDF.File.findObject` specifically `catch`es (by *type*) to
  treat as a `null` object rather than a hard failure. Lean's `IO.Error` has
  no open, `Typeable`-indexed exception hierarchy to hang a same-shaped
  `catch` off of (see `Data.PDF.Core.Exception`'s module doc-comment), so
  `lookupStreamEntry` instead returns `Except Nat (Option Entry)` directly:
  `.error n` is exactly upstream's `UnknownXRefStreamEntryType n`, made a
  first-class, statically-checked return value instead of an exception a
  caller must know to catch by type.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Parsers.XRef
import Linen.Data.PDF.Core.Stream
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Core.IO.Buffer
import Linen.Data.PDF.Core.Name
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.XRef

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.Exception
open Data.PDF.Core.Util (notice)
open Data.PDF.Core.Parsers.XRef
open Data.PDF.Core.IO.Buffer (Buffer)

/-- Build a `Name` from an internal ASCII literal known not to contain a
    `0x00` byte (same local convention as `Data.PDF.Core.Stream`/
    `Stream.Filter.FlateDecode`). -/
private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

-- ── Entries and cross references ──

/-- One entry of a cross-reference table/stream (PDF32000-1:2008 §7.5.4/
    §7.5.8). Mirrors upstream's `Entry`; every field that upstream types as
    `Int`/`Int64` is `Nat` here (an object number, index, or byte offset is
    never negative — the same substitution already used throughout this
    port, e.g. `Data.PDF.Core.IO.Buffer`), except each entry's `generation`,
    kept as `Int` to match `Ref.generation`. -/
inductive Entry where
  /-- A free object slot: the object number of the *next* free object in
      the free list (not a byte offset, despite occupying the same table
      column as `used`'s offset — PDF32000-1:2008 §7.5.4), and the
      generation number to use should this object number be reused. -/
  | free (nextFreeObject : Nat) (generation : Int)
  /-- An in-use object: its byte offset from the start of the file, and its
      generation number. -/
  | used (offset : Nat) (generation : Int)
  /-- An object stored inside an object stream: that stream's object
      number, and this object's index within it. -/
  | compressed (streamObjectNumber : Nat) (indexInStream : Nat)
deriving BEq, Repr

/-- A cross reference, either a classic table or an xref stream. Mirrors
    upstream's `XRef`; the `Int64` offset is `Nat` throughout, as elsewhere
    in this port. -/
inductive XRef where
  /-- A classic xref table, at the given byte offset. -/
  | table (offset : Nat)
  /-- An xref stream, at the given byte offset, together with the already-
      read `Stream` object itself. -/
  | stream (offset : Nat) (s : Object.Stream)
deriving BEq, Repr

-- ── Table detection ──

/-- Check whether the stream is currently positioned at an `"xref"`
    keyword. On success, that keyword and the newline after it are
    consumed (mirrors upstream's `tableXRef` side effect); on failure, the
    stream is left unchanged (mirrors upstream's `catch`-and-return-`False`
    around the underlying `Streams.parseFromStream`, which itself restores
    the stream's contents on parse failure — see
    `Data.PDF.Stream.parseFromStream`). -/
def isTable (is : Data.PDF.Stream.InputStream) : IO Bool :=
  MonadExcept.tryCatch
    (do let _ ← Data.PDF.Stream.parseFromStream tableXRef is
        pure true)
    (fun e => match e with
      | .userError _ => pure false
      | other => throw other)

-- ── Locating and reading a cross reference ──

/-- Read the `XRef` located at absolute byte offset `off`: seeks there, then
    checks whether it's a table (`XRefTable`) or, failing that, reads it as
    an xref stream object (`XRefStream`). Mirrors upstream's `readXRef`. -/
def readXRef (buf : Buffer) (off : Nat) : IO XRef := do
  buf.seek off
  let is := Data.PDF.Core.IO.Buffer.toInputStream buf
  if ← isTable is then
    pure (.table off)
  else
    let s ← Data.PDF.Core.Stream.readStream is off
    pure (.stream off s)

/-- Find the last (most recent) cross reference in the file, by seeking to
    the last ~1KB and locating the final `startxref ... %%EOF` marker there
    (see `Data.PDF.Core.Parsers.XRef.startXRef`'s doc-comment for why only
    the file's tail need be scanned). Mirrors upstream's `lastXRef`. -/
def lastXRef (buf : Buffer) : IO XRef := do
  let sz ← buf.size
  -- Upstream: `Buffer.seek buf $ max 0 (sz - 1024)`; `Nat` subtraction
  -- already saturates at `0`, so no explicit `max` is needed.
  buf.seek (sz - 1024)
  let off ←
    MonadExcept.tryCatch
      (Data.PDF.Stream.parseFromStream startXRef (Data.PDF.Core.IO.Buffer.toInputStream buf))
      (fun e => match e with
        | .userError msg => throw (corrupted "lastXRef" [msg])
        | other => throw other)
  readXRef buf off

/-- Read the trailer dictionary for a cross reference: for a table, skip
    past every subsection to the `trailer` keyword and parse the dictionary
    after it; for a stream, the stream's own dictionary already *is* the
    trailer. Mirrors upstream's `trailer`. -/
def trailer (buf : Buffer) (xref : XRef) : IO Dict := do
  match xref with
  | .stream _ s => pure s.dict
  | .table off => do
    buf.seek off
    let is := Data.PDF.Core.IO.Buffer.toInputStream buf
    unless (← isTable is) do
      throw (unexpected "trailer" ["table not found"])
    MonadExcept.tryCatch
      (do skipTable buf is
          Data.PDF.Stream.parseFromStream parseTrailerAfterTable is)
      (fun e => match e with
        | .userError msg => throw (corrupted "trailer" [msg])
        | other => throw other)
where
  /-- Parse one subsection header, wrapping any parse failure as
      `corrupted`. Mirrors upstream's un-caught `subsectionHeader`, called
      either directly (here) or via `nextSubsectionHeader`'s own local
      catch. -/
  subsectionHeader (is : Data.PDF.Stream.InputStream) : IO (Nat × Nat) :=
    MonadExcept.tryCatch
      (Data.PDF.Stream.parseFromStream parseSubsectionHeader is)
      (fun e => match e with
        | .userError msg => throw (corrupted msg)
        | other => throw other)
  /-- Drop `count * 20` bytes: one xref-table subsection's worth of
      20-byte-per-row entries. Mirrors upstream's `skipSubsection`. -/
  skipSubsection (is : Data.PDF.Stream.InputStream) (count : Nat) : IO Unit :=
    Data.PDF.Core.IO.Buffer.dropExactly (count * 20) is
  /-- Skip one subsection, then try to parse the next subsection's header;
      `none` (rather than propagating the parse failure) signals that there
      is no further subsection. Mirrors upstream's `nextSubsectionHeader`. -/
  nextSubsectionHeader (is : Data.PDF.Stream.InputStream) (count : Nat) :
      IO (Option (Nat × Nat)) :=
    message "nextSubsectionHeader" do
      skipSubsection is count
      MonadExcept.tryCatch
        (some <$> subsectionHeader is)
        (fun e => match e with
          | .userError _ => pure none
          | other => throw other)
  /-- Repeatedly skip subsections until `nextSubsectionHeader` reports none
      remain. `fuel`, seeded from the buffer's total byte size, bounds the
      loop: each iteration consumes at least the bytes of one subsection
      header, so the loop can never need more iterations than the buffer
      has bytes (a deliberately loose but always-sufficient bound, the same
      style of argument used by `Parsers.XRef.collectStartXRefs`). -/
  skipTableLoop : Nat → Data.PDF.Stream.InputStream → Nat → IO Unit
    | 0, _, _ => pure ()
    | fuel + 1, is, count => do
      match ← nextSubsectionHeader is count with
      | none => pure ()
      | some (_, count') => skipTableLoop fuel is count'
  /-- Skip past every subsection of an xref table, leaving the input
      positioned right at the `trailer` keyword. Mirrors upstream's
      `skipTable`. -/
  skipTable (buf : Buffer) (is : Data.PDF.Stream.InputStream) : IO Unit :=
    message "skipTable" do
      let (_, count) ← subsectionHeader is
      let fuel ← buf.size
      skipTableLoop fuel is count

/-- Find the previous (older) cross reference in the chain, following the
    current one's trailer `/Prev` entry, if any. Mirrors upstream's
    `prevXRef`. -/
def prevXRef (buf : Buffer) (xref : XRef) : IO (Option XRef) :=
  message "prevXRef" do
    let tr ← trailer buf xref
    match tr.get? (mkName "Prev") with
    | none => pure none
    | some prevObj => do
      let off ← sure (notice (intValue prevObj) "Prev in trailer should be an integer")
      some <$> readXRef buf off.toNat

-- ── Looking up one ref's entry ──

/-- Read one xref-table row (`offset generation char`), wrapping any parse
    failure as `corrupted`. Shared by `lookupTableEntry`'s row lookup below. -/
private def parseRow (is : Data.PDF.Stream.InputStream) : IO (Nat × Nat × Bool) :=
  MonadExcept.tryCatch
    (Data.PDF.Stream.parseFromStream parseTableEntry is)
    (fun e => match e with
      | .userError msg => throw (corrupted "parseTableEntry failed" [msg])
      | other => throw other)

/-- Walk an xref table's subsections looking for the one containing
    `index`, then read and validate that row against `gen`. `fuel`, seeded
    from the buffer's total byte size, bounds the walk for the same reason
    as `trailer.skipTableLoop`: each iteration consumes at least one
    subsection header's worth of bytes. Mirrors upstream's local `go`
    helper inside `lookupTableEntry` (the stray `print` issued just before
    throwing on a generation mismatch is dropped — see the module
    doc-comment; the throw itself is kept). -/
private def lookupTableEntryGo (buf : Buffer) (index gen : Int) :
    Nat → Data.PDF.Stream.InputStream → Nat × Nat → IO (Option Entry)
  | 0, _, _ => pure none
  | fuel + 1, is, (start, count) => do
    if index ≥ Int.ofNat start ∧ index < Int.ofNat (start + count) then do
      let pos ← buf.tell
      buf.seek (pos + (index - Int.ofNat start).toNat * 20)
      let (off, gen', free) ← parseRow (Data.PDF.Core.IO.Buffer.toInputStream buf)
      if free ∨ gen == Int.ofNat gen' then
        pure (some (if free then .free off gen else .used off gen))
      else
        throw (corrupted "Generation mismatch")
    else do
      skipSubsectionFor is count
      let next ←
        MonadExcept.tryCatch
          (some <$> parseSubsectionHeaderIO is)
          (fun e => match e with
            | .userError _ => pure none
            | other => throw other)
      match next with
      | none => pure none
      | some next' => lookupTableEntryGo buf index gen fuel is next'
where
  /-- Drop one subsection's worth of 20-byte rows, mirroring `trailer`'s
      local `skipSubsection`. -/
  skipSubsectionFor (is : Data.PDF.Stream.InputStream) (count : Nat) : IO Unit :=
    Data.PDF.Core.IO.Buffer.dropExactly (count * 20) is
  /-- Parse one subsection header, mirroring `trailer`'s local
      `subsectionHeader` (kept separate since that one is private to
      `trailer`). -/
  parseSubsectionHeaderIO (is : Data.PDF.Stream.InputStream) : IO (Nat × Nat) :=
    Data.PDF.Stream.parseFromStream parseSubsectionHeader is

/-- Read the cross-reference entry for `ref` out of an xref *table* (fails
    on an xref stream — see `lookupStreamEntry` for that case). Walks the
    table's subsections, seeking directly to the relevant 20-byte row once
    the containing subsection is found. Mirrors upstream's
    `lookupTableEntry`. -/
def lookupTableEntry (buf : Buffer) (xref : XRef) (ref : Ref) : IO (Option Entry) :=
  message "lookupTableEntry" do
    match xref with
    | .stream .. => throw (unexpected "lookupTableEntry" ["Only xref table allowed"])
    | .table tableOff => do
      buf.seek tableOff
      let is := Data.PDF.Core.IO.Buffer.toInputStream buf
      unless (← isTable is) do
        throw (unexpected "Not a table")
      let fuel ← buf.size
      MonadExcept.tryCatch
        (do
          let header ← Data.PDF.Stream.parseFromStream parseSubsectionHeader is
          lookupTableEntryGo buf ref.index ref.generation fuel is header)
        (fun e => match e with
          | .userError err => throw (corrupted err)
          | other => throw other)

-- ── Looking up one ref's entry in an xref stream ──

/-- Big-endian-decode a fixed-width byte string into a `Nat` (each of an
    xref-stream row's three fields, per PDF32000-1:2008 §7.5.8 Table 18). -/
private def bigEndianNat (bytes : List UInt8) : Nat :=
  bytes.foldl (fun acc b => acc * 256 + b.toNat) 0

/-- Parse a stream dictionary's `/Index` entry into `(from, count)` pairs
    (§7.5.8 Table 17), defaulting to a single `(0, size)` range covering
    every object number when `/Index` is absent. Mirrors upstream's inline
    `convertIndex`. -/
private def parseIndexRanges (dict : Dict) (size : Nat) : Except String (List (Nat × Nat)) :=
  match dict.get? (mkName "Index") with
  | none => .ok [(0, size)]
  | some (.array arr) => convert arr.toList
  | some _ => .error "Index should be an array"
where
  /-- Convert a flat `[from₁, count₁, from₂, count₂, …]` object list into
      `(from, count)` pairs. Structurally recursive on the list, consuming
      two elements per step. -/
  convert : List Object → Except String (List (Nat × Nat))
    | [] => .ok []
    | x1 :: x2 :: xs => do
      let from_ ← notice (intValue x1) "from index should be an integer"
      let count ← notice (intValue x2) "count should be an integer"
      let rest ← convert xs
      .ok ((from_.toNat, count.toNat) :: rest)
    | _ => .error "Malformed Index in xref stream"

/-- Parse a stream dictionary's `/W` entry into its three field widths
    (§7.5.8 Table 17: entry type, object number/offset, generation/index). -/
private def parseWidths (dict : Dict) : Except String (Nat × Nat × Nat) := do
  let ws ←
    match dict.get? (mkName "W") with
    | some (.array arr) => arr.toList.mapM (fun o => notice (intValue o) "W should contains integers")
    | _ => .error "W should be an array"
  match ws with
  | [a, b, c] => .ok (a.toNat, b.toNat, c.toNat)
  | _ => .error "Malformed W array in xref stream"

/-- Locate the byte offset (within the decoded xref-stream data, before the
    per-row `totalWidth` fields) of the row for `objNumber`, given the
    `/Index` ranges and the combined row width. Mirrors upstream's inline
    `loop`. Structurally recursive on the ranges list. -/
private def findRowOffset (totalWidth objNumber : Nat) : Nat → List (Nat × Nat) → Option Nat
  | _, [] => none
  | pos, (from_, count) :: rest =>
    if objNumber < from_ ∨ objNumber ≥ from_ + count then
      findRowOffset totalWidth objNumber (pos + totalWidth * count) rest
    else
      some (pos + totalWidth * (objNumber - from_))

/-- Read the cross-reference entry for `ref` out of an already-decoded xref
    *stream*'s content (§7.5.8), given that stream's own dictionary.
    Mirrors upstream's `lookupStreamEntry`; see the module doc-comment for
    why an unrecognized entry-type tag surfaces as `Except.error` (upstream's
    typed `UnknownXRefStreamEntryType` exception) rather than a thrown
    error, and for how the "impossible" `collect` case is avoided by
    construction via `List.take`/`List.drop` rather than reproduced. -/
def lookupStreamEntry (dict : Dict) (is : Data.PDF.Stream.InputStream) (ref : Ref) :
    IO (Except Nat (Option Entry)) :=
  message "lookupStreamEntry" do
    let size ←
      sure (notice (dict.get? (mkName "Size") >>= intValue) "Size should be an integer")
    let indexRanges ← sure (parseIndexRanges dict size.toNat)
    let (w1, w2, w3) ← sure (parseWidths dict)
    let totalWidth := w1 + w2 + w3
    let objNumber := ref.index.toNat
    match findRowOffset totalWidth objNumber 0 indexRanges with
    | none => pure (.ok none)
    | some p => do
      Data.PDF.Core.IO.Buffer.dropExactly p is
      let bytes ← (Data.PDF.Stream.readExactly totalWidth is)
      let bs := bytes.toList
      let g1 := bs.take w1
      let g2 := (bs.drop w1).take w2
      let g3 := (bs.drop (w1 + w2)).take w3
      let v1 := bigEndianNat g1
      let v2 := bigEndianNat g2
      let v3 := bigEndianNat g3
      match v1 with
      | 0 => pure (.ok (some (.free v2 (Int.ofNat v3))))
      | 1 => pure (.ok (some (.used v2 (Int.ofNat v3))))
      | 2 => pure (.ok (some (.compressed v2 v3)))
      | n => pure (.error n)
