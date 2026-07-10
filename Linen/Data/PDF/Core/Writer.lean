/-
  Data.PDF.Core.Writer — write PDF files

  Ports `Pdf.Core.Writer` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Writer.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/core/lib/Pdf/Core/Writer.hs`),
  module 19 (the last) of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  Used to generate a new PDF file (`writeHeader`, then a number of
  `writeObject`/`writeStream`, then `writeXRefTable` or `writeXRefStream`),
  or to incrementally update an existing one (omit `writeHeader`; append the
  result to the existing file).

  ## Design

  - Upstream's `Writer` wraps an `IORef State` around a generic
    `System.IO.Streams.OutputStream ByteString` — `linen` has no `io-streams`
    port (nothing elsewhere in this import needed a *generic* byte sink; see
    `docs/imports/PdfToolboxCore/dependencies.md`'s external-dependency list),
    so per `AGENTS.md`'s stdlib-first precedence this substitutes the
    closest thing `linen` already has for "somewhere to put bytes as they're
    produced": an accumulating `Data.ByteString.Builder` behind an `IO.Ref`
    (`Builder`'s own O(1)-append design, `Linen/Data/ByteString/Builder.lean`,
    makes this cheap). `Writer.output`/`Writer.toBytes` below expose the
    accumulated bytes directly — the natural replacement for "already having
    written them to some external sink" that also happens to make this
    module directly testable without real file I/O.

  - Upstream's `State` keeps two separate counters, `stOffset` (a running
    total, updated by `countWritten`) and `stCount` (an `IO Int64` action
    from `System.IO.Streams.countOutput`, read and added into `stOffset` on
    every `countWritten` call). This only produces the right running byte
    position if `countOutput`'s action returns the number of bytes written
    **since it was last read** (resetting an internal counter each time) —
    a real but easy-to-miss detail of `io-streams`'s `countOutput`, not
    documented in its type. Since this port drives its own output directly
    (rather than wrapping an opaque `OutputStream`), it can track the
    running byte position with a single plain `Nat` field (`State.count`),
    updated by exactly the number of bytes each write actually adds —
    `countWritten` below is that field's current value, with no
    reconstruction of upstream's two-counters-that-must-agree scheme needed.

  - Upstream's `Elem`'s `Ord`/`Eq` instances (`compare \`on\` elemIndex`,
    `(==) \`on\` elemIndex`) back a `Set Elem` used purely as an
    index-keyed, duplicate-rejecting, sorted-by-index collection. This port
    uses a `Std.HashMap Int Elem` (keyed by `Elem.index`) for O(1) duplicate
    detection in `addElem`, sorting into ascending-by-index order (via
    `Array.qsort`) only where upstream's `Set.toAscList` is actually
    consulted (`buildXRefTable`/`buildXRefStream`'s callers) — observably
    identical to upstream's `Set Elem`, since a `Set` ordered by `elemIndex`
    alone is exactly "a duplicate-index-rejecting collection with an
    ascending-by-index view".

  - Upstream's `xrefSection [] = error "impossible"`/`buildXRefTableSection
    [] = error "impossible"`/`sectionIndex [] = error "impossible"` (three
    dead branches the type checker can't rule out, but which are never
    actually reached: `xrefSections` only ever calls `xrefSection`/consumes
    its result on a list it already knows is non-empty) get real totality
    treatment via a type that makes the "empty" case unrepresentable, per
    `AGENTS.md`'s "a proof or a type" guidance and the dependency doc's
    "Scope" note: `xrefSection`/`xrefSections` below represent a
    known-non-empty run of `Elem`s as `Elem × List Elem` (head, tail)
    instead of a bare `List Elem` — so `buildXRefTableSection`/
    `sectionIndex` (renamed `xrefSectionIndex` here) take that same
    `Elem × List Elem` shape and have no `[]` case to (not) handle at all.

  - `xrefSections`'s recursion (mirroring upstream's `xrefSections xs = let
    (s, rest) = xrefSection xs in s : xrefSections rest`) is genuinely
    terminating — `rest` is always strictly shorter than the list
    `xrefSections` was called on, since `xrefSection` always consumes at
    least the head element — but that fact isn't visible to Lean's
    structural-recursion checker without help. Per `AGENTS.md`'s ban on
    `partial`/fuel-as-a-dodge, `xrefSectionsGo_length_le` below proves the
    inner accumulator loop never grows the remaining tail, `xrefSection`'s
    corollary `xrefSection_snd_length_lt` sharpens that to a strict
    decrease of the *whole* list (head included), and `xrefSections` uses
    that lemma directly as its `termination_by`/`decreasing_by` measure —
    a real termination proof, not a fuel parameter (unlike, e.g.,
    `Data.PDF.Core.File`'s mutual group, which fuels a genuinely
    open-ended, IO-driven resolution chain that has no such static bound).

  - Upstream's `addElem` throwing a plain `error` on a duplicate index, and
    `writeXRefStream`'s `sectionIndex`, are the only two remaining places
    this module can still fail at runtime; both are ported as ordinary
    `throw`n `IO.Error`s (via `Data.PDF.Core.Exception.unexpected`), never a
    Lean `panic`/`sorry` — genuine caller-misuse guards (writing the same
    object index twice), not upstream totality gaps.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Builder
import Linen.Data.PDF.Core.Exception
import Linen.Data.ByteString.Builder
import Linen.Data.ByteString.Lazy
import Linen.Data.Scientific
import Std.Data.HashMap

namespace Data.PDF.Core.Writer

open Data.PDF.Core.Object
open Data.PDF.Core.Exception
open Data.ByteString (Builder)
open Data.ByteString.Builder

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── Tracked objects ── -/

/-- One tracked object for the cross-reference table/stream: its index and
    generation number, a byte offset (or, for a deleted/free object, the
    next free object's index — see `deleteObject`), and whether it's free.
    Mirrors upstream's `Elem`. -/
structure Elem where
  /-- The object's index. -/
  index : Int
  /-- The object's generation number. -/
  generation : Int
  /-- The object's byte offset (in-use), or the next free object's index
      (free). -/
  offset : Nat
  /-- Whether this entry marks a deleted (free) object. -/
  free : Bool
deriving BEq, Repr

/-! ── Internal state ── -/

/-- Internal writer state: the accumulated output, the tracked objects
    (keyed by index, for O(1) duplicate detection — see the module
    doc-comment), and the running byte count. -/
structure State where
  /-- The bytes written so far. -/
  output : Builder
  /-- Tracked objects, keyed by `Elem.index`. -/
  objects : Std.HashMap Int Elem
  /-- The running byte count (mirrors upstream's `countWritten`, folded
      into a single plain counter — see the module doc-comment). -/
  count : Nat

/-- A PDF file writer. Mirrors upstream's `Writer` (an opaque `IORef State`
    wrapper); see the module doc-comment for why the output sink is an
    accumulating `Builder` rather than a generic `OutputStream`. -/
structure Writer where
  private mk ::
  /-- The mutable internal state. -/
  ref : IO.Ref State

/-- Build a fresh `Writer`. Mirrors upstream's `makeWriter` (specialised
    away from its `OutputStream ByteString` argument — see the module
    doc-comment). -/
def makeWriter : IO Writer := do
  let ref ← IO.mkRef { output := Builder.empty, objects := {}, count := 0 }
  pure ⟨ref⟩

/-- The bytes written so far. -/
def Writer.output (w : Writer) : IO Data.ByteString.Lazy.LazyByteString := do
  pure (← w.ref.get).output.toLazyByteString

/-! ── Low-level state manipulation ── -/

/-- The current running byte count: the offset the *next* write will land
    at. Mirrors upstream's `countWritten`. -/
def countWritten (w : Writer) : IO Nat := do
  pure (← w.ref.get).count

/-- Append already-built bytes to the output, advancing the byte count by
    their length. -/
private def appendOutput (w : Writer) (bytes : Data.ByteString.Lazy.LazyByteString) : IO Unit :=
  w.ref.modify fun st =>
    { st with
      output := st.output ++ Builder.lazyByteString bytes
      count := st.count + bytes.length }

/-- Track `e`, failing if its index is already tracked. Mirrors upstream's
    `addElem` (an ordinary `throw`n error, not a panic — see the module
    doc-comment). -/
private def addElem (w : Writer) (e : Elem) : IO Unit := do
  let st ← w.ref.get
  if st.objects.contains e.index then
    throw (unexpected s!"Writer: attempt to write object with the same index: {e.index}")
  w.ref.set { st with objects := st.objects.insert e.index e }

/-- All tracked objects, ascending by index. Mirrors upstream's
    `Set.toAscList (stObjects st)`. -/
private def Writer.trackedAscending (w : Writer) : IO (List Elem) := do
  let st ← w.ref.get
  pure (st.objects.values.toArray.qsort (fun a b => a.index < b.index)).toList

/-! ── Public writing API ── -/

/-- Write the PDF header. Used for generating new PDF files; should be the
    first call, and is not used for incremental updates. Mirrors upstream's
    `writeHeader`. -/
def writeHeader (w : Writer) : IO Unit :=
  appendOutput w (stringUtf8 "%PDF-1.7\n").toLazyByteString

/-- Write an indirect object. Mirrors upstream's `writeObject`. -/
def writeObject (w : Writer) (ref : Ref) (obj : Object) : IO Unit := do
  let pos ← countWritten w
  addElem w { index := ref.index, generation := ref.generation, offset := pos, free := false }
  let built ← sure (Data.PDF.Core.Object.Builder.buildIndirectObject ref obj)
  appendOutput w built.toLazyByteString

/-- Write an indirect stream. Mirrors upstream's `writeStream`. -/
def writeStream (w : Writer) (ref : Ref) (dict : Dict)
    (dat : Data.ByteString.Lazy.LazyByteString) : IO Unit := do
  let pos ← countWritten w
  addElem w { index := ref.index, generation := ref.generation, offset := pos, free := false }
  let built ← sure (Data.PDF.Core.Object.Builder.buildIndirectStream ref dict dat)
  appendOutput w built.toLazyByteString

/-- Mark an object as deleted (free), pointing at the next free object's
    index (`0` to end the free-object chain). Mirrors upstream's
    `deleteObject`. -/
def deleteObject (w : Writer) (ref : Ref) (nextFree : Nat) : IO Unit :=
  addElem w { index := ref.index, generation := ref.generation, offset := nextFree, free := true }

/-! ── Cross-reference sections (see the module doc-comment for why the
    "empty section" dead branch is made unrepresentable by this shape) ── -/

/-- Extend a run of `Elem`s with contiguous ascending indices, starting
    immediately after `i`, from the front of `ys`. Returns the matched
    elements (in original order) and the unconsumed tail. -/
private def xrefSectionGo (i : Int) (acc : List Elem) : List Elem → List Elem × List Elem
  | [] => (acc.reverse, [])
  | y :: ys =>
    if i == y.index then xrefSectionGo (i + 1) (y :: acc) ys
    else (acc.reverse, y :: ys)

/-- `xrefSectionGo` never returns a tail longer than the list it was given. -/
private theorem xrefSectionGo_snd_length_le (i : Int) (acc : List Elem) (ys : List Elem) :
    (xrefSectionGo i acc ys).2.length ≤ ys.length := by
  induction ys generalizing i acc with
  | nil => simp [xrefSectionGo]
  | cons y ys ih =>
    simp only [xrefSectionGo]
    split
    · exact Nat.le_trans (ih (i + 1) (y :: acc)) (Nat.le_succ _)
    · simp

/-- Split off the first maximal run of `Elem`s with contiguous ascending
    indices from a non-empty ascending list `x :: xs`, represented as the
    head/tail pair `(x, xs)` — so the run returned is provably non-empty by
    construction (`x`, plus whatever of `xs` matched). Returns that run
    (again as a head/tail pair) together with the remaining tail. Mirrors
    upstream's `xrefSection`. -/
def xrefSection (x : Elem) (xs : List Elem) : (Elem × List Elem) × List Elem :=
  let (matched, rest) := xrefSectionGo (x.index + 1) [] xs
  ((x, matched), rest)

/-- `xrefSection`'s remaining tail is always strictly shorter than its full
    input `x :: xs`. -/
theorem xrefSection_snd_length_lt (x : Elem) (xs : List Elem) :
    (xrefSection x xs).2.length < (x :: xs).length := by
  simp only [xrefSection, List.length_cons]
  have := xrefSectionGo_snd_length_le (x.index + 1) [] xs
  omega

/-- Split an ascending list of `Elem`s into maximal runs of contiguous
    ascending indices, each represented as a non-empty head/tail pair (see
    `xrefSection`). Mirrors upstream's `xrefSections`. -/
def xrefSections : List Elem → List (Elem × List Elem)
  | [] => []
  | x :: xs => (xrefSection x xs).1 :: xrefSections (xrefSection x xs).2
termination_by xs => xs.length
decreasing_by exact xrefSection_snd_length_lt x xs

/-! ── Building xref tables/streams ── -/

/-- Left-pad (or, if too long, front-truncate — matching upstream's
    `take len $ show i` exactly, an edge case that never triggers for
    well-formed offsets/generations, which the PDF xref-table format itself
    bounds to 10/5 digits respectively) the decimal representation of `i` to
    exactly `len` characters with `c`. Mirrors upstream's `buildFixed`. -/
def buildFixed (len : Nat) (c : Char) (i : Nat) : Builder :=
  let v := ((toString i).take len).toString
  let pad := String.ofList (List.replicate (len - v.length) c)
  stringUtf8 (pad ++ v)

/-- Build one classic xref table section's entries (its `index count`
    header line, then one fixed-width line per entry). Mirrors upstream's
    `buildXRefTableSection` (total by construction — see the module
    doc-comment). -/
def buildXRefTableSection (sec : Elem × List Elem) : Builder :=
  let (hd, tl) := sec
  let entries := hd :: tl
  let header := intDec hd.index ++ char8 ' ' ++ intDec (Int.ofNat entries.length) ++ char8 '\n'
  let line (e : Elem) : Builder :=
    buildFixed 10 '0' e.offset ++ char8 ' ' ++
    buildFixed 5 '0' e.generation.toNat ++ char8 ' ' ++
    char8 (if e.free then 'f' else 'n') ++ stringUtf8 "\r\n"
  header ++ entries.foldl (init := Builder.empty) (fun acc e => acc ++ line e)

/-- Build a classic xref table's entries (everything after the `xref`
    keyword, before the `trailer` keyword). Mirrors upstream's
    `buildXRefTable`. -/
def buildXRefTable (entries : List Elem) : Builder :=
  (xrefSections entries).foldl (init := Builder.empty) (fun acc sec => acc ++ buildXRefTableSection sec)

/-- Build one xref-stream section's entries: one 17-byte record per tracked
    object (a 1-byte type field, then two 8-byte big-endian fields). Mirrors
    upstream's `buildXRefStreamSection`. -/
def buildXRefStreamSection (sec : Elem × List Elem) : Builder :=
  let (hd, tl) := sec
  let entry (e : Elem) : Builder :=
    if e.free then
      word8 0 ++ word64BE 0 ++ word64BE (UInt64.ofNat (e.generation.toNat + 1))
    else
      word8 1 ++ word64BE (UInt64.ofNat e.offset) ++ word64BE (UInt64.ofNat e.generation.toNat)
  (hd :: tl).foldl (init := Builder.empty) (fun acc e => acc ++ entry e)

/-- Build an xref stream's raw (undecoded) content. Mirrors upstream's
    `buildXRefStream`. -/
def buildXRefStream (entries : List Elem) : Builder :=
  (xrefSections entries).foldl (init := Builder.empty)
    (fun acc sec => acc ++ buildXRefStreamSection sec)

/-- The `/Index` array entries (`[firstIndex, count]` per section) for an
    xref stream's trailer dictionary. Mirrors upstream's `sectionIndex`
    (renamed here to avoid clashing with `xrefSection`; total by
    construction — see the module doc-comment). -/
def xrefSectionIndex (sec : Elem × List Elem) : List Int :=
  let (hd, tl) := sec
  [hd.index, Int.ofNat (tl.length + 1)]

/-! ── Public trailer-writing API ── -/

/-- Write a classic xref table plus trailer. Should be the last call; used
    both for generating new files and for incremental updates (only when
    the original PDF uses a classic xref table — use `writeXRefStream`
    otherwise). `offset` is the size of the original PDF file, `0` for a
    new file. Mirrors upstream's `writeXRefTable`. -/
def writeXRefTable (w : Writer) (offset : Nat) (tr : Dict) : IO Unit := do
  let off := (← countWritten w) + offset
  let elems := (← w.trackedAscending).map (fun e => { e with offset := e.offset + offset })
  let trailerBuilder ← sure (Data.PDF.Core.Object.Builder.buildDict tr)
  let content :=
    stringUtf8 "xref\n" ++ buildXRefTable elems ++
    stringUtf8 "trailer\n" ++ trailerBuilder ++
    stringUtf8 "\nstartxref\n" ++ intDec (Int.ofNat off) ++ stringUtf8 "\n%%EOF\n"
  appendOutput w content.toLazyByteString

/-- Write an xref stream plus trailer. Should be the last call; used both
    for generating new files and for incremental updates (only when the
    original PDF uses an xref stream — use `writeXRefTable` otherwise).
    `offset` is the size of the original PDF file, `0` for a new file. This
    updates/deletes the `/Type`, `/W`, `/Index`, `/Filter`, `/Length` keys
    of `tr`. Mirrors upstream's `writeXRefStream`. -/
def writeXRefStream (w : Writer) (offset : Nat) (ref : Ref) (tr : Dict) : IO Unit := do
  let pos ← countWritten w
  addElem w { index := ref.index, generation := ref.generation, offset := pos, free := false }
  let elems := (← w.trackedAscending).map (fun e => { e with offset := e.offset + offset })
  let off := pos + offset
  let content := (buildXRefStream elems).toLazyByteString
  let trIndex := (xrefSections elems).flatMap xrefSectionIndex
  let dict :=
    tr
    |>.insert (mkName "Type") (Object.name (mkName "XRef"))
    |>.insert (mkName "W") (Object.array #[Object.number 1, Object.number 8, Object.number 8])
    |>.insert (mkName "Index")
        (Object.array (trIndex.map (fun n => Object.number (Data.Scientific.fromInt n))).toArray)
    |>.insert (mkName "Length") (Object.number (Data.Scientific.fromInt (Int.ofNat content.length)))
    |>.erase (mkName "Filter")
  let streamBuilder ← sure (Data.PDF.Core.Object.Builder.buildIndirectStream ref dict content)
  let tail := stringUtf8 "\nstartxref\n" ++ intDec (Int.ofNat off) ++ stringUtf8 "\n%%EOF\n"
  appendOutput w streamBuilder.toLazyByteString
  appendOutput w tail.toLazyByteString

end Data.PDF.Core.Writer
