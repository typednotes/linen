/-
  Data.PDF.Core.IO.Buffer — a cursor-based file/byte-source abstraction

  Ports `Pdf.Core.IO.Buffer` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/IO/Buffer.hs`),
  module 4 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  A `Buffer` abstracts a file's contents behind five primitives: sequential
  `read`, `size`, absolute `seek`, relative `back` (rewind), and `tell`
  (current position) — exactly upstream's five-field record.

  ## Design

  Per the dependency doc, `Buffer` adapts directly onto
  `Data.PDF.Stream.InputStream` (`Linen/Data/PDF/Stream.lean`) rather than
  going through upstream's separate `io-streams` `InputStream ByteString`
  type: `toInputStream` below builds a `Data.PDF.Stream.InputStream` by
  supplying its `_read`/`_unRead` fields directly from a `Buffer`'s own
  `read`/`back` — precisely the adaptation `Data.PDF.Stream`'s own
  doc-comment anticipates.

  Upstream's fields are typed over `Int64` (a fixed 64-bit signed integer);
  Lean has no built-in fixed-width signed-integer type, and every quantity
  here (a byte offset, a byte count, a file size) is inherently
  non-negative, so `Nat` is used throughout — a strictly more precise
  substitute (no silent 64-bit wraparound) rather than a weakening.

  `back`'s `Nat` subtraction saturates at `0` rather than going negative;
  this only matters if a caller calls `back` with more bytes than have been
  read so far, which is a caller bug either way (upstream's `Int64` version
  would simply produce a negative, equally-nonsensical, cursor position).

  `fromHandle` reads the whole file eagerly via `IO.FS.Handle.readBinToEnd`
  and delegates to `fromBytes`, rather than upstream's true lazy/seekable
  handle-backed `Buffer`: Lean's `IO.FS.Handle` (`Init.System.IO`) exposes no
  `seek`/`tell`/file-size primitives (only sequential `read`/`write` plus
  `rewind`, i.e. seek-to-0) to build a genuinely cursor-based reader on top
  of, and — per `Data.PDF.Stream`'s own module doc-comment — every real
  `pdf-toolbox-*` call site already reads a fully buffer-resident PDF file,
  so full residency here is consistent with that established project idiom,
  not a new shortcut.
-/
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.IO.Buffer

/-- Interface to a file/byte source: sequential `read`, `size`, absolute
    `seek`, relative `back` (rewind by `n` bytes), and `tell` (current
    position). Mirrors upstream's `Buffer` record exactly, field for field. -/
structure Buffer where
  /-- Read the next chunk, or `none` at end-of-source. -/
  read : IO (Option ByteArray)
  /-- The source's total size in bytes. -/
  size : IO Nat
  /-- Move the cursor to an absolute byte offset. -/
  seek : Nat → IO Unit
  /-- Move the cursor back by `n` bytes (saturating at `0`; see the module
      doc-comment). -/
  back : Nat → IO Unit
  /-- The cursor's current byte offset. -/
  tell : IO Nat

/-- Adapt a `Buffer` directly onto a `Data.PDF.Stream.InputStream`, by
    supplying `InputStream`'s own `_read`/`_unRead` fields from `Buffer`'s
    `read`/`back`: pushing a chunk back onto the stream is exactly rewinding
    the buffer's cursor by that chunk's length. -/
def toInputStream (buf : Buffer) : Data.PDF.Stream.InputStream :=
  { _read := buf.read
    _unRead := fun chunk => buf.back chunk.size }

/-- A `Buffer` over a resident `ByteArray`. Each `read` returns *all*
    remaining bytes in one chunk (matching upstream's `fromBytes` exactly —
    unlike `fromHandle`, which upstream chunks at `defaultSize = 32752`
    bytes per `read`; that chunking has no counterpart here since
    `fromHandle` below is itself implemented via `fromBytes`, see the
    module doc-comment). -/
def fromBytes (bytes : ByteArray) : IO Buffer := do
  let pos ← IO.mkRef 0
  pure {
    read := do
      let p ← pos.get
      let chunk := bytes.extract p bytes.size
      pos.set (p + chunk.size)
      if chunk.size == 0 then pure none else pure (some chunk)
    seek := fun p => pos.set p
    size := pure bytes.size
    back := fun n => pos.modify (· - n)
    tell := pos.get
  }

/-- A `Buffer` over a file's entire contents, read eagerly up front (see the
    module doc-comment for why full residency replaces upstream's seekable
    `Handle`-backed reader). -/
def fromHandle (h : IO.FS.Handle) : IO Buffer := do
  fromBytes (← h.readBinToEnd)

/-- Drop exactly `n` bytes from an input stream (discarding them).
    Mirrors upstream's `dropExactly = void . Streams.readExactly n`. -/
def dropExactly (n : Nat) (s : Data.PDF.Stream.InputStream) : IO Unit :=
  Functor.discard (Data.PDF.Stream.readExactly n s)

end Data.PDF.Core.IO.Buffer
