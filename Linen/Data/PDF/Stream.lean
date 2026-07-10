/-
  Data.PDF.Stream — buffer-resident `io-streams` input/output streams

  Ports the scoped slice of Hackage's `io-streams` package documented in
  `docs/imports/IoStreams/dependencies.md`: `System.IO.Streams.{Internal,Core,
  Combinators,List,ByteString,Attoparsec,Attoparsec.ByteString,Zlib}`. Every
  real call site in `pdf-toolbox-*` reads a fully-resident PDF file/buffer —
  never a genuinely unbounded/incremental network source — so this is a
  `ByteArray`-backed, cursor-plus-pushback abstraction rather than upstream's
  general lazy/incremental stream machinery.

  ## Design

  `InputStream`/`OutputStream` carry the same raw `_read`/`_unRead`/`_write`
  constructor fields as upstream's `System.IO.Streams.Internal` (rather than
  hiding them behind smart constructors only), because `Pdf.Core.IO.Buffer`
  (a later port) adapts its own cursor-based `Buffer` type directly to an
  `InputStream` by supplying these fields itself — see the dependency doc's
  note that `Buffer`'s cursor state will end up sharing/collapsing with this
  module's. This task only builds the stream abstraction itself, not that
  integration.

  Every constructor here (`fromByteString`, `fromList`, `makeInputStream`,
  `countInput`/`countOutput`, `takeBytes`, `decompress`) is expressed as a
  `mkRef`-backed producer/consumer closure. No `partial def` is needed: the
  only unbounded loop (`toList`, draining a stream to completion) is written
  as a `while` loop over local mutable state, matching this project's
  established idiom for unbounded IO reads (e.g. `Network.WebApp
  .strictRequestBody`) rather than upstream's `partial` recursive drain.
-/
import Linen.Crypto.Zlib.FFI
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Stream

-- ── Core types ──

/-- A pull-based, pushback-capable input stream. Mirrors upstream's
    `System.IO.Streams.Internal.InputStream(_read, _unRead)` constructor
    fields directly, so callers (e.g. a later `Pdf.Core.IO.Buffer` adapter)
    may build one from arbitrary IO actions of their own. -/
structure InputStream where
  /-- Pull the next chunk. `none` signals end-of-stream. Well-behaved
      producers never return `some` with an empty `ByteArray` (matching
      upstream's convention that streams never emit empty chunks). -/
  _read : IO (Option ByteArray)
  /-- Push a chunk back onto the stream, so that the *next* `_read` call
      returns exactly this chunk. -/
  _unRead : ByteArray → IO Unit

/-- A push-based output stream. Mirrors upstream's
    `System.IO.Streams.Internal.OutputStream(_write)` constructor field. -/
structure OutputStream where
  /-- Write one chunk to the underlying sink. -/
  _write : ByteArray → IO Unit

-- ── InputStream: reading ──

/-- Pull the next chunk from a stream, or `none` at end-of-stream.
    $$\text{read} : \text{InputStream} \to \text{IO}(\text{Option ByteArray})$$ -/
def read (s : InputStream) : IO (Option ByteArray) := s._read

/-- Push a chunk back onto a stream, so the next `read` returns it.
    $$\text{unRead} : \text{ByteArray} \to \text{InputStream} \to \text{IO Unit}$$ -/
def unRead (chunk : ByteArray) (s : InputStream) : IO Unit := s._unRead chunk

-- ── InputStream: construction ──

/-- Build an `InputStream` from a caller-supplied producer action. The
    resulting stream keeps its own pushback stack in front of `produce`, so
    `unRead` composes correctly no matter what `produce` does. -/
def makeInputStream (produce : IO (Option ByteArray)) : IO InputStream := do
  let pushback ← IO.mkRef ([] : List ByteArray)
  pure {
    _read := do
      match ← pushback.get with
      | c :: rest => pushback.set rest; pure (some c)
      | [] => produce
    _unRead := fun chunk => pushback.modify (chunk :: ·)
  }

/-- An `InputStream` that yields the entire `ByteArray` as a single chunk,
    then behaves as an already-exhausted stream. An empty `ByteArray`
    produces an immediately-exhausted stream (matching upstream's convention
    that streams never emit empty chunks). -/
def fromByteString (bytes : ByteArray) : IO InputStream := do
  let done ← IO.mkRef bytes.isEmpty
  makeInputStream do
    if ← done.get then
      pure none
    else
      done.set true
      pure (some bytes)

/-- An `InputStream` that yields each chunk of the given list in order (empty
    chunks are skipped, matching upstream's convention), then `none`. The
    remaining chunk list itself doubles as the stream's pushback stack. -/
def fromList (chunks : List ByteArray) : IO InputStream := do
  let remaining ← IO.mkRef (chunks.filter (·.size > 0))
  pure {
    _read := do
      match ← remaining.get with
      | [] => pure none
      | c :: rest => remaining.set rest; pure (some c)
    _unRead := fun chunk => remaining.modify (chunk :: ·)
  }

-- ── InputStream: consumption ──

/-- Drain a stream to completion, returning every chunk it yields, in order.
    Written as a `while` loop over local mutable state (no decreasing
    measure exists for an arbitrary producer action), matching this
    project's no-`partial` idiom for unbounded IO reads. -/
def toList (s : InputStream) : IO (List ByteArray) := do
  let mut acc : List ByteArray := []
  let mut done := false
  while !done do
    match ← read s with
    | some chunk => acc := chunk :: acc
    | none => done := true
  return acc.reverse

/-- Wrap a stream so that every byte pulled through the wrapper (net of any
    `unRead` pushback back through the wrapper) is tallied. Returns the
    wrapped stream together with an action reading the running total. -/
def countInput (s : InputStream) : IO (InputStream × IO Nat) := do
  let count ← IO.mkRef 0
  let wrapped : InputStream := {
    _read := do
      match ← read s with
      | some chunk => count.modify (· + chunk.size); pure (some chunk)
      | none => pure none
    _unRead := fun chunk => do
      count.modify (· - chunk.size)
      unRead chunk s
  }
  pure (wrapped, count.get)

/-- Wrap a stream so that at most `limit` bytes total are ever yielded;
    once the cap is reached, behaves as an exhausted stream regardless of
    what the underlying stream still has. Chunks that would cross the cap
    are truncated and the untaken tail is pushed back onto the *underlying*
    stream, so it is left positioned exactly after the taken prefix. -/
def takeBytes (limit : Nat) (s : InputStream) : IO InputStream := do
  let remaining ← IO.mkRef limit
  makeInputStream do
    if (← remaining.get) = 0 then
      pure none
    else
      match ← read s with
      | none => pure none
      | some chunk =>
        let cap ← remaining.get
        if chunk.size ≤ cap then
          remaining.set (cap - chunk.size)
          pure (some chunk)
        else
          let taken := chunk.extract 0 cap
          let rest := chunk.extract cap chunk.size
          unRead rest s
          remaining.set 0
          pure (some taken)

/-- Read exactly `n` bytes from a stream, concatenating chunks as needed.
    Throws if the stream is exhausted before `n` bytes have been read
    (matching upstream's `readExactly`, which raises `TooFewBytesReadException`
    on a short read). -/
def readExactly (n : Nat) (s : InputStream) : IO ByteArray := do
  let mut acc := ByteArray.empty
  let mut done := false
  while !done && acc.size < n do
    match ← read s with
    | some chunk => acc := acc ++ chunk
    | none => done := true
  if acc.size < n then
    throw (IO.userError
      s!"readExactly: needed {n} bytes, only {acc.size} were available")
  else if acc.size = n then
    pure acc
  else
    -- Overshot: push the surplus back so the stream resumes right after
    -- the requested prefix.
    unRead (acc.extract n acc.size) s
    pure (acc.extract 0 n)

-- ── OutputStream ──

/-- Write one chunk to an output stream.
    $$\text{write} : \text{ByteArray} \to \text{OutputStream} \to \text{IO Unit}$$ -/
def write (chunk : ByteArray) (s : OutputStream) : IO Unit := s._write chunk

/-- Write a UTF-8-encoded string to an output stream (upstream's
    `writeLazyByteString`, specialised to `ByteArray` since `linen` has no
    separate lazy `ByteString` representation to distinguish from the
    strict one here). -/
def writeLazyByteString (s : String) (out : OutputStream) : IO Unit :=
  write s.toUTF8 out

/-- Wrap an output stream so that every byte written through the wrapper is
    tallied. Returns the wrapped stream together with an action reading the
    running total. -/
def countOutput (s : OutputStream) : IO (OutputStream × IO Nat) := do
  let count ← IO.mkRef 0
  let wrapped : OutputStream := {
    _write := fun chunk => do
      count.modify (· + chunk.size)
      write chunk s
  }
  pure (wrapped, count.get)

-- ── Parsing directly against the resident buffer ──

/-- Run a `Std.Internal.Parsec.ByteArray.Parser` directly against an
    `InputStream`'s current resident buffer, starting at its current cursor,
    rather than incrementally feeding it chunk-by-chunk (sufficient because
    every stream here is buffer-resident by construction — see the module
    doc-comment). On success, the stream is left positioned exactly at the
    parser's post-consumption offset (any unconsumed suffix of the buffer is
    pushed back as a single chunk so the next `read` continues right where
    the parse left off). On failure, the stream's contents are left
    unchanged (via `unRead` of everything just pulled) and the parse error
    is thrown. -/
def parseFromStream (p : Std.Internal.Parsec.ByteArray.Parser α)
    (s : InputStream) : IO α := do
  -- The buffer-resident design means at most one nonempty chunk is ever
  -- pending; pull it (concatenating in the pathological case of several)
  -- so the parser sees one contiguous buffer.
  let mut buffer := ByteArray.empty
  let mut more := true
  while more do
    match ← read s with
    | some chunk => buffer := buffer ++ chunk
    | none => more := false
  match p ⟨buffer, 0⟩ with
  | .success it res =>
    if it.idx < buffer.size then
      unRead (buffer.extract it.idx buffer.size) s
    pure res
  | .error it err =>
    unRead buffer s
    throw (IO.userError s!"parseFromStream: offset {it.idx}: {err}")

-- ── Zlib decompression ──

/-- Wrap an `InputStream` to yield its zlib-inflated (`FlateDecode`) contents.
    The underlying stream is drained to completion up front and the whole
    compressed payload is inflated in one shot via `Crypto.Zlib.decompress`
    (sufficient since the source is already fully resident — see the module
    doc-comment); the result is then served through a fresh in-memory
    `InputStream`. -/
def decompress (s : InputStream) : IO InputStream := do
  let chunks ← toList s
  let compressed := chunks.foldl (· ++ ·) ByteArray.empty
  let inflated ← Crypto.Zlib.decompress compressed
  fromByteString inflated

end Data.PDF.Stream
