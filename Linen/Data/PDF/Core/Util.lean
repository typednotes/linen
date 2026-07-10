/-
  Data.PDF.Core.Util — unclassified PDF-parsing tools

  Ports `Pdf.Core.Util` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Util.hs`),
  module 10 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  ## The "impossible" dead branch

  Upstream's `readCompressedObject` parses `num + 1` `(objNum, offset)`
  header pairs via `replicateM (num + 1) headerP`, then guards
  `when (null res) $ error "readCompressedObject: imposible"` before taking
  `last res` — a runtime-checked dead branch flagged by
  `docs/imports/PdfToolboxCore/dependencies.md`'s "Scope" note as needing
  real termination/totality treatment rather than a literal panic, since
  `replicateM (num + 1)` can never produce a list shorter than `num + 1 ≥ 1`.

  Rather than materialize a `List`/`Array` of headers and inspect whether
  it's empty (which would need that same "impossible, but the type doesn't
  say so" guard to extract the last element), `parseHeaderUpTo` below
  parses exactly `n + 1` headers by direct structural recursion on `n` and
  returns the *last* one directly, as an `α` rather than a `List α`/
  `Option α` — so the "the list has at least one element" fact is baked
  into the function's type/shape and is never a runtime-checked branch at
  all. -/
import Linen.Data.PDF.Stream
import Linen.Data.PDF.Core.IO.Buffer
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Parsers.Object
import Linen.Data.PDF.Core.Parsers.Util
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Core.Util

open Data.PDF.Core.Object Data.PDF.Core.Exception
open Std.Internal.Parsec Std.Internal.Parsec.ByteArray
open Data.PDF.Core.Parsers.Util

/-- Add a message to a `none`. Mirrors upstream's
    `notice :: Maybe a -> String -> Either String a`. -/
def notice (o : Option α) (msg : String) : Except String α :=
  match o with
  | some a => .ok a
  | none => .error msg

/-- Read the indirect object located at byte offset `off` in `buf`. Returns
    the object's `Ref` together with the object itself; if the object is a
    stream, its payload offset is updated to the buffer's current position
    (i.e. right after the `stream` keyword's end-of-line) rather than the
    placeholder `0` that `parseIndirectObject` leaves it at. Mirrors
    upstream's `readObjectAtOffset`. -/
def readObjectAtOffset (buf : Data.PDF.Core.IO.Buffer.Buffer) (off : Nat) :
    IO (Ref × Object) :=
  message "readObjectAtOffset" do
    buf.seek off
    let (ref, o) ←
      MonadExcept.tryCatch
        (Data.PDF.Stream.parseFromStream
          Data.PDF.Core.Parsers.Object.parseIndirectObject
          (Data.PDF.Core.IO.Buffer.toInputStream buf))
        (fun e => match e with
          | .userError s => throw (corrupted s)
          | other => throw other)
    match o with
    | .stream (.mk entries _) =>
      let pos ← buf.tell
      pure (ref, Object.stream (Stream.mk entries pos))
    | .ref _ => throw (corrupted "Indirect object can't be a Ref")
    | _ => pure (ref, o)

/-- One `(objNum, offset)` header pair inside an object stream's header
    table (PDF32000-1:2008 §7.5.7): two whitespace-separated decimal
    numbers. -/
private def headerPair : Parser (Nat × Nat) := do
  let n ← digits
  skipSpace
  let off ← digits
  skipSpace
  pure (n, off)

/-- Parse exactly `n + 1` header pairs, returning only the *last* one — see
    the module doc-comment for why this sidesteps upstream's "impossible"
    dead-branch guard entirely: the recursion's shape already guarantees at
    least one pair is read, so there is no empty case to (not) handle. -/
def parseHeaderUpTo : Nat → Parser (Nat × Nat)
  | 0 => headerPair
  | n + 1 => do
    let _ ← headerPair
    parseHeaderUpTo n

/-- Read object number `num` from a decoded PDF object stream
    (PDF32000-1:2008 §7.5.7). `first` is the stream dictionary's `"First"`
    value: the byte offset (from the start of the stream's decoded data) at
    which the first object's data begins. Never returns a `Stream` (an
    object stream cannot itself directly embed a stream object). Mirrors
    upstream's `readCompressedObject`. -/
def readCompressedObject (is : Data.PDF.Stream.InputStream) (first : Nat) (num : Nat) :
    IO Object := do
  let (is', counter) ← Data.PDF.Stream.countInput is
  let (_, off) ←
    MonadExcept.tryCatch
      (Data.PDF.Stream.parseFromStream (parseHeaderUpTo num) is')
      (fun e => match e with
        | .userError s => throw (corrupted "Object stream" [s])
        | other => throw other)
  let pos ← counter
  -- Upstream computes `first + off - pos` over `Int64` (which could in
  -- principle go negative on malformed input); `Nat` subtraction saturates
  -- at `0` instead, which is no less safe: a negative skip is nonsensical
  -- either way, and `dropExactly 0` is a harmless no-op rather than a
  -- crash, for input that was already malformed.
  Data.PDF.Core.IO.Buffer.dropExactly (first + off - pos) is
  MonadExcept.tryCatch
    (Data.PDF.Stream.parseFromStream Data.PDF.Core.Parsers.Object.parseObject is)
    (fun e => match e with
      | .userError s => throw (corrupted "Object in object stream" [s])
      | other => throw other)

end Data.PDF.Core.Util
