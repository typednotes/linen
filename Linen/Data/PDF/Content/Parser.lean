/-
  Data.PDF.Content.Parser — parse a content stream into operators

  Ports `Pdf.Content.Parser` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/Parser.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/Parser.hs`),
  module 10 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  A PDF content stream (PDF32000-1:2008 §7.8.2) is a flat sequence of
  operands (plain `Object`s) interspersed with operator keywords; each
  operator "closes" the run of operands parsed since the previous operator.
  This module parses one `Data.PDF.Content.Ops.Expr` at a time
  (`parseContent`) and glues a run of `Expr`s into a complete
  `Data.PDF.Content.Ops.Operator` (`readNextOperator`).

  ## Adapting `InputStream Expr` to a raw-byte `InputStream`

  Upstream's `readNextOperator` reads from an `InputStream Expr` — a stream
  *already* tokenized into `Expr`s by feeding raw bytes through
  `parseContent` via `io-streams`' `Streams.Attoparsec.parserToInputStream`
  (built elsewhere, upstream-side, outside this file). `linen`'s
  `Data.PDF.Stream.InputStream` (the `io-streams` port used throughout this
  project, see `docs/imports/IoStreams/dependencies.md`) is a raw-byte
  stream, not one generic in its element type, so there is no direct
  `InputStream Expr` to receive here.

  `readNextOperator` below is therefore adapted to take the raw-byte
  `Data.PDF.Stream.InputStream` directly and pull `Expr`s off it itself, one
  `parseContent` call at a time via `Data.PDF.Stream.parseFromStream` —
  exactly the tokenization step upstream performs once, up front, when
  building its `InputStream Expr`, just interleaved with the consuming loop
  instead of being a separate stream stage. The externally observable
  behaviour (which operator/operands are produced, in what order, and which
  malformed inputs are rejected as `Corrupted`) is unchanged.

  ## Termination

  Upstream's `go` accumulator loop recurses once per `Expr` pulled off the
  stream, with no bound visible to a caller (a content stream can carry
  arbitrarily many operands before its operator). Reading through
  `Data.PDF.Stream.InputStream` is itself an arbitrary `IO` action with no
  decreasing measure to prove termination against — exactly the situation
  `Data.PDF.Stream.toList`/`Network.WebApp.strictRequestBody` are already in
  in this codebase, and both are written as a `while` loop over local
  mutable state rather than a recursive call, needing no termination proof
  and no `partial`. `readNextOperator` follows that same established idiom.
-/
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Parsers.Object
import Linen.Data.PDF.Core.Parsers.Util
import Linen.Data.PDF.Content.Ops
import Linen.Data.PDF.Stream
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Content.Parser

open Std.Internal.Parsec Std.Internal.Parsec.ByteArray
open Data.PDF.Core.Object Data.PDF.Content.Ops
open Data.PDF.Core.Parsers.Object (isRegularChar parseObject)

/-! ── Parsing one expression ── -/

/-- Parse one content-stream expression: an operand `Object`, or an operator
    keyword (any run of `isRegularChar` bytes that isn't itself a valid
    `Object`), or `none` at end of input. Mirrors upstream's `parseContent`,
    which treats `%`-comments as spaces via `Data.PDF.Core.Parsers.Util
    .skipSpace` (already comment-aware, so it is reused directly rather than
    duplicated). The `Object` alternative is wrapped in `attempt` since it
    can consume input before failing (e.g. on an unterminated string or
    dictionary) — see `Data.PDF.Core.Parsers.Object`'s "Backtracking" note,
    the same convention followed here. -/
def parseContent : Parser (Option Expr) := do
  Data.PDF.Core.Parsers.Util.skipSpace
  (eof *> pure none) <|>
    some <$>
      (attempt (Expr.obj <$> parseObject) <|>
       ((Expr.op ∘ toOp ∘ Data.ByteString.pack ∘ Array.toList) <$> many1 (satisfy isRegularChar)))

/-! ── Reading a complete operator ── -/

/-- Read the next operator (together with the operands that preceded it)
    off a raw content-stream byte source, or `none` at end of input with no
    pending operands. Throws `Data.PDF.Core.Exception.corrupted` if input
    ends with operands pending but no closing operator, or if the underlying
    parse itself fails. See the module doc-comment for how this adapts
    upstream's `InputStream Expr`-based `readNextOperator` to a raw-byte
    `Data.PDF.Stream.InputStream`, and for the `while`-loop termination
    argument. -/
def readNextOperator (is : Data.PDF.Stream.InputStream) : IO (Option Operator) :=
  Data.PDF.Core.Exception.message "readNextOperator" do
    let mut args : List Object := []
    let mut result : Option Operator := none
    let mut done := false
    while !done do
      let expr ←
        MonadExcept.tryCatch (Data.PDF.Stream.parseFromStream parseContent is)
          (fun e => match e with
            | .userError s => throw (Data.PDF.Core.Exception.corrupted s)
            | other => throw other)
      match expr with
      | none =>
        done := true
        if !args.isEmpty then
          throw (Data.PDF.Core.Exception.corrupted s!"Args without op: {reprStr args.reverse}")
      | some (.obj o) => args := o :: args
      | some (.op o) =>
        done := true
        result := some (o, args.reverse)
    return result

end Data.PDF.Content.Parser
