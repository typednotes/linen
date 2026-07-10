/-
  Tests for `Linen.Data.PDF.Content.Parser`.

  `parseContent` is a pure `Parser`, checked with `#guard`. `readNextOperator`
  is `IO`-based (it reads off a `Data.PDF.Stream.InputStream`), so it is
  checked with `#eval` instead, per `Tests/Linen/Data/PDF/StreamTest.lean`'s
  established convention for this project's `io-streams`-backed types.
-/
import Linen.Data.PDF.Content.Parser
import Linen.Data.PDF.Stream

open Data.PDF.Core.Object Data.PDF.Content.Ops Data.PDF.Content.Parser
open Std.Internal.Parsec ByteArray

namespace Tests.Data.PDF.Content.Parser

private def bytes (s : String) : ByteArray := String.toUTF8 s

private def bs (s : String) : Data.ByteString := Data.ByteString.pack s.toUTF8.toList

/-- Fail the enclosing `IO` action with `msg` unless `cond` holds. -/
private def check (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

-- ── `parseContent` ──

-- An operand parses as `Expr.obj`.
#guard match Parser.run parseContent (bytes "42") with
  | .ok (some (.obj (.number n))) => n.toBoundedInteger == some 42
  | _ => false

-- A keyword parses as `Expr.op`.
#guard match Parser.run parseContent (bytes "cm") with
  | .ok (some (.op .cm)) => true
  | _ => false

-- Leading whitespace/comments are skipped before the expression.
#guard match Parser.run parseContent (bytes "  % a comment\n  q") with
  | .ok (some (.op .q)) => true
  | _ => false

-- End of input (after skipping trailing space) yields `none`.
#guard match Parser.run parseContent (bytes "   ") with
  | .ok none => true
  | _ => false

-- An unknown keyword still parses, as `UnknownOp`.
#guard match Parser.run parseContent (bytes "Frobnicate") with
  | .ok (some (.op (.UnknownOp b))) => b == bs "Frobnicate"
  | _ => false

-- ── `readNextOperator` ──

-- A single operator with two preceding operands.
#eval show IO Unit from do
  let s ← Data.PDF.Stream.fromByteString (bytes "1 2 cm")
  let some (op, args) ← readNextOperator s | throw (IO.userError "expected an operator")
  check (op == Op.cm) "expected Op.cm"
  check (args.length == 2) s!"expected 2 args, got {args.length}"

-- A no-operand operator (`q`) reads with an empty argument list.
#eval show IO Unit from do
  let s ← Data.PDF.Stream.fromByteString (bytes "q")
  let some (op, args) ← readNextOperator s | throw (IO.userError "expected an operator")
  check (op == Op.q) "expected Op.q"
  check (args.isEmpty) "expected no arguments"

-- Two operators in sequence are read one at a time, in order.
#eval show IO Unit from do
  let s ← Data.PDF.Stream.fromByteString (bytes "q Q")
  let some (op1, _) ← readNextOperator s | throw (IO.userError "expected first operator")
  check (op1 == Op.q) "expected Op.q first"
  let some (op2, _) ← readNextOperator s | throw (IO.userError "expected second operator")
  check (op2 == Op.Q) "expected Op.Q second"

-- Exhausted input with no pending operands yields `none`.
#eval show IO Unit from do
  let s ← Data.PDF.Stream.fromByteString (bytes "   ")
  let .none ← readNextOperator s | throw (IO.userError "expected none at end of input")
  pure ()

-- Trailing operands with no closing operator is reported as corrupted.
#eval show IO Unit from do
  let s ← Data.PDF.Stream.fromByteString (bytes "1 2")
  match ← (readNextOperator s).toBaseIO with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "expected a corrupted-input error")

end Tests.Data.PDF.Content.Parser
