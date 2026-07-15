/-
  Linen.Database.Redis.Protocol — the RESP2 wire-format encoder/decoder

  ## Haskell source
  `Database.Redis.Protocol` from https://hackage.haskell.org/package/hedis
  (module 3 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Protocol.hs`. Exposes `Reply`, `renderRequest`, `reply`.

  ## `scanner` substitution
  Upstream builds its incremental parser on the `scanner` package
  (`anyChar8`, `takeWhileChar8`, `char8`, `take`). Per
  `docs/imports/hedis/dependencies.md`'s external-dependency note, this is
  exactly the shape of parser already resolved onto Lean stdlib's
  `Std.Internal.Parsec`/`Std.Internal.Parsec.ByteArray` elsewhere in this
  codebase (e.g. `Data.PDF.Core.Parsers.Util`, `Graphics.Netpbm`), so `reply`
  is written directly against `Std.Internal.Parsec.ByteArray.Parser`, with no
  separate `scanner` port. Upstream's `integral` (via `Data.Text.Read`) is
  replaced by the stdlib `digits`/`pbyte` combinators directly.

  ## Termination
  A `*<n>\r\n` (`MultiBulk`) reply recursively parses `n` further `Reply`
  values, each of which may itself be a `MultiBulk` — unboundedly deep in
  general — so the recursion is not visibly structurally decreasing to
  Lean's termination checker. Rather than fake a bound with an explicit
  `fuel : Nat` (the pattern `AGENTS.md` forbids), `replyStep` is a genuine
  well-founded recursion on the parser's real, shrinking measure: the number
  of input bytes still to consume, `ByteArray.Iterator.remainingBytes`.

  Exactly as in `Control.Lens.Plated.foldChildrenOf` (see that module's
  termination note for the same argument in full), `replyStep` is written
  directly against `WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf`,
  with the induction hypothesis `ih` supplied by hand: `ih cur _` re-parses a
  nested reply starting from iterator `cur`, and is only callable once given
  a proof that `cur.remainingBytes < it.remainingBytes` — strictly fewer
  bytes remain than at the enclosing call's entry.

  That decrease proof is obtained *by construction*, needing no monotonicity
  lemmas about the intermediate combinators (`signedInt`, `crlf`, `lineBody`,
  `bulkBody`). Such lemmas are in fact unavailable here: several of these
  route through stdlib `partial` scanners (`digits` → `digitsCore`,
  `lineBody` → `takeWhile`) whose bodies are opaque to the logic and so admit
  no equational reasoning at all. Instead the `MultiBulk` element loop
  (`multiBulkLoop` — an ordinary *structural* recursion on the element
  count, needing no well-founded argument of its own) guards every recursive
  descent with `if h : cur.remainingBytes < it.remainingBytes then ih cur h
  else …`: the `dite` hands the required decrease proof `h` straight to `ih`.
  This is not a fuel cap — the guard *is* the well-foundedness condition
  itself. Every reply's first step is `any`, which on success always advances
  the iterator by exactly one byte, so a genuine nested element always has
  strictly fewer bytes remaining and the guard always passes for well-formed
  input; it can only fail on input that failed to advance at all (impossible
  for a forward-moving parse), where reporting a parse error is exactly right
  rather than looping forever.

  Because each element's *output* iterator is fed into the next element's
  *input*, and `any`/`take`/… only ever move forward, no fuel-style seed is
  needed: `reply` is `replyStep` applied directly to the real input iterator.
  On empty or truncated input the first `any`/`take` fails with `Error.eof` —
  the genuine "come back with more bytes" signal that
  `ProtocolPipelining.parseOneReply` relies on — which the old `fuel + 1`
  seeding trick had to contort itself to preserve.
-/
import Std.Internal.Parsec.ByteArray

namespace Database.Redis.Protocol

open Std.Internal.Parsec ByteArray

-- ── Reply ──

/-- Low-level representation of a reply from the Redis server (RESP2). -/
inductive Reply where
  /-- A `+`-prefixed single-line reply, e.g. `+OK\r\n`. -/
  | singleLine (s : ByteArray)
  /-- A `-`-prefixed error reply, e.g. `-ERR foo\r\n`. -/
  | error (s : ByteArray)
  /-- A `:`-prefixed integer reply, e.g. `:42\r\n`. -/
  | integer (i : Int)
  /-- A `$`-prefixed bulk string reply, `none` for the null bulk `$-1\r\n`. -/
  | bulk (b : Option ByteArray)
  /-- A `*`-prefixed array of replies, `none` for the null array `*-1\r\n`. -/
  | multiBulk (rs : Option (List Reply))
  deriving BEq, Inhabited

-- ── Request rendering ──

/-- RESP's line terminator. -/
def crlfBytes : ByteArray := "\r\n".toUTF8

/-- Render one request argument as a RESP bulk string: `$<len>\r\n<arg>\r\n`. -/
def renderArg (arg : ByteArray) : ByteArray :=
  (("$" ++ toString arg.size).toUTF8 ++ crlfBytes) ++ arg ++ crlfBytes

/-- Render a full request (e.g. `["SET", "k", "v"]`) as a RESP multi-bulk
    command: `*<argc>\r\n$<len1>\r\n<arg1>\r\n...`. -/
def renderRequest (args : List ByteArray) : ByteArray :=
  args.foldl (fun acc arg => acc ++ renderArg arg)
    (("*" ++ toString args.length).toUTF8 ++ crlfBytes)

-- ── Low-level reply parsers ──

/-- Consume the two-byte `\r\n` line terminator. -/
def crlf : Parser Unit :=
  skipByte '\r'.toUInt8 *> skipByte '\n'.toUInt8

/-- Consume the rest of a line up to (excluding) the terminating `\r\n`. -/
def lineBody : Parser ByteArray := do
  let slice ← takeUntil (fun b => b == '\r'.toUInt8)
  crlf
  pure slice.toByteArray

/-- Parse a signed decimal integer such as `-123` or `42`, matching
    upstream's `integral` (a full line read then parsed via
    `Text.signed Text.decimal`, folded here into a direct digit parse). -/
def signedInt : Parser Int :=
  ((skipByte '-'.toUInt8 *> pure true) <|> pure false) >>= fun neg => do
    let n ← digits
    pure (if neg then -(Int.ofNat n) else Int.ofNat n)

/-- Parse a `$`-prefixed bulk-string reply body (the length prefix and its
    `\r\n` have already been consumed by the caller only up to the tag; here
    we read the declared length itself). -/
def bulkBody : Parser Reply := do
  let n ← signedInt
  crlf
  if n < 0 then
    pure (Reply.bulk none)
  else do
    let slice ← take n.toNat
    crlf
    pure (Reply.bulk (some slice.toByteArray))

-- ── The main reply parser (well-founded on remaining bytes, see module doc) ──

/-- The `MultiBulk` element loop, factored out of `replyStep`. Parses
    `remaining` further `Reply` values starting from iterator `cur`, feeding
    each element's output iterator into the next element's input.

    This is an ordinary *structural* recursion on the element count
    `remaining` — no well-founded argument of its own. The nested-reply parse
    is done by `ih` (the induction hypothesis of `replyStep`'s underlying
    `WellFounded.fix`, threaded in as a parameter). Each descent is guarded by
    `if h : cur.remainingBytes < it.remainingBytes`, whose `dite` supplies the
    exact decrease proof `h` that `ih` demands; see the module doc-comment for
    why this guard is the genuine well-foundedness condition, not a fuel cap.
    On the impossible-for-well-formed-input case where the guard fails (an
    element that consumed nothing), it reports a parse error rather than
    looping. -/
def multiBulkLoop (it : ByteArray.Iterator)
    (ih : (cur : ByteArray.Iterator) →
      cur.remainingBytes < it.remainingBytes → ParseResult Reply ByteArray.Iterator)
    (remaining : Nat) (cur : ByteArray.Iterator) (acc : Array Reply) :
    ParseResult (Array Reply) ByteArray.Iterator :=
  match remaining with
  | 0 => .success cur acc
  | remaining + 1 =>
    if h : cur.remainingBytes < it.remainingBytes then
      match ih cur h with
      | .error pos err => .error pos err
      | .success cur' r => multiBulkLoop it ih remaining cur' (acc.push r)
    else
      .error cur (.other "Redis.Protocol: multi-bulk element consumed no input")

/-- Parse a single `Reply`. A well-founded recursion on the iterator's
    `remainingBytes`: nested replies are re-parsed through the hand-supplied
    induction hypothesis `ih`, and the `MultiBulk` element loop
    (`multiBulkLoop`) discharges the required decrease obligation via a runtime
    guard. See the module doc-comment for the full termination argument. -/
def replyStep : Parser Reply :=
  WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf
    (fun it ih =>
      match any it with
      | .error pos err => .error pos err
      | .success it1 tag =>
        if tag == '+'.toUInt8 then
          (Reply.singleLine <$> lineBody) it1
        else if tag == '-'.toUInt8 then
          (Reply.error <$> lineBody) it1
        else if tag == ':'.toUInt8 then
          (do let n ← signedInt; crlf; pure (Reply.integer n)) it1
        else if tag == '$'.toUInt8 then
          bulkBody it1
        else if tag == '*'.toUInt8 then
          match (do let n ← signedInt; crlf; pure n) it1 with
          | .error pos err => .error pos err
          | .success it2 n =>
            if n < 0 then
              .success it2 (Reply.multiBulk none)
            else
              match multiBulkLoop it ih n.toNat it2 #[] with
              | .error pos err => .error pos err
              | .success it3 rs => .success it3 (Reply.multiBulk (some rs.toList))
        else
          .error it1 (.other s!"Redis.Protocol: unrecognised reply type tag {tag}"))

/-- Parse a single `Reply` from the input. Public entry point.

    No fuel-style seed is needed (see the module doc-comment): `replyStep` is
    applied directly to the real input iterator, and on empty or truncated
    input the first `any`/`take` fails with `Error.eof` — the genuine "come
    back with more bytes" signal that
    `Linen.Database.Redis.ProtocolPipelining.parseOneReply` relies on. -/
def reply : Parser Reply := replyStep

end Database.Redis.Protocol
