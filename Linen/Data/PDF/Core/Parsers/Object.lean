/-
  Data.PDF.Core.Parsers.Object — parsing `Object` values

  Ports `Pdf.Core.Parsers.Object` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox,
  `core/lib/Pdf/Core/Parsers/Object.hs`), module 8 of the `pdf-toolbox-core`
  import documented in `docs/imports/PdfToolboxCore/dependencies.md`.

  ## Backtracking

  Upstream's `attoparsec` alternative (`<|>`) always backtracks fully to the
  choice point on failure, even after consuming input. `Std.Internal
  .Parsec`'s `orElse`/`<|>` only backtracks on a **zero-consumption** failure
  (see `Std.Internal.Parsec.Basic.orElse`). Every alternative below that can
  consume more than zero bytes before possibly failing is therefore wrapped
  in `attempt`, so the combined behaviour matches attoparsec's automatic
  backtracking exactly.

  ## Termination

  `parseObject`/`parseDict`/`parseArray` are mutually recursive (an
  `array`/`dict` value's elements are themselves `Object`s), and `takeStr`
  (the literal-string body) loops until it sees the closing `)` at nesting
  level 0. Neither recursion is visibly structurally decreasing to Lean's
  termination checker on its own. Rather than fake a bound with an explicit
  `fuel : Nat` (the pattern `AGENTS.md` forbids), every one of these is a
  genuine well-founded recursion on the parser's real, shrinking measure: the
  number of input bytes still to consume, `ByteArray.Iterator.remainingBytes`.

  Exactly as in `Database.Redis.Protocol.replyStep` and
  `Control.Lens.Plated.foldChildrenOf` (see those modules' termination notes
  for the same argument in full), each recursion is written directly against
  `WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf`, with the
  induction hypothesis `ih` supplied by hand: `ih cur _` re-parses starting
  from iterator `cur`, and is only callable once given a proof that
  `cur.remainingBytes < it.remainingBytes` — strictly fewer bytes remain than
  at the enclosing call's entry.

  That decrease proof is obtained *by construction*, needing no monotonicity
  lemmas about the intermediate combinators (`skipSpace`, `parseName`,
  `readOctalEscape`, …). Such lemmas are in fact unavailable here: several of
  these route through stdlib `partial` scanners (`digits`, `skipWhile`,
  `many`) whose bodies are opaque to the logic and so admit no equational
  reasoning at all. Instead every recursive descent is guarded by
  `if h : cur.remainingBytes < it.remainingBytes then ih cur h else …`: the
  `dite` hands the required decrease proof `h` straight to `ih`. This is not a
  fuel cap — the guard *is* the well-foundedness condition itself. Every step
  that recurses has first consumed at least one byte (a literal-string body
  byte via `any`, an opening `(`/`<<`/`[` delimiter, or a `/Name` key), so a
  genuine nested parse always has strictly fewer bytes remaining and the guard
  always passes for well-formed input; it can only fail on input that failed
  to advance at all (impossible for a forward-moving parse), where reporting a
  parse error is exactly right rather than looping forever.

  `takeStr` is a single self-recursion of this shape. The object family is
  two-level: `parseObject` (`parseObjectStep`) is the `WellFounded.fix` whose
  `ih` re-parses a nested object, and the dictionary/array element loops
  (`dictLoop`/`arrayLoop`) are each their *own* `WellFounded.fix` on
  `remainingBytes` — looping until the closing `>>`/`]` — into which
  `parseObjectStep`'s `ih` is threaded so their per-element object parses
  descend through the same measure. No fuel-style seed tied to
  `it.remainingBytes` is needed at any public entry point any more: the
  parsers are applied directly to the real input iterator, and on truncated or
  unterminated input the underlying `any`/`skipByte` fails with a genuine
  parse error.

  ## Octal string escapes

  Upstream's `take3Digits` reconstructs a backslash-escape's octal value by
  consing newly-read octal digits onto the *front* of an accumulator and
  padding missing trailing digits with `'0'`, then computing
  `sum (zipWith (*) [1,8,64] ds)` over the (now up-to-3-digit, front-padded)
  list. Read digit-by-digit this always amounts to the standard big-endian
  octal value with missing *trailing* digits defaulting to `0`
  (`readOctalEscape` below computes this directly as `d1*64 + d2*8 + d3`,
  each `dN` defaulting to `0` when absent) — including upstream's one
  genuinely underspecified quirk, faithfully preserved here: the first
  escape character is *always* treated as an octal digit value via
  `fromEnum ch' - 48`, even when it is not actually one of `0`-`7` (e.g. the
  literal escapes `\(`, `\)`, `\\` and the named escapes `\r\n\f\b\t` are
  all handled by earlier, more specific cases first, so this fallback is
  only reached for a backslash followed by some other, genuinely-unexpected
  byte — upstream does not validate it as a digit there either).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Parsers.Util
import Linen.Data.Scientific
import Std.Internal.Parsec.ByteArray

namespace Data.PDF.Core.Parsers.Object

open Std.Internal.Parsec Std.Internal.Parsec.ByteArray
open Data.PDF.Core.Object Data.PDF.Core.Parsers.Util

-- ── Names ──

/-- Bytes disallowed inside a PDF name's body (PDF32000-1:2008 §7.3.5),
    mirroring upstream's `isRegularChar`. -/
def isRegularChar (b : UInt8) : Bool :=
  !( b == '['.toUInt8 || b == ']'.toUInt8 || b == '('.toUInt8 || b == ')'.toUInt8
   || b == '/'.toUInt8 || b == '<'.toUInt8 || b == '>'.toUInt8 || b == '{'.toUInt8
   || b == '}'.toUInt8 || b == '%'.toUInt8 || b == ' '.toUInt8
   || b == '\n'.toUInt8 || b == '\r'.toUInt8 )

/-- Parse a `/Name` (PDF32000-1:2008 §7.3.5). -/
def parseName : Parser Data.PDF.Core.Name.Name := do
  skipByte '/'.toUInt8
  let chars ← many1 (satisfy isRegularChar)
  match Data.PDF.Core.Name.Name.make (Data.ByteString.pack chars.toList) with
  | .ok n => pure n
  | .error e => fail e

-- ── Numbers ──

/-- Is `b` an ASCII decimal digit? -/
def isDigitByte (b : UInt8) : Bool := b ≥ '0'.toUInt8 && b ≤ '9'.toUInt8

/-- Zero-or-more decimal digits, as raw bytes (never fails). -/
def digitsOpt : Parser (Array UInt8) := many (satisfy isDigitByte)

/-- Interpret a (possibly empty) run of decimal-digit bytes as a `Nat`,
    most-significant digit first (`0` for an empty run). -/
def digitsToNat (ds : Array UInt8) : Nat :=
  ds.foldl (fun acc d => acc * 10 + (d - '0'.toUInt8).toNat) 0

/-- Parse a PDF number (PDF32000-1:2008 §7.3.3): an optional sign, digits,
    an optional `.`-fraction (upstream also accepts a leading-dot number
    such as `.5`, with no integer part, via its `scientific`-parser
    fallback — accepted here directly since integer and fraction parts are
    each independently optional, so long as at least one of them is
    present). -/
def parseNumber : Parser Data.Scientific := do
  let neg ← (skipByte '-'.toUInt8 *> pure true) <|> (skipByte '+'.toUInt8 *> pure false)
    <|> pure false
  let intDigits ← digitsOpt
  let fracDigits ←
    (attempt do
      skipByte '.'.toUInt8
      digitsOpt) <|> pure #[]
  if intDigits.isEmpty && fracDigits.isEmpty then
    fail "parseNumber: expected a number"
  else
    let coeff : Int := (digitsToNat intDigits : Int) * (10 ^ fracDigits.size : Nat)
      + (digitsToNat fracDigits : Int)
    let coeff := if neg then -coeff else coeff
    let base := Data.Scientific.scientific coeff (-(fracDigits.size : Int))
    pure base

-- ── Literal strings ──

/-- Is `b` an ASCII octal digit? -/
def isOctDigitByte (b : UInt8) : Bool := b ≥ '0'.toUInt8 && b ≤ '7'.toUInt8

/-- An octal digit byte's numeric value. -/
def octVal (b : UInt8) : UInt8 := b - '0'.toUInt8

/-- Optionally consume one more octal-digit byte, returning its value. -/
def optOctDigit : Parser (Option UInt8) := do
  match ← peekWhen? isOctDigitByte with
  | some b => let _ ← any; pure (some (octVal b))
  | none => pure none

/-- Read the (up to two) remaining digits of a `\ddd` octal escape, given
    the already-consumed first escape byte `first` (treated as an octal
    digit value regardless of whether it actually is one — see the module
    doc-comment). -/
def readOctalEscape (first : UInt8) : Parser UInt8 := do
  let d1 := first - '0'.toUInt8
  let d2 ← optOctDigit
  let d3 ← optOctDigit
  match d2, d3 with
  | some d2, some d3 => pure (d1 * 64 + d2 * 8 + d3)
  | some d2, none => pure (d1 * 8 + d2)
  | none, _ => pure d1

/-- Parse the body of a literal string (PDF32000-1:2008 §7.3.4.2), after the
    opening `(` has already been consumed, up to and including the matching
    closing `)`. `lvl` tracks unescaped-parenthesis nesting depth (a literal
    string may itself contain balanced, unescaped `(`/`)` pairs); `acc`
    accumulates decoded bytes, most-recent first.

    A single well-founded self-recursion on the iterator's `remainingBytes`
    (see the module doc-comment): every recursive step first consumes at
    least one byte via `any`, so the guarded `ih` descent always has strictly
    fewer bytes remaining. `lvl`/`acc` are ordinary parameters threaded
    outside the well-founded argument (they play no part in termination). -/
def takeStrCore : ByteArray.Iterator → Nat → List UInt8 →
    ParseResult (List UInt8) ByteArray.Iterator :=
  WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf
    (fun it ih lvl acc =>
      match any it with
      | .error pos err => .error pos err
      | .success it1 b =>
        if h : it1.remainingBytes < it.remainingBytes then
          if b == ')'.toUInt8 then
            if lvl == 0 then .success it1 acc.reverse
            else ih it1 h (lvl - 1) (b :: acc)
          else if b == '('.toUInt8 then ih it1 h (lvl + 1) (b :: acc)
          else if b == '\\'.toUInt8 then
            match any it1 with
            | .error pos err => .error pos err
            | .success it2 e =>
              if h2 : it2.remainingBytes < it.remainingBytes then
                if e == '('.toUInt8 then ih it2 h2 lvl ('('.toUInt8 :: acc)
                else if e == ')'.toUInt8 then ih it2 h2 lvl (')'.toUInt8 :: acc)
                else if e == '\\'.toUInt8 then ih it2 h2 lvl ('\\'.toUInt8 :: acc)
                else if e == 'r'.toUInt8 then ih it2 h2 lvl ('\r'.toUInt8 :: acc)
                else if e == 'n'.toUInt8 then ih it2 h2 lvl ('\n'.toUInt8 :: acc)
                else if e == 'f'.toUInt8 then ih it2 h2 lvl (12 :: acc)
                else if e == 'b'.toUInt8 then ih it2 h2 lvl (8 :: acc)
                else if e == 't'.toUInt8 then ih it2 h2 lvl ('\t'.toUInt8 :: acc)
                else if e == '\r'.toUInt8 then
                  -- `\<CR>` (or `\<CR><LF>`) is a line continuation: dropped.
                  match (attempt (skipByte '\n'.toUInt8) <|> pure ()) it2 with
                  | .error pos err => .error pos err
                  | .success it3 _ =>
                    if h3 : it3.remainingBytes < it.remainingBytes then
                      ih it3 h3 lvl acc
                    else .error it3 (.other "parseString: unterminated string")
                else if e == '\n'.toUInt8 then
                  -- `\<LF>` is likewise a line continuation.
                  ih it2 h2 lvl acc
                else
                  match readOctalEscape e it2 with
                  | .error pos err => .error pos err
                  | .success it3 v =>
                    if h3 : it3.remainingBytes < it.remainingBytes then
                      ih it3 h3 lvl (v :: acc)
                    else .error it3 (.other "parseString: unterminated string")
              else .error it2 (.other "parseString: unterminated string")
          else ih it1 h lvl (b :: acc)
        else .error it1 (.other "parseString: unterminated string"))

/-- Parse the body of a literal string as a `Parser`, given the current
    parenthesis-nesting depth `lvl` and decoded-byte accumulator `acc`. -/
def takeStr (lvl : Nat) (acc : List UInt8) : Parser (List UInt8) :=
  fun it => takeStrCore it lvl acc

/-- Parse a literal string (PDF32000-1:2008 §7.3.4.2), `(...)`. -/
def parseString : Parser Data.ByteString := do
  skipByte '('.toUInt8
  let bytes ← takeStr 0 []
  pure (Data.ByteString.pack bytes)

/-- Parse a hexadecimal string (PDF32000-1:2008 §7.3.4.3), `<...>`. An odd
    trailing hex digit is padded with an implicit trailing `0`, matching
    upstream's `Numeric.readHex`-based parser (which pads the same way via
    an even-length requirement met by appending `'0'` beforehand — folded
    here into pair-parsing directly). -/
def parseHexString : Parser Data.ByteString := do
  skipByte '<'.toUInt8
  let digits ← many (satisfy (fun b =>
    isDigitByte b || (b ≥ 'a'.toUInt8 && b ≤ 'f'.toUInt8) || (b ≥ 'A'.toUInt8 && b ≤ 'F'.toUInt8)))
  skipByte '>'.toUInt8
  let hexVal (b : UInt8) : UInt8 :=
    if b ≥ '0'.toUInt8 && b ≤ '9'.toUInt8 then b - '0'.toUInt8
    else if b ≥ 'a'.toUInt8 && b ≤ 'f'.toUInt8 then b - 'a'.toUInt8 + 10
    else b - 'A'.toUInt8 + 10
  let rec pair : List UInt8 → List UInt8
    | [] => []
    | [d] => [hexVal d * 16]
    | d1 :: d2 :: rest => (hexVal d1 * 16 + hexVal d2) :: pair rest
  pure (Data.ByteString.pack (pair digits.toList))

-- ── Booleans ──

/-- Parse a PDF boolean (PDF32000-1:2008 §7.3.2). -/
def parseBool : Parser Bool :=
  attempt (skipString "true" *> pure true) <|> attempt (skipString "false" *> pure false)

-- ── Indirect references ──

/-- Parse an indirect reference `idx gen R` (PDF32000-1:2008 §7.3.10). -/
def parseRef : Parser Data.PDF.Core.Object.Ref := do
  let idx ← digits
  skipSpace
  let gen ← digits
  skipSpace
  skipByte 'R'.toUInt8
  pure { index := (idx : Int), generation := (gen : Int) }

-- ── Objects, dictionaries, arrays (mutually recursive) ──

/-- The non-recursive leading object alternatives, tried in upstream's order:
    `null`, `/Name`, boolean. Each is wrapped in `attempt` so a
    partial-consumption failure backtracks fully (see the Backtracking note). -/
def objectHead : Parser Object :=
  attempt (skipString "null" *> pure Object.null) <|>
  attempt (Object.name <$> parseName) <|>
  attempt (Object.bool <$> parseBool)

/-- The non-recursive trailing object alternatives, tried after the recursive
    dictionary/array alternatives: literal string, hex string, indirect
    reference, number (the final, non-`attempt` fallback). -/
def objectTail : Parser Object :=
  attempt (Object.string <$> parseString) <|>
  attempt (Object.string <$> parseHexString) <|>
  attempt (Object.ref <$> parseRef) <|>
  (Object.number <$> parseNumber)

/-- Parse the elements of an array `[ obj ... ]`, after the opening `[` has
    been consumed, looping until the closing `]`.

    A well-founded recursion on the *cursor*'s `remainingBytes` (its own
    induction hypothesis `ihLoop`): each iteration parses one element then
    loops at the strictly-advanced cursor. `it`/`ih` are `parseObjectStep`'s
    own entry iterator and induction hypothesis, threaded in so each element's
    nested object parse descends through that measure (guarded by
    `it0.remainingBytes < it.remainingBytes`); see the module doc-comment. -/
def arrayLoop (it : ByteArray.Iterator)
    (ih : (cur : ByteArray.Iterator) → cur.remainingBytes < it.remainingBytes →
      ParseResult Object ByteArray.Iterator) :
    ByteArray.Iterator → Array Object → ParseResult (Array Object) ByteArray.Iterator :=
  WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf
    (fun cursor ihLoop acc =>
      match skipSpace cursor with
      | .error pos err => .error pos err
      | .success it0 _ =>
        match skipByte ']'.toUInt8 it0 with
        | .success r _ => .success r acc
        | .error _ _ =>
          if h : it0.remainingBytes < it.remainingBytes then
            match ih it0 h with
            | .error pos err => .error pos err
            | .success r o =>
              if h2 : r.remainingBytes < cursor.remainingBytes then
                ihLoop r h2 (acc.push o)
              else .error r (.other "parseArray: element consumed no input")
          else .error it0 (.other "parseArray: too deeply nested"))

/-- Parse the `/Key value` pairs of a dictionary `<< ... >>`, after the
    opening `<<` has been consumed, looping until the closing `>>`, and
    returning its internal association-array representation (see
    `Data.PDF.Core.Object`'s module doc-comment).

    Structured exactly like `arrayLoop`: a well-founded recursion on the
    cursor's `remainingBytes`, with `parseObjectStep`'s `it`/`ih` threaded in
    for each pair's value (an arbitrary nested object). The key name is parsed
    by the non-recursive `parseName`, which consumes at least the `/` before
    the value's descent. See the module doc-comment. -/
def dictLoop (it : ByteArray.Iterator)
    (ih : (cur : ByteArray.Iterator) → cur.remainingBytes < it.remainingBytes →
      ParseResult Object ByteArray.Iterator) :
    ByteArray.Iterator → Array (Data.PDF.Core.Name.Name × Object) →
      ParseResult (Array (Data.PDF.Core.Name.Name × Object)) ByteArray.Iterator :=
  WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf
    (fun cursor ihLoop acc =>
      match skipSpace cursor with
      | .error pos err => .error pos err
      | .success it0 _ =>
        match attempt (skipString ">>") it0 with
        | .success r _ => .success r acc
        | .error _ _ =>
          match parseName it0 with
          | .error pos err => .error pos err
          | .success itn key =>
            match skipSpace itn with
            | .error pos err => .error pos err
            | .success its _ =>
              if h : its.remainingBytes < it.remainingBytes then
                match ih its h with
                | .error pos err => .error pos err
                | .success r value =>
                  if h2 : r.remainingBytes < cursor.remainingBytes then
                    ihLoop r h2 (acc.push (key, value))
                  else .error r (.other "parseDict: element consumed no input")
              else .error its (.other "parseDict: too deeply nested"))

/-- The recursive dictionary alternative for `parseObjectStep`: consume `<<`
    (backtracking fully on any failure, per the Backtracking note) and loop
    its pairs via `dictLoop`. `none` signals "this alternative did not apply",
    so the caller falls through to the next. -/
def dictAlt (it : ByteArray.Iterator)
    (ih : (cur : ByteArray.Iterator) → cur.remainingBytes < it.remainingBytes →
      ParseResult Object ByteArray.Iterator)
    (it0 : ByteArray.Iterator) : Option (ParseResult Object ByteArray.Iterator) :=
  match attempt (skipString "<<") it0 with
  | .error _ _ => none
  | .success it1 _ =>
    match dictLoop it ih it1 #[] with
    | .success r entries => some (.success r (Object.dictRaw entries))
    | .error _ _ => none

/-- The recursive array alternative for `parseObjectStep`: consume `[` and
    loop its elements via `arrayLoop`. `none` signals fall-through. -/
def arrayAlt (it : ByteArray.Iterator)
    (ih : (cur : ByteArray.Iterator) → cur.remainingBytes < it.remainingBytes →
      ParseResult Object ByteArray.Iterator)
    (it0 : ByteArray.Iterator) : Option (ParseResult Object ByteArray.Iterator) :=
  match skipByte '['.toUInt8 it0 with
  | .error _ _ => none
  | .success it1 _ =>
    match arrayLoop it ih it1 #[] with
    | .success r items => some (.success r (Object.array items))
    | .error _ _ => none

/-- Parse any PDF object (PDF32000-1:2008 §7.3), trying each alternative in
    upstream's order: `null`, name, boolean, dictionary, array, literal
    string, hex string, indirect reference, number.

    A well-founded recursion on the iterator's `remainingBytes`: the
    dictionary and array alternatives descend into their element loops
    (`dictLoop`/`arrayLoop`) carrying this call's induction hypothesis `ih`,
    which re-parses their nested objects at a strictly smaller iterator. See
    the module doc-comment for the full termination argument. -/
def parseObjectStep : Parser Object :=
  WellFounded.fix (measure ByteArray.Iterator.remainingBytes).wf
    (fun it ih =>
      match skipSpace it with
      | .error pos err => .error pos err
      | .success it0 _ =>
        match objectHead it0 with
        | .success r x => .success r x
        | .error _ _ =>
          match dictAlt it ih it0 with
          | some res => res
          | none =>
            match arrayAlt it ih it0 with
            | some res => res
            | none => objectTail it0)

/-- Parse any PDF object. Public entry point. -/
def parseObject : Parser Object := parseObjectStep

/-- Parse a dictionary `<< /Key value ... >>` (PDF32000-1:2008 §7.3.7),
    returning the public `Dict` type. -/
def parseDict : Parser Dict := fun it =>
  match skipString "<<" it with
  | .error pos err => .error pos err
  | .success it1 _ =>
    match dictLoop it (fun cur _ => parseObjectStep cur) it1 #[] with
    | .success rem entries => .success rem (Std.HashMap.ofList entries.toList)
    | .error pos err => .error pos err

/-- Parse an array `[ obj ... ]` (PDF32000-1:2008 §7.3.6). -/
def parseArray : Parser (Array Object) := fun it =>
  match skipByte '['.toUInt8 it with
  | .error pos err => .error pos err
  | .success it1 _ => arrayLoop it (fun cur _ => parseObjectStep cur) it1 #[]

-- ── Indirect objects ──

/-- Skip up to (and including) the `stream` keyword and its trailing
    end-of-line, signalling that the dictionary just parsed introduces a
    stream's raw data rather than being a bare dictionary object. -/
def parseTillStreamData : Parser Unit := do
  skipSpace
  skipString "stream"
  endOfLine

/-- Parse a complete indirect object, `idx gen obj <object> [stream ...]`
    (PDF32000-1:2008 §7.3.10, §7.3.8). If the parsed object is a dictionary
    immediately followed by the `stream` keyword, it is reinterpreted as a
    `Stream` object (with its data offset left at `0`, to be filled in by
    the caller once it knows the stream's actual starting position — mirrors
    upstream, which likewise defers that to its own callers). -/
def parseIndirectObject : Parser (Data.PDF.Core.Object.Ref × Object) := do
  skipSpace
  let idx ← digits
  skipSpace
  let gen ← digits
  skipSpace
  skipString "obj"
  let ref : Data.PDF.Core.Object.Ref := { index := (idx : Int), generation := (gen : Int) }
  let obj ← parseObject
  match obj with
  | .dictRaw entries =>
    attempt (parseTillStreamData *> pure (ref, Object.stream (Stream.mk entries 0)))
      <|> pure (ref, obj)
  | _ => pure (ref, obj)

end Data.PDF.Core.Parsers.Object
