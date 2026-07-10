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

  `parseObject`/`parseDict`/`parseArray`/`parseKey` are mutually recursive
  (an `array`/`dict` value's elements are themselves `Object`s), and
  `takeStr` (the literal-string body) loops until it sees the closing `)` at
  nesting level 0. Neither recursion is visibly structurally decreasing to
  Lean's termination checker on its own, so both are parameterised by an
  explicit `fuel : Nat` that decreases by exactly one on every recursive
  step — a directly structural recursion on `fuel` requiring no
  `termination_by` at all. `fuel` is seeded from the parser's own
  `it.remainingBytes` at every public entry point: since a well-formed parse
  can never recurse (or consume literal-string escape bytes) more times than
  there are bytes left to consume, `fuel` is a genuine bound tied to the
  input size, not an arbitrary cap that could truncate a valid parse — it
  only ever "runs out" on malformed/unterminated input, where it reports a
  parse failure instead of looping forever (which is exactly what should
  happen).

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
    accumulates decoded bytes, most-recent first. See the module doc-comment
    for the `fuel` termination argument. -/
def takeStr : Nat → Nat → List UInt8 → Parser (List UInt8)
  | 0, _, _ => fail "parseString: unterminated string"
  | fuel + 1, lvl, acc => do
    let b ← any
    if b == ')'.toUInt8 then
      if lvl == 0 then
        pure acc.reverse
      else
        takeStr fuel (lvl - 1) (b :: acc)
    else if b == '('.toUInt8 then
      takeStr fuel (lvl + 1) (b :: acc)
    else if b == '\\'.toUInt8 then do
      let e ← any
      if e == '('.toUInt8 then takeStr fuel lvl ('('.toUInt8 :: acc)
      else if e == ')'.toUInt8 then takeStr fuel lvl (')'.toUInt8 :: acc)
      else if e == '\\'.toUInt8 then takeStr fuel lvl ('\\'.toUInt8 :: acc)
      else if e == 'r'.toUInt8 then takeStr fuel lvl ('\r'.toUInt8 :: acc)
      else if e == 'n'.toUInt8 then takeStr fuel lvl ('\n'.toUInt8 :: acc)
      else if e == 'f'.toUInt8 then takeStr fuel lvl (12 :: acc)
      else if e == 'b'.toUInt8 then takeStr fuel lvl (8 :: acc)
      else if e == 't'.toUInt8 then takeStr fuel lvl ('\t'.toUInt8 :: acc)
      else if e == '\r'.toUInt8 then
        -- `\<CR>` (or `\<CR><LF>`) is a line continuation: dropped entirely.
        (attempt (skipByte '\n'.toUInt8) <|> pure ()) *> takeStr fuel lvl acc
      else if e == '\n'.toUInt8 then
        -- `\<LF>` is likewise a line continuation.
        takeStr fuel lvl acc
      else do
        let v ← readOctalEscape e
        takeStr fuel lvl (v :: acc)
    else
      takeStr fuel lvl (b :: acc)

/-- Read the current position/iterator without consuming anything — used to
    seed `takeStr`'s `fuel` from the remaining input at the point the string
    body starts (see the module doc-comment). -/
private def getIt : Parser ByteArray.Iterator := fun it => .success it it

/-- Parse a literal string (PDF32000-1:2008 §7.3.4.2), `(...)`. -/
def parseString : Parser Data.ByteString := do
  skipByte '('.toUInt8
  let it ← getIt
  let bytes ← takeStr it.remainingBytes 0 []
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

mutual
  /-- Parse any PDF object (PDF32000-1:2008 §7.3), trying each alternative
      in upstream's order: `null`, name, boolean, dictionary, array, literal
      string, hex string, indirect reference, number. See the module
      doc-comment for the `fuel` termination argument (needed because
      `dict`/`array` recurse into this function for their elements). -/
  def parseObjectFuel : Nat → Parser Object
    | 0 => fail "parseObject: too deeply nested"
    | fuel + 1 => do
      skipSpace
      attempt (skipString "null" *> pure Object.null) <|>
      attempt (Object.name <$> parseName) <|>
      attempt (Object.bool <$> parseBool) <|>
      attempt (Object.dictRaw <$> parseDictFuel fuel) <|>
      attempt (Object.array <$> parseArrayFuel fuel) <|>
      attempt (Object.string <$> parseString) <|>
      attempt (Object.string <$> parseHexString) <|>
      attempt (Object.ref <$> parseRef) <|>
      (Object.number <$> parseNumber)

  /-- Parse one `/Key value` pair inside a dictionary. -/
  private def parseKeyFuel (fuel : Nat) : Parser (Data.PDF.Core.Name.Name × Object) := do
    skipSpace
    let key ← parseName
    skipSpace
    let value ← parseObjectFuel fuel
    pure (key, value)

  /-- Parse a dictionary `<< /Key value ... >>` (PDF32000-1:2008 §7.3.7),
      returning its internal association-array representation (see
      `Data.PDF.Core.Object`'s module doc-comment). -/
  def parseDictFuel : Nat → Parser (Array (Data.PDF.Core.Name.Name × Object))
    | 0 => fail "parseDict: too deeply nested"
    | fuel + 1 => do
      skipString "<<"
      let rec loop (fuel : Nat) (acc : Array (Data.PDF.Core.Name.Name × Object)) :
          Parser (Array (Data.PDF.Core.Name.Name × Object)) :=
        match fuel with
        | 0 => fail "parseDict: too deeply nested"
        | fuel + 1 => do
          skipSpace
          attempt (skipString ">>" *> pure acc) <|> do
            let kv ← parseKeyFuel fuel
            loop fuel (acc.push kv)
      loop fuel #[]

  /-- Parse an array `[ obj ... ]` (PDF32000-1:2008 §7.3.6). -/
  def parseArrayFuel : Nat → Parser (Array Object)
    | 0 => fail "parseArray: too deeply nested"
    | fuel + 1 => do
      skipByte '['.toUInt8
      let rec loop (fuel : Nat) (acc : Array Object) : Parser (Array Object) :=
        match fuel with
        | 0 => fail "parseArray: too deeply nested"
        | fuel + 1 => do
          skipSpace
          attempt (skipByte ']'.toUInt8 *> pure acc) <|> do
            let o ← parseObjectFuel fuel
            loop fuel (acc.push o)
      loop fuel #[]
end

/-- Parse any PDF object. Public entry point: seeds `fuel` from the parser's
    own remaining input, per the module doc-comment. -/
def parseObject : Parser Object := fun it => parseObjectFuel it.remainingBytes it

/-- Parse a dictionary, returning the public `Dict` type. -/
def parseDict : Parser Dict := fun it =>
  match parseDictFuel it.remainingBytes it with
  | .success rem entries => .success rem (Std.HashMap.ofList entries.toList)
  | .error pos err => .error pos err

/-- Parse an array of objects. -/
def parseArray : Parser (Array Object) := fun it => parseArrayFuel it.remainingBytes it

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
