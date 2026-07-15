/-
  Linen.Database.Redis.Types — `RedisArg`/`RedisResult` encode/decode classes

  ## Haskell source
  `Database.Redis.Types` from https://hackage.haskell.org/package/hedis
  (module 5 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Types.hs`.

  ## `bytestring-lexing` substitution
  Upstream's `RedisResult Integer`/`Int64`/`Double` `decode` instances use
  `Data.ByteString.Lex.Integral`'s `readSigned`/`readDecimal` and
  `Data.ByteString.Lex.Fractional`'s `readSigned`/`readExponential` to parse
  a bulk/single-line reply payload into a number. Per
  `docs/imports/hedis/dependencies.md`'s external-dependency note, these are
  substituted by the two small hand-written parsers below
  (`readSignedDecimal`, `readSignedExponential`), built directly over
  `ByteArray.toList` (plain structural recursion on `List UInt8`, no
  `partial`, no fuel). Like upstream's `readSigned`/`readDecimal` /
  `readExponential`, both parsers only consume a *prefix* of the input and
  silently ignore any trailing bytes.

  ## `Either Reply a` → `Except Reply a`
  Upstream's `decode :: Reply -> Either Reply a` (`Left` = failure, `Right`
  = success) is ported as `decode : Reply → Except Reply a`
  (`Except.error` = failure, `Except.ok` = success) — Lean's stdlib
  `Either`-shaped sum type for exactly this purpose.

  ## Deviations
  - `RedisResult RedisType`'s upstream instance calls `error` (a partial,
    crashing function) on an unrecognised type-name string. AGENTS.md
    forbids introducing crashes/`partial`; unrecognised strings decode to
    `Except.error r` instead, which is a strictly safer behaviour and still
    signals "this reply could not be decoded as a `RedisType`" to the
    caller.
  - Upstream's `RedisResult [(k, v)]` overlaps `RedisResult [a]` via GHC's
    `OVERLAPPABLE`/instance-selection machinery (a flat multi-bulk of `2n`
    elements decodes to `n` key/value pairs, taken two elements at a time).
    Lean's typeclass resolution has no such overlap mechanism — adding both
    as instances of `RedisResult (List α)` would make the more specific one
    unreachable (or ambiguous) rather than preferred. It is ported as the
    plain function `decodeKeyValuePairs` instead of a `RedisResult`
    instance; callers needing this shape (e.g. `HGETALL`) call it directly.
-/
import Linen.Database.Redis.Protocol

namespace Database.Redis.Types

open Database.Redis.Protocol (Reply)

-- ── Classes of types Redis understands ──

/-- A value that can be encoded as a RESP request argument. -/
class RedisArg (α : Type) where
  encode : α → ByteArray

/-- A value that can be decoded from a `Reply`. `Except.error` carries the
    original `Reply` back, so callers can inspect why decoding failed. -/
class RedisResult (α : Type) where
  decode : Reply → Except Reply α

export RedisArg (encode)
export RedisResult (decode)

-- ── `RedisArg` instances ──

instance : RedisArg ByteArray where
  encode := id

instance : RedisArg Int where
  encode n := (toString n).toUTF8

instance : RedisArg Float where
  encode a :=
    if a.isInf ∧ a > 0 then "+inf".toUTF8
    else if a.isInf ∧ a < 0 then "-inf".toUTF8
    else (toString a).toUTF8

-- ── Hand-written number parsers (the `bytestring-lexing` substitution) ──

/-- Consume a run of ASCII digits from the front of `l`, folding them into
    `acc` (most-significant digit first), counting how many digits were
    consumed. Structural recursion on `l`. -/
private def takeDigitsAux : List UInt8 → Nat → Nat → Nat × Nat × List UInt8
  | [], acc, cnt => (acc, cnt, [])
  | b :: rest, acc, cnt =>
    if '0'.toUInt8 ≤ b ∧ b ≤ '9'.toUInt8 then
      takeDigitsAux rest (acc * 10 + (b - '0'.toUInt8).toNat) (cnt + 1)
    else
      (acc, cnt, b :: rest)

/-- Consume a run of ASCII digits from the front of `l`, returning
    `(value, digitCount, rest)`. -/
private def takeDigits (l : List UInt8) : Nat × Nat × List UInt8 :=
  takeDigitsAux l 0 0

/-- Parse a signed decimal integer from the *prefix* of `bytes`, ignoring
    any trailing bytes — matching upstream's `readSigned readDecimal`
    (which likewise ignores what follows the parsed number). `none` if no
    digits were found. -/
def readSignedDecimal (bytes : ByteArray) : Option Int :=
  let l0 := bytes.toList
  let (neg, l1) := match l0 with
    | b :: rest => if b == '-'.toUInt8 then (true, rest) else (false, l0)
    | [] => (false, l0)
  let (n, count, _) := takeDigits l1
  if count == 0 then
    none
  else
    some (if neg then -(Int.ofNat n) else Int.ofNat n)

/-- Parse a signed decimal-or-exponential floating-point number from the
    *prefix* of `bytes` (`[-]digits['.'digits][('e'|'E')['+'|'-']digits]`),
    ignoring any trailing bytes — matching upstream's
    `readSigned readExponential`. `none` if no digits were found. -/
def readSignedExponential (bytes : ByteArray) : Option Float :=
  let l0 := bytes.toList
  let (neg, l1) := match l0 with
    | b :: rest => if b == '-'.toUInt8 then (true, rest) else (false, l0)
    | [] => (false, l0)
  let (intVal, intCount, l2) := takeDigits l1
  let (mantissa, fracCount, l3) := match l2 with
    | b :: rest => if b == '.'.toUInt8 then takeDigitsAux rest intVal 0 else (intVal, 0, l2)
    | [] => (intVal, 0, l2)
  if intCount == 0 ∧ fracCount == 0 then
    none
  else
    let explicitExp : Int := match l3 with
      | b :: rest =>
        if b == 'e'.toUInt8 ∨ b == 'E'.toUInt8 then
          let (expNeg, l4) := match rest with
            | b' :: r => if b' == '-'.toUInt8 then (true, r) else if b' == '+'.toUInt8 then (false, r) else (false, rest)
            | [] => (false, rest)
          let (expDigits, _, _) := takeDigits l4
          if expNeg then -(Int.ofNat expDigits) else Int.ofNat expDigits
        else
          0
      | [] => 0
    let effectiveExp : Int := explicitExp - Int.ofNat fracCount
    let value := Float.ofScientific mantissa (effectiveExp < 0) effectiveExp.natAbs
    some (if neg then -value else value)

-- ── `RedisResult` instances ──

/-- A Redis status reply (`+OK`, `+PONG`, or an arbitrary status string). -/
inductive Status where
  | ok
  | pong
  | status (s : ByteArray)
  deriving BEq, Inhabited

/-- The Redis key type reported by e.g. `TYPE`. -/
inductive RedisType where
  | none
  | string
  | hash
  | list
  | set
  | zset
  deriving BEq, DecidableEq, Repr, Inhabited

instance : RedisResult Reply where
  decode := Except.ok

instance : RedisResult ByteArray where
  decode
    | .singleLine s => Except.ok s
    | .bulk (some s) => Except.ok s
    | r => Except.error r

instance : RedisResult Int where
  decode
    | .integer n => Except.ok n
    | r =>
      match decode r with
      | Except.error _ => Except.error r
      | Except.ok (bs : ByteArray) =>
        match readSignedDecimal bs with
        | some n => Except.ok n
        | none => Except.error r

instance : RedisResult Float where
  decode r :=
    match decode r with
    | Except.error _ => Except.error r
    | Except.ok (bs : ByteArray) =>
      match readSignedExponential bs with
      | some f => Except.ok f
      | none => Except.error r

instance : RedisResult Status where
  decode
    | .singleLine s =>
      Except.ok (if s == "OK".toUTF8 then Status.ok
                 else if s == "PONG".toUTF8 then Status.pong
                 else Status.status s)
    | r => Except.error r

instance : RedisResult RedisType where
  decode
    | .singleLine s =>
      if s == "none".toUTF8 then Except.ok RedisType.none
      else if s == "string".toUTF8 then Except.ok RedisType.string
      else if s == "hash".toUTF8 then Except.ok RedisType.hash
      else if s == "list".toUTF8 then Except.ok RedisType.list
      else if s == "set".toUTF8 then Except.ok RedisType.set
      else if s == "zset".toUTF8 then Except.ok RedisType.zset
      -- Upstream `error`s (crashes) here; see the module doc-comment's
      -- "Deviations" section for why this decodes to a safe failure
      -- instead.
      else Except.error (Reply.singleLine s)
    | r => Except.error r

instance : RedisResult Bool where
  decode
    | .integer 1 => Except.ok true
    | .integer 0 => Except.ok false
    | .bulk none => Except.ok false
    | r => Except.error r

instance [RedisResult α] : RedisResult (Option α) where
  decode
    | .bulk none => Except.ok none
    | .multiBulk none => Except.ok none
    | r => Option.some <$> decode r

instance [RedisResult α] : RedisResult (List α) where
  decode
    | .multiBulk (some rs) => rs.mapM decode
    | r => Except.error r

instance [RedisResult α] [RedisResult β] : RedisResult (α × β) where
  decode
    | .multiBulk (some [x, y]) => do
      let a ← decode x
      let b ← decode y
      pure (a, b)
    | r => Except.error r

/-- Decode a flat multi-bulk reply of `2n` elements as `n` key/value pairs
    (e.g. `HGETALL`'s reply shape), taken two elements at a time. See the
    module doc-comment's "Deviations" section for why this is a plain
    function rather than a `RedisResult (List (κ × ν))` instance. -/
def decodeKeyValuePairs [RedisResult κ] [RedisResult ν] (r : Reply) :
    Except Reply (List (κ × ν)) :=
  match r with
  | .multiBulk (some rs) => go rs
  | _ => Except.error r
where
  go : List Reply → Except Reply (List (κ × ν))
    | [] => Except.ok []
    | [_] => Except.error r
    | r1 :: r2 :: rest => do
      let k ← decode r1
      let v ← decode r2
      let kvs ← go rest
      pure ((k, v) :: kvs)

end Database.Redis.Types
