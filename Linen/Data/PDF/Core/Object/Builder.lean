/-
  Data.PDF.Core.Object.Builder — render `Object` to bytes

  Ports `Pdf.Core.Object.Builder` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Object/Builder.hs`),
  module 7 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  ## Design

  - Upstream's `buildObject :: Object -> Builder` is a **partial** function:
    `buildObject (Stream _) = error "buildObject: please don't pass streams
    to me"`. Per `AGENTS.md`'s explicit ban on `sorry`/panics as a substitute
    for real totality treatment, `buildObject` (and the `buildDict`/
    `buildArray` it mutually recurses with, since a dictionary or array can
    itself contain a `stream` `Object`) are ported as **total** functions
    returning `Except String Builder`: the `stream` case yields
    `.error "buildObject: please don't pass streams to me"` — the exact same
    message, but as an ordinary caught value instead of an uncatchable
    `error` panic.

  - The mutual recursion between `buildObject`, `buildDictEntries` (which
    walks a dictionary's entries, building each key/value pair), and
    `buildArrayItems` (which walks an array's items) is written as direct
    structural pattern-matching recursion over `List`, one cons-cell at a
    time — not via a generic fold combinator. A generic-combinator
    formulation (`List.foldlM`, etc.) obscures the decreasing measure from
    Lean's structural-recursion checker enough that it can't see the mutual
    recursion terminates; the direct `[] | x :: xs` form lets it see the
    measure (each recursive call is on a strictly smaller sub-`Object` or a
    shorter list) automatically, with no need for a hand-written
    `termination_by`.

  - Upstream's `buildNumber` uses `Scientific.floatingOrInteger`, which
    (unlike `Data.Scientific.toBoundedInteger` in `Linen/Data/Scientific.lean`,
    used by `Data.PDF.Core.Object.Util`) has *no* fixed-width bound — it is
    polymorphic in the target `Integral` type, and Haskell's own `intDec`
    call site there ultimately runs over an arbitrary-precision `Integer`.
    So `buildNumber` below reconstructs the integer value directly from a
    `Data.Scientific`'s `coefficient`/`base10Exponent` fields (`toExactInt`)
    rather than going through the bounded `toBoundedInteger`, preserving
    upstream's unbounded-integer rendering.

  - Upstream's fractional-number branch calls `Text.Printf`'s
    `printf "%f" (d :: Double)`, i.e. fixed-point (never exponent) notation
    with (`printf`'s C-derived default) six digits after the decimal point.
    `linen` has no `printf`/`Text.Printf` port, and none is warranted for one
    call site, so `formatFixed` below is a small local helper reproducing
    exactly that: six-decimal-digit fixed-point formatting with round-half-up
    rounding, ported directly against Lean core `Float`'s arithmetic
    (`Float.floor`, `Float.toUInt64`) — see its doc-comment for the one
    caveat this brings (very large/small floats fall outside `UInt64`'s
    range; the same practical constraint upstream has via C's underlying
    `%f` machinery, which is also finite-precision).

  - Upstream's `buildString` picks a literal (parenthesised, escaped) vs. hex
    encoding by testing `Char8.all isPrint`, i.e. Haskell's full (Latin-1)
    `Data.Char.isPrint` classification. This port uses the plain ASCII
    printable range `0x20`–`0x7E` instead: a minor, non-correctness-affecting
    simplification — both encodings are always valid PDF string syntax, so
    this only ever changes the cosmetic choice between the two equally-valid
    encodings, never the decoded content. `linen` has no `Data.ByteString.
    Base16` port either, so hex-pair encoding is a two-line local helper
    (`hexByte`) rather than a new import, matching `Data.ByteString.Builder`'s
    own existing `wordHex`, but zero-padded to two digits per byte (unlike
    `wordHex`, which is for decimal-style multi-digit numbers, not
    fixed-width byte pairs).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.ByteString.Builder
import Linen.Data.ByteString.Lazy

namespace Data.PDF.Core.Object.Builder

open Data.PDF.Core.Object
open Data.ByteString (Builder)
open Data.ByteString.Builder

/-! ── Fixed-point float formatting ── -/

/-- Format `x` in fixed-point (never exponent) notation with exactly six
    digits after the decimal point, round-half-up — matching `Text.Printf`'s
    `printf "%f"` on a `Double`.

    Caveat: the whole-number part is materialised via `Float.toUInt64`, so
    this is only exact for `|x| < 2^64 / 10^6`; PDF numeric objects are
    always far smaller than that in practice (PDF32000-1:2008 §7.3.3 already
    limits real numbers to roughly this range on conforming readers). -/
def formatFixed (x : Float) : String :=
  let neg := x < 0.0
  let ax := if neg then -x else x
  let scaled := (ax * 1000000.0 + 0.5).floor
  let scaledNat := scaled.toUInt64.toNat
  let intPart := scaledNat / 1000000
  let fracPart := scaledNat % 1000000
  let fracStr := toString fracPart
  let pad := String.ofList (List.replicate (6 - fracStr.length) '0')
  let sign := if neg then "-" else ""
  s!"{sign}{intPart}.{pad}{fracStr}"

/-! ── Hex-pair byte encoding ── -/

/-- Encode a byte as two lowercase hex digits, e.g. `0x0f ↦ "0f"`. -/
def hexByte (b : UInt8) : String :=
  let digits := Nat.toDigits 16 b.toNat
  let padded := if digits.length < 2 then '0' :: digits else digits
  String.ofList padded

/-! ── Numbers, booleans, names ── -/

/-- Reconstruct the exact (unbounded) integer value of an integral
    `Data.Scientific`. Only meaningful when `s.isInteger`. -/
def toExactInt (s : Data.Scientific) : Int :=
  let n := s.normalize
  if n.base10Exponent ≥ 0 then
    n.coefficient * (10 : Int) ^ n.base10Exponent.toNat
  else
    n.coefficient

/-- Build a number: an integral `Data.Scientific` renders as a plain decimal
    integer; a fractional one renders in fixed-point notation
    (`formatFixed`). Mirrors upstream's
    `either bFloat intDec . floatingOrInteger`. -/
def buildNumber (n : Data.Scientific) : Builder :=
  if n.isInteger then
    intDec (toExactInt n)
  else
    stringUtf8 (formatFixed n.toRealFloat)

/-- Build a bool as `true`/`false`. -/
def buildBool (b : Bool) : Builder :=
  stringUtf8 (if b then "true" else "false")

/-- Build a name as `/name` (escaping is not yet implemented, matching
    upstream's `-- XXX: escaping` comment on `buildName`). -/
def buildName (n : Name) : Builder :=
  char8 '/' ++ byteString n.toByteString

/-- Intercalate a separator `Builder` between a list of `Builder`s. -/
def intercalate (sep : Builder) : List Builder → Builder
  | [] => Builder.empty
  | [x] => x
  | x :: xs => x ++ sep ++ intercalate sep xs

/-! ── Strings ── -/

/-- Escape one printable-string byte for a literal `(...)`-delimited PDF
    string, mirroring upstream's `escape`. -/
def escapeByte (c : Char) : String :=
  match c with
  | '(' => "\\("
  | ')' => "\\)"
  | '\\' => "\\\\"
  | '\n' => "\\n"
  | '\r' => "\\r"
  | '\t' => "\\t"
  | '\x08' => "\\b"
  | ch => String.singleton ch

/-- Is `b` an ASCII printable byte (`0x20`–`0x7E`)? See the module
    doc-comment for why this replaces upstream's full Latin-1 `isPrint`. -/
def isAsciiPrintable (b : UInt8) : Bool :=
  b ≥ 0x20 && b ≤ 0x7E

/-- Build a string: a literal `(...)`-delimited (escaped) string if every
    byte is ASCII-printable, otherwise a `<...>`-delimited hex string.
    Mirrors upstream's `buildString`. -/
def buildString (s : Data.ByteString) : Builder :=
  if Data.ByteString.all isAsciiPrintable s then
    let escaped := String.join (s.unpack.map (fun b => escapeByte (Char.ofNat b.toNat)))
    char8 '(' ++ stringUtf8 escaped ++ char8 ')'
  else
    let hex := String.join (s.unpack.map hexByte)
    char8 '<' ++ stringUtf8 hex ++ char8 '>'

/-- Build an indirect reference as `i g R`. -/
def buildRef (r : Ref) : Builder :=
  intDec r.index ++ char8 ' ' ++ intDec r.generation ++ stringUtf8 " R"

/-! ── Objects, dictionaries, arrays ──

    See the module doc-comment for why `buildObject`/`buildDictEntries`/
    `buildArrayItems` are total (`Except String Builder`-returning) and
    mutually recursive via direct `List` pattern matching. -/

mutual
  /-- Build a flattened `key value key value ...` list of builders for a
      dictionary's entries (upstream's
      `concatMap (\(k,v) -> [buildName k, buildObject v])`). -/
  def buildDictEntries : List (Name × Object) → Except String (List Builder)
    | [] => .ok []
    | (k, v) :: rest => do
        let vb ← buildObject v
        let restBs ← buildDictEntries rest
        return buildName k :: vb :: restBs

  /-- Build the list of builders for an array's items. -/
  def buildArrayItems : List Object → Except String (List Builder)
    | [] => .ok []
    | x :: xs => do
        let bx ← buildObject x
        let bxs ← buildArrayItems xs
        return bx :: bxs

  /-- Render an inline object (without the `obj`/`endobj` wrapper). It is an
      error to pass a `stream`, because a stream can never be inlined — it
      must always be an indirect object (see `buildIndirectStream`). Mirrors
      upstream's `buildObject`, but as a total function (see the module
      doc-comment). -/
  def buildObject : Object → Except String Builder
    | .number n => .ok (buildNumber n)
    | .bool b => .ok (buildBool b)
    | .name n => .ok (buildName n)
    | .dictRaw entries => do
        let parts ← buildDictEntries entries.toList
        return stringUtf8 "<<" ++ intercalate (char8 ' ') parts ++ stringUtf8 ">>"
    | .array items => do
        let parts ← buildArrayItems items.toList
        return char8 '[' ++ intercalate (char8 ' ') parts ++ char8 ']'
    | .string s => .ok (buildString s)
    | .ref r => .ok (buildRef r)
    | .stream _ => .error "buildObject: please don't pass streams to me"
    | .null => .ok (stringUtf8 "null")
end

/-- Build a dictionary on its own (e.g. a stream's dictionary). -/
def buildDict (d : Dict) : Except String Builder := do
  let parts ← buildDictEntries d.toList
  return stringUtf8 "<<" ++ intercalate (char8 ' ') parts ++ stringUtf8 ">>"

/-- Build an array on its own. -/
def buildArray (items : Array Object) : Except String Builder := do
  let parts ← buildArrayItems items.toList
  return char8 '[' ++ intercalate (char8 ' ') parts ++ char8 ']'

/-! ── Streams and indirect objects ── -/

/-- Build a stream: its dictionary, followed by `stream\n`, its raw content,
    then `\nendstream`. The function makes no attempt to encode/encrypt the
    content — the caller must already have done so. -/
def buildStream (dict : Dict) (content : Data.ByteString.Lazy.LazyByteString) :
    Except String Builder := do
  let db ← buildDict dict
  return db ++ stringUtf8 "stream\n" ++ Data.ByteString.Builder.lazyByteString content ++
    stringUtf8 "\nendstream"

/-- Wrap `inner` as the body of an indirect object: `\ni g obj\n<inner>\nendobj\n`. -/
def buildObjectWith (ref : Ref) (inner : Builder) : Builder :=
  char8 '\n' ++ intDec ref.index ++ char8 ' ' ++ intDec ref.generation ++
    stringUtf8 " obj\n" ++ inner ++ stringUtf8 "\nendobj\n"

/-- Build an indirect object (anything except a stream — use
    `buildIndirectStream` for those). -/
def buildIndirectObject (ref : Ref) (object : Object) : Except String Builder := do
  let inner ← buildObject object
  return buildObjectWith ref inner

/-- Build an indirect stream. -/
def buildIndirectStream (ref : Ref) (dict : Dict)
    (dat : Data.ByteString.Lazy.LazyByteString) : Except String Builder := do
  let inner ← buildStream dict dat
  return buildObjectWith ref inner

end Data.PDF.Core.Object.Builder
