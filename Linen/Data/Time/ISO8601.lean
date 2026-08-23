/-
  Linen.Data.Time.ISO8601 — ISO 8601 basic-format rendering

  Renders a `Data.Time.UTCTime` in ISO 8601 *basic* format: no separators,
  as in `20130524T000000Z`. The counterpart of Haskell's
  `Data.Time.Format.ISO8601`.

  ## Provenance
  `Linen.Data.Time.Clock` gives the instant but no way to print it, and
  `Std.Time`'s canned `Formats.iso8601` is *extended* format
  (`uuuu-MM-dd'T'HH:mm:ssXXX`) — separators and all. Basic format is what
  wire protocols use, AWS Signature Version 4 among them: it wants
  `YYYYMMDD'T'HHMMSS'Z'` in the `X-Amz-Date` header and `YYYYMMDD` as the
  credential scope's date, and rejects anything else.

  ## Design
  Fields are read off `Std.Time.DateTime` and zero-padded directly, rather
  than going through the `datespec` format DSL. The output is fixed and
  small enough that the explicit version is easier to read and to check
  against the specification than a pattern string would be.

  Rendering is always in UTC, so the zone designator is a literal `Z`;
  these functions take a `UTCTime`, which has no other zone to be in.
-/
import Linen.Data.Time.Clock

namespace Data.Time.ISO8601

-- ── Padding ──

/-- Render `n` in decimal, left-padded with zeros to at least `width`
    digits. Values wider than `width` are not truncated — a five-digit year
    stays five digits rather than silently becoming a wrong four-digit one.
    $$|\text{pad}(w, n)| = \max(w, |\text{digits}(n)|)$$

    Truncating subtraction does the case split: when the number is already
    wide enough, `width - length` is `0` and no zeros are prepended. -/
def pad (width : Nat) (n : Nat) : String :=
  String.ofList (List.replicate (width - (toString n).length) '0') ++ toString n

-- ── Components ──

/-- The UTC calendar breakdown of an instant. -/
private def parts (t : UTCTime) : Std.Time.DateTime :=
  Std.Time.DateTime.ofTimestampWithZone t.toTimestamp .UTC

-- ── Basic format ──

/-- The date alone, `YYYYMMDD`.

    This is SigV4's credential-scope date. Years before 1 CE render with the
    minus sign that `toString` produces on a negative `Int`; the format has
    no defined behaviour there and no caller of this module reaches it. -/
def basicDate (t : UTCTime) : String :=
  let d := parts t
  pad 4 d.year.toNat ++ pad 2 d.month.val.toNat ++ pad 2 d.day.val.toNat

/-- The time alone, `HHMMSS`. -/
def basicTime (t : UTCTime) : String :=
  let d := parts t
  pad 2 d.hour.val.toNat ++ pad 2 d.minute.val.toNat ++ pad 2 d.second.val.toNat

/-- Date and time with the `T` separator and a `Z` zone designator,
    `YYYYMMDDTHHMMSSZ` — exactly 16 characters.

    This is SigV4's `X-Amz-Date`. -/
def basicDateTime (t : UTCTime) : String :=
  basicDate t ++ "T" ++ basicTime t ++ "Z"

-- ── Extended format ──

/-- The date alone with separators, `YYYY-MM-DD`. -/
def extendedDate (t : UTCTime) : String :=
  let d := parts t
  pad 4 d.year.toNat ++ "-" ++ pad 2 d.month.val.toNat ++ "-" ++ pad 2 d.day.val.toNat

/-- Date and time with separators, `YYYY-MM-DDTHH:MM:SSZ`. -/
def extendedDateTime (t : UTCTime) : String :=
  let d := parts t
  extendedDate t ++ "T" ++ pad 2 d.hour.val.toNat ++ ":" ++ pad 2 d.minute.val.toNat ++ ":" ++
    pad 2 d.second.val.toNat ++ "Z"

-- ── Proofs ──

/-- Padding widens to `width` and never truncates: the result is exactly as
    long as the wider of the two. -/
theorem pad_length (width n : Nat) :
    (pad width n).length = max width (toString n).length := by
  simp only [pad, String.length_append, String.length_ofList, List.length_replicate]
  omega

/-- In particular, padding never produces something shorter than asked for. -/
theorem pad_length_ge (width n : Nat) : (pad width n).length ≥ width := by
  simp only [pad_length]
  omega

end Data.Time.ISO8601
