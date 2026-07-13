/-
  Linen.Database.SQLite.Simple.Time.Implementation — SQLite date/time
  parsing and rendering

  Module #7 of `docs/imports/sqlite-simple/dependencies.md`. Converts
  between `Data.Time`'s `Day`/`UTCTime`/`TimeOfDay`/`TimeZone` (this
  library's `Linen.Data.Time.*` port) and the textual date/time formats
  SQLite's own date functions accept: `YYYY-MM-DD`, `YYYY-MM-DD
  HH:MM:SS[.SSS]`, and the same shape with a `T` separator (ISO 8601
  `DateTime`), each optionally followed by a `±HH:MM`/`±HH` UTC offset or a
  trailing `Z`.

  ## Deviation from `docs/imports/sqlite-simple/dependencies.md`

  The dependency plan additionally lists "Julian-day and Unix-epoch numeric
  forms" as part of this module's scope. Checked directly against upstream's
  `Database.SQLite.Simple.Time.Implementation` (both the `master` branch and
  the `sqlite-simple-0.4.19.0` release tarball): that module parses and
  renders only the textual forms above. SQLite's own date functions
  separately accept Julian-day/Unix-epoch *modifiers* as a general feature of
  its date/time API, but `sqlite-simple`'s `Time.Implementation` never
  parses them — a numeric SQLite column value reaches `FromField`'s numeric
  instances directly, bypassing this module entirely. So this port matches
  upstream's actual (textual-only) scope rather than the plan's broader
  description.

  ## Design

  Built on `Std.Internal.Parsec.String` (substituting `attoparsec`, per the
  dependency plan's precedence note). Every fixed-width field (month, day,
  hour, minute, second, and the `HH`/`MM` of a UTC offset) is parsed as
  exactly two ASCII digits; the year is parsed as a greedy run of at least
  four digits, mirroring upstream's own leniency there. Fractional seconds
  (up to millisecond precision, per `Linen.Data.Time.LocalTime`'s module
  doc) are parsed via `Float.ofScientific`, which reads a decimal mantissa
  and digit count exactly, with no intermediate rounding.

  Applying a UTC offset (or normalizing a `UTCTime` back into calendar
  fields) is done in integer milliseconds-since-midnight, using `Int.fdiv`
  for the floor-division that lets a time roll over into the previous/next
  calendar day — the same style `Linen.Data.Time.Calendar`'s Gregorian
  conversion uses, and one that avoids ever converting a negative `Float` to
  an unsigned integer.

  As with `Linen.Data.Time.Clock`'s `UTCTime` (nanoseconds since the epoch,
  stored as a `Nat`), dates before 1970-01-01 cannot be represented by this
  port; `dayAndTimeOfDayToUTCTime` clamps such a result to the epoch rather
  than wrapping.
-/

import Linen.Data.Time.Calendar
import Linen.Data.Time.Clock
import Linen.Data.Time.LocalTime
import Std.Internal.Parsec.String

namespace Database.SQLite.Simple.Time

open Data.Time (Day UTCTime TimeOfDay TimeZone)
open Std.Internal.Parsec Std.Internal.Parsec.String

-- ────────────────────────────────────────────────────────────────────
-- Zero-padded rendering helpers
-- ────────────────────────────────────────────────────────────────────

/-- Render `n` in decimal, left-padded with `'0'` to at least `width`
    characters. -/
private def padZeroN (width : Nat) (n : Nat) : String :=
  let s := toString n
  if s.length ≥ width then s else String.ofList (List.replicate (width - s.length) '0') ++ s

@[inline] private def padZero2 (n : Nat) : String := padZeroN 2 n
@[inline] private def padZero3 (n : Nat) : String := padZeroN 3 n

/-- Render a proleptic-Gregorian year, zero-padded to 4 digits for the
    ordinary `[0, 9999]` range upstream's own formatter targets; years
    outside that range (a case upstream's own `pad4` handles inconsistently
    for negative inputs, see the module doc's precedence note on
    simplifications) are rendered as a plain decimal. -/
private def padYear (y : Int) : String :=
  if 0 ≤ y then padZeroN 4 y.toNat else toString y

-- ────────────────────────────────────────────────────────────────────
-- Millisecond-of-day arithmetic
-- ────────────────────────────────────────────────────────────────────

/-- The time of day as whole milliseconds since midnight (always in
    `[0, 86400000)` for a valid `TimeOfDay`). -/
private def timeOfDayToMillis (t : TimeOfDay) : Int :=
  let secWhole := t.sec.floor.toUInt64.toNat
  let fracMillis := ((t.sec - t.sec.floor) * 1000).round.toUInt64.toNat
  (t.hour : Int) * 3600000 + (t.minute : Int) * 60000 + (secWhole : Int) * 1000 + (fracMillis : Int)

/-- Inverse of `timeOfDayToMillis`, for `msInDay ∈ [0, 86400000)`. -/
private def millisToTimeOfDay (msInDay : Nat) : TimeOfDay :=
  let hour := msInDay / 3600000
  let rem1 := msInDay % 3600000
  let minute := rem1 / 60000
  let rem2 := rem1 % 60000
  let secWhole := rem2 / 1000
  let fracMillis := rem2 % 1000
  TimeOfDay.ofHourMinuteSec hour minute (secWhole.toFloat + fracMillis.toFloat / 1000)

/-- Shift a local `(day, timeOfDay)` pair by a UTC offset (in minutes east of
    UTC), rolling the calendar day over as needed — the substitute for
    upstream's `localToUTCTimeOfDay`. -/
private def shiftByOffsetMinutes (day : Day) (t : TimeOfDay) (offsetMinutes : Int) :
    Day × TimeOfDay :=
  let ms := timeOfDayToMillis t - offsetMinutes * 60000
  let dayDelta := ms.fdiv 86400000
  let msInDay := (ms - dayDelta * 86400000).toNat
  (Day.addDays dayDelta day, millisToTimeOfDay msInDay)

/-- The Unix epoch, `1970-01-01`. -/
private def epochDay : Day := Day.fromGregorian 1970 1 1

/-- Combine a UTC calendar day and time-of-day into a `UTCTime`. Pre-epoch
    dates are clamped to the epoch (see the module doc). -/
def dayAndTimeOfDayToUTCTime (day : Day) (t : TimeOfDay) : UTCTime :=
  let days : Int := Day.diffDays day epochDay
  let ms := timeOfDayToMillis t
  UTCTime.ofNanosSinceEpoch ((max 0 (days * 86400000 + ms)).toNat * 1000000)

/-- Decompose a `UTCTime` back into its UTC calendar day and time-of-day. -/
def utcTimeToDayAndTimeOfDay (t : UTCTime) : Day × TimeOfDay :=
  let totalMs : Int := (t.nanosSinceEpoch : Int) / 1000000
  let dayDelta := totalMs.fdiv 86400000
  let msInDay := (totalMs - dayDelta * 86400000).toNat
  (Day.addDays dayDelta epochDay, millisToTimeOfDay msInDay)

-- ────────────────────────────────────────────────────────────────────
-- Parsers
-- ────────────────────────────────────────────────────────────────────

/-- Parse exactly `n` ASCII digits into a `Nat`, failing (without consuming
    input) if fewer are available or a non-digit is found — the substitute
    for upstream's fixed-width `digits` helper. -/
private def fixedDigits (n : Nat) (label : String) : Parser Nat := attempt do
  let s ← take n
  if s.toList.all Char.isDigit then
    return s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  else
    fail s!"{label} is not {n} digits"

/-- Parse a greedy run of at least 4 ASCII digits as a year, matching
    upstream's leniency (a year may have more than 4 digits). -/
private def yearDigits : Parser Int := do
  let s ← many1Chars digit
  if s.length < 4 then
    fail "year must consist of at least 4 digits"
  else
    return Int.ofNat (s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0)

/-- Parse a calendar date, `YYYY-MM-DD`. -/
def dayParser : Parser Day := do
  let y ← yearDigits
  skipChar '-'
  let m ← fixedDigits 2 "month"
  skipChar '-'
  let d ← fixedDigits 2 "day"
  match Day.fromGregorianValid y m d with
  | some day => return day
  | none => fail "invalid date"

/-- Parse the fractional part of a seconds field, `.SSS…`, as a value in
    `[0, 1)`. -/
private def fractionalSeconds : Parser Float := do
  skipChar '.'
  let ds ← many1Chars digit
  let n := ds.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  return Float.ofScientific n true ds.length

/-- Parse a `:SS[.SSS…]` seconds field. -/
private def secondsAndFraction : Parser (Nat × Float) := do
  skipChar ':'
  let s ← fixedDigits 2 "seconds"
  let frac ← (attempt fractionalSeconds) <|> pure (0.0 : Float)
  return (s, frac)

/-- Parse a time of day, `HH:MM[:SS[.SSS…]]` — seconds (and the fractional
    part) may be omitted, matching upstream. -/
def timeOfDayParser : Parser TimeOfDay := do
  let h ← fixedDigits 2 "hours"
  skipChar ':'
  let m ← fixedDigits 2 "minutes"
  let (s, frac) ← (attempt secondsAndFraction) <|> pure ((0 : Nat), (0.0 : Float))
  match TimeOfDay.makeValid h m (s.toFloat + frac) with
  | some t => return t
  | none => fail "invalid time of day"

/-- Parse a UTC offset, `±HH[:MM]`. -/
def timeZoneParser : Parser TimeZone := do
  let sign ← pchar '+' <|> pchar '-'
  let hours ← fixedDigits 2 "timezone"
  let mins ← (attempt (skipChar ':' *> fixedDigits 2 "timezone minutes")) <|> pure 0
  let absMinutes : Int := ((hours * 60 + mins : Nat) : Int)
  return TimeZone.minutesToTimeZone (if sign == '+' then absMinutes else -absMinutes)

/-- Parse a full SQLite timestamp: a date, a `' '`/`'T'` separator, a time
    of day, and an optional UTC offset (`±HH:MM`, a trailing `Z`, or nothing
    at all — all three default the same way upstream does, to UTC). The
    result is normalized to UTC. -/
def utcTimeParser : Parser UTCTime := do
  let d ← dayParser
  let _ ← pchar ' ' <|> pchar 'T'
  let t ← timeOfDayParser
  let tz ← (attempt timeZoneParser) <|> (pchar 'Z' *> pure TimeZone.utc) <|> pure TimeZone.utc
  let (d', t') := shiftByOffsetMinutes d t tz.minutes
  return dayAndTimeOfDayToUTCTime d' t'

-- ────────────────────────────────────────────────────────────────────
-- Public entry points
-- ────────────────────────────────────────────────────────────────────

/-- Parse a `Day` from `YYYY-MM-DD`, requiring the whole string to match. -/
def parseDay (s : String) : Except String Day :=
  Parser.run (dayParser <* eof) s

/-- Parse a `UTCTime` from a full SQLite timestamp, requiring the whole
    string to match. -/
def parseUTCTime (s : String) : Except String UTCTime :=
  Parser.run (utcTimeParser <* eof) s

/-- Render a `Day` as `YYYY-MM-DD`. -/
def dayToString (d : Day) : String :=
  let (y, m, day) := d.toGregorian
  s!"{padYear y}-{padZero2 m}-{padZero2 day}"

/-- Render a `TimeOfDay` as `HH:MM:SS` or `HH:MM:SS.SSS` (the latter only
    when there is a nonzero fractional part), matching upstream's
    millisecond-precision output. -/
def timeOfDayToString (t : TimeOfDay) : String :=
  let secWhole := t.sec.floor.toUInt64.toNat
  let fracMillis := ((t.sec - t.sec.floor) * 1000).round.toUInt64.toNat
  let secStr := if fracMillis == 0 then padZero2 secWhole else s!"{padZero2 secWhole}.{padZero3 fracMillis}"
  s!"{padZero2 t.hour}:{padZero2 t.minute}:{secStr}"

/-- Render a `TimeZone` as `±HH` (when the offset is a whole number of
    hours) or `±HH:MM`. -/
def timeZoneToString (tz : TimeZone) : String :=
  let sign := if tz.minutes ≥ 0 then "+" else "-"
  let absMinutes := tz.minutes.natAbs
  let h := absMinutes / 60
  let m := absMinutes % 60
  if m == 0 then s!"{sign}{padZero2 h}" else s!"{sign}{padZero2 h}:{padZero2 m}"

/-- Render a `UTCTime` as `YYYY-MM-DD HH:MM:SS[.SSS]`, matching SQLite's own
    `datetime()` convention of omitting an explicit UTC-offset suffix. -/
def utcTimeToString (t : UTCTime) : String :=
  let (d, tod) := utcTimeToDayAndTimeOfDay t
  s!"{dayToString d} {timeOfDayToString tod}"

end Database.SQLite.Simple.Time
