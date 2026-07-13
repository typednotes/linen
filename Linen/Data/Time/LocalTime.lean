/-
  Linen.Data.Time.LocalTime — time-of-day and UTC-offset local time

  Another small addition to `linen`'s `Time` port, alongside
  `Linen.Data.Time.Calendar`, made while porting `sqlite-simple`
  (`docs/imports/sqlite-simple/dependencies.md`, module #7): `Database.
  SQLite.Simple.Time.Implementation` renders/parses times against Haskell's
  `Data.Time.LocalTime.TimeOfDay`/`TimeZone`, which had no counterpart here.

  ## Design

  Per `docs/imports/Time/dependencies.md`'s status note, this module
  originally hand-rolled its own hour/minute/fractional-seconds
  representation before this codebase's import process had `Std.Time` in its
  precedence analysis. It is now built on `Std.Time.Time.PlainTime` (for
  `TimeOfDay`) and `Std.Time.Zoned.TimeZone.Offset` (for `TimeZone`), while
  keeping every public name/signature from the ad hoc port unchanged, so
  callers (e.g. `Linen.Database.SQLite.Simple.Time.Implementation`) are
  unaffected by the representation change.

  `TimeOfDay` mirrors upstream: an hour in `[0, 23]`, a minute in `[0, 59]`,
  and seconds (with sub-second precision) in `[0, 60)`, exposed as a `Float`
  (as the ad hoc port originally chose, since `linen` has no
  arbitrary-fixed-precision decimal type and SQLite's own textual formats
  never need more than millisecond precision — see
  `Linen.Database.SQLite.Simple.Time.Implementation`, the only consumer),
  even though the underlying `PlainTime` itself already carries exact
  nanosecond precision. `midnight` (`00:00:00`) is upstream's leap-second
  allowance (`[0, 61)`, to admit a positive leap second): SQLite's date
  functions themselves are leap-second-unaware, so this port keeps the
  ordinary `[0, 60)` bound.

  `TimeZone` mirrors upstream's `Data.Time.LocalTime.TimeZone`, reduced to
  just the signed offset in minutes (`minutes`); upstream's optional
  summer-time flag and textual abbreviation are display-only metadata never
  read by any of `sqlite-simple`'s date/time code, so they are dropped (the
  same reasoning that keeps this module on `Std.Time.TimeZone.Offset` rather
  than the fuller `Std.Time.TimeZone`, which does carry a name/abbreviation/
  DST flag).

  `localToUTCTimeOfDay`/`timeOfDayToTime`-style conversions between a
  `TimeOfDay` and a fractional day offset are folded into `Linen.Database.
  SQLite.Simple.Time.Implementation`, their only consumer, rather than
  exposed as general-purpose combinators here (upstream itself provides
  several redundant framings of the same arithmetic across `LocalTime` and
  `Clock.TAI`; only the one shape `sqlite-simple` needs is ported).
-/
import Std.Time

namespace Data.Time

/-- A time of day: hour `[0, 23]`, minute `[0, 59]`, and (possibly
    fractional) seconds `[0, 60)` — backed by `Std.Time.Time.PlainTime`.
    Mirrors `Data.Time.LocalTime.TimeOfDay`, minus the leap-second allowance
    (see the module doc). -/
structure TimeOfDay where
  /-- The underlying `Std.Time.Time.PlainTime`. -/
  toPlainTime : Std.Time.PlainTime
deriving Repr, DecidableEq, Inhabited

namespace TimeOfDay

instance : BEq TimeOfDay where
  beq a b := decide (a.toPlainTime = b.toPlainTime)

/-- Midnight, `00:00:00`. -/
def midnight : TimeOfDay := ⟨Std.Time.PlainTime.midnight⟩

/-- The hour component, `[0, 23]`. -/
@[inline] def hour (t : TimeOfDay) : Nat := t.toPlainTime.hour.val.toNat

/-- The minute component, `[0, 59]`. -/
@[inline] def minute (t : TimeOfDay) : Nat := t.toPlainTime.minute.val.toNat

/-- The (possibly fractional) seconds component, `[0, 60)`. -/
def sec (t : TimeOfDay) : Float :=
  t.toPlainTime.second.val.toNat.toFloat + t.toPlainTime.nanosecond.val.toNat.toFloat / 1000000000

/-- Build a `TimeOfDay` directly from an hour/minute/seconds triple, with no
    range validation — matching upstream's bare `TimeOfDay` data
    constructor (only `makeTimeOfDayValid`, ported as `makeValid` below,
    validates). Out-of-range components roll over via `PlainTime.
    ofNanoseconds`'s own modular arithmetic rather than being rejected. -/
def ofHourMinuteSec (hour minute : Nat) (sec : Float) : TimeOfDay :=
  let secWhole := sec.floor.toUInt64.toNat
  let fracNanos := ((sec - sec.floor) * 1000000000).round.toUInt64.toNat
  let totalNanos : Int :=
    (hour : Int) * 3600000000000 + (minute : Int) * 60000000000 +
      (secWhole : Int) * 1000000000 + (fracNanos : Int)
  ⟨Std.Time.PlainTime.ofNanoseconds (.ofInt totalNanos)⟩

/-- Build a `TimeOfDay`, validating every component is in range, matching
    `makeTimeOfDayValid` upstream. -/
def makeValid (hour minute : Nat) (sec : Float) : Option TimeOfDay :=
  if hour ≤ 23 && minute ≤ 59 && sec ≥ 0 && sec < 60 then
    some (ofHourMinuteSec hour minute sec)
  else
    none

/-- The time of day as a fraction of a day, in `[0, 1)`. -/
def toDayFraction (t : TimeOfDay) : Float :=
  t.toPlainTime.toNanoseconds.val.toNat.toFloat / 86400000000000.0

/-- Recover a `TimeOfDay` from a fraction of a day in `[0, 1)` (values outside
    that range are reduced modulo 1, matching how upstream's `timeToTimeOfDay`
    is always applied to an already-normalized `DiffTime`). -/
def ofDayFraction (frac : Float) : TimeOfDay :=
  let frac := frac - frac.floor
  let totalNanos := (frac * 86400000000000.0).round.toUInt64.toNat
  ⟨Std.Time.PlainTime.ofNanoseconds (.ofInt (totalNanos : Int))⟩

end TimeOfDay

/-- A fixed UTC offset, in minutes east of UTC (matching `timeZoneMinutes`),
    backed by `Std.Time.Zoned.TimeZone.Offset`. See the module doc for why
    upstream's summer-time flag and name are dropped. -/
structure TimeZone where
  /-- The underlying `Std.Time.Zoned.TimeZone.Offset`. -/
  toOffset : Std.Time.TimeZone.Offset
deriving Repr, DecidableEq, Inhabited

namespace TimeZone

instance : BEq TimeZone where
  beq a b := decide (a.toOffset = b.toOffset)

/-- UTC itself, offset `0`. -/
def utc : TimeZone := ⟨Std.Time.TimeZone.Offset.zero⟩

/-- Build a `TimeZone` directly from its offset in minutes. -/
def minutesToTimeZone (m : Int) : TimeZone :=
  ⟨Std.Time.TimeZone.Offset.ofSeconds (.ofInt (m * 60))⟩

/-- The offset, in minutes east of UTC. -/
@[inline] def minutes (tz : TimeZone) : Int := tz.toOffset.second.val / 60

end TimeZone
end Data.Time
