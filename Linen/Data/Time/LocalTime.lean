/-
  Linen.Data.Time.LocalTime — time-of-day and UTC-offset local time

  Another small addition to `linen`'s `Time` port, alongside
  `Linen.Data.Time.Calendar`, made while porting `sqlite-simple`
  (`docs/imports/sqlite-simple/dependencies.md`, module #7): `Database.
  SQLite.Simple.Time.Implementation` renders/parses times against Haskell's
  `Data.Time.LocalTime.TimeOfDay`/`TimeZone`, which had no counterpart here.

  ## Design

  `TimeOfDay` mirrors upstream: an hour in `[0, 23]`, a minute in `[0, 59]`,
  and seconds (with sub-second precision) in `[0, 60)` — using `Float` for the
  fractional-seconds field rather than upstream's fixed-precision `Pico`,
  since `linen` has no arbitrary-fixed-precision decimal type and SQLite's own
  textual formats never need more than millisecond precision (see
  `Linen.Database.SQLite.Simple.Time.Implementation`, the only consumer).
  `midnight` (`00:00:00`) is upstream's leap-second allowance (`[0, 61)`, to
  admit a positive leap second): SQLite's date functions themselves are
  leap-second-unaware, so this port keeps the ordinary `[0, 60)` bound.

  `TimeZone` mirrors upstream's `Data.Time.LocalTime.TimeZone`, reduced to
  just the signed offset in minutes (`timeZoneMinutes`); upstream's optional
  summer-time flag and textual abbreviation are display-only metadata never
  read by any of `sqlite-simple`'s date/time code, so they are dropped.

  `localToUTCTimeOfDay`/`timeOfDayToTime`-style conversions between a
  `TimeOfDay` and a fractional day offset are folded into `Linen.Database.
  SQLite.Simple.Time.Implementation`, their only consumer, rather than
  exposed as general-purpose combinators here (upstream itself provides
  several redundant framings of the same arithmetic across `LocalTime` and
  `Clock.TAI`; only the one shape `sqlite-simple` needs is ported).
-/

import Linen.Data.Time.Calendar

namespace Data.Time

/-- A time of day: hour `[0, 23]`, minute `[0, 59]`, and (possibly
    fractional) seconds `[0, 60)`. Mirrors `Data.Time.LocalTime.TimeOfDay`,
    minus the leap-second allowance (see the module doc). -/
structure TimeOfDay where
  hour   : Nat
  minute : Nat
  sec    : Float
deriving Repr, Inhabited

namespace TimeOfDay

/-- Midnight, `00:00:00`. -/
def midnight : TimeOfDay := ⟨0, 0, 0⟩

instance : BEq TimeOfDay where
  beq a b := a.hour == b.hour && a.minute == b.minute && a.sec == b.sec

/-- Build a `TimeOfDay`, validating every component is in range, matching
    `makeTimeOfDayValid` upstream. -/
def makeValid (hour minute : Nat) (sec : Float) : Option TimeOfDay :=
  if hour ≤ 23 && minute ≤ 59 && sec ≥ 0 && sec < 60 then
    some ⟨hour, minute, sec⟩
  else
    none

/-- The time of day as a fraction of a day, in `[0, 1)`. -/
def toDayFraction (t : TimeOfDay) : Float :=
  (t.hour.toFloat * 3600 + t.minute.toFloat * 60 + t.sec) / 86400

/-- Recover a `TimeOfDay` from a fraction of a day in `[0, 1)` (values outside
    that range are reduced modulo 1, matching how upstream's `timeToTimeOfDay`
    is always applied to an already-normalized `DiffTime`). -/
def ofDayFraction (frac : Float) : TimeOfDay :=
  let frac := frac - frac.floor
  let totalSec := frac * 86400
  let hour := (totalSec / 3600).floor
  let rem1 := totalSec - hour * 3600
  let minute := (rem1 / 60).floor
  let sec := rem1 - minute * 60
  ⟨hour.toUInt64.toNat, minute.toUInt64.toNat, sec⟩

end TimeOfDay

/-- A fixed UTC offset, in minutes east of UTC (matching `timeZoneMinutes`).
    See the module doc for why upstream's summer-time flag and name are
    dropped. -/
structure TimeZone where
  minutes : Int
deriving BEq, Repr, Inhabited

namespace TimeZone

/-- UTC itself, offset `0`. -/
def utc : TimeZone := ⟨0⟩

/-- Build a `TimeZone` directly from its offset in minutes. -/
def minutesToTimeZone (m : Int) : TimeZone := ⟨m⟩

end TimeZone
end Data.Time
