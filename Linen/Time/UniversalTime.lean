/-
  Linen.Time.UniversalTime — UT1, the Earth-rotation clock

  Module #7 of `docs/imports/Time/dependencies.md`'s "Genuinely new
  `Linen.*` ports" list, on `Linen.Time.Calendar.Julian` (module #4). Ports
  `Data.Time.Clock.Internal.UniversalTime` from Hackage's `time` package
  (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Clock/Internal/UniversalTime.hs),
  folding in `ut1ToLocalTime`/`localTimeToUT1` from upstream's
  `Data.Time.LocalTime.Internal.LocalTime`
  (https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/LocalTime/Internal/LocalTime.hs)
  — those two are the only pieces of `LocalTime.hs` this module needs:
  `LocalTime.hs`'s title type (`LocalTime`, a day and a time-of-day) is
  already covered by `Std.Time.PlainDateTime` per the dependencies plan's
  substitution list, and the rest of that upstream module
  (`addLocalTime`/`diffLocalTime`/`utcToLocalTime`/`localTimeToUTC`/
  `addLocalDurationClip`/`RollOver`/`diffLocalDurationClip`/`RollOver`) only
  composes already-substituted `Std.Time` pieces with
  `Linen.Time.CalendarDiffTime`, so a caller can already build the
  equivalent directly from those without a bespoke port.

  ## Design

  UT1 — time as measured by the Earth's own rotation, distinct from both
  the atomic-clock-based UTC/TAI (`Std.Time`/`Linen.Time.Clock.TAI`) and the
  proleptic-Gregorian civil calendar — has no fixed relationship to either:
  it drifts against them by a few milliseconds a day, tracked empirically
  (historically via IERS bulletins) rather than computed from first
  principles. Upstream models it as `ModJulianDate :: Rational`, a Modified
  Julian Day count *with* its fractional part (unlike
  `Linen.Time.Calendar.Julian`'s whole-day-only MJD-vs-Unix-epoch count),
  since a sub-day-precision civil time needs the fractional day. `linen`
  reuses the standard library's own `Rat` for this rather than introducing a
  bespoke rational type, matching `Linen`'s general precedent of preferring
  the Lean standard library over re-wrapping.

  `ut1ToLocalTime`/`localTimeToUT1` are the only functions that give this
  type any real behaviour: converting a UT1 instant to/from a
  `Std.Time.PlainDateTime` (upstream's `LocalTime`) on a given meridian
  (a longitude in degrees, positive East) via mean solar time. They are
  ported here directly against `UT1`/`PlainDateTime`, and against
  `Linen.Time.Calendar.Julian.mjdOfUnixEpoch` for the same Unix-epoch-day
  vs. Modified-Julian-Day conversion `Linen.Time.Calendar.Julian` already
  documents and uses.
-/
import Std.Time
import Linen.Time.Calendar.Julian

namespace Time

/-- UT1: the Modified Julian Date (day count since 1858-11-17, *with* its
    fractional part), i.e. time as measured by the Earth's rotation. -/
structure UT1 where
  /-- The Modified Julian Date, as an exact rational. -/
  modJulianDate : Rat
deriving Repr, DecidableEq

namespace UT1

/-- The number of nanoseconds in a day, as a `Rat` (used to convert a
    fractional day into/from a `Std.Time.PlainTime`'s nanoseconds-since-
    midnight). -/
private def nanosPerDay : Rat := 86400 * 1000000000

/-- Get the local mean time of a UT1 instant on a given meridian (a
    longitude in degrees, positive East). -/
def ut1ToLocalTime (long : Rat) (t : UT1) : Std.Time.PlainDateTime :=
  let localTime := t.modJulianDate + long / 360
  let localMJD := localTime.floor
  let dayFraction := localTime - (localMJD : Rat)
  let date := Std.Time.PlainDate.ofEpochDay (.ofInt (localMJD - Calendar.Julian.mjdOfUnixEpoch))
  let nanos := (dayFraction * nanosPerDay).floor
  let time := Std.Time.PlainTime.ofNanoseconds (.ofInt nanos)
  ⟨date, time⟩

/-- Get the UT1 instant of a local mean time on a given meridian (a
    longitude in degrees, positive East). -/
def localTimeToUT1 (long : Rat) (dt : Std.Time.PlainDateTime) : UT1 :=
  let localMJD : Rat := (dt.date.toEpochDay.val + Calendar.Julian.mjdOfUnixEpoch : Int)
  let dayFraction : Rat := (dt.time.toNanoseconds.val : Rat) / nanosPerDay
  ⟨localMJD + dayFraction - long / 360⟩

end UT1
end Time
