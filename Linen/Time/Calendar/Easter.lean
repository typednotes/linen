/-
  Linen.Time.Calendar.Easter — the Gregorian and Orthodox Easter-date
  algorithms

  Module #5 of `docs/imports/Time/dependencies.md`'s "Genuinely new
  `Linen.*` ports" list, on `Linen.Time.Calendar.Julian` (module #4). Ports
  `Data.Time.Calendar.Easter` from Hackage's `time` package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/Easter.hs).

  Formulae from Reingold & Dershowitz, *Calendrical Calculations*, ch. 8, as
  upstream's own module doc cites.

  ## Design

  The Orthodox variant is defined in terms of the proleptic Julian calendar
  (`Linen.Time.Calendar.Julian`); the Gregorian variant in terms of
  `Std.Time.PlainDate` directly. Both work in terms of a single day-count
  representation: `Std.Time.PlainDate`'s Unix-epoch day count
  (`.toEpochDay`/`.ofEpochDay`) substitutes for upstream's Modified Julian
  Day count throughout (matching the same substitution
  `Linen.Time.Calendar.Julian`'s module doc already documents), so
  `sundayAfter` is stated against that count rather than upstream's literal
  `mod (mjd + 3) 7`.
-/
import Std.Time
import Linen.Time.Calendar.Julian

namespace Time.Calendar.Easter

open Std.Time (PlainDate)

/-- The next Sunday strictly after a given date.

    `Std.Time`'s Unix epoch (1970-01-01) is a Thursday; the Modified Julian
    Day epoch (1858-11-17, upstream's `Day` representation) is a Wednesday.
    Upstream's `mod (mjd + 3) 7 == 0` test for "is this a Sunday" is
    re-derived against the Unix-epoch day count via the fixed
    `Std.Time.Day.Offset`-vs-MJD offset (`Julian.mjdOfUnixEpoch`, `40587`),
    giving the equivalent `mod (unixEpochDay + 40587 + 3) 7`. -/
def sundayAfter (date : PlainDate) : PlainDate :=
  let unixEpochDay := date.toEpochDay.val
  let shift := 7 - (unixEpochDay + Julian.mjdOfUnixEpoch + 3) % 7
  date.addDays (.ofInt shift)

-- ── Orthodox Easter (proleptic Julian calendar) ──

/-- Given a year, find the Paschal full moon according to Orthodox
    Christian tradition. -/
def orthodoxPaschalMoon (year : Int) : PlainDate :=
  let shiftedEpact := (14 + 11 * (year % 19)) % 30
  let jyear := if year > 0 then year else year - 1
  let unixEpochDay := Julian.fromJulian jyear 4 19 - shiftedEpact
  PlainDate.ofEpochDay (.ofInt unixEpochDay)

/-- Given a year, find Easter according to Orthodox Christian tradition. -/
def orthodoxEaster (year : Int) : PlainDate :=
  sundayAfter (orthodoxPaschalMoon year)

-- ── Gregorian Easter (`Std.Time.PlainDate` directly) ──

/-- Given a year, find the Paschal full moon according to the Gregorian
    method. -/
def gregorianPaschalMoon (year : Int) : PlainDate :=
  let century := year / 100 + 1
  let shiftedEpact := (14 + 11 * (year % 19) - (3 * century) / 4 + (5 + 8 * century) / 25) % 30
  let adjustedEpact :=
    if shiftedEpact == 0 || (shiftedEpact == 1 && year % 19 < 10)
      then shiftedEpact + 1
      else shiftedEpact
  (PlainDate.ofYearMonthDayClip (.ofInt year) 4 19).addDays (.ofInt (-adjustedEpact))

/-- Given a year, find Easter according to the Gregorian method. -/
def gregorianEaster (year : Int) : PlainDate :=
  sundayAfter (gregorianPaschalMoon year)

end Time.Calendar.Easter
