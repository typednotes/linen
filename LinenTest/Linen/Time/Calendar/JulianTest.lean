/-
  Tests for `Linen.Time.Calendar.Julian`.
-/
import Linen.Time.Calendar.Julian

open Time.Calendar

namespace Tests.Time.Calendar.Julian

-- The proleptic Julian leap-year rule has no century correction: 1900 is a
-- Julian leap year (unlike the Gregorian calendar, where it isn't).
#guard Julian.isLeapYear 1900 == true
#guard Julian.isLeapYear 2000 == true
#guard Julian.isLeapYear 2023 == false
#guard Julian.isLeapYear 2024 == true

-- `toJulian`/`fromJulian` round-trip a valid year/month/day.
#guard Julian.fromJulian (Julian.toJulian 0).1 (Julian.toJulian 0).2.1 (Julian.toJulian 0).2.2 == 0

-- 2000-01-01 (Gregorian) is 1999-12-19 (Julian): the two calendars are 13
-- days apart in the 21st century. The Unix epoch day for 2000-01-01
-- (Gregorian, via `Std.Time.PlainDate`) is used as the day count.
#guard Julian.toJulian (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2000) 1 1).toEpochDay.val
  == (1999, 12, 19)

-- `addJulianMonthsClip`: adding a month clips overflowing days to the
-- month's last day (2005-01-30 Julian + 1 month = 2005-02-28 Julian).
#guard Julian.addJulianMonthsClip 1 (Julian.fromJulian 2005 1 30) == Julian.fromJulian 2005 2 28

-- `addJulianMonthsRollOver`: the same case rolls over instead
-- (2005-01-30 Julian + 1 month = 2005-03-02 Julian).
#guard Julian.addJulianMonthsRollOver 1 (Julian.fromJulian 2005 1 30) == Julian.fromJulian 2005 3 2

-- `addJulianYearsClip`: Feb 29 clips to Feb 28 on a non-leap year.
#guard Julian.addJulianYearsClip 1 (Julian.fromJulian 2024 2 29) == Julian.fromJulian 2025 2 28

-- `addJulianDurationClip`/`RollOver` compose months then days: 2024-01-30
-- Julian + 1 month clips to 2024-02-29 (2024 is a Julian leap year), then
-- + 2 days lands on 2024-03-02; the "roll-over" convention instead keeps
-- day 30 of the (29-day) month, landing one day later still.
#guard Julian.addJulianDurationClip ⟨1, 2⟩ (Julian.fromJulian 2024 1 30) == Julian.fromJulian 2024 3 2
#guard Julian.addJulianDurationRollOver ⟨1, 2⟩ (Julian.fromJulian 2024 1 30) == Julian.fromJulian 2024 3 3

-- `diffJulianDurationClip` recovers the same-day difference as zero.
#guard Julian.diffJulianDurationClip (Julian.fromJulian 2024 1 30) (Julian.fromJulian 2024 1 30) == ⟨0, 0⟩

-- `diffJulianDurationClip` between two dates a whole number of months apart.
#guard Julian.diffJulianDurationClip (Julian.fromJulian 2024 3 15) (Julian.fromJulian 2024 1 15) == ⟨2, 0⟩

-- `diffJulianDurationRollOver` agrees with `addJulianDurationRollOver`, its
-- inverse.
#guard
  let d1 := Julian.fromJulian 2024 1 30
  let d2 := Julian.addJulianDurationRollOver ⟨2, 5⟩ d1
  Julian.addJulianDurationRollOver (Julian.diffJulianDurationRollOver d2 d1) d1 == d2

end Tests.Time.Calendar.Julian
