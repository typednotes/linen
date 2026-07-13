/-
  Tests for `Linen.Time.UniversalTime`.
-/
import Linen.Time.UniversalTime

open Time

namespace Tests.Time.UniversalTime

-- Modified Julian Day `0` is 1858-11-17 00:00:00, the fixed epoch of the
-- Modified Julian Day count.
#guard UT1.ut1ToLocalTime 0 ⟨0⟩ ==
  ⟨Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 1858) 11 17, Std.Time.PlainTime.midnight⟩

-- A half-day fraction gives noon on the same day.
#guard (UT1.ut1ToLocalTime 0 ⟨(1 : Rat) / 2⟩).time == Std.Time.PlainTime.ofHours 12

-- A positive longitude (East) advances local mean time ahead of UT1.
#guard (UT1.ut1ToLocalTime 90 ⟨0⟩).time == Std.Time.PlainTime.ofHours 6

-- `localTimeToUT1`/`ut1ToLocalTime` round-trip on the meridian.
#guard UT1.localTimeToUT1 0
    (Std.Time.PlainDateTime.mk (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 1858) 11 17)
      Std.Time.PlainTime.midnight)
  == UT1.mk 0

#guard UT1.ut1ToLocalTime 0 (UT1.localTimeToUT1 0
  (UT1.ut1ToLocalTime 0 ⟨12345⟩)) == UT1.ut1ToLocalTime 0 ⟨12345⟩

end Tests.Time.UniversalTime
