/-
  Tests for `Linen.Data.Time.Calendar`.
-/
import Linen.Data.Time.Calendar

open Data.Time

namespace Tests.Data.Time.Calendar

#guard Day.fromGregorian 1970 1 1 == Day.ofModifiedJulianDay 40587
#guard (Day.fromGregorian 1970 1 1).toGregorian == (1970, 1, 1)
#guard (Day.fromGregorian 2024 2 29).toGregorian == (2024, 2, 29)
#guard (Day.fromGregorian 1858 11 17).toModifiedJulianDay == 0

-- Out-of-range month/day are clipped rather than rejected.
#guard (Day.fromGregorian 2023 2 30).toGregorian == (2023, 2, 28)

#guard Day.fromGregorianValid 2024 2 29 == some (Day.fromGregorian 2024 2 29)
#guard Day.fromGregorianValid 2023 2 30 == none

#guard Day.addDays 1 (Day.fromGregorian 1970 1 1) == Day.fromGregorian 1970 1 2
#guard Day.diffDays (Day.fromGregorian 1970 1 2) (Day.fromGregorian 1970 1 1) == 1

#guard compare (Day.fromGregorian 1970 1 1) (Day.fromGregorian 1970 1 2) == Ordering.lt

example (d : Day) : Day.addDays 0 d = d := Day.addDays_diffDays_self d

end Tests.Data.Time.Calendar
