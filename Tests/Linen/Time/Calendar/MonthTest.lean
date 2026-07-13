/-
  Tests for `Linen.Time.Calendar.Month`.
-/
import Linen.Time.Calendar.Month

open Time.Calendar

namespace Tests.Time.Calendar.Month

-- `yearMonth`/`toYearMonth` round-trip a valid month-of-year.
#guard (Month.yearMonth (.ofInt 2024) 3).toYearMonth == (Std.Time.Year.Offset.ofInt 2024, 3)

-- Invalid months of year are clipped.
#guard (Month.yearMonth (.ofInt 2024) 0).toYearMonth == (Std.Time.Year.Offset.ofInt 2024, 1)
#guard (Month.yearMonth (.ofInt 2024) 13).toYearMonth == (Std.Time.Year.Offset.ofInt 2024, 12)

-- `year`/`monthOfYear` are the individual projections.
#guard (Month.yearMonth (.ofInt 2024) 3).year == Std.Time.Year.Offset.ofInt 2024
#guard (Month.yearMonth (.ofInt 2024) 3).monthOfYear.val == 3

-- December of one year is immediately before January of the next.
#guard Month.yearMonth (.ofInt 2023) 12 = Month.addMonths (-1) (Month.yearMonth (.ofInt 2024) 1)

-- `addMonths`/`diffMonths` are inverse.
#guard Month.diffMonths (Month.addMonths 5 (Month.yearMonth (.ofInt 2024) 1))
  (Month.yearMonth (.ofInt 2024) 1) == 5

-- `periodFirstDay`/`periodLastDay` give the boundaries of the month.
#guard (Month.yearMonth (.ofInt 2024) 2).periodFirstDay
  == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 2 1
#guard (Month.yearMonth (.ofInt 2024) 2).periodLastDay
  == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 2 29  -- 2024 is a leap year

-- `dayPeriod` gives the `Month` containing a date.
#guard Month.dayPeriod (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 2 15)
  == Month.yearMonth (.ofInt 2024) 2

-- `LE`/`LT` order by the absolute month count.
#guard Month.yearMonth (.ofInt 2024) 1 < Month.yearMonth (.ofInt 2024) 2
#guard Month.yearMonth (.ofInt 2023) 12 < Month.yearMonth (.ofInt 2024) 1

end Tests.Time.Calendar.Month
