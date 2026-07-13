/-
  Tests for `Linen.Time.Calendar.Quarter`.
-/
import Linen.Time.Calendar.Quarter

open Time.Calendar

namespace Tests.Time.Calendar.Quarter

-- `ofMonthOfYear` maps months to quarters.
#guard (QuarterOfYear.ofMonthOfYear 1).val == 1
#guard (QuarterOfYear.ofMonthOfYear 3).val == 1
#guard (QuarterOfYear.ofMonthOfYear 4).val == 2
#guard (QuarterOfYear.ofMonthOfYear 12).val == 4

-- `yearQuarter`/`toYearQuarter` round-trip a valid quarter-of-year.
#guard (Quarter.yearQuarter (.ofInt 2024) 2).toYearQuarter == (Std.Time.Year.Offset.ofInt 2024, 2)

-- Invalid quarters of year are clipped.
#guard (Quarter.yearQuarter (.ofInt 2024) 0).toYearQuarter == (Std.Time.Year.Offset.ofInt 2024, 1)
#guard (Quarter.yearQuarter (.ofInt 2024) 5).toYearQuarter == (Std.Time.Year.Offset.ofInt 2024, 4)

-- `year`/`quarterOfYear` are the individual projections.
#guard (Quarter.yearQuarter (.ofInt 2024) 3).year == Std.Time.Year.Offset.ofInt 2024
#guard (Quarter.yearQuarter (.ofInt 2024) 3).quarterOfYear == 3

-- Q4 of one year is immediately before Q1 of the next.
#guard Quarter.yearQuarter (.ofInt 2023) 4 = Quarter.addQuarters (-1) (Quarter.yearQuarter (.ofInt 2024) 1)

-- `addQuarters`/`diffQuarters` are inverse.
#guard Quarter.diffQuarters (Quarter.addQuarters 3 (Quarter.yearQuarter (.ofInt 2024) 1))
  (Quarter.yearQuarter (.ofInt 2024) 1) == 3

-- `firstMonth`/`lastMonth` bracket the quarter.
#guard (Quarter.yearQuarter (.ofInt 2024) 2).firstMonth == Month.yearMonth (.ofInt 2024) 4
#guard (Quarter.yearQuarter (.ofInt 2024) 2).lastMonth == Month.yearMonth (.ofInt 2024) 6

-- `periodFirstDay`/`periodLastDay` give the boundaries of the quarter.
#guard (Quarter.yearQuarter (.ofInt 2024) 1).periodFirstDay
  == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 1 1
#guard (Quarter.yearQuarter (.ofInt 2024) 1).periodLastDay
  == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 3 31

-- `monthQuarter`/`dayQuarter` give the `Quarter` containing a `Month`/date.
#guard Quarter.monthQuarter (Month.yearMonth (.ofInt 2024) 5) == Quarter.yearQuarter (.ofInt 2024) 2
#guard Quarter.dayQuarter (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 11 20)
  == Quarter.yearQuarter (.ofInt 2024) 4

end Tests.Time.Calendar.Quarter
