/-
  Tests for `Linen.Time.Calendar.Easter`.
-/
import Linen.Time.Calendar.Easter

open Time.Calendar

namespace Tests.Time.Calendar.Easter

-- Known real-world Gregorian Easter dates.
#guard Easter.gregorianEaster 2023 == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2023) 4 9
#guard Easter.gregorianEaster 2024 == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 3 31
#guard Easter.gregorianEaster 2025 == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2025) 4 20

-- A known real-world Orthodox Easter date.
#guard Easter.orthodoxEaster 2024 == Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 5 5

-- Easter always falls on a Sunday.
#guard (Easter.gregorianEaster 2023).weekday == Std.Time.Weekday.sunday
#guard (Easter.orthodoxEaster 2024).weekday == Std.Time.Weekday.sunday

-- `sundayAfter` never returns the same date, and always returns a Sunday.
#guard (Easter.sundayAfter (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 1 1)).weekday
  == Std.Time.Weekday.sunday
#guard Easter.sundayAfter (Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 1 1)
  != Std.Time.PlainDate.ofYearMonthDayClip (.ofInt 2024) 1 1

end Tests.Time.Calendar.Easter
