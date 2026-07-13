/-
  Tests for `Linen.Time.CalendarDiffTime`.
-/
import Linen.Time.CalendarDiffTime

open Time

namespace Tests.Time.CalendarDiffTime

-- `++` adds months and `Duration`s componentwise.
#guard (CalendarDiffTime.mk 1 (Std.Time.Duration.ofSeconds 10)) ++
    (CalendarDiffTime.mk 2 (Std.Time.Duration.ofSeconds 20))
  == CalendarDiffTime.mk 3 (Std.Time.Duration.ofSeconds 30)

-- `empty` is the identity.
#guard CalendarDiffTime.empty == CalendarDiffTime.mk 0 0
#guard (CalendarDiffTime.empty ++ CalendarDiffTime.mk 5 (Std.Time.Duration.ofSeconds 6))
  == CalendarDiffTime.mk 5 (Std.Time.Duration.ofSeconds 6)
#guard (CalendarDiffTime.mk 5 (Std.Time.Duration.ofSeconds 6) ++ CalendarDiffTime.empty)
  == CalendarDiffTime.mk 5 (Std.Time.Duration.ofSeconds 6)

-- Associativity of `++` for concrete periods (the general law, for
-- arbitrary periods, is proved directly in `Linen.Time.CalendarDiffTime`
-- alongside `Append`'s definition).
#guard
  let a := CalendarDiffTime.mk 1 (Std.Time.Duration.ofSeconds 10)
  let b := CalendarDiffTime.mk 2 (Std.Time.Duration.ofSeconds 20)
  let c := CalendarDiffTime.mk 3 (Std.Time.Duration.ofSeconds 30)
  a ++ b ++ c == a ++ (b ++ c)

-- `calendarTimeDays` converts whole days into a `Duration`.
#guard CalendarDiffTime.calendarTimeDays (Time.Calendar.CalendarDiffDays.mk 2 3)
  == CalendarDiffTime.mk 2 (Std.Time.Duration.ofSeconds (.ofInt (3 * 86400)))

-- `calendarTimeTime` wraps a bare `Duration` with zero months.
#guard CalendarDiffTime.calendarTimeTime (Std.Time.Duration.ofSeconds 42)
  == CalendarDiffTime.mk 0 (Std.Time.Duration.ofSeconds 42)

-- Scaling by an integer factor.
#guard (2 : Int) * CalendarDiffTime.mk 1 (Std.Time.Duration.ofSeconds 3)
  == CalendarDiffTime.mk 2 (Std.Time.Duration.ofSeconds 6)

end Tests.Time.CalendarDiffTime
