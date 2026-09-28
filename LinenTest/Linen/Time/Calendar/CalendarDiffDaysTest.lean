/-
  Tests for `Linen.Time.Calendar.CalendarDiffDays`.
-/
import Linen.Time.Calendar.CalendarDiffDays

open Time.Calendar

namespace Tests.Time.Calendar.CalendarDiffDays

-- `calendarDay`/`calendarWeek`/`calendarMonth`/`calendarYear` are the
-- expected fixed periods.
#guard CalendarDiffDays.calendarDay == ⟨0, 1⟩
#guard CalendarDiffDays.calendarWeek == ⟨0, 7⟩
#guard CalendarDiffDays.calendarMonth == ⟨1, 0⟩
#guard CalendarDiffDays.calendarYear == ⟨12, 0⟩

-- `++` adds componentwise.
#guard (CalendarDiffDays.mk 1 2) ++ (CalendarDiffDays.mk 3 4) == CalendarDiffDays.mk 4 6

-- `empty` is the identity.
#guard CalendarDiffDays.empty == CalendarDiffDays.mk 0 0
#guard (CalendarDiffDays.empty ++ CalendarDiffDays.mk 5 6) == CalendarDiffDays.mk 5 6
#guard (CalendarDiffDays.mk 5 6 ++ CalendarDiffDays.empty) == CalendarDiffDays.mk 5 6

-- Associativity of `++`, as a `Prop`-valued law.
example (a b c : CalendarDiffDays) : a ++ b ++ c = a ++ (b ++ c) := by
  simp only [HAppend.hAppend, Append.append, CalendarDiffDays.append, CalendarDiffDays.mk.injEq]
  omega

-- Scaling by an integer factor.
#guard (2 : Int) * CalendarDiffDays.mk 1 3 == CalendarDiffDays.mk 2 6
#guard CalendarDiffDays.scale (-1) (CalendarDiffDays.mk 1 3) == CalendarDiffDays.mk (-1) (-3)

end Tests.Time.Calendar.CalendarDiffDays
