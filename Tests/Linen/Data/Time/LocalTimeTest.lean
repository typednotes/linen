/-
  Tests for `Linen.Data.Time.LocalTime`.
-/
import Linen.Data.Time.LocalTime

open Data.Time

namespace Tests.Data.Time.LocalTime

#guard TimeOfDay.midnight == TimeOfDay.mk 0 0 0.0

#guard TimeOfDay.makeValid 23 59 59.5 == some (TimeOfDay.mk 23 59 59.5)
#guard TimeOfDay.makeValid 24 0 0 == none
#guard TimeOfDay.makeValid 12 60 0 == none
#guard TimeOfDay.makeValid 12 0 60 == none

#guard TimeOfDay.midnight.toDayFraction == 0.0
#guard (TimeOfDay.mk 12 0 0).toDayFraction == 0.5

#guard TimeOfDay.ofDayFraction 0.0 == TimeOfDay.mk 0 0 0.0
#guard TimeOfDay.ofDayFraction 0.5 == TimeOfDay.mk 12 0 0.0

#guard TimeZone.utc == TimeZone.mk 0
#guard TimeZone.minutesToTimeZone 90 == TimeZone.mk 90

end Tests.Data.Time.LocalTime
