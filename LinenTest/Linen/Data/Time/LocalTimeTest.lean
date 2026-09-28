/-
  Tests for `Linen.Data.Time.LocalTime`.
-/
import Linen.Data.Time.LocalTime

open Data.Time

namespace Tests.Data.Time.LocalTime

#guard TimeOfDay.midnight == TimeOfDay.ofHourMinuteSec 0 0 0.0

#guard TimeOfDay.makeValid 23 59 59.5 == some (TimeOfDay.ofHourMinuteSec 23 59 59.5)
#guard TimeOfDay.makeValid 24 0 0 == none
#guard TimeOfDay.makeValid 12 60 0 == none
#guard TimeOfDay.makeValid 12 0 60 == none

#guard TimeOfDay.midnight.toDayFraction == 0.0
#guard (TimeOfDay.ofHourMinuteSec 12 0 0).toDayFraction == 0.5

#guard TimeOfDay.ofDayFraction 0.0 == TimeOfDay.ofHourMinuteSec 0 0 0.0
#guard TimeOfDay.ofDayFraction 0.5 == TimeOfDay.ofHourMinuteSec 12 0 0.0

#guard (TimeOfDay.ofHourMinuteSec 23 59 59.5).hour == 23
#guard (TimeOfDay.ofHourMinuteSec 23 59 59.5).minute == 59
#guard (TimeOfDay.ofHourMinuteSec 23 59 59.5).sec == 59.5

#guard TimeZone.utc == TimeZone.minutesToTimeZone 0
#guard (TimeZone.minutesToTimeZone 90).minutes == 90

end Tests.Data.Time.LocalTime
