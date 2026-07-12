/-
  Tests for `Linen.Database.SQLite.Simple.Time.Implementation`.
-/
import Linen.Database.SQLite.Simple.Time.Implementation

open Data.Time
open Database.SQLite.Simple.Time

namespace Tests.Database.SQLite.Simple.Time.Implementation

/-! ### `parseDay` / `dayToString` -/

#guard match parseDay "2024-02-29" with | .ok d => d == Day.fromGregorian 2024 2 29 | .error _ => false
#guard match parseDay "1970-01-01" with | .ok d => d == Day.fromGregorian 1970 1 1 | .error _ => false
#guard (parseDay "2024-02-30").isOk == false        -- invalid calendar date
#guard (parseDay "2024-02-2x").isOk == false         -- non-digit day
#guard (parseDay "2024-02-29 ").isOk == false        -- trailing garbage

#guard dayToString (Day.fromGregorian 2024 2 29) == "2024-02-29"
#guard dayToString (Day.fromGregorian 42 1 1) == "0042-01-01"

/-! ### `parseUTCTime` / `utcTimeToString` -/

-- Plain `YYYY-MM-DD HH:MM:SS`, no offset (assumed UTC).
#guard match parseUTCTime "2024-02-29 13:45:07" with
  | .ok t => t == dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 2 29) (TimeOfDay.mk 13 45 7.0)
  | .error _ => false

-- ISO 8601 `T` separator, fractional seconds, trailing `Z`.
#guard match parseUTCTime "2024-02-29T13:45:07.5Z" with
  | .ok t => t == dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 2 29) (TimeOfDay.mk 13 45 7.5)
  | .error _ => false

-- Explicit positive UTC offset shifts the time back to UTC (and can roll
-- the calendar day over).
#guard match parseUTCTime "2024-01-01 00:30:00+02:00" with
  | .ok t => t == dayAndTimeOfDayToUTCTime (Day.fromGregorian 2023 12 31) (TimeOfDay.mk 22 30 0.0)
  | .error _ => false

-- Explicit negative UTC offset.
#guard match parseUTCTime "2024-01-01 23:30:00-02:00" with
  | .ok t => t == dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 1 2) (TimeOfDay.mk 1 30 0.0)
  | .error _ => false

-- Omitted seconds default to `:00`.
#guard match parseUTCTime "2024-01-01 10:00" with
  | .ok t => t == dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 1 1) (TimeOfDay.mk 10 0 0.0)
  | .error _ => false

-- Round trip through rendering.
#guard utcTimeToString
    (dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 2 29) (TimeOfDay.mk 13 45 7.0)) ==
  "2024-02-29 13:45:07"
#guard utcTimeToString
    (dayAndTimeOfDayToUTCTime (Day.fromGregorian 2024 2 29) (TimeOfDay.mk 13 45 7.5)) ==
  "2024-02-29 13:45:07.500"

#guard (parseUTCTime "not a time").isOk == false

/-! ### `timeZoneParser` / `timeZoneToString` -/

#guard match Std.Internal.Parsec.String.Parser.run timeZoneParser "+02:00" with
  | .ok tz => tz == TimeZone.minutesToTimeZone 120
  | .error _ => false
#guard match Std.Internal.Parsec.String.Parser.run timeZoneParser "-05" with
  | .ok tz => tz == TimeZone.minutesToTimeZone (-300)
  | .error _ => false

#guard timeZoneToString (TimeZone.minutesToTimeZone 120) == "+02"
#guard timeZoneToString (TimeZone.minutesToTimeZone 90) == "+01:30"
#guard timeZoneToString (TimeZone.minutesToTimeZone (-90)) == "-01:30"

end Tests.Database.SQLite.Simple.Time.Implementation
