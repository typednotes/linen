/-
  Tests for `Linen.Time.Clock.TAI`.
-/
import Linen.Time.Clock.TAI

open Time.Clock.TAI

namespace Tests.Time.Clock.TAI

-- `taiEpoch` is zero seconds since itself.
#guard AbsoluteTime.taiEpoch.sinceEpoch == (0 : Std.Time.Duration)

-- `addAbsoluteTime`/`diffAbsoluteTime` are inverse.
#guard AbsoluteTime.diffAbsoluteTime
    (AbsoluteTime.addAbsoluteTime (Std.Time.Duration.ofSeconds 5) AbsoluteTime.taiEpoch)
    AbsoluteTime.taiEpoch
  == Std.Time.Duration.ofSeconds 5

-- `taiNominalDayStart` is `day * 86400` seconds since `taiEpoch`.
#guard AbsoluteTime.diffAbsoluteTime (AbsoluteTime.taiNominalDayStart 1) AbsoluteTime.taiEpoch
  == Std.Time.Duration.ofSeconds 86400

/-- A leap-second map with one leap second, on the boundary of Modified
    Julian Day `58000` (i.e. MJD `57999` runs long by one second, an
    upstream-style stand-in for an actual historical leap second — real
    tables are always caller-supplied, per this module's doc). -/
private def lsmap : LeapSecondMap := fun day => if day < 58000 then some 36 else some 37

-- `utcDayLength` is `86400` seconds on an ordinary day.
#guard utcDayLength lsmap 58000 == some (Std.Time.Duration.ofSeconds 86400)

-- ... and `86401` seconds on the leap-second day itself.
#guard utcDayLength lsmap 57999 == some (Std.Time.Duration.ofSeconds 86401)

-- `utcDayLength` is `none` outside the map's domain.
#guard utcDayLength (fun (_ : MJD) => (none : Option Int)) 0 |>.isNone

-- `dayStart` accounts for the accumulated leap-second offset.
#guard dayStart lsmap 58000 ==
  some (AbsoluteTime.addAbsoluteTime (Std.Time.Duration.ofSeconds (.ofInt (58000 * 86400 + 37)))
    AbsoluteTime.taiEpoch)

-- `utcToTAITime`/`taiToUTCTime` round-trip a UTC time, including across the
-- leap second.
#guard
  let u : UTCTime := ⟨58000, Std.Time.Duration.ofSeconds 100⟩
  (utcToTAITime lsmap u).bind (taiToUTCTime lsmap) == some u

#guard
  let u : UTCTime := ⟨57999, Std.Time.Duration.ofSeconds 86400⟩  -- the leap second itself
  (utcToTAITime lsmap u).bind (taiToUTCTime lsmap) == some u

-- `taiToUTCTime` reports the leap-second day as one second longer.
#guard utcToTAITime lsmap ⟨58000, 0⟩ ==
  some (AbsoluteTime.addAbsoluteTime (Std.Time.Duration.ofSeconds (.ofInt (58000 * 86400 + 37))) AbsoluteTime.taiEpoch)

end Tests.Time.Clock.TAI
