/-
  Tests for `Linen.Data.Time.Clock`.

  `getCurrentTime` performs real IO (reads `Std.Time.Timestamp.now`, the
  wall clock), so it's exercised via `#eval` rather than `#guard`. Everything
  else is pure and gets `#guard`-checked.
-/
import Linen.Data.Time.Clock

open Data.Time

namespace Tests.Data.Time.Clock

/-! ### `NominalDiffTime` -/

#guard NominalDiffTime.zero == NominalDiffTime.fromSeconds 0
#guard (NominalDiffTime.fromSeconds 3).toMicroseconds == 3000000
#guard (NominalDiffTime.fromMilliseconds 250).toMicroseconds == 250000
#guard (NominalDiffTime.fromMicroseconds 500).toMicroseconds == 500

#guard (NominalDiffTime.fromSeconds 3).toSeconds == 3
#guard (NominalDiffTime.fromSeconds 3).toMilliseconds == 3000
#guard (NominalDiffTime.fromSeconds 3).toMicroseconds == 3000000

#guard NominalDiffTime.fromSeconds 3 + NominalDiffTime.fromSeconds 2 == NominalDiffTime.fromSeconds 5
#guard NominalDiffTime.fromSeconds 5 - NominalDiffTime.fromSeconds 2 == NominalDiffTime.fromSeconds 3
#guard -NominalDiffTime.fromSeconds 3 == NominalDiffTime.fromSeconds (-3)
#guard compare (NominalDiffTime.fromSeconds 1) (NominalDiffTime.fromSeconds 2) == Ordering.lt

#guard toString (NominalDiffTime.fromSeconds 3) == "3s"
#guard toString (NominalDiffTime.fromMilliseconds 3500) == "3.500000000s"

example (n : Int) : (NominalDiffTime.fromSeconds n).toSeconds = n :=
  NominalDiffTime.fromSeconds_toSeconds n

/-! ### `UTCTime` -/

#guard compare (UTCTime.ofNanosSinceEpoch 100) (UTCTime.ofNanosSinceEpoch 200) == Ordering.lt
#guard toString (UTCTime.ofNanosSinceEpoch 3000000000) == "UTCTime(3s)"

#guard diffUTCTime (UTCTime.ofNanosSinceEpoch 5000000000) (UTCTime.ofNanosSinceEpoch 2000000000) ==
  NominalDiffTime.fromSeconds 3
#guard addUTCTime (NominalDiffTime.fromSeconds 3) (UTCTime.ofNanosSinceEpoch 2000000000) ==
  UTCTime.ofNanosSinceEpoch 5000000000

example (t : UTCTime) : diffUTCTime t t = NominalDiffTime.zero :=
  diffUTCTime_self t

/-! ### `getCurrentTime` — real IO: genuine wall-clock time -/

-- `getCurrentTime` is wall-clock-based (via `Std.Time.Timestamp.now`), not a
-- monotonic reading: check the resulting instant decodes to a plausible
-- recent calendar year. This is exactly the regression the ad hoc port's bug
-- would have failed — `IO.monoNanosNow` returns nanoseconds since an
-- arbitrary (typically boot-time) epoch, which decodes to a year in the
-- 1970s, not the present.
#eval show IO Unit from do
  let t ← getCurrentTime
  let secs := t.toTimestamp.toSecondsSinceUnixEpoch.val
  -- 1_700_000_000s since the Unix epoch is 2023-11-14; comfortably in the past
  -- for any real run, yet far later than a monotonic clock's tiny reading.
  if secs < 1700000000 then
    throw (IO.userError s!"getCurrentTime looks monotonic, not wall-clock: {secs}s since epoch")

end Tests.Data.Time.Clock
