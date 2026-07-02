/-
  Tests for `Linen.Data.Time.Clock`.

  `getCurrentTime` performs real IO (reads the monotonic clock), so it's
  pinned at the type level. Everything else is pure and gets `#guard`-checked.
-/
import Linen.Data.Time.Clock

open Data.Time

namespace Tests.Data.Time.Clock

/-! ### `NominalDiffTime` -/

#guard NominalDiffTime.zero == ⟨0⟩
#guard NominalDiffTime.fromSeconds 3 == ⟨3000000000⟩
#guard NominalDiffTime.fromMilliseconds 250 == ⟨250000000⟩
#guard NominalDiffTime.fromMicroseconds 500 == ⟨500000⟩

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

#guard compare (UTCTime.mk 100) (UTCTime.mk 200) == Ordering.lt
#guard toString (UTCTime.mk 3000000000) == "UTCTime(3s)"

#guard diffUTCTime (UTCTime.mk 5000000000) (UTCTime.mk 2000000000) ==
  NominalDiffTime.fromSeconds 3
#guard addUTCTime (NominalDiffTime.fromSeconds 3) (UTCTime.mk 2000000000) ==
  UTCTime.mk 5000000000

example (t : UTCTime) : diffUTCTime t t = NominalDiffTime.zero :=
  diffUTCTime_self t

/-! ### `getCurrentTime` — real IO, pinned rather than exercised -/

example : IO UTCTime := getCurrentTime

end Tests.Data.Time.Clock
