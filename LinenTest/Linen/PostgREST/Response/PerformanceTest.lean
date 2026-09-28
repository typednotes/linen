/-
  Tests for `Linen.PostgREST.Response.Performance`.
-/
import Linen.PostgREST.Response.Performance

open PostgREST.Response.Performance

namespace Tests.PostgREST.Response.Performance

/-! ### Constants -/

#guard serverTimingHeader == "Server-Timing"

/-! ### `serverTimingValue` -/

#guard serverTimingValue 123 == "total;dur=123"
#guard serverTimingValue 0 == "total;dur=0"

/-! ### `timingHeaders` -/

#guard timingHeaders 123 == [ ("Server-Timing", "total;dur=123") ]
#guard timingHeaders 123 (some 10) == [ ("Server-Timing", "total;dur=123, plan;dur=10") ]
#guard timingHeaders 123 none (some 100) == [ ("Server-Timing", "total;dur=123, exec;dur=100") ]
#guard timingHeaders 123 (some 10) (some 100) ==
  [ ("Server-Timing", "total;dur=123, plan;dur=10, exec;dur=100") ]

end Tests.PostgREST.Response.Performance
