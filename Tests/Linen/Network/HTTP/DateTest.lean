/-
  Tests for `Linen.Network.HTTP.Date`.

  HTTP date parsing/formatting (RFC 7231) is pure, so behaviour is checked with
  `#guard`, including a parse→format→parse round-trip.
-/
import Linen.Network.HTTP.Date

open Network.HTTP.Date Network.HTTP.Date.HTTPDate

namespace Tests.Network.HTTP.Date

/-! ### Parsing — IMF-fixdate and asctime (RFC 7231 §7.1.1.1) -/

#guard parseHTTPDate "Sun, 06 Nov 1994 08:49:37 GMT" == some ⟨1994, 11, 6, 8, 49, 37⟩
#guard parseHTTPDate "Sun Nov  6 08:49:37 1994" == some ⟨1994, 11, 6, 8, 49, 37⟩
#guard parseHTTPDate "Mon, 01 Jan 2024 00:00:00 GMT" == some ⟨2024, 1, 1, 0, 0, 0⟩
#guard parseHTTPDate "Fri, 31 Dec 1999 23:59:60 GMT" == some ⟨1999, 12, 31, 23, 59, 60⟩  -- leap second

/-! ### Parse rejections -/

#guard (parseHTTPDate "not a date").isNone
#guard (parseHTTPDate "Sun, 06 Zzz 1994 08:49:37 GMT").isNone   -- bad month
#guard (parseHTTPDate "Sun, 06 Nov 1994 25:49:37 GMT").isNone   -- hour > 23
#guard (parseHTTPDate "Sun, 32 Nov 1994 08:49:37 GMT").isNone   -- day > 31
#guard (parseHTTPDate "").isNone

/-! ### Formatting (IMF-fixdate, day-of-week via Zeller) -/

#guard formatHTTPDate ⟨1994, 11, 6, 8, 49, 37⟩ == "Sun, 06 Nov 1994 08:49:37 GMT"
#guard formatHTTPDate ⟨2024, 1, 1, 0, 0, 0⟩ == "Mon, 01 Jan 2024 00:00:00 GMT"

/-! ### Round-trip: parse (format d) = d -/

#guard parseHTTPDate (formatHTTPDate ⟨1994, 11, 6, 8, 49, 37⟩) == some ⟨1994, 11, 6, 8, 49, 37⟩
#guard parseHTTPDate (formatHTTPDate ⟨2024, 1, 1, 12, 30, 45⟩) == some ⟨2024, 1, 1, 12, 30, 45⟩

/-! ### BEq / ToString -/

#guard (⟨1994, 11, 6, 8, 49, 37⟩ : HTTPDate) == ⟨1994, 11, 6, 8, 49, 37⟩
#guard ((⟨1994, 11, 6, 8, 49, 37⟩ : HTTPDate) == ⟨1994, 11, 6, 8, 49, 38⟩) == false
#guard toString (⟨1994, 11, 6, 8, 49, 37⟩ : HTTPDate) == "1994-11-06 08:49:37"

end Tests.Network.HTTP.Date
