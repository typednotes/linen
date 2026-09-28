/-
  Tests for `Linen.Network.HTTP3.QPACK.Table`.

  The QPACK static table (RFC 9204 Appendix A, 0-indexed, 99 entries) and its
  lookup/find are pure, so behaviour is checked with `#guard`.
-/
import Linen.Network.HTTP3.QPACK.Table

open Network.HTTP3.QPACK

namespace Tests.Network.HTTP3.QPACKTable

/-! ### Size and indexed lookup (0-based) -/

#guard staticTableSize == 99
#guard staticTable.size == 99
#guard staticLookup 0 == some (":authority", "")
#guard staticLookup 1 == some (":path", "/")
#guard staticLookup 17 == some (":method", "GET")
#guard staticLookup 23 == some (":scheme", "https")
#guard staticLookup 25 == some (":status", "200")
#guard staticLookup 98 == some ("x-frame-options", "sameorigin")
#guard staticLookup 99 == none

/-! ### staticFind — exact then name-only -/

#guard staticFind ":method" "GET" == some (17, true)
#guard staticFind "accept" "*/*" == some (29, true)
#guard staticFind ":status" "200" == some (25, true)
-- Name-only match returns the first entry with that name (":status" → index 24).
#guard staticFind ":status" "999" == some (24, false)
#guard staticFind ":method" "TRACE" == some (15, false)   -- first :method is index 15 (CONNECT)
#guard staticFind "nonexistent-header" "x" == none

end Tests.Network.HTTP3.QPACKTable
