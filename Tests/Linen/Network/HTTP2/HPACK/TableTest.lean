/-
  Tests for `Linen.Network.HTTP2.HPACK.Table`.

  All of this module is pure: the static table (RFC 7541 Appendix A), the
  dynamic-table FIFO with size-based eviction, and the combined index lookup.
  Behaviour is checked with `#guard`.
-/
import Linen.Network.HTTP2.HPACK.Table

open Network.HTTP2.HPACK

namespace Tests.Network.HTTP2.HPACKTable

/-! ### Static table (RFC 7541 Appendix A) -/

#guard staticTableSize == 61
#guard staticTable.size == 61
#guard staticLookup 1 == some (":authority", "")
#guard staticLookup 2 == some (":method", "GET")
#guard staticLookup 3 == some (":method", "POST")
#guard staticLookup 8 == some (":status", "200")
#guard staticLookup 16 == some ("accept-encoding", "gzip, deflate")
#guard staticLookup 32 == some ("cookie", "")
#guard staticLookup 61 == some ("www-authenticate", "")
#guard staticLookup 0 == none
#guard staticLookup 62 == none

/-! ### entrySize (RFC 7541 §4.1: |name| + |value| + 32) -/

#guard entrySize "" "" == 32
#guard entrySize "a" "b" == 34
#guard entrySize "custom-key" "custom-value" == 10 + 12 + 32

/-! ### Dynamic table — empty / insert / lookup -/

#guard (DynamicTable.empty 4096).maxSize == 4096
#guard (DynamicTable.empty 4096).currentSize == 0
#guard (DynamicTable.empty 4096).size == 0

#guard ((DynamicTable.empty 4096).insert "name" "val").size == 1
#guard ((DynamicTable.empty 4096).insert "name" "val").currentSize == 39  -- 4 + 3 + 32
#guard ((DynamicTable.empty 4096).insert "name" "val").lookup 0 == some ("name", "val")
-- most-recent-first ordering
#guard (((DynamicTable.empty 4096).insert "a" "1").insert "b" "2").lookup 0 == some ("b", "2")
#guard (((DynamicTable.empty 4096).insert "a" "1").insert "b" "2").lookup 1 == some ("a", "1")
#guard (((DynamicTable.empty 4096).insert "a" "1").insert "b" "2").size == 2

/-! ### Eviction (FIFO, oldest first) -/

-- Two 33-octet entries ("a"/"b" with empty value) into a 40-octet table:
-- inserting the second evicts the first.
#guard (((DynamicTable.empty 40).insert "a" "").insert "b" "").size == 1
#guard (((DynamicTable.empty 40).insert "a" "").insert "b" "").lookup 0 == some ("b", "")
-- An entry larger than maxSize empties the table (RFC 7541 §4.4).
#guard ((DynamicTable.empty 40).insert (String.ofList (List.replicate 50 'x')) "").size == 0
#guard ((DynamicTable.empty 40).insert (String.ofList (List.replicate 50 'x')) "").currentSize == 0

/-! ### resize -/

#guard (((DynamicTable.empty 4096).insert "a" "1").resize 0).size == 0   -- evicts everything
#guard (((DynamicTable.empty 4096).insert "a" "1").resize 0).maxSize == 0
#guard ((DynamicTable.empty 4096).resize 100).maxSize == 100

/-! ### find within the dynamic table -/

def dt : DynamicTable := ((DynamicTable.empty 4096).insert "x-custom" "v1").insert "other" "v2"

#guard dt.find "other" "v2" == some (0, true)     -- exact, most recent
#guard dt.find "x-custom" "v1" == some (1, true)
#guard dt.find "x-custom" "different" == some (1, false)  -- name-only match
#guard dt.find "absent" "v" == none

/-! ### Combined index lookup (static then dynamic) -/

#guard indexLookup dt 1 == some (":authority", "")        -- static
#guard indexLookup dt 2 == some (":method", "GET")
#guard indexLookup dt 62 == some ("other", "v2")          -- first dynamic entry
#guard indexLookup dt 63 == some ("x-custom", "v1")
#guard indexLookup dt 64 == none

/-! ### findInTables (1-based HPACK index over static + dynamic) -/

#guard findInTables dt ":method" "GET" == some (2, true)        -- static exact
#guard findInTables dt "accept-encoding" "gzip, deflate" == some (16, true)
#guard findInTables dt ":status" "999" == some (8, false)       -- static name-only (first :status)
#guard findInTables dt "other" "v2" == some (62, true)          -- dynamic exact
#guard findInTables dt "nope" "x" == none

end Tests.Network.HTTP2.HPACKTable
