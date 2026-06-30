/-
  Linen.Network.HTTP2.HPACK.Table — HPACK static and dynamic tables

  Implements the HPACK header compression tables as defined in RFC 7541.

  ## Design

  The static table is a compile-time constant array of 61 entries.
  The dynamic table is a bounded FIFO queue with eviction when the size
  exceeds the maximum allowed by SETTINGS_HEADER_TABLE_SIZE.

  ## Guarantees

  - Static table indices are 1-based (1..61), matching RFC 7541 Appendix A
  - Dynamic table maintains the invariant `currentSize <= maxSize`
  - Eviction is automatic on insertion

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Table` (https://hackage.haskell.org/package/http2)
-/

namespace Network.HTTP2.HPACK

/-- A header field is a name-value pair of strings.
    $$\text{HeaderField} = \text{String} \times \text{String}$$ -/
abbrev HeaderField := String × String

/-- The HPACK static table as defined in RFC 7541 Appendix A.
    Index 1-based, containing 61 pre-defined header fields. -/
def staticTable : Array HeaderField := #[
  (":authority", ""),                                    -- 1
  (":method", "GET"), (":method", "POST"),               -- 2, 3
  (":path", "/"), (":path", "/index.html"),              -- 4, 5
  (":scheme", "http"), (":scheme", "https"),             -- 6, 7
  (":status", "200"), (":status", "204"), (":status", "206"),   -- 8, 9, 10
  (":status", "304"), (":status", "400"), (":status", "404"),   -- 11, 12, 13
  (":status", "500"),                                    -- 14
  ("accept-charset", ""),                                -- 15
  ("accept-encoding", "gzip, deflate"),                  -- 16
  ("accept-language", ""),                               -- 17
  ("accept-ranges", ""),                                 -- 18
  ("accept", ""),                                        -- 19
  ("access-control-allow-origin", ""),                   -- 20
  ("age", ""), ("allow", ""), ("authorization", ""),     -- 21, 22, 23
  ("cache-control", ""),                                 -- 24
  ("content-disposition", ""), ("content-encoding", ""), -- 25, 26
  ("content-language", ""), ("content-length", ""),      -- 27, 28
  ("content-location", ""), ("content-range", ""),       -- 29, 30
  ("content-type", ""),                                  -- 31
  ("cookie", ""), ("date", ""), ("etag", ""),            -- 32, 33, 34
  ("expect", ""), ("expires", ""), ("from", ""), ("host", ""),  -- 35, 36, 37, 38
  ("if-match", ""), ("if-modified-since", ""),           -- 39, 40
  ("if-none-match", ""), ("if-range", ""),               -- 41, 42
  ("if-unmodified-since", ""), ("last-modified", ""),    -- 43, 44
  ("link", ""), ("location", ""), ("max-forwards", ""),  -- 45, 46, 47
  ("proxy-authenticate", ""), ("proxy-authorization", ""),  -- 48, 49
  ("range", ""), ("referer", ""), ("refresh", ""),       -- 50, 51, 52
  ("retry-after", ""), ("server", ""), ("set-cookie", ""),  -- 53, 54, 55
  ("strict-transport-security", ""),                     -- 56
  ("transfer-encoding", ""), ("user-agent", ""),         -- 57, 58
  ("vary", ""), ("via", ""), ("www-authenticate", "")    -- 59, 60, 61
]

/-- Size of the static table. Always 61. -/
def staticTableSize : Nat := 61

/-- Look up an entry in the static table by 1-based index.
    $$\text{staticLookup}(i) = \text{staticTable}[i-1]$$ for $1 \leq i \leq 61$.
    Returns `none` for out-of-range indices. -/
def staticLookup (index : Nat) : Option HeaderField :=
  if index >= 1 && index <= staticTableSize then
    staticTable[index - 1]?
  else none

/-- The HPACK dynamic table. Entries are stored most-recent-first.
    The table has a maximum size in octets, and entries are evicted
    from the end (oldest) when the size would exceed the maximum.

    Size of an entry: `name.length + value.length + 32` (RFC 7541 Section 4.1). -/
structure DynamicTable where
  /-- Entries stored most-recent-first. -/
  entries : Array HeaderField
  /-- Current size in octets. -/
  currentSize : Nat
  /-- Maximum size in octets (from SETTINGS_HEADER_TABLE_SIZE). -/
  maxSize : Nat
  deriving Repr

/-- Calculate the HPACK entry size per RFC 7541 Section 4.1.
    $$\text{entrySize}(n, v) = |n| + |v| + 32$$ -/
@[inline] def entrySize (name value : String) : Nat :=
  name.length + value.length + 32

namespace DynamicTable

/-- Create an empty dynamic table with the given maximum size.
    $$\text{empty}(m) = \{ \text{entries} = [], \text{currentSize} = 0, \text{maxSize} = m \}$$ -/
def empty (maxSize : Nat) : DynamicTable :=
  { entries := #[], currentSize := 0, maxSize := maxSize }

instance : Inhabited DynamicTable := ⟨empty 4096⟩

/-- Get the number of entries in the dynamic table. -/
@[inline] def size (dt : DynamicTable) : Nat := dt.entries.size

/-- Look up an entry by 0-based index (0 = most recent).
    $$\text{lookup}(dt, i) = dt.\text{entries}[i]$$ -/
def lookup (dt : DynamicTable) (index : Nat) : Option HeaderField :=
  dt.entries[index]?

/-- Evict oldest entries (from the end) until the total size is `≤ targetMax`.

    The original `http2` source used a fuel-bounded recursion; since entry
    sizes are strictly positive, the prefix sums of the most-recent-first
    `entries` are monotone, so the surviving set is exactly the longest front
    prefix whose cumulative size is `≤ targetMax` — computed here with a total
    `foldl` that stops at the first entry that would overflow. -/
private def evict (dt : DynamicTable) (targetMax : Nat) : DynamicTable :=
  let (kept, keptSize, _) := dt.entries.foldl
    (fun (acc : Array HeaderField × Nat × Bool) (e : HeaderField) =>
      let (kept, sz, stopped) := acc
      if stopped then (kept, sz, true)
      else
        let eSz := entrySize e.1 e.2
        if sz + eSz ≤ targetMax then (kept.push e, sz + eSz, false)
        else (kept, sz, true))
    (#[], 0, false)
  { entries := kept, currentSize := keptSize, maxSize := dt.maxSize }

/-- Insert a new entry at the front of the dynamic table.
    Evicts old entries as needed to maintain the size invariant.
    If the entry itself is larger than maxSize, the table is emptied.

    $$\text{insert}(dt, n, v) = \text{evict}(\{n:v\} :: dt.\text{entries})$$ -/
def insert (dt : DynamicTable) (name value : String) : DynamicTable :=
  let eSize := entrySize name value
  if eSize > dt.maxSize then
    -- Entry too large: empty the table per RFC 7541 Section 4.4
    { entries := #[], currentSize := 0, maxSize := dt.maxSize }
  else
    let dt' := dt.evict (dt.maxSize - eSize)
    { entries := #[(name, value)] ++ dt'.entries
      currentSize := dt'.currentSize + eSize
      maxSize := dt.maxSize }

/-- Resize the dynamic table to a new maximum size. Evicts entries if needed.
    $$\text{resize}(dt, m) = \text{evict}(dt, m)$$ with updated maxSize. -/
def resize (dt : DynamicTable) (newMaxSize : Nat) : DynamicTable :=
  let dt' := { dt with maxSize := newMaxSize }
  if dt'.currentSize ≤ newMaxSize then dt'
  else dt'.evict newMaxSize

/-- Find a header field in the dynamic table. Returns the 0-based index if found.
    Searches for exact (name, value) match first, then name-only match. -/
def find (dt : DynamicTable) (name value : String) : Option (Nat × Bool) :=
  -- First pass: exact match
  let exactIdx := dt.entries.findIdx? (fun (n, v) => n == name && v == value)
  match exactIdx with
  | some idx => some (idx, true)
  | none =>
    -- Second pass: name-only match
    let nameIdx := dt.entries.findIdx? (fun (n, _) => n == name)
    match nameIdx with
    | some idx => some (idx, false)
    | none => none

end DynamicTable

/-- Look up a header field by HPACK index (1-based, static table first, then dynamic).
    $$\text{indexLookup}(dt, i) = \begin{cases}
      \text{staticTable}[i-1] & \text{if } 1 \leq i \leq 61 \\
      \text{dt.entries}[i-62] & \text{if } i > 61
    \end{cases}$$ -/
def indexLookup (dt : DynamicTable) (index : Nat) : Option HeaderField :=
  if index <= staticTableSize then
    staticLookup index
  else
    dt.lookup (index - staticTableSize - 1)

/-- Find a header field in the combined static + dynamic tables.
    Returns `(index, exactMatch)` where index is 1-based HPACK index. -/
def findInTables (dt : DynamicTable) (name value : String) : Option (Nat × Bool) :=
  -- Search static table first
  let staticResult := do
    let exactIdx := staticTable.findIdx? (fun (n, v) => n == name && v == value)
    match exactIdx with
    | some idx => some (idx + 1, true)
    | none =>
      let nameIdx := staticTable.findIdx? (fun (n, _) => n == name)
      match nameIdx with
      | some idx => some (idx + 1, false)
      | none => none
  match staticResult with
  | some (idx, true) => some (idx, true)
  | staticNameMatch =>
    -- Search dynamic table
    match dt.find name value with
    | some (dIdx, true) => some (dIdx + staticTableSize + 1, true)
    | some (dIdx, false) =>
      -- Prefer static name match over dynamic name match
      match staticNameMatch with
      | some result => some result
      | none => some (dIdx + staticTableSize + 1, false)
    | none => staticNameMatch

end Network.HTTP2.HPACK
