/-
  Linen.Network.HTTP3.QPACK.Table -- QPACK static table

  The QPACK static table (RFC 9204 Appendix A) used for HTTP/3 header compression.

  ## Design

  QPACK uses a different static table from HPACK. The QPACK static table has 99 entries
  (0-indexed) with commonly used HTTP header fields and values. Unlike HPACK, QPACK
  indices are 0-based.

  ## Guarantees

  - Static table is a compile-time constant array
  - Indices are 0-based (0..98), matching RFC 9204 Appendix A
  - `staticTableSize` equals 99

  ## Haskell equivalent
  QPACK table types from the `http3` package
-/

namespace Network.HTTP3.QPACK

/-- A header field is a name-value pair.
    $$\text{HeaderField} = \text{String} \times \text{String}$$ -/
abbrev HeaderField := String × String

/-- The QPACK static table (RFC 9204 Appendix A).
    99 entries, 0-indexed. -/
def staticTable : Array HeaderField := #[
  -- 0
  (":authority", ""),
  -- 1
  (":path", "/"),
  -- 2
  ("age", "0"),
  -- 3
  ("content-disposition", ""),
  -- 4
  ("content-length", "0"),
  -- 5
  ("cookie", ""),
  -- 6
  ("date", ""),
  -- 7
  ("etag", ""),
  -- 8
  ("if-modified-since", ""),
  -- 9
  ("if-none-match", ""),
  -- 10
  ("last-modified", ""),
  -- 11
  ("link", ""),
  -- 12
  ("location", ""),
  -- 13
  ("referer", ""),
  -- 14
  ("set-cookie", ""),
  -- 15
  (":method", "CONNECT"),
  -- 16
  (":method", "DELETE"),
  -- 17
  (":method", "GET"),
  -- 18
  (":method", "HEAD"),
  -- 19
  (":method", "OPTIONS"),
  -- 20
  (":method", "POST"),
  -- 21
  (":method", "PUT"),
  -- 22
  (":scheme", "http"),
  -- 23
  (":scheme", "https"),
  -- 24
  (":status", "103"),
  -- 25
  (":status", "200"),
  -- 26
  (":status", "304"),
  -- 27
  (":status", "404"),
  -- 28
  (":status", "503"),
  -- 29
  ("accept", "*/*"),
  -- 30
  ("accept", "application/dns-message"),
  -- 31
  ("accept-encoding", "gzip, deflate, br"),
  -- 32
  ("accept-ranges", "bytes"),
  -- 33
  ("access-control-allow-headers", "cache-control"),
  -- 34
  ("access-control-allow-headers", "content-type"),
  -- 35
  ("access-control-allow-origin", "*"),
  -- 36
  ("cache-control", "max-age=0"),
  -- 37
  ("cache-control", "max-age=2592000"),
  -- 38
  ("cache-control", "max-age=604800"),
  -- 39
  ("cache-control", "no-cache"),
  -- 40
  ("cache-control", "no-store"),
  -- 41
  ("cache-control", "public, max-age=31536000"),
  -- 42
  ("content-encoding", "br"),
  -- 43
  ("content-encoding", "gzip"),
  -- 44
  ("content-type", "application/dns-message"),
  -- 45
  ("content-type", "application/javascript"),
  -- 46
  ("content-type", "application/json"),
  -- 47
  ("content-type", "application/x-www-form-urlencoded"),
  -- 48
  ("content-type", "image/gif"),
  -- 49
  ("content-type", "image/jpeg"),
  -- 50
  ("content-type", "image/png"),
  -- 51
  ("content-type", "text/css"),
  -- 52
  ("content-type", "text/html; charset=utf-8"),
  -- 53
  ("content-type", "text/plain"),
  -- 54
  ("content-type", "text/plain;charset=utf-8"),
  -- 55
  ("range", "bytes=0-"),
  -- 56
  ("strict-transport-security", "max-age=31536000"),
  -- 57
  ("strict-transport-security", "max-age=31536000; includesubdomains"),
  -- 58
  ("strict-transport-security", "max-age=31536000; includesubdomains; preload"),
  -- 59
  ("vary", "accept-encoding"),
  -- 60
  ("vary", "origin"),
  -- 61
  ("x-content-type-options", "nosniff"),
  -- 62
  ("x-xss-protection", "1; mode=block"),
  -- 63
  (":status", "100"),
  -- 64
  (":status", "204"),
  -- 65
  (":status", "206"),
  -- 66
  (":status", "302"),
  -- 67
  (":status", "400"),
  -- 68
  (":status", "403"),
  -- 69
  (":status", "421"),
  -- 70
  (":status", "425"),
  -- 71
  (":status", "500"),
  -- 72
  ("accept-language", ""),
  -- 73
  ("access-control-allow-credentials", "FALSE"),
  -- 74
  ("access-control-allow-credentials", "TRUE"),
  -- 75
  ("access-control-allow-headers", "*"),
  -- 76
  ("access-control-allow-methods", "get"),
  -- 77
  ("access-control-allow-methods", "get, post, options"),
  -- 78
  ("access-control-allow-methods", "options"),
  -- 79
  ("access-control-expose-headers", "content-length"),
  -- 80
  ("access-control-request-headers", "content-type"),
  -- 81
  ("access-control-request-method", "get"),
  -- 82
  ("access-control-request-method", "post"),
  -- 83
  ("alt-svc", "clear"),
  -- 84
  ("authorization", ""),
  -- 85
  ("content-security-policy", "script-src 'none'; object-src 'none'; base-uri 'none'"),
  -- 86
  ("early-data", "1"),
  -- 87
  ("expect-ct", ""),
  -- 88
  ("forwarded", ""),
  -- 89
  ("if-range", ""),
  -- 90
  ("origin", ""),
  -- 91
  ("purpose", "prefetch"),
  -- 92
  ("server", ""),
  -- 93
  ("timing-allow-origin", "*"),
  -- 94
  ("upgrade-insecure-requests", "1"),
  -- 95
  ("user-agent", ""),
  -- 96
  ("x-forwarded-for", ""),
  -- 97
  ("x-frame-options", "deny"),
  -- 98
  ("x-frame-options", "sameorigin")
]

/-- Size of the QPACK static table. Always 99.
    $$|\text{staticTable}| = 99$$ -/
def staticTableSize : Nat := 99

/-- Look up an entry in the QPACK static table by 0-based index.
    $$\text{staticLookup}(i) = \text{staticTable}[i]$$ for $0 \leq i < 99$.
    Returns `none` for out-of-range indices. -/
def staticLookup (index : Nat) : Option HeaderField :=
  if index < staticTableSize then
    staticTable[index]?
  else none

/-- Find a header field in the static table. Returns the 0-based index.
    Searches for exact (name, value) match first, then name-only match.
    $$\text{staticFind} : \text{String} \to \text{String} \to \text{Option}(\mathbb{N} \times \text{Bool})$$
    Returns `(index, exactMatch)`. -/
def staticFind (name value : String) : Option (Nat × Bool) :=
  -- First pass: exact match
  let exactIdx := staticTable.findIdx? (fun (n, v) => n == name && v == value)
  match exactIdx with
  | some idx => some (idx, true)
  | none =>
    -- Second pass: name-only match
    let nameIdx := staticTable.findIdx? (fun (n, _) => n == name)
    match nameIdx with
    | some idx => some (idx, false)
    | none => none

end Network.HTTP3.QPACK
