/-
  Linen.Network.WebApp.Server.Header — Indexed header lookup

  Converts a list of headers into an array indexed by common header names,
  enabling O(1) lookup for frequently-accessed headers during request processing.

  ## Design
  13 request headers are indexed (Content-Length, Transfer-Encoding, Expect,
  Connection, Range, Host, If-Modified-Since, If-Unmodified-Since, If-Range,
  Referer, User-Agent, If-Match, If-None-Match).
-/
import Linen.Network.HTTP.Types.Header

namespace Network.WebApp.Server

open Network.HTTP.Types
open Data (CI)

/-- Indices for commonly-accessed request headers. -/
inductive RequestHeaderIndex where
  | contentLength       -- 0
  | transferEncoding    -- 1
  | expect              -- 2
  | connection          -- 3
  | range               -- 4
  | host                -- 5
  | ifModifiedSince     -- 6
  | ifUnmodifiedSince   -- 7
  | ifRange             -- 8
  | referer             -- 9
  | userAgent           -- 10
  | ifMatch             -- 11
  | ifNoneMatch         -- 12
deriving BEq, Repr

/-- Total number of indexed request headers. -/
def requestMaxIndex : Nat := 13

/-- Map a header name (case-insensitive) to its index, or `none` if not indexed.
    Uses the folded (lowercased) form for comparison. -/
def requestKeyIndex (name : HeaderName) : Option Nat :=
  let n := name.foldedCase  -- CI stores the lowercased form
  if n == "content-length" then some 0
  else if n == "transfer-encoding" then some 1
  else if n == "expect" then some 2
  else if n == "connection" then some 3
  else if n == "range" then some 4
  else if n == "host" then some 5
  else if n == "if-modified-since" then some 6
  else if n == "if-unmodified-since" then some 7
  else if n == "if-range" then some 8
  else if n == "referer" then some 9
  else if n == "user-agent" then some 10
  else if n == "if-match" then some 11
  else if n == "if-none-match" then some 12
  else none

/-- Indexed header array for O(1) lookup of common headers.
    $$\text{IndexedHeader} = \text{Array}\ (\text{Option}\ \text{HeaderValue})$$ -/
abbrev IndexedHeader := Array (Option String)

/-- Build an indexed header array from request headers.
    Complexity: O(n) where n = number of headers. -/
def indexRequestHeader (hdrs : RequestHeaders) : IndexedHeader :=
  let arr : IndexedHeader := #[none, none, none, none, none, none, none, none, none, none, none, none, none]
  hdrs.foldl (init := arr) fun acc (name, value) =>
    match requestKeyIndex name with
    | some idx =>
      if h : idx < acc.size then acc.set idx (some value)
      else acc
    | none => acc

/-- Look up an indexed header by its index.
    O(1) array access. -/
@[inline] def IndexedHeader.lookup (ih : IndexedHeader) (idx : Nat) : Option String :=
  if h : idx < ih.size then ih[idx]
  else none

end Network.WebApp.Server
