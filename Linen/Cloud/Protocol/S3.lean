/-
  `Cloud.Protocol.S3` — REST with the bucket in the path and XML replies

  The dialect AWS S3 speaks, and therefore the one Scaleway Object Storage
  speaks too. One client, two clouds, differing only in
  `Cloud.S3.endpoint?`'s host.

  ## Path-style addressing

  `/{bucket}/{key}`, not `{bucket}.s3.…/{key}`. Path-style needs no per-bucket
  DNS entry and no wildcard certificate, and it is what S3-compatible stores
  implement most consistently — including the local ones (MinIO, LocalStack)
  that make this namespace debuggable without an account.

  AWS has announced the retirement of path-style for *new* buckets on S3
  itself; if that lands, this is the module that changes, and the endpoint
  table is the only other thing that would need to.

  ## Keys are not paths

  An S3 key is an opaque byte string that merely *looks* like a path. `a//b`,
  `a/` and `/a` are three different keys, and a key may contain any character
  at all. So `objectPath` concatenates without normalising, and the percent
  encoding happens once, in `Cloud.Call.wirePath`, derived from the same
  unencoded value the signature covers.

  ## `Content-MD5` is not the payload hash

  S3 requires an integrity header — `Content-MD5` or one of the
  `x-amz-checksum-*` family — on several configuration writes, and refuses them
  outright without one:

      HTTP 400 InvalidRequest: Missing required header for this request:
      Content-MD5 OR x-amz-checksum-*

  SigV4's `x-amz-content-sha256` does **not** satisfy it: that is a signing
  input, this is an integrity declaration, and having one does not imply the
  other.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Aws/Protocols.lean`, the `S3` namespace).
-/
import Linen.Cloud.Transport
import Linen.Cloud.Page
import Linen.Text.XML
import Linen.Data.Base64
import Linen.Crypto.MD5

namespace Cloud.Protocol.S3

open Cloud
open Network.HTTP.Client (Response)
open Network.HTTP.Types (Query)

-- ── Paths ───────────────────────────────────────────────────────────────────

/-- The path of a bucket-level call: `/{bucket}`. -/
def bucketPath (bucket : String) : String := "/" ++ bucket

/-- The path of an object: `/{bucket}/{key}`, unencoded.

    No normalisation: a key is an opaque byte string, so `a//b` is preserved
    rather than collapsed. Encoding happens once, downstream, in
    `Cloud.Call.wirePath`. -/
def objectPath (bucket key : String) : String := "/" ++ bucket ++ "/" ++ key

/-- Base64 of a body's MD5 digest, for S3's `Content-MD5` integrity header.

    See the module header on why this is not interchangeable with SigV4's
    payload hash. -/
def contentMd5 (body : ByteArray) : String :=
  Data.Base64.encode (Crypto.MD5.hash body)

-- ── Calls ───────────────────────────────────────────────────────────────────

/-- Build an S3 call.

    `doubleEncodePath := false` is not a default worth overriding: S3 is the one
    AWS service that signs its path exactly as sent. -/
def call (creds : Credentials) (ep : Endpoint) (method path : String)
    (query : Query := []) (headers : List (String × String) := [])
    (body : ByteArray := ByteArray.empty) (unsignedBody : Bool := false) : Call :=
  { method, endpoint := ep, path, query, headers, body
  , auth := Auth.forEndpoint creds ep
  , doubleEncodePath := false
  , unsignedBody }

/-- Issue an S3 call and return the raw response. -/
def send (t : Transport) (c : Call) : IO (Except Error Response) :=
  performNow t c

/-- Issue an S3 call and parse the reply as XML.

    An empty body is an error rather than an empty document: every S3 operation
    that answers 200 with XML answers with *some* XML, so an empty body means
    something else went wrong. -/
def sendXml (t : Transport) (c : Call) : IO (Except Error Text.XML.Element) := do
  match ← performNow t c with
  | .error e => return .error e
  | .ok resp =>
    match bodyText resp with
    | .error e => return .error e
    | .ok text =>
      if text.trimAscii.isEmpty then
        return .error (Error.protocol s!"{c.method} {c.path}: empty XML response")
      match Text.XML.parse text with
      | .ok el   => return .ok el
      | .error m => return .error (Error.protocol s!"{c.method} {c.path}: malformed XML: {m}")

/-- Issue an S3 call that answers 200 with no body — `PUT`, `DELETE`. -/
def sendUnit (t : Transport) (c : Call) : IO (Except Error Unit) := do
  match ← performNow t c with
  | .error e => return .error e
  | .ok _    => return .ok ()

-- ── Reading replies ─────────────────────────────────────────────────────────

/-- A required child element's text, or a `protocol` error naming what was
    missing.

    A provider that stops sending a documented field should produce a
    diagnosable failure rather than a silently defaulted value. -/
def requireChildText (el : Text.XML.Element) (name : String) : Except Error String :=
  match el.childText name with
  | some s => .ok s
  | none   => .error (Error.protocol s!"response is missing <{name}>")

/-- An optional child element's text. -/
def childText? (el : Text.XML.Element) (name : String) : Option String :=
  el.childText name

/-- Parse S3's `true`/`false` text, defaulting to `false`.

    `IsTruncated` is the field this exists for, and a missing one means "not
    truncated" — the safe reading, and the one S3 documents. -/
def parseBool (s : Option String) : Bool :=
  match s with
  | some v => v.trimAscii.toString == "true"
  | none   => false

/-- Parse a decimal size, defaulting to zero. -/
def parseNat (s : Option String) : Nat :=
  (s.bind (·.trimAscii.toString.toNat?)).getD 0

/-- Strip the quotes S3 wraps an ETag in.

    S3 sends `"d41d8cd9…"` including the quote characters. Comparing an
    unstripped ETag against a computed digest silently never matches. -/
def unquoteETag (s : String) : String :=
  let t := s.trimAscii.toString
  if t.length >= 2 && t.startsWith "\"" && t.endsWith "\"" then
    ((t.drop 1).dropEnd 1).toString
  else t

/-- S3's pagination cursor, if the listing is truncated.

    Read from `NextContinuationToken`, and **only when `IsTruncated` is
    true**: S3 has been observed to send a token on a final page, and treating
    that as "there is more" costs an extra request that returns nothing and, in
    a bounded read, reports the listing incomplete when it is not. -/
def nextContinuationToken? (el : Text.XML.Element) : Option Cursor :=
  if parseBool (el.childText "IsTruncated") then
    (el.childText "NextContinuationToken").map Cursor.mk
  else none

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard bucketPath "assets" == "/assets"
#guard objectPath "assets" "logs/2026/a.json" == "/assets/logs/2026/a.json"

-- A key is an opaque string, so nothing is normalised away.
#guard objectPath "b" "a//b" == "/b/a//b"
#guard objectPath "b" "" == "/b/"

-- The empty body's MD5, base64-encoded. AWS's own documented value.
#guard contentMd5 ByteArray.empty == "1B2M2Y8AsgTpgAmY7PhCfg=="

#guard parseBool (some "true") == true
#guard parseBool (some "false") == false
#guard parseBool none == false

#guard parseNat (some "1024") == 1024
#guard parseNat none == 0

#guard unquoteETag "\"d41d8cd98f00b204e9800998ecf8427e\"" == "d41d8cd98f00b204e9800998ecf8427e"
#guard unquoteETag "d41d8cd98f00b204e9800998ecf8427e" == "d41d8cd98f00b204e9800998ecf8427e"
#guard unquoteETag "" == ""

end Cloud.Protocol.S3
