/-
  Linen.Crypto.SigV4 — AWS Signature Version 4 request signing

  The signing scheme used by every AWS API and by the many S3-compatible
  services that reimplement it (Scaleway Object Storage, MinIO, Cloudflare R2,
  Backblaze B2, …). Given credentials, a scope and a request, it produces the
  `Authorization` header the service expects.

  ## Provenance
  Built on primitives Linen already had — `Crypto.SHA256.digest` and
  `Crypto.JOSE.FFI.hmac` (OpenSSL-backed), `Network.URI.escapeURIString` with
  `isUnreserved` (RFC 3986 §2.3, uppercase hex — exactly what the scheme
  requires), `Data.Hex.encode` and `Data.Time.ISO8601.basicDateTime`. Only the
  protocol logic is new; no new native dependency.

  ## The scheme
  Four steps, each a named function below, so each can be checked against
  AWS's published worked example independently:

  1. `CanonicalRequest.render` — a normalised rendering of the request.
  2. `stringToSign` — that rendering's hash, under an algorithm and scope.
  3. `signingKey` — a key derived from the secret by four chained HMACs, so
     the long-lived secret never signs anything directly.
  4. `authorization` — the header, carrying the signature and the exact set of
     headers it covers.

  ## What is *not* here
  Query-string (presigned URL) signing, `AWS4-HMAC-SHA256-PAYLOAD` chunked
  uploads, and Signature Version 4A (multi-region). Each is a separate scheme
  layered on this one.
-/
import Linen.Crypto.SHA256
import Linen.Crypto.JOSE.FFI
import Linen.Data.Hex
import Linen.Data.Time.ISO8601
import Linen.Network.HTTP.Types.URI
import Linen.Network.URI

namespace Crypto.SigV4

-- ── Constants ──

/-- The only algorithm this module implements. Appears verbatim in both the
    string-to-sign and the `Authorization` header. -/
def algorithm : String := "AWS4-HMAC-SHA256"

/-- The terminator ending every credential scope. -/
def terminator : String := "aws4_request"

/-- `HexEncode(SHA256(""))`, the payload hash of a request with no body.
    Precomputed because it appears in nearly every GET and DELETE. -/
def emptyPayloadHash : String :=
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

/-- The literal S3 accepts in place of a payload hash to skip hashing the body.
    Only valid over HTTPS, and only for services that opt into it. -/
def unsignedPayload : String := "UNSIGNED-PAYLOAD"

-- ── Credentials and scope ──

/-- Long-lived or temporary credentials. `sessionToken` is present only for
    temporary credentials (STS, instance roles), and when present must also be
    sent as the `x-amz-security-token` header. -/
structure Credentials where
  accessKeyId     : String
  secretAccessKey : String
  sessionToken    : Option String := none
  deriving Repr

/-- What a signature is valid for: one day, one region, one service. Narrowing
    a derived key to a scope is why a leaked signature cannot be replayed
    against another service or another day. -/
structure Scope where
  /-- `YYYYMMDD`, from `Data.Time.ISO8601.basicDate`. -/
  date    : String
  region  : String
  service : String
  deriving Repr, DecidableEq

/-- `date/region/service/aws4_request`. -/
def Scope.render (s : Scope) : String :=
  s!"{s.date}/{s.region}/{s.service}/{terminator}"

-- ── Header canonicalisation ──

/-- Replace each run of spaces with a single space. Structurally recursive on
    the character list; `lastWasSpace` carries whether the previous character
    was one. -/
private def collapseSpaces : List Char → Bool → List Char → List Char
  | [],        _,            acc => acc.reverse
  | c :: rest, lastWasSpace, acc =>
    if c == ' ' then
      if lastWasSpace then collapseSpaces rest true acc
      else collapseSpaces rest true (' ' :: acc)
    else collapseSpaces rest false (c :: acc)

/-- Trim a header value and collapse internal runs of spaces to one, as the
    specification requires before signing. -/
def normalizeValue (v : String) : String :=
  String.ofList (collapseSpaces v.trimAscii.toString.toList false [])

/-- Merge adjacent entries that share a name, comma-joining their values.
    Correct only on a name-sorted list, which is the only way it is called. -/
private def mergeAdjacent : List (String × String) → List (String × String)
  | []           => []
  | (n, v) :: rest =>
    match mergeAdjacent rest with
    | []                 => [(n, v)]
    | (n', v') :: tail   =>
      if n == n' then (n, v ++ "," ++ v') :: tail else (n, v) :: (n', v') :: tail

/-- Lowercase every name, normalise every value, sort by name, and merge
    duplicates. The shared preprocessing behind both `canonicalHeaders` and
    `signedHeaders`, so the two can never disagree about which headers are
    covered. -/
def prepareHeaders (headers : List (String × String)) : List (String × String) :=
  let lowered := headers.map fun (n, v) => (n.toLower, normalizeValue v)
  mergeAdjacent (lowered.mergeSort fun a b => compare a.1 b.1 != .gt)

/-- Each prepared header as `name:value`, one per line, with a trailing
    newline after the last. -/
def canonicalHeaders (headers : List (String × String)) : String :=
  String.join ((prepareHeaders headers).map fun (n, v) => s!"{n}:{v}\n")

/-- The names of the prepared headers, semicolon-separated. This is what the
    signature commits to: a header outside this list is not covered, and a
    header inside it cannot be altered in flight. -/
def signedHeaders (headers : List (String × String)) : String :=
  ";".intercalate ((prepareHeaders headers).map (·.1))

-- ── Path canonicalisation ──

/-- Percent-encode a path, segment by segment, leaving the separators.

    `doubleEncode` reflects a real split in AWS's own rules: S3 signs the path
    exactly as sent, while every other service expects it normalised and
    encoded a second time. Passing the wrong one produces a signature that
    mismatches for no visible reason, so it is an explicit argument rather
    than a default. -/
def canonicalUri (path : String) (doubleEncode : Bool := false) : String :=
  let enc (s : String) : String :=
    let once := Network.URI.escapeURIString Network.URI.isUnreserved s
    if doubleEncode then Network.URI.escapeURIString Network.URI.isUnreserved once else once
  if path.isEmpty || path == "/" then "/"
  else
    let segments := path.splitOn "/"
    "/".intercalate (segments.map enc)

-- ── Step 1: the canonical request ──

/-- A request reduced to the parts the signature covers. Everything else about
    the request — the HTTP version, header order, unlisted headers — is
    deliberately not signed. -/
structure CanonicalRequest where
  method      : String
  /-- Already canonical: see `canonicalUri`. -/
  uri         : String
  /-- Already canonical: see `Network.HTTP.Types.canonicalQuery`. -/
  query       : String
  headers     : List (String × String)
  /-- Hex SHA-256 of the body, or `unsignedPayload`. -/
  payloadHash : String

/-- The canonical request, as the exact bytes that get hashed in step 2. -/
def CanonicalRequest.render (r : CanonicalRequest) : String :=
  r.method ++ "\n" ++
  r.uri ++ "\n" ++
  r.query ++ "\n" ++
  canonicalHeaders r.headers ++ "\n" ++
  signedHeaders r.headers ++ "\n" ++
  r.payloadHash

-- ── Hashing helpers ──

/-- Hex SHA-256 of a byte string. -/
def hashHex (data : ByteArray) : IO String := do
  return Data.Hex.encode (← Crypto.SHA256.digest data)

/-- Hex SHA-256 of a UTF-8 string. -/
def hashStringHex (s : String) : IO String := hashHex s.toUTF8

/-- HMAC-SHA256. `0` selects SHA-256 in the JOSE FFI's algorithm encoding. -/
def hmacSha256 (key data : ByteArray) : IO ByteArray :=
  Crypto.JOSE.FFI.hmac key data 0

-- ── Step 2: the string to sign ──

/-- What actually gets signed: the algorithm, the timestamp, the scope, and
    the canonical request's hash — never the request itself.

    `amzDate` is `YYYYMMDD'T'HHMMSS'Z'` (`Data.Time.ISO8601.basicDateTime`) and
    its date part must agree with `scope.date`, or the service rejects the
    signature. -/
def stringToSign (amzDate : String) (scope : Scope) (canonicalRequestHash : String) : String :=
  algorithm ++ "\n" ++
  amzDate ++ "\n" ++
  scope.render ++ "\n" ++
  canonicalRequestHash

-- ── Step 3: the signing key ──

/-- Derive the signing key: four chained HMACs over date, region, service and
    terminator, starting from `"AWS4" ++ secret`.

    The chaining is what scopes the key. A key derived for one day, region and
    service cannot sign for another, so the secret itself never leaves the
    caller and a captured derived key expires with its scope.

    $$k = H(H(H(H(\texttt{AWS4}\|s,\ d),\ r),\ v),\ \texttt{aws4\_request})$$ -/
def signingKey (secretAccessKey : String) (scope : Scope) : IO ByteArray := do
  let kDate    ← hmacSha256 ("AWS4" ++ secretAccessKey).toUTF8 scope.date.toUTF8
  let kRegion  ← hmacSha256 kDate scope.region.toUTF8
  let kService ← hmacSha256 kRegion scope.service.toUTF8
  hmacSha256 kService terminator.toUTF8

/-- The hex signature of a string-to-sign under a derived key. -/
def signWith (key : ByteArray) (stringToSign : String) : IO String := do
  return Data.Hex.encode (← hmacSha256 key stringToSign.toUTF8)

-- ── Step 4: the Authorization header ──

/-- The `Authorization` header value.

    The signed-header list travels in the clear precisely so the service can
    reconstruct the same canonical request; it is part of the signed material
    itself, so it cannot be tampered with. -/
def authorization (creds : Credentials) (scope : Scope)
    (signedHeaders signature : String) : String :=
  s!"{algorithm} Credential={creds.accessKeyId}/{scope.render}, \
SignedHeaders={signedHeaders}, Signature={signature}"

-- ── The whole thing ──

/-- A request to be signed, before canonicalisation. -/
structure Request where
  method  : String
  /-- Path only, no host and no query. -/
  path    : String
  query   : Network.HTTP.Types.Query := []
  /-- Must include `host`. `x-amz-date`, and `x-amz-security-token` for
      temporary credentials, are added by `sign`. -/
  headers : List (String × String) := []
  payload : ByteArray := ByteArray.empty
  /-- S3 signs the path as sent; other services double-encode it. -/
  doubleEncodePath : Bool := false
  /-- Skip hashing the body and send `UNSIGNED-PAYLOAD` instead. S3 only. -/
  unsignedBody : Bool := false

/-- Sign a request, returning **the headers to add to it**: `x-amz-date`,
    `x-amz-content-sha256`, `Authorization`, and `x-amz-security-token` when
    the credentials are temporary.

    Returns the headers rather than a mutated request so the caller stays in
    control of its own request type — `Network.HTTP.Client.Request`, or
    anything else.

    The `x-amz-date` used is derived from `time`, so a caller signing several
    requests from one timestamp gets consistent scopes. -/
def sign (creds : Credentials) (region service : String)
    (time : Data.Time.UTCTime) (req : Request) : IO (List (String × String)) := do
  let amzDate := Data.Time.ISO8601.basicDateTime time
  let scope : Scope := { date := Data.Time.ISO8601.basicDate time, region, service }
  let payloadHash ← if req.unsignedBody then pure unsignedPayload else hashHex req.payload
  -- Headers signed: whatever the caller gave, plus the ones we add. The token
  -- must be signed too, or the service rejects it as tampering.
  let extra := [("x-amz-date", amzDate), ("x-amz-content-sha256", payloadHash)]
    ++ (match creds.sessionToken with
        | some t => [("x-amz-security-token", t)]
        | none   => [])
  let allHeaders := req.headers ++ extra
  let canonical : CanonicalRequest :=
    { method := req.method.toUpper
      uri := canonicalUri req.path req.doubleEncodePath
      query := Network.HTTP.Types.canonicalQuery req.query
      headers := allHeaders
      payloadHash := payloadHash }
  let crHash ← hashStringHex canonical.render
  let sts := stringToSign amzDate scope crHash
  let key ← signingKey creds.secretAccessKey scope
  let sig ← signWith key sts
  return extra ++ [("Authorization", authorization creds scope (signedHeaders allHeaders) sig)]

end Crypto.SigV4
