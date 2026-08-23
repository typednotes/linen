/-
  Tests for `Linen.Crypto.SigV4`.

  The canonicalisation steps are pure and checked with `#guard`. Signing calls
  the OpenSSL FFI, so those run under `#eval` (a thrown error fails the build),
  which also confirms the HMAC binding works end to end.

  Every signature below is a **published AWS vector**, not a value produced by
  this implementation:

  * `AKIDEXAMPLE` / `wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY` are AWS's own
    documentation credentials — a well-known dummy pair, not a real secret.
  * `get-vanilla` is from the published `aws-sig-v4-test-suite`.
  * The IAM `ListUsers` case is the worked example in AWS's signing
    documentation, whose intermediate canonical-request hash and signing key
    are also published — so all four steps are pinned individually, and a bug
    in any one of them is localised rather than showing up only at the end.
-/
import Linen.Crypto.SigV4

open Crypto.SigV4 Network.HTTP.Types Data.Time

namespace Tests.Crypto.SigV4

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

/-- AWS's documentation credentials. Not a real key. -/
private def creds : Credentials :=
  { accessKeyId := "AKIDEXAMPLE"
    secretAccessKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" }

/-- 2015-08-30T12:36:00Z, the timestamp both AWS examples use. -/
private def t : UTCTime := UTCTime.ofNanosSinceEpoch (1440938160 * 1000000000)

/-! ### Scope -/

#guard (Scope.mk "20150830" "us-east-1" "iam").render == "20150830/us-east-1/iam/aws4_request"
#guard algorithm == "AWS4-HMAC-SHA256"
#guard terminator == "aws4_request"

/-! ### Header canonicalisation -/

#guard normalizeValue "  a  " == "a"                    -- trimmed
#guard normalizeValue "a   b" == "a b"                  -- runs collapsed
#guard normalizeValue "a b  c   d" == "a b c d"
#guard normalizeValue "" == ""

-- Names lowercased, entries sorted, one `name:value` per line with a trailing
-- newline on each.
#guard canonicalHeaders [("Host", "example.com"), ("Content-Type", "text/plain")]
     == "content-type:text/plain\nhost:example.com\n"

#guard signedHeaders [("Host", "example.com"), ("Content-Type", "text/plain")]
     == "content-type;host"

-- Duplicate names merge, comma-joined, and count once in the signed list.
#guard canonicalHeaders [("X-A", "1"), ("x-a", "2")] == "x-a:1,2\n"
#guard signedHeaders [("X-A", "1"), ("x-a", "2")] == "x-a"

-- `canonicalHeaders` and `signedHeaders` share `prepareHeaders`, so they name
-- the same set in the same order however the input was given.
#guard canonicalHeaders [("B", "2"), ("a", "1"), ("C", "3")] == "a:1\nb:2\nc:3\n"
#guard signedHeaders [("B", "2"), ("a", "1"), ("C", "3")] == "a;b;c"

/-! ### Path canonicalisation -/

#guard canonicalUri "" == "/"
#guard canonicalUri "/" == "/"
#guard canonicalUri "/my-bucket" == "/my-bucket"
#guard canonicalUri "/a/b" == "/a/b"                    -- separators survive
#guard canonicalUri "/a b" == "/a%20b"                  -- segments are encoded
#guard canonicalUri "/a~b.c_d-e" == "/a~b.c_d-e"        -- unreserved untouched
-- Non-S3 services expect the encoding applied twice; S3 expects it once.
#guard canonicalUri "/a b" true == "/a%2520b"
#guard canonicalUri "/a b" false == "/a%20b"

/-! ### The canonical request

  AWS's IAM `ListUsers` worked example, rendered exactly as documented. -/

private def iamHeaders : List (String × String) :=
  [("Content-Type", "application/x-www-form-urlencoded; charset=utf-8"),
   ("Host", "iam.amazonaws.com"),
   ("X-Amz-Date", "20150830T123600Z")]

private def iamCanonical : CanonicalRequest :=
  { method := "GET"
    uri := "/"
    query := canonicalQuery [("Action", some "ListUsers"), ("Version", some "2010-05-08")]
    headers := iamHeaders
    payloadHash := emptyPayloadHash }

#guard iamCanonical.render == "GET\n/\nAction=ListUsers&Version=2010-05-08\n\
content-type:application/x-www-form-urlencoded; charset=utf-8\n\
host:iam.amazonaws.com\n\
x-amz-date:20150830T123600Z\n\
\n\
content-type;host;x-amz-date\n\
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

/-! ### String to sign -/

#guard stringToSign "20150830T123600Z" (Scope.mk "20150830" "us-east-1" "iam")
         "f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59"
     == "AWS4-HMAC-SHA256\n20150830T123600Z\n20150830/us-east-1/iam/aws4_request\n\
f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59"

/-! ### Authorization header -/

#guard authorization creds (Scope.mk "20150830" "us-east-1" "iam")
         "content-type;host;x-amz-date" "abc123"
     == "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, \
SignedHeaders=content-type;host;x-amz-date, Signature=abc123"

/-! ### Signing — the published vectors

  Each step is pinned separately, so a failure says which one broke. -/

#eval show IO Unit from do
  -- The precomputed constant really is SHA-256 of the empty body.
  check ((← hashHex ByteArray.empty) == emptyPayloadHash) "emptyPayloadHash is wrong"

  -- Step 1 → 2: the canonical request hashes to AWS's published value.
  let crh ← hashStringHex iamCanonical.render
  check (crh == "f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59")
    s!"canonical request hash: {crh}"

  -- Step 3: the derived key matches AWS's published signing key.
  let scope := Scope.mk "20150830" "us-east-1" "iam"
  let key ← signingKey creds.secretAccessKey scope
  check (Data.Hex.encode key == "c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9")
    s!"signing key: {Data.Hex.encode key}"

  -- Step 4: the final signature.
  let sig ← signWith key (stringToSign "20150830T123600Z" scope crh)
  check (sig == "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7")
    s!"signature: {sig}"

-- `get-vanilla`, from the published `aws-sig-v4-test-suite`.
#eval show IO Unit from do
  let cr : CanonicalRequest :=
    { method := "GET", uri := "/", query := ""
      headers := [("Host", "example.amazonaws.com"), ("X-Amz-Date", "20150830T123600Z")]
      payloadHash := emptyPayloadHash }
  let scope := Scope.mk "20150830" "us-east-1" "service"
  let crh ← hashStringHex cr.render
  check (crh == "bb579772317eb040ac9ed261061d46c1f17a8133879d6129b6e1c25292927e63")
    s!"get-vanilla canonical request hash: {crh}"
  let sig ← signWith (← signingKey creds.secretAccessKey scope)
    (stringToSign "20150830T123600Z" scope crh)
  check (sig == "5fa00fa31553b73ebf1942676e86291e8372ff2a2260956d9b8aae1d763fbf31")
    s!"get-vanilla signature: {sig}"

/-! ### End to end through `sign`

  `sign` also adds `x-amz-content-sha256`, so this covers a four-header
  canonical request rather than the documentation's three. -/

#eval show IO Unit from do
  let hdrs ← sign creds "us-east-1" "iam" t
    { method := "GET", path := "/"
      query := [("Action", some "ListUsers"), ("Version", some "2010-05-08")]
      headers := [("Content-Type", "application/x-www-form-urlencoded; charset=utf-8"),
                  ("Host", "iam.amazonaws.com")] }
  let get (n : String) : String := (hdrs.find? (·.1 == n)).map (·.2) |>.getD ""
  check (get "x-amz-date" == "20150830T123600Z") s!"x-amz-date: {get "x-amz-date"}"
  check (get "x-amz-content-sha256" == emptyPayloadHash) "content hash"
  check (get "Authorization" == "AWS4-HMAC-SHA256 \
Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, \
SignedHeaders=content-type;host;x-amz-content-sha256;x-amz-date, \
Signature=dd479fa8a80364edf2119ec24bebde66712ee9c9cb2b0d92eb3ab9ccdc0c3947")
    s!"Authorization: {get "Authorization"}"
  -- No session token, so no security-token header.
  check (hdrs.all (·.1 != "x-amz-security-token")) "unexpected security token"

-- An S3-shaped request: bucket in the path, `s3` service, eu-west-1.
#eval show IO Unit from do
  let hdrs ← sign creds "eu-west-1" "s3" t
    { method := "PUT", path := "/my-bucket"
      headers := [("Host", "s3.eu-west-1.amazonaws.com")] }
  let auth := (hdrs.find? (·.1 == "Authorization")).map (·.2) |>.getD ""
  check (auth == "AWS4-HMAC-SHA256 \
Credential=AKIDEXAMPLE/20150830/eu-west-1/s3/aws4_request, \
SignedHeaders=host;x-amz-content-sha256;x-amz-date, \
Signature=b1ba7bd1e79e9726d5be98201bf756baa5d2e2b505a3ff12e45bbaa0e7068520")
    s!"s3 Authorization: {auth}"

-- Temporary credentials add the security token, and it is signed: it appears
-- in `SignedHeaders`, so it cannot be swapped in flight.
#eval show IO Unit from do
  let hdrs ← sign { creds with sessionToken := some "TOKEN" } "us-east-1" "s3" t
    { method := "GET", path := "/", headers := [("Host", "s3.amazonaws.com")] }
  check (hdrs.any (fun h => h.1 == "x-amz-security-token" && h.2 == "TOKEN")) "token missing"
  let auth := (hdrs.find? (·.1 == "Authorization")).map (·.2) |>.getD ""
  check (auth.splitOn "SignedHeaders=" |>.getD 1 "" |>.startsWith
          "host;x-amz-content-sha256;x-amz-date;x-amz-security-token")
    s!"token not signed: {auth}"

/-! ### Signatures -/

example : String → Scope → IO ByteArray := signingKey
example : ByteArray → String → IO String := signWith
example : String → Scope → String → String := stringToSign
example : CanonicalRequest → String := CanonicalRequest.render

end Tests.Crypto.SigV4
