/-
  Tests for `Cloud.Protocol.S3`.

  The `ListObjectsV2` document below is a real S3 reply, kept whole: the
  namespace attribute, the sibling elements a client ignores, and — the detail
  that matters — the `ETag` arriving as `&quot;…&quot;`, so the XML unescaping
  and the quote-stripping are both exercised on the shape they actually meet.
-/
import Linen.Cloud.Protocol.S3

open Cloud Cloud.Protocol.S3
open Network.HTTP.Types (status200 status404)

namespace Tests.Cloud.Protocol.S3

-- ── Paths ───────────────────────────────────────────────────────────────────

#guard bucketPath "assets" == "/assets"
#guard objectPath "assets" "logs/2026/a.json" == "/assets/logs/2026/a.json"

/- A key is an opaque byte string, not a path: `a//b`, `a/` and `/a` are three
   different keys, so nothing is normalised. Collapsing `//` here would make a
   whole family of keys unaddressable. -/
#guard objectPath "b" "a//b" == "/b/a//b"
#guard objectPath "b" "a/" == "/b/a/"
#guard objectPath "b" "/a" == "/b//a"

-- ── `Content-MD5` ───────────────────────────────────────────────────────────

/- AWS's own documented value for the empty body. S3 refuses several
   configuration writes without this header, and SigV4's
   `x-amz-content-sha256` does not satisfy it — different purposes. -/
#guard contentMd5 ByteArray.empty == "1B2M2Y8AsgTpgAmY7PhCfg=="
#guard contentMd5 "hello".toUTF8 == "XUFAKrxLKna5cZ2REBfFkg=="

-- ── A captured `ListObjectsV2` reply ────────────────────────────────────────

def listReply : String :=
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">" ++
    "<Name>assets</Name>" ++
    "<Prefix>logs/</Prefix>" ++
    "<KeyCount>2</KeyCount>" ++
    "<MaxKeys>1000</MaxKeys>" ++
    "<Delimiter>/</Delimiter>" ++
    "<IsTruncated>true</IsTruncated>" ++
    "<NextContinuationToken>1ueGcxLPRx1Tr/XYExHnhbYLgveDs2J/wm36Hy4vbOwM=</NextContinuationToken>" ++
    "<Contents>" ++
      "<Key>logs/2026/a.json</Key>" ++
      "<LastModified>2026-09-01T10:00:00.000Z</LastModified>" ++
      "<ETag>&quot;d41d8cd98f00b204e9800998ecf8427e&quot;</ETag>" ++
      "<Size>1024</Size>" ++
      "<StorageClass>STANDARD</StorageClass>" ++
    "</Contents>" ++
    "<Contents>" ++
      "<Key>logs/2026/b.json</Key>" ++
      "<ETag>&quot;e2fc714c4727ee9395f324cd2e7f331f&quot;</ETag>" ++
      "<Size>2048</Size>" ++
    "</Contents>" ++
  "</ListBucketResult>"

/-- The parsed document, for the checks below. -/
def parsed : Text.XML.Element :=
  match Text.XML.parse listReply with
  | .ok el => el
  | .error _ => { name := { local' := "parse-failed" } }

#guard parsed.name.local' == "ListBucketResult"
#guard parsed.childText "Name" == some "assets"
#guard (parsed.named "Contents").length == 2

#guard (parsed.named "Contents").head?.bind (·.childText "Key") == some "logs/2026/a.json"
#guard parseNat ((parsed.named "Contents").head?.bind (·.childText "Size")) == 1024

/- **The ETag arrives quoted, and `&quot;` must be unescaped first.** Comparing
   an unstripped ETag against a computed MD5 silently never matches, which is
   the kind of bug that looks like a corruption problem. -/
#guard (parsed.named "Contents").head?.bind (·.childText "ETag")
  == some "\"d41d8cd98f00b204e9800998ecf8427e\""
#guard unquoteETag ((((parsed.named "Contents").head?.bind (·.childText "ETag"))).getD "")
  == "d41d8cd98f00b204e9800998ecf8427e"

-- ── Truncation and the continuation token ───────────────────────────────────

#guard parseBool (parsed.childText "IsTruncated") == true
#guard nextContinuationToken? parsed
  == some ⟨"1ueGcxLPRx1Tr/XYExHnhbYLgveDs2J/wm36Hy4vbOwM="⟩

/-- A final page: `IsTruncated` false and no token. -/
def finalReply : String :=
  "<ListBucketResult><Name>assets</Name><KeyCount>0</KeyCount>" ++
  "<IsTruncated>false</IsTruncated></ListBucketResult>"

#guard match Text.XML.parse finalReply with
  | .ok el => (nextContinuationToken? el).isNone && !parseBool (el.childText "IsTruncated")
  | .error _ => false

/- **A token on a final page is ignored.** S3 has been observed to send one;
   trusting it costs a request that returns nothing and, in a bounded read,
   reports the listing incomplete when it is not. `IsTruncated` is the
   authority. -/
#guard match Text.XML.parse
    ("<ListBucketResult><IsTruncated>false</IsTruncated>" ++
     "<NextContinuationToken>stale</NextContinuationToken></ListBucketResult>") with
  | .ok el => (nextContinuationToken? el).isNone
  | .error _ => false

-- ── Missing fields are diagnosable ──────────────────────────────────────────

#guard match requireChildText parsed "Name" with
  | .ok v => v == "assets"
  | .error _ => false

/- A provider that stops sending a documented field produces an error naming
   it, rather than a silently defaulted value. -/
#guard match requireChildText parsed "NoSuchElement" with
  | .error e => e.klass == .protocol && (e.message.splitOn "NoSuchElement").length == 2
  | .ok _ => false

-- ── Calls, through a stub transport ─────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def ep : Endpoint :=
  { host := "s3.eu-west-3.amazonaws.com", service := "s3", region := "eu-west-3" }

def respondWith (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status)
    (body : String) : Transport :=
  Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.path}{req.queryString}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/- S3 signs its path exactly as sent, so `doubleEncodePath` is false. Every
   other AWS dialect double-encodes, and getting it wrong yields
   `SignatureDoesNotMatch` with nothing to indicate why. -/
#guard (call creds ep "GET" (bucketPath "assets")).doubleEncodePath == false

/-- info: (["GET /assets?list-type=2&prefix=logs%2F"], "assets") -/
#guard_msgs in
#eval show IO (List String × String) from do
  let log ← IO.mkRef []
  let c := call creds ep "GET" (bucketPath "assets")
    (query := [("list-type", some "2"), ("prefix", some "logs/")])
  match ← sendXml (respondWith log status200 listReply) c with
  | .ok el   => return (← log.get, (el.childText "Name").getD "?")
  | .error e => return (← log.get, toString e)

/-- info: (Cloud.Class.notFound, "NoSuchKey") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "<Error><Code>NoSuchKey</Code><Message>The specified key does not exist.</Message></Error>"
  match ← sendXml (respondWith log status404 body) (call creds ep "GET" (objectPath "assets" "gone")) with
  | .error e => return (e.klass, e.code)
  | .ok _ => return (.protocol, "")

/- A 200 with an empty body where XML was expected is a `protocol` error, not
   an empty document: every S3 operation that answers XML answers with some. -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  match ← sendXml (respondWith log status200 "") (call creds ep "GET" (bucketPath "assets")) with
  | .error e => return e.klass
  | .ok _ => return .notFound

/- `PUT` and `DELETE` answer 200 with nothing, and that is success. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  let c := call creds ep "PUT" (objectPath "assets" "a.json") (body := "{}".toUTF8)
  match ← sendUnit (respondWith log status200 "") c with
  | .ok _ => return true
  | .error _ => return false

end Tests.Cloud.Protocol.S3
