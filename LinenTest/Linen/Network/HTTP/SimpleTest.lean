/-
  Tests for `Linen.Network.HTTP.Simple`.

  `parseUrl` is pure and gets `#guard`-checked directly. `simpleHttp`,
  `httpBS`, and `httpLbs` perform real network IO, so — like the rest of
  the HTTP client layer — they're pinned at the type level.
-/
import Linen.Network.HTTP.Simple

open Network.HTTP.Client
open Network.HTTP.Types

namespace Tests.Network.HTTP.Simple

/-! ### `parseUrl` -/

/-- `Request` has no `BEq` (its `Method`/`RequestHeaders` fields don't derive one),
    so parsed results are checked field-by-field instead. -/
private def checkParsed (r : Option Request) (host : String) (port : UInt16) (path : String)
    (queryString : String) (isSecure : Bool) : Bool :=
  match r with
  | some req => req.host == host && req.port == port && req.path == path &&
                req.queryString == queryString && req.isSecure == isSecure
  | none => false

#guard checkParsed (Network.HTTP.Simple.parseUrl "http://example.com") "example.com" 80 "/" "" false
#guard checkParsed (Network.HTTP.Simple.parseUrl "https://example.com") "example.com" 443 "/" "" true
#guard checkParsed (Network.HTTP.Simple.parseUrl "http://example.com:8080/path")
  "example.com" 8080 "/path" "" false
#guard checkParsed (Network.HTTP.Simple.parseUrl "http://example.com/path?q=1")
  "example.com" 80 "/path" "?q=1" false

#guard (Network.HTTP.Simple.parseUrl "ftp://example.com").isNone
#guard (Network.HTTP.Simple.parseUrl "not a url").isNone

/-! ### IO signatures — real network IO, pinned rather than exercised -/

example : String → IO Request := Network.HTTP.Simple.parseUrl!
example : String → IO ByteArray := Network.HTTP.Simple.simpleHttp
example : Request → IO Response := Network.HTTP.Simple.httpBS
example : Request → IO ByteArray := Network.HTTP.Simple.httpLbs

end Tests.Network.HTTP.Simple
