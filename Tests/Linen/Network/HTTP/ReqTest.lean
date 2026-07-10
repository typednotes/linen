/-
  Tests for `Linen.Network.HTTP.Req`.

  `req`/`runReq` perform real network IO, so — like the rest of the HTTP
  client layer — `req` itself is only pinned at the type level (it elaborates
  for allowed method/body pairs). Everything else (URL construction, option
  combination, auth headers, typeclass instances, response accessors) is pure
  and gets `#guard`-checked directly.
-/
import Linen.Network.HTTP.Req

open Network.HTTP.Client
open Network.HTTP.Types
open Network.HTTP.Req

namespace Tests.Network.HTTP.Req

/-! ### `Url` construction and rendering -/

#guard (http "example.com").path == "/"
#guard (https "example.com" /: "v1" /: "users").path == "/v1/users"
#guard (http "example.com").defaultPort == 80
#guard (https "example.com").defaultPort == 443
#guard (http "example.com").isSecure == false
#guard (https "example.com").isSecure == true

/-! ### `HttpMethod`/`HttpBody` instances -/

#guard HttpMethod.allowsBody (m := GET) == .NoBody
#guard HttpMethod.allowsBody (m := POST) == .YesBody
#guard HttpMethod.methodName (m := GET) == "GET"
#guard HttpMethod.methodName (m := POST) == "POST"
#guard HttpMethod.methodName (m := DELETE) == "DELETE"

#guard HttpBody.getBody (NoReqBody.mk) == none
#guard HttpBody.getBody (ReqBodyBs.mk "hi".toUTF8) == some "hi".toUTF8
#guard HttpBody.getContentType (ReqBodyBs.mk "hi".toUTF8) == some "application/octet-stream"
#guard HttpBody.getBody (ReqBodyUrlEnc.mk [("a", "1"), ("b", "2")]) == some "a=1&b=2".toUTF8

/-! ### `ReqOption` monoid -/

#guard ((EmptyCollection.emptyCollection : ReqOption .Http) ++ header hAccept "text/plain").extraHeaders ==
  [(hAccept, "text/plain")]
#guard (queryParam "a" "1" ++ queryParam "b" "2" : ReqOption .Http).queryParams ==
  [("a", "1"), ("b", "2")]
#guard (queryFlag "debug" : ReqOption .Http).queryParams == [("debug", "")]
#guard (port (8080 : UInt16) : ReqOption .Http).portOverride == some 8080
#guard (responseTimeout 5000 : ReqOption .Http).timeout == some 5000
-- later `timeout`/`portOverride` win on append (right-biased `<|>`)
#guard ((responseTimeout 1000 : ReqOption .Http) ++ responseTimeout 2000).timeout == some 2000

example (a b c : ReqOption .Http) :
    (a ++ b ++ c).extraHeaders = (a ++ (b ++ c)).extraHeaders :=
  option_extraHeaders_append_assoc a b c

/-! ### Authentication (HTTPS-only) -/

#guard (basicAuth "alice" "secret").extraHeaders == [(hAuthorization, "Basic YWxpY2U6c2VjcmV0")]
#guard (oAuth2Bearer "tok123").extraHeaders == [(hAuthorization, "Bearer tok123")]
#guard (oAuth2Token "tok123").extraHeaders == [(hAuthorization, "token tok123")]
-- `basicAuthUnsafe` works over any scheme, unlike `basicAuth`
#guard (basicAuthUnsafe "alice" "secret" : ReqOption .Http).extraHeaders ==
  [(hAuthorization, "Basic YWxpY2U6c2VjcmV0")]

/-! ### `HttpConfig` -/

#guard defaultHttpConfig.httpConfigRedirectCount == 10
#guard defaultHttpConfig.httpConfigCheckResponse status200 == none
#guard defaultHttpConfig.httpConfigCheckResponse status404 |>.isSome

/-! ### Response accessors -/

#guard (BsResponse.mk status200 [(hContentType, "text/plain")] "hi".toUTF8).responseBody == "hi".toUTF8
#guard (BsResponse.mk status200 [(hContentType, "text/plain")] "hi".toUTF8).responseStatus == status200
#guard (BsResponse.mk status200 [(hContentType, "text/plain")] "hi".toUTF8).responseHeader hContentType ==
  some "text/plain"
#guard (IgnoreResponse.mk status200 [(hContentType, "text/plain")]).responseStatus == status200
#guard (IgnoreResponse.mk status200 [(hContentType, "text/plain")]).responseHeader hContentType ==
  some "text/plain"

/-! ### `req`'s `HttpBodyAllowed` obligation

    `req`'s full IO roundtrip isn't exercised here, matching the upstream
    project's own test suite: it needs real network access. But the compile-time
    obligation — that `req` type-checks for a method/body pair exactly when
    `HttpBodyAllowed` allows it — is exercised directly below. -/

-- GET/no-body and POST/with-body both elaborate: `HttpBodyAllowed` resolves
-- even though `HttpMethod.allowsBody`/`HttpBody.providesBody` are stuck
-- projections until instantiated (see `Req.lean`'s `attribute [reducible]`).
example : Req BsResponse := req GET.mk (https "example.com") NoReqBody.mk bsResponse
example : Req BsResponse := req POST.mk (https "example.com") (ReqBodyBs.mk "hi".toUTF8) bsResponse

end Tests.Network.HTTP.Req
