import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Test`

    Coverage: `toWebAppRequest` correctly splits `path` into `rawPathInfo` /
    `rawQueryString` / `pathInfo` / `queryString`, fills in the fixed
    simulated fields, and gives the body a one-shot read-then-empty
    contract; `get`/`post`/`runSession` drive an `Application` and capture
    its `.responseBuilder` result. -/

open Network.WebApp Network.WebApp.Extra.Test
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Test

#eval show IO Unit from do
  let req ← toWebAppRequest { path := "/a/b" }
  unless req.rawPathInfo == "/a/b" do throw (IO.userError "rawPathInfo mismatch")
  unless req.pathInfo == ["a", "b"] do throw (IO.userError "pathInfo mismatch")
  unless req.rawQueryString == "" do throw (IO.userError "rawQueryString mismatch")
  unless req.queryString == [] do throw (IO.userError "queryString mismatch")

#eval show IO Unit from do
  let req ← toWebAppRequest { path := "/search?q=hi&x=1" }
  unless req.rawPathInfo == "/search" do throw (IO.userError "rawPathInfo mismatch")
  unless req.pathInfo == ["search"] do throw (IO.userError "pathInfo mismatch")
  unless req.queryString == [("q", some "hi"), ("x", some "1")] do
    throw (IO.userError "queryString mismatch")

#eval show IO Unit from do
  let req ← toWebAppRequest {}
  unless req.remoteHost.host == "127.0.0.1" do throw (IO.userError "remoteHost mismatch")
  unless req.requestHeaderHost == some "localhost" do throw (IO.userError "requestHeaderHost mismatch")
  unless req.requestHeaderUserAgent == some "linen-Test/1.0" do
    throw (IO.userError "requestHeaderUserAgent mismatch")

#eval show IO Unit from do
  let req ← toWebAppRequest { isSecure := true }
  unless req.isSecure == true do throw (IO.userError "isSecure mismatch")

#eval show IO Unit from do
  -- One-shot body contract: the first read returns the full body, every
  -- subsequent read returns empty (needed for `strictRequestBody` to halt).
  let req ← toWebAppRequest { body := "hello".toUTF8 }
  let first ← req.requestBody
  let second ← req.requestBody
  unless String.fromUTF8! first == "hello" do throw (IO.userError "expected first read to return the body")
  unless second.isEmpty do throw (IO.userError "expected second read to return empty (EOF)")

def echoApp : Application :=
  fun req respond =>
    AppM.respond respond (responseLBS status200 [] (toString req.requestMethod))

#eval show IO Unit from do
  let resp ← get echoApp "/anything"
  unless String.fromUTF8! resp.simpleBody == "GET" do
    throw (IO.userError "expected get to issue a GET request")
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError "expected status 200")

def echoBodyApp : Application :=
  fun req respond => AppM.respondIO respond (do
    let body ← Network.WebApp.strictRequestBody req
    pure (responseLBS status200 [] (String.fromUTF8! body)))

#eval show IO Unit from do
  let resp ← post echoBodyApp "/submit" "hello".toUTF8
  unless String.fromUTF8! resp.simpleBody == "hello" do
    throw (IO.userError "expected post to deliver the given body")

end Tests.Network.WebApp.Extra.Test
