/-
  Linen.Network.WebApp.Extra.Test — WebApp testing utilities

  A simulated `Network.WebApp` environment for testing `Application`s
  without a real network connection. Ports `Network.Wai.Test`.
-/
import Linen.Network.WebApp
import Linen.Control.Concurrent.Green

namespace Network.WebApp.Extra.Test

open Network.WebApp
open Network.HTTP.Types
open Control.Concurrent.Green (Green)

/-- A simulated request for testing. -/
structure SRequest where
  method : Method := .standard .GET
  path : String := "/"
  headers : RequestHeaders := []
  body : ByteArray := ByteArray.empty
  isSecure : Bool := false

/-- A captured response from testing. -/
structure SResponse where
  simpleStatus : Status
  simpleHeaders : ResponseHeaders
  simpleBody : ByteArray

/-- Build a `Network.WebApp.Request` from an `SRequest`. The body is a
    one-shot chunk reader (yields the full body once, then empty), matching
    the contract every real `requestBody` action follows — required for
    `strictRequestBody`'s read-until-empty loop to terminate. -/
def toWebAppRequest (sreq : SRequest) : IO Request := do
  let (rawPath, rawQuery) := match sreq.path.splitOn "?" with
    | [p, q] => (p, "?" ++ q)
    | _ => (sreq.path, "")
  let segments := rawPath.splitOn "/" |>.filter (!·.isEmpty)
  let consumed ← IO.mkRef false
  let bodyRef : IO ByteArray := do
    if ← consumed.get then
      pure ByteArray.empty
    else
      consumed.set true
      pure sreq.body
  pure
    { requestMethod := sreq.method
      httpVersion := http11
      rawPathInfo := rawPath
      rawQueryString := rawQuery
      requestHeaders := sreq.headers
      isSecure := sreq.isSecure
      remoteHost := ⟨"127.0.0.1", 0⟩
      pathInfo := segments
      queryString := parseQuery rawQuery
      requestBody := bodyRef
      vault := Data.Vault.empty
      requestBodyLength := .knownLength sreq.body.size
      requestHeaderHost := some "localhost"
      requestHeaderRange := none
      requestHeaderReferer := (sreq.headers.find? (·.1 == hReferer)).map (·.2)
      requestHeaderUserAgent := some "linen-Test/1.0" }

/-- Run an `Application` with a simulated request and capture the response,
    via `Green.block`.
    $$\text{runSession} : \text{Application} \to \text{SRequest} \to \text{IO SResponse}$$ -/
def runSession (app : Application) (sreq : SRequest) : IO SResponse := do
  let webAppReq ← toWebAppRequest sreq
  let resultRef ← IO.mkRef (none : Option SResponse)
  let token ← Std.CancellationToken.new
  let _received ← Green.block (app webAppReq fun resp => do
    let body := match resp with
      | .responseBuilder _ _ b => b
      | _ => ByteArray.empty
    (resultRef.set (some ⟨resp.status, resp.headers, body⟩) : IO _)
    return ResponseReceived.done).run token
  match ← resultRef.get with
  | some r => return r
  | none => throw (IO.Error.userError "Application did not call respond")

/-- Convenience: GET request. -/
def get (app : Application) (path : String) : IO SResponse :=
  runSession app { path }

/-- Convenience: POST request with a body. -/
def post (app : Application) (path : String) (body : ByteArray)
    (contentType : String := "application/octet-stream") : IO SResponse :=
  let hdrs : RequestHeaders := [(hContentType, contentType)]
  runSession app { method := .standard .POST, path := path, body := body, headers := hdrs }

end Network.WebApp.Extra.Test
