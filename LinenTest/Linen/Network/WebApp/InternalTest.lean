import Linen.Network.WebApp.Internal
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Internal`

    Coverage: `Response` construction/accessors/laws (all `#guard`/`rfl`),
    `ResponseState.pending_ne_sent`, and the `AppM` indexed monad's runtime
    behaviour (`respond`/`respondIO`/`ioThen`/`unsafeLift`, exercised via
    `Green.block` since `AppM` wraps `Green`). -/

open Network.WebApp Network.HTTP.Types Control.Concurrent.Green

namespace Tests.Network.WebApp.Internal

-- ── Response construction and accessors ──

#guard (Response.responseBuilder status200 [] "hi".toUTF8).status.statusCode == 200
#guard (Response.responseBuilder status404 [] ByteArray.empty).status.statusCode == 404
#guard (Response.responseBuilder status200 [(hContentType, "text/plain")] ByteArray.empty).headers.length == 1
#guard (Response.responseBuilder status200 [] ByteArray.empty).bodyIsEmpty
#guard !(Response.responseBuilder status200 [] "x".toUTF8).bodyIsEmpty
#guard !(Response.responseFile status200 [] "/tmp/x" none).bodyIsEmpty
#guard !(Response.responseStream status200 [] (fun _ _ => pure ())).bodyIsEmpty

-- `mapResponseStatus`/`mapResponseHeaders`
#guard ((Response.responseBuilder status200 [] ByteArray.empty).mapResponseStatus
          (fun _ => status301)).status.statusCode == 301
#guard ((Response.responseBuilder status200 [] ByteArray.empty).mapResponseHeaders
          (fun h => (hServer, "linen") :: h)).headers.length == 1

-- Response accessor laws proved by `rfl` in `Internal.lean` — spot-checked here too.
example : (Response.responseBuilder status200 [] ByteArray.empty).status = status200 := rfl
example : (Response.responseFile status404 [] "/x" none).status = status404 := rfl

-- ── ResponseState ──

example : ResponseState.pending ≠ ResponseState.sent := ResponseState.pending_ne_sent

-- ── AppM: respond/respondIO/ioThen/unsafeLift, run via `Green.block` ──

/-- A minimal `Application`-shaped computation using `AppM.respond` directly. -/
def simpleApp : Request → (Response → Green ResponseReceived) → AppM .pending .sent ResponseReceived :=
  fun _req respond => AppM.respond respond (Response.responseBuilder status200 [] "ok".toUTF8)

/-- Exercises `AppM.respondIO`: compute a `Response` via `IO`, then respond. -/
def echoApp : Request → (Response → Green ResponseReceived) → AppM .pending .sent ResponseReceived :=
  fun req respond =>
    AppM.respondIO respond do
      let body ← req.requestBody
      pure (Response.responseBuilder status200 [] body)

#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block ((simpleApp {
      requestMethod := .standard .GET, httpVersion := http11, rawPathInfo := "/"
      rawQueryString := "", requestHeaders := [], isSecure := false
      remoteHost := ⟨"127.0.0.1", 0⟩, pathInfo := [], queryString := []
      requestBody := pure ByteArray.empty, vault := Data.Vault.empty
      requestBodyLength := .knownLength 0, requestHeaderHost := none
      requestHeaderRange := none, requestHeaderReferer := none
      requestHeaderUserAgent := none } respond).run) tok
  match ← captured.get with
  | some resp =>
    unless resp.status.statusCode == 200 && resp.headers == ([] : ResponseHeaders) do
      throw (IO.userError "simpleApp: unexpected response")
  | none => throw (IO.userError "simpleApp: respond was never called")

#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let req : Request :=
    { requestMethod := .standard .POST, httpVersion := http11, rawPathInfo := "/"
      rawQueryString := "", requestHeaders := [], isSecure := false
      remoteHost := ⟨"127.0.0.1", 0⟩, pathInfo := [], queryString := []
      requestBody := pure "hello".toUTF8, vault := Data.Vault.empty
      requestBodyLength := .knownLength 5, requestHeaderHost := none
      requestHeaderRange := none, requestHeaderReferer := none
      requestHeaderUserAgent := none }
  let _ ← Green.block ((echoApp req respond).run) tok
  match ← captured.get with
  | some resp => unless resp.status.statusCode == 200 && resp.headers.isEmpty do
      throw (IO.userError "echoApp: unexpected status/headers")
  | none => throw (IO.userError "echoApp: respond was never called")

-- `ioThen`: perform IO, then delegate to a state-changing continuation.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let ranIO ← IO.mkRef false
  let respond : Response → Green ResponseReceived := fun _ => pure ResponseReceived.done
  let app : AppM .pending .sent ResponseReceived :=
    AppM.ioThen (ranIO.set true) fun () => AppM.respond respond (Response.responseBuilder status200 [] ByteArray.empty)
  let _ ← Green.block app.run tok
  unless ← ranIO.get do throw (IO.userError "ioThen: the IO action never ran")

-- `unsafeLift`: escape hatch runs its IO action.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let n ← Green.block (AppM.unsafeLift (α := Nat) (pre := .pending) (post := .pending) (pure 7)).run tok
  unless n == 7 do throw (IO.userError s!"unsafeLift expected 7, got {n}")

end Tests.Network.WebApp.Internal
