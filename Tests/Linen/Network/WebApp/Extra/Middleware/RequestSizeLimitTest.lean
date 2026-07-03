import Linen.Network.WebApp.Extra.Middleware.RequestSizeLimit
import Linen.Network.WebApp.Extra.Test
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.RequestSizeLimit`

    Coverage: known-length bodies over the limit are rejected with 413
    before the inner app runs; bodies within the limit pass through; a
    chunked body that grows past the limit while being read raises an
    error from the wrapped body reader. The chunked case needs a custom
    `requestBodyLength`/`requestBody`, which `SRequest` doesn't expose (it
    always reports a known length), so it drives a `Request` directly via
    `Green.block`, following `WebApp.Static.ApplicationTest`. -/

open Network.WebApp Network.WebApp.Extra.Middleware Network.WebApp.Extra.Test
open Network.HTTP.Types
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Extra.Middleware.RequestSizeLimit

def okApp : Application :=
  fun _req respond => AppM.respond respond (responseLBS status200 [] "ok")

#eval show IO Unit from do
  let resp ← post (requestSizeLimit 5 okApp) "/" "0123456789".toUTF8
  unless resp.simpleStatus.statusCode == 413 do
    throw (IO.userError s!"expected 413 for oversized known-length body, got {resp.simpleStatus.statusCode}")

#eval show IO Unit from do
  let resp ← post (requestSizeLimit 100 okApp) "/" "0123456789".toUTF8
  unless resp.simpleStatus.statusCode == 200 do
    throw (IO.userError s!"expected 200 for body within limit, got {resp.simpleStatus.statusCode}")

/-- Run `app` against `req`, capturing the `Response` it produces. -/
def runApp (app : Application) (req : Request) : IO Response := do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block (app req respond).run tok
  match ← captured.get with
  | some resp => pure resp
  | none => throw (IO.userError "app: respond was never called")

#eval show IO Unit from do
  -- Three 4-byte chunks (12 bytes total) against a 10-byte limit: the
  -- wrapped body reader should raise once the running total passes 10.
  let chunks ← IO.mkRef ([ "aaaa".toUTF8, "bbbb".toUTF8, "cccc".toUTF8, ByteArray.empty ] : List ByteArray)
  let body : IO ByteArray := do
    match ← chunks.get with
    | c :: rest => chunks.set rest; pure c
    | [] => pure ByteArray.empty
  let req := { defaultRequest with requestBodyLength := .chunkedBody, requestBody := body }
  let readAllApp : Application :=
    fun req respond =>
      AppM.respondIO respond (do
        let _ ← strictRequestBody req
        pure (responseLBS status200 [] "should not get here"))
  let raised ← (try
    let _ ← runApp (requestSizeLimit 10 readAllApp) req
    pure false
  catch _ => pure true)
  unless raised do
    throw (IO.userError "expected reading past the chunked limit to raise an error")

end Tests.Network.WebApp.Extra.Middleware.RequestSizeLimit
