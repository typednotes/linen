import Linen.Network.WebApp.Extra.Middleware.StreamFile
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.StreamFile`

    Coverage: a `.responseFile` response is converted into a
    `.responseStream` that yields the file's content; other response kinds
    pass through unchanged. `Network.WebApp.Extra.Test`'s `SResponse` only
    captures builder bodies, so this drives `Request`s directly via
    `Green.block`, following `WebApp.Static.ApplicationTest`, and manually
    invokes the returned `StreamingBody`. -/

open Network.WebApp Network.WebApp.Extra.Middleware
open Network.HTTP.Types
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Extra.Middleware.StreamFile

/-- Run `app` against `defaultRequest`, capturing the `Response` it produces. -/
def runApp (app : Application) : IO Response := do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block (app defaultRequest respond).run tok
  match ← captured.get with
  | some resp => pure resp
  | none => throw (IO.userError "app: respond was never called")

/-- Drain a `StreamingBody` into a single `ByteArray`. -/
def drain (body : StreamingBody) : IO ByteArray := do
  let bufRef ← IO.mkRef ByteArray.empty
  body (fun chunk => bufRef.modify (· ++ chunk)) (pure ())
  bufRef.get

#eval show IO Unit from do
  let path := "/tmp/linen_streamfile_test.txt"
  IO.FS.writeFile path "streamed-content"
  let fileApp : Application :=
    fun _req respond => AppM.respond respond (.responseFile status200 [] path none)
  let resp ← runApp (streamFile fileApp)
  match resp with
  | .responseStream _ _ body =>
    let content ← drain body
    unless String.fromUTF8! content == "streamed-content" do
      throw (IO.userError s!"expected file content, got {String.fromUTF8! content}")
  | _ => throw (IO.userError "expected a responseStream")

#eval show IO Unit from do
  let builderApp : Application :=
    fun _req respond => AppM.respond respond (.responseBuilder status200 [] "unchanged".toUTF8)
  let resp ← runApp (streamFile builderApp)
  match resp with
  | .responseBuilder _ _ body =>
    unless String.fromUTF8! body == "unchanged" do
      throw (IO.userError "expected builder response to pass through unchanged")
  | _ => throw (IO.userError "expected a responseBuilder")

end Tests.Network.WebApp.Extra.Middleware.StreamFile
