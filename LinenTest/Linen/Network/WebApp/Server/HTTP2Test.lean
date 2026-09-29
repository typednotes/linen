/-
  Tests for `Linen.Network.WebApp.Server.HTTP2`.

  The mapping from an HTTP/2 request to a `Network.WebApp.Request`, and every
  kind of `Response` written to a recording `Network.HTTP2.Responder`. The
  bridge end to end — over TLS, with ALPN, against our own client and curl —
  is in `Network.WebApp.Server.TLSTest`.
-/
import Linen.Network.WebApp.Server.HTTP2

open Network.WebApp.Server
open Network.WebApp
open Network.HTTP.Types
open Control.Concurrent.Green (Green)

namespace Tests.Network.WebApp.Server.HTTP2

private def h2req (path : String) (headers : List (String × String) := [])
    (authority : Option String := some "example.com") : Network.HTTP2.Request :=
  { streamId := 1, method := "POST", scheme := some "https", authority, path, headers,
    contentLength := some 3, body := pure "abc".toUTF8 }

private def addr : Network.Socket.SockAddr := { host := "127.0.0.1", port := 1 }

/-! ### `http2Request` -/

#guard (http2Request (h2req "/a/b?x=1&y") addr true).rawPathInfo == "/a/b"
#guard (http2Request (h2req "/a/b?x=1&y") addr true).rawQueryString == "?x=1&y"
#guard (http2Request (h2req "/a/b?x=1&y") addr true).pathInfo == ["a", "b"]
#guard (http2Request (h2req "/") addr true).httpVersion == http20
#guard (http2Request (h2req "/") addr true).isSecure
#guard (http2Request (h2req "/") addr true).requestMethod == .standard .POST
#guard (http2Request (h2req "/") addr true).requestBodyLength == .knownLength 3
-- `:authority` stands in for Host…
#guard (http2Request (h2req "/") addr true).requestHeaderHost == some "example.com"
-- …unless a `host` field was sent too.
#guard (http2Request (h2req "/" [("host", "other")]) addr true).requestHeaderHost == some "other"
#guard (http2Request (h2req "/" [] none) addr true).requestHeaderHost == none
#guard (http2Request (h2req "/" [("user-agent", "t"), ("referer", "r")]) addr true).requestHeaderUserAgent
  == some "t"
#guard (http2Request { h2req "/" with contentLength := none } addr true).requestBodyLength == .chunkedBody

/-! ### `sendHttp2Response` over a recording responder -/

/-- What `sendHttp2Response` does with `resp`: the calls it makes, in order. -/
private def record (resp : Response) : IO (List String) := do
  let log ← IO.mkRef ([] : List String)
  let responder : Network.HTTP2.Responder := {
    respond := fun status headers endStream =>
      log.modify (· ++ [s!"respond {status} {headers} end={endStream}"])
    write := fun bytes => log.modify (· ++ [s!"write {String.fromUTF8! bytes}"])
    finish := log.modify (· ++ ["finish"]) }
  let settings : Settings := { defaultSettings with settingsAddServerHeader := false }
  let _ ← Green.block (sendHttp2Response responder settings resp) (← Std.CancellationToken.new)
  log.get

#eval do
  let got ← record (.responseBuilder status200 [(Data.CI.mk' "X-A", "1")] "hi".toUTF8)
  unless got == ["respond 200 [(X-A, 1), (content-length, 2)] end=false", "write hi", "finish"] do
    throw (IO.userError s!"builder: {got}")

-- An empty body ends the stream with the headers.
#eval do
  let got ← record (.responseBuilder status204 [] ByteArray.empty)
  unless got == ["respond 204 [(content-length, 0)] end=true"] do throw (IO.userError s!"empty: {got}")

-- A file: its content-length, then its bytes.
#eval do
  let (h, path) ← IO.FS.createTempFile
  h.putStr "0123456789"
  h.flush
  let got ← record (.responseFile status200 [] path.toString (some ⟨2, 3⟩))
  IO.FS.removeFile path
  unless got == ["respond 200 [(content-length, 3)] end=false", "write 234", "finish"] do
    throw (IO.userError s!"file: {got}")

-- A stream: each write as it is produced, then the end.
#eval do
  let got ← record (.responseStream status200 [] fun write _ => do write "a".toUTF8; write "b".toUTF8)
  unless got == ["respond 200 [] end=false", "write a", "write b", "finish"] do
    throw (IO.userError s!"stream: {got}")

-- `responseRaw` cannot take over a multiplexed stream: its fallback is sent.
#eval do
  let got ← record (.responseRaw (fun _ _ => pure ()) (.responseBuilder status500 [] "no".toUTF8))
  unless got == ["respond 500 [(content-length, 2)] end=false", "write no", "finish"] do
    throw (IO.userError s!"raw: {got}")

-- The Server field, when the settings ask for it.
#eval show IO Unit from do
  let log ← IO.mkRef ([] : List (String × String))
  let responder : Network.HTTP2.Responder := {
    respond := fun _ headers _ => log.set headers
    write := fun _ => pure ()
    finish := pure () }
  let token ← Std.CancellationToken.new
  let send := sendHttp2Response responder defaultSettings (.responseBuilder status200 [] ByteArray.empty)
  let _ ← Green.block send token
  unless (← log.get).lookup "server" == some defaultSettings.settingsServerName do
    throw (IO.userError "no server field")

end Tests.Network.WebApp.Server.HTTP2
