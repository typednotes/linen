/-
  Tests for `Linen.Network.WebApp.Server.Response`.

  The pure rendering helpers are checked with `#guard`; `sendResponseTo` is
  checked over a recording `ResponseSink`, for every kind of response, with
  no socket; the socket-bound wrappers are pinned at the type level.
-/
import Linen.Network.WebApp.Server.Response

open Network.WebApp.Server
open Network.HTTP.Types
open Network.Socket
open Control.Concurrent.Green (Green)

namespace Tests.Network.WebApp.Server.Response

/-! ### Status-line / header rendering (pure) -/

#guard renderStatusLine http11 status200 == "HTTP/1.1 200 OK\r\n"
#guard renderStatusLineBytes http11 status200 == "HTTP/1.1 200 OK\r\n".toUTF8

#guard renderHeaders [(hContentLength, "5")] == "Content-Length: 5\r\n"
#guard renderHeaders [] == ""
#guard renderHeadersBytes [(hContentLength, "5")] == "Content-Length: 5\r\n".toUTF8

/-! ### `filePartLength` -/

#guard filePartLength 10 none == 10
#guard filePartLength 10 (some ⟨2, 3⟩) == 3
#guard filePartLength 10 (some ⟨7, 0⟩) == 3   -- `count = 0`: to the end
#guard filePartLength 10 (some ⟨8, 100⟩) == 2
#guard filePartLength 10 (some ⟨20, 5⟩) == 0

/-! ### `sendResponseTo` over a recording sink -/

/-- The bytes `sendResponseTo` writes for `resp`, all paths of the sink
    recorded in order. -/
private def render (resp : Network.WebApp.Response) : IO String := do
  let out ← IO.mkRef ByteArray.empty
  let sink : ResponseSink :=
    { send := fun b => do (out.modify (· ++ b) : IO _)
      sendIO := fun b => out.modify (· ++ b)
      sendFile := fun path part =>
        Network.Sendfile.sendFileWith (fun b => out.modify (· ++ b)) path part
      rawRecv := pure ByteArray.empty
      rawSend := fun b => out.modify (· ++ b) }
  let settings : Settings := { defaultSettings with settingsAddServerHeader := false }
  let req : Network.WebApp.Request :=
    { requestMethod := .standard .GET, httpVersion := http11, rawPathInfo := "/",
      rawQueryString := "", requestHeaders := [], isSecure := false,
      remoteHost := { host := "127.0.0.1", port := 0 }, pathInfo := [], queryString := [],
      requestBody := pure ByteArray.empty, vault := Data.Vault.empty,
      requestBodyLength := .knownLength 0, requestHeaderHost := none,
      requestHeaderRange := none, requestHeaderReferer := none,
      requestHeaderUserAgent := none }
  let _ ← Green.block (sendResponseTo sink settings req resp) (← Std.CancellationToken.new)
  return String.fromUTF8! (← out.get)

private def expect (got want : String) : IO Unit :=
  unless got == want do throw (IO.userError s!"got {repr got}, want {repr want}")

#eval do
  expect (← render (.responseBuilder status200 [] "hello".toUTF8))
    "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello"

-- A file response carries its Content-Length — of the part, when there is one.
#eval do
  let (h, path) ← IO.FS.createTempFile
  h.putStr "0123456789"
  h.flush
  expect (← render (.responseFile status200 [] path.toString none))
    "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n0123456789"
  expect (← render (.responseFile status206 [] path.toString (some ⟨2, 3⟩)))
    "HTTP/1.1 206 Partial Content\r\nContent-Length: 3\r\n\r\n234"
  -- A header the application set is kept.
  expect (← render (.responseFile status200 [(hContentLength, "10")] path.toString none))
    "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n0123456789"
  IO.FS.removeFile path

-- A streamed body is chunked; empty writes produce no (terminating) chunk.
#eval do
  expect (← render (.responseStream status200 [] fun write _flush => do
      write "ab".toUTF8
      write ByteArray.empty
      write "cde".toUTF8))
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nab\r\n3\r\ncde\r\n0\r\n\r\n"

-- A raw response hands the connection to the application.
#eval do
  expect (← render (.responseRaw (fun _recv send => send "raw".toUTF8)
      (.responseBuilder status500 [] ByteArray.empty)))
    "raw"

/-! ### IO handlers — signatures (need a live connected socket) -/

example : Socket .connected → Settings → Network.WebApp.Request → Network.WebApp.Response →
    Green Network.WebApp.ResponseReceived := sendResponse
example : Socket .connected → Settings → Network.WebApp.Request → Network.WebApp.Response →
    EventDispatcher → Green Network.WebApp.ResponseReceived := sendResponseEL

end Tests.Network.WebApp.Server.Response
