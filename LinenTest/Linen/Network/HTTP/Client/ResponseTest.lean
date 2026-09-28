/-
  Tests for `Linen.Network.HTTP.Client.Response`.

  Parsing runs in `IO` over a `Connection`, so behaviour is checked with `#eval`
  (a thrown error fails the build), driving a mock in-memory connection backed
  by a `ByteArray`.
-/
import Linen.Network.HTTP.Client.Response

open Network.HTTP.Client Network.HTTP.Types

namespace Tests.Network.HTTP.Client.Response

/-- A read-only mock `Connection` that serves `data` in chunks and records nothing. -/
private def readConn (data : ByteArray) : IO Connection := do
  let pos ← IO.mkRef 0
  return {
    connRead := fun n => do
      let p ← pos.get
      let take := min n (data.size - p)
      pos.set (p + take)
      return data.extract p (p + take)
    connWrite := fun _ => pure ()
    connClose := pure ()
    connIsSecure := false }

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

/-! ### parseStatusLine -/

#eval show IO Unit from do
  let (v, s) ← parseStatusLine "HTTP/1.1 404 Not Found"
  check (s.statusCode == 404) s!"status code: {s.statusCode}"
  check (s.statusMessage == "Not Found") s!"reason: {s.statusMessage}"
  check (v.major == 1 && v.minor == 1) "version"

/-! ### receiveResponse — Content-Length body -/

#eval show IO Unit from do
  let conn ← readConn "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Type: text/plain\r\n\r\nhello".toUTF8
  let resp ← receiveResponse conn
  check (resp.statusCode.statusCode == 200) "status"
  check (resp.statusCode.statusMessage == "OK") "reason"
  check (resp.findHeader hContentType == some "text/plain") "content-type"
  check (resp.contentLength == some 5) "content-length"
  check (String.fromUTF8! resp.body == "hello") s!"body: {String.fromUTF8! resp.body}"
  check resp.isSuccess "isSuccess"

/-! ### receiveResponse — chunked transfer encoding -/

#eval show IO Unit from do
  let conn ← readConn "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n".toUTF8
  let resp ← receiveResponse conn
  check (String.fromUTF8! resp.body == "hello world") s!"chunked body: {String.fromUTF8! resp.body}"

/-! ### receiveResponse — read-until-close (no Content-Length / chunked) -/

#eval show IO Unit from do
  let conn ← readConn "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nstreamed body".toUTF8
  let resp ← receiveResponse conn
  check (String.fromUTF8! resp.body == "streamed body") "until-close body"

/-! ### performRequest — sends the request, parses the response -/

#eval show IO Unit from do
  let written ← IO.mkRef ByteArray.empty
  let pos ← IO.mkRef 0
  let respData := "HTTP/1.1 201 Created\r\nContent-Length: 2\r\n\r\nhi".toUTF8
  let conn : Connection := {
    connRead := fun n => do
      let p ← pos.get; let take := min n (respData.size - p)
      pos.set (p + take); return respData.extract p (p + take)
    connWrite := fun bs => written.modify (· ++ bs)
    connClose := pure ()
    connIsSecure := false }
  let resp ← performRequest conn { method := .standard .GET, host := "example.com", port := 80 }
  check (resp.statusCode.statusCode == 201) "perform status"
  check (String.fromUTF8! resp.body == "hi") "perform body"
  let sent := String.fromUTF8! (← written.get)
  check (sent.startsWith "GET / HTTP/1.1\r\n") s!"sent request: {sent}"

end Tests.Network.HTTP.Client.Response
