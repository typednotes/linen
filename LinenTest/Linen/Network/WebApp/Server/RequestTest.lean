/-
  Tests for `Linen.Network.WebApp.Server.Request`.

  Body framing is where an HTTP server is most easily confused into reading
  one request as two (request smuggling), so it is tested three ways: the
  pure framing decision (`requestFraming`), the chunked decoder over
  in-memory readers (`chunkedBodyReader`), and end to end against a live
  server, on a kept-alive connection whose second request must be the one
  the client sent — not bytes from the first request's body.
-/
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.WithApplication
import Linen.Network.Socket.Blocking

open Network.WebApp.Server
open Network.HTTP.Types

namespace Tests.Network.WebApp.Server.Request

/-! ### `parseHttpVersion` -/

#guard parseHttpVersion "HTTP/1.1" == some http11
#guard parseHttpVersion "HTTP/1.0" == some http10
#guard parseHttpVersion "HTTP/0.9" == some http09
#guard parseHttpVersion "HTTP/2.0" == some http20
#guard parseHttpVersion "HTTP/3.7" == some ⟨3, 7⟩
#guard parseHttpVersion "bogus" == none

example : parseHttpVersion "HTTP/1.1" = some http11 := parseHttpVersion_http11
example : parseRequestLine "" = none := parseRequestLine_empty

/-! ### `parseRequestLine` -/

#guard parseRequestLine "GET /path?q=1 HTTP/1.1" ==
  some (.standard .GET, "/path", "?q=1", http11)
#guard parseRequestLine "POST / HTTP/1.0" == some (.standard .POST, "/", "", http10)
#guard parseRequestLine "" == none
#guard parseRequestLine "GET /only-two-fields" == none

/-! ### `parseHeaderLine` / `parseHeaders` -/

#guard parseHeaderLine "Content-Type: text/html" ==
  some (Data.CI.mk' "Content-Type", "text/html")
#guard parseHeaderLine "no-colon-here" == none

#guard parseHeaders ["Host: example.com", "X-Test: 1"] ==
  [(Data.CI.mk' "Host", "example.com"), (Data.CI.mk' "X-Test", "1")]
#guard parseHeaders ["not-a-header"] == []

/-! ### `requestFraming` — RFC 9112 §6 -/

private def hs (pairs : List (String × String)) : RequestHeaders :=
  pairs.map fun (n, v) => (Data.CI.mk' n, v)

private def framing? (pairs : List (String × String)) (version := http11) :
    Option BodyFraming :=
  (requestFraming version (hs pairs)).toOption

-- No framing header: no body (it used to be reported as a chunked body).
#guard framing? [] == some (.length 0)
#guard framing? [("Host", "x")] == some (.length 0)
#guard framing? [("Content-Length", "42")] == some (.length 42)
-- Repeated or list-valued Content-Length is fine when every value agrees…
#guard framing? [("Content-Length", "7"), ("content-length", "7")] == some (.length 7)
#guard framing? [("Content-Length", "7, 7")] == some (.length 7)
-- …and rejected when they disagree, or are not plain digits.
#guard framing? [("Content-Length", "7"), ("Content-Length", "8")] == none
#guard framing? [("Content-Length", "-1")] == none
#guard framing? [("Content-Length", "1_000")] == none
#guard framing? [("Content-Length", "0x10")] == none
-- Chunked, with the header name and value matched case-insensitively.
#guard framing? [("Transfer-Encoding", "chunked")] == some .chunked
#guard framing? [("transfer-encoding", "Chunked")] == some .chunked
#guard framing? [("Transfer-Encoding", "gzip, chunked")] == some .chunked
#guard framing? [("Transfer-Encoding", "gzip"), ("Transfer-Encoding", "chunked")] == some .chunked
-- Transfer-Encoding with Content-Length is the classic smuggling vector.
#guard framing? [("Transfer-Encoding", "chunked"), ("Content-Length", "5")] == none
#guard framing? [("Content-Length", "5"), ("Transfer-Encoding", "chunked")] == none
-- `chunked` must be the final coding, applied once; nothing else is decodable.
#guard framing? [("Transfer-Encoding", "chunked, gzip")] == none
#guard framing? [("Transfer-Encoding", "chunked, chunked")] == none
#guard framing? [("Transfer-Encoding", "identity")] == none
-- HTTP/1.0 has no transfer codings.
#guard framing? [("Transfer-Encoding", "chunked")] (version := http10) == none
#guard framing? [("Content-Length", "3")] (version := http10) == some (.length 3)

#guard BodyFraming.chunked.toBodyLength == .chunkedBody
#guard (BodyFraming.length 3).toBodyLength == .knownLength 3

/-! ### `parseContentLength` / `parseChunkSize` -/

#guard parseContentLength "0" == some 0
#guard parseContentLength "123" == some 123
#guard parseContentLength "" == none
#guard parseContentLength "+1" == none
#guard parseContentLength "1 2" == none

#guard parseChunkSize "0" == some 0
#guard parseChunkSize "1a" == some 26
#guard parseChunkSize "FF" == some 255
#guard parseChunkSize "1a;name=value" == some 26
#guard parseChunkSize "1a ; name" == some 26
#guard parseChunkSize "" == none
#guard parseChunkSize ";ext" == none
#guard parseChunkSize "xyz" == none
#guard parseChunkSize "-1" == none
#guard parseChunkSize "ffffffffffffffff" == some 18446744073709551615
-- Seventeen digits is more than any real body; refused before conversion.
#guard parseChunkSize "10000000000000000" == none

/-! ### `chunkedBodyReader` over in-memory readers -/

/-- Readers over a fixed input with the socket buffer's semantics:
    `readLine` returns a line without its CRLF (the rest of the input when
    there is no CRLF, `""` at the end); `readN n` returns up to `n` bytes.
    Also returns a reader for the input not yet consumed. -/
private def memReaders (input : String) :
    IO (IO String × (Nat → IO ByteArray) × IO String) := do
  let bytes := input.toUTF8
  let pos ← IO.mkRef 0
  let readLine : IO String := do
    let p ← pos.get
    let mut stop := bytes.size
    let mut next := bytes.size
    for i in [p:bytes.size] do
      if bytes.get! i == 13 && i + 1 < bytes.size && bytes.get! (i + 1) == 10 then
        stop := i
        next := i + 2
        break
    pos.set next
    pure (String.fromUTF8! (bytes.extract p stop))
  let readN (n : Nat) : IO ByteArray := do
    let p ← pos.get
    let q := min bytes.size (p + n)
    pos.set q
    pure (bytes.extract p q)
  let rest : IO String := do pure (String.fromUTF8! (bytes.extract (← pos.get) bytes.size))
  pure (readLine, readN, rest)

/-- Decode a whole chunked body from `input`: the body and the unconsumed
    input, or the decoder's error. Bounded, so a decoder that never ends its
    stream fails the test instead of hanging the build. -/
private def decode (input : String) : IO (Except String (String × String)) := do
  let (readLine, readN, rest) ← memReaders input
  let read ← chunkedBodyReader readLine readN
  let mut body := ByteArray.empty
  try
    for _ in [0:1000] do
      let piece ← read
      if piece.isEmpty then
        -- The end is sticky: reading again stays at the end.
        unless (← read).isEmpty do return .error "data after the end"
        return .ok (String.fromUTF8! body, ← rest)
      body := body ++ piece
    return .error "no end of stream"
  catch e => return .error (toString e)

private def decodes (input : String) (body leftover : String) : IO Unit := do
  match ← decode input with
  | .ok (b, l) =>
    unless b == body && l == leftover do
      throw (IO.userError s!"decoded {repr b}, left {repr l}")
  | .error e => throw (IO.userError s!"unexpected error: {e}")

private def rejects (input : String) : IO Unit := do
  if let .ok r ← decode input then
    throw (IO.userError s!"accepted malformed chunked body: {repr r}")

-- Two chunks and the last chunk; the next request is left untouched.
#eval decodes "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\nGET /next HTTP/1.1\r\n"
  "hello world" "GET /next HTTP/1.1\r\n"
-- Uppercase hex and chunk extensions.
#eval decodes "A;name=value\r\n0123456789\r\n0;last\r\n\r\n" "0123456789" ""
-- Trailer fields are consumed, not left for the next request.
#eval decodes "3\r\nabc\r\n0\r\nX-Checksum: 1\r\nX-Other: 2\r\n\r\nNEXT" "abc" "NEXT"
-- An empty body is just the last chunk.
#eval decodes "0\r\n\r\n" "" ""
-- A chunk larger than one read (4096 bytes) arrives in several pieces.
#eval decodes ("2000\r\n" ++ String.ofList (List.replicate 8192 'a') ++ "\r\n0\r\n\r\n")
  (String.ofList (List.replicate 8192 'a')) ""
-- Chunk data may contain CRLF and text that looks like a request.
#eval decodes "18\r\nGET /smuggled HTTP/1.1\r\n\r\n0\r\n\r\n" "GET /smuggled HTTP/1.1\r\n" ""

-- Malformed input is an error, never a short body that looks complete.
#eval rejects "zz\r\nhello\r\n0\r\n\r\n"
#eval rejects "\r\n"
#eval rejects ""
#eval rejects "5\r\nhel"
#eval rejects "5\r\nhello"
#eval rejects "3\r\nabcX\r\n0\r\n\r\n"
#eval rejects ("0\r\n" ++ String.join (List.replicate (maxHeaders + 1) "X: 1\r\n") ++ "\r\n")

/-! ### End to end: a live server on a kept-alive connection -/

open _root_.Network.WebApp in
/-- Responds with the request's path and its whole body, read to the end. -/
private def echoApp : Application := fun req respond =>
  AppM.respondIO respond do
    let mut body := ByteArray.empty
    for _ in [0:10000] do
      let piece ← req.requestBody
      if piece.isEmpty then break
      body := body ++ piece
    pure (responseLBS status200 []
      s!"path={req.rawPathInfo} body={String.fromUTF8! body};")

open _root_.Network.Socket in
/-- Receive until `done` holds of everything received so far, or the peer
    closes; bounded so a test cannot hang the build. -/
private def recvUntil (conn : Socket .connected) (done : String → Bool) : IO String := do
  let mut received := ByteArray.empty
  for _ in [0:1000] do
    if done (String.fromUTF8! received) then break
    let piece ← try Blocking.recv conn catch _ => pure ByteArray.empty
    if piece.isEmpty then break
    received := received ++ piece
  pure (String.fromUTF8! received)

open _root_.Network.Socket in
private def connectTo (port : UInt16) : IO (Socket .connected) := do
  Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port := port }

private def contains (s part : String) : Bool := (s.splitOn part).length > 1

-- A chunked body reaches the application, and on the same connection the
-- next response answers the request the client sent next. Through 1.8.0 the
-- body read as empty and its bytes were parsed as the following request.
#eval show IO Unit from do
  withApplication (pure echoApp) fun port => do
    let conn ← connectTo port
    Network.Socket.Blocking.sendAll conn
      ("POST /upload HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n" ++
       "5\r\nhello\r\n1B\r\n GET /smuggled HTTP/1.1\r\n\r\n\r\n0\r\n\r\n").toUTF8
    let first ← recvUntil conn (contains · ";")
    unless contains first "path=/upload body=hello GET /smuggled HTTP/1.1\r\n\r\n;" do
      throw (IO.userError s!"first response: {repr first}")
    Network.Socket.Blocking.sendAll conn
      "GET /second HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n".toUTF8
    let second ← recvUntil conn (fun _ => false)
    let _ ← Network.Socket.close conn
    unless contains second "path=/second body=;" && !contains second "/smuggled" do
      throw (IO.userError s!"second response: {repr second}")

-- A Content-Length body the application does not read is drained, so the
-- next request on the connection is parsed from the right place.
#eval show IO Unit from do
  let ignoreBody : _root_.Network.WebApp.Application := fun req respond =>
    _root_.Network.WebApp.AppM.respond respond
      (_root_.Network.WebApp.responseLBS status200 [] s!"path={req.rawPathInfo};")
  withApplication (pure ignoreBody) fun port => do
    let conn ← connectTo port
    Network.Socket.Blocking.sendAll conn
      "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 24\r\n\r\nGET /smuggled HTTP/1.1\r\n".toUTF8
    let first ← recvUntil conn (contains · ";")
    unless contains first "path=/a;" do throw (IO.userError s!"first: {repr first}")
    Network.Socket.Blocking.sendAll conn
      "GET /b HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n".toUTF8
    let second ← recvUntil conn (fun _ => false)
    let _ ← Network.Socket.close conn
    unless contains second "path=/b;" && !contains second "/smuggled" do
      throw (IO.userError s!"second: {repr second}")

-- Ambiguous framing gets no response at all: the connection is closed.
#eval show IO Unit from do
  withApplication (pure echoApp) fun port => do
    let conn ← connectTo port
    Network.Socket.Blocking.sendAll conn
      ("POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\nTransfer-Encoding: chunked\r\n\r\n" ++
       "0\r\n\r\n").toUTF8
    let reply ← recvUntil conn (fun _ => false)
    let _ ← Network.Socket.close conn
    unless reply.isEmpty do throw (IO.userError s!"answered: {repr reply}")

/-! ### Over the C `RecvBuffer` (`ByteSource.ofRecvBuffer`) -/

-- `parseRequest` over a live socket's `RecvBuffer`: two pipelined requests,
-- the first with a body, are parsed in turn.
#eval show IO Unit from do
  let server ← _root_.Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← _root_.Network.Socket.getSockName server
  let client ← _root_.Network.Socket.Blocking.connect
    (← _root_.Network.Socket.socket .inet .stream) { host := "127.0.0.1", port := addr.port }
  let (conn, peer) ← _root_.Network.Socket.Blocking.accept server
  _root_.Network.Socket.Blocking.sendAll client
    ("POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 3\r\n\r\nabc" ++
     "GET /b HTTP/1.1\r\nHost: x\r\n\r\n").toUTF8
  let buf ← Network.Socket.FFI.recvBufCreate conn.raw
  let some first ← parseRequest buf peer | throw (IO.userError "first request")
  let body ← first.requestBody
  let rest ← first.requestBody
  let some second ← parseRequest buf peer | throw (IO.userError "second request")
  for s in [client, conn] do let _ ← _root_.Network.Socket.close s
  let _ ← _root_.Network.Socket.close server
  unless first.rawPathInfo == "/a" && body == "abc".toUTF8 && rest.isEmpty &&
      second.rawPathInfo == "/b" && second.requestBodyLength == .knownLength 0 do
    throw (IO.userError s!"parsed {first.rawPathInfo} {String.fromUTF8! body}, then {second.rawPathInfo}")

-- A `Content-Length` body cut short by the peer closing is an error, not a
-- short body that looks complete.
#eval show IO Unit from do
  let (readLine, readN, _) ← memReaders "POST / HTTP/1.1\r\nContent-Length: 10\r\n\r\n0123"
  let some req ← parseRequestFrom { readLine, readN } { host := "127.0.0.1", port := 0 }
    | throw (IO.userError "no request")
  let first ← req.requestBody
  let truncated ← try let _ ← req.requestBody; pure false catch _ => pure true
  unless first == "0123".toUTF8 && truncated do
    throw (IO.userError "a truncated body read as complete")

/-! ### IO entry points — signatures -/

example : Network.Socket.FFI.RecvBuffer → IO (Option (String × HeaderLines)) := recvHeaders
example : Network.Socket.FFI.RecvBuffer → Network.Socket.SockAddr →
    IO (Option Network.WebApp.Request) := parseRequest
example (h : HeaderLines) : h.val.length ≤ maxHeaders := h.property

end Tests.Network.WebApp.Server.Request
