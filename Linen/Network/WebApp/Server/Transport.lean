/-
  Linen.Network.WebApp.Server.Transport — buffered transports shared by
  HTTP/1.1 and HTTP/2, over plain or TLS sockets in either server mode.
-/
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.Response
import Linen.Network.HTTP2.Types

namespace Network.WebApp.Server

open Control.Concurrent.Green (Green)

-- ── The connection's byte transport ────────────────────────────────

/-- A connection as the HTTP engines see it. Reads and writes retain the
    transport's timeout and suspension semantics. -/
structure HttpTransport where
  /-- Next bytes: `some` empty at EOF, `none` on idle timeout. -/
  nextChunk : Green (Option ByteArray)
  /-- Buffered heads and body reads (the application's `IO` interface). -/
  reader : BufferedSource
  /-- Response writes and raw-connection handoffs. -/
  sink : ResponseSink
  /-- Whether the connection uses TLS (`Request.isSecure`). -/
  isSecure : Bool := false

/-- Buffer a complete HTTP/1.1 head. `false` on timeout; EOF leaves whatever
    arrived for the parser to reject. -/
def HttpTransport.bufferHead (t : HttpTransport) : Green Bool := do
  let mut ended := false
  while !ended && !headComplete (← (t.reader.unread : IO _)) do
    match ← t.nextChunk with
    | none => return false
    | some chunk => if chunk.isEmpty then ended := true else (t.reader.feed chunk : IO _)
  return true

-- ── Cleartext protocol detection ───────────────────────────────────

/-- Whether all bytes so far agree with the HTTP/2 client preface. A prefix
    is inconclusive until all 24 bytes arrive: its first blank line is only
    18 bytes in, so HTTP/1.1 head detection alone is insufficient. -/
def http2PrefacePrefix (bytes : ByteArray) : Bool :=
  let n := min bytes.size Network.HTTP2.connectionPrefaceLength
  bytes.extract 0 n == Network.HTTP2.connectionPreface.extract 0 n

/-- Detect prior-knowledge HTTP/2 without consuming any bytes. Fragmented
    prefaces suspend through `nextChunk`; a mismatch immediately leaves the
    buffered input for HTTP/1.1 (`some false`), a complete preface selects
    HTTP/2 (`some true`), and EOF/timeout closes the connection (`none`). -/
def HttpTransport.detectHttp2 (t : HttpTransport) : Green (Option Bool) := do
  repeat
    let bytes ← (t.reader.unread : IO _)
    if !http2PrefacePrefix bytes then return some false
    if bytes.size ≥ Network.HTTP2.connectionPrefaceLength then return some true
    match ← t.nextChunk with
    | none => return none
    | some chunk =>
      if chunk.isEmpty then return none
      (t.reader.feed chunk : IO _)
  return none

end Network.WebApp.Server
