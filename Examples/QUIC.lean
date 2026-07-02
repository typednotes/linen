/-
  Examples.QUIC — the `Linen.Network.QUIC`/`Linen.Network.HTTP3` port end-to-end.

  Wires together the pieces ported from the `quic` and `http3` Haskell
  packages into a single self-checking demo:

  * `Network.QUIC.Types`/`Config` — build a `ConnectionId`, classify a few
    `StreamId`s, and construct a `ServerConfig`/`ClientConfig`;
  * `Network.HTTP3.Server`'s `H3Response`, QPACK-encoded via
    `Network.HTTP3.QPACK.Encode`, framed via `Network.HTTP3.Frame`, and
    decoded back via `Network.HTTP3.Frame`/`Network.HTTP3.QPACK.Decode` —
    the exact wire path `sendResponse`/`handleRequestStream` run on either
    side of a real `QUICStream`;
  * `Network.QUIC.Client.connect`/`Network.QUIC.Server.{run,accept}` — the
    transport layer itself, which is honestly reported as stubbed: no TLS
    1.3 FFI (to `quiche`/`ngtcp2`) is wired up yet, so these calls cannot
    hand back a live `Connection` for the HTTP/3 layer to run over. The demo
    verifies they fail with exactly the documented "not yet implemented"
    errors rather than silently succeeding or crashing.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Network.QUIC.Config
import Linen.Network.QUIC.Client
import Linen.Network.QUIC.Server
import Linen.Network.HTTP3.Server

namespace Examples.QUIC

open Network.QUIC
open Network.HTTP3

-- ── QUIC types & configuration ──

/-- Build a `ConnectionId`, a few classified `StreamId`s, and a
`ServerConfig`/`ClientConfig` pair, printing each so the demo documents the
shapes involved. Returns `true` iff every inline check passed. -/
def demoTypesAndConfig : IO Bool := do
  IO.println "── QUIC types & configuration ──"
  let cid : ConnectionId := { bytes := ByteArray.mk #[0xde, 0xad, 0xbe, 0xef], hLen := by native_decide }
  IO.println s!"  ConnectionId: {cid} ({cid.bytes.size} bytes)"

  let clientStream : StreamId := ⟨0⟩
  let serverStream : StreamId := ⟨1⟩
  let clientUniStream : StreamId := ⟨2⟩
  IO.println s!"  StreamId 0 -> {repr clientStream.streamType}, client-initiated? {clientStream.isClientInitiated}"
  IO.println s!"  StreamId 1 -> {repr serverStream.streamType}, client-initiated? {serverStream.isClientInitiated}"
  IO.println s!"  StreamId 2 -> {repr clientUniStream.streamType}, bidi? {clientUniStream.isBidi}"

  let serverCfg : ServerConfig := { tlsConfig := { alpn := ["h3"] }, port := 443 }
  let clientCfg : ClientConfig := { serverName := "example.com" }
  IO.println s!"  ServerConfig: host={serverCfg.host} port={serverCfg.port} alpn={serverCfg.tlsConfig.alpn}"
  IO.println s!"  ClientConfig: serverName={clientCfg.serverName} port={clientCfg.port}"

  let checks :=
    [ clientStream.streamType == .clientBidi
    , serverStream.streamType == .serverBidi
    , clientUniStream.streamType == .clientUni
    , cid.bytes.size == 4
    , serverCfg.host == "0.0.0.0"
    , clientCfg.port == 443 ]
  pure (checks.all id)

-- ── HTTP/3 wire round trip (no live QUIC connection needed) ──

/-- Encode `resp` exactly as `Network.HTTP3.Server.sendResponse` would (QPACK
headers, then an HTTP/3 HEADERS frame), then decode that frame back and
verify it reproduces the original status and headers. This is the same
codec path a real `QUICStream` carries between client and server — the only
thing missing here is the stream itself. -/
def demoHttp3RoundTrip : IO Bool := do
  IO.println "── HTTP/3 wire round trip ──"
  let resp : H3Response :=
    { status := 200, headers := [("content-type", "text/plain")], body := "hello".toUTF8 }
  let responseHeaders : List QPACK.HeaderField := [(":status", toString resp.status)] ++ resp.headers
  let headerBlock := QPACK.encodeHeaders responseHeaders
  let frame : Frame := { frameType := .headers, payload := headerBlock }
  let wire := frame.encode
  IO.println s!"  encoded {wire.size} bytes for status={resp.status} headers={resp.headers}"

  match Frame.decode wire with
  | none =>
    IO.eprintln "  FAILED: could not decode the frame we just encoded"
    pure false
  | some (decodedFrame, consumed) =>
    match QPACK.decodeHeaders decodedFrame.payload with
    | none =>
      IO.eprintln "  FAILED: could not decode the QPACK header block we just encoded"
      pure false
    | some decodedHeaders =>
      IO.println s!"  decoded {decodedHeaders} ({consumed} bytes consumed)"
      pure <|
        decodedFrame.frameType == .headers &&
        consumed == wire.size &&
        decodedHeaders == responseHeaders

-- ── The stubbed transport layer, exercised honestly ──

/-- Run `action`, expecting it to throw exactly `expected`. Prints the
outcome and returns whether it matched. -/
def expectStubError (label : String) (expected : String) (action : IO Unit) : IO Bool := do
  try
    action
    IO.eprintln s!"  FAILED: {label} unexpectedly succeeded without a TLS 1.3 FFI backend"
    pure false
  catch e =>
    let matched := toString e == expected
    let mark := if matched then "OK" else "MISMATCH"
    IO.println s!"  {label}: {toString e}  [{mark}]"
    pure matched

/-- `Client.connect`/`Server.{run,accept}` are the actual QUIC transport —
this is what stands between the wire round trip above and a real
`QUICStream`. All three are stubbed pending TLS 1.3 FFI; check that they
fail loudly and consistently rather than pretending to work. -/
def demoStubbedTransport : IO Bool := do
  IO.println "── QUIC transport (stubbed pending TLS 1.3 FFI) ──"
  let clientCfg : ClientConfig := { serverName := "example.com" }
  let serverCfg : ServerConfig := { tlsConfig := {} }
  let r1 ← expectStubError "Client.connect"
    "QUIC.Client.connect: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    (discard (Client.connect clientCfg))
  let r2 ← expectStubError "Server.accept"
    "QUIC.Server.accept: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    (discard (Server.accept serverCfg))
  let r3 ← expectStubError "Server.run"
    "QUIC.Server.run: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    (Server.run serverCfg (fun _ => pure ()))
  pure (r1 && r2 && r3)

def run (_args : List String) : IO Unit := do
  let okTypes ← demoTypesAndConfig
  IO.println ""
  let okHttp3 ← demoHttp3RoundTrip
  IO.println ""
  let okTransport ← demoStubbedTransport
  IO.println ""
  if okTypes && okHttp3 && okTransport then
    IO.println "quic demo done · all checks passed"
  else
    throw (IO.userError "quic demo done · some checks failed")

end Examples.QUIC
