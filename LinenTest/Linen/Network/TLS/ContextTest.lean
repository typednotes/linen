/-
  Tests for `Linen.Network.TLS.Types` and `Linen.Network.TLS.Context`.

  Exercises a full TLS handshake over a real loopback TCP connection, using the
  self-signed `CN=localhost` certificate from `TestSupport`. The server side uses
  `createContext`/`acceptSocket`; the client side uses
  `createClientContextWithCA`, trusting the self-signed certificate directly
  as its own CA, so the handshake succeeds fully offline without touching the
  system trust store.
-/
import Linen.Network.TLS.Types
import Linen.Network.TLS.Context
import Linen.Network.Socket.Blocking
import LinenTest.Linen.Network.TLS.TestSupport

open Network.Socket
open Network.Socket.Blocking
open Network.TLS
open Tests.Network.TLS.TestSupport (testCertPem testKeyPem)

namespace Tests.Network.TLS.Context

/-! ### Full TLS handshake round trip: real loopback socket, server + client -/

#eval show IO Unit from do
  let (certHandle, certPath) ← IO.FS.createTempFile
  certHandle.putStr testCertPem
  certHandle.flush
  let (keyHandle, keyPath) ← IO.FS.createTempFile
  keyHandle.putStr testKeyPem
  keyHandle.flush

  let serverCtx ← createContext certPath.toString keyPath.toString
  -- The self-signed cert is its own issuer, so trusting it directly as the
  -- CA lets the client verify the server without touching the system store.
  let clientCtx ← createClientContextWithCA certPath.toString

  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server

  let serverTask ← IO.asTask (prio := .dedicated) do
    let (conn, _peer) ← Blocking.accept server
    let session ← acceptSocket serverCtx conn.raw
    let request ← read session 4096
    write session (request ++ "!".toUTF8)
    close session
    let _ ← Network.Socket.close conn
    pure request

  let clientSock ← socket .inet .stream
  let conn ← Blocking.connect clientSock { host := "127.0.0.1", port := addr.port }
  let session ← connectSocket clientCtx conn.raw "localhost"
  write session "ping".toUTF8
  let reply ← read session 4096
  let version ← getVersion session
  let alpn ← getAlpn session
  close session
  let _ ← Network.Socket.close conn
  let _ ← Network.Socket.close server

  let request ←
    match serverTask.get with
    | .ok bytes => pure bytes
    | .error e => throw e

  IO.FS.removeFile certPath
  IO.FS.removeFile keyPath

  unless request == "ping".toUTF8 do
    throw (IO.userError s!"server received {String.fromUTF8! request}, expected 'ping'")
  unless reply == "ping!".toUTF8 do
    throw (IO.userError s!"client received {String.fromUTF8! reply}, expected 'ping!'")
  -- TLS1.2 is the configured minimum; the real negotiated version must be
  -- at least that (OpenSSL prefers the highest both sides support).
  unless version == "TLSv1.2" || version == "TLSv1.3" do
    throw (IO.userError s!"unexpected negotiated TLS version: {version}")
  unless alpn == none do
    throw (IO.userError s!"expected no ALPN protocol negotiated, got {alpn}")

/- The fallback CA bundle is either none (`""`) or one of the four known
   locations — and then a file this process can actually read, since that is
   the test the C side applies before loading it. Which one depends on the
   host, so the check is the invariant rather than a value. `SSL_CERT_FILE`
   set means no fallback at all. -/
#eval show IO Unit from do
  let path ← fallbackCaBundle
  let known := ["/etc/ssl/certs/ca-certificates.crt", "/etc/pki/tls/certs/ca-bundle.crt",
                "/etc/ssl/ca-bundle.pem", "/etc/ssl/cert.pem"]
  unless path.isEmpty || known.contains path do
    throw (IO.userError s!"fallbackCaBundle returned an unknown path: {path}")
  unless path.isEmpty || (← System.FilePath.pathExists path) do
    throw (IO.userError s!"fallbackCaBundle returned a path that does not exist: {path}")
  if let some v := (← IO.getEnv "SSL_CERT_FILE") then
    unless v.isEmpty || path.isEmpty do
      throw (IO.userError "fallbackCaBundle must defer to SSL_CERT_FILE")
  -- And a client context still builds with it loaded.
  let _ ← createClientContext
  pure ()

end Tests.Network.TLS.Context
