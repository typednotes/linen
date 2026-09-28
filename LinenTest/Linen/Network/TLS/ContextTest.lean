/-
  Tests for `Linen.Network.TLS.Types` and `Linen.Network.TLS.Context`.

  Exercises a full TLS handshake over a real loopback TCP connection, using a
  locally-generated, 100-year-validity self-signed certificate for
  `CN=localhost` (embedded below as string literals — a throwaway test
  fixture, not a production secret). The server side uses
  `createContext`/`acceptSocket`; the client side uses
  `createClientContextWithCA`, trusting the self-signed certificate directly
  as its own CA, so the handshake succeeds fully offline without touching the
  system trust store.
-/
import Linen.Network.TLS.Types
import Linen.Network.TLS.Context
import Linen.Network.Socket.Blocking

open Network.Socket
open Network.Socket.Blocking
open Network.TLS

namespace Tests.Network.TLS.Context

private def testCertPem : String :=
"-----BEGIN CERTIFICATE-----
MIIDCzCCAfOgAwIBAgIUGlCzMjj9XuyMSn2mhWYBpOH/kxEwDQYJKoZIhvcNAQEL
BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MDcwMjE0Mzc1MFoYDzIxMjYw
NjA4MTQzNzUwWjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEB
AQUAA4IBDwAwggEKAoIBAQDRM922uwm9OYdDC3y+gPznsWpkA9W/ChzLREiLvCnI
ITwgTzUVdcdhPkFB4bmG/nPg9LAzNGFyZ9Vfm3TrqV484vxuEm9QB2dkZX5RqjE4
Xha/KZ46ENdF5gAPTfDOa9sNNd8rdUomFsy5SzuEBKS9LdW8T8Ki/QNcGIhQZrAH
EqEbyfbWT67LHnqHG+CS2P0bTwRTBezlSGhrs0HETcrI8Th7EKOPdMTQNNNKCAQ9
3NnwrettMg1riUW11d5M7oT2PqA00VwMMqv4caGQuaa2EFs7MmDnGIhSUs737IEZ
V1ykbmVeAwxwAmf0Rf1AtgCnDP8REYCtZ1KqZBhFQPGDAgMBAAGjUzBRMB0GA1Ud
DgQWBBRj+buvolpodfUAV3BbgHBb48u3pDAfBgNVHSMEGDAWgBRj+buvolpodfUA
V3BbgHBb48u3pDAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQAc
HXA/yafrSLuS5c7XitxVIFxGvR9AaVzWDt5LFcMHDsn8Sy8p1/G0q2CYJp7rw24i
IgJMWBdPcjbzmJQ5OpBIQPGZgc1g3oBGX3NmzlpojMGmgJaPm7l+UdjELQ9bVfs4
mpDCB7cpG2xtKLT2C4Zz1zMaUN5goF+WqZxEbVCI6pCIDDLeAEExuyrgdrZNPYmE
6Lm4po64uUxHgr5ymryDyhIbvsL7wLoXGE5YDWqker+QRh8ypcbBQYNvh5CMKd4y
HMfCW2YubWM3TWdDEWY5roI1ClxE31LwfT51Ni8UTlnuQ18LBIOBbPyAfvgkXnMm
MbICLhJY3hrA4sWYTOqj
-----END CERTIFICATE-----
"

private def testKeyPem : String :=
"-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDRM922uwm9OYdD
C3y+gPznsWpkA9W/ChzLREiLvCnIITwgTzUVdcdhPkFB4bmG/nPg9LAzNGFyZ9Vf
m3TrqV484vxuEm9QB2dkZX5RqjE4Xha/KZ46ENdF5gAPTfDOa9sNNd8rdUomFsy5
SzuEBKS9LdW8T8Ki/QNcGIhQZrAHEqEbyfbWT67LHnqHG+CS2P0bTwRTBezlSGhr
s0HETcrI8Th7EKOPdMTQNNNKCAQ93NnwrettMg1riUW11d5M7oT2PqA00VwMMqv4
caGQuaa2EFs7MmDnGIhSUs737IEZV1ykbmVeAwxwAmf0Rf1AtgCnDP8REYCtZ1Kq
ZBhFQPGDAgMBAAECggEABft8nR0z75g42n12Tg2EbeztLm2ZPxL/QZhmL+Mr5kzk
M8ZTR2K5VpOmeRRT4lzdzz9TPi7PlJGmKktydTGNAAlVQtTxhiSk28JqCzrMzCBa
RStfzdt6bhJgMQ/ZBLlUp8f0TE4lb/mZ71IliKZuiyxhRTcAr8ZNdZfYzUSXRw4M
X9NuLGZsXEXdAFE7sUfYfHQwSPaiZTrHoC9iky4M2d/27QkbM6fPoe243Le7dSLC
i6Iy3hfaMYfx2mgShBAXzJ4JpLirJ2or6GH01Flh88cdrSwsk66Kjti53wnXIf3a
MHohVymOR8vDaVP7SBNkhgBdzQCugJ8692EKYzz2NQKBgQDyrDHaYG+H1av3miOJ
LH7ABG1cWydd7ALT06s2HLdSLUsSDzyXEg7FW/pgf6zONYaQ9Y973Enwev/08tQ8
jv2laUeyF0+w1iAUgTRgpuVc4UEIvj+RC1oa9xY4UpqFDE1pYx6uTiQ5iZrl6tAz
Q51Uk18p+8sRsYz9/xS1dwkCLQKBgQDcsRs7iFSfLhP2EUl61OkSK4nmUYs9IOqL
GuRGlpPpmWScHWOKc/GsABWfYnI+uYDhEi81wizZ96lgnSa+H1WwOzt+ln2S/pip
EOP/iVIKiWle/QtFInHppj7MhnYCd3qQoG+pxZ2iC5Eg7hKvaY82esyGnHut2JsZ
x7VEFl0AbwKBgQDlAll9oxo9jwhdfsngTuxCmyr1SFSLTwmQC6X8R/c8ht/caCKe
0ny8BUPwQyy54Utfi01f7xCb8AeSioJ9r9dwfT0atOMQl9HoZ9IdEANNtolgDeIB
KDxdTCZc+p81xdlcBh1TEw0ee1yBcyoN5tYXlYfuH9+QkATlQg6x/Waz9QKBgEkC
amDyhBQ5GS9xnp59KzHwp2lDls29QvqMBfL4Q6ynK2qeKl0WaGAfkweseOEZW3Ka
InYla9McJLOqqbOCCEYKAm+pd5eWlIhx5wuVsUd9GBftnLndYFQMxH/DB+1e+3Q3
L1m536FJNFTxjcrsIA3E6D6sLBpiK0WHFQeWYmQPAoGACfV0VGYn4u2l87YfNsZc
6V1VMOaZwjJmRtAlbubwaUhJyLwn2nu/HyuFkX0fzSQGJNkgxgXUuFPy3H2xD0Xx
VpjK0taPBjFjrTHw1IFaLa5WGHR9MhBLDSn7rpshgDhLY4NqkYU6B7wm+K5S26N8
HIMkqR8qDNJUuwo0pbfk0h8=
-----END PRIVATE KEY-----
"

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

end Tests.Network.TLS.Context
