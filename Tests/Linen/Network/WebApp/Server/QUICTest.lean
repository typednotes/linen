/-
  Tests for `Linen.Network.WebApp.Server.QUIC`.

  `runH3`/`runQUIC` need a live UDP socket and TLS certs, so they are
  pinned at the type level. The pure config-building helpers are checked
  with `#guard`.
-/
import Linen.Network.WebApp.Server.QUIC

open Network.WebApp.Server.QUIC
open Network.QUIC
open Network.HTTP3

namespace Tests.Network.WebApp.Server.QUIC

#guard (defaultSettings "cert.pem" "key.pem").certFile == "cert.pem"
#guard (defaultSettings "cert.pem" "key.pem").keyFile == "key.pem"
#guard (defaultSettings "cert.pem" "key.pem").port == 443
#guard (defaultSettings "cert.pem" "key.pem").host == "0.0.0.0"

#guard (toQUICConfig (defaultSettings "cert.pem" "key.pem")).tlsConfig.certFile == some "cert.pem"
#guard (toQUICConfig (defaultSettings "cert.pem" "key.pem")).tlsConfig.alpn == ["h3"]
#guard (toQUICConfig { defaultSettings "c" "k" with port := 8443 }).port == 8443

#guard (toH3Settings (defaultSettings "c" "k")).qpackMaxTableCapacity == 4096
#guard (toH3Settings { defaultSettings "c" "k" with qpackBlockedStreams := 10 }).qpackBlockedStreams == 10

private def sampleReq : H3Request where
  method := "GET"
  path := "/hi"
  scheme := "https"
  authority := "example.com"
  headers := [("x-custom", "1")]
  readBody := pure ByteArray.empty

#guard h3RequestToHeaders sampleReq ==
  [(":method", "GET"), (":path", "/hi"), (":scheme", "https"), (":authority", "example.com"),
   ("x-custom", "1")]

/-! ### IO entry points — signatures (need a live QUIC/UDP socket) -/

example : Settings → Connection → H3Handler → IO Unit := handleConnection
example : Settings → H3Handler → IO Unit := runH3
example : ServerConfig → H3Handler → IO Unit := runQUIC

end Tests.Network.WebApp.Server.QUIC
