/-
  Tests for `Linen.Network.QUIC.Config`.

  `ServerConfig`/`ClientConfig` are pure data, so their defaults are checked
  with `#guard`.
-/
import Linen.Network.QUIC.Config

open Network.QUIC

namespace Tests.Network.QUIC.Config

/-! ### ServerConfig -/

#guard (({ tlsConfig := {} } : ServerConfig)).host == "0.0.0.0"
#guard (({ tlsConfig := {} } : ServerConfig)).port == 443
#guard (({ tlsConfig := {} } : ServerConfig)).transportParams == TransportParams.default
#guard (({ tlsConfig := {}, port := 8443 } : ServerConfig)).port == 8443
#guard (({ tlsConfig := { alpn := ["h3"] } } : ServerConfig)).tlsConfig.alpn == ["h3"]

/-! ### ClientConfig -/

#guard (({ serverName := "example.com" } : ClientConfig)).serverName == "example.com"
#guard (({ serverName := "example.com" } : ClientConfig)).port == 443
#guard (({ serverName := "example.com" } : ClientConfig)).tlsConfig == ({} : TLSConfig)
#guard (({ serverName := "example.com" } : ClientConfig)).transportParams == TransportParams.default
#guard (({ serverName := "example.com", port := 4433 } : ClientConfig)).port == 4433

end Tests.Network.QUIC.Config
