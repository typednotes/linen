/-
  Tests for `Linen.Network.WebApp.Server.TLS`.

  `runTLS` needs a live socket and real certificate/key files, so it is
  pinned at the type level. `tlsConnection`/`tlsAcceptLoop` are `private`
  implementation details, exercised only indirectly through `runTLS`.
-/
import Linen.Network.WebApp.Server.TLS

open Network.WebApp.Server.TLS
open Network.WebApp (Application)

namespace Tests.Network.WebApp.Server.TLS

#guard OnInsecure.allowInsecure == OnInsecure.allowInsecure
#guard OnInsecure.denyInsecure "nope" == OnInsecure.denyInsecure "nope"
#guard OnInsecure.allowInsecure != OnInsecure.denyInsecure "nope"

private def sampleTLSSettings : TLSSettings where
  certSettings := .certFile "cert.pem" "key.pem"

#guard sampleTLSSettings.onInsecure == OnInsecure.denyInsecure "This server requires HTTPS"
#guard sampleTLSSettings.alpn == true

example : TLSSettings → Network.WebApp.Server.Settings → Application → IO Unit := runTLS

end Tests.Network.WebApp.Server.TLS
