/-
  Tests for `Linen.Network.HTTP.Client.Connection`.

  `defaultPort` is pure, so it's checked with `#guard`; the connection
  establishers (`connectPlain`/`connectTLS`/`connect`) perform real IO
  (DNS + TCP/TLS handshakes), so they're pinned at the type level instead.
-/
import Linen.Network.HTTP.Client.Connection

open Network.HTTP.Client

namespace Tests.Network.HTTP.Client.Connection

/-! ### defaultPort -/

#guard defaultPort true == 443
#guard defaultPort false == 80

/-! ### Connection establishers — signatures -/

example : String → UInt16 → IO Connection := connectPlain
example : String → UInt16 → IO Connection := connectTLS
example : String → UInt16 → Bool → IO Connection := connect

end Tests.Network.HTTP.Client.Connection
