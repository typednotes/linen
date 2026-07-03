/-
  Tests for `Linen.Network.WebApp.Server.TLS.Internal`.

  A pure re-export aggregator carrying no content of its own — verified by
  using a symbol from the `Network.WebApp.Server.TLS` namespace it exists
  to re-export.
-/
import Linen.Network.WebApp.Server.TLS.Internal

open Network.WebApp.Server.TLS

namespace Tests.Network.WebApp.Server.TLS.Internal

#guard OnInsecure.allowInsecure != OnInsecure.denyInsecure "nope"

end Tests.Network.WebApp.Server.TLS.Internal
