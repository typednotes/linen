/-
  Tests for `Linen.Network.WebApp.Server.Internal`.

  A pure re-export aggregator — verified by using one re-exported symbol
  from each of the modules it pulls in.
-/
import Linen.Network.WebApp.Server.Internal

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Internal

#guard defaultSettings.settingsPort == 3000
#guard Transport.tcp.isSecure == false
#guard packHex 255 == "ff"
#guard readInt "42" == 42
#guard (HeaderMap.empty.insert' "a" "b").find? "a" == some "b"

end Tests.Network.WebApp.Server.Internal
