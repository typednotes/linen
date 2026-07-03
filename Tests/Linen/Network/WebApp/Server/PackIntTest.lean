/-
  Tests for `Linen.Network.WebApp.Server.PackInt`.
-/
import Linen.Network.WebApp.Server.PackInt

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.PackInt

#guard packInt 42 == "42"
#guard packInt 0 == "0"

#guard packHex 255 == "ff"
#guard packHex 0 == "0"
#guard packHex 16 == "10"

end Tests.Network.WebApp.Server.PackInt
