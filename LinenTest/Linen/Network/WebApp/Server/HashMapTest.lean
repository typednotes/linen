/-
  Tests for `Linen.Network.WebApp.Server.HashMap`.
-/
import Linen.Network.WebApp.Server.HashMap

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.HashMap

#guard HeaderMap.empty.find? "content-type" == none

#guard (HeaderMap.empty
  |>.insert' "content-type" "text/plain"
  |>.insert' "content-length" "0"
  |>.find? "content-type") == some "text/plain"

#guard (HeaderMap.empty
  |>.insert' "a" "1"
  |>.find? "missing") == none

end Tests.Network.WebApp.Server.HashMap
