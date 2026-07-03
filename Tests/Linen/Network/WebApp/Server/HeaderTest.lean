/-
  Tests for `Linen.Network.WebApp.Server.Header`.
-/
import Linen.Network.WebApp.Server.Header

open Network.WebApp.Server
open Network.HTTP.Types

namespace Tests.Network.WebApp.Server.Header

#guard requestMaxIndex == 13

#guard requestKeyIndex (Data.CI.mk' "Content-Length") == some 0
#guard requestKeyIndex (Data.CI.mk' "content-length") == some 0
#guard requestKeyIndex (Data.CI.mk' "Host") == some 5
#guard requestKeyIndex (Data.CI.mk' "If-None-Match") == some 12
#guard requestKeyIndex (Data.CI.mk' "X-Custom-Header") == none

def sampleHeaders : RequestHeaders :=
  [(Data.CI.mk' "Host", "example.com"), (Data.CI.mk' "Content-Length", "42")]

#guard (indexRequestHeader sampleHeaders).lookup 5 == some "example.com"
#guard (indexRequestHeader sampleHeaders).lookup 0 == some "42"
#guard (indexRequestHeader sampleHeaders).lookup 9 == none
#guard (indexRequestHeader sampleHeaders).lookup 999 == none

end Tests.Network.WebApp.Server.Header
