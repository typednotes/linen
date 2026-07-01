/-
  Tests for `Linen.PostgREST.Network`.
-/
import Linen.PostgREST.Network

open PostgREST.Network

namespace Tests.PostgREST.Network

#guard resolveHost "!4" == "0.0.0.0"
#guard resolveHost "!6" == "::"
#guard resolveHost "*" == "0.0.0.0"
#guard resolveHost "127.0.0.1" == "127.0.0.1"
#guard resolveHost "example.com" == "example.com"

end Tests.PostgREST.Network
