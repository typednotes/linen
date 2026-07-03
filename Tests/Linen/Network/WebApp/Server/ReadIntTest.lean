/-
  Tests for `Linen.Network.WebApp.Server.ReadInt`.
-/
import Linen.Network.WebApp.Server.ReadInt

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.ReadInt

#guard readInt "42" == 42
#guard readInt "42abc" == 42
#guard readInt "" == 0
#guard readInt "abc" == 0
#guard readInt "007" == 7

#guard readIntBytes (ByteArray.mk #[52, 50]) == 42            -- "42"
#guard readIntBytes (ByteArray.mk #[52, 50, 97]) == 42         -- "42a"
#guard readIntBytes ByteArray.empty == 0
#guard readIntBytes (ByteArray.mk #[97]) == 0                  -- "a"

end Tests.Network.WebApp.Server.ReadInt
