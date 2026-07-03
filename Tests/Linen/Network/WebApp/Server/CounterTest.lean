/-
  Tests for `Linen.Network.WebApp.Server.Counter`.
-/
import Linen.Network.WebApp.Server.Counter

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Counter

#eval show IO Unit from do
  let c ← Counter.new
  assert! (← c.getCount) == 0
  c.increase
  c.increase
  assert! (← c.getCount) == 2
  c.decrease
  assert! (← c.getCount) == 1
  c.decrease
  c.decrease  -- decreasing past zero clamps at zero
  assert! (← c.getCount) == 0
  c.waitForZero  -- already zero, returns immediately

end Tests.Network.WebApp.Server.Counter
