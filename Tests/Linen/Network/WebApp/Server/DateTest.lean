/-
  Tests for `Linen.Network.WebApp.Server.Date`.
-/
import Linen.Network.WebApp.Server.Date

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Date

#eval show IO Unit from do
  withDateCache fun getDate => do
    let d ← getDate
    assert! d.startsWith "Date: epoch "

end Tests.Network.WebApp.Server.Date
