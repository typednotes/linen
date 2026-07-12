/-
  Tests for `Linen.Database.SQLite.Simple.Time`, the thin re-export facade
  over `Linen.Database.SQLite.Simple.Time.Implementation`. Just checks that
  the facade import alone puts the implementation's names in scope.
-/
import Linen.Database.SQLite.Simple.Time

open Data.Time
open Database.SQLite.Simple.Time

namespace Tests.Database.SQLite.Simple.Time

#guard match parseDay "2000-01-01" with | .ok d => d == Day.fromGregorian 2000 1 1 | .error _ => false
#guard dayToString (Day.fromGregorian 2000 1 1) == "2000-01-01"

end Tests.Database.SQLite.Simple.Time
