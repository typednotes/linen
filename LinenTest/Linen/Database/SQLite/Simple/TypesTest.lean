/-
  Tests for `Linen.Database.SQLite.Simple.Types`.
-/
import Linen.Database.SQLite.Simple.Types

open Database.SQLite.Simple.Types

namespace Tests.Database.SQLite.Simple.Types

/-! ### `Null` -/

-- No two `Null`s ever compare equal, matching upstream's deliberately
-- perverse `Eq Null` instance.
#guard (Null.null == Null.null) == false

/-! ### `Query` -/

#guard Query.ofString "SELECT 1" == Query.mk "SELECT 1"
#guard (("SELECT 1" : Query)).fromQuery == "SELECT 1"
#guard toString (Query.mk "SELECT 1") == "SELECT 1"
#guard Query.empty ++ ("SELECT 1" : Query) == ("SELECT 1" : Query)
#guard ("SELECT " : Query) ++ ("1" : Query) == ("SELECT 1" : Query)

/-! ### `Only` -/

#guard (Only.mk 42).fromOnly == 42
#guard Only.mk 1 == Only.mk 1
#guard Only.mk 1 != Only.mk 2

/-! ### Row-cons -/

#guard (⟨1, "a"⟩ : Int :. String).car == 1
#guard (⟨1, "a"⟩ : Int :. String).cdr == "a"
#guard (⟨1, "a"⟩ : Int :. String) == Cons.mk 1 "a"

end Tests.Database.SQLite.Simple.Types
