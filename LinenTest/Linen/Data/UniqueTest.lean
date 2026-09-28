/-
  Tests for `Linen.Data.Unique`.

  `Unique`'s constructor is private, so values can only be minted by
  `newUnique`. Everything (distinctness, `BEq`/`Ord`, `hashUnique`, `ToString`)
  is therefore exercised through `#eval` over freshly allocated values — a
  thrown error fails the build. This is also the only way to test the type:
  there is deliberately no way to fabricate a `Unique`.
-/
import Linen.Data.Unique

open Data

namespace Tests.Data.Unique

#eval show IO Unit from do
  let u1 ← newUnique
  let u2 ← newUnique
  let u3 ← newUnique
  -- distinctness and reflexive equality
  unless u1 != u2 do throw (IO.userError "u1 and u2 should differ")
  unless u2 != u3 do throw (IO.userError "u2 and u3 should differ")
  unless u1 == u1 do throw (IO.userError "a Unique should equal itself")
  -- the counter strictly increases by one per allocation
  unless u1.hashUnique + 1 == u2.hashUnique do
    throw (IO.userError s!"expected u2 = u1+1, got {u1.hashUnique}, {u2.hashUnique}")
  unless u2.hashUnique + 1 == u3.hashUnique do
    throw (IO.userError s!"expected u3 = u2+1, got {u2.hashUnique}, {u3.hashUnique}")
  -- Ord reflects allocation order
  unless compare u1 u2 == Ordering.lt do throw (IO.userError "expected u1 < u2")
  unless compare u2 u1 == Ordering.gt do throw (IO.userError "expected u2 > u1")
  unless compare u1 u1 == Ordering.eq do throw (IO.userError "compare u1 u1 should be eq")
  -- ToString surfaces the underlying id
  unless toString u1 == s!"Unique({u1.hashUnique})" do
    throw (IO.userError s!"unexpected toString: {u1}")

end Tests.Data.Unique
