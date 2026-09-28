/-
  Tests for `Linen.Control.Profunctor.Rep`.

  `Distributive Id`; `Representable` over `Control.Fun` (via `Id`) and
  `Star` (via itself); `firstRep`/`secondRep`.
-/
import Linen.Control.Profunctor.Rep

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Rep

/-! ### Distributive: Id -/

example : (Distributive.distribute (some (5 : Id Nat)) : Id (Option Nat)) = some 5 := rfl
example : (Distributive.collect (F := Id) (G := Option) (fun n => (n + 1 : Id Nat)) (some 5) :
  Id (Option Nat)) = some 6 := rfl

/-! ### Representable: Control.Fun -/

def tabFun : Fun Nat Nat := Representable.tabulate (fun n => (n + 1 : Id Nat))

#guard tabFun.apply 5 == 6

/-! ### Representable: Star -/

def tabStar : Star Option Nat Nat := Representable.tabulate (fun n => some (n + 1))

#guard tabStar.runStar 5 == some 6

/-! ### firstRep / secondRep (via Star) -/

def starInc : Star Option Nat Nat := ⟨fun n => some (n + 1)⟩

#guard (firstRep starInc).runStar (5, "x") == some (6, "x")
#guard (secondRep starInc).runStar ("x", 5) == some ("x", 6)

end Tests.Control.Profunctor.Rep
