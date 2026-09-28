/-
  Tests for `Linen.Control.Profunctor.Monad`.

  `NatTrans` between two concrete profunctors, and `ProfunctorFunctor`'s
  `promap` transporting it through the concrete `Cayley Option` transformer
  (`ProfunctorMonad`/`ProfunctorComonad` themselves are exercised concretely
  in `CayleyTest`/`RanTest`/`YonedaTest`, where the transformers they act on
  are defined).
-/
import Linen.Control.Profunctor.Cayley

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Monad

def inc : Fun Nat Nat := ⟨(· + 1)⟩

/-- A natural transformation from `Fun` to `Star Option`. -/
def toStar : NatTrans Fun (Star Option) := fun f => ⟨fun d => some (f.apply d)⟩

#guard (toStar inc).runStar 5 == some 6

/-! ### ProfunctorFunctor.promap transports `NatTrans` through `Cayley Option` -/

def cInc : Cayley Id Fun Nat Nat := ⟨inc⟩

#guard (ProfunctorFunctor.promap toStar cInc).runCayley.runStar 5 == some 6

end Tests.Control.Profunctor.Monad
