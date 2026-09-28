/-
  Tests for `Linen.Control.Profunctor.Cayley`.

  `Cayley Option Control.Fun`: `Profunctor`/`Strong`/`Choice`/`Closed`
  instances, plus `ProfunctorFunctor`/`ProfunctorMonad`/`ProfunctorComonad`
  (the latter via a bespoke `Comonad` on a one-shot "identity comonad").
-/
import Linen.Control.Profunctor.Cayley

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Cayley

def inc : Fun Nat Nat := ⟨(· + 1)⟩

def cInc : Cayley Option Fun Nat Nat := ⟨some inc⟩

/-! ### Profunctor / Strong / Choice / Closed -/

#guard (Profunctor.rmap (· + 1) cInc).runCayley.map (fun f => f.apply 5) == some 7
#guard (Strong.first' cInc).runCayley.map (fun f => f.apply (5, "x")) == some (6, "x")
#guard (Closed.closed (X := Bool) cInc).runCayley.map (fun f => f.apply (fun _ => 5) true) ==
  some 6

/-! ### ProfunctorFunctor -/

def dropToId : NatTrans Fun (Costar Id) := fun f => ⟨fun a => f.apply a⟩

#guard (ProfunctorFunctor.promap dropToId cInc).runCayley.map (fun k => k.runCostar 5) == some 6

/-! ### ProfunctorMonad (over `Option`, a `Monad`) -/

#guard (ProfunctorMonad.proreturn (T := Cayley Option) inc).runCayley.map (·.apply 5) == some 6

def nested : Cayley Option (Cayley Option Fun) Nat Nat := ⟨some cInc⟩

#guard (ProfunctorMonad.projoin nested).runCayley.map (·.apply 5) == some 6

/-! ### ProfunctorComonad (over a one-shot `Comonad`) -/

instance : Comonad Id where
  extract w := w
  extend f w := f w

#guard (ProfunctorComonad.proextract (T := Cayley Id) (⟨inc⟩ : Cayley Id Fun Nat Nat)).apply 5 == 6

/-! ### mapCayley -/

def toOption (i : Id α) : Option α := some i

#guard (mapCayley (F := Id) (G := Option) toOption
  (⟨inc⟩ : Cayley Id Fun Nat Nat)).runCayley.map (·.apply 5) == some 6

end Tests.Control.Profunctor.Cayley
