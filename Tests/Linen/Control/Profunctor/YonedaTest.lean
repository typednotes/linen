/-
  Tests for `Linen.Control.Profunctor.Yoneda`.

  `Yoneda`/`Coyoneda` over `Control.Fun`: `returnYoneda`/`extractYoneda`,
  `returnCoyoneda`/`extractCoyoneda`, `Profunctor`/`Functor`/`Category`/
  `Strong`/`Choice`, and `ProfunctorFunctor.promap`.
-/
import Linen.Control.Profunctor.Yoneda

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Yoneda

def inc : Fun Nat Nat := ⟨(· + 1)⟩

/-! ### Yoneda -/

def yInc : Yoneda Fun Nat Nat := returnYoneda inc

#guard (extractYoneda yInc).apply 5 == 6
#guard (yInc.runYoneda id id).apply 5 == 6
#guard (Profunctor.rmap (· + 1) yInc |> extractYoneda).apply 5 == 7
#guard (Functor.map (· + 1) yInc |> extractYoneda).apply 5 == 7
#guard ((Category.id : Yoneda Fun Nat Nat) |> extractYoneda).apply 5 == 5
#guard (Strong.first' yInc |> extractYoneda).apply (5, "x") == (6, "x")
#guard (match (Choice.left' yInc |> extractYoneda).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6 | .inr _ => false)

def toStar : NatTrans Fun (Star Option) := fun f => ⟨fun d => some (f.apply d)⟩

#guard ((ProfunctorFunctor.promap toStar yInc |> extractYoneda).runStar 5 : Option Nat) == some 6

/-! ### Coyoneda -/

def cyInc : Coyoneda Fun Nat Nat := returnCoyoneda inc

#guard (extractCoyoneda cyInc).apply 5 == 6
#guard (Profunctor.rmap (· + 1) cyInc |> extractCoyoneda).apply 5 == 7
#guard (Functor.map (· + 1) cyInc |> extractCoyoneda).apply 5 == 7
#guard ((Category.id : Coyoneda Fun Nat Nat) |> extractCoyoneda).apply 5 == 5
#guard (Strong.first' cyInc |> extractCoyoneda).apply (5, "x") == (6, "x")
#guard (match (Choice.left' cyInc |> extractCoyoneda).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6 | .inr _ => false)

#guard ((ProfunctorFunctor.promap toStar cyInc |> extractCoyoneda).runStar 5 : Option Nat) ==
  some 6

end Tests.Control.Profunctor.Yoneda
