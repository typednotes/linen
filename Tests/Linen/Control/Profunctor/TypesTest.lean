/-
  Tests for `Linen.Control.Profunctor.Types`.

  `Profunctor` instances for `Star`, `Costar`, `WrappedArrow`, `Forget`, and
  `Tagged`.
-/
import Linen.Control.Profunctor.Types

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Types

/-! ### Star -/

def starLen : Star Option String Nat := ⟨fun s => if s.isEmpty then none else some s.length⟩

#guard (Profunctor.rmap (· * 2) starLen).runStar "abc" == some 6
#guard (Profunctor.lmap (· ++ "!") starLen).runStar "" == some 1
#guard (Profunctor.dimap (α := String) (γ := Nat) (δ := Nat)
          (· ++ "x") (· + 1) starLen).runStar "ab" == some 4

/-! ### Costar -/

def costarSum : Costar List Nat Nat := ⟨List.foldl (· + ·) 0⟩

#guard (Profunctor.rmap (· * 2) costarSum).runCostar [1, 2, 3] == 12
#guard (Profunctor.lmap (· + 1) costarSum).runCostar [1, 2, 3] == 9

/-! ### WrappedArrow (over `Control.Fun`) -/

def wrapped : WrappedArrow Fun Nat Nat := ⟨⟨(· + 1)⟩⟩

#guard (Profunctor.lmap (· * 2) wrapped).unwrapArrow.apply 3 == 7
#guard (Profunctor.rmap (· * 2) wrapped).unwrapArrow.apply 3 == 8

/-! ### Forget -/

def forgetLen : Forget Nat String Bool := ⟨String.length⟩

#guard (Profunctor.lmap (· ++ "!") forgetLen).runForget "ab" == 3
#guard (Profunctor.rmap (fun b => !b) forgetLen).runForget "ab" == 2

/-! ### Tagged -/

def tag : Tagged String Nat := ⟨42⟩

#guard (Profunctor.rmap (· + 1) tag).unTagged == 43
#guard (Tagged.retag tag : Tagged Bool Nat).unTagged == 42
#guard (Profunctor.lmap (fun _ : Bool => "x") tag : Tagged Bool Nat).unTagged == 42

end Tests.Control.Profunctor.Types
