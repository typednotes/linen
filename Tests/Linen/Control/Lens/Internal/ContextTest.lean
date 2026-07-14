/-
  Tests for `Linen.Control.Lens.Internal.Context`.

  `Context`/`Pretext`: `Functor`, `sell`/`extract`/`duplicate`.
-/
import Linen.Control.Lens.Internal.Context

open Control Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Context

def c : Context Nat Nat Nat := ⟨(· + 1), 5⟩

#guard c.pos == 5
#guard c.peek 5 == 6
#guard (Functor.map (· * 10) c).peek 5 == 60

#guard (Context.sell (B := Nat) 5).pos == 5
#guard (Context.sell (B := Nat) 5).peek 7 == 7

#guard Context.extract c == 6
#guard (Context.duplicate c).pos == 5
#guard Context.extract ((Context.duplicate c).peek (Context.duplicate c).pos) == 6

def p : Pretext Fun Nat Nat Nat :=
  ⟨fun {_F} _ pafb => (fun b => b + 1) <$> pafb.apply 5⟩

example : p.runPretext (F := Id) ⟨fun a => a⟩ = (6 : Id Nat) := rfl
example : (Functor.map (· * 10) p).runPretext (F := Id) ⟨fun a => a⟩ = (60 : Id Nat) := rfl

end Tests.Control.Lens.Internal.Context
