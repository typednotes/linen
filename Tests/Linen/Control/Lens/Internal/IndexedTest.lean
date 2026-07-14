/-
  Tests for `Linen.Control.Lens.Internal.Indexed`.

  `Indexed Nat`: `Profunctor`/`Strong`/`Choice`/`Closed`/`Category`/
  `Conjoined`/`Indexable` instances, plus `Conjoined`/`Indexable` for
  `Control.Fun`.
-/
import Linen.Control.Lens.Internal.Indexed

open Control Control.Profunctor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Indexed

def ix : Indexed Nat Nat Nat := ⟨fun i a => i + a⟩

/-! ### Profunctor -/

#guard (Profunctor.rmap (· + 1) ix).runIndexed 10 5 == 16
#guard (Profunctor.lmap (· + 1) ix).runIndexed 10 5 == 16
#guard (Profunctor.dimap (· + 1) (· + 100) ix).runIndexed 10 5 == 116

/-! ### Strong -/

#guard (Strong.first' (γ := String) ix).runIndexed 10 (5, "x") == (15, "x")
#guard (Strong.second' (γ := String) ix).runIndexed 10 ("x", 5) == ("x", 15)

/-! ### Choice -/

#guard (Choice.left' (γ := String) ix).runIndexed 10 (.inl 5) == .inl 15
#guard (Choice.left' (γ := String) ix).runIndexed 10 (.inr "x") == .inr "x"

/-! ### Closed -/

#guard (Closed.closed (X := Nat) ix).runIndexed 10 (· + 1) 5 == 16

/-! ### Category -/

def ix2 : Indexed Nat Nat Nat := ⟨fun i a => i * a⟩

#guard (Category.comp ix ix2).runIndexed 10 5 == 150

/-! ### Conjoined / Indexable -/

#guard (Conjoined.distrib (F := Option) ix).runIndexed 10 (some 5) == some 15
#guard Indexable.indexed ix 10 5 == 15
#guard (ix.atIndex 10).apply 5 == 15

/-! ### Conjoined / Indexable for `Control.Fun` -/

def f : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Conjoined.distrib (F := Option) f).apply (some 5) == some 6
#guard Indexable.indexed f 999 5 == 6

end Tests.Control.Lens.Internal.Indexed
