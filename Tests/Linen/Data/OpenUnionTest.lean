/-
  Tests for `Linen.Data.OpenUnion`.

  Covers the open union over an effect row: `Union.here`/`Union.there`,
  `Member.inj`/`Member.prj`, and that instance search locates an effect at any
  depth in the row.
-/
import Linen.Data.OpenUnion

open Data.OpenUnion

namespace Tests.Data.OpenUnion

-- Three distinct single-request effects, so a row of them is unambiguous.
inductive Tick : Type → Type where | tick : Tick Nat
  deriving DecidableEq

inductive Beep : Type → Type where | beep : Beep Bool
  deriving DecidableEq

inductive Buzz : Type → Type where | buzz : Buzz Nat
  deriving DecidableEq

abbrev Row : List (Type → Type) := [Tick, Beep, Buzz]

-- ── Membership resolves at every depth in the row ───────────────────────────

-- Depth 1: the effect is the row's head.
#guard (Member.prj (Member.inj Tick.tick : Union Row Nat) : Option (Tick Nat)) == some Tick.tick

-- Depth 2: one `there` away.
#guard (Member.prj (Member.inj Beep.beep : Union Row Bool) : Option (Beep Bool)) == some Beep.beep

-- Depth 3: two `there`s away — instance search recurses through the tail.
#guard (Member.prj (Member.inj Buzz.buzz : Union Row Nat) : Option (Buzz Nat)) == some Buzz.buzz

-- ── Projection is partial: a union holding one effect misses another ────────

-- `Tick` and `Buzz` are both `… → Nat`, so this is a genuine tag test rather
-- than a type mismatch: projecting a `Tick` union as `Buzz` must fail.
#guard (Member.prj (Member.inj Tick.tick : Union Row Nat) : Option (Buzz Nat)) == none
#guard (Member.prj (Member.inj Buzz.buzz : Union Row Nat) : Option (Tick Nat)) == none

-- ── Injection agrees with the raw constructors ──────────────────────────────

-- `inj` at depth 1 is `here`; at depth 3 it is `there (there (here …))`.
#guard ((Member.inj Tick.tick : Union Row Nat) matches .here _)
#guard ((Member.inj Buzz.buzz : Union Row Nat) matches .there (.there (.here _)))

-- ── A single-effect row ─────────────────────────────────────────────────────

#guard (Member.prj (Member.inj Tick.tick : Union [Tick] Nat) : Option (Tick Nat)) == some Tick.tick

-- `Union [] α` is uninhabited, so `elim0` gives anything — the fact that lets
-- `Eff.run` discharge its impossible branch.
example {α : Type} {C : Sort _} (u : Union [] α) : C := u.elim0

end Tests.Data.OpenUnion
