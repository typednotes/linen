/-
  Tests for `Linen.Control.Profunctor.Ran`.

  `Ran`/`Codensity` over `Control.Fun`; `curryRan`/`uncurryRan`;
  `decomposeRan`/`decomposeCodensity`.

  **Universe note.** `decomposeRan`/`decomposeCodensity` combine `Ran`'s/
  `Codensity`'s hidden `∀{X}` field (which bumps their own result universe
  above that of their flat arguments) with `Procompose`'s requirement that
  both composed slots share one literal universe. A flat, concrete
  profunctor like `Control.Fun` can never satisfy that shared-universe
  constraint here, so these two laws are illustrated abstractly instead.
-/
import Linen.Control.Profunctor.Ran

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Ran

def inc : Fun Nat Nat := ⟨(· + 1)⟩
def dbl : Fun Nat Nat := ⟨(· * 2)⟩

/-! ### Ran -/

def ranId : Ran Fun Fun Nat Nat := ⟨fun p => p⟩

#guard (ranId.runRan inc).apply 5 == 6
#guard ((Profunctor.rmap (· + 1) ranId).runRan inc).apply 5 == 7
#guard ((Functor.map (· + 1) ranId).runRan inc).apply 5 == 7

/-! ### decomposeRan -/

example [Profunctor P] [Profunctor Q] (pr : Procompose (Ran Q P) Q α β) :
    decomposeRan pr = pr.outer.runRan pr.inner := rfl

/-! ### curryRan / uncurryRan -/

def curryRanTest : Ran Fun Fun Nat Nat :=
  curryRan (fun {α β} (pq : Procompose Fun Fun α β) => procomposed pq) inc

-- `curryRanTest.runRan dbl = procomposed ⟨inc, dbl⟩ = inc ∘ dbl` (diagrammatic),
-- so at `5`: `dbl 5 = 10`, then `inc 10 = 11`.
#guard (curryRanTest.runRan dbl).apply 5 == 11

/-- Embed a `Fun α β` into `Ran Fun Fun α β` by post-composing whatever gets threaded in. -/
def uncurryRanF : ∀ {α β}, Fun α β → Ran Fun Fun α β :=
  fun {_ _} p => ⟨fun q => ⟨fun x => p.apply (q.apply x)⟩⟩

-- `uncurryRan uncurryRanF ⟨inc, dbl⟩ = (uncurryRanF inc).runRan dbl`, which post-composes
-- `dbl` with `inc`, so at `5`: `dbl 5 = 10`, then `inc 10 = 11`.
#guard (uncurryRan uncurryRanF (⟨inc, dbl⟩ : Procompose Fun Fun Nat Nat)).apply 5 == 11

/-! ### Codensity -/

def codId : Codensity Fun Nat Nat := ⟨fun p => p⟩

#guard (codId.runCodensity inc).apply 5 == 6

/-! ### decomposeCodensity -/

example [Profunctor P] (pp : Procompose (Codensity P) P α β) :
    decomposeCodensity pp = pp.outer.runCodensity pp.inner := rfl

end Tests.Control.Profunctor.Ran
