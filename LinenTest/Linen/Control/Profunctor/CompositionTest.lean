/-
  Tests for `Linen.Control.Profunctor.Composition`.

  `Procompose`/`Rift` over `Control.Fun`, `procomposed`, `decomposeRift`.
-/
import Linen.Control.Profunctor.Composition

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Composition

def inc : Fun Nat Nat := ⟨(· + 1)⟩
def dbl : Fun Nat Nat := ⟨(· * 2)⟩

/-! ### Procompose -/

def composed : Procompose Fun Fun Nat Nat := ⟨inc, dbl⟩

#guard composed.outer.apply (composed.inner.apply 5) == 11
#guard (procomposed composed).apply 5 == 11
#guard (procomposed (Profunctor.rmap (· + 100) composed)).apply 0 == 101
#guard (procomposed (Strong.first' composed)).apply (0, "x") == (1, "x")

/-! ### Rift -/

def riftId : Rift Fun Fun Nat Nat := ⟨fun p => p⟩

#guard (riftId.runRift inc).apply 5 == 6

-- `decomposeRift` combines a `Rift`-wrapper (whose hidden `∀{X}` field bumps
-- its own result universe above that of its flat `P`/`Q` arguments) with a
-- `Procompose` requiring both slots to share one literal universe. A flat,
-- concrete profunctor like `Control.Fun` can never satisfy that shared-
-- universe constraint here, so the law is illustrated abstractly instead.
example [Profunctor P] [Profunctor Q] (pr : Procompose P (Rift P Q) α β) :
    decomposeRift pr = pr.inner.runRift pr.outer := rfl

end Tests.Control.Profunctor.Composition
