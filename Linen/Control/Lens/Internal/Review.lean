/-
  Linen.Control.Lens.Internal.Review — `Reviewable`, `retagged`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Review` (fetched and
  read via Hackage's rendered source: the signatures below were pulled from
  the real source, not recalled from memory). Upstream:

  ```
  class (Profunctor p, Bifunctor p) => Reviewable p
  instance (Profunctor p, Bifunctor p) => Reviewable p

  retagged :: (Profunctor p, Bifunctor p) => p a b -> p s b
  ```

  `Reviewable` is a pure constraint synonym, kept upstream only "for backwards
  compatibility with lens 3.8" and to shorten later signatures — it adds no
  method of its own. `retagged` is the profunctor used to implement `Review`/
  `Prism` running "backwards" (build a `t` from a `b`, never inspect an `a`):
  it plays the role for `Review` that `Accessor`/`Const` play for `Getter`.
  Its implementation (`first absurd . lmap absurd`, using `Void`'s `absurd`)
  works for *any* `a`/`s`, not just `a = Void`, because `absurd : Void -> x`
  is polymorphic in its result: instantiating it once at `x = a` (for `lmap`)
  and once at `x = s` (for `first`) is enough to route around both type
  parameters without ever needing a real value of either. This port uses
  `PEmpty.elim` for Haskell's `Void`'s `absurd` — `PEmpty` rather than the
  fixed-universe `Empty`, since `P`'s two type parameters here live in the
  universe-polymorphic `Type u` — and `Data.Bifunctor.mapFst` for `first`.

  No scope trims: every declaration upstream exports (`Reviewable`, its one
  instance, `retagged`) has a direct, total Lean encoding and is ported below.
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Data.Bifunctor

open Control

namespace Control.Lens.Internal

-- ── Reviewable ─────────────────────────────────

/-- A profunctor that is also a `Bifunctor` — a pure constraint synonym with
    no method of its own, kept (as upstream keeps it) to shorten later
    `Review`/`Prism` signatures. -/
class Reviewable (P : Type u → Type u → Type v) extends Profunctor P, Data.Bifunctor P

/-- Every `Profunctor` that is also a `Bifunctor` is automatically
    `Reviewable`. -/
instance [Profunctor P] [Data.Bifunctor P] : Reviewable P where

-- ── retagged ───────────────────────────────────

/-- Reinterpret a profunctor value at an arbitrary new first type parameter:
    $\text{retagged} : P\,a\,b \to P\,s\,b$. Used internally to implement
    `Review`, playing the same role for `Review` that `Accessor`/`Const` play
    for `Getter`. Well-typed for *any* `a`/`s` because `PEmpty.elim` is
    polymorphic in its result type — see the module docstring. -/
def retagged {P : Type u → Type u → Type v} [Profunctor P] [Data.Bifunctor P]
    {A B S : Type u} (p : P A B) : P S B :=
  let q : P PEmpty B := Profunctor.lmap (β := A) PEmpty.elim p
  Data.Bifunctor.mapFst (γ := S) PEmpty.elim q

end Control.Lens.Internal
