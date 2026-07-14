/-
  Linen.Control.Lens.Internal.Iso — `Exchange`, the van Laarhoven `Iso`'s
  concrete profunctor representation

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Iso` (fetched and
  read directly from Hackage's rendered source, not recalled from memory).
  `Exchange a b s t := Exchange (s -> a) (b -> t)` is used internally by the
  `Iso` code to reify an isomorphism the same way `Context` reifies a `Lens`:
  it packages the two functions that make up an `Iso s t a b` (a "get" `s ->
  a` and a "build" `b -> t`) as a single concrete value, without going
  through the general `Profunctor`-polymorphic encoding.

  **Scope note.** Upstream's real module is *smaller* than this batch's own
  plan assumed: it gives `Exchange` only `Functor` (in `t`) and `Profunctor`
  instances — no `Choice`, `Strong`, `Costrong`, `Category`, or `EqRep`
  instance exists for `Exchange` anywhere upstream (`EqRep`/
  `Bicontravariant` are not upstream concepts at all; they were a plan
  artifact, same as the nonexistent `Bicontravariant` class flagged in this
  batch's `Control.Lens.Internal.Profunctor` port). Both real instances are
  ported below.

  Upstream's module also bundles an unrelated `Reversing` class (`reversing
  :: t -> t`, "a generalized notion of list reversal") together with
  instances for `[a]`, `NonEmpty`, strict/lazy `ByteString`/`Text`, and
  several `Vector`/`Seq` variants. That class has no connection to `Exchange`
  or to reifying an `Iso` — it exists upstream purely to back the public
  `reversed` combinator in the later `Control.Lens.Iso` batch, which is the
  only consumer and is out of scope here. Porting it now would also require
  committing to Lean-side container types for every one of those upstream
  instances well before any of them are otherwise needed. It is deferred to
  whichever later batch ports `Control.Lens.Iso` itself.
-/

import Linen.Control.Profunctor.Types

open Control Control.Profunctor

namespace Control.Lens.Internal

-- ── Exchange ───────────────────────────────────

/-- `Exchange a b s t`: the two functions that make up an isomorphism `Iso s
    t a b`, packaged as a single concrete value — a "get" `s -> a` and a
    "build" `b -> t`. Used internally by the `Iso` code the same way
    `Context` is used internally by the `Lens` code. -/
structure Exchange (A B S T : Type u) where
  /-- The "get" half of the isomorphism. -/
  sa : S → A
  /-- The "build" half of the isomorphism. -/
  bt : B → T

/-- `Exchange A B S` is a `Functor` in its result type `T`:
    $\text{fmap}\;f\;(\text{Exchange}\;sa\;bt) = \text{Exchange}\;sa\;(f \circ bt)$. -/
instance : Functor (Exchange A B S) where
  map f e := ⟨e.sa, f ∘ e.bt⟩

/-- `Exchange A B` is a `Profunctor`: `lmap` precomposes the "get" half,
    `rmap` postcomposes the "build" half. -/
instance : Profunctor (Exchange A B) where
  dimap l r e := ⟨e.sa ∘ l, r ∘ e.bt⟩
  lmap l e := ⟨e.sa ∘ l, e.bt⟩
  rmap r e := ⟨e.sa, r ∘ e.bt⟩

end Control.Lens.Internal
