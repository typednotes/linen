/-
  Linen.Control.Lens.Internal.Prism — `Market`, the van Laarhoven `Prism`'s
  concrete profunctor representation

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Prism` (fetched and
  read directly from Hackage's rendered source, not recalled from memory).
  `Market a b s t := Market (b -> t) (s -> Either t a)` is used internally by
  the `Prism` code to reify a prism the same way `Exchange` reifies an `Iso`:
  a "build" half `b -> t`, and a "match" half `s -> Either t a` that either
  recognizes `s` as an `a` (`Right`) or gives up with a already-built `t`
  (`Left`). Upstream's whole module exports exactly `Market`/`Market'`
  (`Market' a := Market a a`) with `Functor`, `Profunctor`, and `Choice`
  instances — no `Strong` or `Category` instance exists for `Market`
  anywhere upstream; all three real instances are ported below.
-/

import Linen.Control.Profunctor.Choice

open Control Control.Profunctor

namespace Control.Lens.Internal

-- ── Market ─────────────────────────────────────

/-- `Market a b s t`: the two functions that make up a prism `Prism s t a b`,
    packaged as a single concrete value — a "build" `b -> t`, and a "match"
    `s -> Either t a` that either recognizes `s` as an `a` (`.inr`) or gives
    up with an already-rebuilt `t` (`.inl`). Used internally by the `Prism`
    code the same way `Exchange` is used internally by the `Iso` code. -/
structure Market (A B S T : Type u) where
  /-- The "build" half of the prism. -/
  bt : B → T
  /-- The "match" half of the prism: recognize `s` as an `a`, or give up with
      an already-rebuilt `t`. -/
  seta : S → T ⊕ A

/-- `Market' a := Market a a`: the common case where the matched and built
    types agree. -/
abbrev Market' (A S T : Type u) := Market A A S T

/-- `Market A B S` is a `Functor` in its result type `T`: postcompose the
    "build" half with `f`, and re-tag a successful match's leftover `t` with
    `f` while leaving a recognized `a` untouched. -/
instance : Functor (Market A B S) where
  map f m := ⟨f ∘ m.bt, fun s => (m.seta s).elim (fun t => .inl (f t)) .inr⟩

/-- `Market A B` is a `Profunctor`: `lmap` precomposes the "match" half's
    input, `rmap` postcomposes the "build" half and re-tags a leftover `t`
    the same way `Functor.map` does. -/
instance : Profunctor (Market A B) where
  dimap l r m := ⟨r ∘ m.bt, fun s => (m.seta (l s)).elim (fun t => .inl (r t)) .inr⟩
  lmap l m := ⟨m.bt, m.seta ∘ l⟩
  rmap r m := ⟨r ∘ m.bt, fun s => (m.seta s).elim (fun t => .inl (r t)) .inr⟩

/-- `Market A B` is `Choice`: thread an extra alternative `γ` through the
    "match" half, tagging it as an immediate `.inl` leftover on the side that
    doesn't carry a matchable `s`. -/
instance : Choice (Market A B) where
  left' m := ⟨.inl ∘ m.bt, fun s => match s with
    | .inl s => (m.seta s).elim (fun t => .inl (.inl t)) .inr
    | .inr c => .inl (.inr c)⟩
  right' m := ⟨.inr ∘ m.bt, fun s => match s with
    | .inl c => .inl (.inl c)
    | .inr s => (m.seta s).elim (fun t => .inl (.inr t)) .inr⟩

end Control.Lens.Internal
