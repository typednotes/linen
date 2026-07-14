/-
  Linen.Control.Lens.Equality — `Equality`, `simple`, `runEq`, `mapEq`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Equality` (fetched and read
  via Hackage's rendered Haddock and source). Upstream's `Equality s t a b`
  is the optic witnessing that `s`/`a` and `t`/`b` are literally the same
  type — the strongest possible optic, from which every other kind (`Iso`,
  `Lens`, `Traversal`, …) can be derived by weakening its constraint. It is
  represented, like every optic here, as a rank-2 profunctor-and-functor
  polymorphic function `∀ p f, p a (f b) -> p s (f t)`, i.e. `Optic p f s t a
  b` (`Control.Lens.Type`) with *no* constraint placed on `p` or `f` at all.

  **Scope note (kind-polymorphism, `AnEquality`, `Identical`).** Upstream's
  real `Equality` is polymorphic over *three* kinds (`s :: k1`, `a :: k1`,
  `t :: k2`, `b :: k2`), since GHC's type-level equality witnesses
  (`Data.Type.Equality`'s `(:~:)`) apply at any kind, and it ships a second,
  GADT-based representation, `AnEquality` (built from `Identical`), used by
  the rank-2-avoiding combinators (`substEq`, `withEquality`, `overEquality`,
  `cloneEquality`, `fromLeibniz`, …) that pattern-match on that GADT to
  recover a `(~)` constraint from an already-rank-2-instantiated argument —
  a trick GHC needs because a caller cannot re-generalize an already
  rank-2-typed value back into a polymorphic position. Lean's elaborator has
  no such restriction: an `Equality` value here can be instantiated directly
  at whatever `P`/`F` a combinator needs, so `runEq`/`mapEq` below work
  straight off `Equality` itself, with no `AnEquality`/`Identical` detour
  needed, and `linen` has ported no `Data.Type.Equality`-style witness type
  standing in for `(:~:)` — Lean's native `Eq`/pattern-matching on `rfl`
  already covers everything those combinators (`equality`, `equality'`,
  `fromLeibniz`) would otherwise exist to bridge.

  **Scope note (universe polymorphism).** `Control.Lens.Type`'s aliases
  (`Lens`, `Setter`, …) are kept universe-polymorphic (`Type u`) since they
  are never *run* against a manufactured witness profunctor — only ever
  instantiated at concrete `Functor`/`Applicative`/`Settable` functors.
  `runEq`/`mapEq` below, by contrast, must instantiate `Equality`'s own `p`
  at a purpose-built `Type`-valued profunctor carrying a `Prop`-valued
  equality proof (`PLift (x = a)`, since a bare `Prop` cannot inhabit a
  `Type v` slot for a rigid universe variable `v`), and Lean's universe
  checker cannot discharge that instantiation across an open universe
  variable shared with `Equality`'s other, unrelated uses. This module
  therefore fixes `Equality`'s four type indices at a single concrete `Type`
  (rather than reusing `Control.Lens.Type`'s polymorphic `Optic`), which is
  also every real use of `Equality` in practice (witnessing two ordinary,
  already-concrete types are equal). -/

import Linen.Control.Lens.Type

namespace Control.Lens

-- ── Equality ────────────────────────────────────

/-- `Equality s t a b := ∀ p f, p a (f b) -> p s (f t)`: the optic
    witnessing `s ~ a` and `t ~ b` — the strongest optic, since running it
    imposes no constraint on `p` or `f` at all. See the module's scope note
    for why this is fixed at a concrete `Type` rather than reusing
    `Control.Lens.Type`'s universe-polymorphic `Optic`. -/
abbrev Equality (S T A B : Type) :=
  ∀ {P : Type → Type → Type} {F : Type → Type}, P A (F B) → P S (F T)

/-- `Equality' s a := Equality s s a a`. -/
abbrev Equality' (S A : Type) := Equality S S A A

/-- `simple`: forces an `Optic' p f s a` whose type variable is otherwise
    unconstrained to agree with the visible one — useful when a `Lens`/
    `Traversal`/… has a constraint mentioning an unused type variable that
    would otherwise be left ambiguous. Upstream's own doc comment: "useful
    when your `Lens`, `Traversal`, or `Iso` has a constraint on an unused
    argument to force it to be the same as the used argument." -/
@[inline] def simple {P : Type → Type → Type} {F : Type → Type} {A : Type}
    (p : P A (F A)) : P A (F A) := p

/-- Recover the type equality `s = a` witnessed by an `Equality s t a b`, by
    instantiating it at the profunctor `fun x _ => PLift (x = a)` (with `f :=
    id`): feeding in the trivial witness `a = a` yields exactly `s = a`. -/
@[inline] def runEq {S T A B : Type} (eq : Equality S T A B) : S = A :=
  (eq (P := fun X _ => PLift (X = A)) (F := id) ⟨rfl⟩).down

/-- Recover the type equality `t = b` witnessed by an `Equality s t a b`, the
    counterpart of `runEq` for the second index pair. -/
@[inline] def runEq' {S T A B : Type} (eq : Equality S T A B) : T = B :=
  (eq (P := fun _ Y => PLift (Y = B)) (F := id) ⟨rfl⟩).down

/-- `mapEq`: transport an `f s` value along the type equality an `Equality s
    t a b` witnesses, recovering an `f a`. -/
@[inline] def mapEq {S T A B : Type} {F : Type → Type v} (eq : Equality S T A B)
    (fs : F S) : F A :=
  cast (congrArg F (runEq eq)) fs

end Control.Lens
