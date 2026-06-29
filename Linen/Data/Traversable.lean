/-
  Linen.Data.Traversable — the `Traversable` typeclass

  Structures that can be traversed left-to-right, performing an applicative
  action at each element and collecting the results. Core has no `Traversable`
  class, so it is ported here; Haskell's `Identity` functor is Lean core's `Id`.
-/

import Linen.Data.List.NonEmpty

namespace Data

/-- `Traversable` captures structures that can be traversed left-to-right,
    performing an applicative action at each element and collecting results.

    For a traversable $T$ and applicative $G$:
    $$\text{traverse} : (\alpha \to G\,\beta) \to T\,\alpha \to G\,(T\,\beta)$$
    $$\text{sequence} : T\,(G\,\alpha) \to G\,(T\,\alpha)$$

    The key insight: `traverse` generalizes both `map` (using `Id`) and
    `foldMap` (using a constant applicative). -/
class Traversable (T : Type u → Type u) extends Functor T where
  /-- Traverse a structure, applying an effectful function to each element and
      collecting results.
      $$\text{traverse}(f, [x_1, \ldots, x_n]) = f(x_1) \circledast \cdots \circledast f(x_n)$$
      where $\circledast$ denotes applicative combination. -/
  traverse {G : Type u → Type u} [Applicative G] : (α → G β) → T α → G (T β)

namespace Traversable

/-- Sequence effectful values left-to-right (`traverse id`).
    $$\text{sequence}([m_1, \ldots, m_n]) = m_1 \circledast \cdots \circledast m_n$$ -/
@[inline] def sequence [Traversable T] {G : Type u → Type u} [Applicative G]
    (t : T (G α)) : G (T α) :=
  Traversable.traverse id t

end Traversable

/-- Laws for a lawful traversable functor.

    **Identity:** traversing with `pure` (in `Id`) is `pure`.
    $$\text{traverse}(\text{pure}) = \text{pure}$$ -/
class LawfulTraversable (T : Type u → Type u) [Traversable T] : Prop where
  /-- Traversing with `pure` in the identity applicative `Id` is the identity. -/
  traverse_identity : ∀ (t : T α),
    Traversable.traverse (G := Id) (pure : α → Id α) t = pure t

/-! ── Instances ── -/

instance : Traversable List where
  traverse f l :=
    l.foldr (fun a acc => (· :: ·) <$> f a <*> acc) (pure [])

instance : Traversable Option where
  traverse f
    | some a => some <$> f a
    | none => pure none

instance : Traversable List.NonEmpty where
  traverse f ne :=
    let hd := f ne.head
    let tl := ne.tail.foldr (fun a acc => (· :: ·) <$> f a <*> acc) (pure [])
    List.NonEmpty.mk <$> hd <*> tl

/-- `Option` is lawfully traversable: `traverse pure = pure`. -/
instance : LawfulTraversable Option where
  traverse_identity
    | none => rfl
    | some _ => rfl

/-! ── Laws (compile-time) ── -/

/-- `sequence` is `traverse id`. -/
theorem sequence_eq_traverse_id [Traversable T] {G : Type u → Type u} [Applicative G]
    (t : T (G α)) : Traversable.sequence t = Traversable.traverse id t := rfl

/-- Traversing the empty list yields `pure []`. -/
theorem traverse_list_nil [Applicative G] (f : α → G β) :
    Traversable.traverse f ([] : List α) = pure [] := rfl

/-- Traversing `none` yields `pure none`. -/
theorem traverse_option_none [Applicative G] (f : α → G β) :
    Traversable.traverse f (none : Option α) = pure none := rfl

/-- Traversing `some a` maps `some` over the effect on `a`. -/
theorem traverse_option_some [Applicative G] (f : α → G β) (a : α) :
    Traversable.traverse f (some a) = some <$> f a := rfl

end Data
