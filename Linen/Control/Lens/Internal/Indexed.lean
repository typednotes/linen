/-
  Linen.Control.Lens.Internal.Indexed — `Indexed`, `Conjoined`, `Indexable`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Indexed` (fetched
  and read via Hackage's rendered source). `Indexed i a b := i -> a -> b` is
  the profunctor an *indexed* optic (`IndexedLens`, `IndexedTraversal`, …) is
  built from: it behaves like an ordinary function `a -> b`, but also carries
  an index `i` (a position, a key, a path) alongside each element.

  **Scope note (`Conjoined`).** Upstream's real class head is

  ```
  class ( Choice p, Corepresentable p, Comonad (Corep p), Traversable (Corep p)
        , Strong p, Representable p, Monad (Rep p), MonadFix (Rep p), Distributive (Rep p)
        , Costrong p, ArrowLoop p, ArrowApply p, ArrowChoice p, Closed p
        ) => Conjoined p where
    distrib :: Functor f => p a b -> p (f a) (f b)
    conjoined :: ((p ~ (->)) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
  ```

  `linen`'s `Corepresentable` has **no concrete instances at all** (see the
  scope note in `Linen.Control.Profunctor.Rep` — every one of upstream's own
  instances needs a lazy knot-tying `Costrong` implementation with no total
  Lean translation), and `linen` has no `Monad`-of-`Rep`/`MonadFix`/`Arrow*`
  infrastructure either. Requiring any of that here would make `Conjoined`
  an empty class with zero possible instances, defeating its purpose. This
  port keeps only the two superclasses every actual instance below needs
  (`Strong`, `Choice`) and the one method every downstream user of
  `Conjoined` in this batch actually calls (`distrib`); `conjoined` itself
  (the `p ~ (->)`-gated dispatch method) has no call site anywhere in this
  batch's scope (its only upstream use is deep inside `cloneIndexPreservingLens`-
  style combinators, out of scope until `Control.Lens.Lens` in a later batch)
  and its rank-2-with-type-equality encoding has no faithful non-contrived
  Lean translation, so it is dropped rather than manufactured.

  Similarly, upstream's `Indexing`/`Indexing64` (an `Int`/`Int64`-counting
  applicative used by the public `indexing` combinator) and `withIndex`/
  `asIndex` are deferred to whichever later batch ports
  `Control.Lens.Indexed` itself, the only consumer.
-/

import Linen.Control.Lens.Internal.Profunctor
import Linen.Control.Profunctor.Strong
import Linen.Control.Profunctor.Closed

open Control Control.Profunctor

namespace Control.Lens.Internal

-- ── Conjoined / Indexable ──────────────────────

/-- A profunctor `p` that is "conjoined" with the ordinary function arrow: it
    can push a `Functor` through itself the same way `(->)` does via `fmap`.
    See the module docstring for how this simplifies upstream's real
    (much larger) class head. -/
class Conjoined (P : Type u → Type u → Type v) extends Strong P, Choice P where
  /-- Push an arbitrary `Functor` through `P`: $\text{distrib} : P\,a\,b \to P\,(F\,a)\,(F\,b)$. -/
  distrib {F : Type u → Type u} [Functor F] : P α β → P (F α) (F β)

/-- Ordinary functions are `Conjoined`: `distrib = Functor.map`. -/
instance : Conjoined Control.Fun where
  distrib f := ⟨fun fa => f.apply <$> fa⟩

/-- A profunctor `p` indexable by `i`: it can be run at a given index to
    recover an ordinary function. -/
class Indexable (I : Type u) (P : Type u → Type u → Type v) extends Conjoined P where
  /-- Run `p` at a given index, recovering an ordinary function. -/
  indexed : P α β → I → α → β

/-- Ordinary functions are `Indexable` at any index: the index is ignored. -/
instance : Indexable I Control.Fun where
  indexed f _ a := f.apply a

-- ── Indexed ────────────────────────────────────

/-- `Indexed i a b := i -> a -> b`: an ordinary function that also carries an
    index `i`, the profunctor backing every indexed optic. -/
structure Indexed (I A B : Type u) where
  /-- Run the underlying `i -> a -> b`. -/
  runIndexed : I → A → B

namespace Indexed

/-- Forget the index, recovering a plain `Control.Fun` at a fixed index. -/
@[inline] def atIndex (ix : Indexed I A B) (i : I) : Fun A B := ⟨ix.runIndexed i⟩

end Indexed

/-- `Indexed i` is a `Profunctor`: `dimap`/`lmap`/`rmap` thread through the
    index unchanged. -/
instance : Profunctor (Indexed I) where
  dimap l r ix := ⟨fun i a => r (ix.runIndexed i (l a))⟩
  lmap l ix := ⟨fun i a => ix.runIndexed i (l a)⟩
  rmap r ix := ⟨fun i a => r (ix.runIndexed i a)⟩

/-- `Indexed i` is `Strong`: the index passes through untouched. -/
instance : Strong (Indexed I) where
  first' ix := ⟨fun i (a, c) => (ix.runIndexed i a, c)⟩
  second' ix := ⟨fun i (c, a) => (c, ix.runIndexed i a)⟩

/-- `Indexed i` is `Choice`: the index passes through untouched. -/
instance : Choice (Indexed I) where
  left' ix := ⟨fun i s => match s with
    | .inl a => .inl (ix.runIndexed i a)
    | .inr c => .inr c⟩
  right' ix := ⟨fun i s => match s with
    | .inl c => .inl c
    | .inr a => .inr (ix.runIndexed i a)⟩

/-- `Indexed i` is `Closed`: the index passes through untouched. -/
instance : Closed (Indexed I) where
  closed ix := ⟨fun i g x => ix.runIndexed i (g x)⟩

-- Note: upstream also gives `Indexed i` a `Costrong` instance
-- (`unfirst :: Indexed i (a,d) (b,d) -> Indexed i a b`), but its
-- implementation ties a knot through Haskell's laziness exactly like the
-- `Costrong Control.Fun` instance this codebase already omits (see
-- `Linen.Control.Profunctor.Strong`'s note) — there is no total, strict
-- Lean translation, so it is dropped here for the same reason.

/-- `Indexed i` forms a `Category`: composition threads the same index
    through both sides. -/
instance : Category (Indexed I) where
  id := ⟨fun _ a => a⟩
  comp f g := ⟨fun i a => g.runIndexed i (f.runIndexed i a)⟩

/-- `Indexed i` is `Conjoined`: `distrib` maps the index-carrying function
    over the pushed-through functor. -/
instance : Conjoined (Indexed I) where
  distrib ix := ⟨fun i fa => ix.runIndexed i <$> fa⟩

/-- `Indexed i` is `Indexable` at its own index: running it just applies the
    stored function. -/
instance : Indexable I (Indexed I) where
  indexed ix := ix.runIndexed

end Control.Lens.Internal
