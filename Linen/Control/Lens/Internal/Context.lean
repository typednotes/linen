/-
  Linen.Control.Lens.Internal.Context — `Context`/`Pretext`, the van
  Laarhoven `Lens`'s comonadic "one hole" representation

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Context` (fetched
  and read via Hackage's rendered source). `Context a b t` is what you get by
  applying a `Lens s t a b` to a concrete `s`: a stored "position" `a`, and a
  function `b -> t` to plug a replacement back in and recover the whole
  structure — isomorphic to `forall f. Functor f => (a -> f b) -> f t`.
  `Pretext p a b t` generalizes this over a profunctor `p` (constrained to be
  `Conjoined`), letting the same shape support repeated `cloneLens`-style
  reuse.

  **Scope note.** Upstream shares `Context`/`Pretext`/`PretextT` through two
  common two-index classes, `IndexedFunctor` (`ifmap`) and `IndexedComonad`
  (`iextract`/`iduplicate`/`iextend`), plus an `IndexedComonadStore`
  extension (`ipos`/`ipeek`/`iseek`/`iexperiment`) that backs `cloneLens`'s
  actual implementation in a later batch's `Control.Lens.Lens`, and a
  `Sellable` class (`sell`) for building either type from a single value.
  None of that machinery has a call site anywhere in this batch's scope
  (the one real consumer, `cloneLens`, is `Control.Lens.Lens`, out of scope
  until a later batch), and `Pretext`'s own `Comonad` instance needs the
  `coarr` trick (converting a `Conjoined` profunctor value to a plain
  function via `Representable`/`Comonad (Rep p)`) that `Linen.Control.Lens.
  Internal.Indexed`'s deliberately-simplified `Conjoined` (see that module's
  scope note) has no infrastructure left to support. Rather than manufacture
  a two-index comonad hierarchy nothing here exercises, or a `Comonad`
  instance with no possible witness, this port keeps only `Context`'s and
  `Pretext`'s carrier *types* plus the direct, concretely-typed operations
  `Context` itself supports without that extra machinery (`extract`/
  `duplicate`, both well-typed exactly when upstream's own `a ~ b`-constrained
  `Comonad (Context a a)` instance is, expressed here directly in the
  functions' types rather than via a typeclass).
-/

import Linen.Control.Lens.Internal.Indexed

open Control

namespace Control.Lens.Internal

-- ── Context ────────────────────────────────────

/-- `Context a b t`: a stored position `a`, plus a function `b -> t` to plug
    a replacement back in. Isomorphic to `∀ f, Functor f → (a -> f b) -> f t`. -/
structure Context (A B T : Type u) where
  /-- Plug a replacement value back in. -/
  peek : B → T
  /-- The stored position. -/
  pos : A

namespace Context

/-- `Context A B` is a `Functor` in its result type `T`. -/
instance : Functor (Context A B) where
  map f c := ⟨f ∘ c.peek, c.pos⟩

/-- Build the trivial one-hole context around a single value: `sell a =
    Context id a` (upstream's `Sellable`, specialized to `p = (->)`, the only
    instance this batch's scope calls into). -/
@[inline] def sell (a : A) : Context A B B := ⟨id, a⟩

/-- Extract the result, in the case (upstream's `a ~ b`-constrained `Comonad`
    instance) where the position and the replacement slot agree: plug the
    position back into itself. -/
@[inline] def extract (c : Context A A T) : T := c.peek c.pos

/-- Duplicate: wrap the same `peek`, ready to be re-positioned (upstream's
    `Store`-comonad-shaped `duplicate`, in the same `a ~ b` case as `extract`). -/
@[inline] def duplicate (c : Context A A T) : Context A A (Context A A T) :=
  ⟨Context.mk c.peek, c.pos⟩

end Context

-- ── Pretext ────────────────────────────────────

/-- `Pretext p a b t`: `Context` generalized over a profunctor `p`, used
    upstream to let `cloneLens`-style combinators rebuild a lens polymorphic
    in an arbitrary `Conjoined` profunctor rather than committing to `(->)`.
    See the module's scope note for why only the carrier type (no comonadic
    operations) is ported here. -/
structure Pretext (P : Type u → Type u → Type v) (A B T : Type u) where
  /-- Run the pretext against any `Functor`-valued continuation. -/
  runPretext : ∀ {F : Type u → Type u}, [Functor F] → P A (F B) → F T

namespace Pretext

/-- `Pretext P A B` is a `Functor` in its result type `T`. -/
instance : Functor (Pretext P A B) where
  map f p := ⟨fun {_F} _ pafb => f <$> p.runPretext pafb⟩

end Pretext

end Control.Lens.Internal
