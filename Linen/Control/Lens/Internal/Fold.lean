/-
  Linen.Control.Lens.Internal.Fold — `Folding`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Fold` (fetched and
  read via Hackage's rendered source). Upstream defines a handful of small
  monoid-shaped wrappers used by `Control.Lens.Fold`'s combinators:

  - `Folding f a`, wrapping a `Contravariant`/`Applicative` `f a`: its
    `Semigroup`/`Monoid` sequence effects left-to-right with `(*>)`, and the
    identity element is `noEffect` (this module's own `noEffect`, ported in
    `Control.Lens.Internal.Getter`). Backs `foldByOf`-style combinators.
  - `Traversed`/`TraversedF`, wrapping an `Apply`/`Applicative` action whose
    result is discarded (used by `traverseOf_`), and `Sequenced`, the same
    shape for a plain `Monad` (used by `mapM_`); all three implement their
    identity element with `error "…: value used"`, a placeholder value that
    upstream's own combinators are careful never to force.
  - `NonEmptyDList`, a difference-list wrapper around `Data.List.NonEmpty`.
  - `Leftmost`/`Rightmost`, small recursive types backing `firstOf`/`lastOf`
    with a short-circuiting `Semigroup` instance.

  **Scope note.** Only `Folding` is ported here. Every other type above has
  no call site anywhere in this batch's scope (their sole consumers —
  `foldByOf`, `traverseOf_`, `mapM_`, `firstOf`, `lastOf` — live in
  `Control.Lens.Fold` itself, out of scope until a later batch), and each
  has a translation obstacle of its own: `Traversed`/`TraversedF`/`Sequenced`
  encode their `mempty` with `error`, a partial value with no total Lean
  counterpart (and this codebase disallows `sorry`/`partial`); `Leftmost`/
  `Rightmost`'s whole reason to exist is short-circuiting on *infinite*
  Haskell lists, which has no counterpart against Lean's finite `List`.
  Manufacturing any of them with no real caller would just be dead weight;
  they are deferred to whichever later batch ports `Control.Lens.Fold`. -/

import Linen.Control.Lens.Internal.Getter

open Data.Functor

namespace Control.Lens.Internal

/-- `Folding F α`: a `Contravariant`, `Applicative` action `F α`, combined
    monoidally by running one after the other and discarding the
    intermediate result — the `Semigroup`/`Monoid` upstream's `foldByOf`-style
    combinators fold a structure's elements into. -/
structure Folding (F : Type u → Type u) (α : Type u) where
  /-- Unwrap to the underlying action. -/
  runFolding : F α

namespace Folding

/-- Combine two `Folding` actions by running the first, then the second,
    discarding the first's result (upstream's `Semigroup` instance,
    `Folding fa <> Folding fb = Folding (fa *> fb)`). -/
instance [SeqRight F] : Append (Folding F α) where
  append fa fb := ⟨SeqRight.seqRight fa.runFolding (fun _ => fb.runFolding)⟩

/-- The identity element for `Folding`'s `Append`: `noEffect`, upstream's
    `Monoid` instance's `mempty`. -/
instance [Contravariant F] [Pure F] : Inhabited (Folding F α) where
  default := ⟨noEffect⟩

end Folding

end Control.Lens.Internal
