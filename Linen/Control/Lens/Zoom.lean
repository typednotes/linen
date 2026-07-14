/-
  Linen.Control.Lens.Zoom — `zoom`, `magnify`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Zoom` (fetched and read via
  the real source, `src/Control/Lens/Zoom.hs`, not recalled from memory).
  Upstream's own description: `zoom` "runs a monadic action in a larger
  `State` than it was defined in, using a `Lens'` or `Traversal''", and
  `magnify` does the same thing for a `Reader`-shaped environment via a
  `Getter`. Both let a computation written against a *sub-piece* of a
  bigger state/environment be run as if it were a computation against the
  whole thing, without hand-threading the projection at every step.

  Upstream's real `zoom`/`magnify` are methods of two multi-parameter,
  functional-dependency classes, `Zoom m n s t` and `Magnify m n b a`, each
  with roughly a dozen instances — one per monad-transformer shape
  (`StateT` strict/lazy, `RWST` strict/lazy, `WriterT` strict/lazy,
  `ReaderT`, `IdentityT`, `MaybeT`, `ExceptT`, `FreeT`, plus legacy
  `ErrorT`/`ListT`) — dispatched automatically by whichever transformer
  stack the caller's monad happens to be built from.

  **Scope note (the `Zoom`/`Magnify` classes themselves).**
  `Linen.Control.Lens.Internal.Zoom`'s own scope note already explains why
  no such class can be built here: it would need to range over an
  mtl-style `MonadState`/`MonadReader` *class* abstracting over arbitrary
  transformer stacks, and `linen` has ported neither — only the concrete
  `StateT`/`ReaderT` monads themselves (`Linen.Control.Monad.{State,
  Reader}`). With a single concrete transformer shape to dispatch to
  (`StateT`/`ReaderT` over an arbitrary inner monad `Z`, via `Focusing` —
  the only wrapper `Linen.Control.Lens.Internal.Zoom` ported), there is
  also only one instance's worth of behavior to give, so `zoom`/`magnify`
  are ported directly as ordinary functions specialized to that one shape,
  rather than as class methods with no second instance to justify the
  class. This mirrors this codebase's existing precedent for the same
  situation in `Linen.Control.Lens.{Getter.to,Setter.sets}` (a
  `Profunctor`-generalized upstream combinator, specialized to the one
  concrete shape `linen` actually has).

  **Scope note (`zoom`'s generality: `Lens'` only, not `Traversal'`).**
  Upstream's `zoom` works uniformly for both a `Lens''` (needing only
  `Monad m`) and a `Traversal''` (additionally needing `Monoid c`, so the
  per-target results can be combined) — the difference is invisible at the
  type `LensLike' (Zoomed m c) t s -> m c -> n c`, since the class's
  `Zoomed`/`Zoom` machinery picks the right constraint on `Focusing`'s own
  `Functor`/`Applicative` instances automatically once the caller supplies
  a `Lens'` or `Traversal'` term. Here, `zoom` must fix a concrete
  constraint set up front (Lean has no such automatic per-call
  discrimination): it is given below at the `Lens'`-only shape (needing
  just `[Monad Z]`, matching upstream's simplest, always-available
  instance and this batch's only requested use), which needs nothing from
  `Focusing`'s `Applicative` instance (hence no `[Append C] [Inhabited C]`
  burden on every caller). The `Traversal'`-widening (upstream's `zoom
  traverse $ _2 %= f`-style examples, needing a `Monoid c` and a second,
  `Applicative`-flavored code path selected via the very `Zoom` class this
  module cannot build) is left for whichever later batch can encode that
  per-call discrimination some other way.

  **Scope note (`magnify`'s generality: direct `Getter`, no `Magnified`
  machinery).** Upstream's `Magnify` class also ranges over `RWST`,
  `IdentityT`, and a *phantom* `(->) b` instance (`magnify = views`, run
  with no monad at all), each via the `Effect`/`EffectRWS` wrappers
  (`Linen.Control.Lens.Internal.Zoom`'s own scope note explains why neither
  is ported: no `MonadReader` class to generalize over). Since `linen`'s
  `Getter` is already the concretely-`Contravariant`-instantiated shape
  `magnify` needs, and its `ReaderT` is a plain `ρ → m α` function
  (`Linen.Control.Monad.Reader`), `magnify` is ported directly, with no
  `Effect` wrapper at all, as "run the inner `ReaderT` action against the
  projected environment" — semantically identical to upstream's `ReaderT`
  instance (`magnify l (ReaderT m) = ReaderT $ getEffect #. l (Effect #.
  m)`) once `Effect`'s definition is unfolded, and it also subsumes
  upstream's separate `(->) b` instance (`magnify = views`), since
  `linen`'s `Reader ρ α` is *already* `ReaderT ρ Id α`
  (`Linen.Control.Monad.Reader.Reader`) rather than a distinct bare-arrow
  type needing its own instance. -/

import Linen.Control.Lens.Getter
import Linen.Control.Lens.Internal.Zoom

open Control.Lens.Internal

namespace Control.Lens

-- ── zoom ────────────────────────────────────────

/-- `zoom :: Monad m => Lens'' s t -> StateT t m a -> StateT s m a`: run a
    `StateT`-shaped monadic action defined against a sub-piece `S` of a
    larger state `T`, focused there through a `Lens' T S`, as an action
    against the whole `T` — every other component of `T` is left untouched
    across the run. Implemented by instantiating the lens at `Focusing Z C`
    (`Linen.Control.Lens.Internal.Zoom`), the wrapper that threads the
    substate `S` alongside the zoomed action's own result `C`. -/
@[inline] def zoom {S T C : Type u} {Z : Type u → Type u} [Monad Z]
    (l : Lens' T S) (m : StateT S Z C) : StateT T Z C :=
  fun t => (l (F := Focusing Z C) (fun s => (⟨m.run s⟩ : Focusing Z C S)) t).runFocusing

-- ── magnify ─────────────────────────────────────

/-- `magnify :: Getter s t -> ReaderT t m a -> ReaderT s m a`: run a
    `ReaderT`-shaped monadic action defined against a sub-piece `S` of a
    larger environment `T`, focused there through a `Getter T S`, as an
    action against the whole `T` — implemented by simply running the inner
    action with the environment `view l t` projected out first. Unlike
    `zoom`, `magnify` can be used with any `Getter` (or anything weaker,
    e.g. a `Lens'`/`Traversal'`/`Fold`, all of which specialize to a
    `Getter` here), never a `Traversal`/`Fold` in the sense of *combining*
    multiple targets, since a `Reader` environment has exactly one value to
    read at a time. -/
@[inline] def magnify {S T C : Type u} {Z : Type u → Type u}
    (l : Getter T S) (m : ReaderT S Z C) : ReaderT T Z C :=
  fun t => m.run (view l t)

end Control.Lens
