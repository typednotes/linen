/-
  Linen.Control.Lens.Internal.Zoom — `Focusing`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Zoom` (fetched and
  read via Hackage's rendered source). Upstream defines the internal
  newtype wrappers `zoom`/`magnify` (`Control.Lens.Zoom`) use to focus a
  lens into a sub-state or sub-environment of a monad-transformer stack, one
  wrapper per stack shape: `Focusing` (plain `StateT`), `FocusingWith`
  (`RWST`), `FocusingPlus` (`WriterT`), `FocusingOn` (`MaybeT`/`ListT`),
  `FocusingMay`/`May` (`ErrorT`/`Maybe`), `FocusingErr`/`Err` (`Either`),
  `FocusingFree`/`Freed` (`FreeT`), and the phantom-typed `Effect`/
  `EffectRWS` used by `magnify`. Each ships with `Functor`/`Applicative`
  instances that thread the wrapped state/log/error alongside the effect.
  Upstream then ties them all together through a multi-parameter,
  functional-dependency-driven `Zoom m n s t` class with an associated type
  family `Zoomed m :: * -> * -> *` and a method `zoom :: LensLike' (Zoomed
  m c) s t -> m c -> n c`.

  **Scope note.** Only `Focusing` — the wrapper for `zoom`-into-`StateT`, the
  one monad-transformer shape `linen` actually has (Lean core's `StateT`,
  aliased by `Linen.Control.Monad.State`; confirmed by searching `linen` for
  `StateT`/`ReaderT` before writing this module) — is ported here, with its
  real, faithful `Functor`/`Applicative` instances. Everything else is
  dropped:
  - `FocusingWith`/`FocusingPlus`/`FocusingOn`/`FocusingMay`/`May`/
    `FocusingErr`/`Err`/`FocusingFree`/`Freed` each wrap a monad-transformer
    shape (`RWST`, `WriterT`, `MaybeT`/`ListT`, `ErrorT`, `FreeT`) `linen` has
    not ported, and manufacturing that transformer infrastructure was not
    requested by, and is well out of scope for, this batch.
  - `Effect`/`EffectRWS` back `magnify` (the `Reader`-side counterpart of
    `zoom`); `linen`'s `Control.Monad.Reader` only aliases Lean core's
    `ReaderT` and has no `MonadReader`-class abstraction over reader-shaped
    stacks for `Effect` to generalize over, so it is deferred alongside
    `magnify` itself.
  - The `Zoom`/`Zoomed` class itself has no possible Lean encoding here: it
    is keyed to `LensLike'` (`Control.Lens.Type`, not yet ported — out of
    scope until a later batch that ports the public lens/traversal types)
    and to a `MonadState` class abstracting over arbitrary transformer
    stacks, which `linen` also does not have (only the concrete `get`/`set`/
    `StateT` operations Lean core already provides). Introducing either as
    new infrastructure invented for this module alone, with no caller
    anywhere in `linen`, would not be a faithful port of anything upstream
    actually needs at this point — it is deferred to whichever later batch
    ports `Control.Lens.Zoom` together with `Control.Lens.Type`. -/

namespace Control.Lens.Internal

/-- `Focusing M S A := M (S × A)`: the effect of a `zoom`-ed `StateT S M`
    computation, threading the sub-state `S` alongside the result `A`. -/
structure Focusing (M : Type u → Type u) (S A : Type u) where
  /-- Unwrap to the underlying `M (S × A)`. -/
  runFocusing : M (S × A)

namespace Focusing

/-- `Focusing M S` is a `Functor`: map over the result, leaving the threaded
    state untouched. -/
instance [Monad M] : Functor (Focusing M S) where
  map f x := ⟨(fun (s, a) => (s, f a)) <$> x.runFocusing⟩

/-- `Focusing M S`'s `pure`: the state contributed by a pure value is the
    monoidal identity `default` (upstream's `Semigroup`/`Monoid S`
    constraint, matching this codebase's existing `Append`/`Inhabited`
    convention for monoid-shaped type parameters — see `Data.Functor.Const`'s
    `Pure` instance). -/
instance [Monad M] [Inhabited S] : Pure (Focusing M S) where
  pure a := ⟨Pure.pure (default, a)⟩

/-- `Focusing M S`'s `seq`: run both computations in sequence, combining
    their contributed states with `++` (`Append`). -/
instance [Monad M] [Append S] : Seq (Focusing M S) where
  seq f x := ⟨do
    let (s₁, g) ← f.runFocusing
    let (s₂, y) ← (x ()).runFocusing
    pure (s₁ ++ s₂, g y)⟩

/-- `Focusing M S` is `Applicative` whenever `S` is a monoid (`Append` +
    `Inhabited`) and `M` is a `Monad`. -/
instance [Monad M] [Append S] [Inhabited S] : Applicative (Focusing M S) where

end Focusing

end Control.Lens.Internal
