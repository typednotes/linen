/-
  Linen.Control.Monad.IO.Unlift — MonadUnliftIO

  Provides the `MonadUnliftIO` typeclass, which allows running monadic
  actions in `IO` by "unlifting" them. This is the core abstraction from
  Haskell's `unliftio-core` package.

  ## Design

  In Haskell, `MonadUnliftIO` requires `MonadIO` and provides `withRunInIO`.
  In Lean 4, `MonadLift IO m` plays the role of `MonadIO`. We define
  `MonadUnliftIO` with the same API shape.

  The CPS form `withRunInIO` avoids universe issues: the polymorphic
  run function `∀ α, m α → IO α` lives in `Type 1` and cannot be returned
  inside `m : Type → Type`, but it can be consumed in continuation position.

  ## Guarantees

  Laws (expected of instances):
  - `withRunInIO (λ run => run m) = m` (identity)
  - `withRunInIO (λ run => run (liftIO io)) = liftIO io` (lift-run roundtrip)
-/

namespace Control.Monad.IO

/-- Typeclass for monads that can "unlift" back to IO.

    $$\text{withRunInIO} : ((\forall \alpha.\; m\;\alpha \to \text{IO}\;\alpha) \to \text{IO}\;\beta) \to m\;\beta$$

    This enables running callbacks that require plain `IO` from within
    a monadic context `m`. The CPS form avoids universe issues with
    returning the polymorphic run function directly. -/
class MonadUnliftIO (m : Type → Type) [Monad m] [MonadLiftT IO m] where
  /-- Provide a function to run `m` actions in `IO`. -/
  withRunInIO : ((∀ α, m α → IO α) → IO β) → m β

namespace MonadUnliftIO

/-- Convenience: run a single `m` action in `IO`.
    $$\text{toIO}(act) : m\;(\text{IO}\;\alpha)$$ -/
@[inline] def toIO [Monad m] [MonadLiftT IO m] [MonadUnliftIO m]
    (act : m α) : m (IO α) :=
  withRunInIO fun run => pure (run α act)

/-- Lift an IO-expecting callback into `m` by providing the unlift.
    $$\text{liftIOOp} : ((\text{IO}\;\alpha \to \text{IO}\;\alpha) \to \text{IO}\;\beta) \to m\;\alpha \to m\;\beta$$ -/
@[inline] def liftIOOp [Monad m] [MonadLiftT IO m] [MonadUnliftIO m]
    (f : (IO α → IO α) → IO β) (act : m α) : m β :=
  withRunInIO fun run => f (fun _ => run α act)

end MonadUnliftIO

-- ══════════════════════════════════════════════════════════════
-- Instances
-- ══════════════════════════════════════════════════════════════

/-- IO trivially unlifts to itself: the run function is `id`. -/
instance : MonadUnliftIO IO where
  withRunInIO f := f fun _ x => x

/-- `ReaderT r IO` can unlift by capturing the environment. -/
instance : MonadUnliftIO (ReaderT r IO) where
  withRunInIO f := do
    let env ← read
    liftM (f fun _ act => act.run env)

end Control.Monad.IO
