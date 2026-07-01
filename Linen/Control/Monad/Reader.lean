/-
  `ReaderT` / `Reader` — Haskell `mtl`-compatible names

  Lean core already defines `ReaderT`, `read` (Haskell's `ask`),
  `ReaderT.adapt` (Haskell's `local`), and `ReaderT.run`; this module adds
  the Haskell `mtl` names built on top of them, plus the `Reader` type alias
  that core has no dedicated name for.

  ## Haskell source

  https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html
-/

namespace Control.Monad.Reader

/-- The `Reader` monad: `ReaderT` over `Id`.

    $$\text{Reader}\ \rho\ \alpha = \text{ReaderT}\ \rho\ \text{Id}\ \alpha = \rho \to \alpha$$ -/
abbrev Reader (ρ : Type) (α : Type) := ReaderT ρ Id α

/-- Read the environment. Alias for Lean's `read`.

    $$\text{ask} : \text{ReaderT}\ \rho\ m\ \rho$$ -/
@[inline] def ask [Monad m] : ReaderT ρ m ρ :=
  read

/-- Project a function over the environment.

    $$\text{asks}(f) = f \mathbin{<\!\$\!>} \text{ask}$$ -/
@[inline] def asks [Monad m] (f : ρ → α) : ReaderT ρ m α :=
  f <$> read

/-- Run a computation in a modified environment. Alias for Lean's `ReaderT.adapt`.

    $$\text{local}(f, ma) : \text{ReaderT}\ \rho\ m\ \alpha$$

    Runs `ma` with the environment transformed by `f`. -/
@[inline] def «local» (f : ρ → ρ) (ma : ReaderT ρ m α) : ReaderT ρ m α :=
  ReaderT.adapt f ma

/-- Run a `ReaderT` computation with a given environment. Alias for `ReaderT.run`.

    $$\text{runReaderT}(ma, \rho) : m\ \alpha$$ -/
@[inline] def runReaderT (ma : ReaderT ρ m α) (env : ρ) : m α :=
  ma.run env

/-- Run a `Reader` computation with a given environment.

    $$\text{runReader}(ma, \rho) : \alpha$$ -/
@[inline] def runReader (ma : Reader ρ α) (env : ρ) : α :=
  ma.run env

/-- Map over the inner monadic computation.

    $$\text{mapReaderT}(f, ma) : \text{ReaderT}\ \rho\ n\ \beta$$

    where $f : m\ \alpha \to n\ \beta$. -/
@[inline] def mapReaderT (f : m α → n β) (ma : ReaderT ρ m α) : ReaderT ρ n β :=
  fun env => f (ma.run env)

-- ── Proofs ──────────────────────────────────────────

/-- `ask` returns the environment: `runReaderT ask env = pure env`.
    $$\text{runReaderT}(\text{ask}, \rho) = \text{pure}(\rho)$$ -/
theorem ask_run [Monad m] (env : ρ) :
    runReaderT (ask : ReaderT ρ m ρ) env = pure env := rfl

/-- `local id` is identity: does not change the computation.
    $$\text{local}(\text{id}, ma) = ma$$ -/
theorem local_id (ma : ReaderT ρ m α) : «local» id ma = ma := by
  rfl

/-- `runReader` of `pure a` returns `a`.
    $$\text{runReader}(\text{pure}\ a, \rho) = a$$ -/
theorem runReader_pure (a : α) (env : ρ) : runReader (pure a : Reader ρ α) env = a := rfl

end Control.Monad.Reader
