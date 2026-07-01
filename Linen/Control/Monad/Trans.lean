/-
  `MonadTrans` — Haskell `mtl`-compatible lifting

  Haskell's `mtl` has a `MonadTrans t` class with a single method
  `lift : m α → t m α`, plus by-hand instances for every transformer.
  Lean core's `MonadLift m n` (and its lawful counterpart
  `LawfulMonadLift`) already generalize this — `n` need not literally be
  `t m`, and instances already exist for `ExceptT`, `ReaderT`, and `StateT`
  (with proofs that lifting commutes with `pure`/`bind`). So no new class
  or instances are declared here: only the Haskell name `lift` for Lean's
  `monadLift`, plus its two laws restated once, generically, instead of
  per transformer.

  ## Haskell source

  https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Trans.html
-/

namespace Control.Monad.Trans

/-- Lift a computation from an inner monad into an outer monad transformer.
    Alias for Lean's `monadLift`.

    $$\text{lift} : m\ \alpha \to n\ \alpha$$ -/
@[inline] def lift [MonadLift m n] (ma : m α) : n α :=
  monadLift ma

-- ── Laws ─────────────────────────────────────────

/-- `lift` preserves `pure`: `lift (pure a) = pure a`.
    $$\text{lift}(\text{pure}\ a) = \text{pure}\ a$$ -/
theorem lift_pure [Monad m] [Monad n] [MonadLift m n] [LawfulMonadLift m n] (a : α) :
    (lift (pure a : m α) : n α) = pure a :=
  LawfulMonadLift.monadLift_pure a

/-- `lift` distributes over `bind`: `lift (ma >>= f) = lift ma >>= (lift ∘ f)`.
    $$\text{lift}(ma \mathbin{>\!\!>\!\!=} f) = \text{lift}(ma) \mathbin{>\!\!>\!\!=} (\text{lift} \circ f)$$ -/
theorem lift_bind [Monad m] [Monad n] [MonadLift m n] [LawfulMonadLift m n]
    (ma : m α) (f : α → m β) :
    (lift (ma >>= f) : n β) = lift ma >>= fun a => lift (f a) :=
  LawfulMonadLift.monadLift_bind ma f

end Control.Monad.Trans
