/-
  Monad combinators

  Monad utility functions that are not in the Lean standard library.
-/

namespace Control.Monad

/-- Monadic join: flattens a nested monadic value.

    $$\text{join} : m\,(m\;\alpha) \to m\;\alpha$$
    $$\text{join}\;mma = mma \mathbin{>>=} \text{id}$$ -/
def join {m : Type → Type} [Monad m] (mma : m (m α)) : m α :=
  mma >>= id

/-- Repeat a monadic action `n` times, collecting the results.

    $$\text{replicateM}\;n\;ma = [ma, ma, \ldots]\text{ (n times)}$$ -/
def replicateM {m : Type → Type} [Monad m] (n : Nat) (ma : m α) : m (List α) :=
  match n with
  | 0 => pure []
  | n + 1 => do
    let a ← ma
    let as ← replicateM n ma
    pure (a :: as)

/-- Repeat a monadic action `n` times, discarding the results.

    $$\text{replicateM\_}\;n\;ma = ma \mathbin{>>} \cdots \mathbin{>>} ma \mathbin{>>} \text{pure}\;()$$ -/
def replicateM_ {m : Type → Type} [Monad m] (n : Nat) (ma : m α) : m Unit :=
  match n with
  | 0 => pure ()
  | n + 1 => ma >>= fun _ => replicateM_ n ma

/-- **Join-pure law:** joining a pure value is the identity.

    $$\text{join}\;(\text{pure}\;x) = x$$ -/
theorem join_pure {m : Type → Type} [Monad m] [LawfulMonad m] (x : m α) :
    join (pure x) = x := by
  simp [join, pure_bind]

end Control.Monad
