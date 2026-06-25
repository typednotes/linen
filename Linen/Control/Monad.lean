/-
  Monad combinators

  Monad utility functions, in the Haskell-compatible `Control.Monad` style, that
  are **not** in the Lean standard library. The rest of Haskell's
  `Control.Monad` already has a stdlib spelling and should be used directly:

  | Haskell        | Lean stdlib            |
  |----------------|------------------------|
  | `void`         | `Functor.discard`      |
  | `mapM_`/`forM_`| `List.forM`            |
  | `foldM`        | `List.foldlM`          |
  | `filterM`      | `List.filterM`         |
  | `zipWithM`     | `List.zipWithM`        |
  | `>=>` / `<=<`  | `· >=> ·` / `· <=< ·`  |
-/

namespace Control.Monad

/-- Monadic join: flattens a nested monadic value.

    $$\text{join} : m\,(m\;\alpha) \to m\;\alpha$$
    $$\text{join}\;mma = mma \mathbin{>>=} \text{id}$$ -/
def join {m : Type → Type} [Monad m] (mma : m (m α)) : m α :=
  mma >>= id

/-- Conditional execution: run the action only when the boolean is `true`,
otherwise do nothing. (Lean core has no `Control.Monad.when`.)

    $$\text{«when»}\;b\;a = \begin{cases} a & \text{if } b \\ \text{pure}\;() & \text{otherwise} \end{cases}$$ -/
@[inline] def «when» {m : Type → Type} [Monad m] (b : Bool) (action : m Unit) : m Unit :=
  if b then action else pure ()

/-- Conditional execution: run the action only when the boolean is `false`.

    $$\text{unless}\;b\;a = \text{when}\;(\lnot b)\;a$$ -/
@[inline] def «unless» {m : Type → Type} [Monad m] (b : Bool) (action : m Unit) : m Unit :=
  «when» (!b) action

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

/-- `«when» true` runs the action. -/
theorem when_true {m : Type → Type} [Monad m] (a : m Unit) : «when» true a = a := rfl

/-- `«when» false` does nothing. -/
theorem when_false {m : Type → Type} [Monad m] (a : m Unit) : «when» false a = pure () := rfl

/-- `«unless» false` runs the action. -/
theorem unless_false {m : Type → Type} [Monad m] (a : m Unit) : «unless» false a = a := rfl

/-- `«unless» true` does nothing. -/
theorem unless_true {m : Type → Type} [Monad m] (a : m Unit) : «unless» true a = pure () := rfl

end Control.Monad
