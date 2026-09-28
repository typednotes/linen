/-
  Tests for `Linen.Control.Monad.Effect`.

  Covers the `Eff` monad over an open effect row: `pure`/`bind`/`do`-notation,
  `send`, `run`, `runM`, `interpret`, `interpretM`, `reinterpret` and `raise`.
-/
import Linen.Control.Monad.Effect

open Data.OpenUnion Control.Monad.Effect

namespace Tests.Control.Monad.Effect

-- ── The empty row: pure computations ────────────────────────────────────────

-- A computation over the empty row performs no effects and runs to a value.
#guard Eff.run (pure 5 : Eff [] Nat) == 5

-- `do`-notation works at `Eff`'s universe (`Type 1`).
#guard Eff.run (do let x ← pure 1; let y ← pure 2; pure (x + y) : Eff [] Nat) == 3

-- Left-nested binds associate correctly (the case upstream's dropped
-- `FTCQueue` optimises; behaviour is unchanged).
#guard Eff.run ((((pure 1 : Eff [] Nat) >>= fun a => pure (a + 1))
                  >>= fun b => pure (b + 1)) >>= fun c => pure (c + 1)) == 4

-- Monad laws on a concrete computation.
#guard Eff.run ((pure 7 : Eff [] Nat) >>= pure) == 7
#guard Eff.run ((pure 7 : Eff [] Nat) >>= fun a => pure (a * 3)) == 21

-- `Functor`/`Applicative` come from the `Monad` instance.
#guard Eff.run ((· + 1) <$> (pure 41 : Eff [] Nat)) == 42

-- ── A single effect, sent and interpreted ───────────────────────────────────

/-- A teletype-style output effect: one request carrying a string. -/
inductive Emit : Type → Type where
  | emit : String → Emit Unit

def emit {effs : List (Type → Type)} [Member Emit effs] (s : String) :
    Eff effs Unit :=
  send (.emit s : Emit Unit)

/-- Interpret `Emit` by accumulating into the state threaded by hand, so the
    test needs nothing but this module. -/
def collect {α : Type} : Eff [Emit] α → List String → α × List String
  | .protect a,  acc => (a, acc)
  | .impure u k, acc => match u with
    | .here e   => match (e : Emit _) with
      | .emit s => collect (k ()) (acc ++ [s])
    | .there u' => u'.elim0

#guard collect (do emit "a"; emit "b"; emit "c"; pure 0 : Eff [Emit] Nat) []
         == (0, ["a", "b", "c"])

-- ── `interpret` eliminates the head effect from the row ─────────────────────

/-- Ask for a constant `Nat`. -/
inductive Const : Type → Type where
  | value : Const Nat

def constValue {effs : List (Type → Type)} [Member Const effs] : Eff effs Nat :=
  send (.value : Const Nat)

-- Interpreting `Const` away leaves the empty row, so `run` applies.
#guard Eff.run (interpret (fun | .value => .protect 9)
                  (do let a ← constValue; pure (a * 2) : Eff [Const] Nat)) == 18

-- ── `interpretM` lands in a base monad ─────────────────────────────────────

-- With `Id` as the base monad, `interpretM` is `interpret` followed by `run`.
#guard Id.run (interpretM (m := Id) (fun | .value => pure 4)
          (do let a ← constValue; pure (a + 1) : Eff [Const] Nat)) == 5

-- ── `runM` runs a row that is just a base monad ────────────────────────────

#guard Id.run (Eff.runM (pure 3 : Eff [Id] Nat)) == 3

-- ── `raise` weakens a row with an unused effect ────────────────────────────

-- A computation using nothing can stand in where `Const` is permitted; the
-- added effect is simply never requested.
#guard Eff.run (interpret (fun | .value => .protect 0)
                  (raise (pure 6 : Eff [] Nat) : Eff [Const] Nat)) == 6

-- ── `reinterpret` rewrites one effect into another ─────────────────────────

-- Express `Const` in terms of `Emit`: each request emits, then answers 1. The
-- row `[Const]` becomes `[Emit]` — `reinterpret` swaps the head and keeps the
-- tail, so the effect count is unchanged.
#guard collect (reinterpret
                  (fun | Const.value => (do emit "asked"; pure 1 : Eff [Emit] Nat))
                  (do let a ← constValue; let b ← constValue; pure (a + b)
                      : Eff [Const] Nat)) []
         == (2, ["asked", "asked"])

-- ── The row is the whitelist ────────────────────────────────────────────────

-- `Member` is what makes an effect available, so a computation's row *is* the
-- set of effects it may perform. `Emit` is not in `[Const]`, hence there is no
-- way to build an `Eff [Const] α` that emits — the property the whole design
-- rests on. Illustrated positively: the same program typechecks once `Emit`
-- is added to the row.
example : Eff [Const, Emit] Nat := do
  let a ← constValue
  emit "fine"
  pure a

end Tests.Control.Monad.Effect
