/-
  `Control.Monad.Effect.Fresh` — the fresh-name effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Fresh.html
  module #9 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  Hands out a distinct integer on each request — the usual way to generate
  unique names or identifiers without threading a counter by hand.

  ## Substitutions / deviations

  - **`Int` → `Nat`.** Upstream's counter is `Int`; fresh names are never
    negative, and `Nat` matches this library's general convention.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.Fresh

open Data.OpenUnion Control.Monad.Effect

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The fresh-name effect: one request, answered with a number not previously
    handed out. -/
inductive Fresh : Type → Type where
  /-- Request a fresh number. -/
  | fresh : Fresh Nat

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Obtain a number distinct from every one previously returned. -/
def fresh {effs : List (Type → Type)} [Member Fresh effs] : Eff effs Nat :=
  send (.fresh : Fresh Nat)

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Answer requests with successive numbers starting from `start`, removing the
    effect from the row.

    Written as a direct recursion rather than via `interpret` because the
    handler carries the counter between requests. -/
def runFresh {effs : List (Type → Type)} {α : Type} :
    Nat → Eff (Fresh :: effs) α → Eff effs α
  | _, .protect a  => .protect a
  | n, .impure u k => match u with
    | .here e   => match e with
      | .fresh => runFresh (n + 1) (k n)
    | .there u' => .impure u' (fun b => runFresh n (k b))

/-- `runFresh` starting from `0`. -/
def runFresh0 {effs : List (Type → Type)} {α : Type}
    (m : Eff (Fresh :: effs) α) : Eff effs α :=
  runFresh 0 m

end Control.Monad.Effect.Fresh
