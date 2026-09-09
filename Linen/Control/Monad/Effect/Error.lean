/-
  `Control.Monad.Effect.Error` — the error effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Error.html
  module #5 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  ## Relationship to `Control.Monad.Except`

  This does **not** replace `linen`'s mtl-style `Control.Monad.Except`, which
  remains the recommended API for ordinary error propagation. This module puts
  errors *in the effect row*, so a signature records that a computation may fail
  and with what — and lets failure compose with the other row effects under one
  set of handlers.

  ## Substitutions / deviations

  - **`Either` → `Except`.** `runError` returns `Except ε α`, per AGENTS.md's
    stdlib-first rule.

  - **The request answers with `Empty`.** Upstream's GADT is
    `Error e r` for an arbitrary `r`, since a throw never returns. Written in
    Lean as `| throw : ε → Error ε α` with a phantom `α`, the constructor would
    bind `{α : Type}` and so force `Error` into `Type 1`, which cannot be an
    effect (`Type → Type`). Answering with `Empty` says the same thing more
    precisely — the continuation can never be invoked — and `throwError` recovers
    an arbitrary result type by `Empty.elim`. `NonDet.mzero` uses the same device
    for the same reason.

  - **`HasError` locator.** `throwError`'s error type `ε` appears only in its
    `Member (Error ε) effs` constraint, so instance search stalls on
    `Member (Error ?ε) effs` before `ε` is known. `HasError` carries `ε` as an
    `outParam`, so resolving it against the row *determines* `ε` — the same
    device `Control.Monad.Effect.FileSystem` uses for its capability.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.Error

open Data.OpenUnion Control.Monad.Effect

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The error effect: a single request carrying an error of type `ε`.

    It answers with `Empty`, recording that a throw never returns. -/
inductive Error (ε : Type) : Type → Type where
  /-- Abort with an error. -/
  | throw : ε → Error ε Empty

/-- Locates an `Error` effect in the row and recovers *which* error type it
    carries.

    `ε` is an `outParam`, so resolving against the row determines it. Without
    this, `throwError e`'s `ε` would stay a metavariable — it appears nowhere in
    the result type — and instance search would stall. -/
class HasError (effs : List (Type → Type)) (ε : outParam Type) where
  /-- Inject an error request into the row. -/
  inject : {α : Type} → Error ε α → Union effs α

/-- The error effect is the row's head. -/
instance instHasErrorHere {ε : Type} {effs : List (Type → Type)} :
    HasError (Error ε :: effs) ε where
  inject e := .here e

/-- The error effect is somewhere in the row's tail. -/
instance instHasErrorThere {ε : Type} {eff : Type → Type}
    {effs : List (Type → Type)} [HasError effs ε] :
    HasError (eff :: effs) ε where
  inject e := .there (HasError.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Abort the computation with an error.

    The result type is arbitrary because the request answers with `Empty`: there
    is no value to continue with, so `Empty.elim` supplies any type. -/
def throwError {ε : Type} {effs : List (Type → Type)} {α : Type u}
    [hs : HasError effs ε] (e : ε) : Eff effs α :=
  (Eff.impure (hs.inject (.throw e)) Eff.protect : Eff effs Empty).bindH Empty.elim

-- ── Handlers ────────────────────────────────────────────────────────────────

/-- Run a computation that may fail, removing the error effect from the row and
    reporting the outcome as an `Except`.

    The first throw wins: everything after it is discarded, since its
    continuation is unreachable. -/
def runError {ε : Type} {effs : List (Type → Type)} {α : Type} :
    Eff (Error ε :: effs) α → Eff effs (Except ε α)
  | .protect a  => .protect (.ok a)
  | .impure u k => match u with
    | .here e   => match e with
      | .throw x => .protect (.error x)
    | .there u' => .impure u' (fun b => runError (k b))

/-- Recover from an error *without* removing the effect from the row.

    Built on `interpose`, so `m` keeps its access to the error effect and a
    throw raised by `handle` itself propagates normally. -/
def catchError {ε : Type} {effs : List (Type → Type)} {α : Type}
    [HasError effs ε] [Member (Error ε) effs]
    (m : Eff effs α) (handle : ε → Eff effs α) : Eff effs α :=
  interpose (eff := Error ε) pure
    (fun e _ => match e with | .throw x => handle x) m

/-- Replace a failure by a pure fallback value. -/
def orElseValue {ε : Type} {effs : List (Type → Type)} {α : Type}
    [HasError effs ε] [Member (Error ε) effs]
    (m : Eff effs α) (fallback : α) : Eff effs α :=
  catchError m (fun _ => .protect fallback)

end Control.Monad.Effect.Error
