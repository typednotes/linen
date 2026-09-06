/-
  `Control.Monad.Freer` — the `Eff` monad over an open row of effects

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer.html
  (merged with `Control.Monad.Freer.Internal`), module #2 of the `FreerSimple`
  import (see `docs/imports/FreerSimple/dependencies.md`).

  `Eff effs α` is a computation returning `α` that may perform the effects in
  the row `effs` — **and no others**. The row is part of the type, so a
  signature is an effect whitelist: `Eff [Reader Config] α` cannot touch state,
  the filesystem, or the network, because there is no way to build a
  `Union [Reader Config] β` holding anything else.

  Effects are ordinary inductive types (Haskell's effect GADTs); `send` lifts one
  operation into `Eff` given `Member` evidence; a *handler* (`interpret`,
  `interpretM`, or a hand-written recursion) eliminates one effect from the row.
  Running a computation therefore means peeling the row down to `[]` and calling
  `run` — the type records that every effect has been given a meaning.

  ## Substitutions / deviations

  - **`Data.FTCQueue` is dropped.** Upstream stores continuations in a catenable
    "fast type-aligned" queue purely so left-nested `>>=` chains avoid quadratic
    continuation concatenation under GHC. That is a performance device, not a
    semantic one. This port uses the direct Freer encoding, which is behaviour-
    and type-identical and differs only in the amortised cost of pathologically
    left-nested binds.

  - **`Control.Monad.Freer.TH` is dropped** (no Template Haskell in Lean): the
    `send`-wrapping smart constructors `makeEffect` would generate are written by
    hand per effect, as with `lens`'s `Control.Lens.TH`.

  - **Natural transformations are plain functions.** Upstream's handler types use
    `natural-transformation`'s `m :~> n`; Lean writes that directly as
    `{β : Type} → eff β → m β`, so the dependency folds away.

  - **Universe.** `Type → Type` inhabits `Type 1`, so `Eff`'s result universe is
    at least 1. The payload is universe-polymorphic (`Type u` in,
    `Type (max 1 u)` out) rather than pinned to `Type 0`, because
    `Control.Monad.Freer.Coroutine`'s `Status` type holds an
    `Eff effs (Status …)` and so must itself live in `Type 1`; with a `Type 0`-only
    payload that is not expressible. Since the result universe is `max 1 u`, a
    `Type 1` payload still yields a `Type 1` computation, which is what breaks
    the circularity. `Monad.{u,v}` is universe-polymorphic, so
    `Monad (Eff effs)` instantiates per payload universe and `do`-notation works
    as usual.
-/
import Linen.Data.OpenUnion

namespace Control.Monad.Freer

open Data.OpenUnion

-- ── The `Eff` monad ─────────────────────────────────────────────────────────

/-- A computation returning `α` that may perform exactly the effects in `effs`.

    `protect` is a finished value (upstream's `Val`). `impure` is one effect
    request `Union effs β` paired with the continuation `β → Eff effs α` to
    resume with once it is answered — so an `Eff` value is a syntax tree of
    pending requests, and a handler is an interpreter for it.

    The self-reference sits to the right of an arrow, so the type is strictly
    positive and needs no `partial`. -/
inductive Eff : List (Type → Type) → Type u → Type (max 1 u) where
  /-- A pure value; performs no effect. -/
  | protect {effs : List (Type → Type)} {α : Type u} : α → Eff effs α
  /-- An effect request together with its continuation. Effects answer with
      `Type 0` values (`β : Type`), while the computation's own result `α` may
      live in any universe. -/
  | impure  {effs : List (Type → Type)} {α : Type u} {β : Type} :
      Union effs β → (β → Eff effs α) → Eff effs α

/-- Monadic bind across payload universes: graft `f` onto the leaves of the
    request tree.

    The heterogeneous form (`α : Type u`, `β : Type v`) is the one handlers need:
    `interpret`'s handler answers at `Type 0` while the computation's result may
    sit higher. `Eff.bind` below is the homogeneous special case that `Monad`
    requires.

    Structurally recursive: the recursive call's argument `k b` applies `k`,
    a component of the very constructor being destructed, for which `Eff`'s
    `brecOn` supplies the hypothesis at every `b`. No `partial`, no explicit
    termination argument. -/
def Eff.bindH {effs : List (Type → Type)} {α : Type u} {β : Type v} :
    Eff effs α → (α → Eff effs β) → Eff effs β
  | .protect a,  f => f a
  | .impure u k, f => .impure u (fun b => Eff.bindH (k b) f)

/-- Monadic bind within one payload universe — `Eff.bindH` at `u = v`. -/
@[inline] def Eff.bind {effs : List (Type → Type)} {α β : Type u} :
    Eff effs α → (α → Eff effs β) → Eff effs β :=
  Eff.bindH

instance {effs : List (Type → Type)} : Monad (Eff.{u} effs) where
  pure := .protect
  bind := Eff.bindH

-- ── Sending effects ────────────────────────────────────────────────────────

/-- Lift a single effect operation into `Eff`, given evidence that the effect is
    in the row. This is how an effect's smart constructors are written. -/
def send {eff : Type → Type} {effs : List (Type → Type)} {α : Type}
    [Member eff effs] (e : eff α) : Eff effs α :=
  .impure (Member.inj e) .protect

-- ── Running ────────────────────────────────────────────────────────────────

/-- Run a computation whose row is empty, extracting its value.

    Reaching `Eff []` means every effect has been interpreted away, so there is
    nothing left to perform — the `impure` branch is discharged by
    `Union.elim0`, since the empty row is uninhabited. -/
def Eff.run {α : Type u} : Eff [] α → α
  | .protect a  => a
  | .impure u _ => u.elim0

/-- Run a computation whose row is the single monad `m`, in `m`.

    The counterpart of `run` for a base monad such as `IO`: upstream's `runM`. -/
def Eff.runM {m : Type → Type} [Monad m] {α : Type} : Eff [m] α → m α
  | .protect a  => pure a
  | .impure u k => match u with
    | .here e   => e >>= fun b => Eff.runM (k b)
    | .there u' => u'.elim0

-- ── Handlers ───────────────────────────────────────────────────────────────

/-- Give the row's head effect a meaning in terms of the remaining effects,
    removing it from the row.

    The handler answers each request of `eff` with an `Eff effs` computation, so
    it may itself use any effect still in the row (that is what makes handlers
    composable). -/
def interpret {eff : Type → Type} {effs : List (Type → Type)} {α : Type u}
    (h : {β : Type} → eff β → Eff effs β) : Eff (eff :: effs) α → Eff effs α
  | .protect a  => .protect a
  | .impure u k => match u with
    | .here e   => (h e).bindH (fun b => interpret h (k b))
    | .there u' => .impure u' (fun b => interpret h (k b))

/-- Interpret the *only* effect in the row into a base monad `m`, running the
    whole computation there.

    The terminal handler: `interpretM h` is `Eff.runM` composed with an
    interpretation into `m`, and is what a real-world effect (filesystem,
    network, database) uses to land in `IO`. -/
def interpretM {eff m : Type → Type} [Monad m] {α : Type}
    (h : {β : Type} → eff β → m β) : Eff [eff] α → m α
  | .protect a  => pure a
  | .impure u k => match u with
    | .here e   => h e >>= fun b => interpretM h (k b)
    | .there u' => u'.elim0

/-- Replace the row's head effect by a *different* effect, rewriting requests
    rather than eliminating them.

    Useful for expressing one effect in terms of a lower-level one (upstream's
    `reinterpret`). -/
def reinterpret {eff eff' : Type → Type} {effs : List (Type → Type)} {α : Type u}
    (h : {β : Type} → eff β → Eff (eff' :: effs) β) :
    Eff (eff :: effs) α → Eff (eff' :: effs) α
  | .protect a  => .protect a
  | .impure u k => match u with
    | .here e   => (h e).bindH (fun b => reinterpret h (k b))
    | .there u' => .impure (.there u') (fun b => reinterpret h (k b))

/-- Handle an effect while **keeping** it in the row, rather than eliminating it.

    Where `interpret` peels an effect off, `interpose` re-handles requests of an
    effect that remains available — what `catchError` and `interposeC` are built
    from. Requests of other effects are passed through untouched.

    `ret` maps the final value; `h` receives each request together with its
    continuation. -/
def interpose {eff : Type → Type} {effs : List (Type → Type)}
    {α : Type u} {β : Type v}
    [Member eff effs] (ret : α → Eff effs β)
    (h : {γ : Type} → eff γ → (γ → Eff effs β) → Eff effs β) :
    Eff effs α → Eff effs β
  | .protect a  => ret a
  | .impure u k => match Member.prj (eff := eff) u with
    | some e => h e (fun x => interpose ret h (k x))
    | none   => .impure u (fun x => interpose ret h (k x))

/-- Weaken a computation by adding an unused effect to the front of its row.

    Sound in the direction that matters for a whitelist: a computation that uses
    fewer effects can always stand in where more are permitted, never the
    reverse. -/
def raise {eff : Type → Type} {effs : List (Type → Type)} {α : Type u} :
    Eff effs α → Eff (eff :: effs) α
  | .protect a  => .protect a
  | .impure u k => .impure (.there u) (fun b => raise (k b))

end Control.Monad.Freer
