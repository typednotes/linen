/-
  `Control.Monad.Effect.NonDet` — the nondeterminism effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-NonDet.html
  (the `NonDet` type itself lives in `Control.Monad.Freer.Internal` upstream),
  module #7 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  A nondeterministic computation may fail (`mzero`) or branch (`mplus`); running
  it collects every result. The branching trick is upstream's: a choice is a
  single request answered with a `Bool`, and the handler answers it *both* ways,
  so the two branches are the same continuation resumed twice.

  ## Substitutions / deviations

  - **`mzero` answers with `Empty`.** Upstream's `MZero :: NonDet a` is a request
    of arbitrary answer type. Written in Lean as `| mzero : NonDet α`, the
    constructor would bind `{α : Type}` and force `NonDet` into `Type 1`, which
    cannot be an effect (`Type → Type`). Answering with `Empty` says the same
    thing more precisely — a failed branch produces nothing, so its continuation
    is unreachable — and `mzero` recovers an arbitrary result type by
    `Empty.elim`. `Error.throw` uses the same device.

  - **`makeChoiceA` is specialised to `List`.** Upstream is generic over
    `Alternative f`. Lean's `Alternative` has no `many`/`toList` bridge that
    would let the handler build an arbitrary `f` from two branches without also
    assuming a monoid structure on it, so the handler is written at `List`, the
    instance every use of upstream's version actually takes. `Control.Applicative`
    already provides `asum` for folding the result into another `Alternative`.

  - **`msplit` is not ported.** Upstream's `msplit` peels off the first solution
    plus a computation for the rest, via a queue of pending branches. Its
    recursion resumes from a *queued* computation rather than a subterm of the
    one being traversed, so it is not structurally recursive, and no measure is
    available: `sizeOf` gives nothing for the continuation carried by a Freer
    node, and on an infinitely-branching computation `msplit` genuinely
    diverges — upstream is total only by Haskell's laziness. Porting it would
    need `partial` or a fuel parameter, both of which AGENTS.md forbids, so it is
    left out rather than faked. `makeChoiceA` covers the finite-search use.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.NonDet

open Data.OpenUnion Control.Monad.Effect

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The nondeterminism effect.

    `mzero` is failure and answers with `Empty`; `mplus` is a binary choice,
    answered with `Bool` so that a handler can resume the same continuation with
    both `true` and `false`. -/
inductive NonDet : Type → Type where
  /-- Failure: this branch yields no results. -/
  | mzero : NonDet Empty
  /-- A binary choice, answered both ways by the handler. -/
  | mplus : NonDet Bool

-- ── Operations ──────────────────────────────────────────────────────────────

/-- The failing computation. Its result type is arbitrary because the request
    answers with `Empty`. -/
def mzero {effs : List (Type → Type)} {α : Type u} [Member NonDet effs] :
    Eff effs α :=
  (send (.mzero : NonDet Empty)).bindH Empty.elim

/-- Choose between two computations, collecting the results of both. -/
def mplus {effs : List (Type → Type)} {α : Type u} [Member NonDet effs]
    (l r : Eff effs α) : Eff effs α :=
  (send (.mplus : NonDet Bool)).bindH fun b => if b then l else r

/-- Choose among a list of computations; `[]` is `mzero`. -/
def choose {effs : List (Type → Type)} {α : Type} [Member NonDet effs] :
    List (Eff effs α) → Eff effs α
  | []      => mzero
  | m :: ms => mplus m (choose ms)

/-- Nondeterministically pick one element of a list. -/
def select {effs : List (Type → Type)} {α : Type} [Member NonDet effs]
    (xs : List α) : Eff effs α :=
  choose (xs.map .protect)

/-- Keep only branches satisfying `p`; a failing guard prunes the branch. -/
def guard {effs : List (Type → Type)} [Member NonDet effs] (p : Bool) :
    Eff effs Unit :=
  if p then .protect () else mzero

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Collect every result of a nondeterministic computation, removing the effect
    from the row.

    `mplus` resumes the continuation twice — once with `true`, once with
    `false` — and concatenates, so the result lists the branches left to right.
    Structurally recursive: both resumptions apply `k`, a component of the
    constructor being destructed. -/
def makeChoiceA {effs : List (Type → Type)} {α : Type} :
    Eff (NonDet :: effs) α → Eff effs (List α)
  | .protect a  => .protect [a]
  | .impure u k => match u with
    | .here e   => match e with
      | .mzero => .protect []
      | .mplus => (makeChoiceA (k true)).bindH fun xs =>
                  (makeChoiceA (k false)).bindH fun ys => .protect (xs ++ ys)
    | .there u' => .impure u' (fun b => makeChoiceA (k b))

/-- Collect at most the first result, or `none` if every branch fails. -/
def makeChoiceFirst {effs : List (Type → Type)} {α : Type}
    (m : Eff (NonDet :: effs) α) : Eff effs (Option α) :=
  List.head? <$> makeChoiceA m

end Control.Monad.Effect.NonDet
