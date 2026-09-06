/-
  `Control.Monad.Freer.Coroutine` — the coroutine (yield) effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Coroutine.html
  module #8 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  A coroutine suspends itself with `yield`, handing a value out and waiting for
  one back. `runC` turns such a computation into a `Status`: either it is `done`,
  or it will `continue` once given the value it asked for.

  ## The self-referential `Status`, and why `Eff` is universe-polymorphic

  `Status`'s `continue` constructor holds a `b → Eff effs (Status effs a b r)` —
  the type refers to itself *through* `Eff`. This is the case AGENTS.md singles
  out: a faithful port needs the real construction rather than a weakened type.

  It is strictly positive (the self-reference sits under an arrow and inside
  `Eff`'s positive payload position), so it is a legitimate nested inductive.
  What it also needs is room in the universe hierarchy: `Status` must live in
  `Type 1`, since it holds an `Eff`-valued function, and so `Eff effs Status`
  must accept a `Type 1` payload. That is exactly why
  `Control.Monad.Freer.Eff` is universe-polymorphic with result universe
  `max 1 u` rather than pinned to `Type 0` — at `u = 1` the payload and the
  computation share universe `1`, and the circularity closes. Nothing here needs
  `partial` or a termination annotation.

  ## Substitutions / deviations

  - **Driving a coroutine is the caller's job.** Upstream provides no total
    driver either: a coroutine may yield forever, so any loop that runs one to
    completion is potentially non-terminating. Rather than smuggle in `partial`,
    this module exposes `Status` and leaves stepping to the caller, who can bound
    it (see `Tests/…/CoroutineTest.lean`'s step-bounded driver, which recurses on
    a `Nat` budget).
-/
import Linen.Control.Monad.Freer

namespace Control.Monad.Freer.Coroutine

open Data.OpenUnion Control.Monad.Freer

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The yield effect: hand out an `a`, and resume with a `b` mapped through
    `b → c`.

    The embedded function is upstream's, and is what lets `yield` return a value
    of the caller's choosing rather than raw `b`. -/
inductive Yield (a b : Type) : Type → Type where
  /-- Yield `a`, resuming by applying the given function to the reply. -/
  | yield : a → (b → c) → Yield a b c

/-- The state of a coroutine: finished, or suspended awaiting a value.

    Self-referential through `Eff` — see this module's header for why that
    typechecks and why it needs no termination argument. -/
inductive Status (effs : List (Type → Type)) (a b r : Type) : Type 1 where
  /-- The coroutine finished with a result. -/
  | done     : r → Status effs a b r
  /-- The coroutine yielded `a` and will continue once given a `b`. -/
  | continue : a → (b → Eff effs (Status effs a b r)) → Status effs a b r

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Suspend the computation, yielding `x` and resuming with `f` applied to the
    reply. -/
def yield {a b c : Type} {effs : List (Type → Type)} [Member (Yield a b) effs]
    (x : a) (f : b → c) : Eff effs c :=
  send (.yield x f : Yield a b c)

/-- Yield a value and resume with the reply unchanged — `yield` at `f = id`. -/
def yield' {a b : Type} {effs : List (Type → Type)} [Member (Yield a b) effs]
    (x : a) : Eff effs b :=
  yield x id

-- ── Handlers ────────────────────────────────────────────────────────────────

/-- Reply to a yield request by suspending: build the `continue` status.

    Upstream's `replyC`, factored out so `runC` and `interposeC` share it. -/
def replyC {a b c r : Type} {effs : List (Type → Type)}
    (e : Yield a b c) (k : c → Eff effs (Status effs a b r)) :
    Eff effs (Status effs a b r) :=
  match e with
  | .yield v f => .protect (.continue v (fun bv => k (f bv)))

/-- Launch a coroutine and report its status, removing `Yield` from the row. -/
def runC {a b r : Type} {effs : List (Type → Type)} :
    Eff (Yield a b :: effs) r → Eff effs (Status effs a b r)
  | .protect x  => .protect (.done x)
  | .impure u k => match u with
    | .here e   => replyC e (fun c => runC (k c))
    | .there u' => .impure u' (fun bv => runC (k bv))

/-- Report a coroutine's status *without* removing `Yield` from the row.

    Upstream's `interposeC`, which it notes is useful for nesting coroutines. -/
def interposeC {a b r : Type} {effs : List (Type → Type)}
    [Member (Yield a b) effs] :
    Eff effs r → Eff effs (Status effs a b r) :=
  interpose (eff := Yield a b) (fun x => .protect (.done x))
    (fun e k => replyC e k)

/-- Is the coroutine finished? -/
def Status.isDone {effs : List (Type → Type)} {a b r : Type} :
    Status effs a b r → Bool
  | .done _       => true
  | .continue _ _ => false

/-- The result, if the coroutine has finished. -/
def Status.result? {effs : List (Type → Type)} {a b r : Type} :
    Status effs a b r → Option r
  | .done x       => some x
  | .continue _ _ => none

/-- The value most recently yielded, if the coroutine is suspended. -/
def Status.yielded? {effs : List (Type → Type)} {a b r : Type} :
    Status effs a b r → Option a
  | .done _       => none
  | .continue v _ => some v

end Control.Monad.Freer.Coroutine
