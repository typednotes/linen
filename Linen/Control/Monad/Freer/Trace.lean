/-
  `Control.Monad.Freer.Trace` — the tracing effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Trace.html
  module #10 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  Emits diagnostic strings. Its point is that tracing becomes *visible in the
  type*: `Eff [Trace] α` announces that a computation logs, and a computation
  whose row omits `Trace` provably does not — the discipline ambient
  `IO.println` cannot offer.

  Upstream's only handler prints to standard output. `runTracePure` is added
  here so tracing can be tested without performing I/O, which is what lets this
  module's own tests be `#guard`s.
-/
import Linen.Control.Monad.Freer

namespace Control.Monad.Freer.Trace

open Data.OpenUnion Control.Monad.Freer

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The tracing effect: a single request carrying a message. -/
inductive Trace : Type → Type where
  /-- Emit a diagnostic message. -/
  | trace : String → Trace Unit

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Emit a diagnostic message. -/
def trace {effs : List (Type → Type)} [Member Trace effs] (msg : String) :
    Eff effs Unit :=
  send (.trace msg : Trace Unit)

-- ── Handlers ────────────────────────────────────────────────────────────────

/-- Run a traced computation in `IO`, printing each message (upstream's
    `runTrace`). -/
def runTrace {α : Type} : Eff [Trace] α → IO α :=
  interpretM fun
    | .trace msg => IO.println msg

/-- Run a traced computation purely, collecting the messages in order instead of
    printing them, and removing the effect from the row.

    Not upstream — added so tracing is testable without I/O. -/
def runTracePure {effs : List (Type → Type)} {α : Type} :
    Eff (Trace :: effs) α → Eff effs (α × List String) :=
  go []
where
  /-- Collect onto `acc` (kept reversed, so appending is O(1)). -/
  go {α : Type} (acc : List String) :
      Eff (Trace :: effs) α → Eff effs (α × List String)
    | .protect a  => .protect (a, acc.reverse)
    | .impure u k => match u with
      | .here e   => match e with
        | .trace msg => go (msg :: acc) (k ())
      | .there u' => .impure u' (fun b => go acc (k b))

/-- Run a traced computation purely, discarding the messages. -/
def ignoreTrace {effs : List (Type → Type)} {α : Type}
    (m : Eff (Trace :: effs) α) : Eff effs α :=
  Prod.fst <$> runTracePure m

end Control.Monad.Freer.Trace
