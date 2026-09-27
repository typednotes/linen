/-
  `Control.Monad.Effect.Handler` — canonical handlers, and running a whole
  effect row with them

  `Eff.runM` runs a row made of one monad, and each effect's own handler
  (`runHTTP`, `runFileSystem`, `runTrace`, …) runs a row made of that one
  effect. Neither runs `Eff [HTTP cap, Trace, Error String] α`: composing
  handlers into `IO` by hand means peeling the row effect by effect.

  This module names the missing piece once:

  - `Handler eff m` — the effect's **canonical** handler into the monad `m`:
    one function answering each request, `{β} → eff β → m β` — the argument
    `interpretM` takes, promoted to an instance so a row can find it. Effects
    whose meaning in `IO` needs no configuration have a `Handler _ IO`
    instance next to their definition.
  - `Handlers effs m` — every effect of the row has one; derived structurally
    (`[]` has nothing to answer; `eff :: effs` answers `here` with `eff`'s
    handler and `there` with the tail's).
  - `Eff.handle` — run a computation over such a row in `m`, answering
    requests in the order the computation makes them. On a single-effect row
    it is exactly `interpretM Handler.handle` (`Eff.handle_singleton`), so it
    agrees with each effect's own `run…`.

  ## Names, and why not core's `MonadLift`

  "Handler" is the algebraic-effects term (Plotkin & Pretnar) and the one this
  library's effect modules already use for their `run…` functions.

  A handler has the *type* of core's `MonadLift eff m`
  (`monadLift : {α} → m α → n α`, no `Monad` required of the source), but not
  its *design*: `MonadLift`'s source is a `semiOutParam` — the outer monad
  determines the inner one, as in a transformer stack — so an instance must
  fix the source from the target, and `MonadLift (FileSystem cap) IO` (or
  `HTTP cap`, `Error ε`) is rejected: nothing in `IO` determines `cap`.
  Using it would mean disabling that check. Instances on `Union effs` or
  `Eff effs` directly would be worse still: they match every target, and core's
  transitive `MonadLiftT` would enumerate them, with an unknown row, inside
  every unrelated lift search.

  ## Which effects have a handler into `IO`

  Only those that need no configuration, so a row alone determines how it
  runs:

  | Effect | `Handler _ IO` | Meaning |
  |---|---|---|
  | `Trace` | yes | the message, on **stderr** (`runTrace` prints to stdout; a program that runs a row as a service keeps stdout for its own output) |
  | `Error ε` | yes, given `ToString ε` | an `IO.userError` carrying the rendered error |
  | `HTTP cap` | yes | `sendOnce`, as `runHTTP` |
  | `FileSystem cap` | yes | `IO.FS`, as `runFileSystem` |
  | `Reader ρ`, `State σ`, `Writer ω`, `Fresh` | **no** | need an initial value or return a result — peel them with their own handlers first |
  | `PostgreSQL`, `ObjectStore`, `Queue`, `SecretStore` | **no** | need a connection, credentials or a backend |
  | `NonDet`, `Coroutine` | **no** | have no single-answer meaning |

  **`Handlers` is not a whitelist.** Any library may give an effect of its own
  (one wrapping arbitrary `IO`, say) a `Handler _ IO`. The row in the type says
  what a computation may do; whether a given row is *acceptable* is a policy of
  the code that runs it, which should inspect the row itself.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect

open Data.OpenUnion

-- ── Classes ─────────────────────────────────────────────────────────────────

/-- The canonical handler of the effect `eff` into the monad `m`: an answer in
    `m` to each of its requests. -/
class Handler (eff : Type → Type) (m : Type → Type) where
  /-- Answer one request. -/
  handle : {β : Type} → eff β → m β

/-- Every effect in the row has a `Handler` into `m`. Instances are derived
    structurally; there is no reason to write one by hand. -/
class Handlers (effs : List (Type → Type)) (m : Type → Type) where
  /-- Answer one request of any effect in the row. -/
  handleUnion : {β : Type} → Union effs β → m β

/-- The empty row has no requests to answer. -/
instance instHandlersNil {m : Type → Type} : Handlers [] m where
  handleUnion u := u.elim0

/-- A row is handled in `m` when its head and its tail are. -/
instance instHandlersCons {eff m : Type → Type} {effs : List (Type → Type)}
    [Handler eff m] [Handlers effs m] : Handlers (eff :: effs) m where
  handleUnion
    | .here e   => Handler.handle e
    | .there u' => Handlers.handleUnion u'

-- ── Running ─────────────────────────────────────────────────────────────────

/-- Run a computation in `m`, answering each request with its effect's
    canonical `Handler`, in order. The row counterpart of `Eff.runM` (which
    runs `Eff [m] α`) and of `interpretM` (which handles one effect with an
    explicit function). -/
def Eff.handle {effs : List (Type → Type)} {m : Type → Type} [Monad m]
    [Handlers effs m] {α : Type} : Eff effs α → m α
  | .protect a  => pure a
  | .impure u k => Handlers.handleUnion u >>= fun b => Eff.handle (k b)

-- ── Properties ──────────────────────────────────────────────────────────────

/-- A pure computation runs to its value. -/
@[simp] theorem Eff.handle_protect {effs : List (Type → Type)} {m : Type → Type} [Monad m]
    [Handlers effs m] {α : Type} (a : α) :
    Eff.handle (effs := effs) (m := m) (.protect a) = pure a := rfl

/-- On a single-effect row, `handle` is `interpretM` with the effect's
    canonical handler — so it agrees with each effect's own `run…` whenever
    that is `interpretM` with the same function. -/
theorem Eff.handle_singleton {eff m : Type → Type} [Monad m] [Handler eff m] {α : Type}
    (c : Eff [eff] α) : Eff.handle c = interpretM (m := m) Handler.handle c := by
  induction c with
  | protect a => rfl
  | impure u k ih =>
    cases u with
    | here e => simp only [Eff.handle, interpretM, Handlers.handleUnion, ih]
    | there u' => exact u'.elim0

end Control.Monad.Effect
