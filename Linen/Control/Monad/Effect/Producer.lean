/-
  Pure sequential producers, lowered to caller-owned resumable steps.

  `run do ...` supports ordinary Lean branches, finite loops, local variables,
  `yield`, `yieldAll` and `wait`. The script uses linen's existing coroutine
  effect; the exposed step has an empty effect row. A JSON cursor counts the
  instructions already consumed. On resumption, pure control flow is rebuilt
  and earlier instructions are skipped, so no emitted value or wait is repeated.
  No executable closure or effect result is persisted. Reconstruction takes
  time proportional to the consumed prefix; `every` resets it each cycle.

  The caller owns the clock, state and scheduling. Delays are milliseconds;
  they request another call rather than sleeping. This adapter deliberately
  accepts only pure scripts: replaying arbitrary effects would repeat them.
-/
import Lean.Data.Json
import Linen.Control.Monad.Effect.Coroutine

namespace Control.Monad.Effect.Producer

open Coroutine

-- ── Sequential authoring ────────────────────────────────────────────────────

/-- An immediate batch of element emissions, or a relative millisecond delay. -/
inductive Command (β : Type) where
  | values : List β → Command β
  | wait : Nat → Command β

/-- A pure script with coroutine suspension points and ordinary `do` notation.
    Instructions return `Unit`; only the interpreter observes the clock. -/
abbrev Script (β : Type) := Eff [Yield (Command β) Unit]

/-- Emit one value. If the element type itself is a list, emit that whole list. -/
def yield {β : Type} (value : β) : Script β Unit :=
  Coroutine.yield' (Command.values [value])

/-- Emit each list element, in order, without a delay between elements. -/
def yieldAll {β : Type} (values : List β) : Script β Unit :=
  Coroutine.yield' (Command.values values)

/-- Suspend for this many milliseconds. A zero delay continues immediately. -/
def wait {β : Type} (milliseconds : Nat) : Script β Unit :=
  Coroutine.yield' (Command.wait milliseconds : Command β)

-- ── Serializable continuation and lowering ──────────────────────────────────

/-- The consumed instruction count. Reconstruct with the same script/arguments;
    new arguments start a fresh cursor. No local executable closure is stored. -/
structure Cursor where
  position : Nat := 0
  deriving Lean.ToJson, Lean.FromJson, DecidableEq, Repr, Inhabited

/-- Consume the pure prefix and then emit until the next positive wait or end.
    Structural recursion is on the existing coroutine's request tree. -/
private def collect {β : Type} (now : Nat) :
    Script β Unit → Nat → Nat → List β → List β × Cursor × Option Nat
  | .protect (), _, position, emitted => (emitted.reverse, ⟨position⟩, none)
  | .impure (.there impossible) _, _, _, _ => impossible.elim0
  | .impure (.here (.yield command reply)) next, skip, position, emitted =>
    if skip > 0 then
      collect now (next (reply ())) (skip - 1) (position + 1) emitted
    else
      match command with
      | .values values =>
        collect now (next (reply ())) 0 (position + 1) (values.reverse ++ emitted)
      | .wait 0 => collect now (next (reply ())) 0 (position + 1) emitted
      | .wait (milliseconds + 1) =>
        (emitted.reverse, ⟨position + 1⟩, some (now + milliseconds + 1))

/-- Lower a finite pure script to the standard resumable producer signature.
    The caller invokes this only when its previous wake-up is due, and persists
    the returned cursor along with the enclosing graph state. -/
def run {β : Type} (script : Script β Unit) (now : Nat) (state : Option Cursor) :
    Eff [] (List β × Cursor × Option Nat) :=
  pure (collect now script (state.getD {}).position 0 [])

/-- A repeating source retains only its cycle number and current cycle cursor. -/
structure CycleCursor where
  cycle : Nat := 0
  cursor : Cursor := {}
  deriving Lean.ToJson, Lean.FromJson, DecidableEq, Repr, Inhabited

/-- Repeat a finite pure block indefinitely, waiting `periodMs` after each
    completed block. The block receives its zero-based cycle number and may
    contain additional waits. A zero period uses the minimum positive 1 ms.
    On a late call, continue at the actual `now`, without catch-up cycles. -/
def every {β : Type} (periodMs : Nat) (body : Nat → Script β Unit)
    (now : Nat) (state : Option CycleCursor) :
    Eff [] (List β × CycleCursor × Option Nat) := do
  let state := state.getD {}
  let script (cycle : Nat) : Script β Unit := do
    body cycle
    wait (max 1 periodMs)
  let (values, cursor, nextCallAt) := collect now (script state.cycle) state.cursor.position 0 []
  match nextCallAt with
  | some next => pure (values, { state with cursor }, some next)
  | none =>
    let cycle := state.cycle + 1
    let (more, cursor, nextCallAt) := collect now (script cycle) 0 0 []
    pure (values ++ more, { cycle, cursor }, nextCallAt)

end Control.Monad.Effect.Producer
