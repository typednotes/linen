/-
  Tests for `Linen.Control.Monad.Effect.Handler`.

  Covers `Eff.handle` on the empty row, on a composed row (`FileSystem`,
  `Trace`, `Error`), error propagation into `IO`, errors caught before they
  reach `IO`, request ordering through a test-only effect, and the rows that
  have no `Handlers` instance, plus running into a monad other than `IO`.
-/
import Linen.Control.Monad.Effect.Handler
import Linen.Control.Monad.Effect.Trace
import Linen.Control.Monad.Effect.Error
import Linen.Control.Monad.Effect.FileSystem
import Linen.Control.Monad.Effect.HTTP
import Linen.Control.Monad.Effect.Reader

open Data.OpenUnion Control.Monad.Effect
open Control.Monad.Effect.Trace Control.Monad.Effect.Error Control.Monad.Effect.FileSystem

namespace Tests.Control.Monad.Effect.Handler

-- ── Instances are derived for the rows that have one ────────────────────────

example : Handlers [] IO := inferInstance
example : Handlers [Trace] IO := inferInstance
example : Handlers [Trace, Error String] IO := inferInstance
example : Handlers [FileSystem readOnly, Trace, Error String] IO := inferInstance
example : Handlers [HTTP.HTTP HTTP.readOnlyWeb, Trace] IO := inferInstance

-- A row with an effect that needs configuration (`Reader`) has none, so
-- `Eff.handle` does not elaborate for it: peel it with `runReader` first.
/--
error: failed to synthesize instance of type class
  Handlers [Reader.Reader Nat] IO

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
example : IO Nat := Eff.handle (Reader.ask : Eff [Reader.Reader Nat] Nat)

example : IO Nat := Eff.handle (Reader.runReader 3 (Reader.ask : Eff [Reader.Reader Nat, Trace] Nat))

-- ── The empty row ───────────────────────────────────────────────────────────

/-- info: 42 -/
#guard_msgs in
#eval (Eff.handle (pure 42 : Eff [] Nat) : IO Nat)

-- ── A test-only effect records the order requests are answered in ──────────

/-- Append a number to a log. The request carries the log, so the handler
    needs no configuration of its own. -/
inductive Tick : Type → Type where
  | tick : IO.Ref (Array Nat) → Nat → Tick Unit

instance : Handler Tick IO where
  handle | .tick log n => log.modify (·.push n)

def tick {effs : List (Type → Type)} [Member Tick effs] (log : IO.Ref (Array Nat)) (n : Nat) :
    Eff effs Unit :=
  send (Tick.tick log n)

/-- info: (#[1, 2, 3], "done") -/
#guard_msgs in
#eval show IO (Array Nat × String) from do
  let log ← IO.mkRef #[]
  let r ← Eff.handle (do tick log 1; tick log 2; tick log 3; pure "done" : Eff [Trace, Tick] String)
  pure (← log.get, r)

-- ── A composed row: filesystem, trace and error ─────────────────────────────

/-- Write a file, read it back and delete it, tracing as it goes. (The trace
    goes to stderr; `#eval` captures stderr too, hence the first `info`.) -/
def roundTrip (path : Path)
    (hw : full.permits .write path = true := by decide)
    (hr : full.permits .read path = true := by decide)
    (hd : full.permits .delete path = true := by decide) :
    Eff [FileSystem full, Trace, Error String] String := do
  writeFileString path "row round-trip" hw
  trace "written"
  let some contents ← readFileString? path hr | throwError "unreadable"
  deleteFile path hd
  pure contents

/--
info: written
---
info: ("row round-trip", false)
-/
#guard_msgs in
#eval show IO (String × Bool) from do
  let dir ← IO.currentDir
  let contents ← Eff.handle (roundTrip ["linen-runio-test.tmp"])
  pure (contents, ← (dir / "linen-runio-test.tmp").pathExists)

-- ── Errors ──────────────────────────────────────────────────────────────────

-- An uncaught error aborts the run as an `IO` exception carrying its text,
-- and nothing after the throw is performed.
/-- info: (#[1], "boom") -/
#guard_msgs in
#eval show IO (Array Nat × String) from do
  let log ← IO.mkRef #[]
  let r ← try
      let _ ← Eff.handle (do tick log 1; throwError "boom"; tick log 2; pure 0 :
        Eff [Tick, Error String] Nat)
      pure "no error"
    catch e => pure (toString e)
  pure (← log.get, r)

-- A caught error never reaches `IO`.
/-- info: 7 -/
#guard_msgs in
#eval (Eff.handle (orElseValue (throwError "ignored") 7 : Eff [Error String] Nat) : IO Nat)

-- ── Agreement with the single-effect handlers ───────────────────────────────

example (m : Eff [FileSystem full] Nat) : Eff.handle m = runFileSystem full m :=
  handle_eq_runFileSystem full m

example (m : Eff [HTTP.HTTP HTTP.readOnlyWeb] Nat) : Eff.handle m = HTTP.runHTTP _ m :=
  HTTP.handle_eq_runHTTP _ m

example (m : Eff [Tick] Nat) : Eff.handle m = interpretM (m := IO) Handler.handle m :=
  Eff.handle_singleton m

-- ── Any base monad, not only `IO` ───────────────────────────────────────────

/-- A pure test effect, lifted into `Id` rather than `IO`. -/
inductive Const : Type → Type where
  | answer : Const Nat

instance : Handler Const Id where
  handle | .answer => (42 : Nat)

#guard Id.run (Eff.handle (do
    let a ← send Const.answer
    pure (a + 1) : Eff [Const] Nat)) == 43

-- ── Not a whitelist ─────────────────────────────────────────────────────────

-- Any library may hand an effect of its own a handler into `IO` — here, one
-- that performs arbitrary `IO`. `Handlers` decides *how* a row runs, never
-- *whether* it is acceptable (see the module documentation).
inductive Ambient : Type → Type where
  | io {α : Type} : IO α → Ambient α

instance : Handler Ambient IO where
  handle | .io act => act

example : Handlers [Ambient, Trace] IO := inferInstance

end Tests.Control.Monad.Effect.Handler
