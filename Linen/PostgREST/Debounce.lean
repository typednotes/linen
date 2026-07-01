/-
  PostgREST.Debounce — Debounce utility

  Prevents rapid-fire schema cache reloads by ignoring events that
  arrive within a configurable debounce window.

  ## Haskell source
  - `PostgREST.Debounce` (postgrest package)
-/

namespace PostgREST.Debounce

/-- A debouncer that rate-limits an IO action.
    $$\text{Debouncer} = \text{IORef}\ \text{Nat} \times \text{Nat}$$ -/
structure Debouncer where
  /-- Timestamp (in milliseconds) of the last invocation. -/
  lastRun : IO.Ref Nat
  /-- Minimum interval between invocations (in milliseconds). -/
  intervalMs : Nat

/-- Create a new debouncer with the given interval. -/
def Debouncer.create (intervalMs : Nat) : IO Debouncer := do
  let ref ← IO.mkRef 0
  return { lastRun := ref, intervalMs }

/-- Run an action if enough time has passed since the last run. -/
def Debouncer.run (d : Debouncer) (action : IO Unit) : IO Unit := do
  let now ← IO.monoMsNow
  let last ← d.lastRun.get
  if now - last >= d.intervalMs then
    d.lastRun.set now
    action

end PostgREST.Debounce
