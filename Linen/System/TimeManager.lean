/-
  Linen.System.TimeManager — Connection timeout management

  A background sweeper that tracks a set of handles, each with a deadline,
  and fires an `onTimeout` callback for any handle whose deadline elapses
  without being tickled/paused/canceled first.

  ## Haskell equivalent
  `System.TimeManager` from the `wai` package (used by Warp to enforce
  per-connection read/write timeouts).

  ## Design

  `Manager.new` spawns a dedicated background task that wakes up every
  `timeoutUs` microseconds, scans all registered handles, fires `onTimeout`
  for any whose deadline has passed, and compacts canceled handles out of
  the tracking array. The sweep loop is a cooperative-cancellation loop
  (`while !(← token.isCancelled) do ...`), the same pattern already used for
  `Control.AutoUpdate`'s background updater and `Data.Streaming.Network`'s
  accept loop — it terminates via `Manager.stop` cancelling the shared
  `Std.CancellationToken`, not via a fuel parameter or structural recursion.
-/

import Std.Sync.CancellationToken

namespace System.TimeManager

-- ══════════════════════════════════════════════════════════════
-- Handle state
-- ══════════════════════════════════════════════════════════════

/-- The lifecycle state of a tracked handle. -/
inductive HandleState where
  /-- Actively tracked, with a deadline in monotonic nanoseconds. -/
  | active : Nat → HandleState
  /-- Temporarily exempt from timeout. -/
  | paused : HandleState
  /-- No longer tracked; eligible for removal from the manager. -/
  | canceled : HandleState
deriving BEq, Repr

/-- A single tracked timeout, with its own mutable state and callback. -/
structure Handle where
  /-- Current lifecycle state. -/
  state : IO.Ref HandleState
  /-- Action fired once when the deadline elapses. -/
  onTimeout : IO Unit

-- ══════════════════════════════════════════════════════════════
-- Manager
-- ══════════════════════════════════════════════════════════════

/-- A timeout manager: a sweep interval, the set of tracked handles, and a
    cancellation token used to stop the background sweeper. -/
structure Manager where
  /-- Sweep interval and per-handle timeout, in microseconds. -/
  timeoutUs : Nat
  /-- Currently tracked handles. -/
  handles : IO.Ref (Array Handle)
  /-- Signals the background sweeper to stop. -/
  token : Std.CancellationToken

/-- Create a manager and start its background sweeper.
    Every `timeoutUs` microseconds, the sweeper fires `onTimeout` for any
    handle past its deadline and drops canceled handles from tracking. -/
def Manager.new (timeoutUs : Nat := 30000000) : IO Manager := do
  let handles ← IO.mkRef (#[] : Array Handle)
  let token ← Std.CancellationToken.new
  let mgr : Manager := ⟨timeoutUs, handles, token⟩
  let _task ← IO.asTask (prio := .dedicated) do
    while !(← token.isCancelled) do
      IO.sleep (timeoutUs / 1000).toUInt32
      unless (← token.isCancelled) do
        let now ← IO.monoNanosNow
        let hs ← handles.get
        for h in hs do
          let st ← h.state.get
          match st with
          | .active deadline =>
            if now > deadline then
              h.state.set .canceled
              try h.onTimeout catch _ => pure ()
          | _ => pure ()
        let hs' ← handles.get
        let active ← hs'.toList.filterM fun h => do
          let st ← h.state.get
          pure (st != .canceled)
        handles.set active.toArray
  pure mgr

/-- Register a new timeout, due `timeoutUs` microseconds from now. -/
def Manager.register (mgr : Manager) (onTimeout : IO Unit) : IO Handle := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000
  let state ← IO.mkRef (HandleState.active deadline)
  let handle : Handle := ⟨state, onTimeout⟩
  mgr.handles.modify (·.push handle)
  pure handle

/-- Stop the background sweeper. -/
def Manager.stop (mgr : Manager) : IO Unit :=
  mgr.token.cancel .cancel

-- ══════════════════════════════════════════════════════════════
-- Handle operations
-- ══════════════════════════════════════════════════════════════

/-- Push a handle's deadline `mgr.timeoutUs` microseconds into the future. -/
def Handle.tickle (h : Handle) (mgr : Manager) : IO Unit := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000
  h.state.set (.active deadline)

/-- Cancel a handle; it will be dropped from tracking on the next sweep. -/
def Handle.cancel (h : Handle) : IO Unit :=
  h.state.set .canceled

/-- Temporarily exempt a handle from timeout. -/
def Handle.pause (h : Handle) : IO Unit :=
  h.state.set .paused

/-- Resume a paused handle, with a fresh deadline. -/
def Handle.resume (h : Handle) (mgr : Manager) : IO Unit := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000
  h.state.set (.active deadline)

end System.TimeManager
