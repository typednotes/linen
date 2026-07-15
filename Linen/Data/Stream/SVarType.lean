/-
  Linen.Data.Stream.SVarType — the pure stream-scheduling `State` record

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.SVar.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/SVar/Type.hs),
  module #6 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Only the **pure scheduling-state record** `State`, threaded through
  `StreamK`/`Stream`, together with `defState`/`adaptState` and the yield/limit
  accessors, is in scope. The concurrent `SVar` itself (worker pools, rate
  control) is out of scope for this import — see the plan's scope note.

  ## Substitutions / deviations

  - **`streamVar` field dropped.** Upstream's first field is
    `Maybe (SVar t m a)` — a handle to the concurrent scheduler, which is out
    of scope. It is omitted here; `adaptState` therefore only resets the yield
    limit (upstream also nils `streamVar`).
  - **`_maxStreamRate` field dropped.** It holds a `Rate` from the concurrency
    scheduling layer (also out of scope), so it and its `get/setStreamRate`
    accessors are omitted.
  - **`_streamLatency` kept as `Option Int`** (nanoseconds), rather than
    upstream's `NanoSecond64` newtype which lives in the deferred `Time.*`
    tree.
  - **`Count`/`Int64` → `Nat`/`Int`.** The yield-limit count is `Option Nat`
    and the setters take `Int` (matching upstream's `Int`/`Int64` argument
    types) with the same clamping behaviour.
-/

namespace Data.Stream

-- ── Limits ──────────────────────────────────────────────────────────────────

/-- A configurable upper bound: either `unlimited` or `limited` to `n`. -/
inductive Limit where
  | unlimited
  | limited (n : Nat)
  deriving Repr, DecidableEq, Inhabited, BEq

/-- Upstream's `magicMaxBuffer`, the default thread/buffer ceiling. -/
def magicMaxBuffer : Nat := 1500

/-- Default maximum number of worker threads. -/
def defaultMaxThreads : Limit := .limited magicMaxBuffer

/-- Default maximum output-buffer size. -/
def defaultMaxBuffer : Limit := .limited magicMaxBuffer

-- ── The scheduling state ────────────────────────────────────────────────────

/-- The pure stream-scheduling configuration threaded through stream
    evaluation. Fields prefixed `_` upstream are meant to be touched only via
    the accessors below. -/
structure State where
  /-- One-shot yield limit, reset for each API call. -/
  yieldLimit : Option Nat := none
  /-- Persistent maximum thread count. -/
  threadsHigh : Limit := defaultMaxThreads
  /-- Persistent maximum buffer size. -/
  bufferHigh : Limit := defaultMaxBuffer
  /-- Optional measured per-element stream latency, in nanoseconds. -/
  streamLatency : Option Int := none
  /-- Whether inspection/diagnostics mode is on. -/
  inspectMode : Bool := false
  deriving Repr, Inhabited

/-- The default scheduling state. -/
def defState : State := {}

/-- Adapt the scheduling state across a stream type change, resetting the
    one-shot yield limit (upstream also nils the dropped `streamVar`). -/
def adaptState (st : State) : State := { st with yieldLimit := none }

-- ── Accessors ───────────────────────────────────────────────────────────────

/-- Set the yield limit; non-positive clamps to `0`, `none` clears it. -/
def setYieldLimit (lim : Option Int) (st : State) : State :=
  { st with yieldLimit :=
      match lim with
      | none => none
      | some n => if n <= 0 then some 0 else some n.toNat }

/-- Get the yield limit. -/
def getYieldLimit (st : State) : Option Nat := st.yieldLimit

/-- Set the max buffer: `n < 0` → `unlimited`, `n = 0` → default, else limited. -/
def setMaxBuffer (n : Int) (st : State) : State :=
  { st with bufferHigh :=
      if n < 0 then .unlimited
      else if n == 0 then defaultMaxBuffer
      else .limited n.toNat }

/-- Get the max buffer limit. -/
def getMaxBuffer (st : State) : Limit := st.bufferHigh

/-- Set the max thread count: `n < 0` → `unlimited`, `n = 0` → default. -/
def setMaxThreads (n : Int) (st : State) : State :=
  { st with threadsHigh :=
      if n < 0 then .unlimited
      else if n == 0 then defaultMaxThreads
      else .limited n.toNat }

/-- Get the max thread count. -/
def getMaxThreads (st : State) : Limit := st.threadsHigh

/-- Set the measured stream latency (nanoseconds); non-positive clears it. -/
def setStreamLatency (n : Int) (st : State) : State :=
  { st with streamLatency := if n <= 0 then none else some n }

/-- Get the measured stream latency. -/
def getStreamLatency (st : State) : Option Int := st.streamLatency

/-- Turn on inspection mode. -/
def setInspectMode (st : State) : State := { st with inspectMode := true }

/-- Get the inspection-mode flag. -/
def getInspectMode (st : State) : Bool := st.inspectMode

end Data.Stream
